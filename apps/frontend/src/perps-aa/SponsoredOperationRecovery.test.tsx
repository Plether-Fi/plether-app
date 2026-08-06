import { act, render, waitFor } from '@testing-library/react'
import {
  concatHex,
  numberToHex,
  type Address,
  type Hex,
} from 'viem'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { getOrCreateDepositAuthorization } from './authorizationStore'
import {
  canForceUnlockLegacySponsoredOperation,
  createSponsoredOperationSignal,
  LEGACY_AMBIGUOUS_OPERATION_MANIFEST_VERSION,
  releaseSponsoredOperationSignal,
  SPONSORED_OPERATION_AUTOMATIC_RECOVERY_MAX_DELAY_MS,
  SPONSORED_OPERATION_LANE_HEAD_PREFIX,
  SPONSORED_OPERATION_RESOLUTION_PREFIX,
  useSponsoredOperationStore,
} from './operationStore'
import { PIMLICO_SINGLETON_PAYMASTER_V8 } from './paymasterValidity'
import {
  PerpsAaRuntimeContext,
  type ManagedUserOperation,
  type ManagedUserOperationReceipt,
  type PerpsAaSmartAccountRuntime,
  type SponsoredOperationRecoverySnapshot,
  UserOperationReceiptNotSafeError,
} from './runtimeContext'
import { SponsoredOperationRecovery } from './SponsoredOperationRecovery'

const OWNER = '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address
const TOKEN = '0x9999999999999999999999999999999999999999' as Address
const USER_OPERATION_HASH = `0x${'aa'.repeat(32)}` as Hex
const OTHER_USER_OPERATION_HASH = `0x${'cc'.repeat(32)}` as Hex
const TRANSACTION_HASH = `0x${'bb'.repeat(32)}` as Hex
const INCLUDED_BLOCK_HASH = `0x${'dd'.repeat(32)}` as Hex
const MANIFEST_VERSION = 'perps-aa-arbitrum-sepolia-v1'
const AUTHORIZATION_STORAGE_SUFFIX =
  `421614:${OWNER.toLowerCase()}:${ACCOUNT.toLowerCase()}:${TOKEN.toLowerCase()}`
const LEGACY_AUTHORIZATION_KEY =
  `plether_perps_eip3009_v1:${AUTHORIZATION_STORAGE_SUFFIX}`
const CURRENT_AUTHORIZATION_KEY =
  `plether_perps_eip3009_v2:${AUTHORIZATION_STORAGE_SUFFIX}`

function paymasterData(validUntil: bigint): Hex {
  return concatHex([
    '0x01',
    numberToHex(validUntil, { size: 6 }),
    numberToHex(0n, { size: 6 }),
    `0x${'11'.repeat(65)}`,
  ])
}

function signedOperation(input: {
  nonce?: bigint
  validUntil?: bigint
} = {}): ManagedUserOperation {
  return {
    sender: ACCOUNT,
    nonce: input.nonce ?? 7n,
    callData: '0x1234',
    callGasLimit: 1n,
    verificationGasLimit: 2n,
    preVerificationGas: 3n,
    maxFeePerGas: 4n,
    maxPriorityFeePerGas: 5n,
    paymaster: PIMLICO_SINGLETON_PAYMASTER_V8,
    paymasterData: paymasterData(input.validUntil ?? 1_000n),
    paymasterVerificationGasLimit: 6n,
    paymasterPostOpGasLimit: 7n,
    signature: '0xdeadbeef',
  }
}

function receiptNotFoundError(): Error {
  const error = new Error('receipt not found')
  error.name = 'UserOperationReceiptNotFoundError'
  return error
}

function runtimeValue(input: {
  manifestVersion?: string
  status?: PerpsAaSmartAccountRuntime['smartAccount']['getUserOperationStatus']
  receipt?: PerpsAaSmartAccountRuntime['smartAccount']['getUserOperationReceipt']
  nonce?: bigint
  chainTimestamp?: bigint
  userOperationEvidence?:
    SponsoredOperationRecoverySnapshot['userOperationEvidence']
  computedHash?: Hex
  getRecoverySnapshot?: PerpsAaSmartAccountRuntime['getRecoverySnapshot']
  verifyObservedInclusion?:
    PerpsAaSmartAccountRuntime['verifyObservedInclusion']
} = {}): PerpsAaSmartAccountRuntime {
  return {
    chainId: 421614,
    ownerAddress: OWNER,
    factoryAddress:
      '0x6666666666666666666666666666666666666666',
    accountVersion: 'permissionless-simple-v0.8',
    accountIndex: '0',
    manifestVersion: input.manifestVersion ?? MANIFEST_VERSION,
    verifyObservedInclusion: input.verifyObservedInclusion,
    getRecoverySnapshot: input.getRecoverySnapshot ?? vi.fn(async () => ({
      blockNumber: 123n,
      blockTimestamp: input.chainTimestamp ?? 0n,
      accountNonce: input.nonce ?? 0n,
      userOperationEvidence:
        input.userOperationEvidence ?? { kind: 'not-located' },
    })),
    smartAccount: {
      accountAddress: ACCOUNT,
      entryPoint:
        '0x3333333333333333333333333333333333333333',
      prepareUserOperation: vi.fn(),
      signUserOperation: vi.fn(),
      getUserOperationHash: vi.fn(
        () => input.computedHash ?? USER_OPERATION_HASH
      ),
      sendUserOperation: vi.fn(),
      getUserOperationStatus: input.status ?? vi.fn(async () => ({
        status: 'not_found',
        transactionHash: null,
      })),
      getUserOperationReceipt: input.receipt ?? vi.fn(async () => {
        throw receiptNotFoundError()
      }),
    },
  }
}

function beginHashOperation(input: {
  id: string
  manifestVersion?: string
  operation?: ManagedUserOperation
  action?: 'deposit' | 'place-order'
  authorizationToken?: Address
  authorizationNonce?: Hex
}): void {
  useSponsoredOperationStore.getState().beginOperation({
    id: input.id,
    ownerAddress: OWNER,
    accountAddress: ACCOUNT,
    chainId: 421614,
    accountMode: 'simple',
    manifestVersion: input.manifestVersion ?? MANIFEST_VERSION,
    action: input.action ?? 'place-order',
    authorizationToken: input.authorizationToken,
    authorizationNonce: input.authorizationNonce,
  })
  useSponsoredOperationStore.getState().recordUserOperationHash(
    input.id,
    USER_OPERATION_HASH,
    input.operation
      ? { signedUserOperation: input.operation }
      : undefined
  )
  useSponsoredOperationStore.getState().failOperation({
    id: input.id,
    status: 'receipt-timeout',
    reason: 'BUNDLER_UNAVAILABLE',
    retryable: false,
  })
}

describe('SponsoredOperationRecovery', () => {
  beforeEach(() => {
    vi.stubGlobal('navigator', {
      locks: {
        request: vi.fn(async (
          name: string,
          _options: LockOptions,
          callback: (lock: Lock | null) => Promise<unknown> | unknown
        ) => await callback({ name, mode: 'exclusive' } as Lock)),
      } as unknown as LockManager,
    })
    globalThis.localStorage.clear()
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
  })

  afterEach(() => {
    releaseSponsoredOperationSignal('live-operation')
    vi.restoreAllMocks()
    vi.unstubAllGlobals()
  })

  it('releases an interrupted pre-hash lane after reload', async () => {
    useSponsoredOperationStore.getState().beginOperation({
      id: 'interrupted-operation',
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      chainId: 421614,
      accountMode: 'simple',
      manifestVersion: MANIFEST_VERSION,
      action: 'deposit',
    })
    useSponsoredOperationStore.getState().transition(
      'interrupted-operation',
      'requesting-stub'
    )

    render(
      <PerpsAaRuntimeContext value={runtimeValue()}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(
        useSponsoredOperationStore.getState().operations[0]
      ).toMatchObject({
        status: 'failed',
        reason: 'UNKNOWN',
        retryable: true,
      })
    })

    act(() => {
      useSponsoredOperationStore.getState().beginOperation({
        id: 'next-operation',
        ownerAddress: OWNER,
        accountAddress: ACCOUNT,
        chainId: 421614,
        accountMode: 'simple',
        manifestVersion: MANIFEST_VERSION,
        action: 'deposit',
      })
    })
    expect(
      useSponsoredOperationStore.getState().operations.at(-1)?.id
    ).toBe('next-operation')
  })

  it('does not release a live pre-hash operation owned by another tab', async () => {
    const request = vi.fn(async (
      _name: string,
      _options: LockOptions,
      callback: (lock: Lock | null) => Promise<unknown> | unknown
    ) => await callback(null))
    vi.stubGlobal('navigator', {
      locks: { request } as unknown as LockManager,
    })
    useSponsoredOperationStore.getState().beginOperation({
      id: 'other-tab-wallet-approval',
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      chainId: 421614,
      accountMode: 'simple',
      manifestVersion: MANIFEST_VERSION,
      action: 'deposit',
    })
    useSponsoredOperationStore.getState().transition(
      'other-tab-wallet-approval',
      'awaiting-signature'
    )

    render(
      <PerpsAaRuntimeContext value={runtimeValue()}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(request).toHaveBeenCalled()
    })
    expect(
      useSponsoredOperationStore.getState().operations[0]?.status
    ).toBe('awaiting-signature')
    expect(
      useSponsoredOperationStore.getState().activeLanes
    ).not.toEqual({})
  })

  it('recovers any hash-bearing nonterminal record after an old split write', async () => {
    const status = vi.fn(async () => ({
      status: 'not_found' as const,
      transactionHash: null,
    }))
    beginHashOperation({ id: 'split-write-operation' })
    useSponsoredOperationStore.setState((state) => ({
      operations: state.operations.map((operation) => ({
        ...operation,
        status: 'awaiting-signature',
      })),
    }))

    render(
      <PerpsAaRuntimeContext value={runtimeValue({ status })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(status).toHaveBeenCalledWith(USER_OPERATION_HASH)
    })
    expect(
      useSponsoredOperationStore.getState().operations[0]
    ).toMatchObject({
      status: 'receipt-timeout',
      reason: 'BUNDLER_UNAVAILABLE',
      retryable: false,
    })
  })

  it('persists backoff across a recovery runtime remount', async () => {
    const status = vi.fn(async () => ({
      status: 'not_found' as const,
      transactionHash: null,
    }))
    beginHashOperation({ id: 'backed-off-operation' })
    const rendered = render(
      <PerpsAaRuntimeContext value={runtimeValue({ status })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(status).toHaveBeenCalledTimes(1)
    })
    expect(useSponsoredOperationStore.getState().operations[0])
      .toMatchObject({ automaticRecoveryAttemptCount: 1 })

    rendered.rerender(
      <PerpsAaRuntimeContext value={runtimeValue({ status })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0))
    expect(status).toHaveBeenCalledTimes(1)
  })

  it('clears a recovered EIP-3009 authorization after exact inclusion', async () => {
    const initialAuthorization = getOrCreateDepositAuthorization({
      chainId: 421614,
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      token: TOKEN,
      amount: 25_000_000n,
      nowSeconds: 1_000n,
    })
    beginHashOperation({
      id: 'recoverable-deposit',
      action: 'deposit',
      authorizationToken: TOKEN,
      authorizationNonce: initialAuthorization.nonce,
    })
    const receipt = {
      actualGasCost: 1n,
      actualGasUsed: 1n,
      entryPoint:
        '0x3333333333333333333333333333333333333333',
      logs: [],
      nonce: 0n,
      sender: ACCOUNT,
      success: true,
      userOpHash: USER_OPERATION_HASH,
      receipt: {
        transactionHash: TRANSACTION_HASH,
        status: 'success',
        blockNumber: 123n,
        blockHash: INCLUDED_BLOCK_HASH,
      },
    } as ManagedUserOperationReceipt

    render(
      <PerpsAaRuntimeContext value={runtimeValue({
        receipt: vi.fn(async () => receipt),
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(
        useSponsoredOperationStore.getState().operations[0]?.status
      ).toBe('confirmed')
    })

    const nextAuthorization = getOrCreateDepositAuthorization({
      chainId: 421614,
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      token: TOKEN,
      amount: 25_000_000n,
      nowSeconds: 1_000n,
    })
    expect(nextAuthorization.nonce).not.toBe(initialAuthorization.nonce)
  })

  it('retires legacy authorization only at safe confirmation without deleting v2', async () => {
    globalThis.localStorage.setItem(
      LEGACY_AUTHORIZATION_KEY,
      JSON.stringify({ nonce: `0x${'11'.repeat(32)}` })
    )
    const newerAuthorization = getOrCreateDepositAuthorization({
      chainId: 421614,
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      token: TOKEN,
      amount: 25_000_000n,
      nowSeconds: 1_000n,
    })
    beginHashOperation({
      id: 'legacy-recoverable-deposit',
      action: 'deposit',
      authorizationToken: TOKEN,
    })
    const receipt = {
      actualGasCost: 1n,
      actualGasUsed: 1n,
      entryPoint:
        '0x3333333333333333333333333333333333333333',
      logs: [],
      nonce: 0n,
      sender: ACCOUNT,
      success: true,
      userOpHash: USER_OPERATION_HASH,
      receipt: {
        transactionHash: TRANSACTION_HASH,
        status: 'success',
        blockNumber: 123n,
        blockHash: INCLUDED_BLOCK_HASH,
      },
    } as ManagedUserOperationReceipt

    render(
      <PerpsAaRuntimeContext value={runtimeValue({
        receipt: vi.fn(async () => receipt),
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(
        useSponsoredOperationStore.getState().operations[0]?.status
      ).toBe('confirmed')
    })
    expect(globalThis.localStorage.getItem(LEGACY_AUTHORIZATION_KEY)).toBeNull()
    expect(JSON.parse(
      globalThis.localStorage.getItem(CURRENT_AUTHORIZATION_KEY) ?? '{}'
    )).toMatchObject({ nonce: newerAuthorization.nonce })
  })

  it('persists latest-chain inclusion through outages and confirms only at the safe head', async () => {
    let recoveryNow = Date.now()
    vi.spyOn(Date, 'now').mockImplementation(() => recoveryNow)
    const includedReceipt = {
      actualGasCost: 1n,
      actualGasUsed: 1n,
      entryPoint:
        '0x3333333333333333333333333333333333333333',
      logs: [],
      nonce: 0n,
      sender: ACCOUNT,
      success: true,
      userOpHash: USER_OPERATION_HASH,
      receipt: {
        transactionHash: TRANSACTION_HASH,
        status: 'success',
        blockNumber: 123n,
        blockHash: INCLUDED_BLOCK_HASH,
      },
    } as ManagedUserOperationReceipt
    const includedLookup = vi.fn(async () => {
      throw new UserOperationReceiptNotSafeError(includedReceipt)
    })
    const unavailableLookup = vi.fn(async () => {
      throw new Error('Pimlico proxy unavailable')
    })
    const safeLookup = vi.fn(async () => includedReceipt)
    beginHashOperation({ id: 'included-awaiting-safe' })

    const rendered = render(
      <PerpsAaRuntimeContext value={runtimeValue({
        receipt: includedLookup,
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(useSponsoredOperationStore.getState().operations[0])
        .toMatchObject({
          status: 'confirming',
          includedTransactionHash: TRANSACTION_HASH,
          laneReleasedAfterSuccessfulInclusion: true,
        })
    })
    expect(useSponsoredOperationStore.getState().operations[0]?.reason)
      .toBeUndefined()
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({})

    const verifyCanonicalInclusion = vi.fn(async () => 'canonical' as const)
    recoveryNow += SPONSORED_OPERATION_AUTOMATIC_RECOVERY_MAX_DELAY_MS + 10
    rendered.rerender(
      <PerpsAaRuntimeContext value={runtimeValue({
        verifyObservedInclusion: verifyCanonicalInclusion,
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )
    await waitFor(() => {
      expect(verifyCanonicalInclusion).toHaveBeenCalledWith({
        transactionHash: TRANSACTION_HASH,
        blockNumber: 123n,
        blockHash: INCLUDED_BLOCK_HASH,
      })
    })
    expect(useSponsoredOperationStore.getState().operations[0])
      .toMatchObject({
        status: 'confirming',
        includedTransactionHash: TRANSACTION_HASH,
      })

    recoveryNow += SPONSORED_OPERATION_AUTOMATIC_RECOVERY_MAX_DELAY_MS + 10
    rendered.rerender(
      <PerpsAaRuntimeContext value={runtimeValue({
        receipt: unavailableLookup,
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )
    await waitFor(() => {
      expect(unavailableLookup).toHaveBeenCalled()
    })
    expect(useSponsoredOperationStore.getState().operations[0])
      .toMatchObject({
        status: 'confirming',
        includedTransactionHash: TRANSACTION_HASH,
      })

    recoveryNow += SPONSORED_OPERATION_AUTOMATIC_RECOVERY_MAX_DELAY_MS + 10
    rendered.rerender(
      <PerpsAaRuntimeContext value={runtimeValue({
        receipt: safeLookup,
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )
    await waitFor(() => {
      expect(useSponsoredOperationStore.getState().operations[0])
        .toMatchObject({
          status: 'confirmed',
          transactionHash: TRANSACTION_HASH,
          transactionHashVerified: true,
        })
    })
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({})
  })

  it('retracts reorged inclusion without stealing a newer live lane', async () => {
    let recoveryLockSettled = false
    const recoveryLockRequest = vi.fn(async (
      name: string,
      _options: LockOptions,
      callback: (lock: Lock | null) => Promise<unknown> | unknown
    ) => {
      try {
        return await callback({ name, mode: 'exclusive' } as Lock)
      } finally {
        recoveryLockSettled = true
      }
    })
    vi.stubGlobal('navigator', {
      locks: { request: recoveryLockRequest } as unknown as LockManager,
    })
    beginHashOperation({ id: 'reorged-inclusion' })
    expect(useSponsoredOperationStore.getState().recordObservedInclusion(
      'reorged-inclusion',
      {
        transactionHash: TRANSACTION_HASH,
        blockNumber: '123',
        blockHash: INCLUDED_BLOCK_HASH,
      }
    )).toBe(true)
    expect(useSponsoredOperationStore.getState()
      .releaseLaneAfterSuccessfulInclusion(
        'reorged-inclusion',
        {
          transactionHash: TRANSACTION_HASH,
          blockNumber: '123',
          blockHash: INCLUDED_BLOCK_HASH,
          success: true,
        }
      )).toBe(true)
    useSponsoredOperationStore.getState().beginOperation({
      id: 'live-operation',
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      chainId: 421614,
      accountMode: 'simple',
      manifestVersion: MANIFEST_VERSION,
      action: 'place-order',
    })
    createSponsoredOperationSignal('live-operation')
    expect(useSponsoredOperationStore.getState().recordUserOperationHash(
      'live-operation',
      OTHER_USER_OPERATION_HASH
    )).toBe(true)
    const laneHeadKey =
      `${SPONSORED_OPERATION_LANE_HEAD_PREFIX}` +
      `421614:${ACCOUNT.toLowerCase()}:default`
    const liveLaneHead = globalThis.localStorage.getItem(laneHeadKey)
    const verifyObservedInclusion = vi.fn(async () => 'reorged' as const)

    render(
      <PerpsAaRuntimeContext value={runtimeValue({
        verifyObservedInclusion,
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(useSponsoredOperationStore.getState().operations
        .find((operation) => operation.id === 'reorged-inclusion'))
        .toMatchObject({
          status: 'receipt-timeout',
          retryable: false,
          laneReleasedAfterSuccessfulInclusion: true,
        })
    })
    expect(
      useSponsoredOperationStore.getState().operations
        .find((operation) => operation.id === 'reorged-inclusion')
        ?.includedTransactionHash
    ).toBeUndefined()
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'live-operation',
    })
    expect(globalThis.localStorage.getItem(laneHeadKey)).toBe(liveLaneHead)
    expect(recoveryLockRequest.mock.calls.map(([name]) => name)).toContain(
      'plether-perps-user-operation-recovery:' +
      `421614:${ACCOUNT.toLowerCase()}:reorged-inclusion`
    )
    expect(recoveryLockRequest.mock.calls.map(([name]) => name)).not.toContain(
      'plether-perps-user-operation:' +
      `421614:${ACCOUNT.toLowerCase()}:default`
    )
    expect(verifyObservedInclusion).toHaveBeenCalledWith({
      transactionHash: TRANSACTION_HASH,
      blockNumber: 123n,
      blockHash: INCLUDED_BLOCK_HASH,
    })
    await waitFor(() => expect(recoveryLockSettled).toBe(true))

    useSponsoredOperationStore.setState({ operations: [], activeLanes: {} })
    await useSponsoredOperationStore.persist.rehydrate()
    expect(useSponsoredOperationStore.getState().operations
      .find((operation) => operation.id === 'reorged-inclusion'))
      .toMatchObject({
        status: 'receipt-timeout',
        laneReleasedAfterSuccessfulInclusion: true,
      })
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'live-operation',
    })
    releaseSponsoredOperationSignal('live-operation')
  })

  it('retracts a successful inclusion replaced by a failed unsafe receipt', async () => {
    beginHashOperation({ id: 'failed-replacement' })
    const successfulObservation = {
      transactionHash: TRANSACTION_HASH,
      blockNumber: '123',
      blockHash: INCLUDED_BLOCK_HASH,
    }
    expect(useSponsoredOperationStore.getState().recordObservedInclusion(
      'failed-replacement',
      successfulObservation
    )).toBe(true)
    expect(useSponsoredOperationStore.getState()
      .releaseLaneAfterSuccessfulInclusion(
        'failed-replacement',
        { ...successfulObservation, success: true }
      )).toBe(true)

    useSponsoredOperationStore.getState().beginOperation({
      id: 'live-operation',
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      chainId: 421614,
      accountMode: 'simple',
      manifestVersion: MANIFEST_VERSION,
      action: 'place-order',
    })
    createSponsoredOperationSignal('live-operation')
    expect(useSponsoredOperationStore.getState().recordUserOperationHash(
      'live-operation',
      OTHER_USER_OPERATION_HASH
    )).toBe(true)

    const replacementTransactionHash = `0x${'ee'.repeat(32)}` as Hex
    const replacementBlockHash = `0x${'ff'.repeat(32)}` as Hex
    const failedReplacement = {
      actualGasCost: 1n,
      actualGasUsed: 1n,
      entryPoint:
        '0x3333333333333333333333333333333333333333',
      logs: [],
      nonce: 0n,
      sender: ACCOUNT,
      success: false,
      reason: 'execution reverted',
      userOpHash: USER_OPERATION_HASH,
      receipt: {
        transactionHash: replacementTransactionHash,
        status: 'reverted',
        blockNumber: 124n,
        blockHash: replacementBlockHash,
      },
    } as ManagedUserOperationReceipt
    const receipt = vi.fn(async () => {
      throw new UserOperationReceiptNotSafeError(failedReplacement)
    })
    const verifyObservedInclusion = vi.fn(async () => 'reorged' as const)

    render(
      <PerpsAaRuntimeContext value={runtimeValue({
        receipt,
        verifyObservedInclusion,
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(useSponsoredOperationStore.getState().operations
        .find((operation) => operation.id === 'failed-replacement'))
        .toMatchObject({
          status: 'receipt-timeout',
          retryable: false,
          laneReleasedAfterSuccessfulInclusion: true,
        })
    })
    expect(useSponsoredOperationStore.getState().operations
      .find((operation) => operation.id === 'failed-replacement')
      ?.includedTransactionHash).toBeUndefined()
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'live-operation',
    })
    expect(verifyObservedInclusion).toHaveBeenCalledWith({
      transactionHash: TRANSACTION_HASH,
      blockNumber: 123n,
      blockHash: INCLUDED_BLOCK_HASH,
    })
    releaseSponsoredOperationSignal('live-operation')
  })

  it('expires a hash-verified operation only after its safe-chain deadline', async () => {
    beginHashOperation({
      id: 'expired-operation',
      operation: signedOperation(),
    })

    render(
      <PerpsAaRuntimeContext value={runtimeValue({
        nonce: 7n,
        chainTimestamp: 1_001n,
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(
        useSponsoredOperationStore.getState().operations[0]
      ).toMatchObject({
        status: 'expired',
        reason: 'expired',
        retryable: true,
      })
    })
    expect(
      useSponsoredOperationStore.getState().activeLanes
    ).toEqual({})
  })

  it('uses safe-chain expiry proof while Pimlico is unavailable', async () => {
    const getRecoverySnapshot = vi.fn(async () => ({
      blockNumber: 123n,
      blockTimestamp: 1_001n,
      accountNonce: 7n,
      userOperationEvidence: { kind: 'not-located' as const },
    }))
    beginHashOperation({
      id: 'expired-during-pimlico-outage',
      operation: signedOperation(),
    })

    render(
      <PerpsAaRuntimeContext value={runtimeValue({
        receipt: vi.fn(async () => {
          throw new Error('Pimlico proxy unavailable')
        }),
        getRecoverySnapshot,
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(
        useSponsoredOperationStore.getState().operations[0]
      ).toMatchObject({
        status: 'expired',
        reason: 'expired',
        retryable: true,
      })
    })
    expect(getRecoverySnapshot).toHaveBeenCalledWith(
      USER_OPERATION_HASH,
      0n
    )
    expect(
      useSponsoredOperationStore.getState().activeLanes
    ).toEqual({})
  })

  it('keeps the lane locked through the exact sponsorship deadline', async () => {
    beginHashOperation({
      id: 'still-valid-operation',
      operation: signedOperation(),
    })

    render(
      <PerpsAaRuntimeContext value={runtimeValue({
        nonce: 7n,
        chainTimestamp: 1_000n,
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(
        runtimeValue().smartAccount.getUserOperationStatus
      ).toBeDefined()
    })
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0))
    expect(
      useSponsoredOperationStore.getState().operations[0]?.status
    ).toBe('receipt-timeout')
    expect(
      useSponsoredOperationStore.getState().activeLanes
    ).not.toEqual({})
  })

  it('releases an advanced nonce as outcome unknown, never retry-safe', async () => {
    beginHashOperation({
      id: 'nonce-consumed-operation',
      operation: signedOperation(),
    })

    render(
      <PerpsAaRuntimeContext value={runtimeValue({
        nonce: 8n,
        chainTimestamp: 1_001n,
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(
        useSponsoredOperationStore.getState().operations[0]
      ).toMatchObject({
        status: 'outcome-unknown',
        protocolNonceAdvanced: true,
        retryable: false,
      })
    })
    expect(
      useSponsoredOperationStore.getState().activeLanes
    ).toEqual({})
    const resolutionTombstone = globalThis.localStorage.getItem(
      `${SPONSORED_OPERATION_RESOLUTION_PREFIX}` +
      `nonce-consumed-operation:${USER_OPERATION_HASH}:outcome-unknown`
    )
    expect(resolutionTombstone).toContain('"protocolNonceAdvanced":true')
    expect(resolutionTombstone).not.toContain('"signedUserOperation"')
  })

  it('monitors a released outcome without touching a live lane guard', async () => {
    beginHashOperation({
      id: 'released-operation',
      operation: signedOperation(),
    })
    useSponsoredOperationStore.getState().failOperation({
      id: 'released-operation',
      status: 'outcome-unknown',
      retryable: false,
    })
    useSponsoredOperationStore.getState().beginOperation({
      id: 'live-operation',
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      chainId: 421614,
      accountMode: 'simple',
      manifestVersion: MANIFEST_VERSION,
      action: 'place-order',
    })
    createSponsoredOperationSignal('live-operation')
    expect(useSponsoredOperationStore.getState().recordUserOperationHash(
      'live-operation',
      OTHER_USER_OPERATION_HASH
    )).toBe(true)
    const laneHeadKey =
      `${SPONSORED_OPERATION_LANE_HEAD_PREFIX}` +
      `421614:${ACCOUNT.toLowerCase()}:default`
    const liveLaneHead = globalThis.localStorage.getItem(laneHeadKey)
    const status = vi.fn(async () => ({
      status: 'not_found' as const,
      transactionHash: null,
    }))
    const getRecoverySnapshot = vi.fn(async () => ({
      blockNumber: 123n,
      blockTimestamp: 1_000n,
      accountNonce: 7n,
      userOperationEvidence: { kind: 'not-located' as const },
    }))
    render(
      <PerpsAaRuntimeContext value={runtimeValue({
        status,
        getRecoverySnapshot,
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(getRecoverySnapshot).toHaveBeenCalledWith(USER_OPERATION_HASH, 0n)
    })
    expect(status).not.toHaveBeenCalled()
    expect(
      useSponsoredOperationStore.getState().operations
        .find((operation) => operation.id === 'released-operation')
        ?.status
    ).toBe('outcome-unknown')
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'live-operation',
    })
    expect(globalThis.localStorage.getItem(laneHeadKey)).toBe(liveLaneHead)
    releaseSponsoredOperationSignal('live-operation')
  })

  it('upgrades a released unknown outcome only with safe expiry proof', async () => {
    beginHashOperation({
      id: 'released-expired-operation',
      operation: signedOperation(),
    })
    useSponsoredOperationStore.getState().failOperation({
      id: 'released-expired-operation',
      status: 'outcome-unknown',
      retryable: false,
    })

    render(
      <PerpsAaRuntimeContext value={runtimeValue({
        nonce: 7n,
        chainTimestamp: 1_001n,
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(
        useSponsoredOperationStore.getState().operations[0]
      ).toMatchObject({
        status: 'expired',
        reason: 'expired',
        retryable: true,
      })
    })
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({})
  })

  it('uses exact canonical inclusion evidence before nonce recovery', async () => {
    beginHashOperation({ id: 'event-recovered-operation' })

    render(
      <PerpsAaRuntimeContext value={runtimeValue({
        nonce: 8n,
        userOperationEvidence: {
          kind: 'included',
          success: true,
          transactionHash: TRANSACTION_HASH,
          blockNumber: 122n,
        },
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(
        useSponsoredOperationStore.getState().operations[0]
      ).toMatchObject({
        status: 'confirmed',
        transactionHash: TRANSACTION_HASH,
      })
    })
  })

  it('keeps a mismatched signed preimage fail-closed', async () => {
    beginHashOperation({
      id: 'corrupt-metadata-operation',
      operation: signedOperation(),
    })

    render(
      <PerpsAaRuntimeContext value={runtimeValue({
        computedHash: OTHER_USER_OPERATION_HASH,
        nonce: 8n,
        chainTimestamp: 1_001n,
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await new Promise((resolve) => globalThis.setTimeout(resolve, 0))
    expect(
      useSponsoredOperationStore.getState().operations[0]?.status
    ).toBe('receipt-timeout')
    expect(
      useSponsoredOperationStore.getState().activeLanes
    ).not.toEqual({})
  })

  it('queries the nonce key encoded in the verified UserOperation nonce', async () => {
    const nonceKey = 9n
    const nonce = (nonceKey << 64n) | 7n
    const getRecoverySnapshot = vi.fn(async () => ({
      blockNumber: 123n,
      blockTimestamp: 1_001n,
      accountNonce: nonce,
      userOperationEvidence: { kind: 'not-located' as const },
    }))
    beginHashOperation({
      id: 'keyed-nonce-operation',
      operation: signedOperation({ nonce }),
    })

    render(
      <PerpsAaRuntimeContext value={runtimeValue({
        getRecoverySnapshot,
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(getRecoverySnapshot).toHaveBeenCalledWith(
        USER_OPERATION_HASH,
        nonceKey
      )
    })
  })

  it('migrates a stale legacy confirming lock without claiming it expired', async () => {
    beginHashOperation({
      id: 'legacy-operation',
      manifestVersion: LEGACY_AMBIGUOUS_OPERATION_MANIFEST_VERSION,
    })
    useSponsoredOperationStore.setState((state) => ({
      operations: state.operations.map((operation) => ({
        ...operation,
        // This is the state written by the legacy recovery loop after a
        // receipt lookup missed, and is the state seen by the affected user.
        status: 'confirming',
        reason: undefined,
      })),
    }))

    render(
      <PerpsAaRuntimeContext value={runtimeValue({
        manifestVersion: LEGACY_AMBIGUOUS_OPERATION_MANIFEST_VERSION,
        nonce: 11n,
        chainTimestamp: 9_999_999_999n,
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(
        useSponsoredOperationStore.getState().operations[0]?.status
      ).toBe('receipt-timeout')
    })
    expect(
      canForceUnlockLegacySponsoredOperation(
        useSponsoredOperationStore.getState().operations[0]!
      )
    ).toBe(true)
    expect(
      useSponsoredOperationStore.getState().activeLanes
    ).not.toEqual({})
  })

  it('does not reconcile records from another manifest deployment', async () => {
    const status = vi.fn()
    beginHashOperation({
      id: 'other-manifest-operation',
      manifestVersion: 'other-manifest',
    })

    render(
      <PerpsAaRuntimeContext value={runtimeValue({ status })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await new Promise((resolve) => globalThis.setTimeout(resolve, 0))
    expect(status).not.toHaveBeenCalled()
    expect(
      useSponsoredOperationStore.getState().activeLanes
    ).not.toEqual({})
  })

  it('recovers a hash-verified record across a manifest version bump', async () => {
    beginHashOperation({
      id: 'previous-manifest-signed-operation',
      manifestVersion: 'previous-manifest',
      operation: signedOperation(),
    })

    render(
      <PerpsAaRuntimeContext value={runtimeValue({
        nonce: 7n,
        chainTimestamp: 1_001n,
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(
        useSponsoredOperationStore.getState().operations[0]?.status
      ).toBe('expired')
    })
  })
})
