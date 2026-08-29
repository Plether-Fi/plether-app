import { concatHex, numberToHex, type Address, type Hex } from 'viem'
import { describe, expect, it, vi } from 'vitest'
import type { SponsoredOperation } from '../operationStore'
import { PIMLICO_SINGLETON_PAYMASTER_V8 } from '../paymasterValidity'
import { persistManagedUserOperation } from '../persistedUserOperation'
import {
  resolveProtocolOperation,
} from '../protocolOperationResolution'
import type {
  ManagedUserOperation,
  PerpsAaSmartAccountRuntime,
  SponsoredOperationRecoverySnapshot,
} from '../runtimeContext'

const OWNER = '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address
const ENTRY_POINT = '0x3333333333333333333333333333333333333333' as Address
const HASH = `0x${'44'.repeat(32)}` as Hex
const OTHER_HASH = `0x${'55'.repeat(32)}` as Hex
const TRANSACTION_HASH = `0x${'66'.repeat(32)}` as Hex

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

function operation(
  signedUserOperation = signedOperation()
): SponsoredOperation {
  return {
    id: 'operation-1',
    ownerAddress: OWNER,
    accountAddress: ACCOUNT,
    chainId: 421614,
    accountMode: 'simple',
    manifestVersion: 'test-manifest',
    action: 'place-order',
    lane: 'default',
    status: 'receipt-timeout',
    sponsorshipAccepted: true,
    userOperationHash: HASH,
    signedUserOperation: persistManagedUserOperation(signedUserOperation),
    submissionMetadataVersion: 1,
    retryCount: 0,
    createdAt: 1,
    updatedAt: 1,
    statusTimestamps: { 'receipt-timeout': 1 },
  }
}

function runtime(
  snapshot: SponsoredOperationRecoverySnapshot,
  computedHash: Hex = HASH
): PerpsAaSmartAccountRuntime {
  return {
    chainId: 421614,
    ownerAddress: OWNER,
    factoryAddress:
      '0x7777777777777777777777777777777777777777',
    accountVersion: 'permissionless-simple-v0.8',
    accountIndex: '0',
    getRecoverySnapshot: vi.fn(async () => snapshot),
    smartAccount: {
      accountAddress: ACCOUNT,
      entryPoint: ENTRY_POINT,
      prepareUserOperation: vi.fn(),
      signUserOperation: vi.fn(),
      getUserOperationHash: vi.fn(() => computedHash),
      sendUserOperation: vi.fn(),
      getUserOperationStatus: vi.fn(),
      getUserOperationReceipt: vi.fn(),
    },
  }
}

function notLocatedSnapshot(input: {
  accountNonce: bigint
  blockTimestamp: bigint
}): SponsoredOperationRecoverySnapshot {
  return {
    blockNumber: 123n,
    blockTimestamp: input.blockTimestamp,
    accountNonce: input.accountNonce,
    userOperationEvidence: { kind: 'not-located' },
  }
}

describe('resolveProtocolOperation', () => {
  it('expires after the safe deadline when the account nonce equals the operation nonce', async () => {
    const managedRuntime = runtime(notLocatedSnapshot({
      accountNonce: 7n,
      blockTimestamp: 1_001n,
    }))

    await expect(resolveProtocolOperation({
      operation: operation(),
      runtime: managedRuntime,
      userOperationHash: HASH,
    })).resolves.toEqual({ status: 'expired' })
  })

  it('expires after the safe deadline when an earlier nonce gap strands the operation', async () => {
    const managedRuntime = runtime(notLocatedSnapshot({
      accountNonce: 7n,
      blockTimestamp: 1_001n,
    }))

    await expect(resolveProtocolOperation({
      operation: operation(signedOperation({ nonce: 8n })),
      runtime: managedRuntime,
      userOperationHash: HASH,
    })).resolves.toEqual({ status: 'expired' })
  })

  it('keeps the exact deadline and future sponsorship fail-closed', async () => {
    for (const blockTimestamp of [999n, 1_000n]) {
      const managedRuntime = runtime(notLocatedSnapshot({
        accountNonce: 7n,
        blockTimestamp,
      }))

      await expect(resolveProtocolOperation({
        operation: operation(signedOperation({ nonce: 8n })),
        runtime: managedRuntime,
        userOperationHash: HASH,
      })).resolves.toBeUndefined()
    }
  })

  it('keeps nonce advancement outcome-unknown even after sponsorship expiry', async () => {
    const managedRuntime = runtime(notLocatedSnapshot({
      accountNonce: 8n,
      blockTimestamp: 1_001n,
    }))

    await expect(resolveProtocolOperation({
      operation: operation(),
      runtime: managedRuntime,
      userOperationHash: HASH,
    })).resolves.toEqual({
      status: 'outcome-unknown',
      protocolNonceAdvanced: true,
    })
  })

  it.each([
    { success: true, status: 'confirmed' as const },
    { success: false, status: 'execution-reverted' as const },
  ])('lets exact safe inclusion resolve as $status first', async ({
    success,
    status,
  }) => {
    const managedRuntime = runtime({
      blockNumber: 123n,
      blockTimestamp: 1_001n,
      accountNonce: 8n,
      userOperationEvidence: {
        kind: 'included',
        success,
        transactionHash: TRANSACTION_HASH,
        blockNumber: 122n,
      },
    })

    await expect(resolveProtocolOperation({
      operation: operation(),
      runtime: managedRuntime,
      userOperationHash: HASH,
    })).resolves.toEqual({
      status,
      transactionHash: TRANSACTION_HASH,
    })
  })

  it('waits when the exact event range has not reached the safe head', async () => {
    const managedRuntime = runtime({
      blockNumber: 123n,
      blockTimestamp: 1_001n,
      accountNonce: 7n,
      userOperationEvidence: { kind: 'not-safe-yet' },
    })

    await expect(resolveProtocolOperation({
      operation: operation(),
      runtime: managedRuntime,
      userOperationHash: HASH,
    })).resolves.toBeUndefined()
  })

  it('uses the verified operation nonce key for the atomic snapshot', async () => {
    const nonceKey = 9n
    const nonce = (nonceKey << 64n) | 8n
    const managedRuntime = runtime(notLocatedSnapshot({
      accountNonce: nonce,
      blockTimestamp: 1_000n,
    }))

    await resolveProtocolOperation({
      operation: operation(signedOperation({ nonce })),
      runtime: managedRuntime,
      userOperationHash: HASH,
    })

    expect(managedRuntime.getRecoverySnapshot).toHaveBeenCalledWith(
      HASH,
      nonceKey
    )
  })

  it('fails closed when the persisted preimage does not match the recorded hash', async () => {
    const managedRuntime = runtime(notLocatedSnapshot({
      accountNonce: 7n,
      blockTimestamp: 1_001n,
    }), OTHER_HASH)

    await expect(resolveProtocolOperation({
      operation: operation(),
      runtime: managedRuntime,
      userOperationHash: HASH,
    })).resolves.toBeUndefined()
  })

  it('fails closed when the safe snapshot is unavailable', async () => {
    const managedRuntime = runtime(notLocatedSnapshot({
      accountNonce: 7n,
      blockTimestamp: 1_001n,
    }))
    managedRuntime.getRecoverySnapshot = vi.fn(async () => {
      throw new Error('RPC unavailable')
    })

    await expect(resolveProtocolOperation({
      operation: operation(),
      runtime: managedRuntime,
      userOperationHash: HASH,
    })).resolves.toBeUndefined()
  })
})
