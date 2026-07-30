import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import {
  concatHex,
  numberToHex,
  type Address,
  type Hex,
} from 'viem'
import { executeSponsoredPerpsAction } from '../execution'
import type { PerpsAaDeploymentManifest } from '../manifest'
import {
  cancelSponsoredOperationRequest,
  forceUnlockLegacySponsoredOperation,
  restoreSponsoredOperationLane,
  SPONSORED_OPERATION_JOURNAL_PREFIX,
  SPONSORED_OPERATION_LANE_HEAD_PREFIX,
  SPONSORED_OPERATION_RESOLUTION_PREFIX,
  SPONSORED_OPERATION_STORAGE_NAME,
  SponsoredOperationLockedError,
  useSponsoredOperationStore,
} from '../operationStore'
import type {
  ManagedUserOperation,
  ManagedUserOperationReceipt,
  PerpsAaSmartAccountRuntime,
} from '../runtimeContext'
import { UserOperationReceiptNotSafeError } from '../runtimeContext'

const authorizationMocks = vi.hoisted(() => ({
  clearDepositAuthorization: vi.fn(),
}))

const analyticsMocks = vi.hoisted(() => ({
  trackPerpsSponsoredOperation: vi.fn(),
}))

vi.mock('../authorizationStore', () => ({
  clearDepositAuthorization: authorizationMocks.clearDepositAuthorization,
}))

vi.mock('../../analytics/perps', async (importOriginal) => {
  const actual = await importOriginal<typeof import('../../analytics/perps')>()
  return {
    ...actual,
    trackPerpsSponsoredOperation: analyticsMocks.trackPerpsSponsoredOperation,
  }
})

const OWNER = '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address
const ENTRY_POINT =
  '0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108' as Address
const FACTORY =
  '0x13E9ed32155810FDbd067D4522C492D6f68E5944' as Address
const PAYMASTER =
  '0x888888888888Ec68A58AB8094Cc1AD20Ba3D2402' as Address
const TARGET = '0x3333333333333333333333333333333333333333' as Address
const USER_OPERATION_HASH = `0x${'44'.repeat(32)}` as Hex
const OTHER_USER_OPERATION_HASH = `0x${'55'.repeat(32)}` as Hex
const TRANSACTION_HASH = `0x${'66'.repeat(32)}` as Hex
const SPONSORSHIP_VALID_UNTIL = 1_784_869_349n

function paymasterData(): Hex {
  return concatHex([
    '0x01',
    numberToHex(SPONSORSHIP_VALID_UNTIL, { size: 6 }),
    numberToHex(0n, { size: 6 }),
    `0x${'11'.repeat(65)}`,
  ])
}

function manifest(
  input: Partial<PerpsAaDeploymentManifest> = {}
): PerpsAaDeploymentManifest {
  return {
    version: 'perps-aa-arbitrum-sepolia-v1',
    chainId: 421614,
    entryPoint: ENTRY_POINT,
    entryPointVersion: '0.8',
    pimlicoRpcUrl: '/api/perps/v1/aa/pimlico',
    smartAccountMode: 'simple',
    smartAccountVersion: 'permissionless-simple-v0.8',
    smartAccountIndex: '0',
    smartAccountFactory: FACTORY,
    usdc: TARGET,
    usdcSupportsEip3009: false,
    usdcEip712Name: null,
    usdcEip712Version: null,
    marginClearinghouse: TARGET,
    cfdEngine: TARGET,
    orderRouter: TARGET,
    userOperationExplorerUrlTemplate:
      'https://example.com/user-operation/{userOperationHash}',
    transactionExplorerUrlTemplate:
      'https://example.com/transaction/{transactionHash}',
    testnetFaucet: null,
    sponsorshipEnabled: true,
    ...input,
  }
}

function operation(): ManagedUserOperation {
  return {
    sender: ACCOUNT,
    nonce: 0n,
    factory: FACTORY,
    factoryData: '0x1234',
    callData: '0x5678',
    callGasLimit: 1n,
    verificationGasLimit: 2n,
    preVerificationGas: 3n,
    maxFeePerGas: 4n,
    maxPriorityFeePerGas: 5n,
    paymaster: PAYMASTER,
    paymasterData: paymasterData(),
    paymasterVerificationGasLimit: 6n,
    paymasterPostOpGasLimit: 7n,
    signature: '0xdeadbeef',
  }
}

function receipt(): ManagedUserOperationReceipt {
  return {
    actualGasCost: 1n,
    actualGasUsed: 1n,
    entryPoint: ENTRY_POINT,
    logs: [],
    nonce: 0n,
    sender: ACCOUNT,
    success: true,
    userOpHash: USER_OPERATION_HASH,
    receipt: {
      transactionHash: TRANSACTION_HASH,
      status: 'success',
    },
  } as ManagedUserOperationReceipt
}

function runtime(input: {
  signUserOperation?: PerpsAaSmartAccountRuntime['smartAccount']['signUserOperation']
  getUserOperationHash?: PerpsAaSmartAccountRuntime['smartAccount']['getUserOperationHash']
  sendUserOperation?: PerpsAaSmartAccountRuntime['smartAccount']['sendUserOperation']
  getUserOperationStatus?: PerpsAaSmartAccountRuntime['smartAccount']['getUserOperationStatus']
  getUserOperationReceipt?: PerpsAaSmartAccountRuntime['smartAccount']['getUserOperationReceipt']
} = {}): PerpsAaSmartAccountRuntime {
  return {
    chainId: 421614,
    ownerAddress: OWNER,
    factoryAddress: FACTORY,
    accountVersion: 'permissionless-simple-v0.8',
    accountIndex: '0',
    smartAccount: {
      accountAddress: ACCOUNT,
      entryPoint: ENTRY_POINT,
      prepareUserOperation: vi.fn(async () => operation()),
      signUserOperation: input.signUserOperation ??
        vi.fn(async (value) => value),
      getUserOperationHash: input.getUserOperationHash ??
        vi.fn(() => USER_OPERATION_HASH),
      sendUserOperation: input.sendUserOperation ??
        vi.fn(async () => USER_OPERATION_HASH),
      getUserOperationStatus: input.getUserOperationStatus ??
        vi.fn(async () => ({
          status: 'included',
          transactionHash: TRANSACTION_HASH,
        })),
      getUserOperationReceipt: input.getUserOperationReceipt ??
        vi.fn(async () => receipt()),
    },
  }
}

const action = {
  kind: 'deposit' as const,
  account: ACCOUNT,
  calls: [{ to: TARGET, value: 0n, data: '0x1234' as Hex }],
}

describe('executeSponsoredPerpsAction', () => {
  beforeEach(() => {
    authorizationMocks.clearDepositAuthorization.mockReset()
    analyticsMocks.trackPerpsSponsoredOperation.mockReset()
    globalThis.localStorage.clear()
    vi.stubGlobal('navigator', {
      locks: {
        request: vi.fn(async (
          name: string,
          _options: LockOptions,
          callback: (lock: Lock | null) => Promise<unknown> | unknown
        ) => await callback({ name, mode: 'exclusive' } as Lock)),
      } as unknown as LockManager,
    })
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
  })

  afterEach(() => {
    vi.restoreAllMocks()
    vi.unstubAllGlobals()
  })

  it('fails closed when the remote manifest kill switch is off', async () => {
    await expect(executeSponsoredPerpsAction({
      manifest: manifest({ sponsorshipEnabled: false }),
      ownerAddress: OWNER,
      action,
      runtime: runtime(),
    })).rejects.toMatchObject({
      reason: 'SPONSOR_UNAVAILABLE',
      retryable: true,
    })
    expect(analyticsMocks.trackPerpsSponsoredOperation).toHaveBeenCalledWith(
      'preflight_failed',
      expect.objectContaining({
        action_kind: 'deposit',
        reason_code: 'SPONSORSHIP_DISABLED',
        terminal_outcome: 'preflight_failed',
      })
    )
  })

  it('persists the locally computed hash before Pimlico submission', async () => {
    const sendUserOperation = vi.fn(async () => {
      const pendingOperation =
        useSponsoredOperationStore.getState().operations[0]!
      expect(pendingOperation).toMatchObject({
        status: 'submitting',
        userOperationHash: USER_OPERATION_HASH,
        signedUserOperation: {
          nonce: '0',
          paymaster: PAYMASTER,
          paymasterData: paymasterData(),
        },
      })
      expect(JSON.parse(globalThis.localStorage.getItem(
        `${SPONSORED_OPERATION_JOURNAL_PREFIX}${pendingOperation.id}`
      )!)).toMatchObject({
        version: 1,
        operation: {
          id: pendingOperation.id,
          userOperationHash: USER_OPERATION_HASH,
          submissionMetadataVersion: 1,
          signedUserOperation: {
            nonce: '0',
            paymaster: PAYMASTER,
            paymasterData: paymasterData(),
          },
        },
      })
      return USER_OPERATION_HASH
    })

    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: runtime({ sendUserOperation }),
    })).resolves.toMatchObject({
      userOperationHash: USER_OPERATION_HASH,
      transactionHash: TRANSACTION_HASH,
    })

    expect(sendUserOperation).toHaveBeenCalledTimes(1)
    expect(useSponsoredOperationStore.getState().operations[0]).toMatchObject({
      status: 'confirmed',
      userOperationHash: USER_OPERATION_HASH,
      signedUserOperation: {
        nonce: '0',
        paymaster: PAYMASTER,
        paymasterData: paymasterData(),
      },
      submissionMetadataVersion: 1,
      transactionHash: TRANSACTION_HASH,
    })
  })

  it('reports canonical inclusion before safe confirmation without unlocking the lane', async () => {
    vi.useFakeTimers()
    try {
      const includedReceipt = receipt()
      const getUserOperationReceipt = vi.fn()
        .mockRejectedValueOnce(
          new UserOperationReceiptNotSafeError(includedReceipt)
        )
        .mockResolvedValue(includedReceipt)
      const onIncluded = vi.fn()

      const execution = executeSponsoredPerpsAction({
        manifest: manifest(),
        ownerAddress: OWNER,
        action,
        runtime: runtime({ getUserOperationReceipt }),
        onIncluded,
      })

      await vi.waitFor(() => {
        expect(onIncluded).toHaveBeenCalledOnce()
      })
      expect(onIncluded).toHaveBeenCalledWith({
        userOperationHash: USER_OPERATION_HASH,
        receipt: includedReceipt,
        transactionHash: TRANSACTION_HASH,
      })
      const includedOperation =
        useSponsoredOperationStore.getState().operations[0]
      expect(includedOperation).toMatchObject({ status: 'confirming' })
      expect(includedOperation?.transactionHash).toBeUndefined()
      expect(includedOperation?.transactionHashVerified).toBeUndefined()

      await vi.advanceTimersByTimeAsync(1_500)
      await expect(execution).resolves.toMatchObject({
        userOperationHash: USER_OPERATION_HASH,
        transactionHash: TRANSACTION_HASH,
      })
      expect(onIncluded).toHaveBeenCalledOnce()
      expect(useSponsoredOperationStore.getState().operations[0]).toMatchObject({
        status: 'confirmed',
        transactionHash: TRANSACTION_HASH,
        transactionHashVerified: true,
      })
    } finally {
      vi.useRealTimers()
    }
  })

  it('keeps a hash mismatch in reconciliation instead of retrying', async () => {
    const getUserOperationStatus = vi.fn()
    const managedRuntime = runtime({
      sendUserOperation: vi.fn(async () => OTHER_USER_OPERATION_HASH),
      getUserOperationStatus,
    })

    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: managedRuntime,
    })).rejects.toMatchObject({
      terminalStatus: 'receipt-timeout',
      retryable: false,
    })

    expect(managedRuntime.smartAccount.sendUserOperation).toHaveBeenCalledTimes(1)
    expect(getUserOperationStatus).not.toHaveBeenCalled()
    expect(useSponsoredOperationStore.getState().operations[0]).toMatchObject({
      status: 'receipt-timeout',
      userOperationHash: USER_OPERATION_HASH,
      retryable: false,
    })
  })

  it('blocks a new send from the direct lane head when snapshot enumeration misses', async () => {
    useSponsoredOperationStore.getState().beginOperation({
      id: 'existing-operation',
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      chainId: 421614,
      accountMode: 'simple',
      manifestVersion: manifest().version,
      action: 'deposit',
    })
    expect(useSponsoredOperationStore.getState().recordUserOperationHash(
      'existing-operation',
      USER_OPERATION_HASH,
      { signedUserOperation: operation() }
    )).toBe(true)
    useSponsoredOperationStore.getState().failOperation({
      id: 'existing-operation',
      status: 'receipt-timeout',
      reason: 'BUNDLER_UNAVAILABLE',
      retryable: false,
    })

    // Model a shared-snapshot last-writer loss plus a live key-enumeration
    // race. Direct reads by lane and operation ID must still find the record.
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
    globalThis.localStorage.setItem(
      SPONSORED_OPERATION_STORAGE_NAME,
      JSON.stringify({
        state: { operations: [], activeLanes: {} },
        version: 1,
      })
    )
    vi.spyOn(globalThis.localStorage, 'key').mockReturnValue(null)
    const managedRuntime = runtime()

    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: managedRuntime,
    })).rejects.toBeInstanceOf(SponsoredOperationLockedError)

    expect(
      managedRuntime.smartAccount.prepareUserOperation
    ).not.toHaveBeenCalled()
    expect(
      managedRuntime.smartAccount.sendUserOperation
    ).not.toHaveBeenCalled()
    expect(useSponsoredOperationStore.getState().operations[0])
      .toMatchObject({
        id: 'existing-operation',
        userOperationHash: USER_OPERATION_HASH,
        status: 'receipt-timeout',
      })
  })

  it('rechecks legacy lane state after a long wallet signature', async () => {
    let resolveSignature:
      ((value: ManagedUserOperation) => void) | undefined
    const signature = new Promise<ManagedUserOperation>((resolve) => {
      resolveSignature = resolve
    })
    const signUserOperation = vi.fn(async () => await signature)
    const sendUserOperation = vi.fn(async () => USER_OPERATION_HASH)
    const execution = executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: runtime({
        signUserOperation,
        sendUserOperation,
      }),
    })
    await vi.waitFor(() => {
      expect(signUserOperation).toHaveBeenCalledTimes(1)
    })

    const liveOperation =
      useSponsoredOperationStore.getState().operations[0]!
    const legacyOperation = {
      ...liveOperation,
      id: 'legacy-operation',
      status: 'dropped' as const,
      userOperationHash: OTHER_USER_OPERATION_HASH,
      createdAt: liveOperation.createdAt - 1,
      updatedAt: liveOperation.updatedAt - 1,
    }
    globalThis.localStorage.setItem(
      SPONSORED_OPERATION_STORAGE_NAME,
      JSON.stringify({
        state: {
          operations: [legacyOperation],
          activeLanes: {},
        },
        version: 0,
      })
    )
    await useSponsoredOperationStore.persist.rehydrate()
    expect(globalThis.localStorage.getItem(
      `${SPONSORED_OPERATION_JOURNAL_PREFIX}legacy-operation`
    )).toBeNull()

    resolveSignature?.(operation())

    await expect(execution).rejects.toMatchObject({
      reason: 'OPERATION_STORE_UNAVAILABLE',
    })
    expect(sendUserOperation).not.toHaveBeenCalled()
    expect(globalThis.localStorage.getItem(
      `${SPONSORED_OPERATION_LANE_HEAD_PREFIX}` +
      `421614:${ACCOUNT.toLowerCase()}:default`
    )).toContain('legacy-operation')
    expect(useSponsoredOperationStore.getState().operations
      .find((item) => item.id === 'legacy-operation')).toMatchObject({
        status: 'receipt-timeout',
        userOperationHash: OTHER_USER_OPERATION_HASH,
      })
  })

  it('fails the persistence barrier when legacy state lands after final restore', async () => {
    const sendUserOperation = vi.fn(async () => USER_OPERATION_HASH)
    const getUserOperationHash = vi.fn(() => {
      const liveOperation =
        useSponsoredOperationStore.getState().operations[0]!
      const legacyOperation = {
        ...liveOperation,
        id: 'late-legacy-operation',
        status: 'dropped' as const,
        userOperationHash: OTHER_USER_OPERATION_HASH,
        createdAt: liveOperation.createdAt - 1,
        updatedAt: liveOperation.updatedAt - 1,
      }
      // getUserOperationHash runs synchronously after the final locked restore
      // and immediately before recordUserOperationHash's durable barrier.
      globalThis.localStorage.setItem(
        SPONSORED_OPERATION_STORAGE_NAME,
        JSON.stringify({
          state: {
            operations: [legacyOperation],
            activeLanes: {},
          },
          version: 0,
        })
      )
      return USER_OPERATION_HASH
    })

    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: runtime({
        getUserOperationHash,
        sendUserOperation,
      }),
    })).rejects.toMatchObject({
      reason: 'OPERATION_STORE_UNAVAILABLE',
    })

    expect(sendUserOperation).not.toHaveBeenCalled()
    expect(globalThis.localStorage.getItem(
      SPONSORED_OPERATION_STORAGE_NAME
    )).toContain('late-legacy-operation')
    expect(globalThis.localStorage.getItem(
      `${SPONSORED_OPERATION_JOURNAL_PREFIX}late-legacy-operation`
    )).toBeNull()
  })

  it('submits after an exact legacy identity is force-released', async () => {
    useSponsoredOperationStore.getState().beginOperation({
      id: 'seed-operation',
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      chainId: 421614,
      accountMode: 'simple',
      manifestVersion: manifest().version,
      action: 'deposit',
    })
    const legacyOperation = {
      ...useSponsoredOperationStore.getState().operations[0]!,
      id: 'legacy-operation',
      status: 'dropped' as const,
      userOperationHash: OTHER_USER_OPERATION_HASH,
    }
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
    globalThis.localStorage.clear()
    globalThis.localStorage.setItem(
      SPONSORED_OPERATION_STORAGE_NAME,
      JSON.stringify({
        state: {
          operations: [legacyOperation],
          activeLanes: {},
        },
        version: 0,
      })
    )
    restoreSponsoredOperationLane({
      chainId: 421614,
      accountAddress: ACCOUNT,
      lane: 'default',
    })
    expect(await forceUnlockLegacySponsoredOperation(
      'legacy-operation'
    )).toBe(true)

    const sendUserOperation = vi.fn(async () => USER_OPERATION_HASH)
    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: runtime({ sendUserOperation }),
    })).resolves.toMatchObject({
      userOperationHash: USER_OPERATION_HASH,
    })

    expect(sendUserOperation).toHaveBeenCalledTimes(1)
    expect(globalThis.localStorage.getItem(
      SPONSORED_OPERATION_STORAGE_NAME
    )).toContain('legacy-operation')
    expect(globalThis.localStorage.getItem(
      `${SPONSORED_OPERATION_JOURNAL_PREFIX}legacy-operation`
    )).toContain('outcome-unknown')
    expect(globalThis.localStorage.getItem(
      `${SPONSORED_OPERATION_RESOLUTION_PREFIX}` +
      `legacy-operation:${OTHER_USER_OPERATION_HASH}:outcome-unknown`
    )).toContain('outcome-unknown')
  })

  it('never overwrites legacy evidence that races the shared-state read', async () => {
    const originalGetItem =
      globalThis.localStorage.getItem.bind(globalThis.localStorage)
    const originalSetItem =
      globalThis.localStorage.setItem.bind(globalThis.localStorage)
    let armLegacyWrite = false
    let injectedLegacyState = false
    vi.spyOn(globalThis.localStorage, 'getItem').mockImplementation((key) => {
      const staleValue = originalGetItem(key)
      if (
        armLegacyWrite &&
        !injectedLegacyState &&
        key === SPONSORED_OPERATION_STORAGE_NAME
      ) {
        injectedLegacyState = true
        const liveOperation =
          useSponsoredOperationStore.getState().operations[0]!
        originalSetItem(
          SPONSORED_OPERATION_STORAGE_NAME,
          JSON.stringify({
            state: {
              operations: [{
                ...liveOperation,
                id: 'rmw-legacy-operation',
                status: 'dropped',
                userOperationHash: OTHER_USER_OPERATION_HASH,
                createdAt: liveOperation.createdAt - 1,
                updatedAt: liveOperation.updatedAt - 1,
              }],
              activeLanes: {},
            },
            version: 0,
          })
        )
      }
      return staleValue
    })
    const sendUserOperation = vi.fn(async () => USER_OPERATION_HASH)

    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: runtime({
        getUserOperationHash: vi.fn(() => {
          armLegacyWrite = true
          return USER_OPERATION_HASH
        }),
        sendUserOperation,
      }),
    })).rejects.toMatchObject({
      reason: 'OPERATION_STORE_UNAVAILABLE',
    })

    expect(injectedLegacyState).toBe(true)
    expect(sendUserOperation).not.toHaveBeenCalled()
    expect(originalGetItem(SPONSORED_OPERATION_STORAGE_NAME))
      .toContain('rmw-legacy-operation')
  })

  it('revalidates durable lane evidence immediately before network send', async () => {
    const sendUserOperation = vi.fn(async () => USER_OPERATION_HASH)
    let injectedLegacyState = false

    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: runtime({ sendUserOperation }),
      onStatus: (status) => {
        if (status !== 'submitting' || injectedLegacyState) return
        injectedLegacyState = true
        const liveOperation =
          useSponsoredOperationStore.getState().operations[0]!
        globalThis.localStorage.setItem(
          SPONSORED_OPERATION_STORAGE_NAME,
          JSON.stringify({
            state: {
              operations: [{
                ...liveOperation,
                id: 'last-moment-legacy-operation',
                status: 'dropped',
                userOperationHash: OTHER_USER_OPERATION_HASH,
                createdAt: liveOperation.createdAt - 1,
                updatedAt: liveOperation.updatedAt - 1,
              }],
              activeLanes: {},
            },
            version: 0,
          })
        )
      },
    })).rejects.toMatchObject({
      reason: 'OPERATION_STORE_UNAVAILABLE',
    })

    expect(sendUserOperation).not.toHaveBeenCalled()
    expect(globalThis.localStorage.getItem(
      SPONSORED_OPERATION_STORAGE_NAME
    )).toContain('last-moment-legacy-operation')
  })

  it('does not submit when the local request is cancelled during wallet signing', async () => {
    let resolveSignature:
      ((value: ManagedUserOperation) => void) | undefined
    const signature = new Promise<ManagedUserOperation>((resolve) => {
      resolveSignature = resolve
    })
    const sendUserOperation = vi.fn(async () => USER_OPERATION_HASH)
    const managedRuntime = runtime({
      signUserOperation: vi.fn(async () => await signature),
      sendUserOperation,
    })

    const execution = executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: managedRuntime,
    })

    await vi.waitFor(() => {
      expect(
        useSponsoredOperationStore.getState().operations[0]?.status
      ).toBe('awaiting-signature')
    })
    const operationId =
      useSponsoredOperationStore.getState().operations[0]?.id
    expect(operationId).toBeDefined()
    cancelSponsoredOperationRequest(operationId!)
    resolveSignature?.(operation())

    await expect(execution).rejects.toMatchObject({
      name: 'AbortError',
    })
    expect(sendUserOperation).not.toHaveBeenCalled()
    expect(useSponsoredOperationStore.getState().operations[0]).toMatchObject({
      status: 'cancelled',
    })
    expect(
      useSponsoredOperationStore.getState().operations[0]
        ?.userOperationHash
    ).toBeUndefined()
  })

  it('does not submit unless the signed hash and preimage were accepted by the store', async () => {
    const sendUserOperation = vi.fn(async () => USER_OPERATION_HASH)
    const managedRuntime = runtime({
      signUserOperation: vi.fn(async (value) => {
        const operationId =
          useSponsoredOperationStore.getState().operations[0]?.id
        expect(operationId).toBeDefined()
        useSponsoredOperationStore.getState().failOperation({
          id: operationId!,
          reason: 'UNKNOWN',
          retryable: false,
        })
        return value
      }),
      sendUserOperation,
    })

    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: managedRuntime,
    })).rejects.toMatchObject({
      reason: 'OPERATION_STORE_UNAVAILABLE',
    })

    expect(sendUserOperation).not.toHaveBeenCalled()
    expect(
      useSponsoredOperationStore.getState().operations[0]?.userOperationHash
    ).toBeUndefined()
  })

  it('keeps an ambiguous Pimlico submission non-retryable', async () => {
    const sendUserOperation = vi.fn(async () => {
      throw new Error('connection closed before response')
    })

    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: runtime({ sendUserOperation }),
    })).rejects.toMatchObject({
      retryable: false,
      terminalStatus: 'receipt-timeout',
    })

    expect(sendUserOperation).toHaveBeenCalledTimes(1)
    expect(useSponsoredOperationStore.getState().operations[0]).toMatchObject({
      status: 'receipt-timeout',
      userOperationHash: USER_OPERATION_HASH,
      retryable: false,
    })
  })

  it('does not sign or submit sponsorship without a recoverable deadline', async () => {
    const managedRuntime = runtime()
    managedRuntime.smartAccount.prepareUserOperation = vi.fn(async () => ({
      ...operation(),
      paymasterData: '0x90',
    }))

    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: managedRuntime,
    })).rejects.toMatchObject({
      reason: 'SPONSOR_UNAVAILABLE',
      retryable: false,
    })

    expect(
      managedRuntime.smartAccount.signUserOperation
    ).not.toHaveBeenCalled()
    expect(
      managedRuntime.smartAccount.sendUserOperation
    ).not.toHaveBeenCalled()
    expect(useSponsoredOperationStore.getState().operations[0])
      .toMatchObject({
        status: 'failed',
      })
    expect(
      useSponsoredOperationStore.getState().operations[0]?.userOperationHash
    ).toBeUndefined()
  })

  it('keeps a confirmed operation terminal if local authorization cleanup fails', async () => {
    authorizationMocks.clearDepositAuthorization.mockImplementation(() => {
      throw new Error('local storage unavailable')
    })

    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: runtime(),
      authorizationTokenToClearOnConfirmation: TARGET,
    })).resolves.toMatchObject({
      userOperationHash: USER_OPERATION_HASH,
      transactionHash: TRANSACTION_HASH,
    })

    expect(
      authorizationMocks.clearDepositAuthorization
    ).toHaveBeenCalledTimes(1)
    expect(useSponsoredOperationStore.getState().operations[0]).toMatchObject({
      status: 'confirmed',
      userOperationHash: USER_OPERATION_HASH,
      transactionHash: TRANSACTION_HASH,
    })
  })
})
