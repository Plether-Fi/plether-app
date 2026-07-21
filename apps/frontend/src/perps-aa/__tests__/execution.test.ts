import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import type { Address, Hex } from 'viem'
import { executeSponsoredPerpsAction } from '../execution'
import type { PerpsAaDeploymentManifest } from '../manifest'
import {
  cancelSponsoredOperationRequest,
  useSponsoredOperationStore,
} from '../operationStore'
import type {
  ManagedUserOperation,
  ManagedUserOperationReceipt,
  PerpsAaSmartAccountRuntime,
} from '../runtimeContext'

const authorizationMocks = vi.hoisted(() => ({
  clearDepositAuthorization: vi.fn(),
}))

vi.mock('../authorizationStore', () => ({
  clearDepositAuthorization: authorizationMocks.clearDepositAuthorization,
}))

const OWNER = '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address
const ENTRY_POINT =
  '0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108' as Address
const FACTORY =
  '0x13E9ed32155810FDbd067D4522C492D6f68E5944' as Address
const TARGET = '0x3333333333333333333333333333333333333333' as Address
const USER_OPERATION_HASH = `0x${'44'.repeat(32)}` as Hex
const OTHER_USER_OPERATION_HASH = `0x${'55'.repeat(32)}` as Hex
const TRANSACTION_HASH = `0x${'66'.repeat(32)}` as Hex

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
    paymaster: TARGET,
    paymasterData: '0x90',
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
  sendUserOperation?: PerpsAaSmartAccountRuntime['smartAccount']['sendUserOperation']
  getUserOperationStatus?: PerpsAaSmartAccountRuntime['smartAccount']['getUserOperationStatus']
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
      getUserOperationHash: vi.fn(() => USER_OPERATION_HASH),
      sendUserOperation: input.sendUserOperation ??
        vi.fn(async () => USER_OPERATION_HASH),
      getUserOperationStatus: input.getUserOperationStatus ??
        vi.fn(async () => ({
          status: 'included',
          transactionHash: TRANSACTION_HASH,
        })),
      getUserOperationReceipt: vi.fn(async () => receipt()),
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
  })

  it('persists the locally computed hash before Pimlico submission', async () => {
    const sendUserOperation = vi.fn(async () => {
      expect(
        useSponsoredOperationStore.getState().operations[0]
          ?.userOperationHash
      ).toBe(USER_OPERATION_HASH)
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
      transactionHash: TRANSACTION_HASH,
    })
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
