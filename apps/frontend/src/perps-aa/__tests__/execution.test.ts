import { reportAttemptStage } from '../attemptDiagnostics'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import {
  concatHex,
  TimeoutError,
  numberToHex,
  type Address,
  type Hex,
} from 'viem'
import { resumeSponsoredPerpsAction, executeSponsoredPerpsAction } from '../execution'
import type {
  PerpsAaDeploymentManifestV1,
  PerpsAaDeploymentManifestV2,
} from '../manifest'
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
import { SponsorRequestError } from '../errors'
import { resolveProtocolOperation } from '../protocolOperationResolution'
import {
  PLETHER_PAYMASTER_POLICY_ID,
  PLETHER_PAYMASTER_POST_OP_GAS_LIMIT,
  PLETHER_PAYMASTER_VERIFICATION_GAS_LIMIT,
  PLETHER_SIMPLE_ACCOUNT_PROXY_CODE_HASH,
} from '../paymasterValidity'

vi.mock('../attemptDiagnostics', () => ({ reportAttemptStage: vi.fn() }))

const authorizationMocks = vi.hoisted(() => ({
  clearDepositAuthorization: vi.fn(),
  clearLegacyDepositAuthorization: vi.fn(),
}))

const analyticsMocks = vi.hoisted(() => ({
  trackPerpsSponsoredOperation: vi.fn(),
}))

vi.mock('../authorizationStore', () => ({
  clearDepositAuthorization: authorizationMocks.clearDepositAuthorization,
  clearLegacyDepositAuthorization:
    authorizationMocks.clearLegacyDepositAuthorization,
}))

vi.mock('../readiness', () => ({
  refreshReadiness: vi.fn(async () => {}), currentReadiness: () => undefined,
  readinessBlocker: () => undefined, readinessMessage: () => '',
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
const PLETHER_PAYMASTER =
  '0x1234567890123456789012345678901234567890' as Address
const TARGET = '0x3333333333333333333333333333333333333333' as Address
const USER_OPERATION_HASH = `0x${'44'.repeat(32)}` as Hex
const OTHER_USER_OPERATION_HASH = `0x${'55'.repeat(32)}` as Hex
const TRANSACTION_HASH = `0x${'66'.repeat(32)}` as Hex
const INCLUDED_BLOCK_HASH = `0x${'77'.repeat(32)}` as Hex
const REPLACEMENT_TRANSACTION_HASH = `0x${'88'.repeat(32)}` as Hex
const REPLACEMENT_BLOCK_HASH = `0x${'99'.repeat(32)}` as Hex
const AUTHORIZATION_NONCE = `0x${'ab'.repeat(32)}` as Hex
// Successful execution fixtures must remain valid at the test's wall clock.
const SPONSORSHIP_VALID_UNTIL = BigInt(Math.floor(Date.now() / 1000) + 3600)
const ORDER_CLIENT_ID = `0x${'cd'.repeat(32)}` as Hex

const orderRequestV3 = {
  version: 3 as const,
  account: ACCOUNT,
  clientOrderId: ORDER_CLIENT_ID,
  side: 0,
  sizeDelta: '5000000000000000000000',
  marginDelta: '1001500000',
  targetPrice: '100100000',
  isClose: false,
  submitBy: '2000000060',
  executionWindowSeconds: 60,
  allowedExecutionModes: 1,
  expectedConfigHash: `0x${'12'.repeat(32)}` as Hex,
  maxExecutionBountyUsdc: '200000',
  maxExecutionNotionalUsdc: '5005000000',
  maxGrossAccountDebitUsdc: '2500000',
  maxActionChargeUsdc: '2200000',
  maxExplicitFeesUsdc: '300000',
  maxPostPositionSize: '5000000000000000000000',
  minPostSettlementBalanceUsdc: '999000000',
  minPostPositionEquityUsdc: '1001000000',
  maxPostLeverageBps: 50_000,
}

function paymasterData(expiry = SPONSORSHIP_VALID_UNTIL): Hex {
  return concatHex([
    '0x01',
    numberToHex(expiry, { size: 6 }),
    numberToHex(0n, { size: 6 }),
    `0x${'11'.repeat(65)}`,
  ])
}

function manifest(
  input: Partial<PerpsAaDeploymentManifestV1> = {}
): PerpsAaDeploymentManifestV1 {
  return {
    version: 'perps-aa-arbitrum-sepolia-v2',
    orderInterfaceVersion: 3,
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
    orderLifecycleBook: TARGET,
    policyEvaluator: TARGET,
    userOperationExplorerUrlTemplate:
      'https://example.com/user-operation/{userOperationHash}',
    transactionExplorerUrlTemplate:
      'https://example.com/transaction/{transactionHash}',
    testnetFaucet: null,
    sponsorshipEnabled: true,
    ...input,
  }
}

function v2Manifest(): PerpsAaDeploymentManifestV2 {
  const common = { ...manifest() } as Partial<PerpsAaDeploymentManifestV1>
  delete common.version
  delete common.pimlicoRpcUrl
  return {
    ...common as Omit<
      PerpsAaDeploymentManifestV1,
      'version' | 'pimlicoRpcUrl'
    >,
    version: 'perps-aa-arbitrum-sepolia-v2',
    orderInterfaceVersion: 3,
    bundlerRpcUrl: '/api/perps/v1/aa/rpc',
    paymasterRpcUrl: '/api/perps/v1/aa/rpc',
    paymasterAddress: PLETHER_PAYMASTER,
    paymasterVersion: 'plether-verifying-v1',
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

function pletherOperation(): ManagedUserOperation {
  return {
    ...operation(),
    paymaster: PLETHER_PAYMASTER,
    paymasterData: concatHex([
      numberToHex(SPONSORSHIP_VALID_UNTIL, { size: 6 }),
      numberToHex(SPONSORSHIP_VALID_UNTIL - 300n, { size: 6 }),
      numberToHex(1_000_000n, { size: 16 }),
      PLETHER_PAYMASTER_POLICY_ID,
      PLETHER_SIMPLE_ACCOUNT_PROXY_CODE_HASH,
      `0x${'44'.repeat(65)}`,
    ]),
    paymasterVerificationGasLimit:
      PLETHER_PAYMASTER_VERIFICATION_GAS_LIMIT,
    paymasterPostOpGasLimit: PLETHER_PAYMASTER_POST_OP_GAS_LIMIT,
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
      blockNumber: 123n,
      blockHash: INCLUDED_BLOCK_HASH,
    },
  } as ManagedUserOperationReceipt
}

function receiptNotFoundError(): Error {
  const error = new Error('receipt not found')
  error.name = 'UserOperationReceiptNotFoundError'
  return error
}

function statefulLockManager(): {
  lockManager: LockManager
  heldNames: Set<string>
  request: ReturnType<typeof vi.fn>
} {
  const heldNames = new Set<string>()
  const request = vi.fn(async (
    name: string,
    options: LockOptions,
    callback: (lock: Lock | null) => Promise<unknown> | unknown
  ) => {
    if (options.ifAvailable && heldNames.has(name)) {
      return await callback(null)
    }
    if (heldNames.has(name)) {
      throw new Error(`test lock already held: ${name}`)
    }
    heldNames.add(name)
    try {
      return await callback({ name, mode: 'exclusive' } as Lock)
    } finally {
      heldNames.delete(name)
    }
  })
  return {
    lockManager: { request } as unknown as LockManager,
    heldNames,
    request,
  }
}

function runtime(input: {
  prepareUserOperation?: PerpsAaSmartAccountRuntime['smartAccount']['prepareUserOperation']
  signUserOperation?: PerpsAaSmartAccountRuntime['smartAccount']['signUserOperation']
  getUserOperationHash?: PerpsAaSmartAccountRuntime['smartAccount']['getUserOperationHash']
  sendUserOperation?: PerpsAaSmartAccountRuntime['smartAccount']['sendUserOperation']
  getUserOperationStatus?: PerpsAaSmartAccountRuntime['smartAccount']['getUserOperationStatus']
  getUserOperationReceipt?: PerpsAaSmartAccountRuntime['smartAccount']['getUserOperationReceipt']
  verifyObservedInclusion?:
    PerpsAaSmartAccountRuntime['verifyObservedInclusion']
} = {}): PerpsAaSmartAccountRuntime {
  return {
    chainId: 421614,
    ownerAddress: OWNER,
    factoryAddress: FACTORY,
    accountVersion: 'permissionless-simple-v0.8',
    accountIndex: '0',
    verifyObservedInclusion: input.verifyObservedInclusion,
    smartAccount: {
      accountAddress: ACCOUNT,
      entryPoint: ENTRY_POINT,
      prepareUserOperation: input.prepareUserOperation ??
        vi.fn(async () => operation()),
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
  it('refuses sponsorship and signing when the reviewed order has fewer than twenty seconds left', async () => {
    const now = Date.now()
    const prepareUserOperation = vi.fn(async () => operation())
    const signUserOperation = vi.fn(async () => operation())
    const sendUserOperation = vi.fn(async () => USER_OPERATION_HASH)
    await expect(executeSponsoredPerpsAction({
      manifest: manifest(), ownerAddress: OWNER, action,
      orderRequestV3: { ...orderRequestV3, submitBy: String(Math.floor(now / 1000) + 19) },
      runtime: runtime({ prepareUserOperation, signUserOperation, sendUserOperation }),
    })).rejects.toMatchObject({ reason: 'INVALID_ORDER_DEADLINE' })
    expect(prepareUserOperation).not.toHaveBeenCalled()
    expect(signUserOperation).not.toHaveBeenCalled()
    expect(sendUserOperation).not.toHaveBeenCalled()
  })

  it('saves a late signature without submitting or releasing its recovery lane', async () => {
    const now = Math.floor(Date.now() / 1000) * 1000
    const clock = vi.spyOn(Date, 'now').mockReturnValue(now)
    const sendUserOperation = vi.fn(async () => USER_OPERATION_HASH)
    try {
      await expect(executeSponsoredPerpsAction({
        manifest: manifest(), ownerAddress: OWNER, action,
        orderRequestV3: { ...orderRequestV3, submitBy: String(now / 1000 + 60) },
        runtime: runtime({ sendUserOperation, prepareUserOperation: vi.fn(async () => ({ ...operation(), paymasterData: paymasterData(BigInt(now / 1000 + 60)) })), signUserOperation: vi.fn(async (prepared) => {
          clock.mockReturnValue(now + 51_000)
          return prepared
        }) }),
      })).rejects.toMatchObject({ reason: 'DEADLINE_TOO_CLOSE', terminalStatus: 'signed-not-submitted' })
      expect(sendUserOperation).not.toHaveBeenCalled()
      expect(reportAttemptStage).toHaveBeenCalledWith(expect.any(String), 'deadline_elapsed')
      const record = useSponsoredOperationStore.getState().operations[0]
      expect(record).toMatchObject({ status: 'signed-not-submitted', userOperationHash: USER_OPERATION_HASH })
      expect(record.signedUserOperation).toBeDefined()
      expect(useSponsoredOperationStore.getState().activeLanes).not.toEqual({})
      await useSponsoredOperationStore.persist.rehydrate()
      const restored = useSponsoredOperationStore.getState().operations[0]
      expect(restored.status).toBe('signed-not-submitted')
      const recoveryRuntime = runtime({ sendUserOperation })
      const snapshot = { blockNumber: 123n, accountNonce: operation().nonce,
        blockTimestamp: BigInt(now / 1000 + 60),
        userOperationEvidence: { kind: 'not-located' as const } }
      recoveryRuntime.getRecoverySnapshot = vi.fn(async () => snapshot)
      expect(await resolveProtocolOperation({ operation: restored, runtime: recoveryRuntime, userOperationHash: USER_OPERATION_HASH })).toBeUndefined()
      expect(useSponsoredOperationStore.getState().getActiveOperation(ACCOUNT)?.id).toBe(record.id)
      snapshot.blockTimestamp += 1n
      const resolution = await resolveProtocolOperation({ operation: restored, runtime: recoveryRuntime, userOperationHash: USER_OPERATION_HASH })
      expect(resolution).toEqual({ status: 'expired' })
      useSponsoredOperationStore.getState().failOperation({ id: record.id, status: 'expired', reason: 'expired', retryable: true })
      expect(useSponsoredOperationStore.getState().getActiveOperation(ACCOUNT)).toBeUndefined()
      expect(sendUserOperation).not.toHaveBeenCalled()
    } finally { clock.mockRestore() }
  })
  beforeEach(() => {
    authorizationMocks.clearDepositAuthorization.mockReset()
    authorizationMocks.clearLegacyDepositAuthorization.mockReset()
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

  const resumableStatus = { version: 1 as const, authorizationState: 'signed', validUntil: SPONSORSHIP_VALID_UNTIL.toString(),
    serverTime: String(Math.floor(Date.now() / 1000)), safeBlockTimestamp: null, phase: 'prepared' as const, reason: 'RESUMABLE',
    recoverable: true, freshReviewAllowed: true, userOperationHash: USER_OPERATION_HASH, transactionHash: null }

  it.each(['deposit', 'withdraw', 'withdraw-to-owner', 'place-order', 'place-protected-order', 'cancel-order',
    'create-protection', 'replace-protection', 'cancel-protection', 'add-margin', 'settle-claim'] as const)(
    'resumes a rejected native %s with the original preparation after reload', async kind => {
      const prepare = vi.fn(async () => pletherOperation())
      const sign = vi.fn(async (value: ManagedUserOperation) => value)
      sign.mockRejectedValueOnce({ code: 4001 })
      const managed = runtime({ prepareUserOperation: prepare, signUserOperation: sign })
      managed.smartAccount.getPreparationStatus = vi.fn(async () => resumableStatus)
      const input = { manifest: { ...v2Manifest(), preparationRpcVersion: 1 as const }, ownerAddress: OWNER,
        action: { ...action, kind }, runtime: managed, authorizationTokenToClearOnConfirmation: TARGET,
        authorizationNonceToClearOnConfirmation: AUTHORIZATION_NONCE }
      await expect(executeSponsoredPerpsAction(input)).rejects.toThrow('Signature declined. Your transaction was not sent.')
      const declined = useSponsoredOperationStore.getState().operations[0]
      expect(declined.status).toBe('signature-declined')
      expect(declined.preparedOperation?.operation).not.toHaveProperty('signature')
      expect(declined.signedUserOperation).toBeUndefined()
      expect(declined.userOperationHash).toBeUndefined()
      expect(managed.smartAccount.sendUserOperation).not.toHaveBeenCalled()
      expect(authorizationMocks.clearDepositAuthorization).not.toHaveBeenCalled()
      await useSponsoredOperationStore.persist.rehydrate()
      const saved = useSponsoredOperationStore.getState().operations[0]
      await resumeSponsoredPerpsAction(saved, managed)
      expect(prepare.mock.calls).toHaveLength(2)
      expect(prepare).toHaveBeenNthCalledWith(1, expect.objectContaining({ preparationId: declined.id }))
      expect(prepare).toHaveBeenNthCalledWith(2, expect.objectContaining({ preparationId: declined.id }))
      expect(sign.mock.calls[0][0]).toEqual(sign.mock.calls[1][0])
      expect(useSponsoredOperationStore.getState().operations).toHaveLength(1)
      expect(managed.smartAccount.sendUserOperation).toHaveBeenCalledTimes(1)
      expect(useSponsoredOperationStore.getState().operations[0].userOperationHash).toBe(USER_OPERATION_HASH)
    })

  it.each(['nonce', 'callGasLimit', 'maxFeePerGas', 'callData'] as const)('refuses changed prepared %s before another wallet prompt', async field => {
    const prepare = vi.fn(async () => pletherOperation())
    const sign = vi.fn(async () => { throw { code: 4001 } })
    const managed = runtime({ prepareUserOperation: prepare, signUserOperation: sign })
    managed.smartAccount.getPreparationStatus = vi.fn(async () => resumableStatus)
    await expect(executeSponsoredPerpsAction({ manifest: { ...v2Manifest(), preparationRpcVersion: 1 }, ownerAddress: OWNER, action, runtime: managed })).rejects.toThrow()
    const saved = useSponsoredOperationStore.getState().operations[0]
    prepare.mockResolvedValueOnce({ ...pletherOperation(), [field]: field === 'callData' ? '0x123456' : 999n })
    await expect(resumeSponsoredPerpsAction(saved, managed)).rejects.toThrow('payload changed')
    expect(sign).toHaveBeenCalledTimes(1)
    expect(managed.smartAccount.sendUserOperation).not.toHaveBeenCalled()
  })

  it.each(['INSUFFICIENT_FREE_EQUITY','INVALID_ORDER_DEADLINE','SIMULATION_FAILED','MUST_CLOSE_OPPOSING'])('persists %s as an explicit refusal without retrying, signing, sending, or releasing the durable lane', async reason => {
    const prepare=vi.fn().mockRejectedValue(new SponsorRequestError({reason,retryable:false,message:'Simulation rejected'}))
    const managed=runtime({prepareUserOperation:prepare})
    await expect(executeSponsoredPerpsAction({manifest:{...v2Manifest(),preparationRpcVersion:1},ownerAddress:OWNER,action,runtime:managed})).rejects.toThrow('Simulation rejected')
    await useSponsoredOperationStore.persist.rehydrate()
    const saved=useSponsoredOperationStore.getState().operations[0]
    expect(saved).toMatchObject({status:'sponsorship-refused',reason,retryable:false})
    expect(prepare).toHaveBeenCalledTimes(1)
    expect(managed.smartAccount.signUserOperation).not.toHaveBeenCalled()
    expect(managed.smartAccount.sendUserOperation).not.toHaveBeenCalled()
    expect(useSponsoredOperationStore.getState().getActiveOperation(ACCOUNT)?.id).toBe(saved.id)
  })

  it('preserves an account-confirmation rejection across reload and resumes the same attempt', async () => {
    const prepare = vi.fn(async () => pletherOperation()).mockRejectedValueOnce(new SponsorRequestError({
      reason: 'ACCOUNT_DEPLOYMENT_PENDING', retryable: true, message: 'Account awaiting safe confirmation',
    }))
    const managed = runtime({ prepareUserOperation: prepare })
    managed.smartAccount.getPreparationStatus = vi.fn().mockRejectedValue(new Error('PREPARATION_NOT_AUTHORIZED'))
    await expect(executeSponsoredPerpsAction({ manifest: { ...v2Manifest(), preparationRpcVersion: 1 }, ownerAddress: OWNER, action, runtime: managed })).rejects.toThrow()
    await useSponsoredOperationStore.persist.rehydrate()
    const saved = useSponsoredOperationStore.getState().operations[0]
    expect(saved).toMatchObject({ status: 'preparation-pending', reason: 'ACCOUNT_DEPLOYMENT_PENDING', retryable: true })
    expect(saved.preparedOperation).toBeUndefined()
    expect(useSponsoredOperationStore.getState().getActiveOperation(ACCOUNT)?.id).toBe(saved.id)
    expect(managed.smartAccount.signUserOperation).not.toHaveBeenCalled()
    expect(managed.smartAccount.sendUserOperation).not.toHaveBeenCalled()
    await resumeSponsoredPerpsAction(saved, managed)
    expect(prepare).toHaveBeenNthCalledWith(2, expect.objectContaining({ preparationId: saved.id }))
    expect(managed.smartAccount.getPreparationStatus).not.toHaveBeenCalled()
    expect(managed.smartAccount.sendUserOperation).toHaveBeenCalledTimes(1)
    expect(useSponsoredOperationStore.getState().operations).toHaveLength(1)
    expect(useSponsoredOperationStore.getState().operations[0].reason).toBeUndefined()
  })

  it('requires fresh review after a backend deadline rejection even when the local deadline looks valid', async () => {
    const prepare = vi.fn().mockRejectedValue(new SponsorRequestError({ reason: 'INVALID_ORDER_DEADLINE', retryable: false, message: 'Deadline rejected' }))
    const managed = runtime({ prepareUserOperation: prepare })
    await expect(executeSponsoredPerpsAction({ manifest: { ...v2Manifest(), preparationRpcVersion: 1 }, ownerAddress: OWNER,
      action, runtime: managed, orderRequestV3: { ...orderRequestV3, submitBy: String(Math.floor(Date.now() / 1000) + 600) } })).rejects.toThrow('Deadline rejected')
    const saved = useSponsoredOperationStore.getState().getActiveOperation(ACCOUNT)!
    await expect(resumeSponsoredPerpsAction(saved, managed)).rejects.toMatchObject({ reason: 'INVALID_ORDER_DEADLINE' })
    expect(prepare).toHaveBeenCalledOnce()
    expect(managed.smartAccount.signUserOperation).not.toHaveBeenCalled()
    expect(managed.smartAccount.sendUserOperation).not.toHaveBeenCalled()
    expect(useSponsoredOperationStore.getState().getActiveOperation(ACCOUNT)?.id).toBe(saved.id)
  })

  it.each([
    [new Error('connection interrupted'), 'UNKNOWN'],
    [new DOMException('Request timed out', 'TimeoutError'), 'SPONSOR_REQUEST_TIMEOUT'],
  ])('retries a dropped preparation response through its original ID without another journal entry (%s)', async (error, reason) => {
    const prepare = vi.fn(async () => pletherOperation()).mockRejectedValueOnce(error)
    const managed = runtime({ prepareUserOperation: prepare })
    await expect(executeSponsoredPerpsAction({ manifest: { ...v2Manifest(), preparationRpcVersion: 1 }, ownerAddress: OWNER, action, runtime: managed })).rejects.toThrow()
    const saved = useSponsoredOperationStore.getState().operations[0]
    expect(saved.nativePreparation).toBeDefined()
    expect(saved.preparedOperation).toBeUndefined()
    expect(saved).toMatchObject({ status: 'preparation-pending', reason })
    await resumeSponsoredPerpsAction(saved, managed)
    expect(prepare).toHaveBeenNthCalledWith(2, expect.objectContaining({ preparationId: saved.id }))
    expect(useSponsoredOperationStore.getState().operations).toHaveLength(1)
  })

  it('keeps ambiguous wallet errors separate from confirmed rejection', async () => {
    const managed = runtime({ prepareUserOperation: vi.fn(async () => pletherOperation()), signUserOperation: vi.fn(async () => { throw new Error('wallet disconnected') }) })
    await expect(executeSponsoredPerpsAction({ manifest: { ...v2Manifest(), preparationRpcVersion: 1 }, ownerAddress: OWNER, action, runtime: managed })).rejects.toThrow('wallet disconnected')
    expect(useSponsoredOperationStore.getState().operations[0]).toMatchObject({ status: 'preparation-pending', walletPreparationOutcome: 'unknown' })
    expect(managed.smartAccount.sendUserOperation).not.toHaveBeenCalled()
  })

  it('does not open a wallet when the unsigned recovery journal cannot be saved', async () => {
    const managed = runtime({ prepareUserOperation: vi.fn(async () => pletherOperation()) })
    const setItem = localStorage.setItem
    vi.spyOn(localStorage, 'setItem').mockImplementation(function (this: Storage, key, value) {
      if (key.startsWith(SPONSORED_OPERATION_JOURNAL_PREFIX) && value.includes('expectedHash')) throw new Error('quota exceeded')
      setItem.call(this, key, value)
    })
    await expect(executeSponsoredPerpsAction({ manifest: { ...v2Manifest(), preparationRpcVersion: 1 }, ownerAddress: OWNER, action, runtime: managed })).rejects.toThrow('could not be saved')
    expect(managed.smartAccount.signUserOperation).not.toHaveBeenCalled()
    expect(managed.smartAccount.sendUserOperation).not.toHaveBeenCalled()
  })

  it('refuses another signature when server status reports expiry or settlement', async () => {
    const managed = runtime({ prepareUserOperation: vi.fn(async () => pletherOperation()), signUserOperation: vi.fn(async () => { throw { code: 4001 } }) })
    await expect(executeSponsoredPerpsAction({ manifest: { ...v2Manifest(), preparationRpcVersion: 1 }, ownerAddress: OWNER, action, runtime: managed })).rejects.toThrow()
    managed.smartAccount.getPreparationStatus = vi.fn(async () => ({ ...resumableStatus, recoverable: false }))
    await expect(resumeSponsoredPerpsAction(useSponsoredOperationStore.getState().operations[0], managed)).rejects.toThrow('cannot currently be resumed')
    expect(managed.smartAccount.signUserOperation).toHaveBeenCalledTimes(1)
    expect(managed.smartAccount.prepareUserOperation).toHaveBeenCalledTimes(1)
  })

  it('serializes concurrent resume clicks with the existing browser lane lock', async () => {
    const browserLocks = statefulLockManager()
    vi.stubGlobal('navigator', { locks: browserLocks.lockManager })
    let completeSignature!: (operation: ManagedUserOperation) => void
    const pendingSignature = new Promise<ManagedUserOperation>(resolve => { completeSignature = resolve })
    const sign = vi.fn(async () => pendingSignature).mockRejectedValueOnce({ code: 4001 })
    const managed = runtime({ prepareUserOperation: vi.fn(async () => pletherOperation()), signUserOperation: sign })
    managed.smartAccount.getPreparationStatus = vi.fn(async () => resumableStatus)
    await expect(executeSponsoredPerpsAction({ manifest: { ...v2Manifest(), preparationRpcVersion: 1 }, ownerAddress: OWNER, action, runtime: managed })).rejects.toThrow()
    const saved = useSponsoredOperationStore.getState().operations[0]
    const first = resumeSponsoredPerpsAction(saved, managed)
    await vi.waitFor(() => { expect(sign).toHaveBeenCalledTimes(2) })
    await expect(resumeSponsoredPerpsAction(saved, managed)).rejects.toThrow()
    expect(sign).toHaveBeenCalledTimes(2)
    expect(managed.smartAccount.sendUserOperation).not.toHaveBeenCalled()
    completeSignature(pletherOperation())
    await first
    expect(managed.smartAccount.sendUserOperation).toHaveBeenCalledTimes(1)
  })

  it.each(['Deposit authorization expired', 'Deposit authorization consumed', 'Order deadline expired', 'Protection changed', 'Simulation failed', 'Prepared gas insufficient'])(
    'keeps the preparation and blocks signing after revalidation reports %s', async message => {
      const prepare = vi.fn(async () => pletherOperation())
      const sign = vi.fn(async () => { throw { code: 4001 } })
      const managed = runtime({ prepareUserOperation: prepare, signUserOperation: sign })
      managed.smartAccount.getPreparationStatus = vi.fn(async () => resumableStatus)
      await expect(executeSponsoredPerpsAction({ manifest: { ...v2Manifest(), preparationRpcVersion: 1 }, ownerAddress: OWNER, action, runtime: managed })).rejects.toThrow()
      const saved = useSponsoredOperationStore.getState().operations[0]
      prepare.mockRejectedValueOnce(new Error(message))
      await expect(resumeSponsoredPerpsAction(saved, managed)).rejects.toThrow()
      expect(sign).toHaveBeenCalledTimes(1)
      expect(managed.smartAccount.sendUserOperation).not.toHaveBeenCalled()
      expect(useSponsoredOperationStore.getState().operations[0].preparedOperation).toEqual(saved.preparedOperation)
      expect(authorizationMocks.clearDepositAuthorization).not.toHaveBeenCalled()
    })

  it('retains an abandoned preparation past history cleanup until its reservation is resolved', async () => {
    const managed = runtime({ prepareUserOperation: vi.fn(async () => pletherOperation()), signUserOperation: vi.fn(async () => { throw { code: 4001 } }) })
    await expect(executeSponsoredPerpsAction({ manifest: { ...v2Manifest(), preparationRpcVersion: 1 }, ownerAddress: OWNER, action, runtime: managed })).rejects.toThrow()
    const saved = useSponsoredOperationStore.getState().operations[0]
    useSponsoredOperationStore.getState().transition(saved.id, 'cancelled')
    vi.spyOn(Date, 'now').mockReturnValue(Date.now() + 25 * 60 * 60 * 1000)
    useSponsoredOperationStore.getState().cleanupOperations()
    expect(useSponsoredOperationStore.getState().operations).toHaveLength(1)
    useSponsoredOperationStore.getState().markPreparationResolved(saved.id)
    useSponsoredOperationStore.getState().cleanupOperations()
    expect(useSponsoredOperationStore.getState().operations).toHaveLength(0)
  })

  it('does not restore an older rejection after a later wallet attempt becomes uncertain', async () => {
    const sign = vi.fn(async () => { throw new Error('wallet disconnected') }).mockRejectedValueOnce({ code: 4001 })
    const managed = runtime({ prepareUserOperation: vi.fn(async () => pletherOperation()), signUserOperation: sign })
    managed.smartAccount.getPreparationStatus = vi.fn(async () => resumableStatus)
    await expect(executeSponsoredPerpsAction({ manifest: { ...v2Manifest(), preparationRpcVersion: 1 }, ownerAddress: OWNER, action, runtime: managed })).rejects.toThrow()
    const saved = useSponsoredOperationStore.getState().operations[0]
    const stale = localStorage.getItem(SPONSORED_OPERATION_STORAGE_NAME)!
    await expect(resumeSponsoredPerpsAction(saved, managed)).rejects.toThrow()
    localStorage.setItem(SPONSORED_OPERATION_STORAGE_NAME, stale)
    await useSponsoredOperationStore.persist.rehydrate()
    expect(useSponsoredOperationStore.getState().operations[0].walletPreparationOutcome).toBe('unknown')
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
      expect(pendingOperation.sponsorshipAuthority).toBeUndefined()
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

  it('journals an immutable bounded order before requesting its signature', async () => {
    const signUserOperation = vi.fn(async (value: ManagedUserOperation) => {
      const pendingOperation =
        useSponsoredOperationStore.getState().operations[0]!
      expect(pendingOperation).toMatchObject({
        status: 'awaiting-signature',
        orderRequestV3,
      })
      const exactJournal = JSON.parse(
        globalThis.localStorage.getItem(
          `${SPONSORED_OPERATION_JOURNAL_PREFIX}${pendingOperation.id}`
        )!
      )
      expect(exactJournal).toMatchObject({
        version: 1,
        operation: {
          id: pendingOperation.id,
          status: 'awaiting-signature',
          orderRequestV3,
        },
      })
      expect(pendingOperation.userOperationHash).toBeUndefined()
      return value
    })

    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action: { ...action, kind: 'place-order' },
      runtime: runtime({ signUserOperation }),
      orderRequestV3,
    })).resolves.toMatchObject({
      userOperationHash: USER_OPERATION_HASH,
      transactionHash: TRANSACTION_HASH,
    })

    expect(signUserOperation).toHaveBeenCalledOnce()
  })

  it('durably journals a protection-only intent before requesting a signature', async () => {
    const protectionIntent = { version: 1 as const, book: TARGET, protectionId: '7', takeProfitTriggerPrice: '90000000', stopLossTriggerPrice: '110000000' }
    const signUserOperation = vi.fn(async (value: ManagedUserOperation) => {
      const pendingOperation = useSponsoredOperationStore.getState().operations[0]!
      expect(pendingOperation).toMatchObject({ status: 'awaiting-signature', protectionIntent })
      expect(JSON.parse(globalThis.localStorage.getItem(`${SPONSORED_OPERATION_JOURNAL_PREFIX}${pendingOperation.id}`)!)).toMatchObject({
        operation: { id: pendingOperation.id, protectionIntent, status: 'awaiting-signature' },
      })
      expect(pendingOperation.orderRequestV3).toBeUndefined()
      return value
    })
    await executeSponsoredPerpsAction({ manifest: manifest(), ownerAddress: OWNER, action: { ...action, kind: 'replace-protection' }, runtime: runtime({ signUserOperation }), protectionIntent })
    expect(signUserOperation).toHaveBeenCalledOnce()
  })

  it('does not pre-sign journal a generic hashless operation', async () => {
    const signUserOperation = vi.fn(async (value: ManagedUserOperation) => {
      const pendingOperation =
        useSponsoredOperationStore.getState().operations[0]!
      expect(globalThis.localStorage.getItem(
        `${SPONSORED_OPERATION_JOURNAL_PREFIX}${pendingOperation.id}`
      )).toBeNull()
      return value
    })

    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: runtime({ signUserOperation }),
    })).resolves.toMatchObject({
      userOperationHash: USER_OPERATION_HASH,
      transactionHash: TRANSACTION_HASH,
    })

    expect(signUserOperation).toHaveBeenCalledOnce()
  })

  it('journals and submits the exact Plether-sponsored v2 operation', async () => {
    const preparedOperation = pletherOperation()
    const signUserOperation = vi.fn(async (value) => value)
    const sendUserOperation = vi.fn(async (value) => {
      expect(value).toEqual(preparedOperation)
      const pendingOperation =
        useSponsoredOperationStore.getState().operations[0]!
      expect(pendingOperation).toMatchObject({
        status: 'submitting',
        userOperationHash: USER_OPERATION_HASH,
        sponsorshipAuthority: {
          version: 1,
          paymasterAddress: PLETHER_PAYMASTER,
          validUntil: SPONSORSHIP_VALID_UNTIL.toString(),
        },
        signedUserOperation: {
          paymaster: PLETHER_PAYMASTER,
          paymasterData: preparedOperation.paymasterData,
        },
      })
      expect(JSON.parse(globalThis.localStorage.getItem(
        `${SPONSORED_OPERATION_JOURNAL_PREFIX}${pendingOperation.id}`
      )!)).toMatchObject({
        version: 1,
        operation: {
          id: pendingOperation.id,
          userOperationHash: USER_OPERATION_HASH,
          sponsorshipAuthority: {
            version: 1,
            paymasterAddress: PLETHER_PAYMASTER,
            validUntil: SPONSORSHIP_VALID_UNTIL.toString(),
          },
        },
      })
      return USER_OPERATION_HASH
    })

    await expect(executeSponsoredPerpsAction({
      manifest: v2Manifest(),
      ownerAddress: OWNER,
      action,
      runtime: runtime({
        prepareUserOperation: vi.fn(async () => preparedOperation),
        signUserOperation,
        sendUserOperation,
      }),
    })).resolves.toMatchObject({
      userOperationHash: USER_OPERATION_HASH,
      transactionHash: TRANSACTION_HASH,
    })

    expect(signUserOperation).toHaveBeenCalledWith(preparedOperation)
    expect(sendUserOperation).toHaveBeenCalledOnce()
  })

  it('resolves at exact successful inclusion and leaves safe confirmation to recovery', async () => {
    const includedReceipt = receipt()
    const getUserOperationReceipt = vi.fn(async () => {
      throw new UserOperationReceiptNotSafeError(includedReceipt)
    })
    const onIncluded = vi.fn()

    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: runtime({ getUserOperationReceipt }),
      onIncluded,
    })).resolves.toMatchObject({
      userOperationHash: USER_OPERATION_HASH,
      transactionHash: TRANSACTION_HASH,
    })

    expect(getUserOperationReceipt).toHaveBeenCalledOnce()
    expect(onIncluded).toHaveBeenCalledOnce()
    expect(onIncluded).toHaveBeenCalledWith({
      userOperationHash: USER_OPERATION_HASH,
      receipt: includedReceipt,
      transactionHash: TRANSACTION_HASH,
    })
    const includedOperation =
      useSponsoredOperationStore.getState().operations[0]
    expect(includedOperation).toMatchObject({
      status: 'confirming',
      includedTransactionHash: TRANSACTION_HASH,
      laneReleasedAfterSuccessfulInclusion: true,
    })
    expect(includedOperation?.transactionHash).toBeUndefined()
    expect(includedOperation?.transactionHashVerified).toBeUndefined()
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({})
  })

  it('compare-deletes the exact authorization nonce before releasing inclusion', async () => {
    authorizationMocks.clearDepositAuthorization.mockImplementation(
      (input: { expectedNonce?: Hex }) => {
        expect(input.expectedNonce).toBe(AUTHORIZATION_NONCE)
        expect(useSponsoredOperationStore.getState().operations[0])
          .not.toHaveProperty('laneReleasedAfterSuccessfulInclusion')
        expect(useSponsoredOperationStore.getState().activeLanes)
          .not.toEqual({})
      }
    )
    const includedReceipt = receipt()

    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: runtime({
        getUserOperationReceipt: vi.fn(async () => {
          throw new UserOperationReceiptNotSafeError(includedReceipt)
        }),
      }),
      authorizationTokenToClearOnConfirmation: TARGET,
      authorizationNonceToClearOnConfirmation: AUTHORIZATION_NONCE,
    })).resolves.toMatchObject({
      userOperationHash: USER_OPERATION_HASH,
      transactionHash: TRANSACTION_HASH,
    })

    expect(authorizationMocks.clearDepositAuthorization)
      .toHaveBeenCalledWith(expect.objectContaining({
        token: TARGET,
        expectedNonce: AUTHORIZATION_NONCE,
      }))
    expect(useSponsoredOperationStore.getState().operations[0])
      .toMatchObject({ laneReleasedAfterSuccessfulInclusion: true })
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({})
  })

  it('keeps unsafe inclusion locked when authorization cleanup fails', async () => {
    vi.useFakeTimers()
    try {
      authorizationMocks.clearDepositAuthorization
        .mockImplementationOnce(() => {
          throw new Error('local storage unavailable')
        })
        .mockImplementation(() => true)
      const includedReceipt = receipt()
      const getUserOperationReceipt = vi.fn()
        .mockRejectedValueOnce(
          new UserOperationReceiptNotSafeError(includedReceipt)
        )
        .mockResolvedValue(includedReceipt)
      let settled = false
      const execution = executeSponsoredPerpsAction({
        manifest: manifest(),
        ownerAddress: OWNER,
        action,
        runtime: runtime({ getUserOperationReceipt }),
        authorizationTokenToClearOnConfirmation: TARGET,
        authorizationNonceToClearOnConfirmation: AUTHORIZATION_NONCE,
      }).finally(() => {
        settled = true
      })

      await vi.waitFor(() => {
        expect(authorizationMocks.clearDepositAuthorization)
          .toHaveBeenCalledOnce()
      })
      expect(settled).toBe(false)
      expect(useSponsoredOperationStore.getState().operations[0])
        .not.toHaveProperty('laneReleasedAfterSuccessfulInclusion')
      expect(useSponsoredOperationStore.getState().activeLanes)
        .not.toEqual({})

      await vi.advanceTimersByTimeAsync(2_000)
      await expect(execution).resolves.toMatchObject({
        userOperationHash: USER_OPERATION_HASH,
        transactionHash: TRANSACTION_HASH,
      })
      expect(useSponsoredOperationStore.getState().operations[0])
        .toMatchObject({ status: 'confirmed' })
      expect(useSponsoredOperationStore.getState().activeLanes).toEqual({})
      expect(authorizationMocks.clearDepositAuthorization)
        .toHaveBeenCalledTimes(2)
    } finally {
      vi.useRealTimers()
    }
  })

  it('releases durable and browser lanes so a second action submits before safe', async () => {
    const locks = statefulLockManager()
    vi.stubGlobal('navigator', { locks: locks.lockManager })
    const firstSend = vi.fn(async () => USER_OPERATION_HASH)
    const includedReceipt = receipt()

    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: runtime({
        sendUserOperation: firstSend,
        getUserOperationReceipt: vi.fn(async () => {
          throw new UserOperationReceiptNotSafeError(includedReceipt)
        }),
      }),
    })).resolves.toMatchObject({
      userOperationHash: USER_OPERATION_HASH,
      transactionHash: TRANSACTION_HASH,
    })
    await vi.waitFor(() => expect(locks.heldNames.size).toBe(0))

    const secondReceipt = {
      ...receipt(),
      userOpHash: OTHER_USER_OPERATION_HASH,
      nonce: 1n,
      receipt: {
        ...receipt().receipt,
        transactionHash: REPLACEMENT_TRANSACTION_HASH,
        blockNumber: 124n,
        blockHash: REPLACEMENT_BLOCK_HASH,
      },
    } as ManagedUserOperationReceipt
    const secondSend = vi.fn(async () => OTHER_USER_OPERATION_HASH)
    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: runtime({
        getUserOperationHash: vi.fn(() => OTHER_USER_OPERATION_HASH),
        sendUserOperation: secondSend,
        getUserOperationReceipt: vi.fn(async () => secondReceipt),
      }),
    })).resolves.toMatchObject({
      userOperationHash: OTHER_USER_OPERATION_HASH,
      transactionHash: REPLACEMENT_TRANSACTION_HASH,
    })

    expect(firstSend).toHaveBeenCalledOnce()
    expect(secondSend).toHaveBeenCalledOnce()
    expect(locks.request).toHaveBeenCalledTimes(2)
  })

  it('keeps vendor-only inclusion locked until exact chain evidence arrives', async () => {
    vi.useFakeTimers()
    try {
      const locks = statefulLockManager()
      vi.stubGlobal('navigator', { locks: locks.lockManager })
      const getUserOperationReceipt = vi.fn(async () => {
        throw receiptNotFoundError()
      })
      const getUserOperationStatus = vi.fn(async () => ({
        status: 'included' as const,
        transactionHash: TRANSACTION_HASH,
      }))
      const firstExecution = executeSponsoredPerpsAction({
        manifest: manifest(),
        ownerAddress: OWNER,
        action,
        runtime: runtime({
          getUserOperationReceipt,
          getUserOperationStatus,
        }),
      }).catch((error: unknown) => error)

      await vi.waitFor(() => {
        expect(getUserOperationStatus).toHaveBeenCalled()
      })
      expect(useSponsoredOperationStore.getState().operations[0])
        .not.toHaveProperty('laneReleasedAfterSuccessfulInclusion')
      expect(useSponsoredOperationStore.getState().activeLanes)
        .not.toEqual({})

      const secondRuntime = runtime({
        getUserOperationHash: vi.fn(() => OTHER_USER_OPERATION_HASH),
        sendUserOperation: vi.fn(async () => OTHER_USER_OPERATION_HASH),
      })
      await expect(executeSponsoredPerpsAction({
        manifest: manifest(),
        ownerAddress: OWNER,
        action,
        runtime: secondRuntime,
      })).rejects.toBeInstanceOf(SponsoredOperationLockedError)
      expect(secondRuntime.smartAccount.prepareUserOperation)
        .not.toHaveBeenCalled()

      await vi.advanceTimersByTimeAsync(120_000)
      await expect(firstExecution).resolves.toMatchObject({
        terminalStatus: 'receipt-timeout',
      })
    } finally {
      vi.useRealTimers()
    }
  })

  it('keeps an exact failed unsafe receipt locked until safe resolution', async () => {
    vi.useFakeTimers()
    try {
      const failedReceipt = {
        ...receipt(),
        success: false,
        reason: 'execution reverted',
        receipt: {
          ...receipt().receipt,
          status: 'success',
        },
      } as ManagedUserOperationReceipt
      const getUserOperationReceipt = vi.fn(async () => {
        throw new UserOperationReceiptNotSafeError(failedReceipt)
      })
      const execution = executeSponsoredPerpsAction({
        manifest: manifest(),
        ownerAddress: OWNER,
        action,
        runtime: runtime({ getUserOperationReceipt }),
      }).catch((error: unknown) => error)

      await vi.waitFor(() => {
        expect(getUserOperationReceipt).toHaveBeenCalled()
      })
      expect(useSponsoredOperationStore.getState().operations[0])
        .not.toHaveProperty('laneReleasedAfterSuccessfulInclusion')
      expect(useSponsoredOperationStore.getState().activeLanes)
        .not.toEqual({})

      await vi.advanceTimersByTimeAsync(120_000)
      await expect(execution).resolves.toMatchObject({
        terminalStatus: 'receipt-timeout',
        message: expect.stringContaining('failed onchain'),
      })
      expect(useSponsoredOperationStore.getState().operations[0])
        .toMatchObject({ includedSuccess: false, includedTransactionHash: TRANSACTION_HASH })
    } finally {
      vi.useRealTimers()
    }
  })

  it('does not let an optional inclusion callback retain the released lane', async () => {
    const locks = statefulLockManager()
    vi.stubGlobal('navigator', { locks: locks.lockManager })
    const includedReceipt = receipt()
    const callbackError = new Error('receipt consumer failed')
    const secondReceipt = {
      ...receipt(),
      userOpHash: OTHER_USER_OPERATION_HASH,
      nonce: 1n,
      receipt: {
        ...receipt().receipt,
        transactionHash: REPLACEMENT_TRANSACTION_HASH,
        blockNumber: 124n,
        blockHash: REPLACEMENT_BLOCK_HASH,
      },
    } as ManagedUserOperationReceipt
    const secondSend = vi.fn(async () => OTHER_USER_OPERATION_HASH)
    let secondExecution:
      ReturnType<typeof executeSponsoredPerpsAction> | undefined
    const onIncluded = vi.fn(() => {
      // Queue the next action from the consumer callback itself. If the first
      // execution only releases its Web Lock in the outer finally block, this
      // microtask races ahead of that continuation and deterministically sees
      // a busy lane.
      secondExecution = Promise.resolve().then(async () =>
        await executeSponsoredPerpsAction({
          manifest: manifest(),
          ownerAddress: OWNER,
          action,
          runtime: runtime({
            getUserOperationHash: vi.fn(() => OTHER_USER_OPERATION_HASH),
            sendUserOperation: secondSend,
            getUserOperationReceipt: vi.fn(async () => secondReceipt),
          }),
        })
      )
      throw callbackError
    })

    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: runtime({
        getUserOperationReceipt: vi.fn(async () => {
          throw new UserOperationReceiptNotSafeError(includedReceipt)
        }),
      }),
      onIncluded,
    })).resolves.toMatchObject({
      userOperationHash: USER_OPERATION_HASH,
      transactionHash: TRANSACTION_HASH,
    })
    expect(onIncluded).toHaveBeenCalledOnce()
    expect(secondExecution).toBeDefined()
    await expect(secondExecution).resolves.toMatchObject({
      userOperationHash: OTHER_USER_OPERATION_HASH,
      transactionHash: REPLACEMENT_TRANSACTION_HASH,
    })
    expect(secondSend).toHaveBeenCalledOnce()
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({})
  })

  it('preserves the last reconciliation error as the receipt timeout cause', async () => {
    vi.useFakeTimers()
    try {
      const reconciliationError =
        new Error('canonical receipt nonce mismatch')
      const getUserOperationReceipt = vi.fn(async () => {
        throw reconciliationError
      })
      const execution = executeSponsoredPerpsAction({
        manifest: manifest(),
        ownerAddress: OWNER,
        action,
        runtime: runtime({ getUserOperationReceipt }),
      }).catch((error: unknown) => error)

      await vi.waitFor(() => {
        expect(getUserOperationReceipt).toHaveBeenCalled()
      })
      await vi.advanceTimersByTimeAsync(120_000)

      await expect(execution).resolves.toMatchObject({
        name: 'BundlerRequestError',
        terminalStatus: 'receipt-timeout',
        cause: reconciliationError,
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
      terminalStatus: 'submission-unknown',
      retryable: false,
    })

    expect(managedRuntime.smartAccount.sendUserOperation).toHaveBeenCalledTimes(1)
    expect(getUserOperationStatus).not.toHaveBeenCalled()
    expect(useSponsoredOperationStore.getState().operations[0]).toMatchObject({
      status: 'submission-unknown',
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

  it('retains a nested gateway failure without misreporting a receipt timeout or retrying submission', async () => {
    const sendUserOperation = vi.fn(async () => { throw { cause: { data: { reason: 'SECURITY_ATTESTATION_UNAVAILABLE', retryable: true } } } })
    await expect(executeSponsoredPerpsAction({ manifest: manifest(), ownerAddress: OWNER, action,
      runtime: runtime({ sendUserOperation }),
    })).rejects.toMatchObject({ terminalStatus: 'submission-unknown', reason: 'SECURITY_ATTESTATION_UNAVAILABLE', retryable: false })
    expect(useSponsoredOperationStore.getState().operations[0]).toMatchObject({ status: 'submission-unknown', reason: 'SECURITY_ATTESTATION_UNAVAILABLE' })
    expect(useSponsoredOperationStore.getState().getActiveOperation(ACCOUNT)).toBeDefined()
    expect(sendUserOperation).toHaveBeenCalledOnce()
  })

  it.each([
    new Error('connection closed before response'),
    new TimeoutError({ url: 'https://example.test/aa/rpc' }),
  ])('keeps an ambiguous submission non-retryable after %s', async cause => {
    const sendUserOperation = vi.fn(async () => {
      throw cause
    })

    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: OWNER,
      action,
      runtime: runtime({ sendUserOperation }),
    })).rejects.toMatchObject({
      retryable: false,
      terminalStatus: 'submission-unknown',
      reason: 'SUBMISSION_OUTCOME_UNKNOWN',
    })

    expect(sendUserOperation).toHaveBeenCalledTimes(1)
    expect(useSponsoredOperationStore.getState().operations[0]).toMatchObject({
      status: 'submission-unknown',
      userOperationHash: USER_OPERATION_HASH,
      retryable: false,
    })
    expect(useSponsoredOperationStore.getState().getActiveOperation(ACCOUNT)).toBeDefined()
    expect(useSponsoredOperationStore.getState().operations[0]?.signedUserOperation).toBeDefined()
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

  it('does not accept a legacy Pimlico envelope for a new v2 operation', async () => {
    const managedRuntime = runtime()

    await expect(executeSponsoredPerpsAction({
      manifest: v2Manifest(),
      ownerAddress: OWNER,
      action,
      runtime: managedRuntime,
    })).rejects.toMatchObject({
      reason: 'SPONSOR_UNAVAILABLE',
      retryable: false,
    })

    expect(managedRuntime.smartAccount.signUserOperation)
      .not.toHaveBeenCalled()
    expect(managedRuntime.smartAccount.sendUserOperation)
      .not.toHaveBeenCalled()
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
      authorizationNonceToClearOnConfirmation: '0x1234',
    })).resolves.toMatchObject({
      userOperationHash: USER_OPERATION_HASH,
      transactionHash: TRANSACTION_HASH,
    })

    expect(
      authorizationMocks.clearDepositAuthorization
    ).toHaveBeenCalled()
    expect(useSponsoredOperationStore.getState().operations[0]).toMatchObject({
      status: 'confirmed',
      userOperationHash: USER_OPERATION_HASH,
      transactionHash: TRANSACTION_HASH,
    })
  })

  it('retires only the legacy cache for token-only safe metadata', async () => {
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

    expect(authorizationMocks.clearLegacyDepositAuthorization)
      .toHaveBeenCalledWith(expect.objectContaining({ token: TARGET }))
    expect(authorizationMocks.clearDepositAuthorization).not.toHaveBeenCalled()
  })
})

vi.mock('../deadlineClock', () => ({ deadlineNow: () => Date.now(), refreshDeadlineClock: async () => ({ now: () => Date.now() }), observeDeadlineResponse: () => {} }))
