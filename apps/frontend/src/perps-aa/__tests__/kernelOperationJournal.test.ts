import { beforeEach, describe, expect, it, vi } from 'vitest'
import { concatHex, createPublicClient, custom, padHex, toHex, zeroAddress, type Address, type Hex } from 'viem'
import { entryPoint09Address, type UserOperationReceipt } from 'viem/account-abstraction'
import { privateKeyToAccount } from 'viem/accounts'
import { arbitrumSepolia } from 'viem/chains'
import { trackPerpsKernelOutcome } from '../../analytics/perps'
import { createKernelTradingAccount, encodeKernelCalls } from '../kernelAccount'
import { kernelSepoliaCanaryProfile } from '../kernelDeploymentPins'
import {
  discardUnsentKernelAttempt,
  getActiveKernelAttempt,
  getKernelAttempt,
  kernelAttemptCanRetry,
  reconcileKernelAttempt,
  retryKernelAttempt,
  retryKernelCancellation,
  saveKernelSignedAttempt,
  signAndSubmitKernelCancellation,
  submitKernelAttempt,
  withKernelOperationLane,
  type KernelAttemptScope,
} from '../kernelOperationJournal'
import {
  getKernelOperationHash,
  getKernelOperationStatus,
  sendKernelCancellation,
  sendKernelOperation,
  type KernelPreparedCancellation,
  type KernelPreparedOperation,
} from '../kernelRpc'

const { records, send, cancel, status, release } = vi.hoisted(() => ({
  records: new Map<string, unknown>(),
  send: vi.fn(),
  cancel: vi.fn(),
  status: vi.fn(),
  release: vi.fn(async () => undefined),
}))

vi.mock('idb-keyval', () => ({
  createStore: () => undefined,
  get: async (key: string) => structuredClone(records.get(key)),
  update: async (key: string, edit: (value: unknown) => unknown) => {
    records.set(key, structuredClone(edit(structuredClone(records.get(key)))))
  },
}))
vi.mock('../laneLock', () => ({
  acquireSponsoredOperationBrowserLane: vi.fn(async () => release),
  acquireSponsoredOperationBrowserRecoveryLock: vi.fn(async () => release),
}))
vi.mock('../deadlineClock', () => ({
  deadlineNow: () => Date.now(),
  refreshDeadlineClock: async () => ({ now: Date.now }),
}))
vi.mock('../kernelRpc', async importOriginal => ({
  ...await importOriginal<typeof import('../kernelRpc')>(),
  sendKernelOperation: send,
  sendKernelCancellation: cancel,
  getKernelOperationStatus: status,
}))
vi.mock('../../analytics/perps', () => ({ trackPerpsKernelOutcome: vi.fn() }))

const owner = privateKeyToAccount(toHex(1, { size: 32 }))
const scope: KernelAttemptScope = {
  deployment: 'kernel-local-test',
  chainId: arbitrumSepolia.id,
  entryPoint: entryPoint09Address.toLowerCase() as Address,
  sender: '0x00000000000000000000000000000000000000ab',
}
const paymaster: Address = '0x00000000000000000000000000000000000000cd'
const now = 1_800_000_000_000

function prepared(input: {
  slot?: number
  mode?: 'eth' | 'sponsored'
  expiresAt?: number
  callData?: Hex
} = {}): KernelPreparedOperation {
  const { slot = 1, mode = 'eth', expiresAt = Math.floor(now / 1_000) + 120, callData = '0x1234' } = input
  const preparationId = toHex(slot, { size: 32 })
  const operation = {
    sender: scope.sender,
    nonce: 2n,
    callData,
    callGasLimit: 100_000n,
    verificationGasLimit: 150_000n,
    preVerificationGas: 50_000n,
    maxFeePerGas: BigInt(slot + 1),
    maxPriorityFeePerGas: 1n,
    signature: '0x' as const,
    ...(mode === 'sponsored' ? {
      paymaster,
      paymasterData: concatHex([
        toHex(expiresAt + 30, { size: 6 }), toHex(expiresAt - 120, { size: 6 }),
        toHex(10_000_000n, { size: 16 }), toHex(0n, { size: 129 }),
      ]),
      paymasterVerificationGasLimit: 100_000n,
      paymasterPostOpGasLimit: 0n,
    } : {}),
  }
  return {
    version: 1, preparationId, entryPoint: scope.entryPoint, mode, operation,
    userOperationHash: getKernelOperationHash(operation), expiresAt,
    maxNetworkGasWei: (300_000n + (operation.paymasterVerificationGasLimit ?? 0n)) * operation.maxFeePerGas,
    recoveryToken: `v1.${preparationId}.${toHex(2n, { size: 32 })}.1800000200.${'5'.repeat(64)}`,
  }
}

function sign(quote: KernelPreparedOperation) {
  return owner.signMessage({ message: { raw: quote.userOperationHash } })
}

async function save(quote: KernelPreparedOperation) {
  return saveKernelSignedAttempt({ scope, ownerAddress: owner.address, action: 'place-order', prepared: quote, signature: await sign(quote) })
}

describe('Kernel operation journal', () => {
  beforeEach(() => {
    vi.restoreAllMocks()
    vi.clearAllMocks()
    vi.spyOn(Date, 'now').mockReturnValue(now)
    records.clear()
    send.mockReset()
    cancel.mockReset()
    status.mockReset()
  })

  it('journals and relays canonical bytes from a wallet signature with uppercase r and s', async () => {
    const quote = prepared()
    const canonical = await sign(quote)
    const walletSignature = `0x${canonical.slice(2, -2).toUpperCase()}${canonical.endsWith('1b') ? '00' : '01'}` as const
    expect(walletSignature.slice(2, 66)).toMatch(/[A-F]/)
    expect(walletSignature.slice(66, 130)).toMatch(/[A-F]/)

    const client = createPublicClient({
      chain: arbitrumSepolia,
      transport: custom({
        request: async ({ method }) => {
          if (method === 'eth_getCode') return '0x'
          if (method === 'eth_call') return padHex(scope.sender, { size: 32 })
          throw new Error(`Unexpected RPC method ${method}`)
        },
      }),
    })
    const account = await createKernelTradingAccount(client, kernelSepoliaCanaryProfile, {
      address: owner.address,
      signMessage: async () => walletSignature,
    })
    const signature = await account.signUserOperation(quote.operation)
    const saved = await saveKernelSignedAttempt({ scope, ownerAddress: owner.address, action: 'place-order', prepared: quote, signature })

    expect(signature).toBe(canonical)
    await expect(getKernelAttempt(scope, quote.userOperationHash)).resolves.toMatchObject({ phase: 'signed', signature: canonical })
    send.mockResolvedValueOnce(quote.userOperationHash)
    await expect(submitKernelAttempt(saved)).resolves.toBe(quote.userOperationHash)
    expect(sendKernelOperation).toHaveBeenCalledExactlyOnceWith(quote, canonical)
  })

  it('persists exact signed bytes before relay and keeps both payment modes fenced on a lost response', async () => {
    const quote = prepared()
    const initial = await save(quote)
    const lostResponse = new Error('Lost response')
    send.mockImplementationOnce(async () => {
      const record = await getActiveKernelAttempt(scope)
      expect(record?.phase).toBe('sending')
      expect(record?.prepared).toEqual(quote)
      expect(record?.signature).toBe(initial.signature)
      throw lostResponse
    })
    await expect(submitKernelAttempt(initial)).rejects.toBe(lostResponse)
    const sendPhase = await getKernelAttempt(scope, quote.userOperationHash)
    expect(sendPhase?.phase).toBe('sending')
    await expect(save(prepared({ slot: 2, mode: 'sponsored' }))).rejects.toThrow('already being submitted')
    await expect(withKernelOperationLane(scope, async () => undefined)).rejects.toThrow('already being submitted')
    status.mockResolvedValueOnce({ state: 'pending', receipt: null })
    await expect(reconcileKernelAttempt(sendPhase!)).resolves.toMatchObject({ phase: 'sending' })
    expect(getKernelOperationStatus).toHaveBeenCalledWith(expect.objectContaining({
      userOperationHash: quote.userOperationHash, nonce: 2n, sender: scope.sender, paymaster: zeroAddress,
      recoveryToken: quote.recoveryToken, entryPoint: scope.entryPoint,
    }))
    send.mockResolvedValueOnce(quote.userOperationHash)
    await expect(retryKernelAttempt(sendPhase!)).resolves.toBe(quote.userOperationHash)
    expect(sendKernelOperation).toHaveBeenLastCalledWith(quote, initial.signature)
    await expect(getActiveKernelAttempt(scope)).resolves.toMatchObject({ phase: 'submitted' })
  })

  it('retires a canonical revert and tracks its actual cost exactly once', async () => {
    const quote = prepared({ mode: 'sponsored' })
    send.mockResolvedValueOnce(quote.userOperationHash)
    await submitKernelAttempt(await save(quote))
    const pending = await getActiveKernelAttempt(scope)
    const receipt = { success: false, actualGasCost: 12_345n } as UserOperationReceipt<'0.9'>
    status.mockResolvedValue({ state: 'included', receipt })
    await expect(reconcileKernelAttempt(pending!)).resolves.toMatchObject({ phase: 'included', receipt })
    await expect(reconcileKernelAttempt(pending!)).resolves.toMatchObject({ phase: 'included', receipt })
    await expect(getActiveKernelAttempt(scope)).resolves.toBeUndefined()
    expect(trackPerpsKernelOutcome).toHaveBeenCalledExactlyOnceWith({
      mode: 'sponsored', outcome: 'execution_reverted', maximum: quote.maxNetworkGasWei, actual: receipt.actualGasCost,
    })
    await expect(save(prepared({ slot: 2 }))).resolves.toMatchObject({ phase: 'signed' })
  })

  it('retains the exact signed cancellation before sending and releases only after canonical status', async () => {
    const quote = prepared()
    send.mockResolvedValueOnce(quote.userOperationHash)
    await submitKernelAttempt(await save(quote))
    const pending = await getActiveKernelAttempt(scope)
    const replacement: KernelPreparedCancellation = {
      ...prepared({ slot: 2, callData: encodeKernelCalls([{ to: scope.sender }]) }),
      mode: 'eth', cancelsUserOperationHash: quote.userOperationHash,
    }
    const signCancellation = vi.fn(sign)
    cancel.mockImplementationOnce(async () => {
      const record = await getActiveKernelAttempt(scope)
      expect(record?.phase).toBe('submitted')
      expect(record?.cancellation?.phase).toBe('sending')
      expect(record?.cancellation?.prepared).toEqual(replacement)
      expect(record && kernelAttemptCanRetry(record)).toBe(false)
      return replacement.userOperationHash
    })
    await expect(signAndSubmitKernelCancellation(pending!, replacement, signCancellation)).resolves.toBe(replacement.userOperationHash)
    expect(signCancellation).toHaveBeenCalledExactlyOnceWith(replacement)
    expect(sendKernelCancellation).toHaveBeenCalledExactlyOnceWith(quote, replacement, await sign(replacement))
    const cancellation = await getActiveKernelAttempt(scope)
    vi.mocked(Date.now).mockReturnValue((replacement.expiresAt + 1) * 1_000)
    cancel.mockResolvedValueOnce(replacement.userOperationHash)
    await expect(retryKernelCancellation(cancellation!)).resolves.toBe(replacement.userOperationHash)
    expect(sendKernelCancellation).toHaveBeenLastCalledWith(quote, replacement, await sign(replacement))
    const receipt = { success: true, actualGasCost: 30_000n } as UserOperationReceipt<'0.9'>
    status.mockResolvedValueOnce({ state: 'cancelled', receipt })
    await expect(reconcileKernelAttempt(cancellation!)).resolves.toMatchObject({ phase: 'cancelled', receipt })
    expect(status).toHaveBeenLastCalledWith(expect.objectContaining({
      userOperationHash: quote.userOperationHash, cancellationHash: replacement.userOperationHash, paymaster: zeroAddress,
    }))
    await expect(getActiveKernelAttempt(scope)).resolves.toBeUndefined()
  })

  it('keeps a cancellation saved during an in-progress status check for the next canonical lookup', async () => {
    const quote = prepared()
    send.mockResolvedValueOnce(quote.userOperationHash)
    await submitKernelAttempt(await save(quote))
    const pending = (await getActiveKernelAttempt(scope))!
    const replacement: KernelPreparedCancellation = {
      ...prepared({ slot: 2, callData: encodeKernelCalls([{ to: scope.sender }]) }),
      mode: 'eth', cancelsUserOperationHash: quote.userOperationHash,
    }
    const staleStatus = Promise.withResolvers<{ state: 'nonce_consumed'; receipt: null }>()
    const statusRequested = Promise.withResolvers<void>()
    status.mockImplementationOnce(() => {
      statusRequested.resolve()
      return staleStatus.promise
    })
    const firstPoll = reconcileKernelAttempt(pending)
    await statusRequested.promise
    expect(status).toHaveBeenLastCalledWith(expect.objectContaining({ cancellationHash: undefined }))

    cancel.mockResolvedValueOnce(replacement.userOperationHash)
    await signAndSubmitKernelCancellation(pending, replacement, sign)
    staleStatus.resolve({ state: 'nonce_consumed', receipt: null })
    await expect(firstPoll).resolves.toMatchObject({ phase: 'submitted', cancellation: { prepared: replacement } })
    expect(trackPerpsKernelOutcome).not.toHaveBeenCalled()

    const receipt = { success: true, actualGasCost: 30_000n } as UserOperationReceipt<'0.9'>
    status.mockResolvedValueOnce({ state: 'cancelled', receipt })
    await expect(reconcileKernelAttempt(pending)).resolves.toMatchObject({ phase: 'cancelled', receipt })
    expect(status).toHaveBeenLastCalledWith(expect.objectContaining({ cancellationHash: replacement.userOperationHash }))
  })

  it('discards an expired attempt only when the committed phase still proves it unsent', async () => {
    const quote = prepared()
    const unsent = await save(quote)
    await expect(discardUnsentKernelAttempt(unsent)).rejects.toThrow('expired and unsent')
    const sentScope = { ...scope, deployment: 'kernel-other-test' }
    const sent = await saveKernelSignedAttempt({ ...unsent, scope: sentScope })
    send.mockRejectedValueOnce(new Error('Connection timed out'))
    await expect(submitKernelAttempt(sent)).rejects.toThrow('Connection timed out')
    vi.mocked(Date.now).mockReturnValue((quote.expiresAt + 1) * 1000)
    await expect(discardUnsentKernelAttempt(unsent)).resolves.toBeUndefined()
    await expect(getKernelAttempt(scope, quote.userOperationHash)).resolves.toMatchObject({ phase: 'discarded' })
    await expect(discardUnsentKernelAttempt(sent)).rejects.toThrow('expired and unsent')
    await expect(getActiveKernelAttempt(sentScope)).resolves.toMatchObject({ phase: 'sending' })
  })
})
