import type { PerpsActionPlan } from '@plether-fi/perps-aa-client'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { keccak256, type Address, type PublicClient, type WalletClient } from 'viem'
import { entryPoint09Address } from 'viem/account-abstraction'
import { arbitrumSepolia } from 'viem/chains'
import { PERPS_ARBITRUM_SEPOLIA } from '../../contracts/perpsAddresses'
import type { PersistedPerpsOrderRequestV3 } from '../../contracts/perpsOrderV3'
import { writeGasPaymentPreference } from '../gasPaymentPreference'
import { createKernelRuntime } from '../kernelRuntime'
import type { KernelPreparedOperation } from '../kernelRpc'
import type { KernelPerpsAaDeploymentManifest } from '../manifest'

const { activeAttempt, fetchQuote, sign, save, submit, getAttempt, refresh, verifyAccountAtBlock, getNonce, fetchManifest, serverNow, getBytecode } = vi.hoisted(() => ({
  activeAttempt: vi.fn(), fetchQuote: vi.fn(), sign: vi.fn(), save: vi.fn(), submit: vi.fn(), getAttempt: vi.fn(), refresh: vi.fn(),
  verifyAccountAtBlock: vi.fn(), getNonce: vi.fn(), fetchManifest: vi.fn(), serverNow: vi.fn(), getBytecode: vi.fn(),
}))
vi.mock('../kernelDeploymentPins', async importOriginal => ({
  ...await importOriginal<typeof import('../kernelDeploymentPins')>(),
  kernelSepoliaCanaryPaymasterCodeHash: (await import('viem')).keccak256('0x01'),
}))
vi.mock('../kernelAccount', async importOriginal => {
  const actual = await importOriginal<typeof import('../kernelAccount')>()
  return { ...actual, verifyKernelTradingAccountAtBlock: verifyAccountAtBlock,
    createKernelTradingAccount: async () => ({ address: sender, factoryArgs: { factory: staker, factoryData: '0x01' }, getNonce, signUserOperation: sign }) }
})
vi.mock('../kernelRpc', async importOriginal => {
  const actual = await importOriginal<typeof import('../kernelRpc')>()
  return { ...actual, prepareKernelOperation: fetchQuote, fetchKernelManifest: fetchManifest, assertKernelPreparedOperation: vi.fn() }
})
vi.mock('../kernelOperationJournal', () => ({
  getActiveKernelAttempt: activeAttempt,
  withKernelOperationLane: <T,>(_scope: unknown, work: () => Promise<T>) => work(),
  saveKernelSignedAttempt: save,
  submitKernelAttempt: submit,
  getKernelAttempt: getAttempt,
  refreshKernelAttempt: refresh,
}))
vi.mock('../deadlineClock', () => ({ deadlineNow: serverNow, refreshDeadlineClock: async () => ({ now: serverNow }) }))
vi.mock('../readiness', () => ({ refreshReadiness: vi.fn(), currentReadiness: vi.fn(), readinessBlocker: vi.fn(), readinessMessage: vi.fn() }))

const owner: Address = '0x1111111111111111111111111111111111111111'
const sender: Address = '0x2222222222222222222222222222222222222222'
const factory: Address = '0x3333333333333333333333333333333333333333'
const staker: Address = '0x5555555555555555555555555555555555555555'
const paymaster: Address = '0x4444444444444444444444444444444444444444'
const hash = `0x${'12'.repeat(32)}` as const
const code = '0x01'
const codeHash = keccak256(code)
const profile = {
  deployment: 'kernel-sepolia', chainId: 421614, entryPoint: entryPoint09Address, entryPointVersion: '0.9',
  accountVersion: 'kernel-v4-uups', accountIndex: 0, factory, staker, implementation: factory, validator: factory,
  entryPointCodeHash: codeHash, factoryCodeHash: codeHash, stakerCodeHash: codeHash, implementationCodeHash: codeHash,
  validatorCodeHash: codeHash, accountCodeHash: codeHash,
} as const
const manifest: KernelPerpsAaDeploymentManifest = {
  ...PERPS_ARBITRUM_SEPOLIA,
  version: 'perps-aa-kernel-v1:kernel-sepolia', entryPointVersion: '0.9', entryPoint: entryPoint09Address,
  kernel: { ...PERPS_ARBITRUM_SEPOLIA, version: 1, profile, paymaster, readiness: { eth: true, sponsored: true },
    protectionCommitsEnabled: false, perpsRelease: 'v2.0.0' },
  chainId: 421614, orderInterfaceVersion: 3, smartAccountMode: 'kernel', smartAccountFactory: staker,
  smartAccountVersion: 'kernel-v4-uups', smartAccountIndex: '0', sponsorshipEnabled: true,
  settlementTokenSupportsEip3009: false, settlementTokenEip712Name: null, settlementTokenEip712Version: null,
  userOperationExplorerUrlTemplate: 'https://example.test/op/{userOperationHash}',
  transactionExplorerUrlTemplate: 'https://example.test/tx/{transactionHash}', testnetFaucet: null,
}
const action: PerpsActionPlan = { kind: 'settle-claim', account: sender, calls: [{ to: factory, value: 0n, data: '0x1234' }] }
const publicClient = {
  chain: arbitrumSepolia,
  getBlockNumber: async () => 100n,
  getBlock: async ({ blockNumber }: { blockNumber?: bigint }) => ({ number: blockNumber ?? 100n, hash }),
  getBytecode,
  readContract: async () => ({ orderId: 0n }),
} as unknown as PublicClient
const walletClient = {
  account: { address: owner }, chain: arbitrumSepolia, getAddresses: async () => [owner], getChainId: async () => 421614,
} as unknown as WalletClient

const createRuntime = () => createKernelRuntime({ manifest, ownerAddress: owner, walletClient, publicClient })

beforeEach(() => {
  localStorage.clear()
  vi.clearAllMocks()
  serverNow.mockReturnValue(1_000_000)
  getNonce.mockResolvedValue(0n)
  fetchManifest.mockResolvedValue(manifest.kernel)
  getBytecode.mockResolvedValue(code)
  verifyAccountAtBlock.mockImplementation(async (_client: unknown, _profile: unknown, _owner: unknown, blockNumber: bigint) =>
    ({ address: sender, ownerAddress: owner, deployed: blockNumber > 100n, blockNumber }))
  fetchQuote.mockImplementation(async (_manifest: unknown, input: { mode: 'sponsored' | 'eth'; callData: `0x${string}`; factory?: Address; expectedNonce: bigint }) => ({
    version: 1, entryPoint: entryPoint09Address, preparationId: hash, userOperationHash: hash,
    mode: input.mode, maxNetworkGasWei: 35n, expiresAt: 1_100, recoveryToken: 'local-test',
    operation: { sender, callData: input.callData, nonce: input.expectedNonce, factory: input.factory,
      ...(input.mode === 'sponsored' ? { paymaster } : {}) },
  } satisfies Omit<KernelPreparedOperation, 'operation'> & { operation: Pick<KernelPreparedOperation['operation'], 'sender' | 'callData' | 'nonce' | 'factory' | 'paymaster'> }))
})

describe('Kernel review execution', () => {
  it('keeps the account stable and rejects a reviewed quote after switching away and back', async () => {
    const runtime = await createRuntime()
    const sponsored = await runtime.kernel.quoteAction(action)
    writeGasPaymentPreference(runtime.gasPayment.scope, 'eth')
    const paid = await runtime.kernel.quoteAction(action)
    writeGasPaymentPreference(runtime.gasPayment.scope, 'sponsored')

    expect(fetchQuote.mock.calls.map(([, intent]) => intent)).toEqual([
      expect.objectContaining({ mode: 'sponsored', sender, factory: staker, factoryData: '0x01', expectedNonce: 0n }),
      expect.objectContaining({ mode: 'eth', sender, factory: staker, factoryData: '0x01', expectedNonce: 0n }),
    ])
    expect(paid).toMatchObject({ mode: 'eth', maxNetworkGasWei: 35n })
    await expect(runtime.kernel.execute({ runtime, manifest, ownerAddress: owner, action, kernelQuote: sponsored }))
      .rejects.toThrow('gas setting changed')
    expect(sign).not.toHaveBeenCalled()
  })

  it('binds the approved quote to the exact action and saves the signed operation before relay', async () => {
    const runtime = await createRuntime()
    writeGasPaymentPreference(runtime.gasPayment.scope, 'eth')
    const quote = await runtime.kernel.quoteAction(action)
    await expect(runtime.kernel.execute({ runtime, manifest, ownerAddress: owner, kernelQuote: quote,
      action: { ...action, calls: [{ ...action.calls[0], value: 1n }] } })).rejects.toThrow('Review the gas fee')
    expect(sign).not.toHaveBeenCalled()

    sign.mockResolvedValue('0x01')
    save.mockImplementation(async stored => stored)
    getAttempt.mockResolvedValue({ phase: 'submitted' })
    refresh.mockResolvedValue({ phase: 'included', receipt: { success: true,
      receipt: { status: 'success', transactionHash: hash, blockNumber: 105n, blockHash: hash } } })
    await expect(runtime.kernel.execute({ runtime, manifest, ownerAddress: owner, action, kernelQuote: quote }))
      .resolves.toMatchObject({ userOperationHash: hash, transactionHash: hash })
    expect(save).toHaveBeenCalledWith(expect.objectContaining({ prepared: expect.objectContaining({ mode: 'eth' }), signature: '0x01' }))
    expect(save.mock.invocationCallOrder[0]).toBeLessThan(submit.mock.invocationCallOrder[0])
    expect(verifyAccountAtBlock).toHaveBeenLastCalledWith(publicClient, profile, owner, 105n, sender)
  })

  it('checks exact gas mode availability again before requesting a signature', async () => {
    const runtime = await createRuntime()
    writeGasPaymentPreference(runtime.gasPayment.scope, 'eth')
    const quote = await runtime.kernel.quoteAction(action)
    fetchManifest.mockResolvedValueOnce({ ...manifest.kernel, readiness: { eth: false, sponsored: true } })
    await expect(runtime.kernel.execute({ runtime, manifest, ownerAddress: owner, action, kernelQuote: quote }))
      .rejects.toMatchObject({ reason: 'PAYMENT_MODE_UNAVAILABLE' })
    expect(sign).not.toHaveBeenCalled()
    expect(save).not.toHaveBeenCalled()
  })

  it('requires the reviewed sponsor contract bytecode before opening the wallet', async () => {
    const runtime = await createRuntime()
    const quote = await runtime.kernel.quoteAction(action)
    const execute = () => runtime.kernel.execute({ runtime, manifest, ownerAddress: owner, action, kernelQuote: quote })
    getBytecode.mockResolvedValueOnce('0x02')
    await expect(execute()).rejects.toThrow('sponsorship contract does not match')
    expect(sign).not.toHaveBeenCalled()

    const walletDecline = new Error('Wallet declined')
    sign.mockRejectedValueOnce(walletDecline)
    await expect(execute()).rejects.toBe(walletDecline)
    expect(getBytecode).toHaveBeenLastCalledWith({ address: paymaster, blockTag: 'safe' })
    expect(sign).toHaveBeenCalledOnce()
    expect(save).not.toHaveBeenCalled()
  })

  it('binds a quote to the live root nonce and rejects nonce changes before or during signing', async () => {
    const runtime = await createRuntime()
    writeGasPaymentPreference(runtime.gasPayment.scope, 'eth')
    getNonce.mockResolvedValue(4n)
    const quote = await runtime.kernel.quoteAction(action)
    expect(fetchQuote).toHaveBeenLastCalledWith(manifest.kernel, expect.objectContaining({ expectedNonce: 4n }), undefined)
    const execute = () => runtime.kernel.execute({ runtime, manifest, ownerAddress: owner, action, kernelQuote: quote })

    getNonce.mockResolvedValueOnce(5n)
    await expect(execute()).rejects.toThrow('nonce changed')
    expect(sign).not.toHaveBeenCalled()

    sign.mockImplementationOnce(async () => {
      getNonce.mockResolvedValueOnce(5n)
      return '0x01'
    })
    await expect(execute()).rejects.toThrow('nonce changed')
    expect(sign).toHaveBeenCalledOnce()
    expect(save).not.toHaveBeenCalled()
    expect(submit).not.toHaveBeenCalled()
  })

  it('requires over 30 seconds of order submission time both before and after the wallet prompt', async () => {
    const runtime = await createRuntime()
    writeGasPaymentPreference(runtime.gasPayment.scope, 'eth')
    const quote = await runtime.kernel.quoteAction(action, undefined, hash)
    const execute = (submitBy: string) => runtime.kernel.execute({ runtime, manifest, ownerAddress: owner, action,
      kernelQuote: quote, orderRequestV3: { submitBy, clientOrderId: hash } as PersistedPerpsOrderRequestV3 })

    await expect(execute('1030')).rejects.toThrow('order deadline is too close')
    expect(sign).not.toHaveBeenCalled()

    sign.mockImplementationOnce(async () => {
      serverNow.mockReturnValue(1_001_000)
      return '0x01'
    })
    await expect(execute('1031')).rejects.toThrow('order deadline is too close')
    expect(sign).toHaveBeenCalledOnce()
    expect(save).not.toHaveBeenCalled()
    expect(submit).not.toHaveBeenCalled()
  })
})
