import type { PerpsActionPlan } from '@plether-fi/perps-aa-client'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { keccak256, type Address, type PublicClient, type WalletClient } from 'viem'
import { entryPoint09Address } from 'viem/account-abstraction'
import { arbitrumSepolia } from 'viem/chains'
import { PERPS_ARBITRUM_SEPOLIA } from '../../contracts/perpsAddresses'
import { writeGasPaymentPreference } from '../gasPaymentPreference'
import { createKernelRuntime } from '../kernelRuntime'
import type { KernelPreparedOperation } from '../kernelRpc'
import type { KernelPerpsAaDeploymentManifest } from '../manifest'

const { activeAttempt, fetchQuote, sign, save, submit, getAttempt, refresh, verifyAccountAtBlock } = vi.hoisted(() => ({
  activeAttempt: vi.fn(), fetchQuote: vi.fn(), sign: vi.fn(), save: vi.fn(), submit: vi.fn(), getAttempt: vi.fn(), refresh: vi.fn(),
  verifyAccountAtBlock: vi.fn(),
}))
vi.mock('../kernelAccount', async importOriginal => {
  const actual = await importOriginal<typeof import('../kernelAccount')>()
  return { ...actual, verifyKernelTradingAccountAtBlock: verifyAccountAtBlock,
    createKernelTradingAccount: async () => ({ address: sender, factoryArgs: { factory: staker, factoryData: '0x01' }, signUserOperation: sign }) }
})
vi.mock('../kernelRpc', async importOriginal => {
  const actual = await importOriginal<typeof import('../kernelRpc')>()
  return { ...actual, prepareKernelOperation: fetchQuote, assertKernelPreparedOperation: vi.fn() }
})
vi.mock('../kernelOperationJournal', () => ({
  getActiveKernelAttempt: activeAttempt,
  withKernelOperationLane: <T,>(_scope: unknown, work: () => Promise<T>) => work(),
  saveKernelSignedAttempt: save,
  submitKernelAttempt: submit,
  getKernelAttempt: getAttempt,
  refreshKernelAttempt: refresh,
}))
vi.mock('../deadlineClock', () => ({ deadlineNow: () => 1_000_000, refreshDeadlineClock: async () => ({ now: () => 1_000_000 }) }))
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
  getBlock: async ({ blockNumber }: { blockNumber?: bigint }) => ({ number: blockNumber ?? 100n, hash }),
} as unknown as PublicClient
const walletClient = {
  account: { address: owner }, chain: arbitrumSepolia, getAddresses: async () => [owner], getChainId: async () => 421614,
} as unknown as WalletClient

const createRuntime = () => createKernelRuntime({ manifest, ownerAddress: owner, walletClient, publicClient })

beforeEach(() => {
  localStorage.clear()
  vi.clearAllMocks()
  verifyAccountAtBlock.mockImplementation(async (_client: unknown, _profile: unknown, _owner: unknown, blockNumber: bigint) =>
    ({ address: sender, ownerAddress: owner, deployed: blockNumber > 100n, blockNumber }))
  fetchQuote.mockImplementation(async (_manifest: unknown, input: { mode: 'sponsored' | 'eth'; callData: `0x${string}`; factory?: Address }) => ({
    version: 1, entryPoint: entryPoint09Address, preparationId: hash, userOperationHash: hash,
    mode: input.mode, maxNetworkGasWei: 35n, expiresAt: 1_100, recoveryToken: 'local-test',
    operation: { sender, callData: input.callData, nonce: 0n, factory: input.factory },
  } satisfies Omit<KernelPreparedOperation, 'operation'> & { operation: Pick<KernelPreparedOperation['operation'], 'sender' | 'callData' | 'nonce' | 'factory'> }))
})

describe('Kernel review execution', () => {
  it('keeps the account stable and rejects a reviewed quote after switching away and back', async () => {
    const runtime = await createRuntime()
    const sponsored = await runtime.kernel.quoteAction(action)
    writeGasPaymentPreference(runtime.gasPayment.scope, 'eth')
    const paid = await runtime.kernel.quoteAction(action)
    writeGasPaymentPreference(runtime.gasPayment.scope, 'sponsored')

    expect(fetchQuote.mock.calls.map(([, intent]) => intent)).toEqual([
      expect.objectContaining({ mode: 'sponsored', sender, factory: staker, factoryData: '0x01' }),
      expect.objectContaining({ mode: 'eth', sender, factory: staker, factoryData: '0x01' }),
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
})
