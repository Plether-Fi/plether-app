import { afterEach, describe, expect, it, vi } from 'vitest'
import { decodeFunctionData, type Address, type Hex, type PublicClient, type WalletClient } from 'viem'
import rawManifest from '../../../public/perps-aa-manifest.json'
import { parsePerpsAaManifest } from '../manifest'
import { commitNativeClose, executeNativeOrder, historicalClosePayload, nativeProtectionAction, type NativeCloseContext } from '../nativeClose'
import { permissivePerpsExecutionBounds, type PerpsOrderRequestV3 } from '../../contracts/perpsOrderV3'
import { PERPS_ORDER_ROUTER_ABI } from '../../contracts/abis'
vi.mock('../../contracts/verifyPerpsV3Bindings', () => ({ verifyPerpsV3DeploymentBindings: vi.fn(async () => ({ blockNumber: 10n })) }))
const manifest = parsePerpsAaManifest({ ...rawManifest, orderInterfaceVersion: 3 })
const account = '0x1111111111111111111111111111111111111111' as Address
const owner = '0x2222222222222222222222222222222222222222' as Address
const implementation = '0x3333333333333333333333333333333333333333' as Address
const hash = `0x${'ab'.repeat(32)}` as Hex
const feeds = Array.from({ length: 6 }, (_, i) => `0x${(i + 1).toString(16).padStart(64, '0')}` as Hex)
const request: PerpsOrderRequestV3 = {
  clientOrderId: hash, side: 0, sizeDelta: 100n * 10n ** 18n, marginDelta: 0n, targetPrice: 101_000_000n, isClose: true,
  bounds: permissivePerpsExecutionBounds({ submitBy: 2_000_000_000n, executionWindowSeconds: 60, expectedConfigHash: hash, executionBountyUsdc: 0n }),
}
function fixture(overrides: Record<string, unknown> = {}) {
  const values: Record<string, unknown> = { getAddress: account, owner, entryPoint: manifest.entryPoint, accountImplementation: implementation, resolveClientIntent: [1, 7n, hash], orderTiming: { executionDeadline: 100n, commitTimestamp: 39n }, nextExecuteId: 7n, pletherOracle: implementation, isOracleFrozen: false, orderSettlementWindow: 60n, getOrderExecutionFee: 13n, ...overrides }
  const client = {
    chain: { id: manifest.chainId }, readContract: vi.fn(async (call: { functionName: string; args?: readonly unknown[] }) => call.functionName === 'pythFeedIds' ? feeds[Number(call.args?.[0])] : values[call.functionName]),
    getStorageAt: vi.fn(async () => `0x${'00'.repeat(12)}${implementation.slice(2)}`), getCode: vi.fn(async () => '0x1234'),
    simulateContract: vi.fn(async (call: Record<string, unknown>) => { if (call.functionName === 'executeOrder') throw new Error('price data required'); return { request: call } }),
    waitForTransactionReceipt: vi.fn(async () => ({ status: 'success', blockNumber: 11n, blockHash: hash, logs: [] })),
    getBlock: vi.fn(async () => ({ number: 10n, timestamp: 100n, hash })), call: vi.fn(async () => ({})),
  }
  const wallet = { account: { address: owner }, chain: { id: manifest.chainId }, writeContract: vi.fn(async (_call: Record<string, unknown>) => { void _call; return hash }), sendTransaction: vi.fn(async (_call: { to: Address; data: Hex; value: bigint }) => { void _call; return hash }) }
  const context: NativeCloseContext = { client: client as unknown as PublicClient, wallet: wallet as unknown as WalletClient, manifest, account, owner }
  return { context, client, wallet }
}
function serve(time = 40, ids = feeds) {
  const fetcher = vi.fn(async (_url: URL | RequestInfo) => { void _url; return new Response(JSON.stringify({ binary: { data: ['abcd'] }, parsed: ids.map(id => ({ id: id.slice(2), price: { publish_time: time } })) })) })
  vi.stubGlobal('fetch', fetcher)
  return fetcher
}
afterEach(() => vi.unstubAllGlobals())
describe('native close commitment', () => {
  it('owner executes through the same account without sponsor, bundler, deposit or approval', async () => {
    const { context, wallet } = fixture(); const fetcher = serve()
    expect(await commitNativeClose(context, request)).toMatchObject({ account, orderId: 7n, hash })
    const submitted = wallet.writeContract.mock.calls[0][0]
    expect(submitted.address).toBe(account)
    expect(submitted.functionName).toBe('execute')
    const args = submitted.args as [Address, bigint, Hex]
    expect(args[0]).toBe(manifest.orderRouter); expect(args[1]).toBe(0n)
    expect(decodeFunctionData({ abi: PERPS_ORDER_ROUTER_ABI, data: args[2] })).toMatchObject({ functionName: 'commitOrder', args: [request] })
    expect(fetcher).not.toHaveBeenCalled()
  })
  it.each(['owner', 'getAddress', 'entryPoint', 'accountImplementation'])('rejects mismatched %s before signing', async key => {
    const { context, wallet } = fixture({ [key]: '0x4444444444444444444444444444444444444444' })
    await expect(commitNativeClose(context, request)).rejects.toThrow('does not match')
    expect(wallet.writeContract).not.toHaveBeenCalled()
  })
  it('rejects opens, a wrong chain and an undeployed account', async () => {
    const { context, client, wallet } = fixture()
    await expect(commitNativeClose(context, { ...request, isClose: false })).rejects.toThrow('restricted to closes')
    wallet.chain.id = 1
    await expect(commitNativeClose(context, request)).rejects.toThrow('owner wallet')
    wallet.chain.id = manifest.chainId; client.getCode.mockResolvedValue('0x')
    await expect(commitNativeClose(context, request)).rejects.toThrow('does not match')
    expect(wallet.writeContract).not.toHaveBeenCalled()
  })
  it('requires canonical intent reconciliation after inclusion', async () => {
    const { context } = fixture({ resolveClientIntent: [0, 0n, hash] })
    await expect(commitNativeClose(context, request)).rejects.toThrow('needs reconciliation')
  })
})
describe('independent historical oracle source', () => {
  it('requests the first postcommit tick and validates all configured components', async () => {
    const { context } = fixture(); const fetcher = serve()
    expect(await historicalClosePayload(context.client, implementation, 40n, 99n)).toEqual(['0xabcd'])
    const url = fetcher.mock.calls[0][0] as URL
    expect(url.origin).toBe('https://hermes.pyth.network'); expect(url.pathname).toBe('/v2/updates/price/40')
    expect(url.searchParams.getAll('ids[]')).toHaveLength(6)
  })
  it.each([39, 100])('rejects out-of-window tick %s', async time => {
    const { context } = fixture(); serve(time)
    await expect(historicalClosePayload(context.client, implementation, 40n, 99n)).rejects.toThrow('outside the order window')
  })
  it('rejects missing or duplicate feeds', async () => {
    const { context } = fixture()
    for (const ids of [feeds.slice(1), [...feeds.slice(1), feeds[1]]]) {
      serve(40, ids)
      await expect(historicalClosePayload(context.client, implementation, 40n, 99n)).rejects.toThrow('incomplete')
    }
  })
})
describe('permissionless execution and cleanup', () => {
  it('equality is not expiry', async () => {
    const { context, wallet } = fixture()
    await expect(executeNativeOrder(context, 7n, true)).rejects.toThrow('strictly after')
    expect(wallet.sendTransaction).not.toHaveBeenCalled()
  })
  it('expires without oracle data and does not infer execution from transaction status', async () => {
    const { context, client, wallet } = fixture(); const fetcher = serve()
    client.getBlock.mockResolvedValue({ number: 10n, timestamp: 101n, hash })
    await expect(executeNativeOrder(context, 7n, true)).rejects.toThrow('remains pending')
    const submitted = wallet.sendTransaction.mock.calls[0][0]
    expect(submitted.value).toBe(0n)
    expect(decodeFunctionData({ abi: PERPS_ORDER_ROUTER_ABI, data: submitted.data })).toMatchObject({ functionName: 'expireOrder', args: [7n] })
    expect(fetcher).not.toHaveBeenCalled()
  })
  it('does not skip a live FIFO predecessor', async () => {
    const { context, wallet } = fixture({ nextExecuteId: 6n })
    await expect(executeNativeOrder(context, 7n)).rejects.toThrow('Order 6 is ahead')
    expect(wallet.sendTransaction).not.toHaveBeenCalled()
  })
  it('quotes the exact oracle native fee and refuses success without a terminal receipt', async () => {
    const { context, wallet } = fixture(); serve()
    await expect(executeNativeOrder(context, 7n)).rejects.toThrow('remains pending')
    const submitted = wallet.sendTransaction.mock.calls[0][0]
    expect(submitted.to).toBe(manifest.orderRouter); expect(submitted.value).toBe(13n)
    expect(decodeFunctionData({ abi: PERPS_ORDER_ROUTER_ABI, data: submitted.data })).toMatchObject({ functionName: 'executeOrder', args: [7n, ['0xabcd']] })
  })
})

// Full event evidence must authenticate, even when the transaction itself succeeded.
describe('native terminal evidence', () => {
  async function terminalFixture(verified = true) {
    const { orderLifecycleV5Abi, hashOrderReceiptV4 } = await import('@plether-fi/perps-aa-client')
    const { encodeAbiParameters, encodeEventTopics } = await import('viem')
    const event = orderLifecycleV5Abi.find(x => x.type === 'event' && x.name === 'OrderFinalized')!
    type Parameter = import('viem').AbiParameter
    function zero(p: Parameter): unknown {
      if (p.type === 'tuple' && 'components' in p) return Object.fromEntries(p.components.map(c => [c.name, zero(c)]))
      if (p.type === 'address') return `0x${'00'.repeat(20)}`
      if (p.type === 'bool') return false
      if (p.type.startsWith('bytes')) return `0x${'00'.repeat(Number(p.type.slice(5)))}`
      return Number(p.type.replace(/\D/g, '')) <= 48 ? 0 : 0n
    }
    const receipt = { ...(zero(event.inputs[6]) as import('@plether-fi/perps-aa-client').OrderReceiptV4), orderId: 7n, account, status: 2, reason: 1, executionPrice: 80_000_000n }
    const receiptHash = hashOrderReceiptV4({ chainId: BigInt(manifest.chainId), book: manifest.orderLifecycleBook, router: manifest.orderRouter, terminalBlock: 11n, terminalTime: 101n, receipt })
    const { context, client, wallet } = fixture({ terminalOutcome: { receiptHash, account }, verifyReceipt: verified, isOracleFrozen: true })
    const log = {
      address: manifest.orderLifecycleBook,
      topics: encodeEventTopics({ abi: orderLifecycleV5Abi, eventName: 'OrderFinalized', args: { orderId: 7n, account, clientOrderId: receipt.clientOrderId } }),
      data: encodeAbiParameters(event.inputs.filter(p => !p.indexed), [receiptHash, 11n, 101n, receipt]),
    }
    client.waitForTransactionReceipt.mockResolvedValue({ status: 'success', blockNumber: 11n, blockHash: hash, logs: [log] as never[] })
    return { context, client, wallet }
  }
  it('returns the authenticated execution result', async () => {
    const { context, client } = await terminalFixture()
    expect(await executeNativeOrder(context, 7n)).toMatchObject({ hash, executionPrice: 80_000_000n, failedReason: undefined })
    expect(client.readContract).toHaveBeenCalledWith(expect.objectContaining({ functionName: 'verifyReceipt', blockNumber: 11n }))
  })
  it('rejects a receipt that the canonical Book does not authenticate', async () => {
    const { context } = await terminalFixture(false)
    await expect(executeNativeOrder(context, 7n)).rejects.toThrow('failed verification')
  })
})

describe('native protection management', () => {
  it.each(['cancel', 'retry'] as const)('authenticates %s against the same account and canonical post-action state', async action => {
    const before = { account, status: action === 'cancel' ? 2 : 8, linkedOrderId: 5n }
    const { context, client, wallet } = fixture({ getPositionProtection: before })
    const read = client.readContract.getMockImplementation()!
    client.readContract.mockImplementation(async call => call.functionName === 'getPositionProtection' && 'blockNumber' in call ? { account, status: action === 'cancel' ? 6 : 3, linkedOrderId: 9n } : read(call))
    expect(await nativeProtectionAction(context, 1n, action)).toEqual({ hash })
    expect(wallet.writeContract.mock.calls[0][0]).toMatchObject(action === 'cancel' ? { address: account, functionName: 'execute' } : { address: manifest.positionProtectionBook, functionName: 'retryPositionProtectionClose' })
  })
  it('refuses a foreign protection and an unchanged retry result', async () => {
    const { context, wallet } = fixture({ getPositionProtection: { account: owner, status: 8, linkedOrderId: 5n } })
    await expect(nativeProtectionAction(context, 1n, 'retry')).rejects.toThrow('different Trading Account')
    expect(wallet.writeContract).not.toHaveBeenCalled()
    const same = fixture({ getPositionProtection: { account, status: 8, linkedOrderId: 5n } })
    await expect(nativeProtectionAction(same.context, 1n, 'retry')).rejects.toThrow('needs reconciliation')
  })
})

describe('native failure recovery', () => {
  it('does not sign when the owner lacks native funds at simulation', async () => {
    const { context, client, wallet } = fixture()
    client.simulateContract.mockRejectedValue(new Error('Insufficient funds for gas'))
    await expect(commitNativeClose(context, request)).rejects.toThrow('Insufficient funds')
    expect(wallet.writeContract).not.toHaveBeenCalled()
  })
  it('rejects orphaned inclusion without automatically resubmitting', async () => {
    const { context, client, wallet } = fixture()
    client.getBlock.mockResolvedValue({ number: 10n, timestamp: 100n, hash: `0x${'cd'.repeat(32)}` })
    await expect(commitNativeClose(context, request)).rejects.toThrow('inclusion changed')
    expect(wallet.writeContract).toHaveBeenCalledTimes(1)
  })
})
