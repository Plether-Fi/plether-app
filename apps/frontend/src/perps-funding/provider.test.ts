import { describe, expect, it, vi } from 'vitest'
import { decodeFunctionData, encodeAbiParameters, encodeFunctionData, erc20Abi, getAddress, pad, parseAbiParameters, zeroAddress, type Hex } from 'viem'
import fixture from './acrossQuote.fixture.json'
import { ACROSS_FUNDING_ABI } from './acrossAbi'
import { prepareFundingTransactions, sendFundingTransactions, type FundingWallet } from './provider'
import { ACROSS_ETHEREUM_SPOKE_POOL, ACROSS_PERIPHERY, ETHEREUM_USDT } from './sources'
import { parseFundingManifest } from './validation'
import { destinationFixture, quoteFixture, releaseFixture, OWNER, RECEIVER, SOURCE_HASH, OTHER_ADDRESS } from './testFixtures'
import type { FundingQuote, SourceTransaction } from './types'

const manifest = parseFundingManifest(releaseFixture())
const destination = destinationFixture()
function directQuote(): FundingQuote {
  const quote = quoteFixture()
  const data = encodeFunctionData({ abi: ACROSS_FUNDING_ABI, functionName: 'depositV3', args: [OWNER, RECEIVER, quote.sourceToken, quote.token, BigInt(quote.sourceAmount), BigInt(quote.minimumAmount), 42161n, zeroAddress, quote.expiresAt - 180, quote.expiresAt + 300, 0, '0x'] })
  return { ...quote, sourceTransactions: [
    { kind: 'approval', chainId: 1, to: quote.sourceToken, value: '0', data: encodeFunctionData({ abi: erc20Abi, functionName: 'approve', args: [ACROSS_ETHEREUM_SPOKE_POOL, 2n ** 256n - 1n] }) },
    { kind: 'bridge', chainId: 1, to: ACROSS_ETHEREUM_SPOKE_POOL, data, value: '0' },
  ] }
}
function reviewedQuote() {
  const tx: SourceTransaction = { kind: 'bridge', chainId: 1, to: ACROSS_PERIPHERY, data: fixture.swapTx.data as Hex, value: '0' }
  const decoded = decodeFunctionData({ abi: ACROSS_FUNDING_ABI, data: tx.data })
  if (decoded.functionName !== 'swapAndBridge') throw new Error('Unexpected fixture selector')
  const d = { ...destination, owner: decoded.args[0].depositData.depositor }
  const quote: FundingQuote = { ...quoteFixture(), ownerAddress: d.owner, receiver: '0x0000000000000000000000000000000000000002', sourceToken: ETHEREUM_USDT, sourceAmount: fixture.inputAmount, expiresAt: Math.min(fixture.quoteExpiryTimestamp, decoded.args[0].depositData.quoteTimestamp + 180), minimumAmount: fixture.minOutputAmount, estimatedAmount: fixture.expectedOutputAmount, sourceTransactions: [tx] }
  return { quote, destination: d, decoded, now: (quote.expiresAt - 10) * 1000 }
}

describe('Across source signing boundary', () => {
  it('accepts the actual read-only USDT swap quote with exact destination transfer-and-log recipe', () => {
    const { quote, destination, now } = reviewedQuote()
    expect(prepareFundingTransactions(quote, destination, manifest, now)).toEqual(quote.sourceTransactions)
  })
  it('rejects a destination message that drains to anyone other than the quoted receiver', () => {
    const { quote, destination, now } = reviewedQuote()
    expect(() => prepareFundingTransactions({ ...quote, receiver: RECEIVER }, destination, manifest, now)).toThrow()
  })
  it('rejects extra destination calls even at the reviewed handler', () => {
    const { quote, destination, now, decoded } = reviewedQuote()
    const abi = parseAbiParameters('((address target,bytes callData,uint256 value)[] calls,address fallbackRecipient)')
    const swap = decoded.args[0]
    const data = encodeFunctionData({ abi: ACROSS_FUNDING_ABI, functionName: 'swapAndBridge', args: [{ ...swap, depositData: { ...swap.depositData, message: encodeAbiParameters(abi, [{ calls: [], fallbackRecipient: zeroAddress }]) } }] })
    expect(() => prepareFundingTransactions({ ...quote, sourceTransactions: [{ ...quote.sourceTransactions[0], data }] }, destination, manifest, now)).toThrow()
  })
  it('rewrites unlimited approvals to the reviewed amount and rejects another spender', () => {
    const quote = directQuote()
    const prepared = prepareFundingTransactions(quote, destination, manifest, 1_999_999_999_000)
    expect(decodeFunctionData({ abi: erc20Abi, data: prepared[0].data }).args).toEqual([getAddress(ACROSS_ETHEREUM_SPOKE_POOL), 100000000n])
    quote.sourceTransactions[0].data = encodeFunctionData({ abi: erc20Abi, functionName: 'approve', args: [OTHER_ADDRESS, 1n] })
    expect(() => prepareFundingTransactions(quote, destination, manifest, 1_999_999_999_000)).toThrow()
  })
  it('rejects expired quotes, positive native value, changed destination and wrong source chain', () => {
    const quote = directQuote()
    expect(() => prepareFundingTransactions(quote, destination, manifest, quote.expiresAt * 1000)).toThrow()
    for (const bad of [{ ...quote, destinationChainId: 421614 }, { ...quote, beneficiary: OTHER_ADDRESS }, { ...quote, sourceChainId: 10 }, { ...quote, sourceTransactions: quote.sourceTransactions.map(t => ({ ...t, value: '1' })) }]) {
      expect(() => prepareFundingTransactions(bad, destination, manifest, 1_999_999_999_000)).toThrow()
    }
  })
  it('does not send when the source wallet owner changes after approval', async () => {
    const quote = directQuote()
    let sends = 0
    const wallet: FundingWallet = { request: vi.fn(async ({ method }) => {
      if (method === 'eth_accounts') return [sends ? OTHER_ADDRESS : OWNER]
      if (method === 'eth_chainId') return '0x1'
      if (method === 'eth_sendTransaction') { sends++; return SOURCE_HASH }
      return null
    }) }
    const beforeBridge = vi.fn()
    await expect(sendFundingTransactions({ wallet, quote, destination, manifest, beforeBridge, onBridgeHash: vi.fn(), onStep: vi.fn() })).rejects.toThrow('Reconnect')
    expect(sends).toBe(1)
    expect(beforeBridge).not.toHaveBeenCalled()
  })
  it('persists the ambiguous-send marker before requesting the bridge and the hash after broadcast', async () => {
    const quote = directQuote()
    const order: string[] = []
    const wallet: FundingWallet = { request: vi.fn(async ({ method, params }) => {
      if (method === 'eth_accounts') return [OWNER]
      if (method === 'eth_chainId') return '0x1'
      if (method === 'eth_getTransactionReceipt') return { status: '0x1' }
      if (method === 'eth_sendTransaction') { order.push((params?.[0] as { to: string }).to.toLowerCase() === ACROSS_ETHEREUM_SPOKE_POOL ? 'bridge-send' : 'approval-send'); return SOURCE_HASH }
      return null
    }) }
    await sendFundingTransactions({ wallet, quote, destination, manifest, beforeBridge: () => order.push('persist-pending'), onBridgeHash: () => order.push('persist-hash'), onStep: vi.fn() })
    expect(order).toEqual(['approval-send', 'persist-pending', 'bridge-send', 'persist-hash'])
  })
  it('rejects a swap that changes output token despite unchanged outer target', () => {
    const { quote, destination, now, decoded } = reviewedQuote()
    const swap = decoded.args[0]
    const data = encodeFunctionData({ abi: ACROSS_FUNDING_ABI, functionName: 'swapAndBridge', args: [{ ...swap, depositData: { ...swap.depositData, outputToken: pad(OTHER_ADDRESS) } }] })
    expect(() => prepareFundingTransactions({ ...quote, sourceTransactions: [{ ...quote.sourceTransactions[0], data }] }, destination, manifest, now)).toThrow()
  })
})
