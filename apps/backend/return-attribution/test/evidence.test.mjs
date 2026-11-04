import fs from 'node:fs'
import test from 'node:test'
import assert from 'node:assert/strict'
import { encodeFunctionData } from 'viem'
import { settlementAbi, TOPICS, decodeActions, decodeLpValuations, capitalEvent, topic } from '../evidence.mjs'
const address = number => '0x' + number.toString(16).padStart(40, '0')
const word = n => BigInt(n).toString(16).padStart(64, '0')
const data = values => '0x' + values.map(word).join('')
const account = address(10)
const release = { contracts: Object.fromEntries(['cfdEngine', 'cfdEngineSettlementSidecar', 'orderRouter', 'pletherOracle', 'housePool', 'juniorVault'].map((key, i) => [key, { address: address(i + 1) }])) }
const defaults = input => input.type === 'tuple' ? Object.fromEntries(input.components.map(field => [field.name, defaults(field)])) : input.type === 'bool' ? false : input.type === 'address' ? address(0) : 0n
function settlement(kind) {
  const entry = settlementAbi.find(item => item.name === 'execute' + kind)
  const args = entry.inputs.map(defaults)
  Object.assign(args[0], { account, price: 101n, sizeDelta: 10n ** 20n, posSize: 10n ** 20n, valid: true, liquidatable: true })
  if (kind === 'Liquidation') args[1] = 7n
  else args[2] = 7n
  return encodeFunctionData({ abi: settlementAbi, functionName: entry.name, args })
}
function evidence(kind = 'Open', shape = 'order') {
  const c = release.contracts
  const signatures = {
    order: 'updateOrderExecutionPrice(address,bytes[],(uint64,uint256,uint8,bool,bool))',
    batch: 'updateBatchOrderExecutionPrice(address,bytes[],(uint64,uint256,uint8,bool,bool),(bool,uint64,uint256,uint256,uint64,uint64))',
    liquidation: 'updateLiquidationBatchPrice(address,bytes[])',
  }
  const quote = shape === 'liquidation' ? [101n, 99n, 100n, 7n, 0n] : [1n, 101n, 100n, 7n, 0n, 60n, 0n, 0n, 0n, ...(shape === 'batch' ? Array(6).fill(0n) : [])]
  return {
    trace: { type: 'CALL', calls: [
      { type: 'CALL', from: c.orderRouter.address, to: c.pletherOracle.address, input: topic(signatures[shape]).slice(0, 10), output: data(quote) },
      { type: 'CALL', from: c.cfdEngine.address, to: c.cfdEngineSettlementSidecar.address, input: settlement(kind) },
    ] },
    logs: [{ address: c.cfdEngine.address, topics: [TOPICS[kind.toLowerCase()], data([10n])], data: data([0n, 10n ** 20n, 101n, 0n]), logIndex: '0x3' }],
  }
}
for (const [kind, shape] of [['Open', 'order'], ['Close', 'batch'], ['Liquidation', 'liquidation']]) {
  test(`${kind} decodes the pinned settlement ABI and ${shape} oracle return shape`, () => {
    const e = evidence(kind, shape), [action] = decodeActions(e.trace, e.logs, release)
    assert.equal(action.kind, kind); assert.equal(action.markPrice, 100n); assert.equal(action.logIndex, 3)
  })
}
test('forged caller, reverted ancestors, unmatched receipts and unsuccessful oracle results are rejected', () => {
  for (const mutate of [
    e => { e.trace.calls[1].from = address(99) },
    e => { e.trace.error = 'revert' },
    e => { e.logs[0].topics[1] = data([11n]) },
    e => { e.logs[0].data = data([0n, 1n, 101n, 0n]) },
    e => { e.trace.calls[0].output = data([0n, 101n, 100n, 7n, 0n, 60n, 0n, 0n, 0n]) },
    e => { e.trace.calls[0].from = address(99) },
  ]) {
    const e = evidence(); mutate(e); assert.throws(() => decodeActions(e.trace, e.logs, release))
  }
})
test('failed ancillary calls do not supply neutral prices', () => {
  const e = evidence();e.trace.calls[0] = { type: 'CALL', error: 'reverted', calls: [e.trace.calls[0]] }
  assert.throws(() => decodeActions(e.trace, e.logs, release), /neutral oracle quote/)
})
test('capital includes bootstrap assignment and excludes senior and unrecognized mints', () => {
  const c = release.contracts
  const log = { address: c.housePool.address, topics: [TOPICS.seed, data([0n]), data([10n])], data: data([100n, 100000n]), logIndex: '0x5' }
  assert.deepEqual(capitalEvent(log, release), { assetDelta: 100n, shareDelta: 100000n, logIndex: 5 })
  log.topics[0] = TOPICS.assigned
  assert.equal(capitalEvent(log, release).assetDelta, 100n)
  log.topics[1] = data([1n]); assert.equal(capitalEvent(log, release), undefined)
  log.address = c.juniorVault.address;log.topics[0] = TOPICS.redeem;log.data = data([100000n, 100n])
  assert.deepEqual(capitalEvent(log, release), { assetDelta: -100n, shareDelta: -100000n, logIndex: 5 })
})

test('decodes public deployed Open, Close and Liquidation receipts without regenerating their calldata', () => {
  const live = JSON.parse(fs.readFileSync(new URL('./fixtures/sepolia-settlements.json', import.meta.url)))
  const manifest = JSON.parse(fs.readFileSync(new URL('../../../../config/perps/arbitrum-sepolia-v2.0.0.json', import.meta.url)))
  assert.equal(live.sourceCommit, manifest.release.sourceCommit)
  assert.deepEqual(live.cases.map(item => item.kind).sort(), ['Close', 'Liquidation', 'Open'])
  for (const item of live.cases) {
    const [action] = decodeActions(item.trace, item.logs, manifest)
    assert.equal(action.kind, item.kind)
    assert.equal(action.markPrice.toString(), item.expectedMarkPrice)
    assert.equal(action.delta.price.toString(), item.expectedPrice)
    assert.equal(item.logs[0].transactionHash, item.transactionHash)
    assert.equal(item.logs[0].blockHash, item.blockHash)
  }
})

const lpFixture = () => JSON.parse(fs.readFileSync(new URL('./fixtures/sepolia-lp-valuation.json', import.meta.url)))
const deployedRelease = () => JSON.parse(fs.readFileSync(new URL('../../../../config/perps/arbitrum-sepolia-v2.0.0.json', import.meta.url)))
function eachCall(node, visit) { visit(node); for (const child of node.calls ?? []) eachCall(child, visit) }
test('the actual hourly LP settlement proves the entire previously unexplained valuation occurred before the deposit', () => {
  const e = lpFixture()
  const [boundary] = decodeLpValuations(e.trace, e.logs, deployedRelease())
  assert.equal(boundary.logIndex, 2)
  assert.equal(boundary.terminalPnlBeforeFlow - BigInt(e.previousTerminalPnl), BigInt(e.expectedValuationChangeUsdg))
  const contributionPp = Number(BigInt(e.expectedValuationChangeUsdg) * 10n ** 21n * 100n)
    / Number((BigInt(e.supplyBeforeFlow) + 1000n) * BigInt(e.periodStartDilutedSharePrice))
  assert.ok(Math.abs(contributionPp - 0.0004483698100118797) < 1e-15)
})
test('LP timing evidence rejects mismatched receipts, missing NAV and nested engine mutations', () => {
  const manifest = deployedRelease()
  for (const mutate of [
    e => { const receipt = e.logs.find(log => log.address === manifest.contracts.juniorVault.address && log.topics[0] === TOPICS.deposit); receipt.data = data([1n, 2n]) },
    e => eachCall(e.trace, node => { if (node.to === manifest.contracts.cfdEngine.address && node.input === topic('terminalNavSnapshot()').slice(0, 10)) node.error = 'reverted' }),
    e => eachCall(e.trace, node => { if (node.to === manifest.contracts.cfdEngine.address && node.input?.slice(0, 10) === topic('checkpointCarryIndexes()').slice(0, 10)) node.input = topic('updateMarkPrice(uint256,uint64)').slice(0, 10) + word(1n) + word(1n) }),
    e => { e.trace.error = 'reverted' },
  ]) {
    const e = lpFixture(); mutate(e)
    assert.throws(() => decodeLpValuations(e.trace, e.logs, manifest))
  }
})
