import fs from 'node:fs'
import assert from 'node:assert/strict'
import { execFileSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import { createPublicClient, http, keccak256, parseUnits } from 'viem'

// Read-only reproduction: calls the deployed pure planner and evaluator with
// explicitly synthetic account snapshots. Never submits a transaction.
const release = JSON.parse(fs.readFileSync(new URL('../../../config/perps/arbitrum-sepolia-v2.json', import.meta.url)))
const bundle = process.argv[2]
if (!bundle) throw new Error('Usage: node scripts/reproduce-close-review.mjs <pinned ABI bundle.tar.gz>')
if (createHash('sha256').update(fs.readFileSync(bundle)).digest('hex') !== release.release.bundleSha256) throw new Error('ABI bundle checksum mismatch')
const abi = name => JSON.parse(execFileSync('tar', ['-xOzf', bundle, `perps-${release.release.version}-arbitrum-sepolia/abi/${name}.json`], { encoding: 'utf8' }))
const plannerAbi = abi('CfdEnginePlanner')
const evaluatorAbi = abi('CfdOrderPolicyEvaluator')
const engineAbi = abi('CfdEngine')
const routerAbi = abi('ArbitrumSepoliaReleaseRouter')
const client = createPublicClient({ transport: http('https://sepolia-rollup.arbitrum.io/rpc') })
// Pin deployed parameters and code to the block used for the recorded reproduction.
const block = await client.getBlock({ blockNumber: 308891788n })
for (const key of ['cfdEnginePlanner', 'cfdOrderPolicyEvaluator']) {
  if (keccak256(await client.getCode({ address: release.contracts[key].address, blockNumber: block.number })) !== release.contracts[key].runtimeCodeHash) throw new Error(`Deployed ${key} bytecode mismatch`)
}
const read = (key, contractAbi, functionName, args = []) => client.readContract({
  address: release.contracts[key].address, abi: contractAbi, functionName, args, blockNumber: block.number,
})
const [riskValues, bounty, executionFeeBps] = await Promise.all([
  read('cfdEngine', engineAbi, 'riskParams'),
  read('orderRouter', routerAbi, 'closeOrderExecutionBountyUsdc'),
  read('cfdEngine', engineAbi, 'executionFeeBps'),
])
const planFunction = plannerAbi.find(item => item.name === 'planClose')
function empty(parameter) {
  if (parameter.type.endsWith('[]')) return []
  if (parameter.type === 'tuple') return Object.fromEntries(parameter.components.map(item => [item.name, empty(item)]))
  if (parameter.type === 'bool') return false
  if (parameter.type === 'address') return '0x0000000000000000000000000000000000000001'
  if (parameter.type.startsWith('bytes')) return `0x${'00'.repeat(Number(parameter.type.slice(5)))}`
  return 0n
}
const SIZE = parseUnits('15927400', 18)
// Inferred from rounded exposure 16,103,387.58 / quantity; not an exact oracle tick.
const PRICE = 98_895_064n
const CAP = 200_000_000n
const LOTS = SIZE / 10n ** 20n
const targetPrice = PRICE * 999n / 1000n
const boundsParameter = evaluatorAbi.find(item => item.name === 'evaluateClose').inputs[2]
const bounds = empty(boundsParameter)
for (const field of boundsParameter.components) {
  if (field.name.startsWith('max')) bounds[field.name] = (1n << BigInt(field.type.slice(4))) - 1n
}
bounds.allowedExecutionModes = 7

function snapshot(marginUsdc) {
  const snap = empty(planFunction.inputs[0])
  snap.currentTimestamp = block.timestamp
  snap.lastMarkTime = block.timestamp
  snap.lastMarkPrice = PRICE
  snap.capPrice = CAP
  snap.executionFeeBps = executionFeeBps
  snap.poolAssetsUsdc = parseUnits('1000000000', 6)
  snap.poolCashUsdc = snap.poolAssetsUsdc
  const fields = planFunction.inputs[0].components.find(item => item.name === 'riskParams').components
  snap.riskParams = Array.isArray(riskValues)
    ? Object.fromEntries(fields.map((item, index) => [item.name, riskValues[index]])) : riskValues
  Object.assign(snap.position, { size: SIZE, margin: marginUsdc, entryPrice: PRICE,
    side: 1, maxProfitUsdc: LOTS * (CAP - PRICE), lastUpdateTime: block.timestamp, lastCarryTimestamp: block.timestamp })
  snap.positionEntryCostUsdcAtoms = LOTS * PRICE
  Object.assign(snap.shortSide, { maxProfitUsdc: snap.position.maxProfitUsdc, openInterest: SIZE,
    entryNotional: snap.positionEntryCostUsdcAtoms * 10n ** 12n, totalMargin: marginUsdc })
  Object.assign(snap.accountBuckets, { settlementBalanceUsdc: marginUsdc, totalLockedMarginUsdc: marginUsdc, activePositionMarginUsdc: marginUsdc })
  Object.assign(snap.lockedBuckets, { positionMarginUsdc: marginUsdc, totalLockedMarginUsdc: marginUsdc })
  return snap
}
const output = { release: release.release.version, blockNumber: block.number,
  screenshot: { size: SIZE, inferredRawPrice: PRICE, targetPrice, displayedPrice: CAP - PRICE,
    exposureUsdc: LOTS * (CAP - PRICE), displayedExecutionLimit: CAP - targetPrice },
  assumptions: 'Synthetic full short close; entry price equals current price; no pending orders, carry or claims; one-sided pool with 1 billion USDC; varied remaining settlement/margin. Not the screenshot account.',
  riskParams: snapshot(0n).riskParams, cases: [] }
for (const margin of ['3000000', '10000']) {
 for (const assessedPrice of [PRICE, targetPrice + (PRICE - targetPrice) / 2n, targetPrice]) {
  const snap = snapshot(parseUnits(margin, 6))
  const order = { account: snap.account, sizeDelta: SIZE, marginDelta: 0n, targetPrice,
    commitTime: block.timestamp, commitBlock: block.number, orderId: 0n, side: 1, isClose: true }
  const result = { marginUsdc: snap.position.margin, assessedRawPrice: assessedPrice, executionBountyUsdc: bounty }
  try {
    const delta = await read('cfdEnginePlanner', plannerAbi, 'planClose', [snap, order, assessedPrice, block.timestamp])
    result.planner = { valid: delta.valid, revertCode: delta.revertCode, executionFeeUsdc: delta.closeState.executionFeeUsdc,
      realizedCarryUsdc: delta.realizedCarryUsdc, pricePnlPledgeConsumedUsdc: delta.pricePnlPledgeConsumedUsdc,
      actionChargeCollectedUsdc: delta.actionChargeCollectedUsdc, actionRebatePaidUsdc: delta.actionRebatePaidUsdc,
      actionChargeWaivedUsdc: delta.actionChargeWaivedUsdc, deletePosition: delta.deletePosition }
    result.stage = 'evaluateClose'
    const assessment = await read('cfdOrderPolicyEvaluator', evaluatorAbi, 'evaluateClose', [snap, delta, bounds, bounty])
    result.assessment = { postSettlementBalanceUsdc: assessment.postSettlementBalanceUsdc, postPositionSize: assessment.postPositionSize }
  } catch (error) {
    const cause = error.walk?.(item => item.name === 'ContractFunctionRevertedError')
    result.error = { name: cause?.data?.errorName, args: cause?.data?.args, raw: cause?.raw, message: error.shortMessage }
  }
  output.cases.push(result)
 }
}
assert(output.cases.slice(0, 3).every(result => result.assessment), 'Funded control must pass at every reviewed price')
assert(output.cases.slice(3, 5).every(result => result.assessment), 'Low-balance case must pass at the current and midpoint prices')
const failure = output.cases[5]
assert.equal(failure.planner?.valid, true)
assert.equal(failure.error?.name, 'Panic')
assert.equal(failure.error?.args?.[0], 17n)
console.log(JSON.stringify(output, (_, value) => typeof value === 'bigint' ? value.toString() : value, 2))
