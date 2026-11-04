import test from 'node:test'
import assert from 'node:assert/strict'
import { buildAttribution, EntryLedger } from '../accounting.mjs'
function snap(block, assets = 999999n, supply = 999999000n, extra = {}) {
  return { block: BigInt(block), timestamp: BigInt(block) * 3600n, assets, supply, rawSupply: supply,
    sharePrice: (assets + 1n) * 10n ** 21n / (supply + 1000n), rawSenior: 0n,
    rawJunior: assets, highWaterMark: 0n, couponCheckpoint: 0n, seniorRateBps: 0n,
    terminalPnl: 0n, markFresh: true, ...extra }
}
const near = (actual, expected) => assert.ok(Math.abs(actual - expected) < 1e-10, `${actual} != ${expected}`)
const sumPp = report => Object.values(report.components).reduce((sum, value) => sum + value.contributionPp, 0)
const report = (start, entries) => buildAttribution(start, entries.at(-1).snapshot, entries)
test('income around a deposit uses contemporaneous supply, not average capital', () => {
  const result = report(snap(1), [
    { snapshot: snap(2, 1099999n), events: [{ sources: { netVpi: 100000n } }] },
    { snapshot: snap(3, 2199999n, 1999999000n), events: [{ shareDelta: 1000000000n, assetDelta: 1100000n }] },
    { snapshot: snap(4, 2419999n, 1999999000n), events: [{ sources: { realizedCarry: 220000n } }] },
  ])
  near(result.components.netVpi.contributionPp, 10)
  near(result.components.realizedCarry.contributionPp, 11)
  near(sumPp(result), 21)
  assert.equal(result.netReturnUsdg, '320000')
  assert.deepEqual(result.notes, [])
})
test('withdrawals are neutral; subsequent income belongs to remaining shares', () => {
  const result = report(snap(1), [
    { snapshot: snap(2, 499999n, 499999000n), events: [{ shareDelta: -500000000n, assetDelta: -500000n }] },
    { snapshot: snap(3, 549999n, 499999000n), events: [{ sources: { liquidationFees: 50000n } }] },
  ])
  near(sumPp(result), 10)
  assert.equal(result.netReturnUsdg, '50000')
})
test('same-block cash events use capital-flow log ordering', () => {
  const result = report(snap(1), [{ snapshot: snap(2, 2299999n, 1999999000n), events: [
    { sources: { netVpi: 100000n } }, { shareDelta: 1000000000n, assetDelta: 1100000n },
    { sources: { realizedCarry: 100000n } },
  ] }])
  near(result.components.netVpi.contributionPp, 10)
  near(result.components.realizedCarry.contributionPp, 5)
  near(sumPp(result), 15)
  assert.equal(result.netReturnUsdg, '200000')
})
test('issued shares already accrued in effective supply are not diluted twice', () => {
  const start = snap(1), accrued = snap(2, start.assets, 1099999000n, { rawSupply: start.rawSupply })
  const issued = snap(3, start.assets, accrued.supply)
  const result = report(start, [{ snapshot: accrued, events: [] }, { snapshot: issued, events: [] }])
  near(result.components.maintenanceDilution.contributionPp, -100 / 11)
  assert.equal(result.components.maintenanceDilution.amountUsdg, '-90909')
  near(sumPp(result), Number(issued.sharePrice - start.sharePrice) / Number(start.sharePrice) * 100)
})
test('valuation is measured independently and unknown changes remain explicit', () => {
  const result = report(snap(1), [{ snapshot: snap(2, 1049999n, 999999000n, { terminalPnl: 20000n }), events: [] }])
  near(result.components.openPositionValuation.contributionPp, 2)
  assert.equal(result.netReturnUsdg, '50000')
  assert.ok(result.notes.some(note => note.includes('reconciliation difference')))
  near(sumPp(result), 2)
})
test('ambiguous intra-block valuation timing remains in an explained residual', () => {
  const result = report(snap(1), [{ snapshot: snap(2, 2019999n, 1999999000n, { terminalPnl: 20000n }), events: [{ shareDelta: 1000000000n, assetDelta: 1000000n }] }])
  assert.equal(result.components.openPositionValuation.amountUsdg, '0')
  assert.ok(result.notes.some(note => note.includes('intra-block timing')))
})
test('coupon accrual and checkpoint payment are not charged twice', () => {
  const extra = { rawSenior: 315360000n, highWaterMark: 315360000n, seniorRateBps: 100n, couponCheckpoint: 3600n }
  const start = snap(1, 999999n, 999999000n, extra)
  const pending = snap(2, 999639n, 999999000n, { ...extra, rawJunior: 999999n })
  const paid = snap(3, 999279n, 999999000n, { ...extra, rawSenior: 315360720n, highWaterMark: 315360720n, couponCheckpoint: 10800n })
  const result = report(start, [{ snapshot: pending, events: [] }, { snapshot: paid, events: [] }])
  assert.equal(result.components.seniorCoupon.amountUsdg, '-720')
  near(result.components.seniorCoupon.contributionPp, -0.072)
})
test('impairment, stale period boundaries and missing boundaries fail closed', () => {
  const start = snap(1)
  for (const extra of [{ markFresh: false }, { rawSenior: 1n, highWaterMark: 2n }])
    assert.throws(() => report(start, [{ snapshot: snap(2, 999999n, 999999000n, extra), events: [] }]))
  assert.throws(() => buildAttribution(start, snap(3), [{ snapshot: snap(2), events: [] }]), /boundary missing/)
})
const account = '0x' + '12'.repeat(20)
const open = lots => ({ kind: 'Open', markPrice: 100n, delta: { account, sizeDelta: lots * 10n ** 20n, newPosSize: lots * 10n ** 20n, posSide: 0n, posVpiAccruedDelta: 5n } })
const close = (lots, mark = 120n, extra = {}) => ({ kind: 'Close', markPrice: mark, frozenSpreadPaid: 0n, delta: {
  account, sizeDelta: lots * 10n ** 20n, side: 0n, pricePnlClaimConsumedAssets: 0n,
  pricePnlPledgeConsumedAssets: 22n * lots, priceGainAssets: 0n, pendingCarryAssets: 0n, realizedCarryAssets: 0n,
  actionChargeWithheldAssets: 0n, actionChargeCollectedAssets: 0n, actionChargeAssessedAssets: 0n,
  vpiRebateReserveBeforeAssets: 0n, vpiRebateReserveAfterAssets: 0n, executionFeeAssets: 0n, actionRebatePaidAssets: 0n,
  closeState: { vpiDeltaAssets: 0n, executionFeeAssets: 0n, frozenSpreadAssets: 0n }, ...extra,
} })
test('entry history separates confidence from PnL across partial closes', () => {
  const ledger = new EntryLedger(); ledger.apply(open(10n))
  const first = ledger.apply(close(4n)), second = ledger.apply(close(6n))
  assert.equal(first.settledTraderPnl, 80n); assert.equal(first.oracleSpread, 8n)
  assert.equal(second.settledTraderPnl, 120n); assert.equal(second.oracleSpread, 12n)
  assert.equal(ledger.positions.size, 0)
})
test('trader gains are negative and uncollectible losses never become earnings', () => {
  const ledger = new EntryLedger(); ledger.apply(open(10n))
  const gain = ledger.apply(close(5n, 90n, { pricePnlPledgeConsumedAssets: 0n, priceGainAssets: 45n }))
  assert.equal(gain.settledTraderPnl, -50n); assert.equal(gain.oracleSpread, 5n)
  const capped = ledger.apply(close(5n, 120n, { pricePnlPledgeConsumedAssets: 20n }))
  assert.equal(capped.settledTraderPnl, 20n); assert.equal(capped.oracleSpread, 0n)
})
test('only collected frozen spread/carry are recognized, excluding prior carry realization', () => {
  const ledger = new EntryLedger(); ledger.apply(open(1n))
  const action = close(1n, 120n, { pendingCarryAssets: 10n, realizedCarryAssets: 4n,
    actionChargeCollectedAssets: 13n, actionChargeAssessedAssets: 19n, executionFeeAssets: 2n,
    closeState: { vpiDeltaAssets: 3n, executionFeeAssets: 2n, frozenSpreadAssets: 8n } })
  action.frozenSpreadPaid = 2n
  const sources = ledger.apply(action)
  assert.equal(sources.realizedCarry, 6n); assert.equal(sources.netVpi, 3n)
  assert.equal(sources.oracleSpread, 4n)
})
test('liquidations separate LP fees from recovered VPI/carry', () => {
  const ledger = new EntryLedger(); ledger.apply(open(1n))
  const action = close(1n); action.kind = 'Liquidation'; action.delta.posSize = action.delta.sizeDelta
  Object.assign(action.delta, { pendingCarryAssets: 10n, realizedCarryAssets: 3n, actionChargeAssessedAssets: 12n,
    actionChargeCollectedAssets: 9n, lpLiquidationFeeAssets: 6n })
  const sources = ledger.apply(action)
  assert.equal(sources.liquidationFees, 6n); assert.equal(sources.netVpi, 5n); assert.equal(sources.realizedCarry, 4n)
})

test('stale intermediate pool fallbacks do not turn preview changes into trading income', () => {
  const start = snap(1), intermediate = snap(2, 799999n, 999999000n, { markFresh: false })
  const end = snap(3)
  const result = report(start, [{ snapshot: intermediate, events: [] }, { snapshot: end, events: [] }])
  near(sumPp(result), 0); assert.equal(result.netReturnUsdg, '0')
  assert.throws(() => report(start, [{ snapshot: { ...intermediate, supply: 1099999000n }, events: [] }, { snapshot: { ...end, supply: 1099999000n }, events: [] }]), /Maintenance dilution requires fresh/)
})
