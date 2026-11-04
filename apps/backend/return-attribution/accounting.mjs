export class AttributionError extends Error { constructor(message) { super(message); this.name = 'AttributionError' } }
// All amounts are raw six-decimal USDG and supplies are raw nine-decimal shares.
// Fixed-point pp accumulation avoids converting EVM integers to floating point.
export const COMPONENTS = ['netVpi', 'liquidationFees', 'realizedCarry', 'oracleSpread', 'settledTraderPnl', 'openPositionValuation', 'seniorCoupon', 'maintenanceDilution']
export const PP_SCALE = 10n ** 18n
const WAD_PRICE_FACTOR = 10n ** 21n
const VIRTUAL_SHARES = 1000n
const YEAR = 31536000n
const SIZE_QUANTUM = 10n ** 20n
const min = (a, b) => a < b ? a : b
const positive = x => x > 0n ? x : 0n
const bigint = x => BigInt(x)
export const json = value => JSON.stringify(value, (_key, item) => typeof item === 'bigint' ? item.toString() : item)

export function neutralPricePnl(side, lots, entryCost, mark) {
  const longPoolPnl = lots * mark - entryCost
  return BigInt(side) === 0n ? longPoolPnl : -longPoolPnl
}

// Replay from deployment, not just from the displayed window: otherwise a close
// could incorrectly treat its historical entry confidence as trader PnL.
export class EntryLedger {
  constructor() { this.positions = new Map() }
  apply(action) {
    const d = action.delta
    const account = d.account.toLowerCase()
    const old = this.positions.get(account)
    const sources = Object.fromEntries(COMPONENTS.map(id => [id, 0n]))
    if (action.kind === 'Open') {
      const lots = bigint(d.sizeDelta) / SIZE_QUANTUM
      if (lots <= 0n || lots * SIZE_QUANTUM !== bigint(d.sizeDelta)) throw new AttributionError('Invalid lot quantum')
      if (bigint(d.newPosSize) % SIZE_QUANTUM !== 0n || (old && old.side !== BigInt(d.posSide)) || (old?.lots ?? 0n) + lots !== bigint(d.newPosSize) / SIZE_QUANTUM) throw new AttributionError('Entry ledger prefix missing')
      this.positions.set(account, { lots: (old?.lots ?? 0n) + lots, cost: (old?.cost ?? 0n) + lots * action.markPrice, side: BigInt(d.posSide) })
      // A successful open has no fail-soft VPI collection path.
      sources.netVpi = bigint(d.posVpiAccruedDelta)
      return sources
    }
    const size = bigint(action.kind === 'Close' ? d.sizeDelta : d.posSize)
    if (size % SIZE_QUANTUM !== 0n) throw new AttributionError('Invalid close lot quantum')
    const lots = size / SIZE_QUANTUM
    if (!old || lots <= 0n || lots > old.lots || old.side !== BigInt(d.side)) throw new AttributionError('Close has no canonical entry prefix')
    const entry = old.cost * lots / old.lots
    const rawPoolPnl = neutralPricePnl(old.side, lots, entry, action.markPrice)
    const collectedLoss = bigint(d.pricePnlClaimConsumedAssets) + bigint(d.pricePnlPledgeConsumedAssets)
    const settledPricePnl = collectedLoss - bigint(d.priceGainAssets)
    // A confidence-neutral loss must still respect actual collectible backing.
    // Trader gains include claims and withholding; subsequent cash payment of a
    // previously recognized trader claim is not a second trading loss.
    const neutralSettledPnl = rawPoolPnl > 0n ? min(rawPoolPnl, collectedLoss) : rawPoolPnl
    sources.settledTraderPnl = neutralSettledPnl
    sources.oracleSpread = settledPricePnl - neutralSettledPnl
    if (sources.oracleSpread < 0n) throw new AttributionError('Confidence attribution is inconsistent with adverse execution')
    const recovered = bigint(d.actionChargeWithheldAssets) + bigint(d.actionChargeCollectedAssets)
    const remainingCarry = bigint(d.pendingCarryAssets) - bigint(d.realizedCarryAssets)
    if (remainingCarry < 0n) throw new AttributionError('Collected carry exceeds assessed carry')
    if (action.kind === 'Liquidation') {
      const clawback = bigint(d.actionChargeAssessedAssets) - remainingCarry
      if (clawback < 0n) throw new AttributionError('Invalid liquidation action charges')
      sources.netVpi = min(clawback, recovered)
      sources.realizedCarry = min(remainingCarry, positive(recovered - sources.netVpi))
      sources.liquidationFees = bigint(d.lpLiquidationFeeAssets)
    } else {
      const vpi = bigint(d.closeState.vpiDeltaAssets)
      const protectedVpi = min(positive(vpi), bigint(d.vpiRebateReserveBeforeAssets) - bigint(d.vpiRebateReserveAfterAssets))
      const fee = bigint(d.executionFeeAssets)
      const unprotected = positive(recovered - protectedVpi - fee)
      sources.realizedCarry = min(remainingCarry, unprotected)
      sources.netVpi = protectedVpi + min(positive(vpi - protectedVpi), positive(unprotected - sources.realizedCarry)) - bigint(d.actionRebatePaidAssets)
      // Mirrors the deployed sidecar's recovered-spread priority, including
      // negative-VPI offsets. Assessed/waived amounts are not earnings.
      const prior = bigint(d.closeState.executionFeeAssets) + remainingCarry + positive(vpi)
      const effectiveSpread = min(bigint(d.closeState.frozenSpreadAssets), positive(bigint(d.actionChargeAssessedAssets) - prior))
      const paidSpread = min(effectiveSpread, positive(recovered - prior))
      if (paidSpread !== action.frozenSpreadPaid) throw new AttributionError('Frozen spread receipt/trace mismatch')
      sources.oracleSpread += paidSpread
    }
    if (lots === old.lots) this.positions.delete(account)
    else this.positions.set(account, { ...old, lots: old.lots - lots, cost: old.cost - entry })
    return sources
  }
}

export function pendingCoupon(snapshot) {
  const elapsed = snapshot.timestamp > snapshot.couponCheckpoint ? snapshot.timestamp - snapshot.couponCheckpoint : 0n
  return snapshot.rawSupply > 0n ? min(snapshot.rawJunior, snapshot.rawSenior * snapshot.seniorRateBps * elapsed / (10000n * YEAR)) : 0n
}

export function buildAttribution(start, end, transitions) {
  if (start.sharePrice <= 0n || end.timestamp <= start.timestamp) throw new AttributionError('Invalid return boundaries')
  if (!start.markFresh || !end.markFresh) throw new AttributionError('Attribution requires fresh period boundaries')
  const amounts = Object.fromEntries(COMPONENTS.map(id => [id, 0n]))
  const pp = Object.fromEntries(COMPONENTS.map(id => [id, 0n]))
  const notes = new Set()
  let netReturnUsdg = 0n
  let previous = start
  const add = (id, amount, supply) => {
    if (supply <= 0n) throw new AttributionError('Cannot attribute return without LP shares')
    amounts[id] += amount
    pp[id] += amount * WAD_PRICE_FACTOR * 100n * PP_SCALE / ((supply + VIRTUAL_SHARES) * start.sharePrice)
  }
  for (const { snapshot: current, events } of transitions) {
    if (current.block <= previous.block || current.timestamp < previous.timestamp || current.block > end.block) throw new AttributionError('Noncanonical transition order')
    // A non-coupon senior recovery/loss requires source-specific waterfall
    // allocation. Do not publish gross pool revenue as Junior-owned income.
    if (current.rawSenior < current.highWaterMark || previous.rawSenior < previous.highWaterMark) throw new AttributionError('Senior impairment requires separate waterfall allocation')
    const flowShares = events.reduce((sum, event) => sum + (event.shareDelta ?? 0n), 0n)
    const flowAssets = events.reduce((sum, event) => sum + (event.assetDelta ?? 0n), 0n)
    const beforeFlowsSupply = current.supply - flowShares
    if (beforeFlowsSupply < previous.supply) throw new AttributionError('Unexplained non-capital supply reduction')
    const feeShares = beforeFlowsSupply - previous.supply
    if (feeShares > 0n) {
      if (!current.markFresh || !previous.markFresh) throw new AttributionError('Maintenance dilution requires fresh valuation around fee accrual')
      const dilution = -(previous.assets + 1n) * feeShares / (beforeFlowsSupply + VIRTUAL_SHARES)
      amounts.maintenanceDilution += dilution
      pp.maintenanceDilution += -(previous.assets + 1n) * WAD_PRICE_FACTOR * feeShares * 100n * PP_SCALE
        / ((previous.supply + VIRTUAL_SHARES) * (beforeFlowsSupply + VIRTUAL_SHARES) * start.sharePrice)
      netReturnUsdg += dilution
    }
    let supply = beforeFlowsSupply
    const flows = events.filter(event => event.shareDelta !== undefined)
    const hasValuationTiming = flows.length > 0 && flows.every(event => typeof event.terminalPnlBeforeFlow === 'bigint')
    let terminalPnl = previous.terminalPnl
    for (const event of events) {
      if (event.shareDelta !== undefined) {
        if (hasValuationTiming) {
          add('openPositionValuation', event.terminalPnlBeforeFlow - terminalPnl, supply)
          terminalPnl = event.terminalPnlBeforeFlow
        }
        supply += event.shareDelta
      }
      if (event.sources) for (const id of COMPONENTS) if (event.sources[id]) add(id, event.sources[id], supply)
    }
    if (supply !== current.supply) throw new AttributionError('Share supply does not reconcile')
    let paidCoupon = 0n
    if (current.couponCheckpoint > previous.couponCheckpoint) {
      paidCoupon = min(previous.rawJunior, previous.rawSenior * previous.seniorRateBps * (current.couponCheckpoint - previous.couponCheckpoint) / (10000n * YEAR))
    } else if (current.couponCheckpoint < previous.couponCheckpoint) throw new AttributionError('Coupon checkpoint regressed')
    const coupon = paidCoupon + pendingCoupon(current) - pendingCoupon(previous)
    if (coupon < 0n) throw new AttributionError('Coupon accrual did not reconcile')
    add('seniorCoupon', -coupon, beforeFlowsSupply)
    const valuation = current.terminalPnl - previous.terminalPnl
    if (hasValuationTiming) {
      add('openPositionValuation', current.terminalPnl - terminalPnl, supply)
    } else if (flows.length > 0 && valuation !== 0n) {
      // End-of-block state alone cannot place a mark/curve move around multiple
      // liquidity changes in one block. Keep this in the explicit residual.
      notes.add('Open-position valuation in blocks with LP settlements is included in the unexplained reconciliation difference because its intra-block timing cannot be established from boundary states.')
    } else add('openPositionValuation', valuation, beforeFlowsSupply)
    netReturnUsdg += current.assets - previous.assets - flowAssets
    previous = current
  }
  if (previous.block !== end.block || previous.sharePrice !== end.sharePrice) throw new AttributionError('Attribution end boundary missing')
  const sourcesTotal = Object.values(amounts).reduce((sum, amount) => sum + amount, 0n)
  if (sourcesTotal !== netReturnUsdg) notes.add('Other economic changes or integer rounding remain in the explicit reconciliation difference; they have not been assigned to trader PnL or open-position valuation.')
  return {
    method: 'diluted-share-price-v1',
    startTimestamp: Number(start.timestamp), endTimestamp: Number(end.timestamp),
    startDilutedSharePrice: start.sharePrice.toString(), endDilutedSharePrice: end.sharePrice.toString(),
    components: Object.fromEntries(COMPONENTS.map(id => [id, { contributionPp: Number(pp[id]) / Number(PP_SCALE), amountUsdg: amounts[id].toString() }])),
    netReturnUsdg: netReturnUsdg.toString(),
    notes: [...notes],
  }
}
