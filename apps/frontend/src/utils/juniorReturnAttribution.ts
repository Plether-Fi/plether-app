import type { JuniorReturnAttribution, JuniorReturnComponent, VaultHistory } from '../api/types'

export const JUNIOR_RETURN_COMPONENTS = [
  { id: 'netVpi', label: 'Net VPI', lines: ['Net VPI'], color: '#5095ee', description: 'VPI charges collected minus rebates paid.' },
  { id: 'liquidationFees', label: 'Liquidation fees', lines: ['Liquidation', 'fees'], color: '#20c7a0', description: 'The junior pool’s portion of collected liquidation fees.' },
  { id: 'realizedCarry', label: 'Realized carry', lines: ['Realized', 'carry'], color: '#ad83e0', description: 'Carry actually collected by the pool, excluding uncollected accruals.' },
  { id: 'oracleSpread', label: 'Oracle spread', lines: ['Oracle', 'spread'], color: '#f5bf59', description: 'Realized oracle spread, including confidence adjustments and frozen-market close spread. Excluded from settled trader PnL to avoid double counting.' },
  { id: 'settledTraderPnl', label: 'Settled trader PnL', lines: ['Settled trader', 'PnL'], color: '#ed7b9b', description: 'Settled PnL from the pool’s perspective: trader losses add to returns and trader profits reduce returns. Excludes separately attributed spread, fees and carry.' },
  { id: 'openPositionValuation', label: 'Open-position valuation', lines: ['Open-position', 'valuation'], color: '#7793a6', description: 'The effect of changes in outstanding positions’ valuation. This is an independently measured component, not a balancing adjustment.' },
  { id: 'seniorCoupon', label: 'Senior coupon', lines: ['Senior', 'coupon'], color: '#bfc9d7', description: 'Coupon allocated to the senior tranche, deducted from junior returns.' },
  { id: 'maintenanceDilution', label: 'Maintenance dilution', lines: ['Maintenance', 'dilution'], color: '#ee9c6c', description: 'Reduction in existing LP ownership from issued and accrued maintenance-fee shares. The USDG amount is the economic value of dilution, not a cash expense.' },
] as const satisfies readonly { id: JuniorReturnComponent; label: string; lines: readonly string[]; color: string; description: string }[]

export interface JuniorReturnPeriod {
  startTimestamp: number
  endTimestamp: number
  startDilutedSharePrice: string
  endDilutedSharePrice: string
}

export interface ReturnBar {
  id: string
  label: string
  lines: readonly string[]
  color: string
  description: string
  contributionPp: number
  amountUsdg: string | null
  start: number
  end: number
}

const unsignedInteger = (value: unknown): value is string => typeof value === 'string' && /^\d{1,78}$/.test(value)
const signedInteger = (value: unknown): value is string => typeof value === 'string' && /^-?\d{1,78}$/.test(value)
const isRecord = (value: unknown): value is Record<string, unknown> => typeof value === 'object' && value !== null && !Array.isArray(value)

function validAttribution(value: unknown): value is JuniorReturnAttribution {
  if (!isRecord(value) || value.method !== 'diluted-share-price-v1'
    || !signedInteger(value.netReturnUsdg) || !unsignedInteger(value.startDilutedSharePrice)
    || !unsignedInteger(value.endDilutedSharePrice) || !Number.isSafeInteger(value.startTimestamp)
    || !Number.isSafeInteger(value.endTimestamp) || !isRecord(value.components)
    || (value.notes !== undefined && (!Array.isArray(value.notes) || value.notes.length > 16
      || value.notes.some((note: unknown) => typeof note !== 'string' || note.length > 2000)))) return false
  const components = value.components
  return Object.keys(components).length === JUNIOR_RETURN_COMPONENTS.length
    && JUNIOR_RETURN_COMPONENTS.every(({ id }) => {
      const item = components[id]
      return isRecord(item) && typeof item.contributionPp === 'number' && Number.isFinite(item.contributionPp)
        && signedInteger(item.amountUsdg)
        && ((id !== 'seniorCoupon' && id !== 'maintenanceDilution') || (item.contributionPp <= 0 && BigInt(item.amountUsdg) <= 0n))
    })
}
// Far below displayed precision; only suppress arithmetic roundoff, not accounting differences.
const RECONCILIATION_TOLERANCE_PP = 1e-10

export function juniorReturnPeriod(history?: VaultHistory): JuniorReturnPeriod | undefined {
  if (!history?.coverage.complete) return undefined
  const points = history.junior.points
  const first = points.at(0)
  const last = points.at(-1)
  if (!first || !last || points.length < 2
    || first.timestamp !== history.coverage.start || last.timestamp !== history.coverage.end
    || points.some((point, index) => !Number.isSafeInteger(point.timestamp) || !point.markFresh || !unsignedInteger(point.totalSupply) || BigInt(point.totalSupply) <= 0n
      || !unsignedInteger(point.sharePrice) || (index > 0 && (point.timestamp <= points[index - 1].timestamp
        || point.timestamp - points[index - 1].timestamp > 5400)))) return undefined
  return {
    startTimestamp: first.timestamp, endTimestamp: last.timestamp,
    startDilutedSharePrice: first.sharePrice, endDilutedSharePrice: last.sharePrice,
  }
}

export function buildJuniorReturnBreakdown(period?: JuniorReturnPeriod, attribution?: unknown) {
  if (!period || !unsignedInteger(period.startDilutedSharePrice) || !unsignedInteger(period.endDilutedSharePrice)
    || !Number.isSafeInteger(period.startTimestamp) || period.startTimestamp < 0
    || !Number.isSafeInteger(period.endTimestamp) || period.endTimestamp <= period.startTimestamp) return null
  const startPrice = BigInt(period.startDilutedSharePrice)
  const endPrice = BigInt(period.endDilutedSharePrice)
  if (startPrice <= 0n) return null
  // Subtract losslessly before conversion, avoiding cancellation for small returns.
  const periodReturn = Number(endPrice - startPrice) / Number(startPrice)
  const returnPp = periodReturn * 100
  const elapsedSeconds = period.endTimestamp - period.startTimestamp
  const annualized = endPrice === 0n ? -100 : Math.expm1(Math.log1p(periodReturn) * (365 * 86400 / elapsedSeconds)) * 100
  const summary = { ...period, returnPp, elapsedSeconds, apyPercent: Number.isFinite(annualized) ? annualized : null }
  const unavailable = (reason: string) => ({ ...summary, bars: null, residualPp: null, notes: [] as string[], reason })
  if (!attribution) return unavailable('Component attribution is not available for this period. Period return is calculated from diluted share prices; dollar income totals cannot determine the breakdown when liquidity changes.')
  if (!validAttribution(attribution)) return unavailable('Attribution is incomplete or invalid. No missing component has been treated as zero.')
  if (attribution.startTimestamp !== period.startTimestamp || attribution.endTimestamp !== period.endTimestamp
    || attribution.startDilutedSharePrice !== period.startDilutedSharePrice || attribution.endDilutedSharePrice !== period.endDilutedSharePrice) {
    return unavailable('Attribution does not match the selected share-price period. The component breakdown is unavailable.')
  }
  let running = 0
  const bars: ReturnBar[] = JUNIOR_RETURN_COMPONENTS.map((component) => {
    const item = attribution.components[component.id]
    const start = running
    running += item.contributionPp
    return { ...component, ...item, start, end: running }
  })
  if (!Number.isFinite(running)) return unavailable('Attribution is outside the supported numeric range.')
  const residualPp = returnPp - running
  const residualUsdg = BigInt(attribution.netReturnUsdg) - JUNIOR_RETURN_COMPONENTS.reduce((sum, { id }) => sum + BigInt(attribution.components[id].amountUsdg), 0n)
  const hasResidual = Math.abs(residualPp) > RECONCILIATION_TOLERANCE_PP || residualUsdg !== 0n
  if (hasResidual) bars.push({
    id: 'unexplained', label: 'Unexplained difference', lines: ['Unexplained', 'difference'], color: '#f0bd70',
    description: 'Unexplained difference between the supplied attribution and the observed diluted share-price return. It has not been included in trader PnL or open-position valuation.',
    contributionPp: residualPp, amountUsdg: residualUsdg.toString(), start: running, end: returnPp,
  })
  bars.push({
    id: 'total', label: 'Net junior return', lines: ['Net junior', 'return'], color: '#e5edf7',
    description: 'Ending diluted share price divided by starting diluted share price, minus one. Deposits and withdrawals are excluded from earnings. The USDG amount is the pool’s economic gain, not the profit of an individual holder.',
    contributionPp: returnPp, amountUsdg: attribution.netReturnUsdg, start: 0, end: returnPp,
  })
  return { ...summary, bars, residualPp: hasResidual ? residualPp : null, notes: attribution.notes ?? [], reason: null }
}
