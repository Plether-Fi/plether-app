export interface BreakdownLine { label: string; value: bigint }
export interface TradeBreakdown {
  fee: bigint
  vpi: bigint
  lines: BreakdownLine[]
  actual: bigint
  settlementChange: bigint
  claimChange: bigint
  adjustment: bigint
  unexplained: bigint
  notice: string | null
  netRebate: bigint | null
  effectiveFee: bigint | null
  evidencePending: boolean
}

function integer(value: unknown): bigint | null {
  return typeof value === 'string' && /^-?\d+$/.test(value) ? BigInt(value) : null
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return value !== null && typeof value === 'object' && !Array.isArray(value)
}

/** Receipt economics describe trader costs, not cash credited to the treasury. */
export function deriveTradeBreakdown(execution: unknown, type: string): TradeBreakdown | null {
  if (!isRecord(execution) || execution.protocolVersion !== 'v1.2.3' ||
      (execution.status !== 'complete' && execution.status !== 'pending') ||
      !isRecord(execution.receipt) || !Array.isArray(execution.settlements) ||
      !['open', 'close'].includes(type.toLowerCase())) return null
  const r = execution.receipt
  const keys = ['realizedPnlUsdc', 'executionFeeUsdc', 'vpiUsdc', 'carryUsdc', 'frozenSpreadUsdc',
    'executionBountyUsdc', 'preSettlementBalanceUsdc', 'postSettlementBalanceUsdc',
    'preTraderClaimBalanceUsdc', 'postTraderClaimBalanceUsdc'] as const
  const values = keys.map(key => integer(r[key]))
  if (values.some(value => value === null)) return null
  const [pnl, fee, vpi, carry, spread, reward, before, after, claimBefore, claimAfter] = values as bigint[]
  if ([fee, carry, spread, reward, before, after, claimBefore, claimAfter].some(value => value < 0n)) return null
  const isClose = type.toLowerCase() === 'close'
  if (!isClose && pnl !== 0n) return null
  const actual = after - before + claimAfter - claimBefore
  const expected = pnl - fee - vpi - carry - spread - reward
  const adjustment = actual - expected
  const lines: BreakdownLine[] = []
  if (isClose) lines.push({ label: 'Directional realized P&L', value: pnl })
  lines.push({ label: 'Protocol fee assessed', value: -fee },
    { label: vpi < 0n ? 'Gross VPI rebate' : 'Gross VPI charge', value: -vpi },
    { label: 'Carry', value: -carry })
  if (spread !== 0n) lines.push({ label: 'Frozen-close spread assessed', value: -spread })
  lines.push({ label: 'Execution reward', value: -reward })

  const events = new Map<string, { assessed: bigint; recovered: bigint; waived: bigint; raw: Record<string, unknown> }>()
  for (const event of execution.settlements) {
    if (!isRecord(event) || typeof event.kind !== 'string' || !['ActionChargeSettled', 'ActionRebateSettled', 'FrozenCloseSpreadSettled'].includes(event.kind)) return null
    const assessed = integer(event.assessedUsdc), recovered = integer(event.recoveredUsdc), waived = integer(event.waivedUsdc)
    if (assessed === null || recovered === null || waived === null || assessed < 0n || recovered < 0n || waived < 0n ||
        recovered > assessed || waived > assessed || events.has(event.kind) ||
        (event.kind !== 'FrozenCloseSpreadSettled' && recovered + waived !== assessed)) return null
    events.set(event.kind, { assessed, recovered, waived, raw: event })
  }
  const charge = events.get('ActionChargeSettled'), rebate = events.get('ActionRebateSettled')
  const frozen = events.get('FrozenCloseSpreadSettled')
  if (charge && rebate) return null
  if (frozen && frozen.assessed !== spread) return null
  // A pending receipt must not turn partial evidence into a confirmed attribution.
  const complete = execution.status === 'complete'
  let explained = 0n
  let effectiveFee: bigint | null = null
  if (complete && charge && charge.waived > 0n) {
    // Treasury collection alone is not proof of a waiver: a fee can instead be
    // withheld from a rebate/profit. Only classify when isolated by evidence.
    const collectedFee = integer(charge.raw.protocolFeeCollectedUsdc)
    const feeWaiver = collectedFee !== null && collectedFee >= 0n && collectedFee <= fee &&
      pnl <= 0n && vpi >= 0n && spread === 0n && fee - collectedFee === charge.waived
    lines.push({ label: feeWaiver ? 'Fee waived' : 'Charges waived', value: charge.waived })
    explained += charge.waived
    if (feeWaiver) effectiveFee = collectedFee
  }
  if (complete && rebate && rebate.waived > 0n) {
    lines.push({ label: 'Rebate not paid', value: -rebate.waived })
    explained -= rebate.waived
  }
  // Frozen spread waivers are already included in ActionChargeSettled. Never
  // add them a second time; the gross spread/rebate lines likewise stay separate.
  const unexplained = adjustment - explained
  if (unexplained !== 0n) lines.push({ label: 'Other settlement adjustment', value: unexplained })
  const netRebate = complete && rebate ? rebate.recovered : null
  const notice = complete && charge && charge.waived > 0n ? 'Charges partly waived'
    : vpi < 0n && fee > 0n && -vpi > fee + spread + carry ? 'Fee deducted from rebate' : null
  return { fee, vpi, lines, actual, settlementChange: after - before, claimChange: claimAfter - claimBefore,
    adjustment, unexplained, notice, netRebate, effectiveFee, evidencePending: !complete }
}

export function formatTradeUsdc(value: bigint, signed = false, exact = false): string {
  const absolute = value < 0n ? -value : value
  const sign = value < 0n ? '−' : signed && value > 0n ? '+' : ''
  if (!exact && absolute > 0n && absolute < 10_000n) return `${sign}<0.01 USDC`
  const scale = exact ? 1_000_000n : 100n
  const rounded = exact ? absolute : (absolute + 5_000n) / 10_000n
  const whole = new Intl.NumberFormat('en-US').format(rounded / scale)
  return `${sign}${whole}.${(rounded % scale).toString().padStart(exact ? 6 : 2, '0')} USDC`
}
