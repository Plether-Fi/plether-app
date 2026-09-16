import type { WalletActivity } from '../api/types'
import { deriveTradeBreakdown, formatTradeUsdc } from '../utils/tradeBreakdown'

export function TradeFee({ item }: { item: WalletActivity }) {
  const breakdown = deriveTradeBreakdown(item.execution, item.type)
  return <>{breakdown ? formatTradeUsdc(breakdown.fee) : '—'}</>
}

export function TradeVpi({ item }: { item: WalletActivity }) {
  const breakdown = deriveTradeBreakdown(item.execution, item.type)
  const value = breakdown?.vpi
  if (value === undefined) return <>—</>
  return <span className={value < 0n ? 'text-positive' : value > 0n ? 'text-brand-orange' : 'text-content-primary'}>
    {value < 0n ? 'Rebate ' : value > 0n ? 'Charge ' : ''}{formatTradeUsdc(value < 0n ? -value : value)}
  </span>
}

export function TradeNotice({ item }: { item: WalletActivity }) {
  const breakdown = deriveTradeBreakdown(item.execution, item.type)
  return <p className="mt-1 text-xs font-normal text-content-secondary">{breakdown?.notice ?? (breakdown ? null : 'Breakdown unavailable')}</p>
}

export function TradeBreakdownDetails({ item }: { item: WalletActivity }) {
  const breakdown = deriveTradeBreakdown(item.execution, item.type)
  const amount = (value: bigint) => formatTradeUsdc(value, true, true)
  if (!breakdown) return <p className="text-content-secondary">Breakdown unavailable. A complete, unambiguous execution receipt is not available for this trade. Missing costs are not zero.</p>
  return <div className="text-sm">
      <dl className="space-y-2">
        {breakdown.lines.map(line => <div key={line.label} className="flex justify-between gap-6">
          <dt className="text-content-secondary">{line.label}</dt><dd className="whitespace-nowrap tabular-nums">{amount(line.value)}</dd>
        </div>)}
        <div className="flex justify-between gap-6 border-t border-brand-border/30 pt-3 font-semibold">
          <dt>Actual account change</dt><dd className={`whitespace-nowrap tabular-nums ${breakdown.actual < 0n ? 'text-brand-orange' : 'text-positive'}`}>{amount(breakdown.actual)}</dd>
        </div>
        {breakdown.claimChange !== 0n && <>
          <div className="flex justify-between gap-6 text-xs text-content-secondary"><dt>Settlement balance change</dt><dd>{amount(breakdown.settlementChange)}</dd></div>
          <div className="flex justify-between gap-6 text-xs text-content-secondary"><dt>Trader claim change</dt><dd>{amount(breakdown.claimChange)}</dd></div>
        </>}
      </dl>
      {breakdown.effectiveFee !== null && <p className="mt-3 text-xs text-content-secondary">Fee after waiver: {formatTradeUsdc(breakdown.effectiveFee, false, true)}. Already included above.</p>}
      {breakdown.netRebate !== null && <p className="mt-3 text-xs text-content-secondary">Net rebate paid: {formatTradeUsdc(breakdown.netRebate, false, true)}. This subtotal is already included above; do not add it again.</p>}
      {breakdown.evidencePending && <p className="mt-3 text-xs text-content-secondary">Settlement explanations pending. Receipt amounts and account changes are available.</p>}
      {breakdown.unexplained !== 0n && <p className="mt-3 text-xs text-content-secondary">Other settlement adjustment is the remaining difference between assessed costs and the recorded account change. Its cause has not been classified.</p>}
      {breakdown.notice === 'Charges partly waived' && <p className="mt-3 text-xs text-content-secondary">The contract waived charges that settlement could not collect. Collateral reserved for other purposes may still remain in the account.</p>}
      {breakdown.notice === 'Fee deducted from rebate' && <p className="mt-3 text-xs text-content-secondary">The protocol fee reduces the gross VPI rebate. A zero treasury collection does not mean this execution was free.</p>}
      <p className="mt-3 text-xs leading-5 text-content-tertiary">Actual account change includes settlement balance and trader claims. Released position margin is existing collateral, not additional profit.{item.type.toLowerCase() === 'open' ? ' This change covers the opening or increase, not the eventual position return.' : ''}</p>
    </div>
}
