import type { WalletActivity } from '../api/types'
import { deriveTradeBreakdown, formatTradeAssets } from '../utils/tradeBreakdown'

function splitExactUsdg(value: bigint, signed: boolean): {
  primary: string
  subCents: string
  unit: string
} {
  const full = formatTradeAssets(value, signed, true)
  const match = /^([+−]?\d[\d,]*\.\d{2})(\d{4})( USDG)$/.exec(full)
  if (!match) return { primary: full, subCents: '', unit: '' }
  return { primary: match[1], subCents: match[2], unit: match[3] }
}

export function ExactUsdgAmount({
  value,
  signed = false,
  tone = 'default',
}: {
  value: bigint
  signed?: boolean
  tone?: 'default' | 'total'
}) {
  const { primary, subCents, unit } = splitExactUsdg(value, signed)
  const isZero = value === 0n
  const primaryTone =
    tone === 'total'
      ? value < 0n
        ? 'text-brand-orange'
        : value > 0n
          ? 'text-positive'
          : 'text-content-primary'
      : isZero
        ? 'text-content-tertiary'
        : value > 0n && signed
          ? 'text-positive/90'
          : 'text-content-primary'

  return (
    <span className={`whitespace-nowrap font-mono tabular-nums ${primaryTone}`}>
      <span>{primary}</span>
      {subCents ? <span className="text-[11px] opacity-40">{subCents}</span> : null}
      {unit ? <span className="text-xs font-normal text-content-tertiary">{unit}</span> : null}
    </span>
  )
}

export function TradeFee({ item }: { item: WalletActivity }) {
  const breakdown = deriveTradeBreakdown(item.execution, item.type)
  return <>{breakdown ? formatTradeAssets(breakdown.fee) : '—'}</>
}

export function TradeVpi({ item }: { item: WalletActivity }) {
  const breakdown = deriveTradeBreakdown(item.execution, item.type)
  const value = breakdown?.vpi
  if (value === undefined) return <>—</>
  return (
    <span
      className={
        value < 0n ? 'text-positive' : value > 0n ? 'text-brand-orange' : 'text-content-primary'
      }
    >
      {value < 0n ? 'Rebate ' : value > 0n ? 'Charge ' : ''}
      {formatTradeAssets(value < 0n ? -value : value)}
    </span>
  )
}

export function TradeNotice({ item }: { item: WalletActivity }) {
  const breakdown = deriveTradeBreakdown(item.execution, item.type)
  return (
    <p className="mt-1 text-xs font-normal text-content-secondary">
      {breakdown?.notice ?? (breakdown ? null : 'Breakdown unavailable')}
    </p>
  )
}

export function TradeBreakdownDetails({ item }: { item: WalletActivity }) {
  const breakdown = deriveTradeBreakdown(item.execution, item.type)
  if (!breakdown) {
    return (
      <p className="text-content-secondary">
        Breakdown unavailable. A complete, unambiguous execution receipt is not available for this
        trade. Missing costs are not zero.
      </p>
    )
  }
  return (
    <div className="text-sm">
      <dl className="divide-y divide-brand-border/15">
        {breakdown.lines.map((line) => (
          <div key={line.label} className="flex items-baseline justify-between gap-6 py-2.5">
            <dt className={line.value === 0n ? 'text-content-tertiary' : 'text-content-secondary'}>
              {line.label}
            </dt>
            <dd className="text-right">
              <ExactUsdgAmount value={line.value} signed />
            </dd>
          </div>
        ))}
        <div className="flex items-baseline justify-between gap-6 border-t border-brand-border/35 pt-3.5 pb-1 font-semibold">
          <dt className="text-content-primary">Actual account change</dt>
          <dd className="text-right text-base">
            <ExactUsdgAmount value={breakdown.actual} signed tone="total" />
          </dd>
        </div>
        {breakdown.claimChange !== 0n ? (
          <>
            <div className="flex items-baseline justify-between gap-6 py-1.5 text-xs text-content-secondary">
              <dt>USDG balance change</dt>
              <dd>
                <ExactUsdgAmount value={breakdown.settlementChange} signed />
              </dd>
            </div>
            <div className="flex items-baseline justify-between gap-6 py-1.5 text-xs text-content-secondary">
              <dt>Trader claim change</dt>
              <dd>
                <ExactUsdgAmount value={breakdown.claimChange} signed />
              </dd>
            </div>
          </>
        ) : null}
      </dl>
      {breakdown.effectiveFee !== null ? (
        <p className="mt-3 text-xs text-content-secondary">
          Fee after waiver: {formatTradeAssets(breakdown.effectiveFee, false, true)}. Already
          included above.
        </p>
      ) : null}
      {breakdown.netRebate !== null ? (
        <p className="mt-3 text-xs text-content-secondary">
          Net rebate paid: {formatTradeAssets(breakdown.netRebate, false, true)}. This subtotal is
          already included above; do not add it again.
        </p>
      ) : null}
      {breakdown.evidencePending ? (
        <p className="mt-3 text-xs text-content-secondary">
          Settlement explanations pending. Receipt amounts and account changes are available.
        </p>
      ) : null}
      {breakdown.unexplained !== 0n ? (
        <p className="mt-3 text-xs text-content-secondary">
          Other settlement adjustment is the remaining difference between assessed costs and the
          recorded account change. Its cause has not been classified.
        </p>
      ) : null}
      {breakdown.notice === 'Charges partly waived' ? (
        <p className="mt-3 text-xs text-content-secondary">
          The contract waived charges that settlement could not collect. Collateral reserved for
          other purposes may still remain in the account.
        </p>
      ) : null}
      {breakdown.notice === 'Fee deducted from rebate' ? (
        <p className="mt-3 text-xs text-content-secondary">
          The protocol fee reduces the gross VPI rebate. A zero treasury collection does not mean
          this execution was free.
        </p>
      ) : null}
      <p className="mt-3 text-xs leading-5 text-content-tertiary">
        Actual account change includes USDG balance and trader claims. Released position margin is
        existing collateral, not additional profit.
        {item.type.toLowerCase() === 'open'
          ? ' This change covers the opening or increase, not the eventual position return.'
          : ''}
      </p>
    </div>
  )
}
