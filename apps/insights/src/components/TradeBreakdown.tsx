import type { WalletActivity } from '../api/types'
import { deriveTradeBreakdown, formatTradeAssets, type TradeBreakdown } from '../utils/tradeBreakdown'

function shortWaterfallLabel(label: string): string {
  if (label === 'Directional realized P&L') return 'Directional PnL'
  if (label === 'Protocol fee assessed') return 'Protocol fee'
  if (label === 'Gross VPI rebate') return 'VPI rebate'
  if (label === 'Gross VPI charge') return 'VPI charge'
  if (label === 'Frozen-close spread assessed') return 'Frozen spread'
  if (label === 'Execution reward') return 'Keeper reward'
  if (label === 'Fee waived') return 'Waived fee'
  if (label === 'Other settlement adjustment') return 'Adjustment'
  return label
}

function BreakdownWaterfall({ breakdown }: { breakdown: TradeBreakdown }) {
  const nonZeroLines = breakdown.lines.filter((line) => line.value !== 0n)
  if (nonZeroLines.length === 0 && breakdown.actual === 0n) return null

  const steps: {
    label: string
    value: bigint
    from: number
    to: number
    isTotal: boolean
  }[] = []

  let running = 0
  for (const line of nonZeroLines) {
    const delta = Number(line.value) / 1_000_000
    const next = running + delta
    steps.push({
      label: shortWaterfallLabel(line.label),
      value: line.value,
      from: running,
      to: next,
      isTotal: false,
    })
    running = next
  }

  const actualNum = Number(breakdown.actual) / 1_000_000
  steps.push({
    label: 'Actual change',
    value: breakdown.actual,
    from: 0,
    to: actualNum,
    isTotal: true,
  })

  let minVal = 0
  let maxVal = 0
  for (const step of steps) {
    minVal = Math.min(minVal, step.from, step.to)
    maxVal = Math.max(maxVal, step.from, step.to)
  }
  const span = Math.max(maxVal - minVal, 1e-6)
  const toPct = (v: number) => ((v - minVal) / span) * 100
  const zeroPct = toPct(0)

  return (
    <div
      aria-label="Execution reconciliation waterfall"
      className="mb-5 border border-brand-border/20 bg-app-bg/55 p-4"
    >
      <div className="mb-3 flex items-center justify-between gap-2 text-[11px] font-semibold uppercase tracking-[0.12em] text-content-tertiary">
        <span>Account change bridge</span>
        <span className="font-mono font-normal normal-case tracking-normal">0 baseline → net</span>
      </div>
      <div className="space-y-2">
        {steps.map((step) => {
          const left = Math.min(toPct(step.from), toPct(step.to))
          const rawWidth = Math.abs(toPct(step.to) - toPct(step.from))
          const width = Math.max(rawWidth, 0.9)
          const positive = step.value > 0n
          const negative = step.value < 0n
          const barClass = step.isTotal
            ? positive
              ? 'bg-positive'
              : negative
                ? 'bg-brand-orange'
                : 'bg-content-tertiary'
            : positive
              ? 'bg-positive/75'
              : 'bg-brand-orange/75'
          return (
            <div
              key={`${step.label}-${step.isTotal ? 'total' : 'step'}`}
              className={`grid grid-cols-[minmax(0,118px)_1fr_minmax(0,98px)] items-center gap-3 text-xs ${
                step.isTotal ? 'border-t border-brand-border/25 pt-2 font-semibold' : ''
              }`}
            >
              <span
                className={`truncate ${
                  step.isTotal ? 'text-content-primary' : 'text-content-secondary'
                }`}
                title={step.label}
              >
                {step.label}
              </span>
              <div className="relative h-3.5 bg-surface-panel/70">
                <span
                  aria-hidden="true"
                  className="pointer-events-none absolute inset-y-0 w-px bg-brand-peach/35"
                  style={{ left: `${zeroPct.toFixed(2)}%` }}
                />
                <span
                  aria-hidden="true"
                  className={`absolute top-0.5 bottom-0.5 ${barClass}`}
                  style={{
                    left: `${Math.min(left, 99.1).toFixed(2)}%`,
                    width: `${Math.min(width, 100 - Math.min(left, 99.1)).toFixed(2)}%`,
                  }}
                />
              </div>
              <span
                className={`text-right font-mono tabular-nums ${
                  positive
                    ? 'text-positive'
                    : negative
                      ? 'text-brand-orange'
                      : 'text-content-secondary'
                }`}
              >
                {formatTradeAssets(step.value, true, false).replace(/ USDG$/, '')}
              </span>
            </div>
          )
        })}
      </div>
    </div>
  )
}

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
      <BreakdownWaterfall breakdown={breakdown} />
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
