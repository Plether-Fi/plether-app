import { TrancheMark } from './TrancheMark'
import { calculateJuniorExposure, type JuniorExposureInputs } from '../utils/juniorExposure'

const percent = (value: number | undefined) => value === undefined ? 'Unavailable' : `${value.toFixed(2)}%`
const multiple = (value: number | undefined) => value === undefined ? 'Unavailable' : `${value.toFixed(2)}×`
const signedPercent = (value: number | undefined) => value === undefined ? '—' : `${value > 0 ? '+' : ''}${value.toFixed(2)}%`

function SplitBar({ share, label, firstClass = 'bg-brand-peach', secondClass = 'bg-content-secondary/30' }: {
  share: number | undefined
  label: string
  firstClass?: string
  secondClass?: string
}) {
  return (
    <div role="img" aria-label={label} className="flex h-3 overflow-hidden rounded-sm bg-content-secondary/10">
      {share !== undefined && <>
        <div className={firstClass} style={{ width: `${Math.max(0, Math.min(100, share * 100)).toFixed(4)}%` }} />
        <div className={`flex-1 ${secondClass}`} />
      </>}
    </div>
  )
}

export function JuniorMarketExposure({ pool, scrollMarginTop }: {
  scrollMarginTop?: number
  pool: JuniorExposureInputs & { markFresh?: boolean; oracleFrozen?: boolean }
}) {
  const exposure = calculateJuniorExposure(pool)
  const currentPricing = pool.markFresh === true && pool.oracleFrozen === false
  const sensitivity = currentPricing ? exposure.juniorLossPercentForOnePercentRise : undefined
  const totalPositions = pool.longOpenInterest === undefined || pool.shortOpenInterest === undefined
    ? undefined : pool.longOpenInterest + pool.shortOpenInterest
  const longShare = totalPositions === undefined || totalPositions === 0n ? undefined
    : Number(pool.longOpenInterest) / Number(totalPositions)
  const juniorPercent = exposure.juniorShare === undefined ? undefined : exposure.juniorShare * 100
  const reservedPercent = exposure.unavailableCashShare === undefined ? undefined : exposure.unavailableCashShare * 100
  const direction = exposure.netPositionSize === undefined ? 'Positioning unavailable'
    : totalPositions === 0n ? 'No open trader positions'
      : exposure.netPositionSize === 0n ? 'Balanced trader exposure'
        : exposure.netPositionSize > 0n ? 'Pool benefits from USD weakening' : 'Pool benefits from USD strengthening'
  const cardClass = 'border border-brand-border/30 bg-surface-panel p-4 sm:p-5'

  return (
    <section id="market-exposure" data-vault-detail-section="market-exposure" style={{ scrollMarginTop }} aria-labelledby="junior-market-exposure" className="space-y-4">
      <div>
        <p className="text-xs font-semibold uppercase tracking-[0.16em] text-content-secondary">
          Current pool snapshot
        </p>
        <h2 id="junior-market-exposure" className="mt-1 text-2xl font-semibold text-content-primary">The market exposure</h2>
      </div>

      <div className="grid gap-4 md:grid-cols-2">
        <div className={cardClass}>
          <h3 className="text-xs uppercase tracking-wider text-content-secondary">Trader positioning</h3>
          <p className="mt-2 text-base font-semibold text-content-primary">{direction}</p>
          <div className="mt-5 flex flex-wrap justify-between gap-3 text-sm">
            <span className="text-positive">LONG USD <strong>{percent(longShare === undefined ? undefined : longShare * 100)}</strong></span>
            <span className="text-brand-orange">SHORT USD <strong>{percent(longShare === undefined ? undefined : (1 - longShare) * 100)}</strong></span>
          </div>
          <div className="mt-2">
            <SplitBar share={longShare} firstClass="bg-positive" secondClass="bg-brand-orange" label={longShare === undefined ? 'Trader position split unavailable or empty' : `Trader position split: ${percent(longShare * 100)} LONG USD, ${percent((1 - longShare) * 100)} SHORT USD`} />
          </div>
          <div className="mt-5 flex items-end justify-between gap-3 border-t border-brand-border/30 pt-4">
            <span className="text-sm text-content-secondary">Open exposure / pool capital</span>
            <span className="text-2xl font-semibold tabular-nums text-content-primary">{currentPricing ? multiple(exposure.grossExposureMultiple) : '—'}</span>
          </div>
        </div>

        <div className={cardClass}>
          <h3 className="text-xs uppercase tracking-wider text-content-secondary">If the dollar index moves 1%</h3>
          <p className="mt-2 text-sm text-content-secondary">Estimated pool PnL as a % of Junior capital</p>
          <div className="mt-5 grid grid-cols-2 gap-3">
            {[{ label: 'USD weakens', arrow: '↘', value: sensitivity }, { label: 'USD strengthens', arrow: '↗', value: sensitivity === undefined ? undefined : -sensitivity }].map(({ label, arrow, value }) => (
              <div key={label} className="border border-brand-border/30 bg-content-secondary/5 p-3 sm:p-4">
                <div className="text-sm text-content-secondary"><span aria-hidden="true">{arrow} </span>{label}</div>
                <div className="mt-3 text-2xl font-semibold tabular-nums text-content-primary sm:text-3xl">{signedPercent(value)}</div>
                <div className="mt-1 text-xs text-content-secondary">{value === undefined ? 'Unavailable' : value === 0 ? 'Directional PnL offset' : value > 0 ? 'Pool gain / Junior capital' : 'Pool loss / Junior capital'}</div>
              </div>
            ))}
          </div>
          {sensitivity === undefined && <p className="mt-3 text-xs text-warning">Market sensitivity is unavailable without fresh pricing and positive Junior capital.</p>}
          {!currentPricing && <p className="mt-2 text-xs text-warning">Last available positioning; live pricing is unavailable.</p>}
        </div>
      </div>

      <div className="mt-4 grid gap-4 lg:grid-cols-3">
        <div className={cardClass}>
          <h3 className="text-xs uppercase tracking-wider text-content-secondary">Capital split</h3>
          <div className="mt-4 flex items-baseline gap-2"><span className="text-3xl font-semibold tabular-nums text-content-primary">{percent(juniorPercent)}</span><span className="text-sm text-content-secondary">Junior</span></div>
          <div className="mt-4"><SplitBar share={exposure.juniorShare} label={`Junior share of pool capital: ${percent(juniorPercent)}`} /></div>
          <div className="mt-2 flex justify-between text-xs text-content-secondary"><span>Junior · first loss</span><span>Senior {percent(juniorPercent === undefined ? undefined : 100 - juniorPercent)}</span></div>
          <div className="mt-5 border-t border-brand-border/30 pt-4">
            <div className="flex items-center justify-between gap-2 text-sm"><span className="text-content-secondary">1% pool capital loss</span><span aria-hidden="true" className="text-content-secondary">→</span><strong className="text-content-primary">{percent(exposure.capitalMultiple === undefined ? undefined : Math.min(exposure.capitalMultiple, 100))}</strong></div>
            <p className="mt-1 text-right text-xs text-content-secondary">of Junior capital lost</p>
          </div>
        </div>

        <div className={cardClass}>
          <h3 className="text-xs uppercase tracking-wider text-content-secondary">Pool cash usage</h3>
          <div className="mt-4 flex items-baseline gap-2"><span className="text-3xl font-semibold tabular-nums text-content-primary">{percent(reservedPercent)}</span><span className="text-sm text-content-secondary">reserved</span></div>
          <div className="mt-4"><SplitBar share={exposure.unavailableCashShare} firstClass="bg-brand-orange" secondClass="bg-positive" label={`Cash unavailable for LP withdrawals: ${percent(reservedPercent)}`} /></div>
          <div className="mt-2 flex justify-between text-xs text-content-secondary"><span>Unavailable to LPs</span><span>Free {percent(reservedPercent === undefined ? undefined : 100 - reservedPercent)}</span></div>
          <p className="mt-5 border-t border-brand-border/30 pt-4 text-xs leading-5 text-content-secondary">Trader backing + other reserves. More reserved cash leaves less room for withdrawals.</p>
        </div>

        <div className={cardClass}>
          <h3 className="text-xs uppercase tracking-wider text-content-secondary">Senior coupon cost</h3>
          <div className="mt-4 text-3xl font-semibold tabular-nums text-content-primary">{percent(exposure.couponDragPercent)}</div>
          <p className="mt-1 text-xs text-content-secondary">Annualized, relative to Junior capital; A larger Senior / Junior ratio increases this cost.</p>
          <div className="mt-4 grid grid-cols-[auto_minmax(1rem,1fr)_auto] items-center gap-3 text-sm text-content-primary">
            <div className="justify-self-center"><TrancheMark tranche={{ id: 'junior' }} size="md" showLabel /></div>
            <span aria-hidden="true" className="flex min-w-4 flex-1 items-center text-content-secondary">
              <span className="h-px flex-1 bg-current" />
              <svg className="-ml-2 h-4 w-4 shrink-0" viewBox="0 0 16 16" fill="none" focusable="false">
                <path d="M3 8h10m-5-5 5 5-5 5" stroke="currentColor" strokeWidth="1" strokeLinecap="round" strokeLinejoin="round" />
              </svg>
            </span>
            <div className="justify-self-center"><TrancheMark tranche={{ id: 'senior' }} size="md" showLabel /></div>
          </div>
        </div>
      </div>

      <details className="mt-4 border-t border-brand-border/30 pt-4 text-xs leading-5 text-content-secondary">
        <summary className="cursor-pointer font-medium text-content-primary">How to read these estimates</summary>
        <div className="mt-3 grid gap-3 sm:grid-cols-2">
          <p>Open exposure is combined LONG USD + SHORT USD exposure at the displayed index price, divided by Senior + Junior capital. Opposing positions offset directional sensitivity. Reserved cash includes trader backing and other reserves; it does not measure directional exposure.</p>
          <p>The market-move estimate holds positions fixed, before payout and collectible-loss caps, fees, carry and liquidations. Junior absorbs losses up to its remaining capital. Pool gains first restore any Senior impairment before reaching Junior.</p>
          <p>A smaller Junior share concentrates pool losses. The 1% pool-capital loss example is capped at a full Junior wipeout. It is a separate scenario from a 1% index move.</p>
          <p>Coupon cost = Senior target rate × Senior capital ÷ Junior capital, assuming fixed balances and rate, before pool earnings and Junior fees. Payment is limited to available Junior capital. These snapshot estimates are not a return forecast.</p>
        </div>
      </details>
    </section>
  )
}
