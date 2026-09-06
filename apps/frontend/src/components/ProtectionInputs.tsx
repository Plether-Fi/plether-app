import { useId } from 'react'
import { formatUnits } from 'viem'
import { protectionParamsFromInputs, type ProtectionDraft, type PositionProtectionParams } from '../contracts/positionProtection'
import { protectionDistance, protectionPrice, type ProtectionPriceContext } from '../utils/positionProtection'
import { InfoTooltip } from './ui/InfoTooltip'

interface ProtectionInputsProps extends ProtectionPriceContext {
  value: ProtectionDraft
  onChange: (draft: ProtectionDraft) => void
  disabled?: boolean
}

export function ProtectionInputs({ value, onChange, disabled = false, direction, rawMark, cap }: ProtectionInputsProps) {
  const id = useId()
  const context = { direction, rawMark, cap }
  const marketReady = rawMark !== undefined && cap !== undefined && rawMark > 0n && rawMark < cap

  return <fieldset disabled={disabled} className="min-w-0 space-y-4 disabled:opacity-60">
    <legend className="sr-only">Take profit / Stop loss</legend>
    <div className="flex flex-wrap items-center justify-between gap-3">
      <p className="text-xs text-content-secondary">{direction === 'long' ? 'Long' : 'Short'} · Current price <span className="font-medium tabular-nums text-content-primary">{protectionPrice(rawMark, cap)} USDC</span></p>
      <InfoTooltip ariaLabel="How take profit and stop loss work" content="Set either trigger or both. The first one reached queues a full close and cancels the other. The final execution price may differ. % change is measured from the current price, not leveraged return. Calculated percentages are rounded down to four decimals." />
    </div>
    {!marketReady ? <p className="text-xs text-content-secondary">Waiting for the current price to calculate percentage changes.</p> : null}
    <div className="grid grid-cols-[repeat(auto-fit,minmax(min(100%,18rem),1fr))] gap-3">
      {(['takeProfit', 'stopLoss'] as const).map(key => {
        const isProfit = key === 'takeProfit'
        const label = isProfit ? 'Take profit' : 'Stop loss'
        const goesUp = (direction === 'long') === isProfit
        const modeKey = isProfit ? 'takeProfitMode' : 'stopLossMode'
        const inputMode = value[modeKey] ?? value.mode
        let trigger: bigint | undefined
        let error: string | undefined
        if (value[key].trim() && marketReady) {
          try {
            const params = protectionParamsFromInputs({ takeProfit: '', stopLoss: '', ...context, rawMark, cap, mode: inputMode, [key]: value[key] })
            trigger = isProfit ? params.takeProfitTriggerPrice : params.stopLossTriggerPrice
          } catch (cause) { error = cause instanceof Error ? cause.message : 'Check this trigger' }
        }
        const hint = error ?? `${goesUp ? 'Above' : 'Below'} the current price`
        const price = inputMode === 'price' ? value[key] : trigger && cap !== undefined ? formatUnits(cap - trigger, 8) : ''
        const delta = trigger !== undefined && rawMark !== undefined ? rawMark - trigger : 0n
        const percent = inputMode === 'percent' ? value[key] : trigger && marketReady ? formatUnits((delta < 0n ? -delta : delta) * 1_000_000n / (cap - rawMark), 4) : ''
        return <div key={key} className="min-w-0 border border-brand-border/20 bg-app-bg p-3">
          <div className="mb-3 flex items-center justify-between gap-2 text-sm font-medium">
            <span className={isProfit ? 'text-positive' : 'text-[#FFAB96]'}>{label}</span>
            <span className="text-[10px] font-normal uppercase tracking-wider text-content-secondary">Optional</span>
          </div>
          <div className="grid grid-cols-[minmax(0,1.3fr)_minmax(0,1fr)] gap-3">
          {(['price', 'percent'] as const).map(mode => <div key={mode} className="min-w-0">
            <label htmlFor={`${id}-${key}-${mode}`} className="sr-only">{mode === 'price' ? 'Price' : isProfit ? 'Gain' : 'Loss'}</label>
            <div className={`flex items-center border-b pb-2 focus-within:border-[#FFAB96] ${error ? 'border-brand-orange' : 'border-brand-border/40'}`}>
            <input
              id={`${id}-${key}-${mode}`} inputMode="decimal" autoComplete="off" aria-label={`${label} (${mode === 'percent' ? '%' : 'USDC'})`}
              aria-invalid={Boolean(error)} aria-describedby={`${id}-${key}-hint`}
              value={mode === 'price' ? price : percent} placeholder={mode === 'price' ? 'Price' : isProfit ? 'Gain' : 'Loss'}
              onChange={event => { onChange({ ...value, [modeKey]: mode, [key]: event.target.value }) }}
              className="min-w-0 w-full bg-transparent text-lg tabular-nums text-content-primary placeholder:text-content-secondary/40 focus:outline-none"
            />
            <span className="ml-2 text-xs text-content-secondary">{mode === 'percent' ? '%' : 'USDC'}</span>
            </div>
          </div>
          )}
          </div>
          <p id={`${id}-${key}-hint`} className={`mt-2 text-xs leading-5 ${error ? 'text-brand-orange' : 'text-content-secondary'}`}>{hint}</p>
          {value[key] ? <button type="button" className="mt-2 text-xs text-content-secondary underline underline-offset-4 hover:text-content-primary" onClick={() => { onChange({ ...value, [key]: '' }) }}>Remove {label.toLowerCase()}</button> : null}
        </div>
      })}
    </div>
  </fieldset>
}

export function ProtectionPriceSummary({ params, cap, rawMark }: { params: PositionProtectionParams; cap?: bigint; rawMark?: bigint }) {
  return <dl className="grid grid-cols-[repeat(auto-fit,minmax(min(100%,10rem),1fr))] gap-3">
    {([['Take profit', params.takeProfitTriggerPrice, 'text-positive'], ['Stop loss', params.stopLossTriggerPrice, 'text-[#FFAB96]']] as const).map(([label, price, tone]) => <div key={label} className="min-w-0 border border-brand-border/20 bg-app-bg p-3 sm:p-4">
      <dt className={`text-xs font-medium ${tone}`}>{label}</dt>
      <dd>
        <p className="mt-2 break-words text-lg font-semibold tabular-nums text-content-primary sm:text-xl">{price ? protectionPrice(price, cap) : 'Not set'}{price ? <span className="ml-1 text-xs font-normal text-content-secondary">USDC</span> : null}</p>
        {price ? <p className="mt-1 text-xs text-content-secondary">{protectionDistance(price, rawMark, cap)}</p> : <p className="mt-1 text-xs text-content-secondary">No {label.toLowerCase()} trigger</p>}
      </dd>
    </div>)}
  </dl>
}
