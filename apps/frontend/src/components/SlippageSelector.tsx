import { useState } from 'react'
import { useSettingsStore } from '../stores/settingsStore'
import { SLIPPAGE_PRESETS, MAX_SLIPPAGE, PRICE_IMPACT_PRESETS, MAX_PRICE_IMPACT } from '../config/constants'
import { Button, Modal, Input } from './ui'

export function SlippageSelector() {
  const { slippage, setSlippage, maxPriceImpact, setMaxPriceImpact } = useSettingsStore()
  const [isOpen, setIsOpen] = useState(false)
  const [customSlippage, setCustomSlippage] = useState('')
  const [customPriceImpact, setCustomPriceImpact] = useState('')

  const handleSlippagePresetClick = (value: number) => {
    setSlippage(value)
    setCustomSlippage('')
  }

  const handleSlippageCustomChange = (value: string) => {
    setCustomSlippage(value)
    const num = parseFloat(value)
    if (!isNaN(num) && num > 0 && num <= MAX_SLIPPAGE) {
      setSlippage(num)
    }
  }

  const handlePriceImpactPresetClick = (value: number) => {
    setMaxPriceImpact(value)
    setCustomPriceImpact('')
  }

  const handlePriceImpactCustomChange = (value: string) => {
    setCustomPriceImpact(value)
    const num = parseFloat(value)
    if (!isNaN(num) && num > 0 && num <= MAX_PRICE_IMPACT) {
      setMaxPriceImpact(num)
    }
  }

  const isSlippageCustom = !SLIPPAGE_PRESETS.includes(slippage as typeof SLIPPAGE_PRESETS[number])
  const isPriceImpactCustom = !PRICE_IMPACT_PRESETS.includes(maxPriceImpact as typeof PRICE_IMPACT_PRESETS[number])

  return (
    <>
      <button
        onClick={() => { setIsOpen(true); }}
        className="flex items-center gap-1 text-sm text-gray-400 hover:text-white transition-colors"
      >
        <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
          <path
            strokeLinecap="round"
            strokeLinejoin="round"
            strokeWidth={2}
            d="M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z"
          />
          <path
            strokeLinecap="round"
            strokeLinejoin="round"
            strokeWidth={2}
            d="M15 12a3 3 0 11-6 0 3 3 0 016 0z"
          />
        </svg>
        <span>{slippage}% slippage</span>
      </button>

      <Modal
        isOpen={isOpen}
        onClose={() => { setIsOpen(false); }}
        title="Trade Settings"
        size="sm"
      >
        <div className="space-y-6">
          <div className="space-y-3">
            <h3 className="text-sm font-medium text-cyber-text-primary">Slippage Tolerance</h3>
            <p className="text-xs text-cyber-text-secondary">
              Transaction reverts if price moves unfavorably by more than this.
            </p>
            <div className="flex gap-2">
              {SLIPPAGE_PRESETS.map((preset) => (
                <Button
                  key={preset}
                  variant={slippage === preset ? 'primary' : 'secondary'}
                  size="sm"
                  onClick={() => { handleSlippagePresetClick(preset); }}
                  className="flex-1"
                >
                  {preset}%
                </Button>
              ))}
            </div>
            <div>
              <Input
                type="text"
                inputMode="decimal"
                value={customSlippage}
                onChange={(e) => { handleSlippageCustomChange(e.target.value); }}
                placeholder={`Custom (max ${String(MAX_SLIPPAGE)}%)`}
                rightElement={<span className="text-gray-400">%</span>}
                className={isSlippageCustom ? 'ring-2 ring-cyber-bright-blue' : ''}
              />
            </div>
          </div>

          <div className="border-t border-cyber-border-glow/30" />

          <div className="space-y-3">
            <h3 className="text-sm font-medium text-cyber-text-primary">Max Price Impact</h3>
            <p className="text-xs text-cyber-text-secondary">
              Warns before trades with price impact exceeding this threshold.
            </p>
            <div className="flex gap-2">
              {PRICE_IMPACT_PRESETS.map((preset) => (
                <Button
                  key={preset}
                  variant={maxPriceImpact === preset ? 'primary' : 'secondary'}
                  size="sm"
                  onClick={() => { handlePriceImpactPresetClick(preset); }}
                  className="flex-1"
                >
                  {preset}%
                </Button>
              ))}
            </div>
            <div>
              <Input
                type="text"
                inputMode="decimal"
                value={customPriceImpact}
                onChange={(e) => { handlePriceImpactCustomChange(e.target.value); }}
                placeholder={`Custom (max ${String(MAX_PRICE_IMPACT)}%)`}
                rightElement={<span className="text-gray-400">%</span>}
                className={isPriceImpactCustom ? 'ring-2 ring-cyber-bright-blue' : ''}
              />
            </div>
          </div>

          <Button
            variant="primary"
            onClick={() => { setIsOpen(false); }}
            className="w-full"
          >
            Save
          </Button>
        </div>
      </Modal>
    </>
  )
}
