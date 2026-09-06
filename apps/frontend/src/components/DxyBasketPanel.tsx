import { useMemo, useState } from 'react'
import {
  DEFAULT_DXY_BASKET_CHART_INTERVAL,
  type DxyBasketChartInterval,
} from './dxyBasketChartConfig'
import { TradingViewAdvancedChart } from '../tradingview/TradingViewAdvancedChart'
import type { PerpsMarketPhase } from '../utils/perpsMarketSchedule'
import { oraclePriceToDisplayDxyPrice } from '../utils/perps'

export interface DxyBasketPanelProps {
  liquidationPriceRaw?: bigint
  marketPhase?: PerpsMarketPhase
  marketCurrentDuration?: string
}

export function DxyBasketPanel({
  liquidationPriceRaw,
  marketPhase,
  marketCurrentDuration,
}: DxyBasketPanelProps) {
  const [chartInterval, setChartInterval] = useState<DxyBasketChartInterval>(
    DEFAULT_DXY_BASKET_CHART_INTERVAL
  )
  const liquidationPrice = useMemo(() => {
    const displayPrice = oraclePriceToDisplayDxyPrice(liquidationPriceRaw)
    if (displayPrice === undefined || displayPrice <= 0n) return undefined
    return Number(displayPrice) / 1e8
  }, [liquidationPriceRaw])

  return (
    <TradingViewAdvancedChart
      interval={chartInterval}
      liquidationPrice={liquidationPrice}
      marketPhase={marketPhase}
      marketCurrentDuration={marketCurrentDuration}
      onIntervalChange={setChartInterval}
    />
  )
}
