import { useMemo, useState } from 'react'
import {
  DEFAULT_DXY_BASKET_CHART_INTERVAL,
  type DxyBasketChartInterval,
} from './dxyBasketChartConfig'
import { TradingViewAdvancedChart } from '../tradingview/TradingViewAdvancedChart'
import type { PerpsMarketPhase } from '../utils/perpsMarketSchedule'
import { liquidationDisplayPrice } from '../utils/perpsRisk'
import { oraclePriceToDisplayDxyPrice } from '../utils/perps'

export interface DxyBasketPanelProps {
  entryPriceRaw?: bigint
  liquidationPriceRaw?: bigint
  capPrice?: bigint
  takeProfitPriceRaw?: bigint
  stopLossPriceRaw?: bigint
  marketPhase?: PerpsMarketPhase
  marketCurrentDuration?: string
}

export function DxyBasketPanel({
  entryPriceRaw,
  liquidationPriceRaw,
  capPrice = 200_000_000n,
  takeProfitPriceRaw,
  stopLossPriceRaw,
  marketPhase,
  marketCurrentDuration,
}: DxyBasketPanelProps) {
  const [chartInterval, setChartInterval] = useState<DxyBasketChartInterval>(
    DEFAULT_DXY_BASKET_CHART_INTERVAL
  )
  const liquidationPrice = useMemo(() => {
    const displayPrice = liquidationDisplayPrice(liquidationPriceRaw, capPrice)
    if (displayPrice === undefined || displayPrice < 0n) return undefined
    return Number(displayPrice) / 1e8
  }, [liquidationPriceRaw, capPrice])

  return (
    <TradingViewAdvancedChart
      interval={chartInterval}
      entryPrice={entryPriceRaw ? Number(oraclePriceToDisplayDxyPrice(entryPriceRaw)) / 1e8 : undefined}
      liquidationPrice={liquidationPrice}
      takeProfitPrice={takeProfitPriceRaw ? Number(oraclePriceToDisplayDxyPrice(takeProfitPriceRaw)) / 1e8 : undefined}
      stopLossPrice={stopLossPriceRaw ? Number(oraclePriceToDisplayDxyPrice(stopLossPriceRaw)) / 1e8 : undefined}
      marketPhase={marketPhase}
      marketCurrentDuration={marketCurrentDuration}
      onIntervalChange={setChartInterval}
    />
  )
}
