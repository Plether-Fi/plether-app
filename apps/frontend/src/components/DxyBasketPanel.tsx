import { useMemo, useState } from 'react'
import {
  DEFAULT_DXY_BASKET_CHART_INTERVAL,
  type DxyBasketChartInterval,
} from './dxyBasketChartConfig'
import { TradingViewAdvancedChart } from '../tradingview/TradingViewAdvancedChart'
import type { OracleMarkPoint } from '../utils/dxyBasketChart'
import type { PerpsMarketPhase } from '../utils/perpsMarketSchedule'

export interface DxyBasketPanelProps {
  oraclePriceRaw?: bigint
  oraclePublishTime?: number
  marketPhase?: PerpsMarketPhase
  marketCurrentDuration?: string
}

export function DxyBasketPanel({
  oraclePriceRaw,
  oraclePublishTime,
  marketPhase,
  marketCurrentDuration,
}: DxyBasketPanelProps) {
  const [chartInterval, setChartInterval] = useState<DxyBasketChartInterval>(
    DEFAULT_DXY_BASKET_CHART_INTERVAL
  )
  const oracleMark = useMemo<OracleMarkPoint | undefined>(() => {
    if (oraclePriceRaw === undefined || oraclePublishTime === undefined) return undefined

    return {
      timestamp: oraclePublishTime,
      basketPrice: oraclePriceRaw.toString(),
    }
  }, [oraclePriceRaw, oraclePublishTime])

  return (
    <TradingViewAdvancedChart
      interval={chartInterval}
      oracleMark={oracleMark}
      marketPhase={marketPhase}
      marketCurrentDuration={marketCurrentDuration}
      onIntervalChange={setChartInterval}
    />
  )
}
