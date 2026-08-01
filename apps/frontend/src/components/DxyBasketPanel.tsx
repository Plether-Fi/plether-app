import { useEffect, useMemo, useRef, useState } from 'react'
import {
  AreaSeries,
  CandlestickSeries,
  ColorType,
  CrosshairMode,
  createChart,
  type AreaData,
  type CandlestickData,
  type IChartApi,
  type ISeriesApi,
  type UTCTimestamp,
} from 'lightweight-charts'
import {
  usePerpsBasketHistory,
  usePerpsBasketLatest,
  type BasketHistory,
  type BasketLatest,
} from '../api'
import {
  DXY_BASKET_CHART_INTERVALS,
  basketIntervalSecondsForChartInterval,
  basketRequestIntervalSecondsForChartInterval,
  basketRangeForChartInterval,
  type DxyBasketChartInterval,
} from './dxyBasketChartConfig'
import { Alert, Skeleton } from './ui'
import {
  alignBasketPointsToOracleMark,
  buildCandles,
  computeBasketDisplayPriceChange,
  oracleNumberToDisplayDxyPrice,
  type ChartPoint,
  type OracleMarkPoint,
} from '../utils/dxyBasketChart'

const CHART_HEIGHT = 320
const DEFAULT_LINE_COLOR = '#00FF99'
const NEGATIVE_LINE_COLOR = '#FF572D'
const CHART_GRID_COLOR = 'rgba(255, 171, 150, 0.16)'
const CHART_AXIS_COLOR = 'rgba(255, 171, 150, 0.32)'
const CHART_TEXT_COLOR = '#D9CCD3'

export type DxyBasketChartStyle = 'area' | 'candlestick'

function toOraclePrice(raw: string): number {
  return Number(raw) / 1e8
}

function formatPrice(value: number): string {
  return value.toLocaleString('en-US', {
    minimumFractionDigits: 4,
    maximumFractionDigits: 4,
  })
}

function formatPercent(value: number | null | undefined): string {
  if (value == null) return '--'
  const sign = value > 0 ? '+' : ''
  return `${sign}${(value * 100).toFixed(2)}%`
}

export interface DxyBasketPanelViewProps {
  history?: BasketHistory
  changeHistory?: BasketHistory
  latest?: BasketLatest
  oracleMark?: OracleMarkPoint
  chartInterval?: DxyBasketChartInterval
  chartStyle?: DxyBasketChartStyle
  isLoading?: boolean
  isError?: boolean
  onChartIntervalChange: (interval: DxyBasketChartInterval) => void
}

interface DxyBasketChartProps {
  areaData: AreaData<UTCTimestamp>[]
  candlestickData: CandlestickData<UTCTimestamp>[]
  chartStyle: DxyBasketChartStyle
  lineColor: string
}

function areaTopColor(lineColor: string): string {
  return lineColor === DEFAULT_LINE_COLOR ? 'rgba(0, 255, 153, 0.24)' : 'rgba(255, 87, 45, 0.24)'
}

function DxyBasketChart({ areaData, candlestickData, chartStyle, lineColor }: DxyBasketChartProps) {
  const containerRef = useRef<HTMLDivElement | null>(null)
  const chartRef = useRef<IChartApi | null>(null)
  const areaSeriesRef = useRef<ISeriesApi<'Area'> | null>(null)
  const candlestickSeriesRef = useRef<ISeriesApi<'Candlestick'> | null>(null)

  useEffect(() => {
    const container = containerRef.current
    if (!container) return

    const chart = createChart(container, {
      width: container.clientWidth,
      height: container.clientHeight || CHART_HEIGHT,
      layout: {
        background: { type: ColorType.Solid, color: 'transparent' },
        textColor: CHART_TEXT_COLOR,
        fontFamily: 'Uncut Sans, ui-sans-serif, system-ui, sans-serif',
        fontSize: 12,
      },
      grid: {
        vertLines: { color: CHART_GRID_COLOR },
        horzLines: { color: CHART_GRID_COLOR },
      },
      crosshair: {
        mode: CrosshairMode.Normal,
        vertLine: {
          color: 'rgba(255, 171, 150, 0.48)',
          labelBackgroundColor: '#FFAB96',
        },
        horzLine: {
          color: 'rgba(255, 171, 150, 0.28)',
          labelBackgroundColor: '#FFAB96',
        },
      },
      localization: {
        priceFormatter: formatPrice,
      },
      rightPriceScale: {
        borderColor: CHART_AXIS_COLOR,
        scaleMargins: {
          top: 0.14,
          bottom: 0.12,
        },
      },
      timeScale: {
        borderColor: CHART_AXIS_COLOR,
        timeVisible: true,
        secondsVisible: false,
        rightOffset: 4,
        fixLeftEdge: true,
        fixRightEdge: true,
      },
    })
    if (chartStyle === 'candlestick') {
      candlestickSeriesRef.current = chart.addSeries(CandlestickSeries, {
        upColor: '#00FF99',
        downColor: '#FF572D',
        borderUpColor: '#00FF99',
        borderDownColor: '#FF572D',
        wickUpColor: '#00FF99',
        wickDownColor: '#FF572D',
        priceFormat: {
          type: 'price',
          precision: 4,
          minMove: 0.0001,
        },
        lastValueVisible: true,
        priceLineVisible: true,
        priceLineColor: DEFAULT_LINE_COLOR,
      })
    } else {
      areaSeriesRef.current = chart.addSeries(AreaSeries, {
        lineColor: DEFAULT_LINE_COLOR,
        topColor: areaTopColor(DEFAULT_LINE_COLOR),
        bottomColor: 'rgba(13, 10, 28, 0)',
        lineWidth: 2,
        priceFormat: {
          type: 'price',
          precision: 4,
          minMove: 0.0001,
        },
        lastValueVisible: true,
        priceLineVisible: true,
        priceLineColor: DEFAULT_LINE_COLOR,
      })
    }
    const resizeObserver = new ResizeObserver((entries) => {
      const entry = entries[0]

      chart.applyOptions({
        width: Math.floor(entry.contentRect.width),
        height: Math.floor(entry.contentRect.height) || CHART_HEIGHT,
      })
    })

    resizeObserver.observe(container)
    chartRef.current = chart

    return () => {
      resizeObserver.disconnect()
      chart.remove()
      chartRef.current = null
      areaSeriesRef.current = null
      candlestickSeriesRef.current = null
    }
  }, [chartStyle])

  useEffect(() => {
    const chart = chartRef.current
    const areaSeries = areaSeriesRef.current
    const candlestickSeries = candlestickSeriesRef.current
    if (!chart) return

    if (chartStyle === 'candlestick') {
      if (!candlestickSeries) return
      candlestickSeries.setData(candlestickData)
    } else {
      if (!areaSeries) return
      areaSeries.setData(areaData)
    }

    chart.timeScale().fitContent()
  }, [areaData, candlestickData, chartStyle])

  useEffect(() => {
    areaSeriesRef.current?.applyOptions({
      lineColor,
      topColor: areaTopColor(lineColor),
      priceLineColor: lineColor,
    })
    candlestickSeriesRef.current?.applyOptions({
      priceLineColor: lineColor,
    })
  }, [lineColor])

  return (
    <div
      className="relative h-[240px] w-full overflow-hidden sm:h-[320px]"
      role="img"
      aria-label="plDXY Perp price performance chart"
    >
      <div ref={containerRef} className="h-full w-full" />
    </div>
  )
}

export function DxyBasketPanelView({
  history,
  changeHistory,
  latest,
  oracleMark,
  chartInterval = '1m',
  chartStyle = 'candlestick',
  isLoading = false,
  isError = false,
  onChartIntervalChange,
}: DxyBasketPanelViewProps) {
  const points = useMemo(
    () => alignBasketPointsToOracleMark(history?.points ?? [], latest, oracleMark),
    [history?.points, latest, oracleMark]
  )
  const chartPoints = useMemo<ChartPoint[]>(
    () =>
      points.map((point) => ({
        timestamp: point.timestamp,
        price: oracleNumberToDisplayDxyPrice(toOraclePrice(point.basketPrice)),
      })),
    [points]
  )
  const chartIntervalSeconds = basketIntervalSecondsForChartInterval(chartInterval)
  const chartBuckets = useMemo(
    () => buildCandles(chartPoints, chartIntervalSeconds),
    [chartIntervalSeconds, chartPoints]
  )
  const chartSeries = useMemo<AreaData<UTCTimestamp>[]>(() => {
    return chartBuckets.map((candle) => ({
      time: candle.timestamp as UTCTimestamp,
      value: candle.close,
    }))
  }, [chartBuckets])
  const chartCandles = useMemo<CandlestickData<UTCTimestamp>[]>(() => {
    return chartBuckets.map((candle) => ({
      time: candle.timestamp as UTCTimestamp,
      open: candle.open,
      high: candle.high,
      low: candle.low,
      close: candle.close,
    }))
  }, [chartBuckets])

  const headerPrice = oracleMark
    ? oracleNumberToDisplayDxyPrice(toOraclePrice(oracleMark.basketPrice))
    : latest
      ? oracleNumberToDisplayDxyPrice(toOraclePrice(latest.basketPrice))
      : (chartPoints.at(-1)?.price ?? null)
  const changePct = computeBasketDisplayPriceChange(changeHistory?.points, latest) ?? null
  const positiveChange = changePct == null || changePct >= 0
  const lineColor = positiveChange ? DEFAULT_LINE_COLOR : NEGATIVE_LINE_COLOR

  return (
    <section className="bg-surface-panel border border-brand-border/30 overflow-hidden">
      <div className="flex flex-col gap-4 border-b border-brand-border/20 px-3 py-3 sm:px-5 sm:py-4 md:flex-row md:items-center md:justify-between">
        <div>
          <div className="flex items-center gap-2 text-content-secondary text-sm">
            <span className="material-symbols-outlined text-base text-brand-peach">show_chart</span>
            <span>plDXY Perp Price</span>
          </div>
          <div className="mt-1 flex flex-wrap items-baseline gap-x-4 gap-y-1">
            {headerPrice == null && isLoading ? (
              <Skeleton width={126} height={34} />
            ) : (
              <span className="text-2xl font-semibold leading-none text-content-primary sm:text-3xl">
                {headerPrice == null ? '--' : formatPrice(headerPrice)}
              </span>
            )}
            <span className={`text-sm font-semibold leading-none ${positiveChange ? 'text-positive' : 'text-brand-orange'}`}>
              {formatPercent(changePct)}
            </span>
          </div>
        </div>

        <div className="grid w-full grid-cols-4 border border-brand-border/30 bg-app-bg md:w-fit">
          {DXY_BASKET_CHART_INTERVALS.map((item) => (
            <button
              key={item.value}
              type="button"
              aria-label={item.ariaLabel}
              aria-pressed={chartInterval === item.value}
              className={`border px-2 py-2 text-sm font-semibold transition-colors hover:underline hover:underline-offset-4 focus-visible:underline focus-visible:underline-offset-4 sm:px-4 ${
                chartInterval === item.value
                  ? 'border-[#FFAB96] bg-[#FFAB96] text-app-bg'
                  : 'border-transparent text-content-secondary hover:bg-[#3B212D] hover:text-content-primary'
              }`}
              onClick={() => {
                onChartIntervalChange(item.value)
              }}
            >
              {item.label}
            </button>
          ))}
        </div>
      </div>

      <div className="p-3 sm:p-4 md:p-5">
        {isError ? (
          <Alert variant="warning" title="Basket history unavailable">
            The API has not returned stored Pyth basket snapshots yet.
          </Alert>
        ) : isLoading ? (
          <div className="h-[240px] sm:h-[320px]">
            <Skeleton variant="rectangular" height="100%" className="w-full" />
          </div>
        ) : chartSeries.length > 0 ? (
          <DxyBasketChart
            areaData={chartSeries}
            candlestickData={chartCandles}
            chartStyle={chartStyle}
            lineColor={lineColor}
          />
        ) : (
          <Alert variant="info" title="No basket snapshots">
            Waiting for the backend to ingest historical Pyth values.
          </Alert>
        )}
      </div>
    </section>
  )
}

export interface DxyBasketPanelProps {
  oraclePriceRaw?: bigint
  oraclePublishTime?: number
}

export function DxyBasketPanel({ oraclePriceRaw, oraclePublishTime }: DxyBasketPanelProps) {
  const [chartInterval, setChartInterval] = useState<DxyBasketChartInterval>('1m')
  const range = basketRangeForChartInterval(chartInterval)
  const intervalSeconds = basketRequestIntervalSecondsForChartInterval(chartInterval)
  const { data, isLoading, isError } = usePerpsBasketHistory(range, intervalSeconds)
  const { data: changeData } = usePerpsBasketHistory('24h', 60, true)
  const { data: latestData } = usePerpsBasketLatest()
  const oracleMark = useMemo<OracleMarkPoint | undefined>(() => {
    if (oraclePriceRaw === undefined || oraclePublishTime === undefined) return undefined

    return {
      timestamp: oraclePublishTime,
      basketPrice: oraclePriceRaw.toString(),
    }
  }, [oraclePriceRaw, oraclePublishTime])

  return (
    <DxyBasketPanelView
      history={data?.data}
      changeHistory={changeData?.data}
      latest={latestData?.data}
      oracleMark={oracleMark}
      chartInterval={chartInterval}
      isLoading={isLoading}
      isError={isError}
      onChartIntervalChange={setChartInterval}
    />
  )
}
