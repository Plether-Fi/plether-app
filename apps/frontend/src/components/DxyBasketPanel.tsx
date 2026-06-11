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
  type BasketComponentPrice,
} from '../api'
import {
  DXY_BASKET_CHART_INTERVALS,
  basketIntervalSecondsForChartInterval,
  basketRequestIntervalSecondsForChartInterval,
  basketRangeForChartInterval,
  type DxyBasketChartInterval,
} from './dxyBasketChartConfig'
import { Alert, Skeleton } from './ui'

const CHART_HEIGHT = 320
const DEFAULT_LINE_COLOR = '#00FF99'
const COMPONENT_PRICE_FRESH_SECONDS = 10 * 60

export interface ChartPoint {
  timestamp: number
  price: number
}

export interface ChartCandle {
  timestamp: number
  open: number
  high: number
  low: number
  close: number
}

export type DxyBasketChartStyle = 'area' | 'candlestick'

function toOraclePrice(raw: string): number {
  return Number(raw) / 1e8
}

export function oracleNumberToDisplayDxyPrice(rawOraclePrice: number): number {
  if (!Number.isFinite(rawOraclePrice) || rawOraclePrice <= 0) return 0
  return Math.max(0, 2 - rawOraclePrice)
}

function formatPrice(value: number): string {
  return value.toLocaleString('en-US', {
    minimumFractionDigits: 4,
    maximumFractionDigits: 4,
  })
}

function formatCompactPrice(value: number): string {
  return value.toLocaleString('en-US', {
    minimumFractionDigits: 3,
    maximumFractionDigits: 3,
  })
}

function formatPercent(value: number | null | undefined): string {
  if (value == null) return '--'
  const sign = value > 0 ? '+' : ''
  return `${sign}${(value * 100).toFixed(2)}%`
}

function formatUpdateAge(ageSeconds: number): string {
  if (!Number.isFinite(ageSeconds) || ageSeconds < 0) return 'unknown age'
  if (ageSeconds < 60) return `${ageSeconds}s ago`

  const minutes = Math.floor(ageSeconds / 60)
  const seconds = ageSeconds % 60
  if (minutes < 60) return seconds > 0 ? `${minutes}m ${seconds}s ago` : `${minutes}m ago`

  const hours = Math.floor(minutes / 60)
  const remainingMinutes = minutes % 60
  if (hours < 24) return remainingMinutes > 0 ? `${hours}h ${remainingMinutes}m ago` : `${hours}h ago`

  const days = Math.floor(hours / 24)
  const remainingHours = hours % 24
  return remainingHours > 0 ? `${days}d ${remainingHours}h ago` : `${days}d ago`
}

function freshnessTooltip(publishTime: number | undefined, nowSeconds: number): string | undefined {
  if (!publishTime) return undefined
  return `updated ${formatUpdateAge(Math.max(0, nowSeconds - publishTime))}`
}

function componentPrice(component: BasketComponentPrice): string {
  return formatCompactPrice(toOraclePrice(component.price))
}

function componentWeight(component: BasketComponentPrice): string {
  return `${(component.weightBps / 100).toFixed(1)}%`
}

export interface DxyBasketPanelViewProps {
  history?: BasketHistory
  latest?: BasketLatest
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
  return lineColor === '#00FF99' ? 'rgba(0, 255, 153, 0.24)' : 'rgba(255, 0, 204, 0.24)'
}

function ComponentFreshnessDot({ publishTime, nowSeconds }: { publishTime?: number; nowSeconds: number }) {
  const tooltip = freshnessTooltip(publishTime, nowSeconds)
  if (!tooltip) return null

  const ageSeconds = Math.max(0, nowSeconds - (publishTime ?? nowSeconds))
  const isFresh = ageSeconds <= COMPONENT_PRICE_FRESH_SECONDS

  return (
    <span
      className={`group relative h-2 w-2 shrink-0 rounded-full ${isFresh ? 'bg-cyber-neon-green' : 'bg-cyber-electric-fuchsia'}`}
      aria-label={isFresh ? 'Price fresh' : 'Price stale'}
      tabIndex={0}
      title={tooltip}
    >
      <span className="pointer-events-none absolute bottom-full left-1/2 z-20 mb-2 hidden w-max max-w-56 -translate-x-1/2 border border-cyber-border-glow/30 bg-cyber-bg px-2.5 py-1.5 text-xs font-medium leading-4 text-cyber-text-primary shadow-lg shadow-cyber-border-glow/10 group-hover:block group-focus:block">
        {tooltip}
      </span>
    </span>
  )
}

export function buildCandles(points: ChartPoint[], intervalSeconds: number): ChartCandle[] {
  const candles: ChartCandle[] = []
  let currentCandle: ChartCandle | undefined
  let previousClose: number | null = null

  const sortedPoints = [...points].sort((left, right) => left.timestamp - right.timestamp)

  for (const point of sortedPoints) {
    const timestamp = Math.floor(point.timestamp / intervalSeconds) * intervalSeconds

    if (currentCandle?.timestamp === timestamp) {
      currentCandle.high = Math.max(currentCandle.high, point.price)
      currentCandle.low = Math.min(currentCandle.low, point.price)
      currentCandle.close = point.price
      previousClose = point.price
      continue
    }

    if (currentCandle) {
      candles.push(currentCandle)
    }

    const open = previousClose ?? point.price
    currentCandle = {
      timestamp,
      open,
      high: Math.max(open, point.price),
      low: Math.min(open, point.price),
      close: point.price,
    }
    previousClose = point.price
  }

  if (currentCandle) {
    candles.push(currentCandle)
  }

  return candles
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
      height: CHART_HEIGHT,
      layout: {
        background: { type: ColorType.Solid, color: 'transparent' },
        textColor: '#A6B3D9',
        fontFamily: 'Uncut Sans, ui-sans-serif, system-ui, sans-serif',
        fontSize: 12,
      },
      grid: {
        vertLines: { color: 'rgba(74, 0, 255, 0.1)' },
        horzLines: { color: 'rgba(74, 0, 255, 0.18)' },
      },
      crosshair: {
        mode: CrosshairMode.Normal,
        vertLine: {
          color: 'rgba(0, 204, 255, 0.48)',
          labelBackgroundColor: '#00CCFF',
        },
        horzLine: {
          color: 'rgba(0, 204, 255, 0.28)',
          labelBackgroundColor: '#00CCFF',
        },
      },
      localization: {
        priceFormatter: formatPrice,
      },
      rightPriceScale: {
        borderColor: 'rgba(74, 0, 255, 0.28)',
        scaleMargins: {
          top: 0.14,
          bottom: 0.12,
        },
      },
      timeScale: {
        borderColor: 'rgba(74, 0, 255, 0.28)',
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
        downColor: '#FF00CC',
        borderUpColor: '#00FF99',
        borderDownColor: '#FF00CC',
        wickUpColor: '#00FF99',
        wickDownColor: '#FF00CC',
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
        height: CHART_HEIGHT,
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
      className="relative h-[320px] w-full overflow-hidden"
      role="img"
      aria-label="DXY price performance chart"
    >
      <div ref={containerRef} className="h-full w-full" />
    </div>
  )
}

export function DxyBasketPanelView({
  history,
  latest,
  chartInterval = '1m',
  chartStyle = 'candlestick',
  isLoading = false,
  isError = false,
  onChartIntervalChange,
}: DxyBasketPanelViewProps) {
  const points = useMemo(() => history?.points ?? [], [history?.points])
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

  const latestPoint = chartPoints.at(-1) ?? null
  const latestComponents = latest?.components ?? points.at(-1)?.components ?? []
  const [nowSeconds, setNowSeconds] = useState(() => Math.floor(Date.now() / 1000))
  const firstPoint = chartPoints[0] ?? null
  const changePct = firstPoint && latestPoint && firstPoint.price > 0
    ? (latestPoint.price - firstPoint.price) / firstPoint.price
    : null
  const positiveChange = changePct == null || changePct >= 0
  const lineColor = positiveChange ? '#00FF99' : '#FF00CC'

  useEffect(() => {
    const interval = window.setInterval(() => {
      setNowSeconds(Math.floor(Date.now() / 1000))
    }, 5_000)

    return () => {
      window.clearInterval(interval)
    }
  }, [])

  return (
    <section className="bg-cyber-surface-dark border border-cyber-border-glow/30 shadow-lg shadow-cyber-border-glow/10 overflow-hidden">
      <div className="px-5 py-4 border-b border-cyber-border-glow/20 flex flex-col gap-4 md:flex-row md:items-center md:justify-between">
        <div>
          <div className="flex items-center gap-2 text-cyber-text-secondary text-sm">
            <span className="material-symbols-outlined text-base text-cyber-bright-blue">show_chart</span>
            <span>DXY Price</span>
          </div>
          <div className="mt-1 flex flex-wrap items-end gap-x-4 gap-y-1">
            {isLoading ? (
              <Skeleton width={126} height={34} />
            ) : (
              <span className="text-3xl font-semibold text-cyber-text-primary">
                {latestPoint ? formatPrice(latestPoint.price) : '--'}
              </span>
            )}
            <span className={`text-sm font-semibold ${positiveChange ? 'text-cyber-neon-green' : 'text-cyber-electric-fuchsia'}`}>
              {formatPercent(changePct)}
            </span>
          </div>
        </div>

        <div className="inline-grid grid-cols-4 border border-cyber-border-glow/30 bg-cyber-bg/50 w-fit">
          {DXY_BASKET_CHART_INTERVALS.map((item) => (
            <button
              key={item.value}
              type="button"
              aria-label={item.ariaLabel}
              aria-pressed={chartInterval === item.value}
              className={`px-4 py-2 text-sm font-semibold transition-colors ${
                chartInterval === item.value
                  ? 'bg-cyber-bright-blue text-cyber-bg'
                  : 'text-cyber-text-secondary hover:text-cyber-text-primary hover:bg-cyber-surface-light/70'
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

      <div className="p-4 md:p-5">
        {isError ? (
          <Alert variant="warning" title="Basket history unavailable">
            The API has not returned stored Pyth basket snapshots yet.
          </Alert>
        ) : isLoading ? (
          <Skeleton variant="rectangular" height={320} className="w-full" />
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

        <div className="mt-4 grid grid-cols-2 md:grid-cols-3 xl:grid-cols-6 gap-2">
          {latestComponents.map((component) => (
            <div key={component.feedId} className="border border-cyber-border-glow/20 bg-cyber-bg/35 px-3 py-2 min-h-[74px]">
              <div className="flex items-center justify-between gap-2">
                <div className="flex min-w-0 items-center gap-2">
                  <ComponentFreshnessDot publishTime={component.publishTime} nowSeconds={nowSeconds} />
                  <span className="truncate text-sm font-semibold text-cyber-text-primary">{component.symbol}</span>
                </div>
                <span className="shrink-0 text-xs text-cyber-text-secondary">{componentWeight(component)}</span>
              </div>
              <div className="mt-2 text-lg font-semibold text-cyber-bright-blue">{componentPrice(component)}</div>
              <div className="text-xs text-cyber-text-secondary">{component.inverted ? `${component.feedSymbol} inv` : component.feedSymbol}</div>
            </div>
          ))}
        </div>
      </div>
    </section>
  )
}

export function DxyBasketPanel() {
  const [chartInterval, setChartInterval] = useState<DxyBasketChartInterval>('1m')
  const range = basketRangeForChartInterval(chartInterval)
  const intervalSeconds = basketRequestIntervalSecondsForChartInterval(chartInterval)
  const { data, isLoading, isError } = usePerpsBasketHistory(range, intervalSeconds)
  const { data: latestData } = usePerpsBasketLatest()

  return (
    <DxyBasketPanelView
      history={data?.data}
      latest={latestData?.data}
      chartInterval={chartInterval}
      isLoading={isLoading}
      isError={isError}
      onChartIntervalChange={setChartInterval}
    />
  )
}
