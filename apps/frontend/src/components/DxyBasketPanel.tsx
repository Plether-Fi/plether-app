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
import { Alert, Skeleton, Tooltip } from './ui'
import {
  alignBasketPointsToOracleMark,
  buildCandles,
  computeBasketComponentPriceChanges,
  oracleNumberToDisplayDxyPrice,
  type ChartPoint,
  type OracleMarkPoint,
} from '../utils/dxyBasketChart'

const CHART_HEIGHT = 320
const DEFAULT_LINE_COLOR = '#00FF99'
const CHART_GRID_COLOR = 'rgba(255, 171, 150, 0.16)'
const CHART_AXIS_COLOR = 'rgba(255, 171, 150, 0.32)'
const CHART_TEXT_COLOR = '#D9CCD3'
const COMPONENT_PRICE_FRESH_SECONDS = 10 * 60

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
  if (ageSeconds < 60) return `${ageSeconds.toString()}s ago`

  const minutes = Math.floor(ageSeconds / 60)
  const seconds = ageSeconds % 60
  if (minutes < 60) return seconds > 0 ? `${minutes.toString()}m ${seconds.toString()}s ago` : `${minutes.toString()}m ago`

  const hours = Math.floor(minutes / 60)
  const remainingMinutes = minutes % 60
  if (hours < 24) return remainingMinutes > 0 ? `${hours.toString()}h ${remainingMinutes.toString()}m ago` : `${hours.toString()}h ago`

  const days = Math.floor(hours / 24)
  const remainingHours = hours % 24
  return remainingHours > 0 ? `${days.toString()}d ${remainingHours.toString()}h ago` : `${days.toString()}d ago`
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

function componentChangeClass(value: number): string {
  if (Math.abs(value) < 0.00005) return 'text-content-secondary/70'
  return value > 0 ? 'text-positive/75' : 'text-brand-orange/75'
}

export interface DxyBasketPanelViewProps {
  history?: BasketHistory
  componentChangeHistory?: BasketHistory
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
  return lineColor === '#00FF99' ? 'rgba(0, 255, 153, 0.24)' : 'rgba(255, 0, 204, 0.24)'
}

function ComponentFreshnessDot({ publishTime, nowSeconds }: { publishTime?: number; nowSeconds: number }) {
  const tooltip = freshnessTooltip(publishTime, nowSeconds)
  if (!tooltip) return null

  const ageSeconds = Math.max(0, nowSeconds - (publishTime ?? nowSeconds))
  const isFresh = ageSeconds <= COMPONENT_PRICE_FRESH_SECONDS

  return (
    <Tooltip content={tooltip} position="top">
      <span
        className={`h-2 w-2 shrink-0 rounded-full ${isFresh ? 'bg-positive' : 'bg-brand-orange'}`}
        aria-label={isFresh ? 'Price fresh' : 'Price stale'}
        tabIndex={0}
      />
    </Tooltip>
  )
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
      aria-label="plDXY Perp price performance chart"
    >
      <div ref={containerRef} className="h-full w-full" />
    </div>
  )
}

export function DxyBasketPanelView({
  history,
  componentChangeHistory,
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

  const latestPoint = chartPoints.at(-1) ?? null
  const latestComponents = latest?.components ?? points.at(-1)?.components ?? []
  const componentPriceChanges = useMemo(
    () => computeBasketComponentPriceChanges(componentChangeHistory?.points, latest),
    [componentChangeHistory?.points, latest]
  )
  const [nowSeconds, setNowSeconds] = useState(() => Math.floor(Date.now() / 1000))
  const lineColor = '#00FF99'

  useEffect(() => {
    const interval = window.setInterval(() => {
      setNowSeconds(Math.floor(Date.now() / 1000))
    }, 5_000)

    return () => {
      window.clearInterval(interval)
    }
  }, [])

  return (
    <section className="bg-surface-panel border border-brand-border/30 overflow-hidden">
      <div className="px-5 py-4 border-b border-brand-border/20 flex flex-col gap-4 md:flex-row md:items-center md:justify-between">
        <div>
          <div className="flex items-center gap-2 text-content-secondary text-sm">
            <span className="material-symbols-outlined text-base text-brand-peach">show_chart</span>
            <span>plDXY Perp Price</span>
          </div>
          <div className="mt-1 flex flex-wrap items-end gap-x-4 gap-y-1">
            {isLoading ? (
              <Skeleton width={126} height={34} />
            ) : (
              <span className="text-3xl font-semibold text-content-primary">
                {latestPoint ? formatPrice(latestPoint.price) : '--'}
              </span>
            )}
          </div>
        </div>

        <div className="inline-grid grid-cols-4 border border-brand-border/30 bg-app-bg w-fit">
          {DXY_BASKET_CHART_INTERVALS.map((item) => (
            <button
              key={item.value}
              type="button"
              aria-label={item.ariaLabel}
              aria-pressed={chartInterval === item.value}
              className={`border px-4 py-2 text-sm font-semibold transition-colors hover:underline hover:underline-offset-4 focus-visible:underline focus-visible:underline-offset-4 ${
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
          {latestComponents.map((component) => {
            const priceChange = componentPriceChanges[component.feedId || component.symbol]

            return (
              <div key={component.feedId} className="border border-brand-border/20 bg-app-bg px-3 py-2 min-h-[74px]">
                <div className="flex items-center justify-between gap-2">
                  <div className="flex min-w-0 items-center gap-2">
                    <ComponentFreshnessDot publishTime={component.publishTime} nowSeconds={nowSeconds} />
                    <span className="truncate text-sm font-semibold text-content-primary">{component.symbol}</span>
                  </div>
                  <span className="shrink-0 text-xs text-content-secondary">{componentWeight(component)}</span>
                </div>
                <div className="mt-2 flex min-w-0 items-baseline gap-2">
                  <span className="text-lg font-semibold text-brand-peach">{componentPrice(component)}</span>
                  {priceChange !== undefined ? (
                    <span
                      className={`shrink-0 text-[11px] font-medium ${componentChangeClass(priceChange)}`}
                      title="24h change"
                      aria-label={`24 hour change ${formatPercent(priceChange)}`}
                    >
                      {formatPercent(priceChange)}
                    </span>
                  ) : null}
                </div>
                <div className="text-xs text-content-secondary">{component.inverted ? `${component.feedSymbol} inv` : component.feedSymbol}</div>
              </div>
            )
          })}
        </div>
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
  const { data: componentChangeData } = usePerpsBasketHistory('24h', 60, true)
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
      componentChangeHistory={componentChangeData?.data}
      latest={latestData?.data}
      oracleMark={oracleMark}
      chartInterval={chartInterval}
      isLoading={isLoading}
      isError={isError}
      onChartIntervalChange={setChartInterval}
    />
  )
}
