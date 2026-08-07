import { useEffect, useRef, useState, type ReactNode } from 'react'
import { useQueryClient } from '@tanstack/react-query'
import { apiQueryKeys } from '../api'
import type { DxyBasketChartInterval } from '../components/dxyBasketChartConfig'
import type { OracleMarkPoint } from '../utils/dxyBasketChart'
import {
  PLDXY_TRADINGVIEW_SYMBOL,
  PletherDxyDatafeed,
  TRADINGVIEW_RESOLUTIONS,
  chartIntervalForTradingViewResolution,
  tradingViewResolutionForInterval,
} from './pletherDatafeed'
import type {
  TradingViewIntervalSubscription,
  TradingViewNamespace,
  TradingViewWidget,
} from './types'

const APP_BACKGROUND = '#0D0A1C'
const PANEL_BACKGROUND = '#171020'
const GRID_COLOR = 'rgba(255, 171, 150, 0.12)'
const TEXT_COLOR = '#D9CCD3'
const POSITIVE_COLOR = '#00FF99'
const NEGATIVE_COLOR = '#FF572D'
const TRADINGVIEW_TIME_FRAMES = [
  { text: '1y', resolution: '1D', description: '1 Year' },
  { text: '30d', title: '1m', resolution: '60', description: '1 Month' },
  { text: '5d', resolution: '5', description: '5 Days' },
  { text: '1d', resolution: '1', description: '1 Day' },
] as const

const libraryPromises = new Map<string, Promise<TradingViewNamespace>>()

function normalizeLibraryPath(): string {
  const configuredPath = (import.meta.env.VITE_TRADINGVIEW_LIBRARY_PATH as string | undefined)?.trim()
  const path = configuredPath?.length ? configuredPath : `${import.meta.env.BASE_URL}charting_library/`
  return path.endsWith('/') ? path : `${path}/`
}

function loadTradingViewLibrary(libraryPath: string): Promise<TradingViewNamespace> {
  if (window.TradingView) return Promise.resolve(window.TradingView)

  const pending = libraryPromises.get(libraryPath)
  if (pending) return pending

  const promise = new Promise<TradingViewNamespace>((resolve, reject) => {
    const script = document.createElement('script')
    script.src = `${libraryPath}charting_library.standalone.js`
    script.async = true
    script.dataset.tradingviewChartingLibrary = 'true'
    script.addEventListener('load', () => {
      if (window.TradingView) {
        resolve(window.TradingView)
      } else {
        reject(new Error('TradingView Advanced Charts loaded without exposing its widget API'))
      }
    })
    script.addEventListener('error', () => {
      script.remove()
      libraryPromises.delete(libraryPath)
      reject(new Error(`TradingView Advanced Charts assets are unavailable at ${libraryPath}`))
    })
    document.head.append(script)
  })

  libraryPromises.set(libraryPath, promise)
  return promise
}

function advancedChartsEnabled(): boolean {
  if (import.meta.env.MODE === 'test') return false
  return (import.meta.env.VITE_TRADINGVIEW_CHARTS_ENABLED as string | undefined) !== 'false'
}

export interface TradingViewAdvancedChartProps {
  interval: DxyBasketChartInterval
  oracleMark?: OracleMarkPoint
  fallback: ReactNode
  statusOverlay?: ReactNode
  onIntervalChange?: (interval: DxyBasketChartInterval) => void
  onReadyChange?: (ready: boolean) => void
}

export function TradingViewAdvancedChart({
  interval,
  oracleMark,
  fallback,
  statusOverlay,
  onIntervalChange,
  onReadyChange,
}: TradingViewAdvancedChartProps) {
  if (!advancedChartsEnabled()) return fallback

  return (
    <EnabledTradingViewAdvancedChart
      interval={interval}
      oracleMark={oracleMark}
      fallback={fallback}
      statusOverlay={statusOverlay}
      onIntervalChange={onIntervalChange}
      onReadyChange={onReadyChange}
    />
  )
}

function EnabledTradingViewAdvancedChart({
  interval,
  oracleMark,
  fallback,
  statusOverlay,
  onIntervalChange,
  onReadyChange,
}: TradingViewAdvancedChartProps) {
  const queryClient = useQueryClient()
  const containerRef = useRef<HTMLDivElement | null>(null)
  const widgetRef = useRef<TradingViewWidget | null>(null)
  const datafeedRef = useRef<PletherDxyDatafeed | null>(null)
  const intervalRef = useRef(interval)
  const readyRef = useRef(false)
  const oracleMarkRef = useRef(oracleMark)
  const onIntervalChangeRef = useRef(onIntervalChange)
  const onReadyChangeRef = useRef(onReadyChange)
  const [unavailable, setUnavailable] = useState(false)

  useEffect(() => {
    onIntervalChangeRef.current = onIntervalChange
  }, [onIntervalChange])

  useEffect(() => {
    onReadyChangeRef.current = onReadyChange
  }, [onReadyChange])

  useEffect(() => {
    oracleMarkRef.current = oracleMark
  }, [oracleMark])

  useEffect(() => {
    const container = containerRef.current
    if (!container) return

    let cancelled = false
    let intervalSubscription: TradingViewIntervalSubscription | undefined
    let handleIntervalChange: ((resolution: string) => void) | undefined
    const libraryPath = normalizeLibraryPath()
    const datafeed = new PletherDxyDatafeed({
      queryClient,
      oracleMark: oracleMarkRef.current,
      onHistoryGap: () => {
        void queryClient.invalidateQueries({
          queryKey: apiQueryKeys.perps.basketHistoryAll(),
        }).finally(() => {
          widgetRef.current?.activeChart().resetData()
        })
      },
    })
    datafeedRef.current = datafeed

    void loadTradingViewLibrary(libraryPath)
      .then((tradingView) => {
        if (cancelled) return

        const widget = new tradingView.widget({
          container,
          datafeed,
          interval: tradingViewResolutionForInterval(intervalRef.current),
          symbol: PLDXY_TRADINGVIEW_SYMBOL,
          library_path: libraryPath,
          locale: 'en',
          timezone: 'Etc/UTC',
          autosize: true,
          theme: 'dark',
          toolbar_bg: PANEL_BACKGROUND,
          custom_css_url: '../tradingview-chart.css',
          disabled_features: [
            'header_symbol_search',
            'symbol_search_hot_key',
            'header_compare',
            'header_screenshot',
            'header_fullscreen_button',
          ],
          enabled_features: ['iframe_loading_compatibility_mode'],
          favorites: {
            intervals: TRADINGVIEW_RESOLUTIONS,
          },
          time_frames: TRADINGVIEW_TIME_FRAMES.map((timeFrame) => ({ ...timeFrame })),
          custom_font_family: 'Uncut Sans, ui-sans-serif, system-ui, sans-serif',
          loading_screen: {
            backgroundColor: APP_BACKGROUND,
            foregroundColor: '#FFAB96',
          },
          overrides: {
            'paneProperties.backgroundType': 'solid',
            'paneProperties.background': APP_BACKGROUND,
            'paneProperties.vertGridProperties.color': GRID_COLOR,
            'paneProperties.horzGridProperties.color': GRID_COLOR,
            'scalesProperties.backgroundColor': APP_BACKGROUND,
            'scalesProperties.textColor': TEXT_COLOR,
            'mainSeriesProperties.candleStyle.upColor': POSITIVE_COLOR,
            'mainSeriesProperties.candleStyle.downColor': NEGATIVE_COLOR,
            'mainSeriesProperties.candleStyle.borderUpColor': POSITIVE_COLOR,
            'mainSeriesProperties.candleStyle.borderDownColor': NEGATIVE_COLOR,
            'mainSeriesProperties.candleStyle.wickUpColor': POSITIVE_COLOR,
            'mainSeriesProperties.candleStyle.wickDownColor': NEGATIVE_COLOR,
          },
        })
        widgetRef.current = widget
        void Promise.all([widget.chartReady(), widget.headerReady()])
          .then(() => {
            if (cancelled) return

            intervalSubscription = widget.activeChart().onIntervalChanged()
            handleIntervalChange = (resolution) => {
              const nextInterval = chartIntervalForTradingViewResolution(resolution)
              if (!nextInterval || nextInterval === intervalRef.current) return

              intervalRef.current = nextInterval
              onIntervalChangeRef.current?.(nextInterval)
            }
            intervalSubscription.subscribe(null, handleIntervalChange)
            readyRef.current = true

            const desiredResolution = tradingViewResolutionForInterval(intervalRef.current)
            if (widget.activeChart().resolution() !== desiredResolution) {
              void widget.activeChart().setResolution(desiredResolution)
            }
            onReadyChangeRef.current?.(true)
          })
          .catch(() => {
            if (!cancelled) {
              onReadyChangeRef.current?.(false)
              setUnavailable(true)
            }
          })
      })
      .catch(() => {
        if (!cancelled) {
          onReadyChangeRef.current?.(false)
          setUnavailable(true)
        }
      })

    return () => {
      cancelled = true
      readyRef.current = false
      if (intervalSubscription && handleIntervalChange) {
        intervalSubscription.unsubscribe(null, handleIntervalChange)
      }
      onReadyChangeRef.current?.(false)
      widgetRef.current?.remove()
      widgetRef.current = null
      datafeed.destroy()
      if (datafeedRef.current === datafeed) datafeedRef.current = null
    }
  }, [queryClient])

  useEffect(() => {
    datafeedRef.current?.setOracleMark(oracleMark)
  }, [oracleMark])

  useEffect(() => {
    intervalRef.current = interval
    const widget = widgetRef.current
    if (!widget || !readyRef.current) return

    const resolution = tradingViewResolutionForInterval(interval)
    if (widget.activeChart().resolution() === resolution) return
    void widget.activeChart().setResolution(resolution)
  }, [interval])

  if (unavailable) return fallback

  return (
    <div className="space-y-2">
      <div
        className="relative h-[360px] w-full overflow-hidden bg-app-bg sm:h-[460px]"
        role="region"
        aria-label="Interactive TradingView chart for the plDXY perpetual market"
      >
        <div ref={containerRef} className="h-full w-full" />
        <div className="pointer-events-none absolute inset-0 -z-10 bg-app-bg" />
        {statusOverlay ? (
          <div className="pointer-events-none absolute inset-0 z-10 flex items-center justify-center p-4">
            <div className="pointer-events-auto w-full max-w-lg shadow-2xl">{statusOverlay}</div>
          </div>
        ) : null}
      </div>
      <p className="text-right text-xs text-content-secondary">
        Advanced charts powered by{' '}
        <a
          href="https://www.tradingview.com/"
          target="_blank"
          rel="noopener"
          className="font-medium text-content-primary underline underline-offset-4"
        >
          TradingView
        </a>
        .
      </p>
    </div>
  )
}
