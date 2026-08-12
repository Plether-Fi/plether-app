import { useCallback, useEffect, useRef, useState } from 'react'
import { useQueryClient } from '@tanstack/react-query'
import { apiQueryKeys } from '../api'
import { Alert } from '../components/ui'
import type { DxyBasketChartInterval } from '../components/dxyBasketChartConfig'
import type { OracleMarkPoint } from '../utils/dxyBasketChart'
import type { PerpsMarketPhase } from '../utils/perpsMarketSchedule'
import {
  PLDXY_TRADINGVIEW_SYMBOL,
  PletherDxyDatafeed,
  TRADINGVIEW_FAVORITE_RESOLUTIONS,
  chartIntervalForTradingViewResolution,
  isPerpsCandleApiEnabled,
  tradingViewResolutionForInterval,
} from './pletherDatafeed'
import type {
  TradingViewIntervalSubscription,
  TradingViewNamespace,
  TradingViewCustomSymbolStatusAdapter,
  TradingViewChart,
  TradingViewEntityId,
  TradingViewWidget,
} from './types'
import { PLETHER_TRADINGVIEW_CUSTOM_THEMES } from './pletherTheme'

const APP_BACKGROUND = '#250917'
const PANEL_BACKGROUND = '#3B212D'
const GRID_COLOR = 'rgba(255, 171, 150, 0.12)'
const BORDER_COLOR = 'rgba(255, 171, 150, 0.22)'
const TEXT_COLOR = '#D8CBD0'
const BRAND_PEACH = '#FFAB96'
const BRAND_ORANGE = '#FF572D'
const POSITIVE_COLOR = '#00FF99'
const LIQUIDATION_COLOR = '#F7D977'
const MARKET_STATUS_ICON = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor"><circle cx="10" cy="10" r="5" /></svg>'
const CHART_STYLE_OVERRIDES = {
  volumePaneSize: 'small',
  'paneProperties.backgroundType': 'solid',
  'paneProperties.background': APP_BACKGROUND,
  'paneProperties.vertGridProperties.color': GRID_COLOR,
  'paneProperties.horzGridProperties.color': GRID_COLOR,
  'paneProperties.crossHairProperties.color': BRAND_PEACH,
  'paneProperties.crossHairProperties.transparency': 65,
  'paneProperties.separatorColor': BORDER_COLOR,
  'paneProperties.legendProperties.showBackground': false,
  'scalesProperties.lineColor': BORDER_COLOR,
  'scalesProperties.textColor': TEXT_COLOR,
  'scalesProperties.fontSize': 12,
  'mainSeriesProperties.statusViewStyle.showExchange': false,
  'mainSeriesProperties.priceLineColor': BRAND_PEACH,
  'mainSeriesProperties.candleStyle.upColor': POSITIVE_COLOR,
  'mainSeriesProperties.candleStyle.downColor': BRAND_ORANGE,
  'mainSeriesProperties.candleStyle.borderUpColor': POSITIVE_COLOR,
  'mainSeriesProperties.candleStyle.borderDownColor': BRAND_ORANGE,
  'mainSeriesProperties.candleStyle.wickUpColor': POSITIVE_COLOR,
  'mainSeriesProperties.candleStyle.wickDownColor': BRAND_ORANGE,
  'mainSeriesProperties.hollowCandleStyle.downColor': BRAND_ORANGE,
  'mainSeriesProperties.hollowCandleStyle.borderDownColor': BRAND_ORANGE,
  'mainSeriesProperties.hollowCandleStyle.wickDownColor': BRAND_ORANGE,
  'mainSeriesProperties.haStyle.downColor': BRAND_ORANGE,
  'mainSeriesProperties.haStyle.borderDownColor': BRAND_ORANGE,
  'mainSeriesProperties.haStyle.wickDownColor': BRAND_ORANGE,
  'mainSeriesProperties.barStyle.downColor': BRAND_ORANGE,
  'mainSeriesProperties.volCandlesStyle.downColor': BRAND_ORANGE,
  'mainSeriesProperties.volCandlesStyle.borderDownColor': BRAND_ORANGE,
  'mainSeriesProperties.volCandlesStyle.wickDownColor': BRAND_ORANGE,
  'mainSeriesProperties.columnStyle.upColor': POSITIVE_COLOR,
  'mainSeriesProperties.columnStyle.downColor': BRAND_ORANGE,
  'mainSeriesProperties.renkoStyle.downColor': BRAND_ORANGE,
  'mainSeriesProperties.renkoStyle.borderDownColor': BRAND_ORANGE,
  'mainSeriesProperties.renkoStyle.wickDownColor': BRAND_ORANGE,
  'mainSeriesProperties.pbStyle.downColor': BRAND_ORANGE,
  'mainSeriesProperties.pbStyle.borderDownColor': BRAND_ORANGE,
  'mainSeriesProperties.kagiStyle.downColor': BRAND_ORANGE,
  'mainSeriesProperties.pnfStyle.downColor': BRAND_ORANGE,
  'mainSeriesProperties.lineStyle.color': BRAND_PEACH,
  'mainSeriesProperties.lineStyle.linewidth': 2,
  'mainSeriesProperties.areaStyle.color1': 'rgba(255, 171, 150, 0.32)',
  'mainSeriesProperties.areaStyle.color2': 'rgba(255, 171, 150, 0.04)',
  'mainSeriesProperties.areaStyle.linecolor': BRAND_PEACH,
  'mainSeriesProperties.areaStyle.linewidth': 2,
  'mainSeriesProperties.baselineStyle.baselineColor': BORDER_COLOR,
  'mainSeriesProperties.baselineStyle.topFillColor1': 'rgba(0, 255, 153, 0.28)',
  'mainSeriesProperties.baselineStyle.topFillColor2': 'rgba(0, 255, 153, 0.04)',
  'mainSeriesProperties.baselineStyle.bottomFillColor1': 'rgba(255, 87, 45, 0.04)',
  'mainSeriesProperties.baselineStyle.bottomFillColor2': 'rgba(255, 87, 45, 0.28)',
  'mainSeriesProperties.baselineStyle.topLineColor': POSITIVE_COLOR,
  'mainSeriesProperties.baselineStyle.bottomLineColor': BRAND_ORANGE,
} satisfies Record<string, string | number | boolean>
const VOLUME_STUDY_OVERRIDES = {
  'volume.volume.color.0': BRAND_ORANGE,
  'volume.volume.color.1': POSITIVE_COLOR,
  'volume.volume.transparency': 20,
  'volume.volume ma.color': BRAND_PEACH,
  'volume.volume ma.transparency': 35,
  'volume.volume ma.linewidth': 1,
} satisfies Record<string, string | number | boolean>
const TRADINGVIEW_TIME_FRAMES = [
  { text: '1y', resolution: '1D', description: '1 Year' },
  { text: '30d', title: '1m', resolution: '60', description: '1 Month' },
  { text: '5d', resolution: '5', description: '5 Days' },
  { text: '1d', resolution: '1', description: '1 Day' },
] as const

const libraryPromises = new Map<string, Promise<TradingViewNamespace>>()

interface PletherMarketStatus {
  phase: PerpsMarketPhase
  currentDuration?: string
}

function escapeTradingViewHtml(value: string): string {
  return value
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;')
    .replaceAll("'", '&#039;')
}

function marketPhaseDescription(phase: PerpsMarketPhase): string {
  switch (phase) {
    case 'open':
      return 'Plether trading is open.'
    case 'close-only':
      return 'Plether is currently in close-only mode.'
    case 'closed':
      return 'Plether trading is currently closed.'
    case 'degraded':
      return 'Plether is currently in degraded mode.'
    case 'paused':
      return 'Plether trading is currently paused.'
  }
}

function applyPletherMarketStatus(
  adapter: TradingViewCustomSymbolStatusAdapter,
  status: PletherMarketStatus
): void {
  const isOpen = status.phase === 'open'
  const title = isOpen ? 'Market open' : 'Market closed'
  const color = isOpen ? POSITIVE_COLOR : BRAND_ORANGE
  const content = [marketPhaseDescription(status.phase)]
  const duration = status.currentDuration?.trim()

  if (duration) {
    content.push(
      '<br/><br/>',
      `It'll ${isOpen ? 'close' : 'open'} in ${escapeTradingViewHtml(duration)}.`
    )
  } else if (!isOpen) {
    content.push('<br/><br/>', 'A reopening time is not available yet.')
  }
  content.push('<br/><br/>', 'Exchange timezone: UTC')

  adapter.setIcon(MARKET_STATUS_ICON)
  adapter.setColor(color)
  adapter.setTooltip(title)
  adapter.setDropDownContent([{ title, color, content }])
  adapter.setVisible(true)
}

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

export interface TradingViewAdvancedChartProps {
  interval: DxyBasketChartInterval
  oracleMark?: OracleMarkPoint
  liquidationPrice?: number
  marketPhase?: PerpsMarketPhase
  marketCurrentDuration?: string
  onIntervalChange?: (interval: DxyBasketChartInterval) => void
}

export function TradingViewAdvancedChart({
  interval,
  oracleMark,
  liquidationPrice,
  marketPhase = 'open',
  marketCurrentDuration,
  onIntervalChange,
}: TradingViewAdvancedChartProps) {
  const queryClient = useQueryClient()
  const containerRef = useRef<HTMLDivElement | null>(null)
  const widgetRef = useRef<TradingViewWidget | null>(null)
  const datafeedRef = useRef<PletherDxyDatafeed | null>(null)
  const marketStatusAdapterRef = useRef<TradingViewCustomSymbolStatusAdapter | null>(null)
  const liquidationLineRef = useRef<TradingViewEntityId | null>(null)
  const liquidationLineRevisionRef = useRef(0)
  const marketStatusRef = useRef<PletherMarketStatus>({
    phase: marketPhase,
    currentDuration: marketCurrentDuration,
  })
  const intervalRef = useRef(interval)
  const readyRef = useRef(false)
  const oracleMarkRef = useRef(oracleMark)
  const liquidationPriceRef = useRef(liquidationPrice)
  const onIntervalChangeRef = useRef(onIntervalChange)
  const [unavailable, setUnavailable] = useState(false)

  const syncLiquidationLine = useCallback((chart: TradingViewChart, price: number | undefined) => {
    const revision = ++liquidationLineRevisionRef.current
    const previousLine = liquidationLineRef.current
    liquidationLineRef.current = null
    if (previousLine !== null) chart.removeEntity(previousLine)

    if (price === undefined || !Number.isFinite(price) || price <= 0) return

    void chart.createShape(
      { price },
      {
        shape: 'horizontal_line',
        text: 'Liquidation',
        lock: true,
        disableSelection: true,
        disableSave: true,
        disableUndo: true,
        showInObjectsTree: false,
        zOrder: 'top',
        overrides: {
          linecolor: LIQUIDATION_COLOR,
          linestyle: 2,
          linewidth: 2,
          showPrice: true,
          textcolor: LIQUIDATION_COLOR,
          fontsize: 12,
          bold: true,
          horzLabelsAlign: 'right',
          vertLabelsAlign: 'middle',
        },
      }
    ).then((lineId) => {
      if (revision !== liquidationLineRevisionRef.current) {
        if (readyRef.current) chart.removeEntity(lineId)
        return
      }
      if (!readyRef.current) return
      liquidationLineRef.current = lineId
    }).catch(() => {
      // The account panel still exposes the liquidation price if a chart drawing cannot be created.
    })
  }, [])

  useEffect(() => {
    onIntervalChangeRef.current = onIntervalChange
  }, [onIntervalChange])

  useEffect(() => {
    oracleMarkRef.current = oracleMark
  }, [oracleMark])

  useEffect(() => {
    liquidationPriceRef.current = liquidationPrice
    const widget = widgetRef.current
    if (!widget || !readyRef.current) return
    syncLiquidationLine(widget.activeChart(), liquidationPrice)
  }, [liquidationPrice, syncLiquidationLine])

  useEffect(() => {
    const status = { phase: marketPhase, currentDuration: marketCurrentDuration }
    marketStatusRef.current = status
    if (marketStatusAdapterRef.current) {
      applyPletherMarketStatus(marketStatusAdapterRef.current, status)
    }
  }, [marketCurrentDuration, marketPhase])

  useEffect(() => {
    const container = containerRef.current
    if (!container) return

    let cancelled = false
    let intervalSubscription: TradingViewIntervalSubscription | undefined
    let handleIntervalChange: ((resolution: string) => void) | undefined
    const libraryPath = normalizeLibraryPath()
    const useCandleApi = isPerpsCandleApiEnabled()
    const datafeed = new PletherDxyDatafeed({
      queryClient,
      oracleMark: oracleMarkRef.current,
      useCandleApi,
      onHistoryGap: (intervalSeconds) => {
        const candleQueryKey = apiQueryKeys.perps.basketCandlesAll()
        void queryClient.invalidateQueries({
          queryKey: useCandleApi
            ? candleQueryKey
            : apiQueryKeys.perps.basketHistoryAll(),
          predicate: useCandleApi && intervalSeconds !== undefined
            ? (query) => {
                const suffix = query.queryKey.slice(candleQueryKey.length)
                return suffix[0] === intervalSeconds || (
                  suffix[0] === 'current' && suffix[1] === intervalSeconds
                )
              }
            : undefined,
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
          timeframe: '5D',
          autosize: true,
          theme: 'dark',
          custom_themes: PLETHER_TRADINGVIEW_CUSTOM_THEMES,
          toolbar_bg: PANEL_BACKGROUND,
          custom_css_url: '../tradingview-chart.css?v=20260808-2',
          disabled_features: [
            'header_symbol_search',
            'symbol_search_hot_key',
            'header_compare',
            'header_screenshot',
            'header_fullscreen_button',
            'header_quick_search',
            'display_market_status',
            'volume_force_overlay',
          ],
          enabled_features: [
            'hide_left_toolbar_by_default',
            'iframe_loading_compatibility_mode',
            'move_logo_to_main_pane',
            'remove_library_container_border',
          ],
          favorites: {
            intervals: TRADINGVIEW_FAVORITE_RESOLUTIONS,
          },
          time_frames: TRADINGVIEW_TIME_FRAMES.map((timeFrame) => ({ ...timeFrame })),
          custom_font_family: 'Uncut Sans, ui-sans-serif, system-ui, sans-serif',
          loading_screen: {
            backgroundColor: APP_BACKGROUND,
            foregroundColor: BRAND_PEACH,
          },
          overrides: { ...CHART_STYLE_OVERRIDES },
          settings_overrides: { ...CHART_STYLE_OVERRIDES },
          studies_overrides: { ...VOLUME_STUDY_OVERRIDES },
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
            const marketStatusAdapter = widget
              .customSymbolStatus()
              .symbol(widget.activeChart().symbol())
            marketStatusAdapterRef.current = marketStatusAdapter
            applyPletherMarketStatus(marketStatusAdapter, marketStatusRef.current)
            readyRef.current = true
            syncLiquidationLine(widget.activeChart(), liquidationPriceRef.current)

            const desiredResolution = tradingViewResolutionForInterval(intervalRef.current)
            if (widget.activeChart().resolution() !== desiredResolution) {
              void widget.activeChart().setResolution(desiredResolution)
            }
          })
          .catch(() => {
            if (!cancelled) {
              setUnavailable(true)
            }
          })
      })
      .catch(() => {
        if (!cancelled) {
          setUnavailable(true)
        }
      })

    return () => {
      cancelled = true
      readyRef.current = false
      liquidationLineRevisionRef.current += 1
      if (intervalSubscription && handleIntervalChange) {
        intervalSubscription.unsubscribe(null, handleIntervalChange)
      }
      marketStatusAdapterRef.current?.setVisible(false)
      marketStatusAdapterRef.current = null
      liquidationLineRef.current = null
      widgetRef.current?.remove()
      widgetRef.current = null
      datafeed.destroy()
      if (datafeedRef.current === datafeed) datafeedRef.current = null
    }
  }, [queryClient, syncLiquidationLine])

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

  return (
    <div
      className="relative h-[450px] w-full overflow-hidden border border-brand-border/30 bg-app-bg sm:h-[580px]"
      role="region"
      aria-label="Interactive TradingView chart for the plDXY perpetual market"
    >
      <div ref={containerRef} className="h-full w-full" />
      <div className="pointer-events-none absolute inset-0 -z-10 bg-app-bg" />
      {unavailable ? (
        <div className="pointer-events-none absolute inset-0 z-10 flex items-center justify-center p-4">
          <div className="pointer-events-auto w-full max-w-lg shadow-2xl">
            <Alert variant="warning" title="TradingView chart unavailable">
              The interactive market chart could not be loaded. Refresh the page or try again later.
            </Alert>
          </div>
        </div>
      ) : null}
    </div>
  )
}
