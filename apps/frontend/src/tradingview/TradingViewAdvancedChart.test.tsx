import { act, render, waitFor } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { TradingViewAdvancedChart } from './TradingViewAdvancedChart'
import type {
  TradingViewChart,
  TradingViewCustomStatusDropDownContent,
  TradingViewCustomSymbolStatusAdapter,
  TradingViewIntervalSubscription,
  TradingViewNamespace,
  TradingViewResolution,
  TradingViewWidgetOptions,
} from './types'

function deferred() {
  let resolve!: () => void
  let reject!: (error: Error) => void
  const promise = new Promise<void>((resolvePromise, rejectPromise) => {
    resolve = resolvePromise
    reject = rejectPromise
  })
  return { promise, resolve, reject }
}

afterEach(() => {
  vi.restoreAllMocks()
  vi.unstubAllEnvs()
  delete window.TradingView
})

describe('TradingViewAdvancedChart', () => {
  it('uses native controls and synchronizes their interval with the parent', async () => {
    const chartReady = deferred()
    const headerReady = deferred()
    const unsubscribe = vi.fn()
    let intervalCallback: ((resolution: string) => void) | undefined
    let currentResolution: string = '1'
    let widgetOptions: TradingViewWidgetOptions | undefined
    const setVisible = vi.fn()
    const setIcon = vi.fn()
    const setColor = vi.fn()
    const setTooltip = vi.fn()
    const setDropDownContent = vi.fn()
    const marketStatusAdapter: TradingViewCustomSymbolStatusAdapter = {
      setVisible: (visible) => {
        setVisible(visible)
        return marketStatusAdapter
      },
      setIcon: (icon) => {
        setIcon(icon)
        return marketStatusAdapter
      },
      setColor: (color) => {
        setColor(color)
        return marketStatusAdapter
      },
      setTooltip: (tooltip) => {
        setTooltip(tooltip)
        return marketStatusAdapter
      },
      setDropDownContent: (content) => {
        setDropDownContent(content)
        return marketStatusAdapter
      },
    }
    const statusSymbol = vi.fn(() => marketStatusAdapter)

    const subscription: TradingViewIntervalSubscription = {
      subscribe: (_context, callback) => {
        intervalCallback = callback
      },
      unsubscribe,
    }
    const chart: TradingViewChart = {
      resetData: vi.fn(),
      resolution: () => currentResolution,
      symbol: () => 'PLETHER:PLDXY.P',
      setResolution: vi.fn(async (resolution: TradingViewResolution) => {
        currentResolution = resolution
        intervalCallback?.(resolution)
        return true
      }),
      onIntervalChanged: () => subscription,
    }
    const remove = vi.fn()

    class FakeWidget {
      constructor(options: TradingViewWidgetOptions) {
        widgetOptions = options
        currentResolution = options.interval
      }

      chartReady = () => chartReady.promise
      headerReady = () => headerReady.promise
      activeChart = () => chart
      customSymbolStatus = () => ({
        symbol: statusSymbol,
        hideAll: vi.fn(),
      })
      remove = remove
    }

    window.TradingView = {
      widget: FakeWidget as unknown as TradingViewNamespace['widget'],
    }

    const queryClient = new QueryClient({
      defaultOptions: { queries: { retry: false } },
    })
    const onIntervalChange = vi.fn()
    const chartElement = (
      interval: '1m' | '5m' | '1h' | '1d',
      marketPhase: 'open' | 'close-only' | 'closed' | 'degraded' | 'paused' = 'open',
      marketCurrentDuration?: string
    ) => (
      <QueryClientProvider client={queryClient}>
        <TradingViewAdvancedChart
          interval={interval}
          marketPhase={marketPhase}
          marketCurrentDuration={marketCurrentDuration}
          onIntervalChange={onIntervalChange}
        />
      </QueryClientProvider>
    )
    const view = render(chartElement('1m', 'open', '2d 4h'))

    await waitFor(() => {
      expect(widgetOptions).toBeDefined()
    })
    expect(view.getByRole('region', { name: /interactive tradingview chart/i })).toHaveClass(
      'h-[450px]',
      'sm:h-[580px]'
    )
    expect(widgetOptions?.disabled_features).not.toContain('header_resolutions')
    expect(widgetOptions?.disabled_features).not.toContain('timeframes_toolbar')
    expect(widgetOptions?.disabled_features).toContain('display_market_status')
    expect(widgetOptions?.disabled_features).toContain('volume_force_overlay')
    expect(widgetOptions?.timeframe).toBe('5D')
    expect(widgetOptions?.enabled_features).toEqual(expect.arrayContaining([
      'hide_left_toolbar_by_default',
      'move_logo_to_main_pane',
      'remove_library_container_border',
    ]))
    expect(widgetOptions?.favorites.intervals).toEqual(['5', '60', '1D'])
    expect(widgetOptions?.time_frames).toEqual([
      { text: '1y', resolution: '1D', description: '1 Year' },
      { text: '30d', title: '1m', resolution: '60', description: '1 Month' },
      { text: '5d', resolution: '5', description: '5 Days' },
      { text: '1d', resolution: '1', description: '1 Day' },
    ])
    expect(widgetOptions?.toolbar_bg).toBe('#3B212D')
    expect(widgetOptions?.custom_css_url).toBe('../tradingview-chart.css?v=20260808-2')
    expect(widgetOptions?.custom_themes.dark.color2).toMatchObject({
      3: '#FFF5F9',
      8: '#D8CBD0',
      15: '#4A2937',
      16: '#3B212D',
      17: '#250917',
    })
    expect(widgetOptions?.custom_themes.dark.color1[9]).toBe('#FFAB96')
    expect(widgetOptions?.custom_themes.dark.color3[9]).toBe('#FF572D')
    expect(widgetOptions?.custom_themes.dark.color4[9]).toBe('#00FF99')
    expect(widgetOptions?.custom_themes.dark.color7[9]).toBe('#F7D977')
    expect(widgetOptions?.loading_screen).toEqual({
      backgroundColor: '#250917',
      foregroundColor: '#FFAB96',
    })
    expect(widgetOptions?.overrides).toMatchObject({
      volumePaneSize: 'small',
      'paneProperties.background': '#250917',
      'paneProperties.crossHairProperties.color': '#FFAB96',
      'scalesProperties.lineColor': 'rgba(255, 171, 150, 0.22)',
      'scalesProperties.textColor': '#D8CBD0',
      'mainSeriesProperties.statusViewStyle.showExchange': false,
      'mainSeriesProperties.priceLineColor': '#FFAB96',
      'mainSeriesProperties.candleStyle.downColor': '#FF572D',
      'mainSeriesProperties.candleStyle.borderDownColor': '#FF572D',
      'mainSeriesProperties.candleStyle.wickDownColor': '#FF572D',
      'mainSeriesProperties.hollowCandleStyle.downColor': '#FF572D',
      'mainSeriesProperties.haStyle.downColor': '#FF572D',
      'mainSeriesProperties.barStyle.downColor': '#FF572D',
      'mainSeriesProperties.renkoStyle.downColor': '#FF572D',
      'mainSeriesProperties.lineStyle.color': '#FFAB96',
      'mainSeriesProperties.areaStyle.linecolor': '#FFAB96',
      'mainSeriesProperties.baselineStyle.topLineColor': '#00FF99',
      'mainSeriesProperties.baselineStyle.bottomLineColor': '#FF572D',
    })
    expect(widgetOptions?.settings_overrides).toEqual(widgetOptions?.overrides)
    expect(widgetOptions?.studies_overrides).toEqual({
      'volume.volume.color.0': '#FF572D',
      'volume.volume.color.1': '#00FF99',
      'volume.volume.transparency': 20,
      'volume.volume ma.color': '#FFAB96',
      'volume.volume ma.transparency': 35,
      'volume.volume ma.linewidth': 1,
    })

    await act(async () => {
      chartReady.resolve()
      await Promise.resolve()
    })

    await act(async () => {
      headerReady.resolve()
      await Promise.resolve()
    })
    expect(statusSymbol).toHaveBeenCalledWith('PLETHER:PLDXY.P')
    expect(setColor).toHaveBeenLastCalledWith('#00FF99')
    expect(setTooltip).toHaveBeenLastCalledWith('Market open')
    expect(setDropDownContent).toHaveBeenLastCalledWith([
      expect.objectContaining({
        title: 'Market open',
        color: '#00FF99',
        content: expect.arrayContaining(["It'll close in 2d 4h."]),
      }) as TradingViewCustomStatusDropDownContent,
    ])
    expect(setVisible).toHaveBeenLastCalledWith(true)

    view.rerender(chartElement('1m', 'close-only', '1d 3h'))
    expect(setColor).toHaveBeenLastCalledWith('#FF572D')
    expect(setTooltip).toHaveBeenLastCalledWith('Market closed')
    expect(setDropDownContent).toHaveBeenLastCalledWith([
      expect.objectContaining({
        title: 'Market closed',
        color: '#FF572D',
        content: expect.arrayContaining([
          'Plether is currently in close-only mode.',
          "It'll open in 1d 3h.",
        ]),
      }) as TradingViewCustomStatusDropDownContent,
    ])

    view.rerender(chartElement('1m', 'degraded', undefined))
    expect(setTooltip).toHaveBeenLastCalledWith('Market closed')
    expect(setDropDownContent).toHaveBeenLastCalledWith([
      expect.objectContaining({
        title: 'Market closed',
        content: expect.arrayContaining([
          'Plether is currently in degraded mode.',
          'A reopening time is not available yet.',
        ]),
      }) as TradingViewCustomStatusDropDownContent,
    ])

    view.rerender(chartElement('1m', 'closed', '1d 3h'))
    expect(setColor).toHaveBeenLastCalledWith('#FF572D')
    expect(setDropDownContent).toHaveBeenLastCalledWith([
      expect.objectContaining({
        title: 'Market closed',
        content: expect.arrayContaining([
          'Plether trading is currently closed.',
          "It'll open in 1d 3h.",
        ]),
      }) as TradingViewCustomStatusDropDownContent,
    ])

    view.rerender(chartElement('1m', 'paused', undefined))
    expect(setColor).toHaveBeenLastCalledWith('#FF572D')
    expect(setDropDownContent).toHaveBeenLastCalledWith([
      expect.objectContaining({
        title: 'Market closed',
        content: expect.arrayContaining([
          'Plether trading is currently paused.',
          'A reopening time is not available yet.',
        ]),
      }) as TradingViewCustomStatusDropDownContent,
    ])

    act(() => {
      currentResolution = '5'
      intervalCallback?.('5')
    })
    expect(onIntervalChange).toHaveBeenCalledTimes(1)
    expect(onIntervalChange).toHaveBeenCalledWith('5m')

    view.rerender(chartElement('5m'))
    expect(chart.setResolution).not.toHaveBeenCalled()

    act(() => {
      currentResolution = '15'
      intervalCallback?.('15')
    })
    expect(onIntervalChange).toHaveBeenCalledTimes(1)

    view.rerender(chartElement('5m'))
    expect(chart.setResolution).not.toHaveBeenCalled()

    view.rerender(chartElement('1h'))
    await waitFor(() => {
      expect(chart.setResolution).toHaveBeenCalledWith('60')
    })
    expect(onIntervalChange).toHaveBeenCalledTimes(1)

    view.unmount()
    expect(unsubscribe).toHaveBeenCalledWith(null, intervalCallback)
    expect(setVisible).toHaveBeenLastCalledWith(false)
    expect(remove).toHaveBeenCalledOnce()
    queryClient.clear()
  })

  it('shows an explicit unavailable state when the licensed runtime cannot load', async () => {
    vi.stubEnv('VITE_TRADINGVIEW_LIBRARY_PATH', '/missing-charting-library/')
    const append = vi.spyOn(document.head, 'append').mockImplementation(() => {})

    const queryClient = new QueryClient({
      defaultOptions: { queries: { retry: false } },
    })
    const view = render(
      <QueryClientProvider client={queryClient}>
        <TradingViewAdvancedChart interval="5m" />
      </QueryClientProvider>
    )

    await waitFor(() => {
      expect(append).toHaveBeenCalledOnce()
    })
    const script = append.mock.calls[0][0] as HTMLScriptElement
    act(() => {
      script.dispatchEvent(new Event('error'))
    })

    expect(await view.findByText('TradingView chart unavailable')).toBeInTheDocument()
    expect(view.queryByRole('img', { name: /price performance chart/i })).not.toBeInTheDocument()

    view.unmount()
    queryClient.clear()
  })
})
