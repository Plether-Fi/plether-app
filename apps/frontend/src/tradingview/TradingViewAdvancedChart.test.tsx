import { act, render, waitFor } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { TradingViewAdvancedChart } from './TradingViewAdvancedChart'
import { PLETHER_DIRECTIONAL_VOLUME_STUDY_NAME } from './pletherDirectionalVolumeStudy'
import type { PletherDxyDatafeedOptions } from './pletherDatafeed'
import type {
  TradingViewChart,
  TradingViewCustomStatusDropDownContent,
  TradingViewCustomSymbolStatusAdapter,
  TradingViewIntervalChangeParameters,
  TradingViewIntervalChangedCallback,
  TradingViewIntervalSubscription,
  TradingViewNamespace,
  TradingViewResolution,
  TradingViewVisibleRangeChangedCallback,
  TradingViewVisibleRangeSubscription,
  TradingViewWidgetOptions,
} from './types'

const datafeedHarness = vi.hoisted(() => ({
  onVolumeCoverageChange: undefined as PletherDxyDatafeedOptions['onVolumeCoverageChange'],
}))

vi.mock('./pletherDatafeed', async (importOriginal) => {
  const actual = await importOriginal<typeof import('./pletherDatafeed')>()
  return {
    ...actual,
    PletherDxyDatafeed: class extends actual.PletherDxyDatafeed {
      constructor(options: PletherDxyDatafeedOptions = {}) {
        super(options)
        datafeedHarness.onVolumeCoverageChange = options.onVolumeCoverageChange
      }
    },
  }
})

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
  datafeedHarness.onVolumeCoverageChange = undefined
  vi.restoreAllMocks()
  vi.unstubAllEnvs()
  delete window.TradingView
})

function installReadyFakeTradingView() {
  let widgetOptions: TradingViewWidgetOptions | undefined
  let resolution: TradingViewResolution = '1'
  const subscription = {
    subscribe: vi.fn(),
    unsubscribe: vi.fn(),
  }
  const statusAdapter: TradingViewCustomSymbolStatusAdapter = {
    setVisible: vi.fn(() => statusAdapter),
    setIcon: vi.fn(() => statusAdapter),
    setColor: vi.fn(() => statusAdapter),
    setTooltip: vi.fn(() => statusAdapter),
    setDropDownContent: vi.fn(() => statusAdapter),
  }
  const mainPane = {
    hasMainSeries: () => true,
    getHeight: () => 400,
    setHeight: vi.fn(),
  }
  const directionalVolumePane = {
    hasMainSeries: () => false,
    getHeight: () => 120,
    setHeight: vi.fn(),
  }
  const chart: TradingViewChart = {
    resetData: vi.fn(),
    resolution: () => resolution,
    symbol: () => 'PLETHER:PLDXY.P',
    getVisibleRange: () => ({ from: 1_800_000_000, to: 1_800_432_000 }),
    getPanes: () => [mainPane, directionalVolumePane],
    setResolution: vi.fn(async (nextResolution) => {
      resolution = nextResolution
      return true
    }),
    onIntervalChanged: () => subscription,
    onVisibleRangeChanged: () => subscription,
    createShape: vi.fn(async () => 'liquidation-line'),
    createStudy: vi.fn(async () => 'directional-volume-study'),
    removeEntity: vi.fn(),
  }

  class FakeWidget {
    constructor(options: TradingViewWidgetOptions) {
      widgetOptions = options
      resolution = options.interval
    }

    chartReady = async () => {}
    headerReady = async () => {}
    activeChart = () => chart
    customSymbolStatus = () => ({ symbol: () => statusAdapter, hideAll: vi.fn() })
    remove = vi.fn()
  }

  window.TradingView = {
    widget: FakeWidget as unknown as TradingViewNamespace['widget'],
  }
  return { chart, directionalVolumePane, widgetOptions: () => widgetOptions }
}

describe('TradingViewAdvancedChart', () => {
  it('draws read-only TP/SL lines and removes them when protection is cleared', async () => {
    const { chart } = installReadyFakeTradingView()
    vi.mocked(chart.createShape).mockImplementation(async (_point, options) => options.text === 'Take profit' ? 'tp-line' : 'sl-line')
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } })
    const content = (enabled: boolean) => <QueryClientProvider client={queryClient}><TradingViewAdvancedChart interval="1m" takeProfitPrice={enabled ? 1.1 : undefined} stopLossPrice={enabled ? 0.9 : undefined} /></QueryClientProvider>
    const view = render(content(true))
    await waitFor(() => {
      expect(chart.createShape).toHaveBeenCalledWith({ price: 1.1 }, expect.objectContaining({ text: 'Take profit', lock: true, disableSelection: true }))
      expect(chart.createShape).toHaveBeenCalledWith({ price: 0.9 }, expect.objectContaining({ text: 'Stop loss', lock: true, disableSelection: true }))
    })
    view.rerender(content(false))
    await waitFor(() => {
      expect(chart.removeEntity).toHaveBeenCalledWith('tp-line')
      expect(chart.removeEntity).toHaveBeenCalledWith('sl-line')
    })
    view.unmount()
    queryClient.clear()
  })

  it('shows volume degradation only for the active interval and clears it after recovery', async () => {
    vi.stubEnv('VITE_PERPS_CANDLE_API_ENABLED', 'true')
    const fakeTradingView = installReadyFakeTradingView()
    const queryClient = new QueryClient({
      defaultOptions: { queries: { retry: false } },
    })
    const chart = (interval: '1m' | '5m') => (
      <QueryClientProvider client={queryClient}>
        <TradingViewAdvancedChart interval={interval} />
      </QueryClientProvider>
    )
    const view = render(chart('5m'))

    await waitFor(() => {
      expect(fakeTradingView.widgetOptions()).toBeDefined()
      expect(datafeedHarness.onVolumeCoverageChange).toBeTypeOf('function')
      expect(fakeTradingView.chart.createStudy).toHaveBeenCalledWith(
        PLETHER_DIRECTIONAL_VOLUME_STUDY_NAME,
        false,
        true
      )
      expect(fakeTradingView.directionalVolumePane.setHeight).toHaveBeenCalledWith(60)
    })
    expect(fakeTradingView.widgetOptions()?.disabled_features)
      .toContain('create_volume_indicator_by_default')
    expect(fakeTradingView.widgetOptions()?.custom_indicators_getter)
      .toBeTypeOf('function')
    expect(view.queryByText('Volume temporarily unavailable')).not.toBeInTheDocument()

    act(() => {
      datafeedHarness.onVolumeCoverageChange?.({
        intervalSeconds: 60,
        state: 'unavailable',
      })
    })
    expect(view.queryByText('Volume temporarily unavailable')).not.toBeInTheDocument()

    view.rerender(chart('1m'))
    expect(view.getByText('Volume temporarily unavailable')).toBeInTheDocument()
    expect(view.getByText(
      '— Price data is still live. Volume is being indexed for this market.'
    )).toBeInTheDocument()
    expect(view.getByRole('region', { name: /interactive tradingview chart/i }))
      .toBeInTheDocument()

    act(() => {
      datafeedHarness.onVolumeCoverageChange?.({
        intervalSeconds: 60,
        state: 'available',
      })
    })
    expect(view.queryByText('Volume temporarily unavailable')).not.toBeInTheDocument()

    view.unmount()
    queryClient.clear()
  })

  it('uses native controls and synchronizes their interval with the parent', async () => {
    const chartReady = deferred()
    const headerReady = deferred()
    const unsubscribe = vi.fn()
    const unsubscribeVisibleRange = vi.fn()
    let intervalCallback: TradingViewIntervalChangedCallback | undefined
    let visibleRangeCallback: TradingViewVisibleRangeChangedCallback | undefined
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
    const visibleRangeSubscription: TradingViewVisibleRangeSubscription = {
      subscribe: (_context, callback) => {
        visibleRangeCallback = callback
      },
      unsubscribe: unsubscribeVisibleRange,
    }
    const chart: TradingViewChart = {
      resetData: vi.fn(),
      resolution: () => currentResolution,
      symbol: () => 'PLETHER:PLDXY.P',
      getVisibleRange: vi.fn(() => ({
        from: 1_800_000_000,
        to: 1_800_432_000,
      })),
      getPanes: () => [],
      setResolution: vi.fn(async (resolution: TradingViewResolution) => {
        currentResolution = resolution
        intervalCallback?.(resolution, {})
        return true
      }),
      onIntervalChanged: () => subscription,
      onVisibleRangeChanged: () => visibleRangeSubscription,
      createShape: vi.fn(async () => 'liquidation-line'),
      createStudy: vi.fn(async () => 'directional-volume-study'),
      removeEntity: vi.fn(),
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
      interval: '1m' | '5m' | '15m' | '1h' | '1d',
      marketPhase: 'open' | 'close-only' | 'closed' | 'degraded' | 'paused' = 'open',
      marketCurrentDuration?: string,
      showLiquidationLine = true
    ) => (
      <QueryClientProvider client={queryClient}>
        <TradingViewAdvancedChart
          interval={interval}
          marketPhase={marketPhase}
          marketCurrentDuration={marketCurrentDuration}
          liquidationPrice={showLiquidationLine ? 1.0169 : undefined}
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
    expect(widgetOptions?.disabled_features).not.toContain('create_volume_indicator_by_default')
    expect(widgetOptions?.custom_indicators_getter).toBeUndefined()
    expect(widgetOptions?.studies_overrides['volume.volume.color.0']).toBe('#FFAB96')
    expect(widgetOptions?.studies_overrides['volume.volume.color.1']).toBe('#FFAB96')
    expect(widgetOptions?.timeframe).toBe('5D')
    expect(widgetOptions?.enabled_features).toEqual(expect.arrayContaining([
      'determine_first_data_request_size_using_visible_range',
      'hide_left_toolbar_by_default',
      'move_logo_to_main_pane',
      'remove_library_container_border',
    ]))
    expect(widgetOptions?.favorites.intervals).toEqual(['5', '60', '1D'])
    expect(widgetOptions?.time_frames).toEqual([
      { text: '1y', resolution: '1D', description: '1 Year' },
      { text: '30d', title: '1m', resolution: '60', description: '1 Month' },
      { text: '5d', resolution: '15', description: '5 Days' },
      { text: '1d', resolution: '5', description: '1 Day' },
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
      'volume.volume.color.0': '#FFAB96',
      'volume.volume.color.1': '#FFAB96',
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
    expect(chart.createShape).toHaveBeenCalledWith(
      { price: 1.0169 },
      expect.objectContaining({
        shape: 'horizontal_line',
        text: 'Liquidation',
        lock: true,
        disableSelection: true,
        disableSave: true,
        disableUndo: true,
        showInObjectsTree: false,
        overrides: expect.objectContaining({
          linecolor: '#F7D977',
          linestyle: 2,
          showPrice: true,
        }),
      })
    )

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
      visibleRangeCallback?.({
        from: 1_800_010_000,
        to: 1_800_420_000,
      })
    })

    // TradingView can begin recalculating the time scale before it emits the
    // interval event. Ignore that intermediate range because it belongs to a
    // resolution the parent has not accepted yet.
    act(() => {
      currentResolution = '5'
      visibleRangeCallback?.({
        from: 1_799_000_000,
        to: 1_801_000_000,
      })
    })

    const resolutionOnlyChange: TradingViewIntervalChangeParameters = {}
    act(() => {
      intervalCallback?.('5', resolutionOnlyChange)
    })
    expect(resolutionOnlyChange.timeframe).toEqual({
      type: 'time-range',
      from: 1_800_010_000,
      to: 1_800_420_000,
    })
    expect(onIntervalChange).toHaveBeenCalledTimes(1)
    expect(onIntervalChange).toHaveBeenCalledWith('5m')

    const explicitRangeChange: TradingViewIntervalChangeParameters = {
      timeframe: { type: 'period-back', value: '5D' },
    }
    act(() => {
      intervalCallback?.('5', explicitRangeChange)
    })
    expect(explicitRangeChange.timeframe).toEqual({
      type: 'period-back',
      value: '5D',
    })

    view.rerender(chartElement('5m'))
    expect(chart.setResolution).not.toHaveBeenCalled()

    act(() => {
      currentResolution = '15'
      intervalCallback?.('15', {})
    })
    expect(onIntervalChange).toHaveBeenCalledTimes(2)
    expect(onIntervalChange).toHaveBeenLastCalledWith('15m')

    view.rerender(chartElement('15m'))
    expect(chart.setResolution).not.toHaveBeenCalled()

    view.rerender(chartElement('1h'))
    await waitFor(() => {
      expect(chart.setResolution).toHaveBeenCalledWith('60')
    })
    expect(onIntervalChange).toHaveBeenCalledTimes(2)

    view.rerender(chartElement('1h', 'open', undefined, false))
    await waitFor(() => {
      expect(chart.removeEntity).toHaveBeenCalledWith('liquidation-line')
    })

    view.unmount()
    expect(unsubscribe).toHaveBeenCalledWith(null, intervalCallback)
    expect(unsubscribeVisibleRange).toHaveBeenCalledWith(null, visibleRangeCallback)
    expect(setVisible).toHaveBeenLastCalledWith(false)
    expect(remove).toHaveBeenCalledOnce()
    queryClient.clear()
  })

  it('shows an explicit unavailable state when the licensed runtime cannot load', async () => {
    vi.stubEnv('VITE_PERPS_CANDLE_API_ENABLED', 'true')
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
    act(() => {
      datafeedHarness.onVolumeCoverageChange?.({
        intervalSeconds: 300,
        state: 'unavailable',
      })
    })
    expect(view.queryByText('Volume temporarily unavailable')).not.toBeInTheDocument()
    expect(view.queryByRole('img', { name: /price performance chart/i })).not.toBeInTheDocument()

    view.unmount()
    queryClient.clear()
  })
})
