import { act, render, waitFor } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { TradingViewAdvancedChart } from './TradingViewAdvancedChart'
import type {
  TradingViewChart,
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
  vi.unstubAllEnvs()
  delete window.TradingView
})

describe('TradingViewAdvancedChart', () => {
  it('uses native controls and synchronizes their interval with the parent', async () => {
    vi.stubEnv('MODE', 'development')
    vi.stubEnv('VITE_TRADINGVIEW_CHARTS_ENABLED', 'true')

    const chartReady = deferred()
    const headerReady = deferred()
    const unsubscribe = vi.fn()
    let intervalCallback: ((resolution: string) => void) | undefined
    let currentResolution: string = '1'
    let widgetOptions: TradingViewWidgetOptions | undefined

    const subscription: TradingViewIntervalSubscription = {
      subscribe: (_context, callback) => {
        intervalCallback = callback
      },
      unsubscribe,
    }
    const chart: TradingViewChart = {
      resetData: vi.fn(),
      resolution: () => currentResolution,
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
      remove = remove
    }

    window.TradingView = {
      widget: FakeWidget as unknown as TradingViewNamespace['widget'],
    }

    const queryClient = new QueryClient({
      defaultOptions: { queries: { retry: false } },
    })
    const onIntervalChange = vi.fn()
    const onReadyChange = vi.fn()
    const chartElement = (interval: '1m' | '5m' | '1h' | '1d') => (
      <QueryClientProvider client={queryClient}>
        <TradingViewAdvancedChart
          interval={interval}
          fallback={<div>Fallback chart</div>}
          onIntervalChange={onIntervalChange}
          onReadyChange={onReadyChange}
        />
      </QueryClientProvider>
    )
    const view = render(chartElement('1m'))

    await waitFor(() => {
      expect(widgetOptions).toBeDefined()
    })
    expect(widgetOptions?.disabled_features).not.toContain('header_resolutions')
    expect(widgetOptions?.disabled_features).not.toContain('timeframes_toolbar')
    expect(widgetOptions?.favorites.intervals).toEqual(['1', '5', '60', '1D'])
    expect(widgetOptions?.time_frames).toEqual([
      { text: '1y', resolution: '1D', description: '1 Year' },
      { text: '30d', title: '1m', resolution: '60', description: '1 Month' },
      { text: '5d', resolution: '5', description: '5 Days' },
      { text: '1d', resolution: '1', description: '1 Day' },
    ])

    await act(async () => {
      chartReady.resolve()
      await Promise.resolve()
    })
    expect(onReadyChange).not.toHaveBeenCalledWith(true)

    await act(async () => {
      headerReady.resolve()
      await Promise.resolve()
    })
    expect(onReadyChange).toHaveBeenCalledWith(true)

    act(() => {
      currentResolution = '5'
      intervalCallback?.('5')
    })
    expect(onIntervalChange).toHaveBeenCalledTimes(1)
    expect(onIntervalChange).toHaveBeenCalledWith('5m')

    view.rerender(chartElement('5m'))
    expect(chart.setResolution).not.toHaveBeenCalled()

    view.rerender(chartElement('1h'))
    await waitFor(() => {
      expect(chart.setResolution).toHaveBeenCalledWith('60')
    })
    expect(onIntervalChange).toHaveBeenCalledTimes(1)

    view.unmount()
    expect(unsubscribe).toHaveBeenCalledWith(null, intervalCallback)
    expect(remove).toHaveBeenCalledOnce()
    expect(onReadyChange).toHaveBeenLastCalledWith(false)
    queryClient.clear()
  })
})
