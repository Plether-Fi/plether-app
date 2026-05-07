import { useEffect, useState } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { DxyBasketPanelView, type DxyBasketChartStyle } from '../components/DxyBasketPanel'
import {
  basketRangeForChartInterval,
  type DxyBasketChartInterval,
} from '../components/dxyBasketChartConfig'
import type { BasketComponentPrice, BasketHistory } from '../api'

type MarketShape = 'rally' | 'selloff'
type StoryState = 'ready' | 'loading' | 'error' | 'empty'

interface DxyBasketStoryProps {
  initialInterval: DxyBasketChartInterval
  market: MarketShape
  chartStyle: DxyBasketChartStyle
  state: StoryState
}

interface ComponentSeed {
  symbol: string
  feedSymbol: string
  feedId: string
  basePrice: number
  rawPrice: number
  exponent: number
  inverted: boolean
  weightBps: number
  phase: number
}

const GENERATED_AT = Date.UTC(2026, 3, 30, 16, 0, 0) / 1000

const COMPONENT_SEEDS: ComponentSeed[] = [
  {
    symbol: 'EUR/USD',
    feedSymbol: 'EUR/USD',
    feedId: 'storybook-eur-usd',
    basePrice: 1.175,
    rawPrice: 1.175,
    exponent: -8,
    inverted: false,
    weightBps: 5760,
    phase: 0.2,
  },
  {
    symbol: 'JPY/USD',
    feedSymbol: 'USD/JPY',
    feedId: 'storybook-usd-jpy',
    basePrice: 0.00638,
    rawPrice: 156.74,
    exponent: -8,
    inverted: true,
    weightBps: 1360,
    phase: 1.4,
  },
  {
    symbol: 'GBP/USD',
    feedSymbol: 'GBP/USD',
    feedId: 'storybook-gbp-usd',
    basePrice: 1.3448,
    rawPrice: 1.3448,
    exponent: -8,
    inverted: false,
    weightBps: 1190,
    phase: 2.1,
  },
  {
    symbol: 'CAD/USD',
    feedSymbol: 'USD/CAD',
    feedId: 'storybook-usd-cad',
    basePrice: 0.7288,
    rawPrice: 1.3711,
    exponent: -8,
    inverted: true,
    weightBps: 910,
    phase: 2.7,
  },
  {
    symbol: 'SEK/USD',
    feedSymbol: 'USD/SEK',
    feedId: 'storybook-usd-sek',
    basePrice: 0.1086,
    rawPrice: 9.2081,
    exponent: -8,
    inverted: true,
    weightBps: 420,
    phase: 3.3,
  },
  {
    symbol: 'CHF/USD',
    feedSymbol: 'USD/CHF',
    feedId: 'storybook-usd-chf',
    basePrice: 1.261,
    rawPrice: 0.7929,
    exponent: -8,
    inverted: true,
    weightBps: 360,
    phase: 4,
  },
]

const INTERVAL_CONFIG: Record<
  DxyBasketChartInterval,
  { points: number; intervalSeconds: number; totalMove: number }
> = {
  '5m': { points: 24 * 12 + 1, intervalSeconds: 5 * 60, totalMove: 0.0065 },
  '1h': { points: 7 * 24 + 1, intervalSeconds: 60 * 60, totalMove: 0.019 },
  '1d': { points: 30 + 1, intervalSeconds: 24 * 60 * 60, totalMove: 0.034 },
}

function toOracleString(value: number): string {
  return String(Math.round(value * 100_000_000))
}

function componentAt(seed: ComponentSeed, timestamp: number, index: number, move: number): BasketComponentPrice {
  const localMove = move * (0.45 + seed.weightBps / 20_000) + Math.sin(index * 0.23 + seed.phase) * 0.002
  const normalizedPrice = seed.basePrice * (1 + localMove)
  const rawPrice = seed.rawPrice * (1 + (seed.inverted ? -localMove : localMove) * 0.75)

  return {
    symbol: seed.symbol,
    feedSymbol: seed.feedSymbol,
    feedId: seed.feedId,
    price: toOracleString(normalizedPrice),
    rawPrice: toOracleString(rawPrice),
    confidence: toOracleString(Math.max(normalizedPrice * 0.00035, 0.000001)),
    exponent: seed.exponent,
    publishTime: timestamp,
    inverted: seed.inverted,
    weightBps: seed.weightBps,
    basePrice: toOracleString(seed.basePrice),
  }
}

function makeHistory(interval: DxyBasketChartInterval, market: MarketShape): BasketHistory {
  const direction = market === 'rally' ? 1 : -1
  const config = INTERVAL_CONFIG[interval]
  const range = basketRangeForChartInterval(interval)
  const firstTimestamp = GENERATED_AT - (config.points - 1) * config.intervalSeconds
  const points = Array.from({ length: config.points }, (_, index) => {
    const progress = index / (config.points - 1)
    const wave =
      Math.sin(progress * Math.PI * 6) * 0.0035 +
      Math.sin(progress * Math.PI * 17 + 0.8) * 0.0015
    const move = direction * config.totalMove * progress + wave
    const timestamp = firstTimestamp + index * config.intervalSeconds

    return {
      timestamp,
      basketPrice: toOracleString(1 + move),
      components: COMPONENT_SEEDS.map((seed) => componentAt(seed, timestamp, index, move)),
    }
  })
  const firstPrice = Number(points[0].basketPrice)
  const latestPrice = Number(points.at(-1)?.basketPrice ?? firstPrice)

  return {
    range,
    intervalSeconds: config.intervalSeconds,
    source: 'pyth_benchmarks',
    generatedAt: GENERATED_AT,
    latestPrice: String(latestPrice),
    changePct: (latestPrice - firstPrice) / firstPrice,
    points,
  }
}

const HISTORIES: Record<MarketShape, Record<DxyBasketChartInterval, BasketHistory>> = {
  rally: {
    '5m': makeHistory('5m', 'rally'),
    '1h': makeHistory('1h', 'rally'),
    '1d': makeHistory('1d', 'rally'),
  },
  selloff: {
    '5m': makeHistory('5m', 'selloff'),
    '1h': makeHistory('1h', 'selloff'),
    '1d': makeHistory('1d', 'selloff'),
  },
}

function emptyHistory(interval: DxyBasketChartInterval): BasketHistory {
  return {
    ...HISTORIES.rally[interval],
    latestPrice: null,
    changePct: null,
    points: [],
  }
}

function DxyBasketStory({ initialInterval, market, chartStyle, state }: DxyBasketStoryProps) {
  const [chartInterval, setChartInterval] = useState<DxyBasketChartInterval>(initialInterval)
  const history = state === 'empty' ? emptyHistory(chartInterval) : HISTORIES[market][chartInterval]

  useEffect(() => {
    setChartInterval(initialInterval)
  }, [initialInterval])

  return (
    <div className="min-h-screen bg-cyber-bg p-4 md:p-8">
      <div className="mx-auto max-w-5xl">
        <DxyBasketPanelView
          history={state === 'loading' || state === 'error' ? undefined : history}
          chartInterval={chartInterval}
          chartStyle={chartStyle}
          isLoading={state === 'loading'}
          isError={state === 'error'}
          onChartIntervalChange={setChartInterval}
        />
      </div>
    </div>
  )
}

const meta: Meta<typeof DxyBasketStory> = {
  title: 'Perps/DXY Basket Panel',
  component: DxyBasketStory,
  tags: ['autodocs'],
  parameters: {
    layout: 'fullscreen',
  },
  args: {
    initialInterval: '1h',
    market: 'rally',
    chartStyle: 'area',
    state: 'ready',
  },
  argTypes: {
    initialInterval: {
      control: 'select',
      options: ['5m', '1h', '1d'],
    },
    market: {
      control: 'select',
      options: ['rally', 'selloff'],
    },
    chartStyle: {
      control: 'select',
      options: ['area', 'candlestick'],
    },
    state: {
      control: 'select',
      options: ['ready', 'loading', 'error', 'empty'],
    },
  },
}

export default meta
type Story = StoryObj<typeof meta>

export const MockedHistory: Story = {}

export const Selloff: Story = {
  args: {
    market: 'selloff',
  },
}

export const Candles: Story = {
  args: {
    initialInterval: '1d',
    chartStyle: 'candlestick',
  },
}

export const Loading: Story = {
  args: {
    state: 'loading',
  },
}

export const Empty: Story = {
  args: {
    state: 'empty',
  },
}
