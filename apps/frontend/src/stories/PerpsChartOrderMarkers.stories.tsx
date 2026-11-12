import { useEffect, useState } from 'react'
import { Result } from 'better-result'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { perpsApi } from '../api'
import { DxyBasketPanel } from '../components/DxyBasketPanel'
import type { PerpsOrderHistoryRow } from '../hooks/usePerpsHistory'

const INTERVAL_SECONDS = 900
const PAGE_SECONDS = INTERVAL_SECONDS * 500
const SAMPLE_ACCOUNT = '0x1111111111111111111111111111111111111111'

function OrderMarkerPreview() {
  const [orders, setOrders] = useState<PerpsOrderHistoryRow[]>([])
  const [message, setMessage] = useState('Loading sample markers from recent candles…')

  useEffect(() => {
    let cancelled = false
    const cursor = Math.ceil(Date.now() / 1000 / PAGE_SECONDS) * PAGE_SECONDS
    void perpsApi.getPerpsBasketCandles(INTERVAL_SECONDS, cursor).then((result) => {
      if (cancelled) return
      if (Result.isError(result)) {
        setMessage('Sample markers could not load from the candle API.')
        return
      }
      const candles = result.value.data.candles.filter((candle) => candle.priceComplete)
      const samples = [candles.at(-50), candles.at(-19)]
      if (samples.some((sample) => sample === undefined)) {
        setMessage('There are not enough recent candles for this preview.')
        return
      }
      const sampleOrders = samples.map((candle, index): PerpsOrderHistoryRow => ({
        orderId: BigInt(index + 1),
        time: new Date(candle!.timestamp * 1000).toLocaleString(),
        terminalTimestampSeconds: candle!.timestamp + 30,
        market: 'plDXY Perp',
        side: 'Long',
        type: index === 0 ? 'Open' : 'Close',
        price: '--',
        size: '--',
        status: 'Executed',
        account: SAMPLE_ACCOUNT,
        clientOrderId: `0x${String(index + 1).padStart(64, '0')}`,
        executionPriceRaw: BigInt(candle!.rawClosePrice),
      }))
      setOrders([
        ...sampleOrders,
        { ...sampleOrders[0], orderId: 3n, terminalTimestampSeconds: samples[0]!.timestamp + 60 },
        { ...sampleOrders[1], orderId: 4n, terminalTimestampSeconds: samples[1]!.timestamp + 60 },
      ])
      setMessage('Illustrative: two buys and two sells on real candles. These are sample orders, not account trades.')
    }).catch(() => {
      if (!cancelled) setMessage('Sample markers could not load from the candle API.')
    })
    return () => { cancelled = true }
  }, [])

  return (
    <div className="min-h-screen bg-app-bg p-4 sm:p-8">
      <div className="mx-auto max-w-6xl">
        <h1 className="mb-2 text-2xl font-semibold text-content-primary">plDXY trade markers</h1>
        <p className="mb-4 text-sm text-content-secondary">{message}</p>
        <DxyBasketPanel orderHistory={orders} />
      </div>
    </div>
  )
}

const meta = {
  title: 'Perps/Chart Order Markers',
  component: OrderMarkerPreview,
  parameters: { layout: 'fullscreen' },
} satisfies Meta<typeof OrderMarkerPreview>

export default meta
type Story = StoryObj<typeof meta>

export const SampleBuyAndSell: Story = {}
