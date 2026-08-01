import { render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { DxyBasketPanelView } from '../DxyBasketPanel'
import type { BasketComponentPrice, BasketLatest } from '../../api'

const component: BasketComponentPrice = {
  symbol: 'EUR/USD',
  feedSymbol: 'EUR/USD',
  feedId: 'eur-usd',
  price: '115300000',
  rawPrice: '115300000',
  confidence: '1000',
  exponent: -8,
  publishTime: 100,
  inverted: false,
  weightBps: 5760,
  basePrice: '117500000',
}

const latest: BasketLatest = {
  timestamp: 100,
  basketPrice: '97600000',
  components: [component],
  generatedAt: 101,
  source: 'database',
}

describe('DxyBasketPanelView', () => {
  it('keeps the header price visible while interval history is loading', () => {
    render(
      <DxyBasketPanelView
        latest={latest}
        chartInterval="5m"
        isLoading
        onChartIntervalChange={() => {}}
      />
    )

    expect(screen.getByText('1.0240')).toBeInTheDocument()
    expect(screen.queryByText('EUR/USD')).not.toBeInTheDocument()
  })
})
