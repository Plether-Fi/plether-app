import { render, screen } from '@testing-library/react'
import { describe, expect, it, vi } from 'vitest'
import { DxyBasketPanelView } from '../DxyBasketPanel'
import type { BasketLatest } from '../../api'

vi.mock('../../tradingview/TradingViewAdvancedChart', () => ({
  TradingViewAdvancedChart: ({ liquidationPrice }: { liquidationPrice?: number }) => (
    <div data-testid="advanced-chart-liquidation-price">{liquidationPrice}</div>
  ),
}))

const latest: BasketLatest = {
  timestamp: 100,
  basketPrice: '97600000',
  components: [],
  generatedAt: 101,
  source: 'database',
}

describe('DxyBasketPanelView', () => {
  it('does not duplicate the price summary above the chart', () => {
    render(
      <DxyBasketPanelView
        latest={latest}
        chartInterval="5m"
        useAdvancedChart={false}
        isLoading
        onChartIntervalChange={() => {}}
      />
    )

    expect(screen.queryByText('plDXY Perp Price')).not.toBeInTheDocument()
    expect(screen.queryByText('1.0240')).not.toBeInTheDocument()
    expect(screen.getByRole('button', { name: '5 minute interval' })).toBeInTheDocument()
  })

  it('maps the account liquidation threshold to the displayed plDXY chart price', () => {
    render(
      <DxyBasketPanelView
        latest={latest}
        liquidationPriceRaw={98_310_000n}
        chartInterval="5m"
        onChartIntervalChange={() => {}}
      />
    )

    expect(screen.getByTestId('advanced-chart-liquidation-price')).toHaveTextContent('1.0169')
  })
})
