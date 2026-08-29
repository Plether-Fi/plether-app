import { render, screen } from '@testing-library/react'
import { describe, expect, it, vi } from 'vitest'
import { DxyBasketPanel } from '../DxyBasketPanel'

vi.mock('../../tradingview/TradingViewAdvancedChart', () => ({
  TradingViewAdvancedChart: ({ liquidationPrice }: { liquidationPrice?: number }) => (
    <div data-testid="advanced-chart-liquidation-price">{liquidationPrice}</div>
  ),
}))

describe('DxyBasketPanel', () => {
  it('maps the account liquidation threshold to the displayed plDXY chart price', () => {
    render(<DxyBasketPanel liquidationPriceRaw={98_310_000n} />)

    expect(screen.getByTestId('advanced-chart-liquidation-price')).toHaveTextContent('1.0169')
  })

  it('omits the line when no in-range liquidation threshold exists', () => {
    render(<DxyBasketPanel />)

    expect(screen.getByTestId('advanced-chart-liquidation-price')).toBeEmptyDOMElement()
  })
})
