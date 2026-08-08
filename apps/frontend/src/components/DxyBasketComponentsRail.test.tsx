import { fireEvent, render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import type { BasketComponentPrice } from '../api'
import { DxyBasketComponentsRail } from './DxyBasketComponentsRail'

const components: BasketComponentPrice[] = [
  {
    symbol: 'EUR/USD',
    feedSymbol: 'EUR/USD',
    feedId: 'eur-usd',
    price: '115300000',
    rawPrice: '115300000',
    confidence: '1000',
    exponent: -8,
    publishTime: 950,
    inverted: false,
    weightBps: 5760,
    basePrice: '117500000',
  },
  {
    symbol: 'JPY/USD',
    feedSymbol: 'USD/JPY',
    feedId: 'usd-jpy',
    price: '600000',
    rawPrice: '15674000000',
    confidence: '1000',
    exponent: -8,
    publishTime: 300,
    inverted: true,
    weightBps: 1360,
    basePrice: '638000',
  },
]

describe('DxyBasketComponentsRail', () => {
  it('renders the basket values in one horizontally snapping rail', () => {
    render(
      <DxyBasketComponentsRail
        components={components}
        priceChanges={{
          'eur-usd': 0.0018,
          'usd-jpy': -0.0003,
        }}
        nowSeconds={1000}
      />
    )

    const rail = screen.getByRole('list', { name: 'Basket components' })
    expect(rail).toHaveClass('flex', 'overflow-x-auto', 'snap-x', 'snap-mandatory')
    expect(screen.getAllByRole('listitem')).toHaveLength(2)
    expect(screen.getByText('EUR/USD')).toBeInTheDocument()
    expect(screen.getByText('JPY/USD')).toBeInTheDocument()
    expect(screen.queryByText('USD/JPY')).not.toBeInTheDocument()
    expect(screen.getByText('57.6%')).toBeInTheDocument()
    expect(screen.getByText('1.153')).toBeInTheDocument()
    expect(screen.getByText('+0.18%')).toHaveAttribute('aria-label', '24 hour change +0.18%')
    expect(screen.queryByText('USD/JPY inv')).not.toBeInTheDocument()
    expect(screen.getByLabelText('Price fresh')).toBeInTheDocument()
    expect(screen.getByLabelText('Price stale')).toBeInTheDocument()

    for (const card of screen.getAllByRole('listitem')) {
      expect(card).toHaveClass('snap-start')
    }
  })

  it('keeps component freshness details available to pointer and keyboard users', () => {
    render(<DxyBasketComponentsRail components={components.slice(0, 1)} nowSeconds={1000} />)

    fireEvent.focus(screen.getByLabelText('Price fresh'))

    expect(screen.getByRole('tooltip')).toHaveTextContent('updated 50s ago')
  })

  it('renders distinct loading, empty, and error states', () => {
    const { rerender } = render(<DxyBasketComponentsRail isLoading />)

    const loading = screen.getByRole('status', { name: 'Basket components' })
    expect(loading).toHaveAttribute('aria-busy', 'true')
    expect(loading).toHaveClass('overflow-x-auto', 'snap-x')
    expect(screen.getByText('Loading basket components')).toBeInTheDocument()

    rerender(<DxyBasketComponentsRail />)
    expect(screen.getByRole('status')).toHaveTextContent('No basket components available.')

    rerender(<DxyBasketComponentsRail isError />)
    expect(screen.getByRole('status')).toHaveTextContent('Basket components unavailable.')
  })
})
