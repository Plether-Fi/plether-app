import { fireEvent, render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import type { OracleBasketComponent } from '../api'
import { OracleBasketPieChart } from './OracleBasketPieChart'

const SAMPLE_COMPONENTS: OracleBasketComponent[] = [
  {
    symbol: 'EUR/USD',
    feedSymbol: 'EUR/USD',
    feedId: '0xa995d00bb36a63cef7fd2c287dc105fc8f3d93779f062f09551b0af3e81ec30b',
    price: '108620000',
    rawPrice: '108620000',
    confidence: '42000',
    confidenceBps: 4,
    confidenceExceeded: false,
    exponent: -8,
    publishTime: '2026-11-27T21:14:02Z',
    driftFromMinSeconds: 0,
    weightBps: 5760,
    basePrice: '117500000',
    contribution: '53247080',
    effectiveShareBps: 5629,
    changeFromBaseBps: -756,
    inverted: false,
  },
  {
    symbol: 'JPY/USD',
    feedSymbol: 'USD/JPY',
    feedId: '0xef2c98c804ba503c6a707e38be4dfbb16683775f195b091252bf24693042fd52',
    price: '653594',
    rawPrice: '15300000000',
    confidence: '3800000',
    confidenceBps: 2,
    confidenceExceeded: false,
    exponent: -8,
    publishTime: '2026-11-27T21:14:03Z',
    driftFromMinSeconds: 1,
    weightBps: 1360,
    basePrice: '638000',
    contribution: '13932411',
    effectiveShareBps: 1473,
    changeFromBaseBps: 244,
    inverted: true,
  },
]

describe('OracleBasketPieChart', () => {
  it('renders effective value share by default and toggles to target weights', () => {
    render(
      <OracleBasketPieChart
        components={SAMPLE_COMPONENTS}
        basketPrice="94589000"
        reconstructedBasketPrice="94589000"
      />,
    )

    expect(screen.getByText('DXY Currency Basket Composition')).toBeInTheDocument()
    expect(screen.getByText('1.05411')).toBeInTheDocument()
    expect(screen.getByText('56.29%')).toBeInTheDocument()
    expect(screen.getByText('14.73%')).toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: 'Target Weights' }))

    expect(screen.getByText('57.60%')).toBeInTheDocument()
    expect(screen.getByText('13.60%')).toBeInTheDocument()
  })

  it('pins and displays a currency readout in the center when a slice is clicked', () => {
    render(
      <OracleBasketPieChart
        components={SAMPLE_COMPONENTS}
        basketPrice="94589000"
      />,
    )

    const jpySlice = screen.getByRole('button', { name: 'JPY/USD 14.73%' })
    fireEvent.click(jpySlice)

    expect(jpySlice).toHaveAttribute('aria-pressed', 'true')
    expect(screen.getByText('0.13932 raw')).toBeInTheDocument()
  })
})
