import { render, screen } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { describe, expect, it } from 'vitest'
import { OraclePriceLink } from './OraclePriceLink'

describe('OraclePriceLink', () => {
  it('links to a specific oracle publication when updateId is provided', () => {
    render(
      <MemoryRouter>
        <OraclePriceLink updateId="48219">1.05256 plDXY</OraclePriceLink>
      </MemoryRouter>,
    )

    const link = screen.getByRole('link', { name: '1.05256 plDXY' })
    expect(link).toHaveAttribute('href', '/oracle/updates/48219')
    expect(link).toHaveAttribute('title', 'Inspect oracle publication #48219')
  })

  it('defaults to /oracle/updates/latest when updateId is omitted or invalid', () => {
    render(
      <MemoryRouter>
        <OraclePriceLink>1.05256</OraclePriceLink>
        <OraclePriceLink updateId="not-an-id">1.04999</OraclePriceLink>
      </MemoryRouter>,
    )

    expect(screen.getByRole('link', { name: '1.05256' })).toHaveAttribute(
      'href',
      '/oracle/updates/latest',
    )
    expect(screen.getByRole('link', { name: '1.04999' })).toHaveAttribute(
      'href',
      '/oracle/updates/latest',
    )
  })

  it('renders a plain anchor when mounted outside a Router context', () => {
    render(<OraclePriceLink updateId="77">1.04000</OraclePriceLink>)

    const link = screen.getByRole('link', { name: '1.04000' })
    expect(link).toHaveAttribute('href', '/oracle/updates/77')
  })
})
