import { render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { Sparkline } from './Sparkline'

describe('Sparkline', () => {
  it('renders an explicit unavailable state when fewer than two finite points exist', () => {
    render(<Sparkline values={[1]} label="Share price" />)
    expect(screen.getByRole('img', { name: 'Share price: chart unavailable' })).toHaveTextContent('Unavailable')
  })

  it('renders an SVG polyline for a valid series', () => {
    const { container } = render(<Sparkline values={[1.0, 1.05, 1.02, 1.1]} label="Share price" tone="positive" />)
    expect(screen.getByRole('img', { name: 'Share price' })).toBeInTheDocument()
    const polyline = container.querySelector('polyline')
    expect(polyline).not.toBeNull()
    expect(polyline?.getAttribute('points')).toContain(',')
  })
})
