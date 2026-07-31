import { fireEvent, render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { PerpsInstrumentPanel } from './PerpsInstrumentPanel'

describe('PerpsInstrumentPanel directional limit', () => {
  it('reveals the integrated limit details on hover and keyboard focus', () => {
    render(
      <PerpsInstrumentPanel
        stats={[
          {
            label: 'Directional limit used',
            directionalLimit: {
              usagePercent: 87,
              side: 'long',
              netExposure: '307.2M USDC',
              limit: '353.1M USDC',
            },
          },
          { label: 'Pool liquidity', value: '6.3M USDC' },
          { label: 'Cost of carry', value: '5.24%' },
        ]}
      />
    )

    const trigger = screen.getByRole('button', { name: 'Directional limit used details' })
    const details = screen.getByText('Directional limit').closest('[aria-hidden]')
    const metric = trigger.parentElement?.parentElement
    const coveredStats = screen.getByText('Pool liquidity').closest('[aria-hidden]')

    expect(trigger).toHaveAttribute('aria-expanded', 'false')
    expect(details).toHaveAttribute('aria-hidden', 'true')
    expect(coveredStats).toBeNull()

    fireEvent.mouseEnter(metric!)
    expect(trigger).toHaveAttribute('aria-expanded', 'true')
    expect(details).toHaveAttribute('aria-hidden', 'false')
    expect(screen.getByText('Pool liquidity').closest('[aria-hidden]')).toHaveAttribute('aria-hidden', 'true')
    expect(screen.getByText('13% remaining')).toBeInTheDocument()

    fireEvent.mouseLeave(metric!)
    expect(trigger).toHaveAttribute('aria-expanded', 'false')
    expect(screen.getByText('Pool liquidity').closest('[aria-hidden]')).toBeNull()

    fireEvent.focus(trigger)
    expect(trigger).toHaveAttribute('aria-expanded', 'true')

    fireEvent.blur(trigger)
    expect(trigger).toHaveAttribute('aria-expanded', 'false')
  })
})
