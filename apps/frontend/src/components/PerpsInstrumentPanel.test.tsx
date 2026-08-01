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
              totalExposure: '882.9M USDC',
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
    const overlay = details?.parentElement
    const metric = trigger.parentElement?.parentElement
    const coveredStats = screen.getByText('Pool liquidity').closest('[aria-hidden]')

    expect(trigger).toHaveAttribute('aria-expanded', 'false')
    expect(details).toHaveAttribute('aria-hidden', 'true')
    expect(overlay).toHaveClass('grid-rows-[0fr]', 'opacity-0', 'shadow-none')
    expect(overlay).not.toHaveClass('hidden')
    expect(coveredStats).toBeNull()

    fireEvent.mouseEnter(metric!)
    expect(trigger).toHaveAttribute('aria-expanded', 'true')
    expect(details).toHaveAttribute('aria-hidden', 'false')
    expect(overlay).toHaveClass(
      'grid-rows-[1fr]',
      'opacity-100',
      'shadow-[0_20px_32px_-16px_rgba(0,0,0,0.8)]'
    )
    expect(screen.getByText('Pool liquidity')).toBeVisible()
    expect(screen.getByText('13% remaining')).toBeInTheDocument()
    expect(screen.getByText('Total LONG exposure')).toBeVisible()
    expect(screen.getByText('Net LONG exposure')).toBeVisible()
    expect(screen.getByText('882.9M USDC')).toBeVisible()

    fireEvent.mouseLeave(metric!)
    expect(trigger).toHaveAttribute('aria-expanded', 'false')
    expect(overlay).toHaveClass('grid-rows-[0fr]', 'opacity-0', 'shadow-none')
    expect(screen.getByText('Pool liquidity').closest('[aria-hidden]')).toBeNull()

    fireEvent.focus(trigger)
    expect(trigger).toHaveAttribute('aria-expanded', 'true')

    fireEvent.blur(trigger)
    expect(trigger).toHaveAttribute('aria-expanded', 'false')
  })

  it('labels only the total exposure for the current heavy side', () => {
    const { rerender } = render(
      <PerpsInstrumentPanel
        directionalLimitDetailsExpanded
        stats={[{
          label: 'Directional limit used',
          directionalLimit: {
            usagePercent: 62,
            side: 'short',
            totalExposure: '580.8M USDC',
            netExposure: '225.4M USDC',
            limit: '363.3M USDC',
          },
        }]}
      />
    )

    expect(screen.getByText('Total SHORT exposure')).toBeVisible()
    expect(screen.getByText('Net SHORT exposure')).toBeVisible()
    expect(screen.queryByText('Total LONG exposure')).not.toBeInTheDocument()

    rerender(
      <PerpsInstrumentPanel
        directionalLimitDetailsExpanded
        stats={[{
          label: 'Directional limit used',
          directionalLimit: {
            usagePercent: 0,
            side: 'balanced',
            totalExposure: '529.8M USDC',
            netExposure: '0 USDC',
            limit: '353.1M USDC',
          },
        }]}
      />
    )

    expect(screen.getByText('Exposure per side')).toBeVisible()
    expect(screen.getByText('Net exposure')).toBeVisible()
    expect(screen.queryByText('Total SHORT exposure')).not.toBeInTheDocument()
  })
})
