import { act, fireEvent, render, screen } from '@testing-library/react'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { PerpsInstrumentPanel } from './PerpsInstrumentPanel'

afterEach(() => {
  vi.useRealTimers()
})

describe('PerpsInstrumentPanel price details', () => {
  it('reveals an interactive full-width rail without putting collapsed content in the tab order', () => {
    vi.useFakeTimers()

    render(
      <PerpsInstrumentPanel
        stats={[
          {
            label: 'plDXY Perp price',
            value: '1.0153',
            hoverDetails: (
              <button type="button">EUR/USD basket component</button>
            ),
          },
          { label: '24h change', value: '-0.17%' },
        ]}
      />
    )

    const trigger = screen.getByRole('button', { name: 'plDXY Perp price basket components' })
    const railAction = screen.getByText('EUR/USD basket component').closest('button')
    const details = railAction?.closest('[aria-hidden]')
    const overlay = details?.parentElement

    expect(trigger).toHaveAttribute('aria-expanded', 'false')
    expect(details).toHaveAttribute('aria-hidden', 'true')
    expect(details).toHaveAttribute('inert')
    expect(overlay).toHaveClass('grid-rows-[0fr]', 'opacity-0', 'pointer-events-none', 'shadow-none')

    fireEvent.mouseEnter(trigger)

    expect(trigger).toHaveAttribute('aria-expanded', 'true')
    expect(details).toHaveAttribute('aria-hidden', 'false')
    expect(details).not.toHaveAttribute('inert')
    expect(overlay).toHaveClass(
      'grid-rows-[1fr]',
      'opacity-100',
      'pointer-events-auto',
      'shadow-[0_20px_32px_-16px_rgba(0,0,0,0.8)]'
    )

    fireEvent.mouseLeave(trigger)
    fireEvent.mouseEnter(overlay!)
    act(() => { vi.advanceTimersByTime(300) })
    expect(trigger).toHaveAttribute('aria-expanded', 'true')

    fireEvent.mouseLeave(overlay!)
    act(() => { vi.advanceTimersByTime(300) })
    expect(trigger).toHaveAttribute('aria-expanded', 'false')

    fireEvent.focus(trigger)
    fireEvent.blur(trigger)
    fireEvent.focus(railAction!)
    act(() => { vi.advanceTimersByTime(300) })
    expect(trigger).toHaveAttribute('aria-expanded', 'true')

    fireEvent.blur(railAction!)
    act(() => { vi.advanceTimersByTime(300) })
    expect(trigger).toHaveAttribute('aria-expanded', 'false')
  })

  it('keeps the price, directional-limit, and pool-liquidity overlays mutually exclusive', () => {
    render(
      <PerpsInstrumentPanel
        stats={[
          {
            label: 'plDXY Perp price',
            value: '1.0153',
            hoverDetails: <div>EUR/USD</div>,
          },
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
          {
            label: 'Pool liquidity',
            value: '6.3M USDC',
            hoverDetailsType: 'pool-liquidity',
            hoverDetailsLabel: 'Pool liquidity details',
            hoverDetails: <div>Capital waterfall</div>,
          },
        ]}
      />
    )

    const priceTrigger = screen.getByRole('button', { name: 'plDXY Perp price basket components' })
    const directionalTrigger = screen.getByRole('button', { name: 'Directional limit used details' })
    const directionalMetric = directionalTrigger.parentElement?.parentElement
    const poolTrigger = screen.getByRole('button', { name: 'Pool liquidity details' })

    fireEvent.mouseEnter(priceTrigger)
    expect(priceTrigger).toHaveAttribute('aria-expanded', 'true')
    expect(directionalTrigger).toHaveAttribute('aria-expanded', 'false')
    expect(poolTrigger).toHaveAttribute('aria-expanded', 'false')

    fireEvent.mouseEnter(directionalMetric!)
    expect(priceTrigger).toHaveAttribute('aria-expanded', 'false')
    expect(directionalTrigger).toHaveAttribute('aria-expanded', 'true')
    expect(poolTrigger).toHaveAttribute('aria-expanded', 'false')

    fireEvent.mouseEnter(poolTrigger)
    expect(priceTrigger).toHaveAttribute('aria-expanded', 'false')
    expect(directionalTrigger).toHaveAttribute('aria-expanded', 'false')
    expect(poolTrigger).toHaveAttribute('aria-expanded', 'true')

    fireEvent.mouseEnter(priceTrigger)
    expect(priceTrigger).toHaveAttribute('aria-expanded', 'true')
    expect(directionalTrigger).toHaveAttribute('aria-expanded', 'false')
    expect(poolTrigger).toHaveAttribute('aria-expanded', 'false')
  })
})

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
    expect(screen.getByRole('img', { name: '87% used; 13% remaining' })).toBeVisible()
    expect(screen.getByText('Total LONG exposure')).toBeVisible()
    expect(screen.getByText('Net LONG exposure')).toBeVisible()
    expect(screen.getByText('882.9M USDC')).toBeVisible()
    expect(screen.getByText(
      'Market-wide LONG/SHORT imbalance, not an order quote. It affects VPI and trading costs, can change before execution, and other checks apply.'
    )).toBeVisible()
    expect(screen.getByRole('link', { name: 'Read: Virtual Price Impact' })).toHaveAttribute(
      'href',
      'https://docs.plether.com/how-plether-works/trading-costs-fees-carry-and-vpi#virtual-price-impact'
    )

    fireEvent.mouseLeave(metric!)
    expect(trigger).toHaveAttribute('aria-expanded', 'false')
    expect(overlay).toHaveClass('grid-rows-[0fr]', 'opacity-0', 'shadow-none')
    expect(screen.getByText('Pool liquidity').closest('[aria-hidden]')).toBeNull()

    fireEvent.focus(trigger)
    expect(trigger).toHaveAttribute('aria-expanded', 'true')

    const readMoreLink = screen.getByRole('link', { name: 'Read: Virtual Price Impact' })
    fireEvent.blur(trigger, { relatedTarget: readMoreLink })
    fireEvent.focus(readMoreLink)
    expect(trigger).toHaveAttribute('aria-expanded', 'true')

    fireEvent.blur(readMoreLink)
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
