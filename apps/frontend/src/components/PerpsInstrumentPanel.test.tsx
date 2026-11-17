import { act, fireEvent, render, screen, within } from '@testing-library/react'
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
              totalExposure: '882.9M USDG',
              netExposure: '307.2M USDG',
              limit: '353.1M USDG',
            },
          },
          {
            label: 'Free pool liquidity',
            value: '6.3M USDG',
            hoverDetailsType: 'pool-liquidity',
            hoverDetailsLabel: 'Free pool liquidity details',
            hoverDetails: <div>Capital waterfall</div>,
          },
        ]}
      />
    )

    const priceTrigger = screen.getByRole('button', { name: 'plDXY Perp price basket components' })
    const directionalTrigger = screen.getByRole('button', { name: 'Directional limit used details' })
    const directionalMetric = directionalTrigger.parentElement?.parentElement
    const poolTrigger = screen.getByRole('button', { name: 'Free pool liquidity details' })

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
    vi.useFakeTimers()

    render(
      <PerpsInstrumentPanel
        stats={[
          {
            label: 'Directional limit used',
            directionalLimit: {
              usagePercent: 87,
              side: 'long',
              totalExposure: '882.9M USDG',
              netExposure: '307.2M USDG',
              limit: '353.1M USDG',
            },
          },
          { label: 'Free pool liquidity', value: '6.3M USDG' },
          { label: 'Cost of carry', value: '5.24%' },
        ]}
      />
    )

    const trigger = screen.getByRole('button', { name: 'Directional limit used details' })
    const details = screen.getByText('Directional limit').closest('[aria-hidden]')
    const overlay = details?.parentElement
    const metric = trigger.parentElement?.parentElement
    const coveredStats = screen.getByText('Free pool liquidity').closest('[aria-hidden]')

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
    expect(screen.getByText('Free pool liquidity')).toBeVisible()
    expect(screen.getByText('13% remaining')).toBeInTheDocument()
    expect(screen.getByRole('img', { name: '87% used; 13% remaining' })).toBeVisible()
    expect(screen.getByText('Total LONG exposure')).toBeVisible()
    expect(screen.getByText('Net LONG exposure')).toBeVisible()
    expect(screen.getByText('882.9M USDG')).toBeVisible()
    expect(screen.getByText(
      'Position imbalance affects VPI. Payout backing also limits new positions.'
    )).toBeVisible()
    const learnMoreLink = screen.getByRole('link', { name: 'Read: Virtual Price Impact' })
    expect(learnMoreLink).toHaveTextContent('Learn more')
    expect(learnMoreLink).toHaveAttribute(
      'href',
      'https://docs.plether.com/how-plether-works/trading-costs-fees-carry-and-vpi#virtual-price-impact'
    )

    fireEvent.mouseLeave(metric!)
    expect(trigger).toHaveAttribute('aria-expanded', 'true')
    fireEvent.mouseEnter(overlay!)
    act(() => { vi.advanceTimersByTime(300) })
    expect(trigger).toHaveAttribute('aria-expanded', 'true')

    fireEvent.mouseLeave(overlay!)
    act(() => { vi.advanceTimersByTime(300) })
    expect(trigger).toHaveAttribute('aria-expanded', 'false')
    expect(overlay).toHaveClass('grid-rows-[0fr]', 'opacity-0', 'shadow-none')
    expect(screen.getByText('Free pool liquidity').closest('[aria-hidden]')).toBeNull()

    fireEvent.focus(trigger)
    expect(trigger).toHaveAttribute('aria-expanded', 'true')

    fireEvent.blur(trigger, { relatedTarget: learnMoreLink })
    fireEvent.focus(learnMoreLink)
    act(() => { vi.advanceTimersByTime(300) })
    expect(trigger).toHaveAttribute('aria-expanded', 'true')

    fireEvent.blur(learnMoreLink)
    act(() => { vi.advanceTimersByTime(300) })
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
            totalExposure: '580.8M USDG',
            netExposure: '225.4M USDG',
            limit: '363.3M USDG',
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
            totalExposure: '529.8M USDG',
            netExposure: '0 USDG',
            limit: '353.1M USDG',
          },
        }]}
      />
    )

    expect(screen.getByText('Exposure per side')).toBeVisible()
    expect(screen.getByText('Net exposure')).toBeVisible()
    expect(screen.queryByText('Total SHORT exposure')).not.toBeInTheDocument()
  })
})

describe('PerpsInstrumentPanel pool capacity', () => {
  it('shows capacity only inside the directional-limit dropdown', () => {
    vi.useFakeTimers()
    render(
      <PerpsInstrumentPanel
        stats={[{
          label: 'Directional limit used',
          directionalLimit: { usagePercent: 16, side: 'long' },
        }]}
        poolCapacity={{ long: '3.802M', short: '86K' }}
      />
    )

    expect(screen.getByText('LONG-HEAVY')).toBeVisible()
    expect(screen.queryByRole('group', { name: 'Estimated pool capacity' })).not.toBeInTheDocument()
    const trigger = screen.getByRole('button', { name: 'Directional limit used details' })
    fireEvent.focus(trigger)

    const capacity = screen.getByRole('group', { name: 'Estimated pool capacity' })
    expect(capacity.closest('[aria-hidden]')).toHaveAttribute('id', trigger.getAttribute('aria-controls'))
    expect(within(capacity).getByText('Long capacity')).toBeVisible()
    expect(within(capacity).getByText('3.802M')).toBeVisible()
    expect(within(capacity).getByText('Short capacity')).toBeVisible()
    expect(within(capacity).getByText('86K')).toBeVisible()
    expect(within(capacity).getAllByText('plDXY', { exact: true })).toHaveLength(2)

    expect(capacity.closest('[aria-hidden]')).toHaveTextContent('before VPI and fees. Your Max may be lower.')

    fireEvent.blur(trigger)
    act(() => { vi.advanceTimersByTime(300) })
    expect(screen.queryByRole('group', { name: 'Estimated pool capacity' })).not.toBeInTheDocument()
  })

  it('distinguishes zero capacity from missing and loading data', () => {
    const { rerender } = render(<PerpsInstrumentPanel directionalLimitDetailsExpanded poolCapacity={{ long: '0' }} />)

    const capacity = screen.getByRole('group', { name: 'Estimated pool capacity' })
    expect(within(capacity).getByText('0', { exact: true })).toBeVisible()
    expect(within(capacity).getByLabelText('Short capacity unavailable')).toHaveTextContent('--')
    expect(within(capacity).queryByLabelText('Long capacity unavailable')).not.toBeInTheDocument()

    rerender(<PerpsInstrumentPanel directionalLimitDetailsExpanded poolCapacity={{ isLoading: true }} />)
    expect(screen.getByLabelText('Loading long capacity')).toHaveTextContent('...')
    expect(screen.getByLabelText('Loading short capacity')).toHaveTextContent('...')

    rerender(<PerpsInstrumentPanel />)
    expect(screen.queryByRole('group', { name: 'Estimated pool capacity' })).not.toBeInTheDocument()
  })
})
