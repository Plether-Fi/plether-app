import { act, cleanup, render, screen } from '@testing-library/react'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { PerpsMarketStatePanel } from '../PerpsMarketStatePanel'

afterEach(() => {
  cleanup()
  vi.useRealTimers()
})

describe('PerpsMarketStatePanel', () => {
  it('updates the countdown and waits for the reported phase at the Friday boundary', () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2026-07-17T20:29:00Z'))
    const { container, rerender } = render(<PerpsMarketStatePanel currentPhase="open" />)
    expect(container).toHaveTextContent('Market is open for another 1m. Then close-only for 2d 0h 45m.')

    act(() => { vi.advanceTimersByTime(30_000) })
    expect(screen.getByText('<1m')).toBeInTheDocument()

    act(() => { vi.advanceTimersByTime(30_000) })
    expect(container).toHaveTextContent('Market is open.')
    expect(container).not.toHaveTextContent('for another')
    expect(container).not.toHaveTextContent('Then')

    rerender(<PerpsMarketStatePanel currentPhase="close-only" />)
    expect(container).toHaveTextContent('Market is close-only for another 2d 0h 45m. Then open for 4d 23h 15m.')
  })

  it.each(['closed', 'degraded', 'paused', 'close-only'] as const)(
    'does not promise a scheduled recovery for an unscheduled %s state',
    (phase) => {
      const { container } = render(
        <PerpsMarketStatePanel currentPhase={phase} now={new Date('2026-07-20T12:00:00Z')} />
      )
      expect(container).toHaveTextContent(`Market is ${phase}.`)
      expect(container).not.toHaveTextContent('for another')
      expect(container).not.toHaveTextContent('Then')
    }
  )
})
