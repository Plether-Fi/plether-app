import { fireEvent, render, screen } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { Button } from './Button'

const analyticsMock = vi.hoisted(() => ({
  trackPerpsButtonClicked: vi.fn(),
}))

vi.mock('../../analytics/perps', () => ({
  trackPerpsButtonClicked: analyticsMock.trackPerpsButtonClicked,
}))

describe('Button analytics', () => {
  beforeEach(() => {
    vi.clearAllMocks()
  })

  it('captures only when an analytics id is provided and preserves onClick', () => {
    const onClick = vi.fn()

    render(
      <>
        <Button analyticsId="review_trade" analyticsProperties={{ direction: 'long' }} onClick={onClick}>
          Review
        </Button>
        <Button>Plain</Button>
      </>
    )

    fireEvent.click(screen.getByRole('button', { name: 'Review' }))
    fireEvent.click(screen.getByRole('button', { name: 'Plain' }))

    expect(onClick).toHaveBeenCalledTimes(1)
    expect(analyticsMock.trackPerpsButtonClicked).toHaveBeenCalledTimes(1)
    expect(analyticsMock.trackPerpsButtonClicked).toHaveBeenCalledWith('review_trade', {
      surface: 'perps',
      direction: 'long',
    })
  })

  it('does not capture disabled or loading clicks', () => {
    render(
      <>
        <Button analyticsId="disabled" disabled>Disabled</Button>
        <Button analyticsId="loading" isLoading>Loading</Button>
      </>
    )

    fireEvent.click(screen.getByRole('button', { name: 'Disabled' }))
    fireEvent.click(screen.getByRole('button', { name: /Loading/ }))

    expect(analyticsMock.trackPerpsButtonClicked).not.toHaveBeenCalled()
  })
})
