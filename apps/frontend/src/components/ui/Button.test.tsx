import { fireEvent, render, screen } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { Button } from './Button'
import { replaceTextNodes } from '../../test/replaceTextNodes'

const analyticsMock = vi.hoisted(() => ({
  trackPerpsButtonClicked: vi.fn(),
}))

vi.mock('../../analytics/perps', () => ({
  trackPerpsButtonClicked: analyticsMock.trackPerpsButtonClicked,
}))

describe('Button analytics', () => {
  it('can add/remove the spinner and update a translated label without losing the button', () => {
    const view = render(<Button>Review</Button>)
    replaceTextNodes(screen.getByRole('button'))
    expect(() => view.rerender(<Button isLoading>Preparing</Button>)).not.toThrow()
    expect(screen.getByRole('button', { name: 'Preparing' })).toBeDisabled()
    replaceTextNodes(screen.getByRole('button'))
    expect(() => view.rerender(<Button>Review</Button>)).not.toThrow()
    expect(screen.getByRole('button', { name: 'Review' })).toBeEnabled()
  })
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
