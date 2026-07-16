import { act, fireEvent, render, screen } from '@testing-library/react'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { Tooltip } from './Tooltip'

const DOCS_LINK = {
  href: 'https://docs.plether.com/example',
  title: 'Helpful context',
}
const DOCS_LINK_TEXT = `Read: "${DOCS_LINK.title}"`

function renderTooltip() {
  render(
    <Tooltip content="Helpful context" docsLink={DOCS_LINK}>
      <button type="button">Details</button>
    </Tooltip>
  )

  return screen.getByRole('button', { name: 'Details' })
}

describe('Tooltip', () => {
  afterEach(() => {
    vi.useRealTimers()
  })

  it('renders an external Read link using the referenced docs title', () => {
    const trigger = renderTooltip()

    fireEvent.mouseEnter(trigger)

    expect(screen.getByRole('link', { name: DOCS_LINK_TEXT })).toHaveAttribute('href', DOCS_LINK.href)
    expect(screen.getByRole('link', { name: DOCS_LINK_TEXT })).toHaveAttribute('target', '_blank')
    expect(screen.getByRole('link', { name: DOCS_LINK_TEXT })).toHaveAttribute('rel', 'noopener noreferrer')
    expect(screen.queryByRole('link', { name: 'Learn more...' })).not.toBeInTheDocument()
    expect(screen.getByText(`"${DOCS_LINK.title}"`)).toHaveClass('italic')
  })

  it('stays open while the pointer moves from the trigger into the tooltip', () => {
    vi.useFakeTimers()
    const trigger = renderTooltip()

    fireEvent.mouseEnter(trigger)
    const tooltip = screen.getByRole('tooltip')

    fireEvent.mouseLeave(trigger.parentElement!)
    fireEvent.mouseEnter(tooltip)
    act(() => {
      vi.advanceTimersByTime(300)
    })

    expect(screen.getByRole('tooltip')).toBeInTheDocument()

    fireEvent.mouseLeave(tooltip)
    act(() => {
      vi.advanceTimersByTime(300)
    })

    expect(screen.queryByRole('tooltip')).not.toBeInTheDocument()
  })

  it('keeps the tooltip open when focus moves to its portaled link', () => {
    vi.useFakeTimers()
    const trigger = renderTooltip()

    fireEvent.focus(trigger)
    const docsLink = screen.getByRole('link', { name: DOCS_LINK_TEXT })

    fireEvent.blur(trigger, { relatedTarget: docsLink })
    fireEvent.focus(docsLink, { relatedTarget: trigger })
    act(() => {
      vi.advanceTimersByTime(300)
    })

    expect(screen.getByRole('tooltip')).toBeInTheDocument()
  })
})
