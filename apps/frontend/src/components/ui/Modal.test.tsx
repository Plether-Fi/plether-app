import { fireEvent, render, screen, waitFor } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { Modal } from './Modal'

const analyticsMock = vi.hoisted(() => ({
  trackPerpsModalOpened: vi.fn(),
  trackPerpsModalClosed: vi.fn(),
}))

vi.mock('../../analytics/perps', () => ({
  trackPerpsModalOpened: analyticsMock.trackPerpsModalOpened,
  trackPerpsModalClosed: analyticsMock.trackPerpsModalClosed,
}))

describe('Modal analytics', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    vi.restoreAllMocks()
  })

  it('captures open and close dwell time from the close button', async () => {
    const onClose = vi.fn()

    const { rerender } = render(
      <Modal isOpen onClose={onClose} title="Tracked" analyticsId="tracked_modal">
        Body
      </Modal>
    )

    expect(analyticsMock.trackPerpsModalOpened).toHaveBeenCalledWith('tracked_modal', {
      surface: 'perps',
    })

    fireEvent.click(screen.getByRole('button'))
    expect(onClose).toHaveBeenCalledWith('close_button')

    rerender(
      <Modal isOpen={false} onClose={onClose} title="Tracked" analyticsId="tracked_modal">
        Body
      </Modal>
    )

    await waitFor(() => {
      expect(analyticsMock.trackPerpsModalClosed).toHaveBeenCalledWith('tracked_modal', {
        surface: 'perps',
        close_reason: 'close_button',
        duration_ms: expect.any(Number),
      })
    })
    expect(analyticsMock.trackPerpsModalClosed.mock.calls[0][1].duration_ms).toBeGreaterThanOrEqual(0)
  })

  it('captures Escape, backdrop, and unmount close paths', () => {
    const onClose = vi.fn()
    const { rerender, unmount } = render(
      <Modal isOpen onClose={onClose} title="Tracked" analyticsId="tracked_modal">
        Body
      </Modal>
    )

    fireEvent.keyDown(document, { key: 'Escape' })
    expect(onClose).toHaveBeenLastCalledWith('escape')

    const backdrop = document.body.querySelector('.absolute.inset-0.cursor-pointer.bg-app-bg\\/85')
    expect(backdrop).not.toBeNull()
    fireEvent.click(backdrop!)
    expect(onClose).toHaveBeenLastCalledWith('backdrop')

    rerender(
      <Modal isOpen={false} onClose={onClose} title="Tracked" analyticsId="tracked_modal">
        Body
      </Modal>
    )
    rerender(
      <Modal isOpen onClose={onClose} title="Tracked" analyticsId="tracked_modal">
        Body
      </Modal>
    )
    unmount()

    expect(analyticsMock.trackPerpsModalClosed).toHaveBeenLastCalledWith('tracked_modal', expect.objectContaining({
      close_reason: 'state_change',
    }))
  })

  it('names the dialog, traps focus, and restores the trigger', async () => {
    const onClose = vi.fn()
    const { rerender } = render(
      <>
        <button type="button">Open preview</button>
        <Modal isOpen={false} onClose={onClose} title="Accessible preview">
          <button type="button">Confirm action</button>
        </Modal>
      </>
    )

    const trigger = screen.getByRole('button', { name: 'Open preview' })
    trigger.focus()

    rerender(
      <>
        <button type="button">Open preview</button>
        <Modal isOpen onClose={onClose} title="Accessible preview" inertBackground>
          <button type="button">Confirm action</button>
        </Modal>
      </>
    )

    const dialog = screen.getByRole('dialog', { name: 'Accessible preview' })
    const closeButton = screen.getByRole('button', { name: 'Close dialog' })
    const confirmButton = screen.getByRole('button', { name: 'Confirm action' })
    expect(dialog).toBeInTheDocument()
    await waitFor(() => {
      expect(closeButton).toHaveFocus()
    })

    fireEvent.keyDown(document, { key: 'Tab', shiftKey: true })
    expect(confirmButton).toHaveFocus()

    rerender(
      <>
        <button type="button">Open preview</button>
        <Modal isOpen={false} onClose={onClose} title="Accessible preview">
          <button type="button">Confirm action</button>
        </Modal>
      </>
    )

    await waitFor(() => {
      expect(screen.getByRole('button', { name: 'Open preview' })).toHaveFocus()
    })
  })

  it('can focus the dialog without weakening the focus trap', async () => {
    render(
      <Modal isOpen onClose={vi.fn()} title="Review order" initialFocus="dialog">
        <button type="button">Confirm order</button>
      </Modal>
    )

    const dialog = screen.getByRole('dialog', { name: 'Review order' })
    const closeButton = screen.getByRole('button', { name: 'Close dialog' })
    const confirmButton = screen.getByRole('button', { name: 'Confirm order' })

    await waitFor(() => {
      expect(dialog).toHaveFocus()
    })

    fireEvent.keyDown(document, { key: 'Tab', shiftKey: true })
    expect(confirmButton).toHaveFocus()

    dialog.focus()
    fireEvent.keyDown(document, { key: 'Tab' })
    expect(closeButton).toHaveFocus()
  })
})
