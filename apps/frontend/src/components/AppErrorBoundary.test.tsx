import { StrictMode, Suspense, lazy, useEffect } from 'react'
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { AppErrorBoundary } from './AppErrorBoundary'
import { captureFrontendLog, captureReactException } from '../analytics/client'

vi.mock('../analytics/client', () => ({ captureFrontendLog: vi.fn(), captureReactException: vi.fn() }))
vi.mock('../config/buildInfo', () => ({ BUILD_COMMIT: 'test-release-123' }))

function BrokenProvider(): never { throw new Error('private-provider-detail') }
function BrokenEffect() { useEffect(() => { throw new Error('effect failure') }, []); return <p>Trading app</p> }

describe('application recovery boundary', () => {
  beforeEach(() => { vi.clearAllMocks(); vi.spyOn(console, 'error').mockImplementation(() => {}) })
  afterEach(() => { cleanup(); vi.restoreAllMocks() })

  it('passes healthy children through', () => {
    render(<AppErrorBoundary><p>Trading app</p></AppErrorBoundary>)
    expect(screen.getByText('Trading app')).toBeVisible()
    expect(captureReactException).not.toHaveBeenCalled()
  })

  it('catches provider failures, reports once, and preserves storage on manual reload', () => {
    localStorage.setItem('saved-operation', 'recovery-record')
    const reload = vi.spyOn(window.location, 'reload').mockImplementation(() => {})
    const clear = vi.spyOn(localStorage, 'clear')
    const remove = vi.spyOn(localStorage, 'removeItem')
    const view = render(<StrictMode><AppErrorBoundary><BrokenProvider /></AppErrorBoundary></StrictMode>)
    expect(screen.getByRole('alert')).toBeVisible()
    expect(screen.getByRole('heading', { name: /interface stopped/ })).toBeVisible()
    expect(screen.queryByText('private-provider-detail')).not.toBeInTheDocument()
    expect(screen.getByText('test-release-123')).toBeVisible()
    const reference = screen.getByText(/^ui-/).textContent
    expect(captureReactException).toHaveBeenCalledTimes(1)
    expect(captureReactException).toHaveBeenCalledWith(expect.any(Error), expect.any(Object), 'caught', reference)
    expect(captureFrontendLog).toHaveBeenCalledWith('error', expect.any(String), expect.objectContaining({ support_reference: reference }))
    view.rerender(<StrictMode><AppErrorBoundary><p>Do not automatically remount</p></AppErrorBoundary></StrictMode>)
    expect(screen.getByText(reference!)).toBeVisible()
    expect(screen.queryByText('Do not automatically remount')).not.toBeInTheDocument()
    expect(reload).not.toHaveBeenCalled()
    fireEvent.click(screen.getByRole('button', { name: 'Reload page' }))
    expect(reload).toHaveBeenCalledOnce()
    expect(clear).not.toHaveBeenCalled()
    expect(remove).not.toHaveBeenCalled()
    expect(localStorage.getItem('saved-operation')).toBe('recovery-record')
  })

  it('handles effect failures', () => {
    render(<AppErrorBoundary><BrokenEffect /></AppErrorBoundary>)
    expect(screen.getByRole('alert')).toBeVisible()
  })

  it('handles rejected lazy imports instead of leaving a blank page', async () => {
    const BrokenLazy = lazy(() => Promise.reject(new Error('Failed to fetch dynamically imported module')))
    render(<AppErrorBoundary><Suspense fallback={<p>Loading</p>}><BrokenLazy /></Suspense></AppErrorBoundary>)
    await waitFor(() => expect(screen.getByRole('alert')).toBeVisible())
  })

  it('still renders the recovery screen if telemetry throws', () => {
    vi.mocked(captureReactException).mockImplementationOnce(() => { throw new Error('telemetry failed') })
    render(<AppErrorBoundary><BrokenProvider /></AppErrorBoundary>)
    expect(screen.getByRole('button', { name: 'Reload page' })).toBeVisible()
  })
})
