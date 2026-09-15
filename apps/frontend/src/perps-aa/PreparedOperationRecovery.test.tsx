import { act, fireEvent, render, screen } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { PreparedOperationRecovery } from './PreparedOperationRecovery'
import { PerpsAaRuntimeContext, type PerpsAaSmartAccountRuntime } from './runtimeContext'
import type { SponsoredOperation } from './operationStore'
const resume = vi.hoisted(() => vi.fn(async () => {}))
vi.mock('./execution', () => ({ resumeSponsoredPerpsAction: resume }))
const operation = { id: 'prepared', action: 'deposit', status: 'signature-declined', walletPreparationOutcome: 'declined',
  nativePreparation: { version: 1, manifest: { chainId: 421614 }, preparationId: 'prepared', action: { kind: 'deposit', calls: [] } },
  preparedOperation: { version: 1 },
} as unknown as SponsoredOperation
const status = { version: 1, authorizationState: 'signed', phase: 'prepared', recoverable: true, freshReviewAllowed: true, reason: 'RESUMABLE' }
function mount(getPreparationStatus = vi.fn(async () => status)) {
  const runtime = { smartAccount: { getPreparationStatus } } as unknown as PerpsAaSmartAccountRuntime
  const view = render(<PerpsAaRuntimeContext value={runtime}><PreparedOperationRecovery operation={operation} /></PerpsAaRuntimeContext>)
  return { ...view, getPreparationStatus }
}
describe('visible preparation recovery', () => {
  beforeEach(() => { vi.useFakeTimers(); resume.mockClear() })
  afterEach(() => { vi.useRealTimers() })
  it('polls every 15 seconds, refreshes on focus, and never signs without a click', async () => {
    const view = mount()
    await act(async () => {})
    expect(view.getPreparationStatus).toHaveBeenCalledTimes(1)
    await act(async () => { await vi.advanceTimersByTimeAsync(15_000) })
    expect(view.getPreparationStatus).toHaveBeenCalledTimes(2)
    await act(async () => { window.dispatchEvent(new Event('focus')) })
    expect(view.getPreparationStatus).toHaveBeenCalledTimes(3)
    expect(resume).not.toHaveBeenCalled()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Resume deposit margin' })) })
    expect(resume).toHaveBeenCalledTimes(1)
    view.unmount()
    await act(async () => { await vi.advanceTimersByTimeAsync(60_000) })
    expect(view.getPreparationStatus).toHaveBeenCalledTimes(3)
  })
  it('backs off errors for 60 seconds and stops polling resolved status', async () => {
    const get = vi.fn(async () => ({ ...status, phase: 'resolved', recoverable: false })).mockRejectedValueOnce(new Error('offline'))
    const view = mount(get)
    await act(async () => {})
    expect(screen.getByRole('button', { name: 'Resume deposit margin' })).toBeDisabled()
    await act(async () => { await vi.advanceTimersByTimeAsync(59_999) })
    expect(get).toHaveBeenCalledTimes(1)
    await act(async () => { await vi.advanceTimersByTimeAsync(1) })
    expect(get).toHaveBeenCalledTimes(2)
    await act(async () => { await vi.advanceTimersByTimeAsync(120_000) })
    expect(get).toHaveBeenCalledTimes(2)
    expect(resume).not.toHaveBeenCalled()
    view.unmount()
  })
  it('shows the reservation wait only when status says assistance blocks the action', async () => {
    const view = mount(vi.fn(async () => ({ ...status, reason: 'SAFE_EXPIRY_WAIT', recoverable: false, freshReviewAllowed: false })))
    await act(async () => {})
    expect(screen.getByText('Waiting for sponsorship reservation to clear')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Discard saved transaction' })).toBeDisabled()
    expect(resume).not.toHaveBeenCalled()
    view.unmount()
  })
})
