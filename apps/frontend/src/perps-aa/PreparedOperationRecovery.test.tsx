import { act, fireEvent, render, screen } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { PreparedOperationRecovery } from './PreparedOperationRecovery'
import { PerpsAaRuntimeContext, type PerpsAaSmartAccountRuntime } from './runtimeContext'
import { useSponsoredOperationStore, type SponsoredOperation } from './operationStore'
import { createDeploymentConfirmationGate } from './deploymentConfirmation'
import { AccountDeploymentConfirmation } from './AccountDeploymentConfirmation'
import { SponsorRequestError } from './errors'
import type { WalletPreparationRecovery } from './walletRecovery'
vi.mock('./laneLock', () => ({ acquireSponsoredOperationBrowserLane: vi.fn(async () => async () => {}) }))
vi.mock('./operationStore', async importOriginal => ({ ...await importOriginal<typeof import('./operationStore')>(), restoreSponsoredOperationLane: vi.fn() }))
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
  afterEach(() => { vi.restoreAllMocks(); vi.useRealTimers() })
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
  it('waits for account confirmation without polling a missing preparation, then enables manual resume', async () => {
    const safeCode = vi.fn(async () => false)
    const gate = createDeploymentConfirmationGate(safeCode, () => Date.now())
    await expect(gate(async () => { throw new SponsorRequestError({
      reason: 'ACCOUNT_DEPLOYMENT_PENDING', retryable: true, message: 'pending',
    }) })).rejects.toThrow()
    const getPreparationStatus = vi.fn().mockRejectedValue(new Error('PREPARATION_NOT_AUTHORIZED'))
    const runtime = { smartAccount: { getPreparationStatus }, deploymentConfirmation: gate } as unknown as PerpsAaSmartAccountRuntime
    const pending = { ...operation, preparedOperation: undefined, status: 'preparation-pending' as const,
      reason: 'ACCOUNT_DEPLOYMENT_PENDING' as const }
    const view = render(<PerpsAaRuntimeContext value={runtime}>
      <AccountDeploymentConfirmation /><PreparedOperationRecovery operation={pending} />
    </PerpsAaRuntimeContext>)
    await act(async () => { await vi.advanceTimersByTimeAsync(15_000) })
    expect(screen.getByRole('button', { name: 'Resume deposit margin' })).toBeDisabled()
    expect(screen.getByRole('button', { name: 'Discard saved transaction' })).toBeDisabled()
    expect(getPreparationStatus).not.toHaveBeenCalled()
    safeCode.mockResolvedValue(true)
    await act(async () => { await vi.advanceTimersByTimeAsync(15_000) })
    expect(screen.getByText('Trading Account confirmed. Resume this saved attempt to continue.')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Resume deposit margin' })).toBeEnabled()
    // Confirmation permits a manual retry, never automatic execution or discard.
    expect(screen.getByRole('button', { name: 'Discard saved transaction' })).toBeDisabled()
    expect(getPreparationStatus).not.toHaveBeenCalled()
    expect(resume).not.toHaveBeenCalled()
    resume.mockRejectedValueOnce(new Error('The saved review has expired'))
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Resume deposit margin' })) })
    expect(resume).toHaveBeenCalledWith(pending, runtime)
    expect(screen.getByRole('alert')).toHaveTextContent('Your saved attempt is retained')
    view.unmount()
  })
  it('requires explicit verification and authoritative retirement before cancelling locally', async () => {
    const verify = vi.fn(async () => {})
    const retired = { version: 1, recoveryState: 'retired', reason: 'PREPARATION_RETIRED', operationHashes: [], canRetire: false }
    const check = vi.fn(async () => ({ ...retired, recoveryState: 'missing', reason: 'PREPARATION_NOT_CREATED', canRetire: true }))
    const retire = vi.fn().mockResolvedValueOnce({ ...retired, recoveryState: 'unresolved', reason: 'RECOVERY_LIABILITY_PENDING' }).mockResolvedValue(retired)
    const cancel = vi.fn(), resolve = vi.fn()
    vi.spyOn(useSponsoredOperationStore, 'getState').mockReturnValue({ ...useSponsoredOperationStore.getState(),
      operations: [operation], transition: cancel, markPreparationResolved: resolve, recordConfirmationTiming: vi.fn(),
    })
    vi.spyOn(useSponsoredOperationStore.persist, 'rehydrate').mockResolvedValue(undefined)
    const getPreparationStatus = vi.fn().mockRejectedValue({ reason: 'PREPARATION_NOT_AUTHORIZED' })
    const runtime = { smartAccount: { getPreparationStatus }, preparationRecovery: { verify, status: check, retire } as unknown as WalletPreparationRecovery } as unknown as PerpsAaSmartAccountRuntime
    const view = render(<PerpsAaRuntimeContext value={runtime}><PreparedOperationRecovery operation={operation} /></PerpsAaRuntimeContext>)
    await act(async () => {})
    expect(verify).not.toHaveBeenCalled()
    expect(retire).not.toHaveBeenCalled()
    expect(screen.getByRole('button', { name: 'Resume deposit margin' })).toBeDisabled()
    expect(screen.getByRole('button', { name: 'Discard saved transaction' })).toBeDisabled()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Verify wallet to recover' })) })
    expect(verify).toHaveBeenCalledWith(operation.id)
    expect(resume).not.toHaveBeenCalled()
    expect(retire).not.toHaveBeenCalled()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Discard saved transaction' })) })
    expect(retire).toHaveBeenCalledTimes(1)
    expect(cancel).not.toHaveBeenCalled()
    expect(resolve).not.toHaveBeenCalled()
    await act(async () => { window.dispatchEvent(new Event('focus')) })
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Discard saved transaction' })) })
    expect(resolve).toHaveBeenCalledWith(operation.id)
    expect(cancel).toHaveBeenCalledWith(operation.id, 'cancelled')
    expect(resume).not.toHaveBeenCalled()
    view.unmount()
  })
  it('rejects another connected account before any recovery request or wallet prompt', async () => {
    const verify = vi.fn(), getPreparationStatus = vi.fn()
    const runtime = { chainId: 1, smartAccount: { getPreparationStatus }, preparationRecovery: { verify } } as unknown as PerpsAaSmartAccountRuntime
    const view = render(<PerpsAaRuntimeContext value={runtime}><PreparedOperationRecovery operation={operation} /></PerpsAaRuntimeContext>)
    await act(async () => {})
    expect(screen.getByText(/Connect the owner wallet/)).toBeInTheDocument()
    expect(getPreparationStatus).not.toHaveBeenCalled()
    expect(verify).not.toHaveBeenCalled()
    view.unmount()
  })
  it('tracks ambiguous outcomes and never offers to resume an arbitrary match', async () => {
    const hash = `0x${'1'.repeat(64)}`
    const recovery = { verify: vi.fn(async () => {}), status: vi.fn(async () => ({ version: 1, recoveryState: 'ambiguous',
      reason: 'RECOVERY_MULTIPLE_PREPARATIONS', canRetire: false, operationHashes: [hash],
      operationOutcomes: [{ hash, state: 'submitted', transactionHash: null, executionSuccess: null }],
    })) }
    const runtime = { smartAccount: { getPreparationStatus: vi.fn().mockRejectedValue({ reason: 'PREPARATION_NOT_AUTHORIZED' }) }, preparationRecovery: recovery } as unknown as PerpsAaSmartAccountRuntime
    const view = render(<PerpsAaRuntimeContext value={runtime}><PreparedOperationRecovery operation={operation} /></PerpsAaRuntimeContext>)
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Verify wallet to recover' })) })
    expect(screen.getByText(/Submitted; awaiting safe confirmation/)).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Resume deposit margin' })).toBeDisabled()
    expect(screen.getByRole('button', { name: 'Discard saved transaction' })).toBeDisabled()
    expect(resume).not.toHaveBeenCalled()
    view.unmount()
  })
  it('still checks an existing prepared payload after an account-confirmation rejection', async () => {
    const getPreparationStatus = vi.fn(async () => ({ ...status, recoverable: false, freshReviewAllowed: false }))
    const runtime = { smartAccount: { getPreparationStatus } } as unknown as PerpsAaSmartAccountRuntime
    const view = render(<PerpsAaRuntimeContext value={runtime}><PreparedOperationRecovery operation={{
      ...operation, status: 'preparation-pending', reason: 'ACCOUNT_DEPLOYMENT_PENDING',
    }} /></PerpsAaRuntimeContext>)
    await act(async () => {})
    expect(getPreparationStatus).toHaveBeenCalledTimes(1)
    expect(screen.getByRole('button', { name: 'Resume deposit margin' })).toBeDisabled()
    expect(screen.getByRole('button', { name: 'Discard saved transaction' })).toBeDisabled()
    view.unmount()
  })
})
