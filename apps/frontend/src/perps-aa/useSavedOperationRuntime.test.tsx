import { act, render, screen } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { AccountOperationNotice } from '../components/AccountOperationNotice'
import { useSavedOperationConfirmation } from './useSavedOperationRuntime'
import { createDeploymentConfirmationGate } from './deploymentConfirmation'
import { PerpsAaRuntimeContext, type PerpsAaSmartAccountRuntime } from './runtimeContext'
import type { SponsoredOperation } from './operationStore'
import type { PerpsAaDeploymentManifestV2 } from './manifest'

const owner = '0x1111111111111111111111111111111111111111'
const account = '0x2222222222222222222222222222222222222222'
const operation = { id: 'account-pending', ownerAddress: owner, accountAddress: account, chainId: 421614,
  action: 'place-order', status: 'preparation-pending', reason: 'ACCOUNT_DEPLOYMENT_PENDING',
  createdAt: Date.now() - 17 * 60_000, statusTimestamps: {},
} as SponsoredOperation
function Notice({ saved = operation }: { saved?: SponsoredOperation }) {
  const confirmation = useSavedOperationConfirmation(saved)
  return <AccountOperationNotice operation={saved} confirmation={confirmation} now={operation.createdAt + 17 * 60_000} onOpen={() => {}} />
}
function runtime(check: () => Promise<boolean>) {
  return { chainId: 421614, ownerAddress: owner, smartAccount: { accountAddress: account },
    deploymentConfirmation: createDeploymentConfirmationGate(check, Date.now),
  } as PerpsAaSmartAccountRuntime
}
describe('saved account confirmation outside Activity', () => {
  beforeEach(() => { vi.useFakeTimers() })
  afterEach(() => { vi.useRealTimers() })
  it('restores a pending check from the saved reason and updates the ticket without opening Activity', async () => {
    const check = vi.fn(async () => false)
    const value = runtime(check)
    const view = render(<PerpsAaRuntimeContext value={value}><Notice /></PerpsAaRuntimeContext>)
    await act(async () => {})
    expect(screen.getByText('Confirming your Trading Account')).toBeInTheDocument()
    check.mockResolvedValue(true)
    await act(async () => { await vi.advanceTimersByTimeAsync(15_000) })
    expect(screen.getByText('Trading Account confirmed')).toBeInTheDocument()
    expect(operation.reason).toBe('ACCOUNT_DEPLOYMENT_PENDING')
    view.unmount()
    const count = check.mock.calls.length
    await act(async () => { await vi.advanceTimersByTimeAsync(30_000) })
    expect(check).toHaveBeenCalledTimes(count)
  })
  it('distinguishes an unavailable check and refreshes on focus', async () => {
    const check = vi.fn(async () => false).mockRejectedValueOnce(new Error('offline'))
    const value = runtime(check)
    const view = render(<PerpsAaRuntimeContext value={value}><Notice /></PerpsAaRuntimeContext>)
    await act(async () => {})
    expect(screen.getByText('Unable to check account confirmation')).toBeInTheDocument()
    check.mockResolvedValue(true)
    await act(async () => { window.dispatchEvent(new Event('focus')) })
    expect(screen.getByText('Trading Account confirmed')).toBeInTheDocument()
    view.unmount()
  })
  it('never borrows confirmation from another owner, account or network', async () => {
    for (const override of [{ chainId: 1 }, { ownerAddress: account }, { smartAccount: { accountAddress: owner } }]) {
      const check = vi.fn(async () => true)
      const value = { ...runtime(check), ...override } as PerpsAaSmartAccountRuntime
      const view = render(<PerpsAaRuntimeContext value={value}><Notice /></PerpsAaRuntimeContext>)
      await act(async () => {})
      expect(screen.queryByText('Trading Account confirmed')).not.toBeInTheDocument()
      expect(check).not.toHaveBeenCalled()
      view.unmount()
    }
  })
  it('resolves the saved deployment instead of using the current deployment monitor', async () => {
    const currentCheck = vi.fn(async () => true)
    const savedCheck = vi.fn(async () => false)
    const savedRuntime = runtime(savedCheck)
    const getPreparedOperationRuntime = vi.fn(async () => savedRuntime)
    const value = { ...runtime(currentCheck), getPreparedOperationRuntime }
    const manifest = { version: 'historical' } as PerpsAaDeploymentManifestV2
    const saved = { ...operation, nativePreparation: { manifest } } as SponsoredOperation
    const view = render(<PerpsAaRuntimeContext value={value}><Notice saved={saved} /></PerpsAaRuntimeContext>)
    await act(async () => {})
    expect(getPreparedOperationRuntime).toHaveBeenCalledWith(manifest)
    expect(savedCheck).toHaveBeenCalledOnce()
    expect(currentCheck).not.toHaveBeenCalled()
    expect(screen.getByText('Confirming your Trading Account')).toBeInTheDocument()
    view.unmount()
  })
})
