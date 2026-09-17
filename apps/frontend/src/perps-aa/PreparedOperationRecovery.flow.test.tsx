import { act, fireEvent, render, screen } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import deployedManifest from '../../public/perps-aa-manifest.json'
import type { PerpsAaDeploymentManifestV2 } from './manifest'
import { executeSponsoredPerpsAction, resumeSponsoredPerpsAction } from './execution'
import { SponsorRequestError } from './errors'
import { usePerpsUiStore } from '../stores/perpsUiStore'
import type { PersistedPerpsOrderRequestV2 } from '../contracts/perpsOrderV2'
import { PreparedOperationRecovery } from './PreparedOperationRecovery'
import { PerpsAaRuntimeContext, type PerpsAaSmartAccountRuntime } from './runtimeContext'
import { useSponsoredOperationStore } from './operationStore'
import { PreparationRecoveryError, type WalletRecoveryResult } from './walletRecovery'
import { trackPerpsPreparationRecovery } from '../analytics/perps'

vi.mock('./laneLock', async original => ({
  ...await original<typeof import('./laneLock')>(),
  acquireSponsoredOperationBrowserLane: vi.fn(async () => async () => {}),
}))
vi.mock('./readiness', async original => ({
  ...await original<typeof import('./readiness')>(), refreshReadiness: vi.fn(async () => {}),
}))
vi.mock('../analytics/perps', async original => ({
  ...await original<typeof import('../analytics/perps')>(),
  trackPerpsPreparationRecovery: vi.fn(), trackPerpsSponsoredOperation: vi.fn(),
}))

const owner = '0x1111111111111111111111111111111111111111'
const account = '0x2222222222222222222222222222222222222222'
const manifest = deployedManifest as PerpsAaDeploymentManifestV2
const resolved: WalletRecoveryResult = {
  version: 1, phase: 'resolved', authorizationState: 'expired', reason: 'PREPARATION_UNUSABLE',
  serverTime: '2000000000', safeBlockTimestamp: '2000000000', validUntil: '1999999000',
  recoverable: false, freshReviewAllowed: true, userOperationHash: `0x${'a'.repeat(64)}`,
  transactionHash: null, recoveryVerified: true, canRetire: true,
}
const retired: WalletRecoveryResult = {
  version: 1, recoveryState: 'retired', reason: 'PREPARATION_RETIRED', canRetire: false, operationHashes: [],
}

async function lostResponse() {
  const prepare = vi.fn().mockRejectedValue(new Error('Response lost after backend authorization'))
  const verify = vi.fn<(id: string) => Promise<void>>(async () => {})
  const check = vi.fn<(id: string) => Promise<WalletRecoveryResult>>(async () => resolved)
  const retire = vi.fn<(id: string) => Promise<WalletRecoveryResult>>(async () => retired)
  const sign = vi.fn(), submit = vi.fn()
  const runtime = {
    chainId: manifest.chainId, ownerAddress: owner, factoryAddress: manifest.smartAccountFactory,
    accountVersion: manifest.smartAccountVersion, accountIndex: manifest.smartAccountIndex,
    smartAccount: {
      accountAddress: account, entryPoint: manifest.entryPoint, prepareUserOperation: prepare,
      signUserOperation: sign, sendUserOperation: submit,
      getPreparationStatus: vi.fn(async () => resolved),
    },
    preparationRecovery: { verify, status: check, retire },
  } as unknown as PerpsAaSmartAccountRuntime
  const input = {
    manifest, ownerAddress: owner, runtime,
    action: { kind: 'cancel-protection', account, calls: [{ to: manifest.positionProtectionBook!, value: 0n, data: '0x1234' }] },
  } as Parameters<typeof executeSponsoredPerpsAction>[0]
  await expect(executeSponsoredPerpsAction(input)).rejects.toThrow('Response lost')
  await useSponsoredOperationStore.persist.rehydrate()
  const saved = useSponsoredOperationStore.getState().operations[0]
  expect(saved).toMatchObject({ status: 'preparation-pending', reason: 'UNKNOWN' })
  expect(saved.preparedOperation).toBeUndefined()
  expect(useSponsoredOperationStore.getState().getActiveOperation(account)?.id).toBe(saved.id)
  const view = render(<PerpsAaRuntimeContext value={runtime}><PreparedOperationRecovery operation={saved} /></PerpsAaRuntimeContext>)
  await act(async () => {})
  return { ...view, saved, runtime, input, prepare, verify, check, retire, sign, submit }
}

describe('lost preparation response recovery with the durable trading lane', () => {
  beforeEach(() => {
    vi.useFakeTimers()
    vi.clearAllMocks()
    localStorage.clear()
    usePerpsUiStore.setState({ orderReviewRequest: null })
    useSponsoredOperationStore.setState({ operations: [], activeLanes: {} })
  })
  afterEach(() => { vi.useRealTimers() })

  it('rejects an expired resume before sponsorship and restores inputs only after verified retirement', async () => {
    const flow = await lostResponse()
    flow.unmount()
    // Resolve the unrelated setup fixture before creating the order under test.
    useSponsoredOperationStore.getState().markPreparationResolved(flow.saved.id)
    useSponsoredOperationStore.getState().transition(flow.saved.id, 'cancelled')
    flow.prepare.mockRejectedValue(new SponsorRequestError({ reason: 'ACCOUNT_DEPLOYMENT_PENDING', retryable: true, message: 'Account pending' }))
    const draft = { version: 1 as const, direction: 'short' as const, orderQuantity: '125', leverage: 3, slippage: 0.5,
      reduceOnly: false, fullClose: false, maxOpen: false, protectionEnabled: false,
      protection: { mode: 'price' as const, takeProfit: '', stopLoss: '' } }
    const order = { validUntil: String(Math.floor(Date.now() / 1000) + 120), sizeDelta: String(125n * 10n ** 18n), side: 1,
      isClose: false } as PersistedPerpsOrderRequestV2
    await expect(executeSponsoredPerpsAction({ ...flow.input, action: { ...flow.input.action, kind: 'place-order' },
      orderRequestV2: order, orderDraft: draft })).rejects.toThrow('Account pending')
    const pending = useSponsoredOperationStore.getState().getActiveOperation(account)!
    await act(async () => { await vi.advanceTimersByTimeAsync(20 * 60_000) })
    const prepareCount = flow.prepare.mock.calls.length
    await expect(resumeSponsoredPerpsAction(pending, flow.runtime)).rejects.toMatchObject({ reason: 'INVALID_ORDER_DEADLINE' })
    expect(flow.prepare).toHaveBeenCalledTimes(prepareCount)
    expect(flow.sign).not.toHaveBeenCalled()
    expect(flow.submit).not.toHaveBeenCalled()
    await useSponsoredOperationStore.persist.rehydrate()
    const saved = useSponsoredOperationStore.getState().getActiveOperation(account)!
    expect(saved.orderDraft).toEqual(draft)
    flow.check.mockResolvedValue({ version: 1, recoveryState: 'missing', reason: 'PREPARATION_NOT_CREATED', canRetire: true, operationHashes: [] })
    const view = render(<PerpsAaRuntimeContext value={flow.runtime}><PreparedOperationRecovery operation={saved} /></PerpsAaRuntimeContext>)
    await act(async () => {})
    expect(screen.queryByRole('button', { name: /^Resume/ })).not.toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Review order again' })).toBeDisabled()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Verify wallet to recover' })) })
    expect(screen.getByRole('button', { name: 'Review order again' })).toBeEnabled()
    // A racing liability still prevents retirement despite the earlier advisory permission.
    flow.retire.mockResolvedValueOnce({ version: 1, recoveryState: 'unresolved', reason: 'RECOVERY_LIABILITY_PENDING', canRetire: false, operationHashes: [] })
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Review order again' })) })
    expect(useSponsoredOperationStore.getState().getActiveOperation(account)?.id).toBe(saved.id)
    expect(usePerpsUiStore.getState().orderReviewRequest).toBeNull()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Check recovery again' })) })
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Review order again' })) })
    expect(useSponsoredOperationStore.getState().getActiveOperation(account)).toBeUndefined()
    expect(usePerpsUiStore.getState().orderReviewRequest?.operation).toMatchObject({ id: saved.id, status: 'cancelled', preparationResolved: true, orderDraft: draft })
    expect(flow.sign).not.toHaveBeenCalled()
    expect(flow.submit).not.toHaveBeenCalled()
    view.unmount()
  })

  it('unlocks a fresh action only after owner verification and authoritative retirement of the expired attempt', async () => {
    const flow = await lostResponse()
    expect(screen.getByText(/unused sponsorship has safely expired/)).toBeInTheDocument()
    expect(screen.queryByRole('button', { name: /Resume/ })).not.toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Discard saved transaction' })).toBeDisabled()
    expect(flow.verify).not.toHaveBeenCalled()
    expect(flow.retire).not.toHaveBeenCalled()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Verify wallet to recover' })) })
    expect(flow.verify).toHaveBeenCalledExactlyOnceWith(flow.saved.id)
    expect(useSponsoredOperationStore.getState().getActiveOperation(account)?.id).toBe(flow.saved.id)

    let finishRetirement!: (result: WalletRecoveryResult) => void
    flow.retire.mockImplementationOnce(() => new Promise(resolve => { finishRetirement = resolve }))
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Discard saved transaction' })) })
    expect(useSponsoredOperationStore.getState().getActiveOperation(account)?.id).toBe(flow.saved.id)
    await act(async () => { finishRetirement(retired) })
    expect(useSponsoredOperationStore.getState().operations[0]).toMatchObject({ status: 'cancelled', preparationResolved: true })
    expect(useSponsoredOperationStore.getState().getActiveOperation(account)).toBeUndefined()
    await useSponsoredOperationStore.persist.rehydrate()
    expect(useSponsoredOperationStore.getState().getActiveOperation(account)).toBeUndefined()
    // A fresh reviewed action can reach preparation; recovery itself never does.
    expect(flow.prepare).toHaveBeenCalledTimes(1)
    await expect(executeSponsoredPerpsAction(flow.input)).rejects.toThrow('Response lost')
    expect(flow.prepare).toHaveBeenCalledTimes(2)
    expect(useSponsoredOperationStore.getState().operations).toHaveLength(2)
    expect(flow.sign).not.toHaveBeenCalled()
    expect(flow.submit).not.toHaveBeenCalled()
    flow.unmount()
  })

  it('retries a failed verified status read without another signature and keeps the lane locked', async () => {
    const flow = await lostResponse()
    flow.check.mockRejectedValueOnce(new PreparationRecoveryError('RECOVERY_TIMEOUT'))
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Verify wallet to recover' })) })
    expect(screen.getByRole('alert')).toHaveTextContent('timed out')
    expect(screen.queryByRole('button', { name: 'Verify wallet to recover' })).not.toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Discard saved transaction' })).toBeDisabled()
    expect(useSponsoredOperationStore.getState().getActiveOperation(account)?.id).toBe(flow.saved.id)
    expect(trackPerpsPreparationRecovery).toHaveBeenCalledWith({ attemptId: flow.saved.id, outcome: 'status-failed', reason: 'RECOVERY_TIMEOUT' })
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Check recovery again' })) })
    expect(screen.queryByRole('alert')).not.toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Discard saved transaction' })).toBeEnabled()
    expect(flow.verify).toHaveBeenCalledTimes(1)
    expect(flow.retire).not.toHaveBeenCalled()
    flow.unmount()
  })

  it('retains the lock after a lost retirement response and clears it after an explicit idempotent retry', async () => {
    const flow = await lostResponse()
    flow.retire.mockRejectedValueOnce(new PreparationRecoveryError('RECOVERY_UNAVAILABLE'))
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Verify wallet to recover' })) })
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Discard saved transaction' })) })
    expect(useSponsoredOperationStore.getState().getActiveOperation(account)?.id).toBe(flow.saved.id)
    expect(screen.getByRole('alert')).toHaveTextContent('could not be reached')
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Discard saved transaction' })) })
    expect(flow.retire.mock.calls.map(args => args[0])).toEqual([flow.saved.id, flow.saved.id])
    expect(useSponsoredOperationStore.getState().getActiveOperation(account)).toBeUndefined()
    expect(screen.queryByRole('alert')).not.toBeInTheDocument()
    flow.unmount()
  })

  it('invalidates stale discard permission after a failed check and requests a new proof only when the session expires', async () => {
    const flow = await lostResponse()
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Verify wallet to recover' })) })
    expect(screen.getByRole('button', { name: 'Discard saved transaction' })).toBeEnabled()
    flow.check.mockRejectedValueOnce(new PreparationRecoveryError('RECOVERY_TIMEOUT'))
    await act(async () => { window.dispatchEvent(new Event('focus')) })
    expect(screen.getByRole('button', { name: 'Discard saved transaction' })).toBeDisabled()
    expect(screen.queryByRole('button', { name: 'Verify wallet to recover' })).not.toBeInTheDocument()
    flow.check.mockRejectedValueOnce(new PreparationRecoveryError('RECOVERY_VERIFICATION_REQUIRED'))
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Check recovery again' })) })
    expect(screen.getByRole('button', { name: 'Verify wallet to recover' })).toBeEnabled()
    expect(screen.getByRole('button', { name: 'Discard saved transaction' })).toBeDisabled()
    expect(flow.verify).toHaveBeenCalledTimes(1)
    expect(flow.retire).not.toHaveBeenCalled()
    expect(useSponsoredOperationStore.getState().getActiveOperation(account)?.id).toBe(flow.saved.id)
    flow.unmount()
  })
})
