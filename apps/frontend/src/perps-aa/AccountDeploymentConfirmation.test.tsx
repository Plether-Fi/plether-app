import { act, cleanup, render, screen } from '@testing-library/react'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { AccountDeploymentConfirmation } from './AccountDeploymentConfirmation'
import { useAccountDeploymentConfirmation } from './useAccountDeploymentConfirmation'
import { createDeploymentConfirmationGate } from './deploymentConfirmation'
import { SponsorRequestError } from './errors'

const runtime = vi.hoisted(() => ({ current: undefined as unknown }))
vi.mock('./runtimeContext', () => ({ usePerpsAaRuntime: () => runtime.current }))

function Action() {
  return <button disabled={useAccountDeploymentConfirmation() === 'waiting'}>Manual action</button>
}

describe('account confirmation status', () => {
  afterEach(() => {
    cleanup()
    runtime.current = undefined
    vi.useRealTimers()
  })

  it('shows persistent waiting, unlocks after a read, and cleans up polling', async () => {
    vi.useFakeTimers()
    const check = vi.fn().mockResolvedValue(false)
    const gate = createDeploymentConfirmationGate(check, () => Date.now())
    runtime.current = { deploymentConfirmation: gate }
    const view = render(<><AccountDeploymentConfirmation /><Action /></>)
    expect(screen.queryByRole('status')).not.toBeInTheDocument()
    const action = vi.fn().mockRejectedValue(new SponsorRequestError({
      reason: 'ACCOUNT_DEPLOYMENT_PENDING', retryable: true, message: '',
    }))
    await act(async () => {
      await expect(gate(action)).rejects.toMatchObject({ reason: 'ACCOUNT_DEPLOYMENT_PENDING' })
    })
    expect(screen.getByRole('status')).toHaveTextContent('Trading Account awaiting confirmation')
    expect(screen.getByRole('button')).toBeDisabled()
    view.unmount()
    expect(vi.getTimerCount()).toBe(0)
    render(<><AccountDeploymentConfirmation /><Action /></>)
    expect(screen.getByRole('status')).toHaveTextContent('no transaction will be sent automatically')
    check.mockResolvedValue(true)
    await act(async () => { await vi.advanceTimersByTimeAsync(15_000) })
    expect(screen.getByRole('status')).toHaveTextContent('Trading Account confirmed')
    expect(screen.getByRole('button')).not.toBeDisabled()
    expect(action).toHaveBeenCalledTimes(1)
    expect(vi.getTimerCount()).toBe(0)
  })

  it('does not let an old account read change a newly selected account', async () => {
    vi.useFakeTimers()
    let resolve!: (confirmed: boolean) => void
    const first = createDeploymentConfirmationGate(() => new Promise<boolean>(yes => { resolve = yes }), () => Date.now())
    const second = createDeploymentConfirmationGate(async () => false)
    const pending = async () => { throw new SponsorRequestError({ reason: 'ACCOUNT_DEPLOYMENT_PENDING', retryable: true, message: '' }) }
    await expect(first(pending)).rejects.toThrow()
    await expect(second(pending)).rejects.toThrow()
    runtime.current = { deploymentConfirmation: first }
    const view = render(<><AccountDeploymentConfirmation /><Action /></>)
    await act(async () => { await vi.advanceTimersByTimeAsync(15_000) })
    runtime.current = { deploymentConfirmation: second }
    view.rerender(<><AccountDeploymentConfirmation /><Action /></>)
    await act(async () => { resolve(true) })
    expect(screen.getByRole('status')).toHaveTextContent('Trading Account awaiting confirmation')
    expect(screen.getByRole('button')).toBeDisabled()
    expect(second.getSnapshot()).toBe('waiting')
  })
})
