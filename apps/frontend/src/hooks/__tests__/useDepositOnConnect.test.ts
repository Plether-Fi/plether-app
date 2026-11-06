import { renderHook } from '@testing-library/react'
import { describe, expect, it, vi } from 'vitest'
import { useDepositOnConnect } from '../useDepositOnConnect'

function options(overrides: Partial<Parameters<typeof useDepositOnConnect>[0]> = {}) {
  return {
    connected: true, identityKey: '421614:owner:account', enabled: true,
    balance: undefined as bigint | undefined, depositOpen: false, canOpen: true,
    onOpen: vi.fn(), ...overrides,
  }
}

describe('deposit after connecting', () => {
  it('waits for a confirmed zero balance and does not reopen after dismissal or polling', () => {
    const initial = options()
    const view = renderHook(useDepositOnConnect, { initialProps: initial })
    expect(initial.onOpen).not.toHaveBeenCalled()
    view.rerender({ ...initial, balance: 0n })
    expect(initial.onOpen).toHaveBeenCalledOnce()
    view.rerender({ ...initial, balance: 0n, depositOpen: true })
    view.rerender({ ...initial, balance: 0n })
    view.rerender({ ...initial, balance: undefined })
    view.rerender({ ...initial, balance: 0n })
    expect(initial.onOpen).toHaveBeenCalledOnce()
  })

  it.each([1n, 100_000_000n])('does not prompt a funded account or reopen when it later reaches zero (%s)', balance => {
    const initial = options({ balance })
    const view = renderHook(useDepositOnConnect, { initialProps: initial })
    view.rerender({ ...initial, balance: 0n })
    expect(initial.onOpen).not.toHaveBeenCalled()
  })

  it('waits for onboarding, account reads, and other dialogs or actions to finish', () => {
    const initial = options({ balance: 0n, enabled: false })
    const view = renderHook(useDepositOnConnect, { initialProps: initial })
    view.rerender({ ...initial, enabled: true, canOpen: false })
    expect(initial.onOpen).not.toHaveBeenCalled()
    view.rerender({ ...initial, enabled: true })
    expect(initial.onOpen).toHaveBeenCalledOnce()
  })

  it('accepts a manual deposit dialog as the prompt even before the balance loads', () => {
    const initial = options({ enabled: false, depositOpen: true })
    const view = renderHook(useDepositOnConnect, { initialProps: initial })
    view.rerender({ ...initial, enabled: true, balance: 0n, depositOpen: false })
    expect(initial.onOpen).not.toHaveBeenCalled()
  })

  it('checks another owner and checks again after disconnecting and reconnecting', () => {
    const initial = options({ balance: 0n })
    const view = renderHook(useDepositOnConnect, { initialProps: initial })
    expect(initial.onOpen).toHaveBeenCalledOnce()
    view.rerender({ ...initial, identityKey: '421614:other:another-account', balance: undefined })
    expect(initial.onOpen).toHaveBeenCalledOnce()
    view.rerender({ ...initial, identityKey: '421614:other:another-account' })
    expect(initial.onOpen).toHaveBeenCalledTimes(2)
    view.rerender({ ...initial, connected: false })
    view.rerender(initial)
    expect(initial.onOpen).toHaveBeenCalledTimes(3)
  })

  it('does not treat temporary identity loading as a new connection', () => {
    const initial = options({ balance: 0n })
    const view = renderHook(useDepositOnConnect, { initialProps: initial })
    view.rerender({ ...initial, identityKey: undefined, enabled: false })
    view.rerender(initial)
    expect(initial.onOpen).toHaveBeenCalledOnce()
  })
})
