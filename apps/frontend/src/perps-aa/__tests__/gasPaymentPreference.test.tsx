import { act, renderHook } from '@testing-library/react'
import { beforeEach, describe, expect, it } from 'vitest'
import {
  readGasPaymentPreference,
  useGasPaymentPreference,
  writeGasPaymentPreference,
  type GasPaymentPreferenceScope,
} from '../gasPaymentPreference'

const scope: GasPaymentPreferenceScope = {
  chainId: 421614,
  deployment: 'kernel-canary',
  sender: '0x2222222222222222222222222222222222222222',
}

describe('Trading Account gas payment preference', () => {
  beforeEach(() => window.localStorage.clear())

  it('scopes an ETH choice to the specific deployment, chain and account', () => {
    expect(readGasPaymentPreference(scope).mode).toBe('sponsored')
    expect(writeGasPaymentPreference(scope, 'eth')).toBe(true)
    expect(readGasPaymentPreference(scope).mode).toBe('eth')
    expect(readGasPaymentPreference({ ...scope, chainId: 1 }).mode).toBe('sponsored')
    expect(readGasPaymentPreference({ ...scope, deployment: 'another-release' }).mode).toBe('sponsored')
    expect(readGasPaymentPreference({ ...scope, sender: '0x3333333333333333333333333333333333333333' }).mode).toBe('sponsored')
  })

  it('observes other tabs and invalidates a review even when the setting switches back', () => {
    const { result } = renderHook(() => useGasPaymentPreference(scope))
    const initial = result.current
    act(() => { writeGasPaymentPreference(scope, 'eth') })
    expect(result.current.mode).toBe('eth')
    const key = window.localStorage.key(0)
    const newValue = JSON.stringify({ mode: 'sponsored', revision: crypto.randomUUID() })
    if (key === null) throw new Error('Gas preference was not persisted')
    act(() => {
      window.localStorage.setItem(key, newValue)
      window.dispatchEvent(new StorageEvent('storage', { key, newValue, storageArea: window.localStorage }))
    })
    expect(result.current.mode).toBe('sponsored')
    expect(result.current.revision).not.toBe(initial.revision)
  })

  it('honors a choice made before derivation and keeps a previously saved account choice', () => {
    const owner = { chainId: scope.chainId, deployment: scope.deployment, ownerAddress: '0x1111111111111111111111111111111111111111' } as const
    const resolved = { ...owner, sender: scope.sender }
    writeGasPaymentPreference(scope, 'eth')
    expect(readGasPaymentPreference(resolved).mode).toBe('eth')

    writeGasPaymentPreference(owner, 'sponsored')
    expect(readGasPaymentPreference(resolved)).toEqual(readGasPaymentPreference(owner))
    expect(readGasPaymentPreference(resolved).mode).toBe('sponsored')
    writeGasPaymentPreference(owner, 'eth')
    expect(readGasPaymentPreference(resolved).mode).toBe('eth')
    expect(readGasPaymentPreference({ ...owner, ownerAddress: '0x4444444444444444444444444444444444444444' }).mode).toBe('sponsored')
  })
})
