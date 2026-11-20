import { useCallback, useMemo, useSyncExternalStore } from 'react'
import type { Address } from 'viem'

export type GasPaymentMode = 'sponsored' | 'eth'

export type GasPaymentPreferenceScope = {
  chainId: number
  deployment: string
} & ({ ownerAddress: Address; sender?: Address } | { ownerAddress?: undefined; sender: Address })

export interface GasPaymentPreference {
  mode: GasPaymentMode
  revision: string
}

const defaultPreference: GasPaymentPreference = { mode: 'sponsored', revision: 'initial' }
const changeEvent = 'plether-gas-payment-preference-change'

function storageKeys(scope: GasPaymentPreferenceScope): { key: string; previousKey?: string } {
  const prefix = `plether-perps-gas-preference:v1:${scope.chainId.toString()}:${encodeURIComponent(scope.deployment)}`
  if (scope.ownerAddress === undefined) return { key: `${prefix}:${scope.sender.toLowerCase()}` }
  // The pinned Kernel profile derives one account per owner and deployment.
  // The wallet key works before derivation, with the prior sender key as a fallback.
  return {
    key: `${prefix}:owner:${scope.ownerAddress.toLowerCase()}`,
    previousKey: scope.sender === undefined ? undefined : `${prefix}:${scope.sender.toLowerCase()}`,
  }
}

function readStoredPreference(key: string, previousKey?: string): string | null {
  try {
    return window.localStorage.getItem(key) ?? (previousKey === undefined ? null : window.localStorage.getItem(previousKey))
  } catch {
    return null
  }
}

function parsePreference(stored: string | null): GasPaymentPreference {
  if (stored === null) return defaultPreference
  try {
    const value: unknown = JSON.parse(stored)
    if (typeof value === 'object' && value !== null &&
        'mode' in value && (value.mode === 'sponsored' || value.mode === 'eth') &&
        'revision' in value && typeof value.revision === 'string' && /^[0-9a-f-]{36}$/.test(value.revision)) {
      return { mode: value.mode, revision: value.revision }
    }
  } catch {
    return defaultPreference
  }
  return defaultPreference
}

export function readGasPaymentPreference(scope: GasPaymentPreferenceScope): GasPaymentPreference {
  const { key, previousKey } = storageKeys(scope)
  return parsePreference(readStoredPreference(key, previousKey))
}

export function writeGasPaymentPreference(scope: GasPaymentPreferenceScope, mode: GasPaymentMode): boolean {
  const { key } = storageKeys(scope)
  const stored = readStoredPreference(key)
  if (stored !== null && parsePreference(stored).mode === mode) return true
  try {
    const preference: GasPaymentPreference = { mode, revision: globalThis.crypto.randomUUID() }
    window.localStorage.setItem(key, JSON.stringify(preference))
  } catch {
    return false
  }
  window.dispatchEvent(new CustomEvent(changeEvent, { detail: key }))
  return true
}

export function useGasPaymentPreference(scope: GasPaymentPreferenceScope | undefined): GasPaymentPreference {
  const { key, previousKey } = scope ? storageKeys(scope) : {}
  const subscribe = useCallback((listener: () => void) => {
    if (key === undefined) return () => undefined
    const onStorage = (event: StorageEvent) => {
      if (event.key === null || event.key === key || event.key === previousKey) listener()
    }
    const onLocalChange = (event: Event) => {
      const detail: unknown = (event as CustomEvent<unknown>).detail
      if (detail === key || (previousKey !== undefined && detail === previousKey)) listener()
    }
    window.addEventListener('storage', onStorage)
    window.addEventListener(changeEvent, onLocalChange)
    return () => {
      window.removeEventListener('storage', onStorage)
      window.removeEventListener(changeEvent, onLocalChange)
    }
  }, [key, previousKey])
  const snapshot = useSyncExternalStore(subscribe,
    useCallback(() => key === undefined ? null : readStoredPreference(key, previousKey), [key, previousKey]),
    () => null)
  return useMemo(() => parsePreference(snapshot), [snapshot])
}
