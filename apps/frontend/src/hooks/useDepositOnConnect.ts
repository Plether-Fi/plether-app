import { useEffect, useRef } from 'react'

/** Check each connected Trading Account once, after its balance is available. */
export function useDepositOnConnect({
  connected, identityKey, enabled, balance, depositOpen, canOpen, onOpen,
}: {
  connected: boolean
  identityKey?: string
  enabled: boolean
  balance?: bigint
  depositOpen: boolean
  canOpen: boolean
  onOpen: () => void
}) {
  const checked = useRef<{ identityKey: string; done: boolean } | undefined>(undefined)

  useEffect(() => {
    if (!connected) {
      checked.current = undefined
      return
    }
    if (!identityKey) return
    if (checked.current?.identityKey !== identityKey) checked.current = { identityKey, done: false }
    // A manually opened deposit also satisfies the prompt, even while loading.
    if (depositOpen) checked.current.done = true
    if (!enabled || checked.current.done || balance === undefined) return
    if (balance > 0n) {
      checked.current.done = true
    } else if (balance === 0n && canOpen) {
      checked.current.done = true
      onOpen()
    }
  }, [connected, identityKey, enabled, balance, depositOpen, canOpen, onOpen])
}
