import { describe, expect, it } from 'vitest'
import type { Address } from 'viem'
import {
  clearDepositAuthorization,
  clearLegacyDepositAuthorization,
  getOrCreateDepositAuthorization,
} from './authorizationStore'

const OWNER = '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address
const TOKEN = '0x3333333333333333333333333333333333333333' as Address
const STORAGE_SUFFIX =
  `421614:${OWNER.toLowerCase()}:${ACCOUNT.toLowerCase()}:${TOKEN.toLowerCase()}`
const LEGACY_KEY = `plether_perps_eip3009_v1:${STORAGE_SUFFIX}`

function memoryStorage() {
  const values = new Map<string, string>()
  const calls: string[] = []
  return {
    values,
    calls,
    getItem: (key: string) => values.get(key) ?? null,
    setItem: (key: string, value: string) => {
      calls.push(`set:${key}`)
      values.set(key, value)
    },
    removeItem: (key: string) => {
      calls.push(`remove:${key}`)
      values.delete(key)
    },
  }
}

describe('EIP-3009 deposit authorization persistence', () => {
  it('mints a fresh nonce for every deposit invocation', () => {
    const storage = memoryStorage()
    const first = getOrCreateDepositAuthorization({
      chainId: 421614,
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      token: TOKEN,
      amount: 10_000_000n,
      nowSeconds: 1_000n,
      storage,
    })
    const second = getOrCreateDepositAuthorization({
      chainId: 421614,
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      token: TOKEN,
      amount: 10_000_000n,
      nowSeconds: 1_100n,
      storage,
    })

    expect(second.nonce).not.toBe(first.nonce)

    clearDepositAuthorization({
      chainId: 421614,
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      token: TOKEN,
      storage,
    })
    expect([...storage.values.keys()].filter((key) =>
      key.startsWith('plether_perps_eip3009_v2:') &&
      !key.endsWith(':consumed')
    )).toEqual([])

    const replacement = getOrCreateDepositAuthorization({
      chainId: 421614,
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      token: TOKEN,
      amount: 10_000_000n,
      nowSeconds: 1_100n,
      storage,
    })
    expect(replacement.nonce).not.toBe(first.nonce)
    expect(replacement.nonce).not.toBe(second.nonce)
  })

  it('tombstones an older nonce without erasing a newer authorization', () => {
    const storage = memoryStorage()
    const first = getOrCreateDepositAuthorization({
      chainId: 421614,
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      token: TOKEN,
      amount: 10_000_000n,
      nowSeconds: 1_000n,
      storage,
    })

    const second = getOrCreateDepositAuthorization({
      chainId: 421614,
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      token: TOKEN,
      amount: 10_000_000n,
      nowSeconds: 1_100n,
      storage,
    })
    expect(second.nonce).not.toBe(first.nonce)
    storage.setItem(LEGACY_KEY, JSON.stringify({ nonce: second.nonce }))

    expect(clearDepositAuthorization({
      chainId: 421614,
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      token: TOKEN,
      expectedNonce: first.nonce,
      storage,
    })).toBe(true)

    const currentEntry = [...storage.values.entries()].find(([key]) =>
      key.startsWith('plether_perps_eip3009_v2:') &&
      !key.endsWith(':consumed')
    )
    expect(currentEntry).toBeDefined()
    expect(JSON.parse(currentEntry?.[1] ?? '{}')).toMatchObject({
      nonce: second.nonce,
    })
    const consumedEntry = [...storage.values.entries()].find(([key]) =>
      key.endsWith(':consumed')
    )
    expect(consumedEntry?.[1]).toBe(first.nonce.toLowerCase())
    expect(storage.values.has(LEGACY_KEY)).toBe(true)
    const tombstoneWriteIndex = storage.calls.findIndex((call) =>
      call.startsWith('set:plether_perps_eip3009_v2:') &&
      call.endsWith(':consumed')
    )
    expect(tombstoneWriteIndex).toBeGreaterThanOrEqual(0)

    const third = getOrCreateDepositAuthorization({
      chainId: 421614,
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      token: TOKEN,
      amount: 10_000_000n,
      nowSeconds: 1_200n,
      storage,
    })
    expect(third.nonce).not.toBe(first.nonce)
    expect(third.nonce).not.toBe(second.nonce)
  })

  it('retires only the legacy cache after safe legacy confirmation', () => {
    const storage = memoryStorage()
    const current = getOrCreateDepositAuthorization({
      chainId: 421614,
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      token: TOKEN,
      amount: 10_000_000n,
      nowSeconds: 1_000n,
      storage,
    })
    storage.setItem(LEGACY_KEY, JSON.stringify({ nonce: '0xlegacy' }))

    clearLegacyDepositAuthorization({
      chainId: 421614,
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      token: TOKEN,
      storage,
    })

    expect(storage.values.has(LEGACY_KEY)).toBe(false)
    const currentEntry = [...storage.values.entries()].find(([key]) =>
      key.startsWith('plether_perps_eip3009_v2:') &&
      !key.endsWith(':consumed')
    )
    expect(JSON.parse(currentEntry?.[1] ?? '{}')).toMatchObject({
      nonce: current.nonce,
    })
  })

  it('keeps the consumed marker bounded to one key', () => {
    const storage = memoryStorage()
    const firstNonce = `0x${'11'.repeat(32)}` as const
    const secondNonce = `0x${'22'.repeat(32)}` as const

    for (const expectedNonce of [firstNonce, secondNonce]) {
      clearDepositAuthorization({
        chainId: 421614,
        ownerAddress: OWNER,
        accountAddress: ACCOUNT,
        token: TOKEN,
        expectedNonce,
        storage,
      })
    }

    const consumedEntries = [...storage.values.entries()].filter(([key]) =>
      key.endsWith(':consumed')
    )
    expect(consumedEntries).toHaveLength(1)
    expect(consumedEntries[0]?.[1]).toBe(secondNonce)
  })
})
