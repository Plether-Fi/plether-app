import { describe, expect, it } from 'vitest'
import type { Address } from 'viem'
import {
  clearDepositAuthorization,
  getOrCreateDepositAuthorization,
} from './authorizationStore'

const OWNER = '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address
const TOKEN = '0x3333333333333333333333333333333333333333' as Address

function memoryStorage() {
  const values = new Map<string, string>()
  return {
    getItem: (key: string) => values.get(key) ?? null,
    setItem: (key: string, value: string) => {
      values.set(key, value)
    },
    removeItem: (key: string) => {
      values.delete(key)
    },
  }
}

describe('EIP-3009 deposit authorization persistence', () => {
  it('reuses the nonce until inclusion, abandonment, or expiry', () => {
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

    expect(second.nonce).toBe(first.nonce)

    clearDepositAuthorization({
      chainId: 421614,
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      token: TOKEN,
      storage,
    })
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
  })
})
