import { describe, expect, it } from 'vitest'
import type { Address } from 'viem'
import {
  comparePerpsIdentities,
  createPersistedPerpsIdentity,
  perpsIdentityStorageKey,
  readPersistedPerpsIdentity,
  writePersistedPerpsIdentity,
  type PerpsIdentityStorage,
} from './identityPersistence'

const ownerAddress =
  '0x1111111111111111111111111111111111111111' as Address
const accountAddress =
  '0x2222222222222222222222222222222222222222' as Address

function memoryStorage(): PerpsIdentityStorage {
  const values = new Map<string, string>()
  return {
    getItem: (key) => values.get(key) ?? null,
    setItem: (key, value) => {
      values.set(key, value)
    },
    removeItem: (key) => {
      values.delete(key)
    },
  }
}

function sponsoredIdentity(version = 'permissionless-simple-v0.8') {
  return createPersistedPerpsIdentity({
    chainId: 421614,
    ownerAddress,
    accountAddress,
    accountMode: 'simple',
    entryPoint:
      '0x3333333333333333333333333333333333333333',
    entryPointVersion: '0.8',
    factoryAddress:
      '0x4444444444444444444444444444444444444444',
    accountVersion: version,
    accountIndex: '0',
    manifestVersion: 'perps-aa-arbitrum-sepolia-v1',
  })
}

describe('Perps identity persistence', () => {
  it('round-trips the complete deterministic Trading Account tuple', () => {
    const storage = memoryStorage()
    const identity = sponsoredIdentity()

    expect(writePersistedPerpsIdentity(storage, identity)).toEqual({ ok: true })
    expect(
      readPersistedPerpsIdentity(storage, identity.chainId, ownerAddress)
    ).toEqual({ status: 'found', identity })
  })

  it('fails closed on malformed stored identity JSON', () => {
    const storage = memoryStorage()
    storage.setItem(
      perpsIdentityStorageKey(421614, ownerAddress),
      '{"schemaVersion":1}'
    )

    expect(
      readPersistedPerpsIdentity(storage, 421614, ownerAddress)
    ).toMatchObject({ status: 'invalid' })
  })

  it('detects account-version continuity changes', () => {
    const comparison = comparePerpsIdentities(
      sponsoredIdentity('account-v1'),
      sponsoredIdentity('account-v2')
    )

    expect(comparison).toEqual({
      matches: false,
      changedFields: ['accountVersion'],
    })
  })
})
