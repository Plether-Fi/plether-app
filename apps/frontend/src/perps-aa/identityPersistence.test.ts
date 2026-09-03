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
    manifestVersion: 'perps-aa-arbitrum-sepolia-v2',
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

  it('round-trips an identity created from a supported v2 manifest', () => {
    const storage = memoryStorage()
    const identity = {
      ...sponsoredIdentity(),
      manifestVersion: 'perps-aa-arbitrum-sepolia-v2',
    }

    expect(writePersistedPerpsIdentity(storage, identity)).toEqual({ ok: true })
    expect(
      readPersistedPerpsIdentity(storage, identity.chainId, ownerAddress)
    ).toEqual({ status: 'found', identity })
  })

  it('rejects identities from unsupported future manifest versions', () => {
    const identity = { ...sponsoredIdentity() }
    delete (identity as Partial<typeof identity>).schemaVersion
    expect(() => createPersistedPerpsIdentity({
      ...identity,
      manifestVersion: 'perps-aa-arbitrum-sepolia-v3',
    } as Omit<typeof identity, 'schemaVersion'>)).toThrow(
      /manifestVersion is unsupported/
    )
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

  it('reads a structurally valid V1 identity for V2 continuity migration', () => {
    const storage = memoryStorage()
    const identity = {
      ...sponsoredIdentity(),
      manifestVersion: 'perps-aa-arbitrum-sepolia-20260826-v1',
    }
    storage.setItem(
      perpsIdentityStorageKey(identity.chainId, ownerAddress),
      JSON.stringify(identity)
    )

    expect(
      readPersistedPerpsIdentity(storage, identity.chainId, ownerAddress)
    ).toEqual({ status: 'found', identity })
  })

  it('still rejects an identity from an unrecognized manifest generation', () => {
    const storage = memoryStorage()
    const identity = {
      ...sponsoredIdentity(),
      manifestVersion: 'perps-aa-arbitrum-sepolia-20260826-v3',
    }
    storage.setItem(
      perpsIdentityStorageKey(identity.chainId, ownerAddress),
      JSON.stringify(identity)
    )

    expect(
      readPersistedPerpsIdentity(storage, identity.chainId, ownerAddress)
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
