import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import type { Address, Hex } from 'viem'
import {
  migrateSponsoredOperationState,
  restoreSponsoredOperationLane,
  SPONSORED_OPERATION_LANE_HEAD_PREFIX,
  SPONSORED_OPERATION_STORAGE_NAME,
  SponsoredOperationLockedError,
  useSponsoredOperationStore,
  type SponsoredOperation,
} from '../operationStore'

const activeDeployment = vi.hoisted(() => ({ chainId: 421614 }))
vi.mock('../../contracts/perpsAddresses', async importOriginal => ({
  ...await importOriginal<typeof import('../../contracts/perpsAddresses')>(),
  get PERPS_CHAIN_ID() { return activeDeployment.chainId },
}))

const OWNER = '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address
const SEPOLIA = 421614
const MAINNET = 42161
const SEPOLIA_HASH = `0x${'12'.repeat(32)}` as Hex
const MAINNET_HASH = `0x${'34'.repeat(32)}` as Hex

function begin(id: string, chainId: number) {
  useSponsoredOperationStore.getState().beginOperation({
    id,
    ownerAddress: OWNER,
    accountAddress: ACCOUNT,
    chainId,
    accountMode: 'simple',
    manifestVersion: `release-${chainId}`,
    action: 'place-order',
  })
}

function pending(id: string, chainId: number, hash: Hex): SponsoredOperation {
  const now = Date.now()
  return {
    id,
    chainId,
    ownerAddress: OWNER,
    accountAddress: ACCOUNT,
    accountMode: 'simple',
    manifestVersion: `release-${chainId}`,
    action: 'place-order',
    lane: 'default',
    status: 'receipt-timeout',
    sponsorshipAccepted: true,
    userOperationHash: hash,
    retryCount: 0,
    createdAt: now,
    updatedAt: now,
    statusTimestamps: { 'receipt-timeout': now },
  }
}

describe('sponsored operation chain isolation', () => {
  beforeEach(() => {
    globalThis.localStorage.clear()
    activeDeployment.chainId = SEPOLIA
    useSponsoredOperationStore.setState({ operations: [], activeLanes: {} })
  })

  afterEach(() => {
    activeDeployment.chainId = SEPOLIA
    vi.restoreAllMocks()
  })

  it('allows the same account and lane on both chains while blocking duplicates only on their own chain', () => {
    begin('sepolia-pending', SEPOLIA)
    expect(useSponsoredOperationStore.getState().getActiveOperation(ACCOUNT, undefined, MAINNET))
      .toBeUndefined()
    expect(() => begin('mainnet-pending', MAINNET)).not.toThrow()
    const store = useSponsoredOperationStore.getState()

    expect(store.getActiveOperation(ACCOUNT)?.id).toBe('sepolia-pending')
    expect(store.getActiveOperation(ACCOUNT, undefined, MAINNET)?.id).toBe('mainnet-pending')
    expect(() => begin('sepolia-duplicate', SEPOLIA)).toThrow(SponsoredOperationLockedError)
    expect(() => begin('mainnet-duplicate', MAINNET)).toThrow(SponsoredOperationLockedError)

    activeDeployment.chainId = MAINNET
    expect(store.getActiveOperation(ACCOUNT)?.id).toBe('mainnet-pending')
    expect(store.getActiveOperation(ACCOUNT, undefined, SEPOLIA)?.id).toBe('sepolia-pending')
  })

  it('releases a resolved chain without releasing or masking the other chain', () => {
    begin('sepolia-pending', SEPOLIA)
    begin('mainnet-pending', MAINNET)
    const store = useSponsoredOperationStore.getState()
    store.transition('sepolia-pending', 'confirmed')

    expect(store.getActiveOperation(ACCOUNT, undefined, SEPOLIA)).toBeUndefined()
    expect(store.getActiveOperation(ACCOUNT, undefined, MAINNET)?.id).toBe('mainnet-pending')
    expect(() => begin('sepolia-next', SEPOLIA)).not.toThrow()
    expect(() => begin('mainnet-duplicate', MAINNET)).toThrow(SponsoredOperationLockedError)
    store.cleanupOperations()
    expect(store.getActiveOperation(ACCOUNT, undefined, SEPOLIA)?.id).toBe('sepolia-next')
    expect(store.getActiveOperation(ACCOUNT, undefined, MAINNET)?.id).toBe('mainnet-pending')
  })

  it('restores both pending locks from chain-specific durable journals and heads after reload', async () => {
    begin('sepolia-pending', SEPOLIA)
    begin('mainnet-pending', MAINNET)
    const store = useSponsoredOperationStore.getState()
    expect(store.recordUserOperationHash('sepolia-pending', SEPOLIA_HASH)).toBe(true)
    expect(store.recordUserOperationHash('mainnet-pending', MAINNET_HASH)).toBe(true)
    for (const id of ['sepolia-pending', 'mainnet-pending']) {
      store.failOperation({ id, status: 'receipt-timeout', reason: 'BUNDLER_UNAVAILABLE', retryable: false })
    }
    for (const chainId of [SEPOLIA, MAINNET]) {
      const key = `${SPONSORED_OPERATION_LANE_HEAD_PREFIX}${chainId}:${ACCOUNT.toLowerCase()}:default`
      expect(globalThis.localStorage.getItem(key)).not.toBeNull()
    }

    useSponsoredOperationStore.setState({ operations: [], activeLanes: {} })
    activeDeployment.chainId = MAINNET
    await useSponsoredOperationStore.persist.rehydrate()
    for (const chainId of [SEPOLIA, MAINNET]) {
      restoreSponsoredOperationLane({ chainId, accountAddress: ACCOUNT, lane: 'default' })
    }

    expect(store.getActiveOperation(ACCOUNT)?.id).toBe('mainnet-pending')
    expect(store.getActiveOperation(ACCOUNT, undefined, SEPOLIA)?.id).toBe('sepolia-pending')
    expect(() => begin('sepolia-duplicate', SEPOLIA)).toThrow(SponsoredOperationLockedError)
    expect(() => begin('mainnet-duplicate', MAINNET)).toThrow(SponsoredOperationLockedError)
  })

  it.each([0, 1, 2])('reconstructs both chains from a version-%i snapshot whose old lane map collapsed their identities', async version => {
    const snapshot = {
      operations: [
        pending('sepolia-legacy', SEPOLIA, SEPOLIA_HASH),
        pending('mainnet-legacy', MAINNET, MAINNET_HASH),
      ],
      activeLanes: { [`${ACCOUNT.toLowerCase()}:default`]: 'mainnet-legacy' },
    }
    const migrated = migrateSponsoredOperationState(snapshot, version)
    expect(migrated.activeLanes).toEqual({
      [`${ACCOUNT.toLowerCase()}:default`]: 'sepolia-legacy',
      [`${MAINNET}:${ACCOUNT.toLowerCase()}:default`]: 'mainnet-legacy',
    })
    globalThis.localStorage.setItem(SPONSORED_OPERATION_STORAGE_NAME, JSON.stringify({ state: snapshot, version }))
    activeDeployment.chainId = MAINNET
    await useSponsoredOperationStore.persist.rehydrate()

    const store = useSponsoredOperationStore.getState()
    expect(store.getActiveOperation(ACCOUNT)?.id).toBe('mainnet-legacy')
    expect(store.getActiveOperation(ACCOUNT, undefined, SEPOLIA)?.id).toBe('sepolia-legacy')
    expect(() => begin('mainnet-duplicate', MAINNET)).toThrow(SponsoredOperationLockedError)
    expect(() => begin('sepolia-duplicate', SEPOLIA)).toThrow(SponsoredOperationLockedError)
  })
})
