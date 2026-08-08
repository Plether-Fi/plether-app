import {
  createAuthorizationNonce,
  type ReceiveWithAuthorization,
} from '@plether/perps-aa-client'
import { getAddress, type Address, type Hex } from 'viem'

const AUTHORIZATION_STORAGE_PREFIX = 'plether_perps_eip3009_v2'
const LEGACY_AUTHORIZATION_STORAGE_PREFIX = 'plether_perps_eip3009_v1'
const DEFAULT_VALIDITY_SECONDS = 10 * 60
const CLOCK_SKEW_SECONDS = 30

interface PersistedAuthorization {
  from: Address
  to: Address
  token: Address
  chainId: number
  value: string
  validAfter: string
  validBefore: string
  nonce: Hex
}

type AuthorizationStorage = Pick<Storage, 'setItem' | 'removeItem'>

function storageKey(input: {
  chainId: number
  ownerAddress: Address
  accountAddress: Address
  token: Address
}, prefix = AUTHORIZATION_STORAGE_PREFIX): string {
  return [
    prefix,
    input.chainId.toString(),
    getAddress(input.ownerAddress).toLowerCase(),
    getAddress(input.accountAddress).toLowerCase(),
    getAddress(input.token).toLowerCase(),
  ].join(':')
}

function consumedAuthorizationKey(input: {
  chainId: number
  ownerAddress: Address
  accountAddress: Address
  token: Address
}): string {
  // One bounded marker per account/token. Every invocation already mints a
  // fresh nonce, so retaining an unbounded key per successful deposit would
  // only create a localStorage quota failure over time.
  return `${storageKey(input)}:consumed`
}

export function getOrCreateDepositAuthorization(input: {
  chainId: number
  ownerAddress: Address
  accountAddress: Address
  token: Address
  amount: bigint
  nowSeconds?: bigint
  validitySeconds?: number
  storage?: AuthorizationStorage
}): ReceiveWithAuthorization {
  const storage = input.storage ?? globalThis.localStorage
  const key = storageKey(input)
  const nowSeconds = input.nowSeconds ?? BigInt(Math.floor(Date.now() / 1000))

  // Mint per invocation. The authorization is created before the sponsored
  // submission lane is acquired, so reusing a singleton here could hand a
  // later attempt a nonce that an earlier onchain operation already consumed.
  const authorization: ReceiveWithAuthorization = {
    from: getAddress(input.ownerAddress),
    to: getAddress(input.accountAddress),
    value: input.amount,
    validAfter: nowSeconds > BigInt(CLOCK_SKEW_SECONDS)
      ? nowSeconds - BigInt(CLOCK_SKEW_SECONDS)
      : 0n,
    validBefore:
      nowSeconds + BigInt(input.validitySeconds ?? DEFAULT_VALIDITY_SECONDS),
    nonce: createAuthorizationNonce(),
  }
  const persisted: PersistedAuthorization = {
    ...authorization,
    token: getAddress(input.token),
    chainId: input.chainId,
    value: authorization.value.toString(),
    validAfter: authorization.validAfter.toString(),
    validBefore: authorization.validBefore.toString(),
  }
  storage.setItem(key, JSON.stringify(persisted))
  return authorization
}

export function clearDepositAuthorization(input: {
  chainId: number
  ownerAddress: Address
  accountAddress: Address
  token: Address
  /**
   * When supplied, clear only the authorization that owns this nonce. This
   * prevents a late completion for an older deposit from deleting a newer
   * authorization cached under the same account/token key.
   *
   * Omit this only for an explicit user abandonment, which intentionally
   * clears whichever authorization is currently cached.
   */
  expectedNonce?: Hex
  storage?: AuthorizationStorage
}): boolean {
  const storage = input.storage ?? globalThis.localStorage
  const key = storageKey(input)

  if (input.expectedNonce !== undefined) {
    // Never compare then remove the v2 singleton: another invocation can
    // replace it between those calls. A bounded consumed-nonce marker records
    // progress without being able to erase a newer authorization.
    storage.setItem(
      consumedAuthorizationKey(input),
      input.expectedNonce.toLowerCase()
    )
    return true
  }

  // Explicit abandonment intentionally clears whichever authorization is
  // current, including a cache left by the prior app version.
  storage.removeItem(key)
  storage.removeItem(storageKey(input, LEGACY_AUTHORIZATION_STORAGE_PREFIX))
  return true
}

/**
 * Retires only the reuse-based v1 cache after a legacy operation reaches the
 * safe head. A v2 nonce-owned cleanup must never call this: an old tab may
 * already have stored a newer v1 authorization before it acquires the shared
 * submission lane.
 */
export function clearLegacyDepositAuthorization(input: {
  chainId: number
  ownerAddress: Address
  accountAddress: Address
  token: Address
  storage?: AuthorizationStorage
}): void {
  const storage = input.storage ?? globalThis.localStorage
  storage.removeItem(storageKey(input, LEGACY_AUTHORIZATION_STORAGE_PREFIX))
}
