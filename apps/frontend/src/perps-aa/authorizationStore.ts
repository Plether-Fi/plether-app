import {
  createAuthorizationNonce,
  type ReceiveWithAuthorization,
} from '@plether/perps-aa-client'
import { getAddress, isAddressEqual, type Address, type Hex } from 'viem'

const AUTHORIZATION_STORAGE_PREFIX = 'plether_perps_eip3009_v1'
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

type AuthorizationStorage = Pick<Storage, 'getItem' | 'setItem' | 'removeItem'>

function storageKey(input: {
  chainId: number
  ownerAddress: Address
  accountAddress: Address
  token: Address
}): string {
  return [
    AUTHORIZATION_STORAGE_PREFIX,
    input.chainId.toString(),
    getAddress(input.ownerAddress).toLowerCase(),
    getAddress(input.accountAddress).toLowerCase(),
    getAddress(input.token).toLowerCase(),
  ].join(':')
}

function parsePersistedAuthorization(
  serialized: string
): PersistedAuthorization | undefined {
  try {
    const value = JSON.parse(serialized) as Partial<PersistedAuthorization>
    if (
      typeof value.from !== 'string' ||
      typeof value.to !== 'string' ||
      typeof value.token !== 'string' ||
      typeof value.chainId !== 'number' ||
      typeof value.value !== 'string' ||
      typeof value.validAfter !== 'string' ||
      typeof value.validBefore !== 'string' ||
      typeof value.nonce !== 'string'
    ) {
      return undefined
    }
    return {
      from: getAddress(value.from),
      to: getAddress(value.to),
      token: getAddress(value.token),
      chainId: value.chainId,
      value: value.value,
      validAfter: value.validAfter,
      validBefore: value.validBefore,
      nonce: value.nonce,
    }
  } catch {
    return undefined
  }
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
  const serialized = storage.getItem(key)
  const existing = serialized
    ? parsePersistedAuthorization(serialized)
    : undefined

  if (
    existing?.chainId === input.chainId &&
    isAddressEqual(existing.from, input.ownerAddress) &&
    isAddressEqual(existing.to, input.accountAddress) &&
    isAddressEqual(existing.token, input.token) &&
    existing.value === input.amount.toString() &&
    BigInt(existing.validBefore) > nowSeconds + BigInt(CLOCK_SKEW_SECONDS)
  ) {
    return {
      from: existing.from,
      to: existing.to,
      value: BigInt(existing.value),
      validAfter: BigInt(existing.validAfter),
      validBefore: BigInt(existing.validBefore),
      nonce: existing.nonce,
    }
  }

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
  storage?: AuthorizationStorage
}): void {
  const storage = input.storage ?? globalThis.localStorage
  storage.removeItem(storageKey(input))
}
