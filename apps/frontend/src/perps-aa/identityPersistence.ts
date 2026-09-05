import {
  getAddress,
  isAddress,
  isAddressEqual,
  type Address,
} from 'viem'
import {
  PERPS_AA_MANIFEST_SUPPORTED_PATTERN,
  type PerpsSmartAccountMode,
} from './manifest'

export const PERPS_IDENTITY_SCHEMA_VERSION = 2 as const
export const PERPS_IDENTITY_STORAGE_PREFIX = 'plether_perps_identity_v2'

export type PerpsAccountMode = PerpsSmartAccountMode

export interface PersistedPerpsIdentity {
  schemaVersion: typeof PERPS_IDENTITY_SCHEMA_VERSION
  chainId: number
  ownerAddress: Address
  accountAddress: Address
  accountMode: PerpsAccountMode
  entryPoint: Address
  entryPointVersion: '0.8'
  factoryAddress: Address
  accountVersion: string
  accountIndex: string
  manifestVersion: string
}

export type PerpsIdentityField = Exclude<
  keyof PersistedPerpsIdentity,
  'schemaVersion'
>

export type PerpsIdentityStorage = Pick<
  Storage,
  'getItem' | 'setItem' | 'removeItem'
>

export type ReadPersistedPerpsIdentityResult =
  | { status: 'missing' }
  | { status: 'found'; identity: PersistedPerpsIdentity }
  | { status: 'invalid'; error: Error }
  | { status: 'unavailable'; error: Error }

export type WritePersistedPerpsIdentityResult =
  | { ok: true }
  | { ok: false; error: Error }

const IDENTITY_KEYS = [
  'schemaVersion',
  'chainId',
  'ownerAddress',
  'accountAddress',
  'accountMode',
  'entryPoint',
  'entryPointVersion',
  'factoryAddress',
  'accountVersion',
  'accountIndex',
  'manifestVersion',
] as const

const ZERO_ADDRESS = `0x${'0'.repeat(40)}`

export class PerpsIdentityValidationError extends Error {
  constructor(message: string, options?: ErrorOptions) {
    super(message, options)
    this.name = 'PerpsIdentityValidationError'
  }
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value)
}

function assertExactKeys(record: Record<string, unknown>): void {
  const expectedKeys = new Set<string>(IDENTITY_KEYS)
  const unknownKeys = Object.keys(record).filter((key) => !expectedKeys.has(key))
  const missingKeys = IDENTITY_KEYS.filter((key) => !(key in record))

  if (missingKeys.length > 0 || unknownKeys.length > 0) {
    const details = [
      ...missingKeys.map((key) => `missing "${key}"`),
      ...unknownKeys.map((key) => `unknown "${key}"`),
    ].join(', ')
    throw new PerpsIdentityValidationError(
      `Invalid persisted Perps identity fields: ${details}`
    )
  }
}

function parseAddress(value: unknown, field: string): Address {
  if (
    typeof value !== 'string' ||
    !isAddress(value) ||
    value.toLowerCase() === ZERO_ADDRESS
  ) {
    throw new PerpsIdentityValidationError(
      `Persisted Perps identity "${field}" must be a nonzero address`
    )
  }
  return getAddress(value)
}

function parseNonEmptyString(value: unknown, field: string): string {
  if (
    typeof value !== 'string' ||
    value.trim() === '' ||
    value !== value.trim()
  ) {
    throw new PerpsIdentityValidationError(
      `Persisted Perps identity "${field}" must be a non-empty string`
    )
  }
  return value
}

function parseUnsignedDecimal(value: unknown, field: string): string {
  const stringValue = parseNonEmptyString(value, field)
  if (!/^(0|[1-9][0-9]*)$/.test(stringValue)) {
    throw new PerpsIdentityValidationError(
      `Persisted Perps identity "${field}" must be a canonical unsigned decimal string`
    )
  }
  try {
    if (BigInt(stringValue) >= 1n << 256n) {
      throw new Error('uint256 overflow')
    }
  } catch {
    throw new PerpsIdentityValidationError(
      `Persisted Perps identity "${field}" must fit uint256`
    )
  }
  return stringValue
}

function parseAccountMode(value: unknown): PerpsAccountMode {
  if (value !== 'simple') {
    throw new PerpsIdentityValidationError(
      'Persisted Perps identity has an unsupported account mode'
    )
  }
  return 'simple'
}

export function parsePersistedPerpsIdentity(
  value: unknown
): PersistedPerpsIdentity {
  if (!isRecord(value)) {
    throw new PerpsIdentityValidationError(
      'Persisted Perps identity must be an object'
    )
  }
  assertExactKeys(value)

  if (value.schemaVersion !== PERPS_IDENTITY_SCHEMA_VERSION) {
    throw new PerpsIdentityValidationError(
      'Persisted Perps identity schema version is unsupported'
    )
  }
  if (
    typeof value.chainId !== 'number' ||
    !Number.isSafeInteger(value.chainId) ||
    value.chainId <= 0
  ) {
    throw new PerpsIdentityValidationError(
      'Persisted Perps identity chainId must be a positive safe integer'
    )
  }

  const ownerAddress = parseAddress(value.ownerAddress, 'ownerAddress')
  const accountAddress = parseAddress(value.accountAddress, 'accountAddress')
  const accountMode = parseAccountMode(value.accountMode)
  const entryPoint = parseAddress(value.entryPoint, 'entryPoint')
  if (value.entryPointVersion !== '0.8') {
    throw new PerpsIdentityValidationError(
      'Persisted Perps identity entryPointVersion must be "0.8"'
    )
  }
  const factoryAddress = parseAddress(
    value.factoryAddress,
    'factoryAddress'
  )
  const accountVersion = parseNonEmptyString(
    value.accountVersion,
    'accountVersion'
  )
  const accountIndex = parseUnsignedDecimal(
    value.accountIndex,
    'accountIndex'
  )
  const manifestVersion = parseNonEmptyString(
    value.manifestVersion,
    'manifestVersion'
  )
  const isSameAddress = isAddressEqual(ownerAddress, accountAddress)

  if (!PERPS_AA_MANIFEST_SUPPORTED_PATTERN.test(manifestVersion)) {
    throw new PerpsIdentityValidationError(
      'Sponsored identity manifestVersion is unsupported'
    )
  }
  if (isSameAddress) {
    throw new PerpsIdentityValidationError(
      'Simple Trading Account address must differ from ownerAddress'
    )
  }

  return {
    schemaVersion: PERPS_IDENTITY_SCHEMA_VERSION,
    chainId: value.chainId,
    ownerAddress,
    accountAddress,
    accountMode,
    entryPoint,
    entryPointVersion: '0.8',
    factoryAddress,
    accountVersion,
    accountIndex,
    manifestVersion,
  }
}

export function createPersistedPerpsIdentity(
  input: Omit<PersistedPerpsIdentity, 'schemaVersion'>
): PersistedPerpsIdentity {
  return parsePersistedPerpsIdentity({
    schemaVersion: PERPS_IDENTITY_SCHEMA_VERSION,
    ...input,
  })
}

export function perpsIdentityStorageKey(
  chainId: number,
  ownerAddress: Address
): string {
  if (!Number.isSafeInteger(chainId) || chainId <= 0) {
    throw new PerpsIdentityValidationError(
      'Perps identity storage key requires a positive chainId'
    )
  }
  return `${PERPS_IDENTITY_STORAGE_PREFIX}:${String(chainId)}:${getAddress(ownerAddress).toLowerCase()}`
}

export function readPersistedPerpsIdentity(
  storage: PerpsIdentityStorage,
  chainId: number,
  ownerAddress: Address
): ReadPersistedPerpsIdentityResult {
  let serialized: string | null
  try {
    serialized = storage.getItem(
      perpsIdentityStorageKey(chainId, ownerAddress)
    )
  } catch (error) {
    return {
      status: 'unavailable',
      error: new PerpsIdentityValidationError(
        'Perps identity storage is unavailable',
        { cause: error }
      ),
    }
  }

  if (serialized === null) return { status: 'missing' }

  try {
    const identity = parsePersistedPerpsIdentity(
      JSON.parse(serialized) as unknown
    )
    if (
      identity.chainId !== chainId ||
      !isAddressEqual(identity.ownerAddress, ownerAddress)
    ) {
      throw new PerpsIdentityValidationError(
        'Persisted Perps identity does not match its storage key'
      )
    }
    return { status: 'found', identity }
  } catch (error) {
    return {
      status: 'invalid',
      error: error instanceof Error
        ? error
        : new PerpsIdentityValidationError(
            'Persisted Perps identity is invalid'
          ),
    }
  }
}

export function writePersistedPerpsIdentity(
  storage: PerpsIdentityStorage,
  identity: PersistedPerpsIdentity
): WritePersistedPerpsIdentityResult {
  let parsedIdentity: PersistedPerpsIdentity
  try {
    parsedIdentity = parsePersistedPerpsIdentity(identity)
    storage.setItem(
      perpsIdentityStorageKey(
        parsedIdentity.chainId,
        parsedIdentity.ownerAddress
      ),
      JSON.stringify(parsedIdentity)
    )
    return { ok: true }
  } catch (error) {
    return {
      ok: false,
      error: error instanceof Error
        ? error
        : new PerpsIdentityValidationError(
            'Unable to persist Perps identity'
          ),
    }
  }
}

export function removePersistedPerpsIdentity(
  storage: PerpsIdentityStorage,
  chainId: number,
  ownerAddress: Address
): WritePersistedPerpsIdentityResult {
  try {
    storage.removeItem(perpsIdentityStorageKey(chainId, ownerAddress))
    return { ok: true }
  } catch (error) {
    return {
      ok: false,
      error: new PerpsIdentityValidationError(
        'Unable to remove persisted Perps identity',
        { cause: error }
      ),
    }
  }
}

export function comparePerpsIdentities(
  persisted: PersistedPerpsIdentity,
  proposed: PersistedPerpsIdentity
): {
  matches: boolean
  changedFields: readonly PerpsIdentityField[]
} {
  const fields: readonly PerpsIdentityField[] = [
    'chainId',
    'ownerAddress',
    'accountAddress',
    'accountMode',
    'entryPoint',
    'entryPointVersion',
    'factoryAddress',
    'accountVersion',
    'accountIndex',
    'manifestVersion',
  ]
  const changedFields = fields.filter((field) => {
    const persistedValue = persisted[field]
    const proposedValue = proposed[field]
    if (
      typeof persistedValue === 'string' &&
      typeof proposedValue === 'string' &&
      isAddress(persistedValue) &&
      isAddress(proposedValue)
    ) {
      return !isAddressEqual(persistedValue, proposedValue)
    }
    return persistedValue !== proposedValue
  })

  return {
    matches: changedFields.length === 0,
    changedFields,
  }
}
