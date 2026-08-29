import { getAddress, isAddress, type Address } from 'viem'

export const PERPS_AA_MANIFEST_V1_PATTERN =
  /^perps-aa-[a-z0-9]+(?:-[a-z0-9]+)*-v1$/
export const PERPS_AA_MANIFEST_V2_PATTERN =
  /^perps-aa-[a-z0-9]+(?:-[a-z0-9]+)*-v2$/

export const PERPS_ENTRY_POINT_V08 =
  getAddress('0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108')
export const PERMISSIONLESS_SIMPLE_ACCOUNT_V08_FACTORY =
  getAddress('0x13E9ed32155810FDbd067D4522C492D6f68E5944')

export type PerpsSmartAccountMode = 'simple'
export type PerpsEntryPointVersion = '0.8'
export type PerpsSmartAccountVersion = 'permissionless-simple-v0.8'

export interface PerpsAaDeploymentManifest {
  version: string
  chainId: number
  entryPoint: Address
  entryPointVersion: PerpsEntryPointVersion
  pimlicoRpcUrl: string
  smartAccountMode: PerpsSmartAccountMode
  smartAccountVersion: PerpsSmartAccountVersion
  smartAccountIndex: string
  smartAccountFactory: Address
  usdc: Address
  usdcSupportsEip3009: boolean
  usdcEip712Name: string | null
  usdcEip712Version: string | null
  marginClearinghouse: Address
  cfdEngine: Address
  orderRouter: Address
  orderLifecycleBook?: Address
  policyEvaluator?: Address
  userOperationExplorerUrlTemplate: string
  transactionExplorerUrlTemplate: string
  testnetFaucet: string | null
  sponsorshipEnabled: boolean
}

const MANIFEST_KEYS = [
  'version',
  'chainId',
  'entryPoint',
  'entryPointVersion',
  'pimlicoRpcUrl',
  'smartAccountMode',
  'smartAccountVersion',
  'smartAccountIndex',
  'smartAccountFactory',
  'usdc',
  'usdcSupportsEip3009',
  'usdcEip712Name',
  'usdcEip712Version',
  'marginClearinghouse',
  'cfdEngine',
  'orderRouter',
  'userOperationExplorerUrlTemplate',
  'transactionExplorerUrlTemplate',
  'testnetFaucet',
  'sponsorshipEnabled',
] as const

const MANIFEST_V2_KEYS = [
  ...MANIFEST_KEYS,
  'orderLifecycleBook',
  'policyEvaluator',
] as const

const ZERO_ADDRESS = `0x${'0'.repeat(40)}`

export class PerpsAaManifestValidationError extends Error {
  readonly issues: readonly string[]

  constructor(issues: readonly string[]) {
    super(`Invalid Perps AA deployment manifest: ${issues.join('; ')}`)
    this.name = 'PerpsAaManifestValidationError'
    this.issues = issues
  }
}

export class PerpsAaManifestFetchError extends Error {
  constructor(message: string, options?: ErrorOptions) {
    super(message, options)
    this.name = 'PerpsAaManifestFetchError'
  }
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value)
}

function assertExactKeys(
  record: Record<string, unknown>,
  keys: readonly string[]
): void {
  const expected = new Set<string>(keys)
  const unknownKeys = Object.keys(record).filter((key) => !expected.has(key))
  const missingKeys = keys.filter((key) => !(key in record))
  const issues = [
    ...missingKeys.map((key) => `missing required field "${key}"`),
    ...unknownKeys.map((key) => `unknown field "${key}"`),
  ]

  if (issues.length > 0) {
    throw new PerpsAaManifestValidationError(issues)
  }
}

function invalid(field: string, expectation: string): never {
  throw new PerpsAaManifestValidationError([
    `"${field}" ${expectation}`,
  ])
}

function parseNonEmptyString(value: unknown, field: string): string {
  if (typeof value !== 'string' || value.trim() === '') {
    invalid(field, 'must be a non-empty string')
  }
  if (value !== value.trim()) {
    invalid(field, 'must not contain leading or trailing whitespace')
  }
  return value
}

function parseNullableNonEmptyString(value: unknown, field: string): string | null {
  if (value === null) return null
  return parseNonEmptyString(value, field)
}

function parseBoolean(value: unknown, field: string): boolean {
  if (typeof value !== 'boolean') {
    invalid(field, 'must be a boolean')
  }
  return value
}

function parseChainId(value: unknown): number {
  if (
    typeof value !== 'number' ||
    !Number.isSafeInteger(value) ||
    value <= 0
  ) {
    invalid('chainId', 'must be a positive safe integer')
  }
  return value
}

function parseAddress(
  value: unknown,
  field: string
): Address {
  if (typeof value !== 'string' || !isAddress(value)) {
    invalid(field, 'must be a valid checksummed or lowercase address')
  }
  if (value.toLowerCase() === ZERO_ADDRESS) {
    invalid(field, 'must not be the zero address')
  }
  return getAddress(value)
}

function parsePinnedAddress(
  value: unknown,
  field: string,
  expected: Address
): Address {
  const address = parseAddress(value, field)
  if (address !== expected) {
    invalid(field, `must be the reviewed deployment ${expected}`)
  }
  return address
}

function parseUnsignedDecimal(value: unknown, field: string): string {
  const stringValue = parseNonEmptyString(value, field)
  if (!/^(0|[1-9][0-9]*)$/.test(stringValue)) {
    invalid(field, 'must be a canonical unsigned decimal string')
  }
  try {
    const parsed = BigInt(stringValue)
    if (parsed >= 1n << 256n) {
      invalid(field, 'must fit uint256')
    }
  } catch {
    invalid(field, 'must fit uint256')
  }
  return stringValue
}

function isLocalHostname(hostname: string): boolean {
  return hostname === 'localhost' ||
    hostname === '127.0.0.1' ||
    hostname === '[::1]'
}

function parseWebUrl(value: unknown, field: string): string {
  const stringValue = parseNonEmptyString(value, field)
  let url: URL

  try {
    url = new URL(stringValue)
  } catch {
    invalid(field, 'must be an absolute HTTP(S) URL')
  }

  if (
    url.protocol !== 'https:' &&
    !(url.protocol === 'http:' && isLocalHostname(url.hostname))
  ) {
    invalid(field, 'must use HTTPS (HTTP is allowed only for localhost)')
  }
  if (url.username !== '' || url.password !== '') {
    invalid(field, 'must not embed credentials')
  }
  if (url.hash !== '') {
    invalid(field, 'must not include a URL fragment')
  }

  return stringValue
}

function parseRpcUrl(value: unknown, field: string): string {
  const stringValue = parseNonEmptyString(value, field)
  if (
    !stringValue.startsWith('/api/perps/v1/aa/') ||
    stringValue.startsWith('//') ||
    stringValue.includes('#') ||
    stringValue.includes('?')
  ) {
    invalid(
      field,
      'must be a same-origin /api/perps/v1/aa/ path without query parameters or fragments'
    )
  }
  return stringValue
}

function parseUrlTemplate(
  value: unknown,
  field: string,
  placeholder: string
): string {
  const stringValue = parseNonEmptyString(value, field)
  if (stringValue.split(placeholder).length !== 2) {
    invalid(field, `must contain exactly one "${placeholder}" placeholder`)
  }
  parseWebUrl(
    stringValue.replace(placeholder, `0x${'1'.repeat(64)}`),
    field
  )
  return stringValue
}

function parseAccountMode(value: unknown): PerpsSmartAccountMode {
  if (value !== 'simple') {
    invalid('smartAccountMode', 'must be "simple"')
  }
  return 'simple'
}

function parseEntryPointVersion(value: unknown): PerpsEntryPointVersion {
  if (value !== '0.8') {
    invalid('entryPointVersion', 'must be "0.8"')
  }
  return '0.8'
}

function parseSmartAccountVersion(
  value: unknown
): PerpsSmartAccountVersion {
  if (value !== 'permissionless-simple-v0.8') {
    invalid(
      'smartAccountVersion',
      'must be "permissionless-simple-v0.8"'
    )
  }
  return 'permissionless-simple-v0.8'
}

export function parsePerpsAaManifest(
  value: unknown
): PerpsAaDeploymentManifest {
  if (!isRecord(value)) {
    throw new PerpsAaManifestValidationError([
      'manifest must be a JSON object',
    ])
  }
  const version = parseNonEmptyString(value.version, 'version')
  const isV2 = PERPS_AA_MANIFEST_V2_PATTERN.test(version)
  if (!PERPS_AA_MANIFEST_V1_PATTERN.test(version) && !isV2) {
    invalid(
      'version',
      'must identify a supported manifest (perps-aa-<network>-v1 or -v2)'
    )
  }
  assertExactKeys(value, isV2 ? MANIFEST_V2_KEYS : MANIFEST_KEYS)

  const smartAccountMode = parseAccountMode(value.smartAccountMode)

  const usdcSupportsEip3009 = parseBoolean(
    value.usdcSupportsEip3009,
    'usdcSupportsEip3009'
  )
  const usdcEip712Name = parseNullableNonEmptyString(
    value.usdcEip712Name,
    'usdcEip712Name'
  )
  const usdcEip712Version = parseNullableNonEmptyString(
    value.usdcEip712Version,
    'usdcEip712Version'
  )
  if (
    usdcSupportsEip3009 &&
    (usdcEip712Name === null || usdcEip712Version === null)
  ) {
    invalid(
      'usdcSupportsEip3009',
      'requires both USDC EIP-712 name and version'
    )
  }
  if (
    !usdcSupportsEip3009 &&
    (usdcEip712Name !== null || usdcEip712Version !== null)
  ) {
    invalid(
      'usdcSupportsEip3009',
      'requires both USDC EIP-712 fields to be null when disabled'
    )
  }

  return {
    version,
    chainId: parseChainId(value.chainId),
    entryPoint: parsePinnedAddress(
      value.entryPoint,
      'entryPoint',
      PERPS_ENTRY_POINT_V08
    ),
    entryPointVersion: parseEntryPointVersion(value.entryPointVersion),
    pimlicoRpcUrl: parseRpcUrl(value.pimlicoRpcUrl, 'pimlicoRpcUrl'),
    smartAccountMode,
    smartAccountVersion: parseSmartAccountVersion(
      value.smartAccountVersion
    ),
    smartAccountIndex: (() => {
      const index = parseUnsignedDecimal(
        value.smartAccountIndex,
        'smartAccountIndex'
      )
      if (index !== '0') {
        invalid('smartAccountIndex', 'must be "0"')
      }
      return index
    })(),
    smartAccountFactory: parsePinnedAddress(
      value.smartAccountFactory,
      'smartAccountFactory',
      PERMISSIONLESS_SIMPLE_ACCOUNT_V08_FACTORY
    ),
    usdc: parseAddress(value.usdc, 'usdc'),
    usdcSupportsEip3009,
    usdcEip712Name,
    usdcEip712Version,
    marginClearinghouse: parseAddress(
      value.marginClearinghouse,
      'marginClearinghouse'
    ),
    cfdEngine: parseAddress(value.cfdEngine, 'cfdEngine'),
    orderRouter: parseAddress(value.orderRouter, 'orderRouter'),
    ...(isV2
      ? {
          orderLifecycleBook: parseAddress(
            value.orderLifecycleBook,
            'orderLifecycleBook'
          ),
          policyEvaluator: parseAddress(
            value.policyEvaluator,
            'policyEvaluator'
          ),
        }
      : {}),
    userOperationExplorerUrlTemplate: parseUrlTemplate(
      value.userOperationExplorerUrlTemplate,
      'userOperationExplorerUrlTemplate',
      '{userOperationHash}'
    ),
    transactionExplorerUrlTemplate: parseUrlTemplate(
      value.transactionExplorerUrlTemplate,
      'transactionExplorerUrlTemplate',
      '{transactionHash}'
    ),
    testnetFaucet: value.testnetFaucet === null
      ? null
      : parseWebUrl(value.testnetFaucet, 'testnetFaucet'),
    sponsorshipEnabled: parseBoolean(
      value.sponsorshipEnabled,
      'sponsorshipEnabled'
    ),
  }
}

export type PerpsAaManifestFetch = (
  input: RequestInfo | URL,
  init?: RequestInit
) => Promise<Response>

export async function fetchPerpsAaManifest(
  manifestUrl: string,
  options: {
    signal?: AbortSignal
    fetch?: PerpsAaManifestFetch
  } = {}
): Promise<PerpsAaDeploymentManifest> {
  const fetchManifest = options.fetch ?? globalThis.fetch
  if (typeof fetchManifest !== 'function') {
    throw new PerpsAaManifestFetchError(
      'Unable to load Perps AA manifest: fetch is unavailable'
    )
  }

  let response: Response
  try {
    response = await fetchManifest(manifestUrl, {
      // Revalidate on every continuity check, while allowing the browser to
      // turn ETag/Last-Modified matches into a bodyless conditional response.
      // Unlike the default cache mode, this never accepts a cached manifest
      // without first checking it with the origin.
      cache: 'no-cache',
      credentials: 'omit',
      signal: options.signal,
    })
  } catch (error) {
    throw new PerpsAaManifestFetchError(
      'Unable to load Perps AA deployment manifest',
      { cause: error }
    )
  }

  if (!response.ok) {
    throw new PerpsAaManifestFetchError(
      `Unable to load Perps AA deployment manifest (HTTP ${String(response.status)})`
    )
  }

  let json: unknown
  try {
    json = await response.json()
  } catch (error) {
    throw new PerpsAaManifestFetchError(
      'Perps AA deployment manifest is not valid JSON',
      { cause: error }
    )
  }

  return parsePerpsAaManifest(json)
}
