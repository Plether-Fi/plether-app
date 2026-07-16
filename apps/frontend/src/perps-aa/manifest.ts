import { getAddress, isAddress, type Address, type Hex } from 'viem'

export const PERPS_AA_MANIFEST_V1_PATTERN =
  /^perps-aa-[a-z0-9]+(?:-[a-z0-9]+)*-v1$/

export type PerpsSmartAccountMode = 'separate-immutable' | 'eip-7702'

export interface PerpsAaDeploymentManifest {
  version: string
  chainId: number
  entryPoint: Address
  paymaster: Address
  policyId: Hex
  sponsorServiceRpcUrl: string
  bundlerRpcUrl: string
  smartAccountMode: PerpsSmartAccountMode
  smartAccountFactory: Address | null
  smartAccountImplementation: Address
  accountRuntimeCodeHash: Hex
  usdc: Address
  usdcSupportsEip3009: boolean
  usdcEip712Name: string | null
  usdcEip712Version: string | null
  marginClearinghouse: Address
  cfdEngine: Address
  orderRouter: Address
  userOperationExplorerUrlTemplate: string
  transactionExplorerUrlTemplate: string
  testnetFaucet: string | null
  sponsorshipEnabled: boolean
}

const MANIFEST_KEYS = [
  'version',
  'chainId',
  'entryPoint',
  'paymaster',
  'policyId',
  'sponsorServiceRpcUrl',
  'bundlerRpcUrl',
  'smartAccountMode',
  'smartAccountFactory',
  'smartAccountImplementation',
  'accountRuntimeCodeHash',
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

const ZERO_ADDRESS = `0x${'0'.repeat(40)}`
const ZERO_BYTES32 = `0x${'0'.repeat(64)}`

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

function assertExactKeys(record: Record<string, unknown>): void {
  const expected = new Set<string>(MANIFEST_KEYS)
  const unknownKeys = Object.keys(record).filter((key) => !expected.has(key))
  const missingKeys = MANIFEST_KEYS.filter((key) => !(key in record))
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
  field: string,
  options: { nullable: true }
): Address | null
function parseAddress(
  value: unknown,
  field: string,
  options?: { nullable?: false }
): Address
function parseAddress(
  value: unknown,
  field: string,
  options: { nullable?: boolean } = {}
): Address | null {
  if (value === null && options.nullable === true) return null
  if (typeof value !== 'string' || !isAddress(value)) {
    invalid(field, 'must be a valid checksummed or lowercase address')
  }
  if (value.toLowerCase() === ZERO_ADDRESS) {
    invalid(field, 'must not be the zero address')
  }
  return getAddress(value)
}

function parseBytes32(value: unknown, field: string): Hex {
  if (
    typeof value !== 'string' ||
    !/^0x[0-9a-fA-F]{64}$/.test(value) ||
    value.toLowerCase() === ZERO_BYTES32
  ) {
    invalid(field, 'must be a nonzero 32-byte hex value')
  }
  return value.toLowerCase() as Hex
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
  if (value !== 'separate-immutable' && value !== 'eip-7702') {
    invalid(
      'smartAccountMode',
      'must be "separate-immutable" or "eip-7702"'
    )
  }
  return value
}

export function parsePerpsAaManifest(
  value: unknown
): PerpsAaDeploymentManifest {
  if (!isRecord(value)) {
    throw new PerpsAaManifestValidationError([
      'manifest must be a JSON object',
    ])
  }
  assertExactKeys(value)

  const version = parseNonEmptyString(value.version, 'version')
  if (!PERPS_AA_MANIFEST_V1_PATTERN.test(version)) {
    invalid(
      'version',
      'must identify a supported v1 manifest (perps-aa-<network>-v1)'
    )
  }

  const smartAccountMode = parseAccountMode(value.smartAccountMode)
  const smartAccountFactory = parseAddress(
    value.smartAccountFactory,
    'smartAccountFactory',
    { nullable: true }
  )
  if (smartAccountMode === 'separate-immutable' && smartAccountFactory === null) {
    invalid(
      'smartAccountFactory',
      'is required for separate immutable accounts'
    )
  }
  if (smartAccountMode === 'eip-7702' && smartAccountFactory !== null) {
    invalid(
      'smartAccountFactory',
      'must be null for same-address EIP-7702 accounts'
    )
  }

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
    entryPoint: parseAddress(value.entryPoint, 'entryPoint'),
    paymaster: parseAddress(value.paymaster, 'paymaster'),
    policyId: parseBytes32(value.policyId, 'policyId'),
    sponsorServiceRpcUrl: parseWebUrl(
      value.sponsorServiceRpcUrl,
      'sponsorServiceRpcUrl'
    ),
    bundlerRpcUrl: parseWebUrl(value.bundlerRpcUrl, 'bundlerRpcUrl'),
    smartAccountMode,
    smartAccountFactory,
    smartAccountImplementation: parseAddress(
      value.smartAccountImplementation,
      'smartAccountImplementation'
    ),
    accountRuntimeCodeHash: parseBytes32(
      value.accountRuntimeCodeHash,
      'accountRuntimeCodeHash'
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
      cache: 'no-store',
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
