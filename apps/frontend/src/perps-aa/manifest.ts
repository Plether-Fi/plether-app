import { getAddress, isAddress, type Address } from 'viem'

export const PERPS_AA_MANIFEST_V1_PATTERN =
  /^perps-aa-[a-z0-9]+(?:-[a-z0-9]+)*-v1$/
export const PERPS_AA_MANIFEST_V2_PATTERN =
  /^perps-aa-[a-z0-9]+(?:-[a-z0-9]+)*-v2$/
export const PERPS_AA_MANIFEST_SUPPORTED_PATTERN =
  /^perps-aa-[a-z0-9]+(?:-[a-z0-9]+)*-v(?:1|2)$/

export const PERPS_ENTRY_POINT_V08 =
  getAddress('0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108')
export const PERMISSIONLESS_SIMPLE_ACCOUNT_V08_FACTORY =
  getAddress('0x13E9ed32155810FDbd067D4522C492D6f68E5944')
export const PERPS_AA_LEGACY_RPC_PATH = '/api/perps/v1/aa/pimlico'
export const PERPS_AA_NATIVE_RPC_PATH = '/api/perps/v1/aa/rpc'

export type PerpsSmartAccountMode = 'simple'
export type PerpsEntryPointVersion = '0.8'
export type PerpsSmartAccountVersion = 'permissionless-simple-v0.8'
export type PerpsAaManifestV1Version = `perps-aa-${string}-v1`
export type PerpsAaManifestV2Version = `perps-aa-${string}-v2`
export type PerpsAaManifestVersion =
  | PerpsAaManifestV1Version
  | PerpsAaManifestV2Version
export type PerpsPaymasterVersion = 'plether-verifying-v1'

interface PerpsAaDeploymentManifestBase {
  chainId: number
  entryPoint: Address
  entryPointVersion: PerpsEntryPointVersion
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
  orderLifecycleBook: Address
  policyEvaluator: Address
  userOperationExplorerUrlTemplate: string
  transactionExplorerUrlTemplate: string
  testnetFaucet: string | null
  sponsorshipEnabled: boolean
}

// V1 names the legacy RPC schema, not the deployment generation. During the
// provider migration the current bounded-v2 deployment continues to use the
// reviewed Pimlico proxy until its manifest switches atomically to the native
// key set.
export interface PerpsAaDeploymentManifestV1
  extends PerpsAaDeploymentManifestBase {
  version: PerpsAaManifestVersion
  pimlicoRpcUrl: string
}

export interface PerpsAaDeploymentManifestV2
  extends PerpsAaDeploymentManifestBase {
  version: PerpsAaManifestV2Version
  bundlerRpcUrl: string
  paymasterRpcUrl: string
  paymasterAddress: Address
  paymasterVersion: PerpsPaymasterVersion
}

export type PerpsAaDeploymentManifest =
  | PerpsAaDeploymentManifestV1
  | PerpsAaDeploymentManifestV2

const COMMON_MANIFEST_KEYS = [
  'version',
  'chainId',
  'entryPoint',
  'entryPointVersion',
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
  'orderLifecycleBook',
  'policyEvaluator',
  'userOperationExplorerUrlTemplate',
  'transactionExplorerUrlTemplate',
  'testnetFaucet',
  'sponsorshipEnabled',
] as const
const MANIFEST_V1_KEYS = [
  ...COMMON_MANIFEST_KEYS,
  'pimlicoRpcUrl',
] as const
const MANIFEST_V2_KEYS = [
  ...COMMON_MANIFEST_KEYS,
  'bundlerRpcUrl',
  'paymasterRpcUrl',
  'paymasterAddress',
  'paymasterVersion',
] as const
const NATIVE_MANIFEST_FIELDS = [
  'bundlerRpcUrl',
  'paymasterRpcUrl',
  'paymasterAddress',
  'paymasterVersion',
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

function parseRpcUrl(
  value: unknown,
  field: string,
  expectedPath: string
): string {
  const stringValue = parseNonEmptyString(value, field)
  if (stringValue !== expectedPath) {
    invalid(
      field,
      `must equal the reviewed same-origin endpoint "${expectedPath}"`
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

function parsePaymasterVersion(value: unknown): PerpsPaymasterVersion {
  if (value !== 'plether-verifying-v1') {
    invalid('paymasterVersion', 'must be "plether-verifying-v1"')
  }
  return 'plether-verifying-v1'
}

export function isPerpsAaManifestV2(
  manifest: PerpsAaDeploymentManifest
): manifest is PerpsAaDeploymentManifestV2 {
  // The suffix identifies the bounded deployment generation, so the current
  // v2 deployment may still carry the legacy Pimlico schema. Native routing
  // is selected only by the parser-validated native field set.
  return 'bundlerRpcUrl' in manifest
}

export function bundlerRpcUrlForManifest(
  manifest: PerpsAaDeploymentManifest
): string {
  return isPerpsAaManifestV2(manifest)
    ? manifest.bundlerRpcUrl
    : manifest.pimlicoRpcUrl
}

export function paymasterRpcUrlForManifest(
  manifest: PerpsAaDeploymentManifest
): string {
  return isPerpsAaManifestV2(manifest)
    ? manifest.paymasterRpcUrl
    : manifest.pimlicoRpcUrl
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
  const isV1 = PERPS_AA_MANIFEST_V1_PATTERN.test(version)
  const isV2 = PERPS_AA_MANIFEST_V2_PATTERN.test(version)
  if (!isV1 && !isV2) {
    invalid(
      'version',
      'must identify a supported v1 or v2 manifest (perps-aa-<network>-v1|v2)'
    )
  }
  const hasNativeFields = NATIVE_MANIFEST_FIELDS.some(
    (field) => field in value
  )
  const usesNativeShape = isV2 && hasNativeFields
  assertExactKeys(
    value,
    usesNativeShape ? MANIFEST_V2_KEYS : MANIFEST_V1_KEYS
  )

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

  const commonManifest: PerpsAaDeploymentManifestBase = {
    chainId: parseChainId(value.chainId),
    entryPoint: parsePinnedAddress(
      value.entryPoint,
      'entryPoint',
      PERPS_ENTRY_POINT_V08
    ),
    entryPointVersion: parseEntryPointVersion(value.entryPointVersion),
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
    orderLifecycleBook: parseAddress(
      value.orderLifecycleBook,
      'orderLifecycleBook'
    ),
    policyEvaluator: parseAddress(
      value.policyEvaluator,
      'policyEvaluator'
    ),
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

  if (!usesNativeShape) {
    return {
      ...commonManifest,
      version: version as PerpsAaManifestVersion,
      pimlicoRpcUrl: parseRpcUrl(
        value.pimlicoRpcUrl,
        'pimlicoRpcUrl',
        PERPS_AA_LEGACY_RPC_PATH
      ),
    }
  }

  return {
    ...commonManifest,
    version: version as PerpsAaManifestV2Version,
    bundlerRpcUrl: parseRpcUrl(
      value.bundlerRpcUrl,
      'bundlerRpcUrl',
      PERPS_AA_NATIVE_RPC_PATH
    ),
    paymasterRpcUrl: parseRpcUrl(
      value.paymasterRpcUrl,
      'paymasterRpcUrl',
      PERPS_AA_NATIVE_RPC_PATH
    ),
    paymasterAddress: parseAddress(
      value.paymasterAddress,
      'paymasterAddress'
    ),
    paymasterVersion: parsePaymasterVersion(value.paymasterVersion),
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
