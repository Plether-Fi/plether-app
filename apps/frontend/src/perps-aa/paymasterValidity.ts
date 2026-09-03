import {
  normalizePaymasterResponse,
  PLETHER_PAYMASTER_DATA_BYTES,
  validatePletherPaymasterEnvelope,
  type PletherPaymasterProfile,
} from '@plether/perps-aa-client'
import {
  getAddress,
  hexToBigInt,
  hexToNumber,
  isAddress,
  isAddressEqual,
  size,
  slice,
  type Address,
  type Hex,
} from 'viem'
import {
  isPerpsAaManifestV2 as isNativePaymasterManifest,
  PERPS_ENTRY_POINT_V08,
  type PerpsAaDeploymentManifest,
} from './manifest'
import type { ManagedUserOperation } from './runtimeContext'

export const PIMLICO_SINGLETON_PAYMASTER_V8 = getAddress(
  '0x888888888888Ec68A58AB8094Cc1AD20Ba3D2402'
)
export { PLETHER_PAYMASTER_DATA_BYTES }
export const PLETHER_PAYMASTER_POLICY_ID =
  '0x8dd77324b94da492342191f762a32cdf99e828a7f24d77c8ed5ace90cf4f5ae3' as Hex
export const PLETHER_SIMPLE_ACCOUNT_PROXY_CODE_HASH =
  '0x41ee894da413cc99e8dec0a1784470eceb736845ad1591e06ff0ecdf0aca26c9' as Hex
export const PLETHER_PAYMASTER_VERIFICATION_GAS_LIMIT = 100_000n
export const PLETHER_PAYMASTER_POST_OP_GAS_LIMIT = 0n
export const PLETHER_PAYMASTER_MAX_VALIDITY_WINDOW_SECONDS = 600n
export const PLETHER_PAYMASTER_CHAIN_ID = 421614
const VERIFYING_MODE = 0
const MODE_AND_BUNDLER_FLAG_BYTES = 1
const VALIDITY_TIMESTAMP_BYTES = 6
const VERIFYING_CONFIG_BYTES = VALIDITY_TIMESTAMP_BYTES * 2
const MIN_SIGNATURE_BYTES = 64
const MAX_SIGNATURE_BYTES = 65
const UINT48_MAX = (1n << 48n) - 1n
const SPONSORSHIP_AUTHORITY_KEYS = [
  'version',
  'paymasterAddress',
  'validUntil',
] as const

export interface PersistedSponsorshipAuthorityV1 {
  version: 1
  paymasterAddress: Address
  /** Canonical unsigned decimal uint48. */
  validUntil: string
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value)
}

/**
 * Normalizes the native-paymaster authority recorded beside a hash-bound
 * UserOperation. The nested version lets recovery reject future formats
 * without confusing them with pre-authority journals.
 */
export function createSponsorshipAuthority(input: {
  paymasterAddress: Address
  validUntil: bigint
}): PersistedSponsorshipAuthorityV1 {
  if (
    !isAddress(input.paymasterAddress, { strict: true }) ||
    input.paymasterAddress.toLowerCase() === `0x${'0'.repeat(40)}` ||
    input.validUntil <= 0n ||
    input.validUntil > UINT48_MAX
  ) {
    throw new Error('Invalid Plether sponsorship recovery authority')
  }
  return {
    version: 1,
    paymasterAddress: getAddress(input.paymasterAddress),
    validUntil: input.validUntil.toString(),
  }
}

export function parseSponsorshipAuthority(
  value: unknown
): PersistedSponsorshipAuthorityV1 | undefined {
  if (!isRecord(value)) return undefined
  const expectedKeys = new Set<string>(SPONSORSHIP_AUTHORITY_KEYS)
  if (
    Object.keys(value).length !== SPONSORSHIP_AUTHORITY_KEYS.length ||
    Object.keys(value).some((key) => !expectedKeys.has(key)) ||
    value.version !== 1 ||
    typeof value.paymasterAddress !== 'string' ||
    !isAddress(value.paymasterAddress, { strict: true }) ||
    value.paymasterAddress.toLowerCase() === `0x${'0'.repeat(40)}` ||
    typeof value.validUntil !== 'string' ||
    !/^[1-9][0-9]*$/.test(value.validUntil)
  ) {
    return undefined
  }

  try {
    const validUntil = BigInt(value.validUntil)
    if (validUntil > UINT48_MAX) return undefined
    return {
      version: 1,
      paymasterAddress: getAddress(value.paymasterAddress),
      validUntil: value.validUntil,
    }
  } catch {
    return undefined
  }
}

/**
 * Parses the validity deadline from Pimlico SingletonPaymaster verifying-mode
 * data. Unknown formats fail closed by returning undefined, so recovery never
 * releases a lane based on a guessed expiry.
 *
 * paymasterData:
 * mode/allowAllBundlers(1) | validUntil(6) | validAfter(6) | signature(64/65)
 */
export function pimlicoSponsorshipValidUntil(
  paymaster: Address | undefined,
  paymasterData: Hex | undefined
): bigint | undefined {
  if (
    !paymaster ||
    !isAddressEqual(paymaster, PIMLICO_SINGLETON_PAYMASTER_V8) ||
    !paymasterData
  ) {
    return undefined
  }

  try {
    const dataBytes = size(paymasterData)
    const configOffset = MODE_AND_BUNDLER_FLAG_BYTES
    const signatureBytes =
      dataBytes - configOffset - VERIFYING_CONFIG_BYTES
    if (
      signatureBytes < MIN_SIGNATURE_BYTES ||
      signatureBytes > MAX_SIGNATURE_BYTES
    ) {
      return undefined
    }

    const combinedModeByte = hexToNumber(slice(paymasterData, 0, 1))
    const mode = combinedModeByte >> 1
    if (mode !== VERIFYING_MODE) return undefined

    const validUntil = hexToBigInt(
      slice(
        paymasterData,
        configOffset,
        configOffset + VALIDITY_TIMESTAMP_BYTES
      )
    )

    // ERC-4337 treats zero as an unbounded validity window. Such an operation
    // must remain locked until another authoritative signal resolves it.
    return validUntil === 0n ? undefined : validUntil
  } catch {
    return undefined
  }
}

/**
 * Parses the reviewed Plether verifying-paymaster v1 envelope. The expected
 * paymaster comes from the validated deployment manifest; arbitrary
 * paymasters with a coincidentally similar payload are never trusted.
 */
export function pletherSponsorshipValidUntil(
  expectedPaymaster: Address,
  operation: Pick<
    ManagedUserOperation,
    | 'paymaster'
    | 'paymasterData'
    | 'paymasterVerificationGasLimit'
    | 'paymasterPostOpGasLimit'
  >
): bigint | undefined {
  if (
    !operation.paymaster ||
    !isAddressEqual(operation.paymaster, expectedPaymaster) ||
    !operation.paymasterData ||
    operation.paymasterVerificationGasLimit === undefined ||
    operation.paymasterPostOpGasLimit === undefined ||
    size(operation.paymasterData) !== PLETHER_PAYMASTER_DATA_BYTES
  ) {
    return undefined
  }

  try {
    // The contract address is deployment-specific, while the rest of the
    // initial Sepolia profile is reviewed and pinned across the core contract,
    // backend startup attestation, Terraform guards, and this client.
    const profile: PletherPaymasterProfile = {
      chainId: PLETHER_PAYMASTER_CHAIN_ID,
      entryPoint: PERPS_ENTRY_POINT_V08,
      paymaster: expectedPaymaster,
      policyId: PLETHER_PAYMASTER_POLICY_ID,
      accountCodeHash: PLETHER_SIMPLE_ACCOUNT_PROXY_CODE_HASH,
      paymasterVerificationGasLimit:
        PLETHER_PAYMASTER_VERIFICATION_GAS_LIMIT,
      paymasterPostOpGasLimit: PLETHER_PAYMASTER_POST_OP_GAS_LIMIT,
      maxValidityWindowSeconds:
        PLETHER_PAYMASTER_MAX_VALIDITY_WINDOW_SECONDS,
    }
    const envelope = validatePletherPaymasterEnvelope(
      normalizePaymasterResponse({
        paymaster: operation.paymaster,
        paymasterData: operation.paymasterData,
        paymasterVerificationGasLimit:
          operation.paymasterVerificationGasLimit,
        paymasterPostOpGasLimit: operation.paymasterPostOpGasLimit,
      }),
      profile
    )
    return envelope.validUntil
  } catch {
    return undefined
  }
}

/**
 * Resolves a native-paymaster journal deadline only when its immutable
 * authority and signed envelope agree exactly. Invalid or tampered metadata
 * remains lane-locking.
 */
export function authorityBoundSponsorshipValidUntil(
  authority: unknown,
  operation: ManagedUserOperation
): bigint | undefined {
  const parsedAuthority = parseSponsorshipAuthority(authority)
  if (!parsedAuthority) return undefined
  const envelopeValidUntil = pletherSponsorshipValidUntil(
    parsedAuthority.paymasterAddress,
    operation
  )
  return envelopeValidUntil?.toString() === parsedAuthority.validUntil
    ? envelopeValidUntil
    : undefined
}

/**
 * Validates a newly prepared sponsorship against its validated paymaster
 * shape.
 */
export function manifestSponsorshipValidUntil(
  manifest: PerpsAaDeploymentManifest,
  operation: ManagedUserOperation
): bigint | undefined {
  if (isNativePaymasterManifest(manifest)) {
    if (
      manifest.chainId !== PLETHER_PAYMASTER_CHAIN_ID ||
      !isAddressEqual(manifest.entryPoint, PERPS_ENTRY_POINT_V08)
    ) {
      return undefined
    }
    return pletherSponsorshipValidUntil(
      manifest.paymasterAddress,
      operation
    )
  }
  return pimlicoSponsorshipValidUntil(
    operation.paymaster,
    operation.paymasterData
  )
}

/**
 * Recovery registry for journals written before or after the native-paymaster
 * cutover. Pimlico entries remain parseable under a native-paymaster runtime;
 * unknown formats remain lane-locking.
 */
export function knownSponsorshipValidUntil(
  operation: ManagedUserOperation,
  pletherPaymasterAddress?: Address
): bigint | undefined {
  const legacyDeadline = pimlicoSponsorshipValidUntil(
    operation.paymaster,
    operation.paymasterData
  )
  if (legacyDeadline !== undefined) return legacyDeadline
  return pletherPaymasterAddress === undefined
    ? undefined
    : pletherSponsorshipValidUntil(pletherPaymasterAddress, operation)
}
