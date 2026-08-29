import {
  getAddress,
  hexToBigInt,
  hexToNumber,
  isAddressEqual,
  size,
  slice,
  type Address,
  type Hex,
} from 'viem'

export const PIMLICO_SINGLETON_PAYMASTER_V8 = getAddress(
  '0x888888888888Ec68A58AB8094Cc1AD20Ba3D2402'
)
const VERIFYING_MODE = 0
const MODE_AND_BUNDLER_FLAG_BYTES = 1
const VALIDITY_TIMESTAMP_BYTES = 6
const VERIFYING_CONFIG_BYTES = VALIDITY_TIMESTAMP_BYTES * 2
const MIN_SIGNATURE_BYTES = 64
const MAX_SIGNATURE_BYTES = 65

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
