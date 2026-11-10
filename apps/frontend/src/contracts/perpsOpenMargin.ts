import type { Address, ContractFunctionReturnType, PublicClient } from 'viem'
import { PERPS_CFD_ENGINE_LENS_ABI } from './abis'
import { PERPS_ARBITRUM_SEPOLIA } from './perpsAddresses'

export type PerpsOpenMarginPreview = ContractFunctionReturnType<typeof PERPS_CFD_ENGINE_LENS_ABI, 'view', 'previewOpen'>

export function openPreviewLeverageBps(preview: PerpsOpenMarginPreview, price: bigint): bigint | undefined {
  if (!preview.valid || preview.postEquityAssets <= 0n) return undefined
  const notional = preview.postSize * price / 10n ** 20n
  return (notional * 10_000n + preview.postEquityAssets - 1n) / preview.postEquityAssets
}

// These are the canonical planner's collateral failures. Other failures must
// retain their diagnostics rather than being treated as a margin search bound.
const OPEN_MARGIN_FAILURES = [5, 6, 9, 10] as const

/** Find a funded margin for an exact size using canonical reserve, fee, carry,
 * claim and position accounting. Every sample is read at the same block.
 * The upper budget is never exceeded and every returned margin is rechecked. */
export async function quotePerpsOpenMargin(
  client: Pick<PublicClient, 'readContract' | 'getBlockNumber'>,
  input: {
    account: Address
    side: number
    sizeDelta: bigint
    marginBudget: bigint
    oraclePrice: bigint
    selectedMaxLeverageBps: number
    blockNumber?: bigint
    signal?: AbortSignal
  }
): Promise<PerpsOpenMarginPreview> {
  input.signal?.throwIfAborted()
  if (!Number.isSafeInteger(input.selectedMaxLeverageBps) || input.selectedMaxLeverageBps <= 0) {
    throw new Error('Selected maximum leverage is invalid')
  }
  if (input.marginBudget < 0n || input.sizeDelta <= 0n || input.oraclePrice <= 0n || ![0, 1].includes(input.side)) {
    throw new Error('Opening margin inputs are invalid')
  }
  const blockNumber = input.blockNumber ?? await client.getBlockNumber({ cacheTime: 0 })
  const read = async (margin: bigint) => {
    input.signal?.throwIfAborted()
    const preview = await client.readContract({
      address: PERPS_ARBITRUM_SEPOLIA.cfdEngineLens,
      abi: PERPS_CFD_ENGINE_LENS_ABI,
      functionName: 'previewOpen',
      args: [input.account, input.side, input.sizeDelta, margin, input.oraclePrice],
      blockNumber,
    })
    input.signal?.throwIfAborted()
    return preview
  }
  let upper = input.marginBudget
  let best = await read(upper)
  // The planner reports the canonical price, already clamped to CAP_PRICE.
  const price = best.executionPrice
  const fits = (preview: PerpsOpenMarginPreview) => {
    const leverage = openPreviewLeverageBps(preview, price)
    return leverage !== undefined && leverage <= BigInt(input.selectedMaxLeverageBps)
  }
  // An unfundable size keeps its actual planner diagnosis. The caller also
  // checks leverage when the budget funds a valid but over-leveraged position.
  if (!fits(best)) return best

  let lower = -1n
  for (let attempt = 0; upper - lower > 1n && attempt < 64; attempt += 1) {
    const requiredEquity = (best.postSize * price / 10n ** 20n * 10_000n
      + BigInt(input.selectedMaxLeverageBps) - 1n) / BigInt(input.selectedMaxLeverageBps)
    // Usually one projection removes the excess margin exactly. Bisection
    // handles claim-backed positions and reserve allocation thresholds.
    const surplus = best.postEquityAssets - requiredEquity
    if (surplus === 0n) break
    const projected = upper > surplus ? upper - surplus : 0n
    const candidate = projected > lower && projected < upper ? projected : (upper + lower) / 2n
    const preview = await read(candidate)
    if (fits(preview)) {
      upper = candidate
      best = preview
    } else {
      if (!preview.valid && !OPEN_MARGIN_FAILURES.some(reason => reason === preview.invalidReason)) return preview
      lower = candidate
    }
  }
  return best
}
