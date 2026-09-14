import { getPerpsOpenRevertMessage } from '../utils/perpsErrors'
import { PERPS_POSITION_SIZE_QUANTUM, PERPS_POSITION_SIZE_TO_USDC_SCALE } from './perpsConstants'

/** Keep router rewards outside the margin budget supplied to the engine lens. */
export function perpsMaxOpenMarginBudget(
  availableUsdc: bigint,
  maximumOpenBountyUsdc: bigint,
  protectionRewardsUsdc = 0n
): bigint {
  const rewards = maximumOpenBountyUsdc + protectionRewardsUsdc
  return availableUsdc > rewards ? availableUsdc - rewards : 0n
}

/** Search leverage only. A planner/RPC failure must propagate: planner validity
 * can have disconnected ranges, so a rejected size is not a search boundary. */
export async function constrainPerpsMaxOpen<T>(input: {
  maxSizeDelta: bigint
  selectedMaxLeverageBps: number
  assess: (sizeDelta: bigint) => Promise<{ leverageBps: bigint; value: T }>
  signal?: AbortSignal
}): Promise<{ sizeDelta: bigint; value: T } | undefined> {
  if (!Number.isInteger(input.selectedMaxLeverageBps) || input.selectedMaxLeverageBps <= 0) {
    throw new Error('Selected maximum leverage is invalid')
  }
  const limit = BigInt(input.selectedMaxLeverageBps)
  const assess = async (sizeDelta: bigint) => {
    input.signal?.throwIfAborted()
    const result = await input.assess(sizeDelta)
    input.signal?.throwIfAborted()
    return result
  }
  if (input.maxSizeDelta <= 0n) return undefined
  const maximum = await assess(input.maxSizeDelta)
  if (maximum.leverageBps <= limit) return { sizeDelta: input.maxSizeDelta, value: maximum.value }

  let lower = 0n
  let upper = (input.maxSizeDelta + PERPS_POSITION_SIZE_QUANTUM - 1n) / PERPS_POSITION_SIZE_QUANTUM
  let best: { sizeDelta: bigint; value: T } | undefined
  for (let attempt = 0; lower + 1n < upper; attempt += 1) {
    if (attempt >= 64) throw new Error('Maximum size leverage search did not converge')
    const midpoint = (lower + upper) / 2n
    const sizeDelta = midpoint * PERPS_POSITION_SIZE_QUANTUM
    const candidate = await assess(sizeDelta)
    if (candidate.leverageBps <= limit) {
      lower = midpoint
      best = { sizeDelta, value: candidate.value }
    } else {
      upper = midpoint
    }
  }
  return best
}

export function constrainPerpsMaxOpenPreview<T extends {
  valid: boolean; invalidReason: number; postSize: bigint; postEquityUsdc: bigint
}>(input: {
  quote: { maxSizeDelta: bigint; preview: T }
  selectedMaxLeverageBps: number
  oraclePrice: bigint
  capPrice: bigint
  preview: (sizeDelta: bigint) => Promise<T>
  signal?: AbortSignal
}) {
  const price = input.oraclePrice < input.capPrice ? input.oraclePrice : input.capPrice
  return constrainPerpsMaxOpen({
    maxSizeDelta: input.quote.maxSizeDelta,
    selectedMaxLeverageBps: input.selectedMaxLeverageBps,
    signal: input.signal,
    assess: async (sizeDelta) => {
      const preview = sizeDelta === input.quote.maxSizeDelta ? input.quote.preview : await input.preview(sizeDelta)
      if (!preview.valid) throw new Error(getPerpsOpenRevertMessage(preview.invalidReason))
      if (preview.postEquityUsdc <= 0n) throw new Error('Maximum size preview has no positive position equity')
      const notional = preview.postSize * price / PERPS_POSITION_SIZE_TO_USDC_SCALE
      return { leverageBps: (notional * 10_000n + preview.postEquityUsdc - 1n) / preview.postEquityUsdc, value: preview }
    },
  })
}
