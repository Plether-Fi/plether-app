import type { Address, CallParameters, ContractFunctionReturnType, PublicClient } from 'viem'
import { PERPS_CFD_ENGINE_LENS_ABI } from './abis'
import { PERPS_ARBITRUM_SEPOLIA } from './perpsAddresses'
import { PERPS_MAX_OPEN_QUOTER_ABI, PERPS_MAX_OPEN_QUOTER_BYTECODE } from './generated/perpsMaxOpenQuoter'

export type PerpsMaxOpenQuote = ContractFunctionReturnType<typeof PERPS_CFD_ENGINE_LENS_ABI, 'view', 'quoteMaxOpen'>

// Include canonical errors so a planner failure bubbled through the helper keeps
// its original diagnostic. The helper never substitutes an approximate quote.
const abi = [
  ...PERPS_MAX_OPEN_QUOTER_ABI,
  ...PERPS_CFD_ENGINE_LENS_ABI.filter(item => item.type === 'error'),
] as const

// Bounds EVM work for this read-only calculation, including helper creation and
// the canonical planner search. This is unrelated to broadcast transaction gas.
// Pinned viem forwards this CallParameters field at runtime, although its
// readContract type omits it. Tests check the actual serialized eth_call gas.
const MAX_OPEN_CALL_BUDGET = { gas: 10_000_000n } satisfies Pick<CallParameters, 'gas'>

interface MaxOpenInputs {
  account: Address
  side: number
  oraclePrice: bigint
  selectedMaxLeverageBps: number
  blockNumber?: bigint
  signal?: AbortSignal
}

function validate(input: MaxOpenInputs) {
  input.signal?.throwIfAborted()
  if (!Number.isSafeInteger(input.selectedMaxLeverageBps) || input.selectedMaxLeverageBps <= 0) {
    throw new Error('Selected maximum leverage is invalid')
  }
  if (input.side !== 0 && input.side !== 1) throw new Error('Position side is invalid')
}

/** One eth_call executes the canonical quote and leverage search at one block.
 * viem creates this read-only helper only inside the call; nothing is deployed
 * persistently, signed or broadcast. Router rewards are read in the same call. */
export async function quotePerpsMaxOpen(
  client: Pick<PublicClient, 'readContract'>,
  input: MaxOpenInputs & { availableAssets: bigint; protectionRewardsAssets: bigint }
): Promise<{ marginDelta: bigint; quote: PerpsMaxOpenQuote | undefined }> {
  validate(input)
  const [marginDelta, quote] = await client.readContract({
    code: PERPS_MAX_OPEN_QUOTER_BYTECODE,
    abi,
    functionName: 'quoteAvailable',
    args: [PERPS_ARBITRUM_SEPOLIA.cfdEngineLens, PERPS_ARBITRUM_SEPOLIA.cfdEngine,
      PERPS_ARBITRUM_SEPOLIA.orderRouter, input.account, input.side, input.availableAssets,
      input.protectionRewardsAssets, input.oraclePrice, BigInt(input.selectedMaxLeverageBps)],
    blockNumber: input.blockNumber,
    ...MAX_OPEN_CALL_BUDGET,
  })
  input.signal?.throwIfAborted()
  return { marginDelta, quote: marginDelta > 0n ? quote : undefined }
}

/** Review retains its verified block and independently reserved funding budget.
 * Adverse-price assessments, funding checks and simulation still follow this. */
export async function quotePerpsMaxOpenWithMargin(
  client: Pick<PublicClient, 'readContract'>,
  input: MaxOpenInputs & { marginDelta: bigint }
): Promise<PerpsMaxOpenQuote> {
  validate(input)
  const quote = await client.readContract({
    code: PERPS_MAX_OPEN_QUOTER_BYTECODE,
    abi,
    functionName: 'quoteWithMargin',
    args: [PERPS_ARBITRUM_SEPOLIA.cfdEngineLens, PERPS_ARBITRUM_SEPOLIA.cfdEngine,
      input.account, input.side, input.marginDelta, input.oraclePrice, BigInt(input.selectedMaxLeverageBps)],
    blockNumber: input.blockNumber,
    ...MAX_OPEN_CALL_BUDGET,
  })
  input.signal?.throwIfAborted()
  return quote
}
