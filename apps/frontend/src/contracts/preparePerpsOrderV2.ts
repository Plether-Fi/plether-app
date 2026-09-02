import type { Address, Hex, PublicClient } from 'viem'
import {
  PERPS_CFD_ENGINE_ABI,
  PERPS_HOUSE_POOL_ABI,
  PERPS_MARGIN_CLEARINGHOUSE_ABI,
  PERPS_ORDER_LIFECYCLE_BOOK_ABI,
  PERPS_ORDER_POLICY_EVALUATOR_ABI,
  PERPS_ORDER_ROUTER_ABI,
  PERPS_PLETHER_ORACLE_ABI,
  PERPS_POSITION_PROTECTION_BOOK_ABI,
  PERPS_PUBLIC_LENS_ABI,
} from './abis'
import { PERPS_ARBITRUM_SEPOLIA } from './perpsAddresses'
import {
  deriveAdditionalPerpsMarginForLeverage,
  derivePerpsExecutionBounds,
  generatePerpsClientOrderId,
  permissivePerpsExecutionBounds,
  relaxedWebPerpsExecutionBounds,
  type PerpsExecutionAssessment,
  type PerpsOrderReviewSummary,
  type PreparedPerpsOrderV2,
  type PerpsOrderRequestV2,
} from './perpsOrderV2'
import { verifyPerpsV2DeploymentBindings } from './verifyPerpsV2Bindings'
import type { PerpsAaDeploymentManifest } from '../perps-aa/manifest'
import { getPerpsTargetPrice, type PerpsDirection } from '../utils/perps'

const POSITION_SIZE_TO_USDC_SCALE = 10n ** 20n
const ZERO_HASH = `0x${'0'.repeat(64)}`

export interface PreparePerpsOrderV2Input {
  account: Address
  direction: PerpsDirection
  side: number
  sizeDelta: bigint
  marginDelta: bigint
  slippagePercent: number
  isClose: boolean
  selectedMaxLeverageBps: number
  clientOrderId?: Hex
}

export interface ReviewedPerpsOrderV2 {
  preparedOrder: PreparedPerpsOrderV2
  reviewSummary: PerpsOrderReviewSummary
}

export class PerpsOrderFundingShortfallError extends Error {
  readonly reviewedOrder: ReviewedPerpsOrderV2
  readonly shortfallUsdc: bigint

  constructor(reviewedOrder: ReviewedPerpsOrderV2, shortfallUsdc: bigint) {
    super('The reviewed order needs more free margin than the account currently has.')
    this.name = 'PerpsOrderFundingShortfallError'
    this.reviewedOrder = reviewedOrder
    this.shortfallUsdc = shortfallUsdc
  }
}

interface PerpsOrderReviewContext {
  client: PublicClient
  manifest: PerpsAaDeploymentManifest
  orderLifecycleBook: Address
  policyEvaluator: Address
  blockNumber: bigint
  blockHash: Hex
  blockTimestamp: bigint
  maxOrderAge: bigint
  expectedConfigHash: Hex
  openBountyBps: bigint
  minimumOpenBounty: bigint
  maximumOpenBounty: bigint
  closeBounty: bigint
  lastMarkPrice: bigint
  capPrice: bigint
  poolDepthUsdc: bigint
  currentPrice: bigint
  freeBuyingPowerUsdc: bigint
}

function exactExecutionBounty(input: {
  isClose: boolean
  sizeDelta: bigint
  commitReferencePrice: bigint
  openBountyBps: bigint
  minimumOpenBounty: bigint
  maximumOpenBounty: bigint
  closeBounty: bigint
}): bigint {
  if (input.isClose) return input.closeBounty
  const notionalUsdc =
    input.sizeDelta * input.commitReferencePrice /
    POSITION_SIZE_TO_USDC_SCALE
  const proportional = notionalUsdc * input.openBountyBps / 10_000n
  if (proportional < input.minimumOpenBounty) {
    return input.minimumOpenBounty
  }
  return proportional > input.maximumOpenBounty
    ? input.maximumOpenBounty
    : proportional
}

function assessmentPrices(currentPrice: bigint, targetPrice: bigint): bigint[] {
  const midpoint = currentPrice < targetPrice
    ? currentPrice + (targetPrice - currentPrice) / 2n
    : targetPrice + (currentPrice - targetPrice) / 2n
  return [...new Set([currentPrice, midpoint, targetPrice])]
}

function asAssessment(value: unknown): PerpsExecutionAssessment {
  return value as PerpsExecutionAssessment
}

function maximum(values: bigint[]): bigint {
  return values.reduce((result, value) => value > result ? value : result, 0n)
}

function validateInput(input: PreparePerpsOrderV2Input): void {
  if (input.sizeDelta <= 0n) throw new Error('Order size must be positive')
  if (input.isClose && input.marginDelta !== 0n) {
    throw new Error('Close orders must use zero margin delta')
  }
  if (
    !Number.isInteger(input.selectedMaxLeverageBps) ||
    input.selectedMaxLeverageBps <= 0 ||
    input.selectedMaxLeverageBps > 0xffff_ffff
  ) {
    throw new Error('Selected maximum leverage is invalid')
  }
}

async function loadPerpsOrderReviewContext(
  client: PublicClient,
  manifest: PerpsAaDeploymentManifest,
  account: Address
): Promise<PerpsOrderReviewContext> {
  const orderLifecycleBook = manifest.orderLifecycleBook
  const policyEvaluator = manifest.policyEvaluator
  const verified = await verifyPerpsV2DeploymentBindings(client, manifest)
  const blockNumber = verified.blockNumber
  const block = await client.getBlock({ blockNumber })
  if (block.number > 0xffff_ffff_ffff_ffffn) {
    throw new Error('The reviewed block number cannot fit the V2 order format')
  }

  const [
    maxOrderAge,
    expectedConfigHash,
    openBountyBps,
    minimumOpenBounty,
    maximumOpenBounty,
    closeBounty,
    lastMarkPrice,
    capPrice,
    poolDepthUsdc,
    currentPrice,
    activeProtectionId,
    pendingOrders,
    maxPendingOrders,
    freeBuyingPowerUsdc,
  ] = await Promise.all([
    client.readContract({
      address: manifest.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'maxOrderAge',
      blockNumber,
    }),
    client.readContract({
      address: orderLifecycleBook,
      abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI,
      functionName: 'currentExecutionConfigHash',
      blockNumber,
    }),
    client.readContract({
      address: manifest.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'openOrderExecutionBountyBps',
      blockNumber,
    }),
    client.readContract({
      address: manifest.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'minOpenOrderExecutionBountyUsdc',
      blockNumber,
    }),
    client.readContract({
      address: manifest.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'maxOpenOrderExecutionBountyUsdc',
      blockNumber,
    }),
    client.readContract({
      address: manifest.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'closeOrderExecutionBountyUsdc',
      blockNumber,
    }),
    client.readContract({
      address: manifest.cfdEngine,
      abi: PERPS_CFD_ENGINE_ABI,
      functionName: 'lastMarkPrice',
      blockNumber,
    }),
    client.readContract({
      address: manifest.cfdEngine,
      abi: PERPS_CFD_ENGINE_ABI,
      functionName: 'CAP_PRICE',
      blockNumber,
    }),
    client.readContract({
      address: PERPS_ARBITRUM_SEPOLIA.housePool,
      abi: PERPS_HOUSE_POOL_ABI,
      functionName: 'totalAssets',
      blockNumber,
    }),
    client.readContract({
      address: PERPS_ARBITRUM_SEPOLIA.pletherOracle,
      abi: PERPS_PLETHER_ORACLE_ABI,
      functionName: 'getLatestPrice',
      args: [],
      blockNumber,
    }),
    client.readContract({
      address: verified.positionProtectionBook,
      abi: PERPS_POSITION_PROTECTION_BOOK_ABI,
      functionName: 'activePositionProtectionId',
      args: [account],
      blockNumber,
    }),
    client.readContract({
      address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
      abi: PERPS_PUBLIC_LENS_ABI,
      functionName: 'getPendingOrders',
      args: [account],
      blockNumber,
    }),
    client.readContract({
      address: manifest.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'maxPendingOrders',
      blockNumber,
    }),
    client.readContract({
      address: manifest.marginClearinghouse,
      abi: PERPS_MARGIN_CLEARINGHOUSE_ABI,
      functionName: 'getFreeBuyingPowerUsdc',
      args: [account],
      blockNumber,
    }),
  ])

  if (expectedConfigHash.toLowerCase() === ZERO_HASH) {
    throw new Error('The execution configuration hash is unavailable')
  }
  if (activeProtectionId !== 0n) {
    throw new Error(
      `Position protection #${activeProtectionId.toString()} is active. Cancel or finalize it before placing a discretionary order.`
    )
  }
  if (BigInt(pendingOrders.length) >= maxPendingOrders) {
    throw new Error(
      `This account already has ${pendingOrders.length.toString()} pending orders, which is the current limit.`
    )
  }
  if (currentPrice <= 0n) throw new Error('The current plDXY price is unavailable')

  return {
    client,
    manifest,
    orderLifecycleBook,
    policyEvaluator,
    blockNumber,
    blockHash: block.hash,
    blockTimestamp: block.timestamp,
    maxOrderAge,
    expectedConfigHash,
    openBountyBps,
    minimumOpenBounty,
    maximumOpenBounty,
    closeBounty,
    lastMarkPrice,
    capPrice,
    poolDepthUsdc,
    currentPrice,
    freeBuyingPowerUsdc,
  }
}

async function reviewPerpsOrderWithContext(
  context: PerpsOrderReviewContext,
  input: PreparePerpsOrderV2Input,
  includeFinalBoundsAssessment: boolean
): Promise<ReviewedPerpsOrderV2> {
  validateInput(input)
  const {
    client,
    manifest,
    orderLifecycleBook,
    policyEvaluator,
    blockNumber,
    blockHash,
    blockTimestamp,
  } = context
  const validUntil = blockTimestamp + context.maxOrderAge
  const targetPrice = getPerpsTargetPrice({
    direction: input.direction,
    isClose: input.isClose,
    oraclePrice: context.currentPrice,
    slippagePercent: input.slippagePercent,
  })
  const commitReferencePrice = (context.lastMarkPrice === 0n ? 100_000_000n : context.lastMarkPrice) > context.capPrice
    ? context.capPrice
    : context.lastMarkPrice === 0n
      ? 100_000_000n
      : context.lastMarkPrice
  const executionBountyUsdc = exactExecutionBounty({
    isClose: input.isClose,
    sizeDelta: input.sizeDelta,
    commitReferencePrice,
    openBountyBps: context.openBountyBps,
    minimumOpenBounty: context.minimumOpenBounty,
    maximumOpenBounty: context.maximumOpenBounty,
    closeBounty: context.closeBounty,
  })
  const permissiveBounds = permissivePerpsExecutionBounds({
    validUntil,
    expectedConfigHash: context.expectedConfigHash,
    executionBountyUsdc,
  })
  let reviewedMarginDelta = input.marginDelta
  let order = {
    account: input.account,
    sizeDelta: input.sizeDelta,
    marginDelta: reviewedMarginDelta,
    targetPrice,
    commitTime: blockTimestamp,
    commitBlock: blockNumber,
    orderId: 0n,
    side: input.side,
    isClose: input.isClose,
  }
  const prices = assessmentPrices(context.currentPrice, targetPrice)
  const assessAtReviewedPrices = async (bounds = permissiveBounds) => Promise.all(prices.map(async (price) =>
    asAssessment(await client.readContract({
      address: policyEvaluator,
      abi: PERPS_ORDER_POLICY_EVALUATOR_ABI,
      functionName: 'assessOrder',
      args: [
        manifest.cfdEngine,
        order,
        manifest.orderRouter,
        price,
        context.poolDepthUsdc,
        blockTimestamp,
        bounds,
        executionBountyUsdc,
      ],
      blockNumber,
    }))
  ))
  let assessments = await assessAtReviewedPrices()

  if (!input.isClose) {
    const additionalMargin = deriveAdditionalPerpsMarginForLeverage({
      selectedMaxLeverageBps: input.selectedMaxLeverageBps,
      marginDelta: reviewedMarginDelta,
      assessments,
      prices,
      capPrice: context.capPrice,
    })
    if (additionalMargin > 0n) {
      reviewedMarginDelta += additionalMargin
      order = { ...order, marginDelta: reviewedMarginDelta }
      assessments = await assessAtReviewedPrices()
    }
  }

  derivePerpsExecutionBounds({
    validUntil,
    expectedConfigHash: context.expectedConfigHash,
    executionBountyUsdc,
    selectedMaxLeverageBps: input.selectedMaxLeverageBps,
    assessments,
  })
  const executionMode = assessments[0].mode
  const bounds = relaxedWebPerpsExecutionBounds({
    validUntil,
    expectedConfigHash: context.expectedConfigHash,
    executionBountyUsdc,
    executionMode,
  })
  const request: PerpsOrderRequestV2 = {
    clientOrderId: input.clientOrderId ?? generatePerpsClientOrderId(),
    side: input.side,
    sizeDelta: input.sizeDelta,
    marginDelta: reviewedMarginDelta,
    targetPrice,
    isClose: input.isClose,
    bounds,
  }

  const finalAssessments = includeFinalBoundsAssessment
    ? await assessAtReviewedPrices(bounds)
    : assessments
  const reviewSummary: PerpsOrderReviewSummary = {
    requiredMarginUsdc: reviewedMarginDelta,
    executionBountyUsdc,
    requiredFundingUsdc: reviewedMarginDelta + executionBountyUsdc,
    availableFundingUsdc: context.freeBuyingPowerUsdc,
    worstPostLeverageBps: maximum(
      finalAssessments.map((assessment) => assessment.postLeverageBps)
    ),
    reviewedBlockNumber: blockNumber,
    reviewedBlockHash: blockHash,
    reviewedPrice: context.currentPrice,
    currentAssessment: finalAssessments[0],
  }
  const preparedOrder: PreparedPerpsOrderV2 = {
    account: input.account,
    manifestVersion: manifest.version,
    orderRouter: manifest.orderRouter,
    orderLifecycleBook,
    request,
    executionBountyUsdc,
    reviewedBlockNumber: blockNumber,
    reviewedBlockHash: blockHash,
    reviewedPrice: context.currentPrice,
    protection: {
      validUntil,
      executionMode,
      executionBountyUsdc,
    },
    reviewSummary,
  }
  return { preparedOrder, reviewSummary }
}

export async function reviewPerpsOrderV2(
  client: PublicClient,
  manifest: PerpsAaDeploymentManifest,
  input: PreparePerpsOrderV2Input
): Promise<ReviewedPerpsOrderV2> {
  const context = await loadPerpsOrderReviewContext(client, manifest, input.account)
  return reviewPerpsOrderWithContext(context, input, true)
}

export async function simulateReviewedPerpsOrderV2(
  client: PublicClient,
  reviewedOrder: ReviewedPerpsOrderV2
): Promise<void> {
  await client.simulateContract({
    account: reviewedOrder.preparedOrder.account,
    address: reviewedOrder.preparedOrder.orderRouter,
    abi: PERPS_ORDER_ROUTER_ABI,
    functionName: 'commitOrder',
    args: [reviewedOrder.preparedOrder.request],
    blockNumber: reviewedOrder.reviewSummary.reviewedBlockNumber,
  })
}

export async function preparePerpsOrderV2(
  client: PublicClient,
  manifest: PerpsAaDeploymentManifest,
  input: PreparePerpsOrderV2Input
): Promise<PreparedPerpsOrderV2> {
  const reviewedOrder = await reviewPerpsOrderV2(client, manifest, input)
  if (
    !input.isClose &&
    reviewedOrder.reviewSummary.requiredFundingUsdc >
      reviewedOrder.reviewSummary.availableFundingUsdc
  ) {
    throw new PerpsOrderFundingShortfallError(
      reviewedOrder,
      reviewedOrder.reviewSummary.requiredFundingUsdc -
        reviewedOrder.reviewSummary.availableFundingUsdc
    )
  }
  await simulateReviewedPerpsOrderV2(client, reviewedOrder)
  return reviewedOrder.preparedOrder
}
