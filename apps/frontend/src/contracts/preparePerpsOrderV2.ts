import type { Address, Hex, PublicClient } from 'viem'
import {
  PERPS_CFD_CLOSE_PREVIEW_ABI,
  PERPS_CFD_ENGINE_ABI,
  PERPS_CFD_ENGINE_LENS_ABI,
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
import { constrainPerpsMaxOpen, constrainPerpsMaxOpenPreview, perpsMaxOpenMarginBudget } from './perpsMaxOpen'
import {
  deriveAdditionalPerpsMarginForLeverage,
  derivePerpsExecutionBounds,
  generatePerpsClientOrderId,
  permissivePerpsExecutionBounds,
  relaxedWebPerpsExecutionBounds,
  reviewedExecutionBountyMaximum,
  type PerpsExecutionAssessment,
  type PerpsClosePreview,
  type PerpsOrderReviewSummary,
  type PreparedPerpsOrderV2,
  type PerpsOrderRequestV2,
} from './perpsOrderV2'
import { verifyClosePreviewDeployment, verifyPerpsV2DeploymentBindings, verifyProtectionDeployment } from './verifyPerpsV2Bindings'
import type { PerpsAaDeploymentManifest } from '../perps-aa/manifest'
import { getPerpsTargetPrice, type PerpsDirection } from '../utils/perps'
import { PROTECTION_CONFIG_ABI, validateProtectionParams, type PositionProtectionParams } from './positionProtection'
import { getPerpsOpenRevertMessage } from '../utils/perpsErrors'
import { preparationFailure, withPreparationStep } from '../utils/perpsPreparationDiagnostics'

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
  positionProtection?: PositionProtectionParams
  /** Refresh the maximum within selected leverage at the reviewed block. */
  maxSize?: boolean
  signal?: AbortSignal
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

export class PerpsOrderReviewError extends Error {
  readonly reviewSummary: PerpsOrderReviewSummary
  readonly reason: 'validation' | 'leverage'

  constructor(reviewSummary: PerpsOrderReviewSummary, cause: unknown, reason: 'validation' | 'leverage' = 'validation') {
    super(cause instanceof Error ? cause.message : 'The reviewed order is invalid', { cause })
    this.name = 'PerpsOrderReviewError'
    this.reviewSummary = reviewSummary
    this.reason = reason
  }
}

interface PerpsOrderReviewContext {
  closePreviewAddress?: Address
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

function asClosePreview(value: unknown): PerpsClosePreview {
  const preview = value as PerpsClosePreview
  if (![1, 2, 3].includes(preview.assessment.mode)) throw new Error('Close review returned an invalid execution mode.')
  return preview
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
    !input.isClose && (
      !Number.isInteger(input.selectedMaxLeverageBps) ||
      input.selectedMaxLeverageBps <= 0 ||
      input.selectedMaxLeverageBps > 0xffff_ffff
    )
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
  const verified = await withPreparationStep('deployment_verification', undefined, () => verifyPerpsV2DeploymentBindings(client, manifest))
  // Reuse the exact block already fetched for deployment verification.
  const { blockNumber, block } = verified
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
    withPreparationStep('context_read', 'maxOrderAge', () => client.readContract({
      address: manifest.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'maxOrderAge',
      blockNumber,
    })),
    withPreparationStep('context_read', 'currentExecutionConfigHash', () => client.readContract({
      address: orderLifecycleBook,
      abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI,
      functionName: 'currentExecutionConfigHash',
      blockNumber,
    })),
    withPreparationStep('context_read', 'openOrderExecutionBountyBps', () => client.readContract({
      address: manifest.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'openOrderExecutionBountyBps',
      blockNumber,
    })),
    withPreparationStep('context_read', 'minOpenOrderExecutionBountyUsdc', () => client.readContract({
      address: manifest.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'minOpenOrderExecutionBountyUsdc',
      blockNumber,
    })),
    withPreparationStep('context_read', 'maxOpenOrderExecutionBountyUsdc', () => client.readContract({
      address: manifest.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'maxOpenOrderExecutionBountyUsdc',
      blockNumber,
    })),
    withPreparationStep('context_read', 'closeOrderExecutionBountyUsdc', () => client.readContract({
      address: manifest.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'closeOrderExecutionBountyUsdc',
      blockNumber,
    })),
    withPreparationStep('context_read', 'lastMarkPrice', () => client.readContract({
      address: manifest.cfdEngine,
      abi: PERPS_CFD_ENGINE_ABI,
      functionName: 'lastMarkPrice',
      blockNumber,
    })),
    withPreparationStep('context_read', 'CAP_PRICE', () => client.readContract({
      address: manifest.cfdEngine,
      abi: PERPS_CFD_ENGINE_ABI,
      functionName: 'CAP_PRICE',
      blockNumber,
    })),
    withPreparationStep('context_read', 'totalAssets', () => client.readContract({
      address: PERPS_ARBITRUM_SEPOLIA.housePool,
      abi: PERPS_HOUSE_POOL_ABI,
      functionName: 'totalAssets',
      blockNumber,
    })),
    withPreparationStep('context_read', 'getLatestPrice', () => client.readContract({
      address: PERPS_ARBITRUM_SEPOLIA.pletherOracle,
      abi: PERPS_PLETHER_ORACLE_ABI,
      functionName: 'getLatestPrice',
      args: [],
      blockNumber,
    })),
    withPreparationStep('context_read', 'activePositionProtectionId', () => client.readContract({
      address: verified.positionProtectionBook,
      abi: PERPS_POSITION_PROTECTION_BOOK_ABI,
      functionName: 'activePositionProtectionId',
      args: [account],
      blockNumber,
    })),
    withPreparationStep('context_read', 'getPendingOrders', () => client.readContract({
      address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
      abi: PERPS_PUBLIC_LENS_ABI,
      functionName: 'getPendingOrders',
      args: [account],
      blockNumber,
    })),
    withPreparationStep('context_read', 'maxPendingOrders', () => client.readContract({
      address: manifest.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'maxPendingOrders',
      blockNumber,
    })),
    withPreparationStep('context_read', 'getFreeBuyingPowerUsdc', () => client.readContract({
      address: manifest.marginClearinghouse,
      abi: PERPS_MARGIN_CLEARINGHOUSE_ABI,
      functionName: 'getFreeBuyingPowerUsdc',
      args: [account],
      blockNumber,
    })),
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
  input: PreparePerpsOrderV2Input
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
  let executionBountyUsdc = exactExecutionBounty({
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
  let commitmentCarryUsdc: bigint | undefined
  const assessAtReviewedPrices = async (bounds = permissiveBounds) => {
    input.signal?.throwIfAborted()
    if (input.isClose) {
      if (!context.closePreviewAddress) throw new Error('Close review is unavailable: preview deployment is not verified.')
      const previews = await Promise.all(prices.map(async (price) => asClosePreview(await withPreparationStep('order_assessment', 'previewClose', () => client.readContract({
        address: context.closePreviewAddress!,
        abi: PERPS_CFD_CLOSE_PREVIEW_ABI,
        functionName: 'previewClose',
        args: [manifest.cfdEngine, order, manifest.orderRouter, price, blockTimestamp, bounds],
        blockNumber,
      }), {
        chainId: manifest.chainId, address: context.closePreviewAddress!, account: input.account,
        blockNumber, blockHash, samplePrice: price,
        assessmentPoint: price === context.currentPrice ? 'current' : price === targetPrice ? 'limit' : 'midpoint',
      }))))
      input.signal?.throwIfAborted()
      const current = previews[0]
      if (previews.some(preview => preview.executionBountyUsdc !== context.closeBounty ||
        preview.commitmentCarryUsdc !== current.commitmentCarryUsdc) ||
        (commitmentCarryUsdc !== undefined && commitmentCarryUsdc !== current.commitmentCarryUsdc)) {
        throw preparationFailure(new Error('Close review changed at the reviewed block. Refresh and try again.'), 'order_assessment', 'previewClose')
      }
      executionBountyUsdc = current.executionBountyUsdc
      commitmentCarryUsdc = current.commitmentCarryUsdc
      return previews.map(preview => preview.assessment)
    }
    const assessments = await Promise.all(prices.map(async (price) => asAssessment(await withPreparationStep('order_assessment', 'assessOrder', () => client.readContract({
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
    })))))
    input.signal?.throwIfAborted()
    return assessments
  }
  let assessments = await assessAtReviewedPrices()

  // Reserve funding can absorb a margin increase before it becomes position
  // equity. Bound the retries and stop if equity no longer improves.
  // Max keeps the margin budget fixed and adjusts size to fit selected leverage.
  for (let attempt = 0; !input.isClose && !input.maxSize && attempt < 4; attempt += 1) {
    const additionalMargin = deriveAdditionalPerpsMarginForLeverage({
      selectedMaxLeverageBps: input.selectedMaxLeverageBps,
      marginDelta: reviewedMarginDelta,
      assessments,
      prices,
      capPrice: context.capPrice,
    })
    if (additionalMargin === 0n) break
    const previousEquities = assessments.map((assessment) => assessment.postPositionEquityUsdc)
    reviewedMarginDelta += additionalMargin
    order = { ...order, marginDelta: reviewedMarginDelta }
    assessments = await assessAtReviewedPrices()
    if (assessments.every((assessment, index) => assessment.postPositionEquityUsdc <= previousEquities[index])) break
  }

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

  const finalAssessments = await assessAtReviewedPrices(bounds)
  const reviewSummary: PerpsOrderReviewSummary = {
    ...(input.isClose ? { commitmentCarryUsdc } : {}),
    requiredMarginUsdc: reviewedMarginDelta,
    executionBountyUsdc,
    // Reserve for the reviewed maximum, while displaying the current quote
    // separately. Never apply tolerance again to persisted/signed requests.
    requiredFundingUsdc: reviewedMarginDelta + (input.isClose ? executionBountyUsdc : bounds.maxExecutionBountyUsdc),
    availableFundingUsdc: context.freeBuyingPowerUsdc,
    worstPostLeverageBps: maximum(
      finalAssessments.map((assessment) => assessment.postLeverageBps)
    ),
    reviewedBlockNumber: blockNumber,
    reviewedBlockHash: blockHash,
    reviewedPrice: context.currentPrice,
    currentAssessment: finalAssessments[0],
  }
  try {
    // Validate the same final assessments that the review displays. Reductions
    // retain regime/equity/range checks, but do not inherit the opening slider.
    derivePerpsExecutionBounds({
      validUntil,
      expectedConfigHash: context.expectedConfigHash,
      executionBountyUsdc,
      selectedMaxLeverageBps: permissiveBounds.maxPostLeverageBps,
      assessments: finalAssessments,
    })
  } catch (error) {
    throw new PerpsOrderReviewError(reviewSummary, error)
  }
  if (!input.isClose && reviewSummary.worstPostLeverageBps > BigInt(input.selectedMaxLeverageBps)) {
    throw new PerpsOrderReviewError(reviewSummary, new Error(
      `Highest reviewed leverage is ${(Number(reviewSummary.worstPostLeverageBps) / 10_000).toString()}x, above your selected ${(input.selectedMaxLeverageBps / 10_000).toString()}x limit.`
    ), 'leverage')
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
  input.signal?.throwIfAborted()
  validateInput(input)
  const context = await withPreparationStep('context_read', undefined, () => loadPerpsOrderReviewContext(client, manifest, input.account))
  if (input.isClose) {
    context.closePreviewAddress = await withPreparationStep('deployment_verification', undefined,
      () => verifyClosePreviewDeployment(client, manifest, context.blockNumber))
    input.signal?.throwIfAborted()
  }
  let protection: PreparedPerpsOrderV2['positionProtection']
  if (input.positionProtection) {
    await withPreparationStep('deployment_verification', undefined, () => verifyProtectionDeployment(client, manifest, context.blockNumber))
    if (input.isClose) throw new Error('Protection can only be attached to a fresh open')
    validateProtectionParams(input.positionProtection, input.direction, context.lastMarkPrice, context.capPrice)
    const [triggerBountyUsdc, position] = await Promise.all([
      withPreparationStep('context_read', 'positionProtectionTriggerBountyUsdc', () => client.readContract({ address: manifest.orderRouter, abi: PROTECTION_CONFIG_ABI, functionName: 'positionProtectionTriggerBountyUsdc', blockNumber: context.blockNumber })),
      withPreparationStep('context_read', 'getPosition', () => client.readContract({ address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens, abi: PERPS_PUBLIC_LENS_ABI, functionName: 'getPosition', args: [input.account], blockNumber: context.blockNumber })),
    ])
    if (position.exists) throw new Error('Protected opens require an account with no position')
    protection = { book: manifest.positionProtectionBook, params: { ...input.positionProtection }, triggerBountyUsdc, executionBountyUsdc: context.closeBounty }
  }
  const review = async (candidate: PreparePerpsOrderV2Input) => {
    input.signal?.throwIfAborted()
    const reviewed = await reviewPerpsOrderWithContext(context, candidate)
    if (protection) {
      reviewed.preparedOrder.positionProtection = protection
      reviewed.reviewSummary.requiredFundingUsdc += protection.triggerBountyUsdc + protection.executionBountyUsdc
    }
    return reviewed
  }
  if (!input.maxSize || input.isClose) return review(input)

  const marginDelta = perpsMaxOpenMarginBudget(
    context.freeBuyingPowerUsdc,
    reviewedExecutionBountyMaximum(context.maximumOpenBounty),
    protection ? protection.triggerBountyUsdc + protection.executionBountyUsdc : 0n
  )
  if (marginDelta <= 0n) {
    throw new Error('Available margin cannot fund an opening order after execution rewards.')
  }
  input.signal?.throwIfAborted()
  const quote = await withPreparationStep('max_size_quote', 'quoteMaxOpen', () => client.readContract({
    address: PERPS_ARBITRUM_SEPOLIA.cfdEngineLens,
    abi: PERPS_CFD_ENGINE_LENS_ABI,
    functionName: 'quoteMaxOpen',
    args: [input.account, input.side, marginDelta, context.currentPrice, context.blockTimestamp],
    blockNumber: context.blockNumber,
  }))
  input.signal?.throwIfAborted()
  if (quote.maxSizeDelta === 0n || !quote.preview.valid) {
    throw new Error(getPerpsOpenRevertMessage(quote.preview.invalidReason || quote.limitingReason))
  }
  // Apply selected leverage before reviewing adverse execution prices: the
  // unrestricted protocol maximum may already fail entry margin at those prices.
  const opening = await constrainPerpsMaxOpenPreview({
    quote,
    selectedMaxLeverageBps: input.selectedMaxLeverageBps,
    oraclePrice: context.currentPrice,
    capPrice: context.capPrice,
    signal: input.signal,
    preview: (sizeDelta) => withPreparationStep('max_size_quote', 'previewOpen', () => client.readContract({
      address: PERPS_ARBITRUM_SEPOLIA.cfdEngineLens,
      abi: PERPS_CFD_ENGINE_LENS_ABI,
      functionName: 'previewOpen',
      args: [input.account, input.side, sizeDelta, marginDelta, context.currentPrice, context.blockTimestamp],
      blockNumber: context.blockNumber,
    })),
  })
  const constrained = await constrainPerpsMaxOpen({
    maxSizeDelta: opening?.sizeDelta ?? 0n,
    selectedMaxLeverageBps: input.selectedMaxLeverageBps,
    signal: input.signal,
    assess: async (sizeDelta) => {
      try {
        const value = await review({ ...input, sizeDelta, marginDelta })
        return { leverageBps: value.reviewSummary.worstPostLeverageBps, value }
      } catch (error) {
        if (!(error instanceof PerpsOrderReviewError) || error.reason !== 'leverage') throw error
        return { leverageBps: error.reviewSummary.worstPostLeverageBps, value: undefined }
      }
    },
  })
  if (!constrained?.value) throw new Error('Available margin cannot fund an opening order at the selected leverage.')
  return constrained.value
}

export async function simulateReviewedPerpsOrderV2(
  client: PublicClient,
  reviewedOrder: ReviewedPerpsOrderV2
): Promise<void> {
  const protection = reviewedOrder.preparedOrder.positionProtection
  if (protection) {
    await withPreparationStep('commit_simulation', 'commitOpenOrderWithProtection', () => client.simulateContract({
      account: reviewedOrder.preparedOrder.account,
      address: protection.book,
      abi: PERPS_POSITION_PROTECTION_BOOK_ABI,
      functionName: 'commitOpenOrderWithProtection',
      args: [reviewedOrder.preparedOrder.request, protection.params],
      blockNumber: reviewedOrder.reviewSummary.reviewedBlockNumber,
    }))
    return
  }
  await withPreparationStep('commit_simulation', 'commitOrder', () => client.simulateContract({
    account: reviewedOrder.preparedOrder.account,
    address: reviewedOrder.preparedOrder.orderRouter,
    abi: PERPS_ORDER_ROUTER_ABI,
    functionName: 'commitOrder',
    args: [reviewedOrder.preparedOrder.request],
    blockNumber: reviewedOrder.reviewSummary.reviewedBlockNumber,
  }))
}

export async function preparePerpsOrderV2(
  client: PublicClient,
  manifest: PerpsAaDeploymentManifest,
  input: PreparePerpsOrderV2Input
): Promise<PreparedPerpsOrderV2> {
  const reviewedOrder = await withPreparationStep('review_validation', undefined, () => reviewPerpsOrderV2(client, manifest, input))
  if (
    !input.isClose &&
    reviewedOrder.reviewSummary.requiredFundingUsdc >
      reviewedOrder.reviewSummary.availableFundingUsdc
  ) {
    throw preparationFailure(new PerpsOrderFundingShortfallError(
      reviewedOrder,
      reviewedOrder.reviewSummary.requiredFundingUsdc -
        reviewedOrder.reviewSummary.availableFundingUsdc
    ), 'funding_check')
  }
  input.signal?.throwIfAborted()
  await simulateReviewedPerpsOrderV2(client, reviewedOrder)
  return reviewedOrder.preparedOrder
}
