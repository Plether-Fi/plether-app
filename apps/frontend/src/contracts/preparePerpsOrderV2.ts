import type { Address, Hex, PublicClient } from 'viem'
import {
  PERPS_CFD_ENGINE_ABI,
  PERPS_HOUSE_POOL_ABI,
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
  type PerpsExecutionAssessment,
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

export async function preparePerpsOrderV2(
  client: PublicClient,
  manifest: PerpsAaDeploymentManifest,
  input: PreparePerpsOrderV2Input
): Promise<PreparedPerpsOrderV2> {
  const orderLifecycleBook = manifest.orderLifecycleBook
  const policyEvaluator = manifest.policyEvaluator
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
      args: [input.account],
      blockNumber,
    }),
    client.readContract({
      address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
      abi: PERPS_PUBLIC_LENS_ABI,
      functionName: 'getPendingOrders',
      args: [input.account],
      blockNumber,
    }),
    client.readContract({
      address: manifest.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'maxPendingOrders',
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

  const validUntil = block.timestamp + maxOrderAge
  const targetPrice = getPerpsTargetPrice({
    direction: input.direction,
    isClose: input.isClose,
    oraclePrice: currentPrice,
    slippagePercent: input.slippagePercent,
  })
  const commitReferencePrice = (lastMarkPrice === 0n ? 100_000_000n : lastMarkPrice) > capPrice
    ? capPrice
    : lastMarkPrice === 0n
      ? 100_000_000n
      : lastMarkPrice
  const executionBountyUsdc = exactExecutionBounty({
    isClose: input.isClose,
    sizeDelta: input.sizeDelta,
    commitReferencePrice,
    openBountyBps,
    minimumOpenBounty,
    maximumOpenBounty,
    closeBounty,
  })
  const permissiveBounds = permissivePerpsExecutionBounds({
    validUntil,
    expectedConfigHash,
    executionBountyUsdc,
  })
  let reviewedMarginDelta = input.marginDelta
  let order = {
    account: input.account,
    sizeDelta: input.sizeDelta,
    marginDelta: reviewedMarginDelta,
    targetPrice,
    commitTime: block.timestamp,
    commitBlock: block.number,
    orderId: 0n,
    side: input.side,
    isClose: input.isClose,
  }
  const prices = assessmentPrices(currentPrice, targetPrice)
  const assessAtReviewedPrices = async () => Promise.all(prices.map(async (price) =>
    asAssessment(await client.readContract({
      address: policyEvaluator,
      abi: PERPS_ORDER_POLICY_EVALUATOR_ABI,
      functionName: 'assessOrder',
      args: [
        manifest.cfdEngine,
        order,
        manifest.orderRouter,
        price,
        poolDepthUsdc,
        block.timestamp,
        permissiveBounds,
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
      capPrice,
    })
    if (additionalMargin > 0n) {
      reviewedMarginDelta += additionalMargin
      order = { ...order, marginDelta: reviewedMarginDelta }
      assessments = await assessAtReviewedPrices()
    }
  }

  const bounds = derivePerpsExecutionBounds({
    validUntil,
    expectedConfigHash,
    executionBountyUsdc,
    selectedMaxLeverageBps: input.selectedMaxLeverageBps,
    assessments,
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

  // Production requests must pass the evaluator with the final finite bounds
  // at every reviewed price; permissive bounds are never submitted.
  await Promise.all(prices.map((price) => client.readContract({
    address: policyEvaluator,
    abi: PERPS_ORDER_POLICY_EVALUATOR_ABI,
    functionName: 'assessOrder',
    args: [
      manifest.cfdEngine,
      order,
      manifest.orderRouter,
      price,
      poolDepthUsdc,
      block.timestamp,
      bounds,
      executionBountyUsdc,
    ],
    blockNumber,
  })))
  await client.simulateContract({
    account: input.account,
    address: manifest.orderRouter,
    abi: PERPS_ORDER_ROUTER_ABI,
    functionName: 'commitOrder',
    args: [request],
    blockNumber,
  })

  const executionMode = assessments[0].mode
  return {
    account: input.account,
    manifestVersion: manifest.version,
    orderRouter: manifest.orderRouter,
    orderLifecycleBook,
    request,
    executionBountyUsdc,
    reviewedBlockNumber: block.number,
    reviewedBlockHash: block.hash,
    reviewedPrice: currentPrice,
    protection: {
      validUntil,
      executionMode,
      executionBountyUsdc,
      maxGrossAccountDebitUsdc: bounds.maxGrossAccountDebitUsdc,
      maxActionChargeUsdc: bounds.maxActionChargeUsdc,
      maxExplicitFeesUsdc: bounds.maxExplicitFeesUsdc,
      maxPostLeverageBps: bounds.maxPostLeverageBps,
      minPostSettlementBalanceUsdc: bounds.minPostSettlementBalanceUsdc,
      minPostPositionEquityUsdc: bounds.minPostPositionEquityUsdc,
    },
  }
}
