import { encodeFunctionData, getAddress } from 'viem'
import type { PerpsActionPlan } from '@plether-fi/perps-aa-client'
import { PERPS_POSITION_PROTECTION_BOOK_ABI, PERPS_ORDER_ROUTER_ABI } from '../contracts/abis'
import type { PerpsOrderRequestV3 } from '../contracts/perpsOrderV3'

export function buildPlaceOrderV3Action(input: {
  account: `0x${string}`
  orderRouter: `0x${string}`
  request: PerpsOrderRequestV3
}): PerpsActionPlan {
  validateTiming(input.request)
  const account = getAddress(input.account)
  const orderRouter = getAddress(input.orderRouter)
  if (input.request.sizeDelta <= 0n) {
    throw new Error('Order size must be greater than zero')
  }
  if (input.request.targetPrice <= 0n) {
    throw new Error('A V3 order must have a nonzero target price')
  }
  if (input.request.isClose && input.request.marginDelta !== 0n) {
    throw new Error('Close orders must use zero margin delta')
  }

  return Object.freeze({
    kind: 'place-order',
    account,
    calls: Object.freeze([
      Object.freeze({
        to: orderRouter,
        value: 0n,
        data: encodeFunctionData({
          abi: PERPS_ORDER_ROUTER_ABI,
          functionName: 'commitOrder',
          args: [input.request],
        }),
      }),
    ]),
  })
}

/** Uses the V3 ABI independently of the published SDK's older order encoding. */
export function buildProtectedOpenV3Action(input: {
  account: `0x${string}`; book: `0x${string}`; request: PerpsOrderRequestV3;
  params: { takeProfitTriggerPrice: bigint; stopLossTriggerPrice: bigint }
}): PerpsActionPlan {
  validateTiming(input.request)
  buildPlaceOrderV3Action({ account: input.account, orderRouter: input.book, request: input.request })
  if (input.params.takeProfitTriggerPrice === 0n && input.params.stopLossTriggerPrice === 0n) throw new Error('At least one protection trigger is required')
  if (input.request.isClose) throw new Error('Protected orders must open a position')
  return Object.freeze({ kind: 'place-protected-order', account: getAddress(input.account), calls: Object.freeze([
    Object.freeze({ to: getAddress(input.book), value: 0n, data: encodeFunctionData({
      abi: PERPS_POSITION_PROTECTION_BOOK_ABI, functionName: 'commitOpenOrderWithProtection', args: [input.request, input.params],
    }) }),
  ]) })
}

function validateTiming(request: PerpsOrderRequestV3): void {
  if (request.bounds.submitBy <= 0n || request.bounds.submitBy >= 1n << 64n ||
    !Number.isInteger(request.bounds.executionWindowSeconds) || request.bounds.executionWindowSeconds <= 0 || request.bounds.executionWindowSeconds > 3600) {
    throw new Error('Invalid V3 order timing')
  }
}
