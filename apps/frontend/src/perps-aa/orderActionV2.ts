import { encodeFunctionData, getAddress } from 'viem'
import type { PerpsActionPlan } from '@plether/perps-aa-client'
import { PERPS_ORDER_ROUTER_ABI } from '../contracts/abis'
import type { PerpsOrderRequestV2 } from '../contracts/perpsOrderV2'

export function buildPlaceOrderV2Action(input: {
  account: `0x${string}`
  orderRouter: `0x${string}`
  request: PerpsOrderRequestV2
}): PerpsActionPlan {
  const account = getAddress(input.account)
  const orderRouter = getAddress(input.orderRouter)
  if (input.request.sizeDelta <= 0n) {
    throw new Error('Order size must be greater than zero')
  }
  if (input.request.targetPrice <= 0n) {
    throw new Error('A V2 order must have a nonzero target price')
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
