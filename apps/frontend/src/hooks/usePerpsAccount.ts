import { useMemo } from 'react'
import { type Address, zeroAddress } from 'viem'
import { useAccount, useReadContracts } from 'wagmi'
import { ERC20_ABI, PERPS_MARGIN_CLEARINGHOUSE_ABI, PERPS_ORDER_ROUTER_ABI, PERPS_PUBLIC_LENS_ABI } from '../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { formatDisplayDxyPrice, formatPerpsUsdc, formatSignedPerpsUsdc, perpsSideToDirection, sizeDeltaToNotionalUsdc } from '../utils/perps'

interface ContractResult {
  status: 'failure' | 'success'
  result?: unknown
}

export interface PerpsPendingOrder {
  orderId: bigint
  side: number
  direction: 'long' | 'short'
  sizeDelta: bigint
  marginDeltaUsdc: bigint
  acceptablePrice: bigint
  isReduceOnly: boolean
  status: number
  estimatedNotionalUsdc?: bigint
  commitTime?: bigint
  expiryTime?: bigint
}

export interface PerpsPosition {
  exists: boolean
  side: number
  direction: 'long' | 'short'
  size: bigint
  entryPrice: bigint
  marginUsdc: bigint
  unrealizedPnlUsdc: bigint
  maintenanceMarginUsdc: bigint
  liquidatable: boolean
  estimatedNotionalUsdc?: bigint
}

function readResult(data: readonly ContractResult[] | undefined, index: number): unknown {
  const item = data?.[index]
  if (item?.status !== 'success') return undefined
  return item.result
}

function tupleValue(value: unknown, index: number, key: string): unknown {
  if (value && typeof value === 'object' && key in value) {
    return (value as Record<string, unknown>)[key]
  }

  if (Array.isArray(value)) {
    return value[index]
  }

  return undefined
}

function parsePosition(value: unknown, markPrice?: bigint): PerpsPosition | undefined {
  if (!value) return undefined

  const exists = Boolean(tupleValue(value, 0, 'exists'))
  const side = Number(tupleValue(value, 1, 'side') ?? 0)
  const size = tupleValue(value, 2, 'size') as bigint | undefined ?? 0n

  return {
    exists,
    side,
    direction: perpsSideToDirection(side),
    size,
    entryPrice: tupleValue(value, 3, 'entryPrice') as bigint | undefined ?? 0n,
    marginUsdc: tupleValue(value, 4, 'marginUsdc') as bigint | undefined ?? 0n,
    unrealizedPnlUsdc: tupleValue(value, 5, 'unrealizedPnlUsdc') as bigint | undefined ?? 0n,
    maintenanceMarginUsdc: tupleValue(value, 6, 'maintenanceMarginUsdc') as bigint | undefined ?? 0n,
    liquidatable: Boolean(tupleValue(value, 7, 'liquidatable')),
    estimatedNotionalUsdc: sizeDeltaToNotionalUsdc(size, markPrice),
  }
}

function parsePendingOrders(value: unknown, markPrice?: bigint): PerpsPendingOrder[] {
  if (!Array.isArray(value)) return []

  return value.map((item) => {
    const side = Number(tupleValue(item, 1, 'side') ?? 0)
    const sizeDelta = tupleValue(item, 2, 'sizeDelta') as bigint | undefined ?? 0n

    return {
      orderId: tupleValue(item, 0, 'orderId') as bigint | undefined ?? 0n,
      side,
      direction: perpsSideToDirection(side),
      sizeDelta,
      marginDeltaUsdc: tupleValue(item, 3, 'marginDeltaUsdc') as bigint | undefined ?? 0n,
      acceptablePrice: tupleValue(item, 4, 'acceptablePrice') as bigint | undefined ?? 0n,
      isReduceOnly: Boolean(tupleValue(item, 5, 'isReduceOnly')),
      status: Number(tupleValue(item, 6, 'status') ?? 0),
      estimatedNotionalUsdc: sizeDeltaToNotionalUsdc(sizeDelta, markPrice),
    }
  })
}

function parsePendingOrderCommitTime(value: unknown): bigint | undefined {
  const pending = tupleValue(value, 0, 'pending')
  const commitTime = tupleValue(pending, 6, 'commitTime')
  if (typeof commitTime === 'bigint') return commitTime
  if (typeof commitTime === 'number') return BigInt(commitTime)
  if (typeof commitTime === 'string') return BigInt(commitTime)
  return undefined
}

export function usePerpsAccount(markPrice?: bigint) {
  const { address, isConnected } = useAccount()
  const account = address ?? zeroAddress

  const { data, isLoading, error, refetch } = useReadContracts({
    contracts: [
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
        abi: PERPS_PUBLIC_LENS_ABI,
        functionName: 'getTraderAccount',
        args: [account],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
        abi: PERPS_PUBLIC_LENS_ABI,
        functionName: 'getPosition',
        args: [account],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
        abi: PERPS_PUBLIC_LENS_ABI,
        functionName: 'getPendingOrders',
        args: [account],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.usdc,
        abi: ERC20_ABI,
        functionName: 'balanceOf',
        args: [account],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.usdc,
        abi: ERC20_ABI,
        functionName: 'allowance',
        args: [account, PERPS_ARBITRUM_SEPOLIA.marginClearinghouse],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.marginClearinghouse,
        abi: PERPS_MARGIN_CLEARINGHOUSE_ABI,
        functionName: 'getFreeBuyingPowerUsdc',
        args: [account],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
        abi: PERPS_ORDER_ROUTER_ABI,
        functionName: 'maxPendingOrders',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
        abi: PERPS_ORDER_ROUTER_ABI,
        functionName: 'maxOrderAge',
      },
    ],
    query: {
      enabled: isConnected,
      refetchInterval: 15_000,
    },
  })

  const basicPendingOrders = useMemo(
    () => parsePendingOrders(readResult(data, 2), markPrice),
    [data, markPrice]
  )

  const { data: pendingOrderViewsData, isLoading: pendingOrderViewsLoading } = useReadContracts({
    contracts: basicPendingOrders.map((order) => ({
      chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
      address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'getPendingOrderView',
      args: [order.orderId],
    } as const)),
    query: {
      enabled: isConnected && basicPendingOrders.length > 0,
      refetchInterval: 15_000,
    },
  })

  return useMemo(() => {
    const accountView = readResult(data, 0)
    const position = parsePosition(readResult(data, 1), markPrice)
    const walletUsdc = readResult(data, 3) as bigint | undefined
    const marginAllowanceUsdc = readResult(data, 4) as bigint | undefined
    const freeBuyingPowerUsdc = readResult(data, 5) as bigint | undefined
    const maxPendingOrders = readResult(data, 6) as bigint | undefined
    const maxOrderAge = readResult(data, 7) as bigint | undefined
    const withdrawableUsdc = tupleValue(accountView, 1, 'withdrawableUsdc') as bigint | undefined
    const equityUsdc = tupleValue(accountView, 0, 'equityUsdc') as bigint | undefined
    const pendingOrders = basicPendingOrders.map((order, index) => {
      const commitTime = parsePendingOrderCommitTime(readResult(pendingOrderViewsData, index))
      const expiryTime = commitTime !== undefined && maxOrderAge !== undefined
        ? commitTime + maxOrderAge
        : undefined

      return {
        ...order,
        commitTime,
        expiryTime,
      }
    })
    const firstPendingOrderExpiryTime = pendingOrders
      .map((order) => order.expiryTime)
      .filter((expiryTime): expiryTime is bigint => expiryTime !== undefined)
      .sort((a, b) => a < b ? -1 : a > b ? 1 : 0)[0]
    const firstPendingOrderId = pendingOrders
      .filter((order) => order.expiryTime !== undefined)
      .sort((a, b) => {
        const aExpiry = a.expiryTime ?? 0n
        const bExpiry = b.expiryTime ?? 0n
        return aExpiry < bExpiry ? -1 : aExpiry > bExpiry ? 1 : 0
      })[0]?.orderId

    return {
      address: address as Address | undefined,
      isConnected,
      isLoading: isLoading || pendingOrderViewsLoading,
      error,
      refetch,
      walletUsdc,
      marginAllowanceUsdc,
      equityUsdc,
      freeBuyingPowerUsdc,
      withdrawableUsdc,
      pendingOrderMarginUsdc: tupleValue(accountView, 2, 'pendingOrderMarginUsdc') as bigint | undefined,
      pendingExecutionBountyUsdc: tupleValue(accountView, 3, 'pendingExecutionBountyUsdc') as bigint | undefined,
      maxPendingOrders,
      maxOrderAge,
      firstPendingOrderId,
      firstPendingOrderExpiryTime,
      hasOpenPosition: Boolean(tupleValue(accountView, 4, 'hasOpenPosition')) && Boolean(position?.exists),
      liquidatable: Boolean(tupleValue(accountView, 5, 'liquidatable')) || Boolean(position?.liquidatable),
      position,
      pendingOrders,
      display: {
        walletUsdc: formatPerpsUsdc(walletUsdc),
        availableToTrade: formatPerpsUsdc(freeBuyingPowerUsdc ?? withdrawableUsdc),
        equity: formatPerpsUsdc(equityUsdc),
        positionNotional: formatPerpsUsdc(position?.estimatedNotionalUsdc),
        entryPrice: formatDisplayDxyPrice(position?.entryPrice),
        pnl: formatSignedPerpsUsdc(position?.unrealizedPnlUsdc),
      },
    }
  }, [address, basicPendingOrders, data, error, isConnected, isLoading, markPrice, pendingOrderViewsData, pendingOrderViewsLoading, refetch])
}
