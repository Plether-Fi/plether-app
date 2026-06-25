import { useCallback, useEffect, useMemo, useState } from 'react'
import type { Hex } from 'viem'
import { useAccount } from 'wagmi'
import { defaultApiBaseUrl } from '../api/client'
import { formatDisplayDxyPrice, formatPerpsUsdc, formatSignedPerpsUsdc, perpsSideLabel, sizeDeltaToNotionalUsdc } from '../utils/perps'

export interface PerpsOrderHistoryRow {
  orderId: bigint
  time: string
  market: string
  side: string
  type: string
  price: string
  size: string
  status: string
  commitTxHash: Hex
  revealTxHash?: Hex
  failureReason?: string
  executionPriceRaw?: bigint
  vpiUsdcRaw?: bigint
  activitySizeDeltaRaw?: bigint
  activityPriceRaw?: bigint
  activityVpiUsdcRaw?: bigint
}

export interface PerpsTradeHistoryRow {
  time: string
  market: string
  side: string
  price: string
  size: string
  pnl?: string
  txHash: Hex
}

interface PerpsHistoryData {
  orderHistory: PerpsOrderHistoryRow[]
  tradeHistory: PerpsTradeHistoryRow[]
}

interface BackendOrdersResponse {
  data?: {
    orders?: BackendOrderRow[]
  }
}

interface BackendActivityResponse {
  data?: {
    activity?: BackendActivityRow[]
  }
}

interface BackendOrderWaitResponse {
  data?: {
    timedOut?: boolean
    order?: BackendOrderRow | null
  }
}

interface BackendErrorResponse {
  error?: {
    message?: string
  }
}

interface BackendOrderRow {
  orderId?: string
  account?: string
  side?: number
  commitTxHash?: string
  commitBlockNumber?: string
  commitTimestamp?: number
  terminalTxHash?: string
  terminalBlockNumber?: string
  terminalTimestamp?: number
  terminalStatus?: string
  failureReason?: string
  executionPrice?: string
  vpiUsdc?: string
  cleanupActor?: string
  activityType?: string
  activitySizeDelta?: string
  activityPrice?: string
  activityVpiUsdc?: string
  activityPnlUsdc?: string
}

interface BackendActivityRow {
  activityType?: string
  account?: string
  actor?: string
  orderId?: string
  side?: number
  price?: string
  sizeDelta?: string
  amountUsdc?: string
  pnlUsdc?: string
  txHash?: string
  blockNumber?: string
  timestamp?: number
  data?: unknown
}

function shortTime(timestamp: number | undefined): string {
  if (timestamp === undefined) return '--'
  return new Intl.DateTimeFormat(undefined, {
    day: '2-digit',
    month: 'short',
    hour: '2-digit',
    minute: '2-digit',
  }).format(new Date(timestamp * 1000))
}

function perpsApiUrl(path: string): URL {
  const apiBase = defaultApiBaseUrl()
  const normalizedBase = apiBase.endsWith('/') ? apiBase.slice(0, -1) : apiBase
  return new URL(`${normalizedBase}${path}`, window.location.origin)
}

function parseBigInt(value: string | undefined): bigint | undefined {
  if (!value) return undefined
  try {
    return BigInt(value)
  } catch {
    return undefined
  }
}

function asHex(value: string | undefined): Hex | undefined {
  if (!value) return undefined
  return value.startsWith('0x') ? value as Hex : `0x${value}`
}

function orderKind(row: BackendOrderRow): string {
  if (row.activityType === 'Open' || row.activityType === 'Close' || row.activityType === 'Liquidated') {
    return row.activityType
  }
  if (row.terminalStatus === 'Executed') return 'Executed'
  if (row.terminalStatus === 'Expired / Cleaned up') return 'Cleanup'
  return 'Commit'
}

function orderStatus(row: BackendOrderRow): string {
  if (row.terminalStatus === 'Failed' && row.failureReason) {
    return `Failed: ${orderFailureReasonLabel(row.failureReason)}`
  }
  if (row.terminalStatus) return row.terminalStatus
  return 'Committed'
}

function orderFailureReasonLabel(reason: string): string {
  return {
    Expired: 'Expired',
    CloseOnly: 'Close-only',
    SlippageExceeded: 'Slippage exceeded',
    EnginePanic: 'Engine panic',
    AccountLiquidated: 'Account liquidated',
    EngineRevert: 'Engine rejected',
  }[reason] ?? reason
}

function isUnexecutedTerminalOrder(row: BackendOrderRow): boolean {
  return row.terminalStatus === 'Failed' || row.terminalStatus === 'Expired / Cleaned up'
}

function orderSize(row: BackendOrderRow): string {
  const sizeDelta = parseBigInt(row.activitySizeDelta)
  const price = parseBigInt(row.activityPrice ?? row.executionPrice)
  const notional = sizeDeltaToNotionalUsdc(sizeDelta, price)
  if (notional !== undefined) return formatPerpsUsdc(notional)
  return isUnexecutedTerminalOrder(row) ? 'Not executed' : '--'
}

function orderPrice(row: BackendOrderRow): string {
  const price = parseBigInt(row.executionPrice ?? row.activityPrice)
  if (price !== undefined) return formatDisplayDxyPrice(price)
  return isUnexecutedTerminalOrder(row) ? 'Not executed' : '--'
}

function mapOrderRow(row: BackendOrderRow): PerpsOrderHistoryRow | undefined {
  const orderId = parseBigInt(row.orderId)
  const commitTxHash = asHex(row.commitTxHash)
  if (orderId === undefined || commitTxHash === undefined) return undefined
  const executionPriceRaw = parseBigInt(row.executionPrice)
  const vpiUsdcRaw = parseBigInt(row.vpiUsdc)
  const activitySizeDeltaRaw = parseBigInt(row.activitySizeDelta)
  const activityPriceRaw = parseBigInt(row.activityPrice)
  const activityVpiUsdcRaw = parseBigInt(row.activityVpiUsdc)

  return {
    orderId,
    time: shortTime(row.terminalTimestamp ?? row.commitTimestamp),
    market: 'plDXY Perp',
    side: perpsSideLabel(row.side),
    type: orderKind(row),
    price: orderPrice(row),
    size: orderSize(row),
    status: orderStatus(row),
    commitTxHash,
    revealTxHash: asHex(row.terminalTxHash),
    failureReason: row.failureReason,
    executionPriceRaw,
    vpiUsdcRaw,
    activitySizeDeltaRaw,
    activityPriceRaw,
    activityVpiUsdcRaw,
  }
}

function activityMarket(activityType: string | undefined): string {
  if (activityType === 'Deposit' || activityType === 'Withdraw') return 'Margin Account'
  return 'plDXY Perp'
}

function activitySide(row: BackendActivityRow): string {
  const side = perpsSideLabel(row.side)
  switch (row.activityType) {
    case 'Open':
      return `Open ${side}`
    case 'Close':
      return `Close ${side}`
    case 'Liquidated':
      return `Liquidated ${side}`
    case 'Deposit':
      return 'Deposit'
    case 'Withdraw':
      return 'Withdraw'
    case 'Add margin':
      return 'Add margin'
    case 'Cleaned up expired order':
      return 'Cleaned up expired order'
    default:
      return row.activityType ?? 'Activity'
  }
}

function activityPrice(row: BackendActivityRow): string {
  const price = parseBigInt(row.price)
  return price === undefined ? '--' : formatDisplayDxyPrice(price)
}

function activitySize(row: BackendActivityRow): string {
  if (row.activityType === 'Cleaned up expired order') return '--'

  const amountUsdc = parseBigInt(row.amountUsdc)
  if (amountUsdc !== undefined) return formatPerpsUsdc(amountUsdc)

  const sizeDelta = parseBigInt(row.sizeDelta)
  const price = parseBigInt(row.price)
  const notional = sizeDeltaToNotionalUsdc(sizeDelta, price)
  return notional === undefined ? '--' : formatPerpsUsdc(notional)
}

function activityResult(row: BackendActivityRow): string | undefined {
  if (row.activityType === 'Cleaned up expired order' && row.orderId) return `Order ${row.orderId}`
  if (row.activityType === 'Liquidated') {
    const keeperBounty = parseBigInt(row.amountUsdc)
    return keeperBounty === undefined ? undefined : `Liquidation reward ${formatPerpsUsdc(keeperBounty)}`
  }

  const pnl = parseBigInt(row.pnlUsdc)
  return pnl === undefined ? undefined : formatSignedPerpsUsdc(pnl)
}

function mapActivityRow(row: BackendActivityRow): PerpsTradeHistoryRow | undefined {
  const txHash = asHex(row.txHash)
  if (!txHash) return undefined

  return {
    time: shortTime(row.timestamp),
    market: activityMarket(row.activityType),
    side: activitySide(row),
    price: activityPrice(row),
    size: activitySize(row),
    pnl: activityResult(row),
    txHash,
  }
}

async function fetchJson<T>(url: URL, signal?: AbortSignal): Promise<T> {
  let response: Response
  try {
    response = await fetch(url, { signal })
  } catch (error) {
    if (error instanceof DOMException && error.name === 'AbortError') {
      throw error
    }
    throw new Error(
      `Could not reach backend history API. Check that the backend and plether-perps-indexer are running. ${
        error instanceof Error ? error.message : ''
      }`.trim()
    )
  }

  if (!response.ok) {
    const parsed = await response.json().catch(() => undefined) as BackendErrorResponse | undefined
    throw new Error(parsed?.error?.message ?? `Backend history API returned HTTP ${response.status.toString()}`)
  }

  return await response.json() as T
}

export async function waitForPerpsOrderTerminal({
  accountAddress,
  orderId,
  timeoutSeconds = 60,
  signal,
}: {
  accountAddress?: string
  orderId: bigint
  timeoutSeconds?: number
  signal?: AbortSignal
}): Promise<{ timedOut: boolean; order?: PerpsOrderHistoryRow }> {
  const waitUrl = perpsApiUrl(`/perps/orders/${orderId.toString()}/wait`)
  waitUrl.searchParams.set('timeoutSeconds', String(timeoutSeconds))
  if (accountAddress) {
    waitUrl.searchParams.set('account', accountAddress)
  }

  const response = await fetchJson<BackendOrderWaitResponse>(waitUrl, signal)
  const order = response.data?.order ? mapOrderRow(response.data.order) : undefined
  return {
    timedOut: Boolean(response.data?.timedOut),
    order,
  }
}

async function fetchPerpsHistory(accountAddress: string): Promise<PerpsHistoryData> {
  const ordersUrl = perpsApiUrl(`/perps/accounts/${accountAddress}/orders`)
  ordersUrl.searchParams.set('limit', '30')
  const activityUrl = perpsApiUrl(`/perps/accounts/${accountAddress}/activity`)
  activityUrl.searchParams.set('limit', '30')

  const [ordersResponse, activityResponse] = await Promise.all([
    fetchJson<BackendOrdersResponse>(ordersUrl),
    fetchJson<BackendActivityResponse>(activityUrl),
  ])

  return {
    orderHistory: (ordersResponse.data?.orders ?? []).flatMap((row) => {
      const mapped = mapOrderRow(row)
      return mapped ? [mapped] : []
    }),
    tradeHistory: (activityResponse.data?.activity ?? []).flatMap((row) => {
      const mapped = mapActivityRow(row)
      return mapped ? [mapped] : []
    }),
  }
}

export function usePerpsHistory() {
  const { address, isConnected } = useAccount()
  const [orderHistory, setOrderHistory] = useState<PerpsOrderHistoryRow[]>([])
  const [tradeHistory, setTradeHistory] = useState<PerpsTradeHistoryRow[]>([])
  const [isLoading, setIsLoading] = useState(false)
  const [error, setError] = useState<Error | undefined>()

  const refetch = useCallback(async () => {
    if (!isConnected || !address) {
      setOrderHistory([])
      setTradeHistory([])
      setError(undefined)
      setIsLoading(false)
      return
    }

    setIsLoading(true)
    setError(undefined)

    try {
      const nextHistory = await fetchPerpsHistory(address)
      setOrderHistory(nextHistory.orderHistory)
      setTradeHistory(nextHistory.tradeHistory)
      setIsLoading(false)
    } catch (cause) {
      setError(cause instanceof Error ? cause : new Error(String(cause)))
      setOrderHistory([])
      setTradeHistory([])
      setIsLoading(false)
    }
  }, [address, isConnected])

  useEffect(() => {
    if (!isConnected || !address) {
      window.setTimeout(() => {
        setOrderHistory([])
        setTradeHistory([])
        setError(undefined)
        setIsLoading(false)
      }, 0)
      return undefined
    }

    let cancelled = false
    const accountAddress = address

    async function loadHistory() {
      setIsLoading(true)
      setError(undefined)

      try {
        const nextHistory = await fetchPerpsHistory(accountAddress)

        if (!cancelled) {
          setOrderHistory(nextHistory.orderHistory)
          setTradeHistory(nextHistory.tradeHistory)
          setIsLoading(false)
        }
      } catch (cause) {
        if (!cancelled) {
          setError(cause instanceof Error ? cause : new Error(String(cause)))
          setOrderHistory([])
          setTradeHistory([])
          setIsLoading(false)
        }
      }
    }

    void loadHistory()
    const interval = window.setInterval(() => {
      void loadHistory()
    }, 30_000)

    return () => {
      cancelled = true
      window.clearInterval(interval)
    }
  }, [address, isConnected])

  return useMemo(() => ({
    orderHistory,
    tradeHistory,
    isLoading,
    error,
    refetch,
  }), [error, isLoading, orderHistory, refetch, tradeHistory])
}
