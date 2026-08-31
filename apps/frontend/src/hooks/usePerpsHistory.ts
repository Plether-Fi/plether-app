import { useCallback, useEffect, useMemo } from 'react'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import type { Hex } from 'viem'
import { getScopedApiBaseUrl } from '../api/client'
import { executionModeOracleFrozen } from '../contracts/perpsOrderV2'
import { usePerpsIdentity } from '../perps-aa'
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
  account: Hex
  clientOrderId: Hex
  commitTxHash?: Hex
  revealTxHash?: Hex
  receiptHash?: Hex
  terminalBlockNumberRaw?: bigint
  terminalBlockHash?: Hex
  terminalReason?: string
  pendingReason?: string
  executionMode?: string
  failedConstraint?: string
  receiptEconomics?: BackendReceiptEconomics
  executionPriceRaw?: bigint
  executionOraclePriceRaw?: bigint
  executionOracleFrozen?: boolean
  oracleMinPublishTimeRaw?: bigint
  oracleMaxPublishTimeRaw?: bigint
  oracleDerivationVersion?: number
  vpiUsdcRaw?: bigint
  frozenCloseSpreadUsdcRaw?: bigint
  executionEconomicsVersion?: number
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

interface PerpsOrdersHistoryData {
  orderHistory: PerpsOrderHistoryRow[]
  ordersIndexedThroughBlockRaw?: bigint
}

interface PerpsActivityHistoryData {
  tradeHistory: PerpsTradeHistoryRow[]
}

const EMPTY_PERPS_HISTORY = {
  orderHistory: [],
  tradeHistory: [],
} satisfies PerpsOrdersHistoryData & PerpsActivityHistoryData

const PERPS_HISTORY_REFETCH_INTERVAL_MS = 30_000

const perpsHistoryQueryKeys = {
  orders: (accountAddress: string) => [
    'perps',
    'history',
    accountAddress.toLowerCase(),
    'orders',
  ] as const,
  activity: (accountAddress: string) => [
    'perps',
    'history',
    accountAddress.toLowerCase(),
    'activity',
  ] as const,
}

export interface UsePerpsHistoryOptions {
  activityEnabled?: boolean
}

interface BackendOrdersResponse {
  data?: {
    orders?: BackendOrderRow[]
    indexedThroughBlock?: string
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
  clientOrderId?: string
  side?: number
  commitTxHash?: string
  commitBlockNumber?: string
  commitTimestamp?: number
  terminalTxHash?: string
  terminalBlockNumber?: string
  terminalBlockHash?: string
  terminalTimestamp?: number
  terminalStatus?: string
  terminalReason?: string
  pendingReason?: string
  executionMode?: string
  failedConstraint?: string
  receiptHash?: string
  receiptEconomics?: BackendReceiptEconomics
  executionPrice?: string
  executionOraclePrice?: string
  executionOracleFrozen?: boolean
  oracleMinPublishTime?: string
  oracleMaxPublishTime?: string
  oracleDerivationVersion?: number
  vpiUsdc?: string
  frozenCloseSpreadUsdc?: string
  executionEconomicsVersion?: number
  cleanupActor?: string
  activityType?: string
  activitySizeDelta?: string
  activityPrice?: string
  activityVpiUsdc?: string
  activityPnlUsdc?: string
}

interface BackendReceiptEconomics {
  executionNotionalUsdc?: string
  realizedPnlUsdc?: string
  vpiUsdc?: string
  carryUsdc?: string
  executionFeeUsdc?: string
  frozenSpreadUsdc?: string
  actionChargeAssessedUsdc?: string
  actionChargeCollectedUsdc?: string
  grossAccountDebitUsdc?: string
  preSettlementBalanceUsdc?: string
  postSettlementBalanceUsdc?: string
  preTraderClaimBalanceUsdc?: string
  postTraderClaimBalanceUsdc?: string
  postPositionSize?: string
  postPositionMarginUsdc?: string
  postPositionEquityUsdc?: string
  postLeverageBps?: string
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
  const apiBase = getScopedApiBaseUrl('perps')
  const normalizedBase = apiBase.endsWith('/') ? apiBase.slice(0, -1) : apiBase
  return new URL(`${normalizedBase}${path}`, window.location.origin)
}

function parseBigInt(value: string | number | undefined): bigint | undefined {
  if (value === undefined || value === '') return undefined
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
  if (row.terminalStatus === 'Failed' && row.terminalReason) {
    return `Failed: ${orderFailureReasonLabel(row.terminalReason)}`
  }
  if (row.terminalStatus) return row.terminalStatus
  return 'Committed'
}

function orderFailureReasonLabel(reason: string): string {
  return {
    Expired: 'Expired',
    Slippage: 'Slippage',
    ConfigMismatch: 'Config mismatch',
    'Config mismatch': 'Config mismatch',
    ExecutionModeDisallowed: 'Mode disallowed',
    'Mode disallowed': 'Mode disallowed',
    RiskOff: 'Risk off',
    'Risk off': 'Risk off',
    PlannerRejected: 'Planner rejected',
    'Planner rejected': 'Planner rejected',
    ConstraintViolation: 'Constraint violation',
    'Constraint violation': 'Constraint violation',
    AccountLiquidated: 'Account liquidated',
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
  const account = asHex(row.account)
  const clientOrderId = asHex(row.clientOrderId)
  const commitTxHash = asHex(row.commitTxHash)
  if (
    orderId === undefined ||
    account === undefined ||
    clientOrderId === undefined
  ) return undefined
  const executionPriceRaw = parseBigInt(row.executionPrice)
  const executionOraclePriceRaw = parseBigInt(row.executionOraclePrice)
  const oracleMinPublishTimeRaw = parseBigInt(row.oracleMinPublishTime)
  const oracleMaxPublishTimeRaw = parseBigInt(row.oracleMaxPublishTime)
  const vpiUsdcRaw = parseBigInt(row.receiptEconomics?.vpiUsdc)
  const frozenCloseSpreadUsdcRaw = parseBigInt(
    row.receiptEconomics?.frozenSpreadUsdc
  )
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
    account,
    clientOrderId,
    commitTxHash,
    revealTxHash: asHex(row.terminalTxHash),
    receiptHash: asHex(row.receiptHash),
    terminalBlockNumberRaw: parseBigInt(row.terminalBlockNumber),
    terminalBlockHash: asHex(row.terminalBlockHash),
    terminalReason: row.terminalReason,
    pendingReason: row.pendingReason,
    executionMode: row.executionMode,
    failedConstraint: row.failedConstraint,
    receiptEconomics: row.receiptEconomics,
    executionPriceRaw,
    executionOraclePriceRaw,
    executionOracleFrozen: typeof row.executionOracleFrozen === 'boolean'
      ? row.executionOracleFrozen
      : executionModeOracleFrozen(row.executionMode),
    oracleMinPublishTimeRaw,
    oracleMaxPublishTimeRaw,
    oracleDerivationVersion: row.oracleDerivationVersion,
    vpiUsdcRaw,
    frozenCloseSpreadUsdcRaw,
    executionEconomicsVersion: row.executionEconomicsVersion,
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
    if (error instanceof Error && error.name === 'AbortError') {
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

async function fetchPerpsOrdersHistory(
  accountAddress: string,
  signal?: AbortSignal
): Promise<PerpsOrdersHistoryData> {
  const ordersUrl = perpsApiUrl(`/perps/accounts/${accountAddress}/orders`)
  ordersUrl.searchParams.set('limit', '30')
  const ordersResponse = await fetchJson<BackendOrdersResponse>(ordersUrl, signal)

  return {
    orderHistory: (ordersResponse.data?.orders ?? []).flatMap((row) => {
      const mapped = mapOrderRow(row)
      return mapped ? [mapped] : []
    }),
    ordersIndexedThroughBlockRaw: parseBigInt(
      ordersResponse.data?.indexedThroughBlock
    ),
  }
}

async function fetchPerpsActivityHistory(
  accountAddress: string,
  signal?: AbortSignal
): Promise<PerpsActivityHistoryData> {
  const activityUrl = perpsApiUrl(`/perps/accounts/${accountAddress}/activity`)
  activityUrl.searchParams.set('limit', '30')
  const activityResponse = await fetchJson<BackendActivityResponse>(activityUrl, signal)

  return {
    tradeHistory: (activityResponse.data?.activity ?? []).flatMap((row) => {
      const mapped = mapActivityRow(row)
      return mapped ? [mapped] : []
    }),
  }
}

export function usePerpsHistory({
  activityEnabled = false,
}: UsePerpsHistoryOptions = {}) {
  const { ownerAddress, accountAddress } = usePerpsIdentity()
  const queryClient = useQueryClient()
  const historyAccountAddress = ownerAddress === undefined
    ? undefined
    : accountAddress
  const ordersQuery = useQuery({
    queryKey: perpsHistoryQueryKeys.orders(historyAccountAddress ?? 'disconnected'),
    queryFn: ({ signal }) => {
      if (!historyAccountAddress) {
        throw new Error('A Perps account address is required to load order history')
      }
      return fetchPerpsOrdersHistory(historyAccountAddress, signal)
    },
    enabled: historyAccountAddress !== undefined,
    staleTime: 10_000,
    refetchInterval: PERPS_HISTORY_REFETCH_INTERVAL_MS,
    // The interval is the retry policy; avoid multiplying traffic during an
    // indexer/backend outage via the app-wide retry default.
    retry: false,
  })
  const activityQuery = useQuery({
    queryKey: perpsHistoryQueryKeys.activity(historyAccountAddress ?? 'disconnected'),
    queryFn: ({ signal }) => {
      if (!historyAccountAddress) {
        throw new Error('A Perps account address is required to load activity history')
      }
      return fetchPerpsActivityHistory(historyAccountAddress, signal)
    },
    enabled: historyAccountAddress !== undefined && activityEnabled,
    // Always refresh when the tab is reopened, while retaining cached rows during
    // that background refresh.
    staleTime: 0,
    refetchInterval: activityEnabled
      ? PERPS_HISTORY_REFETCH_INTERVAL_MS
      : false,
    retry: false,
  })
  const refetchOrdersHistory = ordersQuery.refetch
  const refetchActivityHistory = activityQuery.refetch

  useEffect(() => {
    if (activityEnabled || !historyAccountAddress) return

    void queryClient.cancelQueries({
      queryKey: perpsHistoryQueryKeys.activity(historyAccountAddress),
      exact: true,
    })
  }, [activityEnabled, historyAccountAddress, queryClient])

  const refetch = useCallback(async () => {
    if (!historyAccountAddress) return

    const requests: Promise<unknown>[] = [refetchOrdersHistory()]
    if (activityEnabled) requests.push(refetchActivityHistory())
    await Promise.all(requests)
  }, [
    activityEnabled,
    historyAccountAddress,
    refetchActivityHistory,
    refetchOrdersHistory,
  ])

  const orderHistory = ordersQuery.data?.orderHistory
    ?? EMPTY_PERPS_HISTORY.orderHistory
  const tradeHistory = activityQuery.data?.tradeHistory
    ?? EMPTY_PERPS_HISTORY.tradeHistory
  const ordersIndexedThroughBlockRaw =
    ordersQuery.data?.ordersIndexedThroughBlockRaw
  const orderHistoryError = ordersQuery.error ?? undefined
  const tradeHistoryError = activityEnabled
    ? activityQuery.error ?? undefined
    : undefined
  const isOrderHistoryLoading = ordersQuery.isLoading
  const isTradeHistoryLoading = activityEnabled && activityQuery.isLoading
  const error = orderHistoryError ?? tradeHistoryError
  const isLoading = isOrderHistoryLoading || isTradeHistoryLoading

  return useMemo(() => ({
    orderHistory,
    tradeHistory,
    ordersIndexedThroughBlockRaw,
    isOrderHistoryLoading,
    isTradeHistoryLoading,
    orderHistoryError,
    tradeHistoryError,
    isLoading,
    error,
    refetch,
  }), [
    error,
    isLoading,
    isOrderHistoryLoading,
    isTradeHistoryLoading,
    orderHistory,
    orderHistoryError,
    ordersIndexedThroughBlockRaw,
    refetch,
    tradeHistory,
    tradeHistoryError,
  ])
}
