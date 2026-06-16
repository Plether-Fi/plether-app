import { useEffect, useMemo, useState } from 'react'
import { parseAbiItem, type Address, type Hex } from 'viem'
import { useAccount, usePublicClient } from 'wagmi'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import { formatDisplayDxyPrice, formatPerpsUsdc, formatSignedPerpsUsdc, perpsSideLabel, sizeDeltaToNotionalUsdc } from '../utils/perps'
import { getPerpsOrderFailureMessage } from '../utils/perpsErrors'

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

interface OrderEventState {
  orderId: bigint
  account: Address
  side: number
  commitBlockNumber: bigint
  commitTxHash: Hex
  executionBlockNumber?: bigint
  executionTxHash?: Hex
  executionPrice?: bigint
  failureReason?: number
}

interface TradeEventState {
  kind: 'Open' | 'Close'
  account: Address
  side: number
  sizeDelta: bigint
  price: bigint
  marginDelta?: bigint
  pnl?: bigint
  blockNumber: bigint
  txHash: Hex
}

const ORDER_COMMITTED_EVENT = parseAbiItem('event OrderCommitted(uint64 indexed orderId, address indexed account, uint8 side)')
const ORDER_EXECUTED_EVENT = parseAbiItem('event OrderExecuted(uint64 indexed orderId, uint256 executionPrice)')
const ORDER_FAILED_EVENT = parseAbiItem('event OrderFailed(uint64 indexed orderId, uint8 reason)')
const POSITION_OPENED_EVENT = parseAbiItem('event PositionOpened(address indexed account, uint8 side, uint256 sizeDelta, uint256 price, uint256 marginDelta)')
const POSITION_CLOSED_EVENT = parseAbiItem('event PositionClosed(address indexed account, uint8 side, uint256 sizeDelta, uint256 price, int256 pnl)')
const HISTORY_BLOCK_LOOKBACK = BigInt(import.meta.env.VITE_PERPS_HISTORY_BLOCK_LOOKBACK ?? '50000')

function shortTime(timestamp: bigint | undefined): string {
  if (timestamp === undefined) return '--'
  return new Intl.DateTimeFormat(undefined, {
    day: '2-digit',
    month: 'short',
    hour: '2-digit',
    minute: '2-digit',
  }).format(new Date(Number(timestamp) * 1000))
}

function normalizeAddress(value: Address): string {
  return value.toLowerCase()
}

function orderKind(row: OrderEventState, trade?: TradeEventState): string {
  if (trade?.kind) return trade.kind
  return row.executionPrice ? 'Executed' : 'Commit'
}

function orderStatus(row: OrderEventState): string {
  if (row.failureReason !== undefined) return getPerpsOrderFailureMessage(row.failureReason)
  if (row.executionPrice !== undefined) return 'Executed'
  return 'Committed'
}

function sortByNewestBlock<T extends { blockNumber?: bigint; commitBlockNumber?: bigint; executionBlockNumber?: bigint }>(rows: T[]): T[] {
  return [...rows].sort((a, b) => {
    const aBlock = a.executionBlockNumber ?? a.commitBlockNumber ?? a.blockNumber ?? 0n
    const bBlock = b.executionBlockNumber ?? b.commitBlockNumber ?? b.blockNumber ?? 0n
    return aBlock > bBlock ? -1 : aBlock < bBlock ? 1 : 0
  })
}

export function usePerpsHistory(markPrice?: bigint) {
  const { address, isConnected } = useAccount()
  const publicClient = usePublicClient({ chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID })
  const [orderHistory, setOrderHistory] = useState<PerpsOrderHistoryRow[]>([])
  const [tradeHistory, setTradeHistory] = useState<PerpsTradeHistoryRow[]>([])
  const [isLoading, setIsLoading] = useState(false)
  const [error, setError] = useState<Error | undefined>()

  useEffect(() => {
    if (!isConnected || !address || !publicClient) {
      setOrderHistory([])
      setTradeHistory([])
      setError(undefined)
      setIsLoading(false)
      return undefined
    }

    let cancelled = false
    const account = address

    async function loadHistory() {
      setIsLoading(true)
      setError(undefined)

      try {
        const latestBlock = await publicClient!.getBlockNumber()
        const fromBlock = latestBlock > HISTORY_BLOCK_LOOKBACK ? latestBlock - HISTORY_BLOCK_LOOKBACK : 0n
        const [commitLogs, executedLogs, failedLogs, openedLogs, closedLogs] = await Promise.all([
          publicClient!.getLogs({
            address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
            event: ORDER_COMMITTED_EVENT,
            args: { account },
            fromBlock,
            toBlock: latestBlock,
          }),
          publicClient!.getLogs({
            address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
            event: ORDER_EXECUTED_EVENT,
            fromBlock,
            toBlock: latestBlock,
          }),
          publicClient!.getLogs({
            address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
            event: ORDER_FAILED_EVENT,
            fromBlock,
            toBlock: latestBlock,
          }),
          publicClient!.getLogs({
            address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
            event: POSITION_OPENED_EVENT,
            args: { account },
            fromBlock,
            toBlock: latestBlock,
          }),
          publicClient!.getLogs({
            address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
            event: POSITION_CLOSED_EVENT,
            args: { account },
            fromBlock,
            toBlock: latestBlock,
          }),
        ])

        const accountLower = normalizeAddress(account)
        const orderById = new Map<bigint, OrderEventState>()
        for (const log of commitLogs) {
          if (!log.args.orderId || !log.args.account || log.args.side === undefined) continue
          orderById.set(log.args.orderId, {
            orderId: log.args.orderId,
            account: log.args.account,
            side: Number(log.args.side),
            commitBlockNumber: log.blockNumber,
            commitTxHash: log.transactionHash,
          })
        }
        for (const log of executedLogs) {
          if (!log.args.orderId) continue
          const row = orderById.get(log.args.orderId)
          if (!row) continue
          row.executionBlockNumber = log.blockNumber
          row.executionTxHash = log.transactionHash
          row.executionPrice = log.args.executionPrice
        }
        for (const log of failedLogs) {
          if (!log.args.orderId) continue
          const row = orderById.get(log.args.orderId)
          if (!row) continue
          row.executionBlockNumber = log.blockNumber
          row.executionTxHash = log.transactionHash
          row.failureReason = log.args.reason === undefined ? undefined : Number(log.args.reason)
        }

        const trades: TradeEventState[] = [
          ...openedLogs.flatMap((log) => {
            if (!log.args.account) return []
            return [{
              kind: 'Open' as const,
              account: log.args.account,
              side: Number(log.args.side ?? 0),
              sizeDelta: log.args.sizeDelta ?? 0n,
              price: log.args.price ?? 0n,
              marginDelta: log.args.marginDelta,
              blockNumber: log.blockNumber,
              txHash: log.transactionHash,
            }]
          }),
          ...closedLogs.flatMap((log) => {
            if (!log.args.account) return []
            return [{
              kind: 'Close' as const,
              account: log.args.account,
              side: Number(log.args.side ?? 0),
              sizeDelta: log.args.sizeDelta ?? 0n,
              price: log.args.price ?? 0n,
              pnl: log.args.pnl,
              blockNumber: log.blockNumber,
              txHash: log.transactionHash,
            }]
          }),
        ].filter((trade) => normalizeAddress(trade.account) === accountLower)

        const blockNumbers = new Set<bigint>()
        for (const row of orderById.values()) blockNumbers.add(row.executionBlockNumber ?? row.commitBlockNumber)
        for (const trade of trades) blockNumbers.add(trade.blockNumber)
        const blockTimestamps = new Map<bigint, bigint>()
        await Promise.all([...blockNumbers].map(async (blockNumber) => {
          const block = await publicClient!.getBlock({ blockNumber })
          blockTimestamps.set(blockNumber, block.timestamp)
        }))

        const tradeByTxHash = new Map<string, TradeEventState>()
        for (const trade of trades) tradeByTxHash.set(trade.txHash.toLowerCase(), trade)

        const nextOrderHistory = sortByNewestBlock([...orderById.values()])
          .slice(0, 30)
          .map((row) => {
            const trade = row.executionTxHash ? tradeByTxHash.get(row.executionTxHash.toLowerCase()) : undefined
            const blockNumber = row.executionBlockNumber ?? row.commitBlockNumber
            return {
              orderId: row.orderId,
              time: shortTime(blockTimestamps.get(blockNumber)),
              market: 'plDXY Perp',
              side: perpsSideLabel(row.side),
              type: orderKind(row, trade),
              price: row.executionPrice ? formatDisplayDxyPrice(row.executionPrice) : trade?.price ? formatDisplayDxyPrice(trade.price) : '--',
              size: trade ? formatPerpsUsdc(sizeDeltaToNotionalUsdc(trade.sizeDelta, trade.price)) : '--',
              status: orderStatus(row),
              commitTxHash: row.commitTxHash,
              revealTxHash: row.executionTxHash,
            }
          })

        const nextTradeHistory = sortByNewestBlock(trades)
          .slice(0, 30)
          .map((trade) => ({
            time: shortTime(blockTimestamps.get(trade.blockNumber)),
            market: 'plDXY Perp',
            side: `${trade.kind} ${perpsSideLabel(trade.side)}`,
            price: formatDisplayDxyPrice(trade.price),
            size: formatPerpsUsdc(sizeDeltaToNotionalUsdc(trade.sizeDelta, trade.price ?? markPrice)),
            pnl: trade.pnl === undefined ? undefined : formatSignedPerpsUsdc(trade.pnl),
            txHash: trade.txHash,
          }))

        if (!cancelled) {
          setOrderHistory(nextOrderHistory)
          setTradeHistory(nextTradeHistory)
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
  }, [address, isConnected, markPrice, publicClient])

  return useMemo(() => ({
    orderHistory,
    tradeHistory,
    isLoading,
    error,
  }), [error, isLoading, orderHistory, tradeHistory])
}
