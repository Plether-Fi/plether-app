import { useCallback } from 'react'
import { useQueryClient } from '@tanstack/react-query'
import { parseEventLogs, type Address, type Hex } from 'viem'
import { useAccount, usePublicClient, useWriteContract } from 'wagmi'
import { ERC20_ABI, PERPS_CFD_ENGINE_ABI, PERPS_CFD_ENGINE_LENS_ABI, PERPS_MARGIN_CLEARINGHOUSE_ABI, PERPS_ORDER_ROUTER_ABI, PERPS_PLETHER_ORACLE_ABI, PERPS_PUBLIC_LENS_ABI } from '../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import {
  directionToPerpsSide,
  fetchPerpsPythUpdatePayloadForWindow,
  fetchPerpsRevealPayload,
  formatPerpsUsdc,
  getPerpsTargetPrice,
  notionalUsdcToSizeDelta,
  type PerpsDirection,
  type PerpsPythUpdatePayload,
} from '../utils/perps'
import { getPerpsCloseInvalidReasonMessage, getPerpsErrorMessage, getPerpsOpenRevertMessage } from '../utils/perpsErrors'

interface CommitOrderInput {
  direction: PerpsDirection
  notionalUsdc: bigint
  sizeDelta?: bigint
  marginUsdc: bigint
  oraclePrice: bigint
  slippagePercent: number
  isClose: boolean
  onWalletRequestStart?: () => void
}

interface CommitOrderResult {
  hash: Hex
  orderId?: bigint
}

interface ExecuteOrderResult {
  hash: Hex
  executionPrice?: bigint
  failedReason?: number
}

interface CleanupExpiredOrderResult {
  hash: Hex
}

type PerpsPublicClient = NonNullable<ReturnType<typeof usePublicClient>>
type PerpsTransactionReceipt = Awaited<ReturnType<PerpsPublicClient['waitForTransactionReceipt']>>
type CommitOrderArgs = readonly [number, bigint, bigint, bigint, boolean]
type BufferedFeeParams =
  | Record<string, never>
  | { maxFeePerGas: bigint; maxPriorityFeePerGas?: bigint }
  | { gasPrice: bigint }
type InjectedEthereumProvider = {
  chainId?: string
  selectedAddress?: string
  isMetaMask?: boolean
  request?: (args: { method: string; params?: unknown[] }) => Promise<unknown>
}

const FEE_ESTIMATE_TIMEOUT_MS = 2_500
const WALLET_PROBE_TIMEOUT_MS = 1_500

function isPerpsCommitDebugEnabled(): boolean {
  if (import.meta.env.MODE === 'test') return false
  if (import.meta.env.DEV) return true

  try {
    return globalThis.localStorage?.getItem('PLETHER_PERPS_DEBUG') === '1'
  } catch {
    return false
  }
}

function debugPerpsCommit(stage: string, details?: Record<string, unknown>): void {
  if (!isPerpsCommitDebugEnabled()) return
  if (details === undefined) {
    console.info(`[perps:commit] ${stage}`)
    return
  }
  console.info(`[perps:commit] ${stage}`, details)
}

function requireClient<T>(client: T | undefined): T {
  if (!client) {
    throw new Error('Wallet client is not ready')
  }
  return client
}

function assertSuccessfulReceipt(receipt: PerpsTransactionReceipt, message: string): PerpsTransactionReceipt {
  if (receipt.status !== 'success') {
    throw new Error(message)
  }
  return receipt
}

function bumpFee(value: bigint): bigint {
  return value + (value / 4n) + 1n
}

function bumpGas(value: bigint): bigint {
  return value + (value / 5n) + 10_000n
}

function withTimeout<T>(promise: Promise<T>, milliseconds: number): Promise<T> {
  return new Promise((resolve, reject) => {
    const timeout = globalThis.setTimeout(() => {
      reject(new Error(`RPC request timed out after ${milliseconds}ms`))
    }, milliseconds)

    promise.then(
      (value) => {
        globalThis.clearTimeout(timeout)
        resolve(value)
      },
      (error: unknown) => {
        globalThis.clearTimeout(timeout)
        reject(error)
      }
    )
  })
}

function getInjectedEthereumProvider(): InjectedEthereumProvider | undefined {
  return (globalThis as unknown as { ethereum?: InjectedEthereumProvider }).ethereum
}

async function probeInjectedWalletProvider(): Promise<void> {
  if (!isPerpsCommitDebugEnabled()) return

  const ethereum = getInjectedEthereumProvider()
  if (!ethereum?.request) {
    debugPerpsCommit('provider-probe:missing-window-ethereum')
    return
  }

  debugPerpsCommit('provider-probe:present', {
    isMetaMask: ethereum.isMetaMask,
    chainId: ethereum.chainId,
    selectedAddress: ethereum.selectedAddress,
  })

  try {
    const chainId = await withTimeout(
      ethereum.request({ method: 'eth_chainId' }),
      WALLET_PROBE_TIMEOUT_MS
    )
    debugPerpsCommit('provider-probe:eth_chainId:success', { chainId })
  } catch (error) {
    debugPerpsCommit('provider-probe:eth_chainId:failed', {
      message: error instanceof Error ? error.message : String(error),
    })
  }

  try {
    const accounts = await withTimeout(
      ethereum.request({ method: 'eth_accounts' }),
      WALLET_PROBE_TIMEOUT_MS
    )
    debugPerpsCommit('provider-probe:eth_accounts:success', { accounts })
  } catch (error) {
    debugPerpsCommit('provider-probe:eth_accounts:failed', {
      message: error instanceof Error ? error.message : String(error),
    })
  }
}

function formatUnixTime(seconds: number | undefined): string | undefined {
  if (seconds === undefined || !Number.isFinite(seconds) || seconds <= 0) return undefined
  return new Date(seconds * 1000).toLocaleString(undefined, {
    month: 'short',
    day: '2-digit',
    hour: '2-digit',
    minute: '2-digit',
    second: '2-digit',
  })
}

function withPythFetchTiming(message: string, payload: PerpsPythUpdatePayload | undefined): string {
  const lowerMessage = message.toLowerCase()
  const isOracleTimingMessage =
    lowerMessage.includes('pyth price data expired') ||
    lowerMessage.includes('stale-price error') ||
    lowerMessage.includes('historical pyth update was unavailable') ||
    lowerMessage.includes('router could not use the historical pyth update') ||
    lowerMessage.includes('historical pyth update was rejected')
  if (!isOracleTimingMessage) {
    return message
  }

  const oldestPublishTime = payload?.publishTimes.length ? Math.min(...payload.publishTimes) : undefined
  const publishLabel = formatUnixTime(oldestPublishTime)
  const fetchedLabel = formatUnixTime(payload?.fetchedAt)
  if (oldestPublishTime === undefined || publishLabel === undefined || fetchedLabel === undefined || payload === undefined) {
    return message
  }

  const newestPublishTime = Math.max(...payload.publishTimes)
  const newestPublishLabel = formatUnixTime(newestPublishTime)
  const publishWindow =
    newestPublishLabel && newestPublishTime !== oldestPublishTime
      ? ` Hermes publish window: ${publishLabel} to ${newestPublishLabel};`
      : ` Hermes publish time: ${publishLabel};`

  return `${message}${publishWindow} app fetch time: ${fetchedLabel}; age at fetch: ${payload.fetchedAt - oldestPublishTime}s; oracle staleness limit: 60s.`
}

function shouldFallbackToHistoricalPythPayload(message: string): boolean {
  const lowerMessage = message.toLowerCase()
  return (
    lowerMessage.includes('historical pyth update was rejected') ||
    lowerMessage.includes('reveal payload unavailable') ||
    lowerMessage.includes('could not fetch cached reveal payload')
  )
}

function readRecordValue(value: unknown, key: string, index: number): unknown {
  if (value && typeof value === 'object' && key in value) {
    return (value as Record<string, unknown>)[key]
  }
  if (Array.isArray(value)) return value[index]
  return undefined
}

function readPendingOrderCommitTime(value: unknown): bigint | undefined {
  const pending = readRecordValue(value, 'pending', 0)
  const commitTime = readRecordValue(pending, 'commitTime', 6)
  if (typeof commitTime === 'bigint') return commitTime
  if (typeof commitTime === 'number') return BigInt(commitTime)
  if (typeof commitTime === 'string') return BigInt(commitTime)
  return undefined
}

function readArrayLength(value: unknown): number {
  if (Array.isArray(value)) return value.length
  return 0
}

function readBoolean(value: unknown, key: string, index: number): boolean | undefined {
  const rawValue = readRecordValue(value, key, index)
  return typeof rawValue === 'boolean' ? rawValue : undefined
}

function readNumber(value: unknown, key: string, index: number): number | undefined {
  const rawValue = readRecordValue(value, key, index)
  if (typeof rawValue === 'number') return rawValue
  if (typeof rawValue === 'bigint') return Number(rawValue)
  if (typeof rawValue === 'string') return Number(rawValue)
  return undefined
}

function readBigInt(value: unknown, key: string, index: number): bigint | undefined {
  const rawValue = readRecordValue(value, key, index)
  if (typeof rawValue === 'bigint') return rawValue
  if (typeof rawValue === 'number') return BigInt(rawValue)
  if (typeof rawValue === 'string') return BigInt(rawValue)
  return undefined
}

function isOrderEventFor(orderEventId: bigint | number | undefined, orderId: bigint): boolean {
  if (orderEventId === undefined) return false
  return BigInt(orderEventId) === orderId
}

function describeTime(seconds: bigint): string {
  return formatUnixTime(Number(seconds)) ?? `${seconds.toString()}`
}

async function getBufferedFeeParams(client: PerpsPublicClient, context = 'transaction'): Promise<BufferedFeeParams> {
  try {
    debugPerpsCommit(`${context}:fee-estimate:eip1559:start`, {
      timeoutMs: FEE_ESTIMATE_TIMEOUT_MS,
    })
    const fees = await withTimeout(client.estimateFeesPerGas(), FEE_ESTIMATE_TIMEOUT_MS)
    if ('maxFeePerGas' in fees && fees.maxFeePerGas !== undefined) {
      const bufferedFees = {
        maxFeePerGas: bumpFee(fees.maxFeePerGas),
        maxPriorityFeePerGas: fees.maxPriorityFeePerGas === undefined
          ? undefined
          : bumpFee(fees.maxPriorityFeePerGas),
      }
      debugPerpsCommit(`${context}:fee-estimate:eip1559:success`, bufferedFees)
      return bufferedFees
    }
  } catch (error) {
    debugPerpsCommit(`${context}:fee-estimate:eip1559:fallback`, {
      reason: error instanceof Error ? error.message : String(error),
    })
    // Fall back to legacy gas price below if the RPC does not expose EIP-1559 fee estimates.
  }

  try {
    debugPerpsCommit(`${context}:fee-estimate:gas-price:start`, {
      timeoutMs: FEE_ESTIMATE_TIMEOUT_MS,
    })
    const legacyFees = {
      gasPrice: bumpFee(await withTimeout(client.getGasPrice(), FEE_ESTIMATE_TIMEOUT_MS)),
    }
    debugPerpsCommit(`${context}:fee-estimate:gas-price:success`, legacyFees)
    return legacyFees
  } catch (error) {
    debugPerpsCommit(`${context}:fee-estimate:none`, {
      reason: error instanceof Error ? error.message : String(error),
    })
    // Fee estimation is a convenience, not a precondition for opening the wallet.
    // Let the connected wallet/provider estimate fees instead of blocking the prompt.
    return {}
  }
}

async function describeCommitFailure({
  client,
  address,
  hash,
  args,
  isClose,
  side,
  sizeDelta,
  marginDelta,
  oraclePrice,
}: {
  client: PerpsPublicClient
  address: Address
  hash: Hex
  args: CommitOrderArgs
  isClose: boolean
  side: number
  sizeDelta: bigint
  marginDelta: bigint
  oraclePrice: bigint
}): Promise<string> {
  const context: string[] = [`Failed tx: ${hash}.`]

  try {
    const [pendingOrders, maxPendingOrders, accountView] = await Promise.all([
      client.readContract({
        address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
        abi: PERPS_PUBLIC_LENS_ABI,
        functionName: 'getPendingOrders',
        args: [address],
      }),
      client.readContract({
        address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
        abi: PERPS_ORDER_ROUTER_ABI,
        functionName: 'maxPendingOrders',
      }),
      client.readContract({
        address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
        abi: PERPS_PUBLIC_LENS_ABI,
        functionName: 'getTraderAccount',
        args: [address],
      }),
    ])
    const equityUsdc = readBigInt(accountView, 'equityUsdc', 0)
    const withdrawableUsdc = readBigInt(accountView, 'withdrawableUsdc', 1)
    const pendingMarginUsdc = readBigInt(accountView, 'pendingOrderMarginUsdc', 2)
    const pendingBountyUsdc = readBigInt(accountView, 'pendingExecutionBountyUsdc', 3)
    context.push(
      `Current account state: ${readArrayLength(pendingOrders)}/${maxPendingOrders.toString()} pending orders, equity ${formatPerpsUsdc(equityUsdc)} USDC, free/withdrawable ${formatPerpsUsdc(withdrawableUsdc)} USDC, pending margin ${formatPerpsUsdc(pendingMarginUsdc)} USDC, pending bounty ${formatPerpsUsdc(pendingBountyUsdc)} USDC.`
    )
  } catch {
    context.push('Could not refresh account diagnostics after the failed commit.')
  }

  try {
    if (!isClose) {
      const latestBlock = await client.getBlock({ blockTag: 'latest' })
      const openRevertCode = await client.readContract({
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngineLens,
        abi: PERPS_CFD_ENGINE_LENS_ABI,
        functionName: 'previewOpenRevertCode',
        args: [address, side, sizeDelta, marginDelta, oraclePrice, latestBlock.timestamp],
      })
      if (openRevertCode !== 0) {
        context.push(`Latest open preview now fails: ${getPerpsOpenRevertMessage(Number(openRevertCode))}`)
      } else {
        context.push('Latest open preview still passes.')
      }
    } else {
      const closePreview = await client.readContract({
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngineLens,
        abi: PERPS_CFD_ENGINE_LENS_ABI,
        functionName: 'previewClose',
        args: [address, sizeDelta, oraclePrice],
      })
      const isValidClose = readBoolean(closePreview, 'valid', 0)
      if (isValidClose === false) {
        context.push(`Latest close preview now fails: ${getPerpsCloseInvalidReasonMessage(readNumber(closePreview, 'invalidReason', 1))}`)
      } else {
        context.push('Latest close preview still passes.')
      }
    }
  } catch {
    context.push('Could not rerun the order preview after the failed commit.')
  }

  try {
    await client.simulateContract({
      account: address,
      address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
      abi: PERPS_ORDER_ROUTER_ABI,
      functionName: 'commitOrder',
      args,
    })
    context.push('A fresh commit simulation still passes, so the mined revert likely came from state changing between simulation and confirmation or from RPC-hidden revert data.')
  } catch (simulationError) {
    context.push(`A fresh commit simulation now fails: ${getPerpsErrorMessage(simulationError, 'commit')}`)
  }

  return `Commit reverted after wallet confirmation, but the receipt did not include decodable revert data. ${context.join(' ')}`
}

export function usePerpsTrading() {
  const { address } = useAccount()
  const publicClient = usePublicClient({ chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID })
  const { writeContractAsync } = useWriteContract()
  const queryClient = useQueryClient()

  const invalidatePerpsReads = useCallback(() => {
    void queryClient.invalidateQueries()
  }, [queryClient])

  const approveUsdcForMargin = useCallback(async (amount: bigint) => {
    try {
      const client = requireClient(publicClient)
      const fees = await getBufferedFeeParams(client)
      const hash = await writeContractAsync({
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.usdc,
        abi: ERC20_ABI,
        functionName: 'approve',
        args: [PERPS_ARBITRUM_SEPOLIA.marginClearinghouse, amount],
        ...fees,
      })

      assertSuccessfulReceipt(
        await client.waitForTransactionReceipt({ hash }),
        'USDC approval transaction reverted'
      )
      invalidatePerpsReads()
      return hash
    } catch (error) {
      throw new Error(getPerpsErrorMessage(error, 'approve'))
    }
  }, [invalidatePerpsReads, publicClient, writeContractAsync])

  const depositMargin = useCallback(async (amount: bigint, allowance?: bigint) => {
    try {
      if (!address) {
        throw new Error('Connect wallet before depositing margin')
      }
      if (amount <= 0n) {
        throw new Error('Deposit amount must be greater than zero')
      }

      if ((allowance ?? 0n) < amount) {
        await approveUsdcForMargin(amount)
      }

      const client = requireClient(publicClient)
      await client.simulateContract({
        account: address,
        address: PERPS_ARBITRUM_SEPOLIA.marginClearinghouse,
        abi: PERPS_MARGIN_CLEARINGHOUSE_ABI,
        functionName: 'depositMargin',
        args: [amount],
      })
      const fees = await getBufferedFeeParams(client)
      const hash = await writeContractAsync({
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.marginClearinghouse,
        abi: PERPS_MARGIN_CLEARINGHOUSE_ABI,
        functionName: 'depositMargin',
        args: [amount],
        ...fees,
      })

      assertSuccessfulReceipt(
        await client.waitForTransactionReceipt({ hash }),
        'Deposit margin transaction reverted'
      )
      invalidatePerpsReads()
      return hash
    } catch (error) {
      throw new Error(getPerpsErrorMessage(error, 'deposit'))
    }
  }, [address, approveUsdcForMargin, invalidatePerpsReads, publicClient, writeContractAsync])

  const withdrawMargin = useCallback(async (amount: bigint) => {
    try {
      if (!address) {
        throw new Error('Connect wallet before withdrawing margin')
      }
      if (amount <= 0n) {
        throw new Error('Withdraw amount must be greater than zero')
      }

      const client = requireClient(publicClient)
      await client.simulateContract({
        account: address,
        address: PERPS_ARBITRUM_SEPOLIA.marginClearinghouse,
        abi: PERPS_MARGIN_CLEARINGHOUSE_ABI,
        functionName: 'withdrawMargin',
        args: [amount],
      })
      const fees = await getBufferedFeeParams(client)
      const hash = await writeContractAsync({
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.marginClearinghouse,
        abi: PERPS_MARGIN_CLEARINGHOUSE_ABI,
        functionName: 'withdrawMargin',
        args: [amount],
        ...fees,
      })

      assertSuccessfulReceipt(
        await client.waitForTransactionReceipt({ hash }),
        'Withdraw margin transaction reverted'
      )
      invalidatePerpsReads()
      return hash
    } catch (error) {
      throw new Error(getPerpsErrorMessage(error, 'withdraw'))
    }
  }, [address, invalidatePerpsReads, publicClient, writeContractAsync])

  const addPositionMargin = useCallback(async (amount: bigint) => {
    try {
      if (!address) {
        throw new Error('Connect wallet before adding position margin')
      }
      if (amount <= 0n) {
        throw new Error('Position margin amount must be greater than zero')
      }

      const client = requireClient(publicClient)
      await client.simulateContract({
        account: address,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
        abi: PERPS_CFD_ENGINE_ABI,
        functionName: 'addMargin',
        args: [address, amount],
      })
      const fees = await getBufferedFeeParams(client)
      const hash = await writeContractAsync({
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
        abi: PERPS_CFD_ENGINE_ABI,
        functionName: 'addMargin',
        args: [address, amount],
        ...fees,
      })

      assertSuccessfulReceipt(
        await client.waitForTransactionReceipt({ hash }),
        'Add position margin transaction reverted'
      )
      invalidatePerpsReads()
      return hash
    } catch (error) {
      throw new Error(getPerpsErrorMessage(error, 'addPositionMargin'))
    }
  }, [address, invalidatePerpsReads, publicClient, writeContractAsync])

  const commitOrder = useCallback(async ({
    direction,
    notionalUsdc,
    sizeDelta: sizeDeltaOverride,
    marginUsdc,
    oraclePrice,
    slippagePercent,
    isClose,
    onWalletRequestStart,
  }: CommitOrderInput): Promise<CommitOrderResult> => {
    try {
      debugPerpsCommit('start', {
        address,
        direction,
        notionalUsdc,
        sizeDeltaOverride,
        marginUsdc,
        oraclePrice,
        slippagePercent,
        isClose,
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
      })
      if (!address) {
        throw new Error('Connect wallet before committing an order')
      }
      if (notionalUsdc <= 0n) {
        throw new Error('Order size must be greater than zero')
      }
      if (oraclePrice <= 0n) {
        throw new Error('plDXY Perp price is not available')
      }

      const side = directionToPerpsSide(direction)
      const sizeDelta = sizeDeltaOverride ?? notionalUsdcToSizeDelta(notionalUsdc, oraclePrice)
      if (sizeDelta <= 0n) {
        throw new Error('Order size is too small')
      }
      const marginDelta = isClose ? 0n : marginUsdc
      const targetPrice = getPerpsTargetPrice({
        direction,
        isClose,
        oraclePrice,
        slippagePercent,
      })
      const args = [side, sizeDelta, marginDelta, targetPrice, isClose] as const
      debugPerpsCommit('args-ready', {
        side,
        sizeDelta,
        marginDelta,
        targetPrice,
        isClose,
      })
      const client = requireClient(publicClient)
      debugPerpsCommit('client-ready')
      const fees = await getBufferedFeeParams(client, 'commit')
      await probeInjectedWalletProvider()
      debugPerpsCommit('wallet-request:start', {
        orderRouter: PERPS_ARBITRUM_SEPOLIA.orderRouter,
        args,
        fees,
      })
      onWalletRequestStart?.()
      const hash = await writeContractAsync({
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
        abi: PERPS_ORDER_ROUTER_ABI,
        functionName: 'commitOrder',
        args,
        ...fees,
      })
      debugPerpsCommit('wallet-request:accepted', { hash })
      debugPerpsCommit('receipt-wait:start', { hash })
      const receipt = await client.waitForTransactionReceipt({ hash })
      debugPerpsCommit('receipt-wait:done', {
        hash,
        status: receipt.status,
      })
      if (receipt.status !== 'success') {
        throw new Error(await describeCommitFailure({
          client,
          address,
          hash,
          args,
          isClose,
          side,
          sizeDelta,
          marginDelta,
          oraclePrice,
        }))
      }
      const [committed] = parseEventLogs({
        abi: PERPS_ORDER_ROUTER_ABI,
        eventName: 'OrderCommitted',
        logs: receipt.logs,
      })
      if (committed?.args.orderId === undefined) {
        throw new Error('Commit transaction succeeded, but no OrderCommitted event was found in the receipt. Refresh account state before retrying.')
      }
      debugPerpsCommit('order-committed', {
        hash,
        orderId: committed.args.orderId,
      })

      invalidatePerpsReads()
      return {
        hash,
        orderId: committed.args.orderId,
      }
    } catch (error) {
      debugPerpsCommit('failed', {
        message: error instanceof Error ? error.message : String(error),
      })
      throw new Error(getPerpsErrorMessage(error, 'commit'))
    }
  }, [address, invalidatePerpsReads, publicClient, writeContractAsync])

  const executeOrder = useCallback(async (orderId: bigint): Promise<ExecuteOrderResult> => {
    let pythPayload: PerpsPythUpdatePayload | undefined
    let executeStage = 'preparing self-execute'
    try {
      if (!address) {
        throw new Error('Connect wallet before self-executing an order')
      }
      if (orderId <= 0n) {
        throw new Error('Missing order ID')
      }

      const client = requireClient(publicClient)
      executeStage = 'reading order state from Arbitrum Sepolia'
      const [pendingOrderView, settlementWindow, maxOrderAge, latestBlock] = await Promise.all([
        client.readContract({
          address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
          abi: PERPS_ORDER_ROUTER_ABI,
          functionName: 'getPendingOrderView',
          args: [orderId],
        }),
        client.readContract({
          address: PERPS_ARBITRUM_SEPOLIA.pletherOracle,
          abi: PERPS_PLETHER_ORACLE_ABI,
          functionName: 'orderSettlementWindow',
        }),
        client.readContract({
          address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
          abi: PERPS_ORDER_ROUTER_ABI,
          functionName: 'maxOrderAge',
        }),
        client.getBlock({ blockTag: 'latest' }),
      ])
      const commitTime = readPendingOrderCommitTime(pendingOrderView)
      if (commitTime === undefined) {
        throw new Error('Could not read order commit time. Refresh and retry self-execute.')
      }
      const minPublishTime = commitTime + 1n
      const maxPublishTime = commitTime + settlementWindow
      const chainNow = latestBlock.timestamp
      const expiryTime = commitTime + maxOrderAge
      if (chainNow < minPublishTime) {
        throw new Error(
          `Order reveal is not ready yet. Commit time: ${describeTime(commitTime)}; earliest execution tick: ${describeTime(minPublishTime)}; chain time: ${describeTime(chainNow)}.`
        )
      }
      if (maxOrderAge > 0n && chainNow > expiryTime) {
        throw new Error(
          `Order expired before self-execute. Commit time: ${describeTime(commitTime)}; expiry: ${describeTime(expiryTime)}; chain time: ${describeTime(chainNow)}; max age: ${maxOrderAge.toString()}s. Commit a new order and execute it before expiry.`
        )
      }
      const minPublishTimeNumber = Number(minPublishTime)
      const maxPublishTimeNumber = Number(maxPublishTime)

      const prepareExecution = async (payload: PerpsPythUpdatePayload) => {
        pythPayload = payload
        if (!payload.publishTimes.length) {
          throw new Error('Hermes returned Pyth update data without parsed publish times, so the app could not verify the order settlement window.')
        }
        const returnedMinPublishTime = BigInt(Math.min(...payload.publishTimes))
        const returnedMaxPublishTime = BigInt(Math.max(...payload.publishTimes))
        if (returnedMinPublishTime < minPublishTime || returnedMaxPublishTime > maxPublishTime) {
          throw new Error(
            `Historical Pyth update was outside the order settlement window. Commit time: ${describeTime(commitTime)}; valid publish window: ${describeTime(minPublishTime)} to ${describeTime(maxPublishTime)}; Hermes returned: ${describeTime(returnedMinPublishTime)} to ${describeTime(returnedMaxPublishTime)}. Retry with a new order.`
          )
        }

        const pythUpdateData = payload.updateData
        executeStage = 'calculating Pyth update fee'
        const updateFee = await client.readContract({
          address: PERPS_ARBITRUM_SEPOLIA.pletherOracle,
          abi: PERPS_PLETHER_ORACLE_ABI,
          functionName: 'getUpdateFee',
          args: [pythUpdateData],
        })
        const args = [orderId, pythUpdateData] as const
        executeStage = 'estimating self-execute transaction gas'
        const [fees, estimatedGas] = await Promise.all([
          getBufferedFeeParams(client),
          client.estimateContractGas({
            account: address,
            address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
            abi: PERPS_ORDER_ROUTER_ABI,
            functionName: 'executeOrder',
            args,
            value: updateFee,
          }),
        ])

        return { args, updateFee, fees, estimatedGas }
      }

      executeStage = 'fetching cached reveal payload from the backend'
      let preparedExecution
      try {
        preparedExecution = await prepareExecution(
          await fetchPerpsRevealPayload(orderId, minPublishTimeNumber, maxPublishTimeNumber)
        )
      } catch (error) {
        const message = getPerpsErrorMessage(error, 'execute')
        if (!shouldFallbackToHistoricalPythPayload(message)) {
          throw error
        }

        executeStage = 'fetching exact historical reveal payload from the backend'
        preparedExecution = await prepareExecution(
          await fetchPerpsPythUpdatePayloadForWindow(minPublishTimeNumber, maxPublishTimeNumber)
        )
      }

      executeStage = 'submitting self-execute transaction to the wallet'
      const hash = await writeContractAsync({
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
        abi: PERPS_ORDER_ROUTER_ABI,
        functionName: 'executeOrder',
        args: preparedExecution.args,
        value: preparedExecution.updateFee,
        gas: bumpGas(preparedExecution.estimatedGas),
        ...preparedExecution.fees,
      })
      executeStage = 'waiting for self-execute transaction confirmation'
      const receipt = assertSuccessfulReceipt(
        await client.waitForTransactionReceipt({ hash }),
        'Self-execute transaction reverted before settling the order'
      )
      const [executed] = parseEventLogs({
        abi: PERPS_ORDER_ROUTER_ABI,
        eventName: 'OrderExecuted',
        logs: receipt.logs,
      }).filter((event) => isOrderEventFor(event.args.orderId, orderId))
      const [failed] = parseEventLogs({
        abi: PERPS_ORDER_ROUTER_ABI,
        eventName: 'OrderFailed',
        logs: receipt.logs,
      }).filter((event) => isOrderEventFor(event.args.orderId, orderId))

      if (executed === undefined && failed === undefined) {
        throw new Error(
          `Self-execute transaction confirmed, but order ${orderId.toString()} did not emit OrderExecuted or OrderFailed. The transaction may have only cleaned earlier queue orders. Refresh account state before retrying.`
        )
      }

      invalidatePerpsReads()
      return {
        hash,
        executionPrice: executed?.args.executionPrice,
        failedReason: failed?.args.reason === undefined ? undefined : Number(failed.args.reason),
      }
    } catch (error) {
      const message = withPythFetchTiming(getPerpsErrorMessage(error, 'execute'), pythPayload)
      const lowerMessage = message.toLowerCase()
      const shouldAddStage = lowerMessage.includes('network request failed') ||
        lowerMessage.includes('could not fetch pyth update data') ||
        lowerMessage.includes('could not fetch cached reveal payload') ||
        lowerMessage.includes('reveal payload unavailable') ||
        lowerMessage.includes('pyth update request failed') ||
        lowerMessage.includes('hermes rate limit reached')
      throw new Error(shouldAddStage ? `${message} Failed while ${executeStage}.` : message)
    }
  }, [address, invalidatePerpsReads, publicClient, writeContractAsync])

  const cleanupExpiredOrder = useCallback(async (orderId: bigint): Promise<CleanupExpiredOrderResult> => {
    try {
      if (!address) {
        throw new Error('Connect wallet before cleaning up an expired order')
      }
      if (orderId <= 0n) {
        throw new Error('Missing order ID')
      }

      const client = requireClient(publicClient)
      const [pendingOrderView, maxOrderAge, latestBlock] = await Promise.all([
        client.readContract({
          address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
          abi: PERPS_ORDER_ROUTER_ABI,
          functionName: 'getPendingOrderView',
          args: [orderId],
        }),
        client.readContract({
          address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
          abi: PERPS_ORDER_ROUTER_ABI,
          functionName: 'maxOrderAge',
        }),
        client.getBlock({ blockTag: 'latest' }),
      ])
      const commitTime = readPendingOrderCommitTime(pendingOrderView)
      if (commitTime === undefined) {
        throw new Error('Could not read order commit time. Refresh and retry cleanup.')
      }
      const expiryTime = commitTime + maxOrderAge
      if (maxOrderAge > 0n && latestBlock.timestamp <= expiryTime) {
        throw new Error(
          `Order has not expired yet. Commit time: ${describeTime(commitTime)}; expiry: ${describeTime(expiryTime)}; chain time: ${describeTime(latestBlock.timestamp)}.`
        )
      }

      const args = [orderId, []] as const
      const [fees, estimatedGas] = await Promise.all([
        getBufferedFeeParams(client),
        client.estimateContractGas({
          account: address,
          address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
          abi: PERPS_ORDER_ROUTER_ABI,
          functionName: 'executeOrder',
          args,
          value: 0n,
        }),
      ])

      const hash = await writeContractAsync({
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
        abi: PERPS_ORDER_ROUTER_ABI,
        functionName: 'executeOrder',
        args,
        value: 0n,
        gas: bumpGas(estimatedGas),
        ...fees,
      })
      assertSuccessfulReceipt(
        await client.waitForTransactionReceipt({ hash }),
        'Expired-order cleanup transaction reverted'
      )

      invalidatePerpsReads()
      return { hash }
    } catch (error) {
      throw new Error(getPerpsErrorMessage(error, 'execute'))
    }
  }, [address, invalidatePerpsReads, publicClient, writeContractAsync])

  return {
    approveUsdcForMargin,
    depositMargin,
    withdrawMargin,
    addPositionMargin,
    commitOrder,
    executeOrder,
    cleanupExpiredOrder,
  }
}
