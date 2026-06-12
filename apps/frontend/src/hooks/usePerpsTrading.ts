import { useCallback } from 'react'
import { useQueryClient } from '@tanstack/react-query'
import { parseEventLogs, type Address, type Hex } from 'viem'
import { useAccount, usePublicClient, useWriteContract } from 'wagmi'
import { ERC20_ABI, PERPS_CFD_ENGINE_LENS_ABI, PERPS_MARGIN_CLEARINGHOUSE_ABI, PERPS_ORDER_ROUTER_ABI, PERPS_PLETHER_ORACLE_ABI, PERPS_PUBLIC_LENS_ABI } from '../contracts/abis'
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
  | { maxFeePerGas: bigint; maxPriorityFeePerGas?: bigint }
  | { gasPrice: bigint }

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

async function getBufferedFeeParams(client: PerpsPublicClient): Promise<BufferedFeeParams> {
  try {
    const fees = await client.estimateFeesPerGas()
    if ('maxFeePerGas' in fees && fees.maxFeePerGas !== undefined) {
      return {
        maxFeePerGas: bumpFee(fees.maxFeePerGas),
        maxPriorityFeePerGas: fees.maxPriorityFeePerGas === undefined
          ? undefined
          : bumpFee(fees.maxPriorityFeePerGas),
      }
    }
  } catch {
    // Fall back to legacy gas price below if the RPC does not expose EIP-1559 fee estimates.
  }

  return {
    gasPrice: bumpFee(await client.getGasPrice()),
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

  const commitOrder = useCallback(async ({
    direction,
    notionalUsdc,
    sizeDelta: sizeDeltaOverride,
    marginUsdc,
    oraclePrice,
    slippagePercent,
    isClose,
  }: CommitOrderInput): Promise<CommitOrderResult> => {
    try {
      if (!address) {
        throw new Error('Connect wallet before committing an order')
      }
      if (notionalUsdc <= 0n) {
        throw new Error('Order size must be greater than zero')
      }
      if (oraclePrice <= 0n) {
        throw new Error('DXY price is not available')
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
      const client = requireClient(publicClient)
      const [pendingOrders, maxPendingOrders] = await Promise.all([
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
      ])
      const pendingOrderCount = readArrayLength(pendingOrders)
      if (BigInt(pendingOrderCount) >= maxPendingOrders) {
        throw new Error(
          `You already have ${pendingOrderCount.toString()} pending orders, which is the current account limit. Execute or let existing orders expire/clean up before committing a new order.`
        )
      }
      if (!isClose) {
        const latestBlock = await client.getBlock({ blockTag: 'latest' })
        const openRevertCode = await client.readContract({
          address: PERPS_ARBITRUM_SEPOLIA.cfdEngineLens,
          abi: PERPS_CFD_ENGINE_LENS_ABI,
          functionName: 'previewOpenRevertCode',
          args: [address, side, sizeDelta, marginDelta, oraclePrice, latestBlock.timestamp],
        })
        if (openRevertCode !== 0) {
          throw new Error(getPerpsOpenRevertMessage(Number(openRevertCode)))
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
          throw new Error(getPerpsCloseInvalidReasonMessage(readNumber(closePreview, 'invalidReason', 1)))
        }
      }
      await client.simulateContract({
        account: address,
        address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
        abi: PERPS_ORDER_ROUTER_ABI,
        functionName: 'commitOrder',
        args,
      })
      const fees = await getBufferedFeeParams(client)
      const hash = await writeContractAsync({
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.orderRouter,
        abi: PERPS_ORDER_ROUTER_ABI,
        functionName: 'commitOrder',
        args,
        ...fees,
      })
      const receipt = await client.waitForTransactionReceipt({ hash })
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

      invalidatePerpsReads()
      return {
        hash,
        orderId: committed.args.orderId,
      }
    } catch (error) {
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
    commitOrder,
    executeOrder,
    cleanupExpiredOrder,
  }
}
