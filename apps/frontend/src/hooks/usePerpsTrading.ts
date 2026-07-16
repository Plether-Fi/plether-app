import { useCallback } from 'react'
import { useQueryClient } from '@tanstack/react-query'
import { isAddressEqual, parseEventLogs, type Address, type Hex } from 'viem'
import { usePublicClient, useSignTypedData } from 'wagmi'
import {
  buildAddMarginAction,
  buildAuthorizedDepositAction,
  buildPlaceOrderAction,
  buildReceiveWithAuthorizationTypedData,
  buildSettleTraderClaimAction,
  buildSmartAccountBalanceDepositAction,
  buildWithdrawAction,
  buildWithdrawToOwnerAction,
  type PerpsActionPlan,
  type SponsoredExecutionStatus,
} from '@plether/perps-aa-client'
import { PERPS_CFD_ENGINE_LENS_ABI, PERPS_ORDER_ROUTER_ABI, PERPS_PUBLIC_LENS_ABI } from '../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import {
  executeSponsoredPerpsAction,
  clearDepositAuthorization,
  getOrCreateDepositAuthorization,
  sponsorReasonMessage,
  findSponsorRequestError,
  usePerpsAaRuntime,
  usePerpsIdentity,
} from '../perps-aa'
import {
  directionToPerpsSide,
  formatPerpsUsdc,
  getPerpsTargetPrice,
  notionalUsdcToSizeDelta,
  type PerpsDirection,
} from '../utils/perps'
import { COMMIT_UNDECODED_FALLBACK_MESSAGE, getPerpsCloseInvalidReasonMessage, getPerpsErrorMessage, getPerpsOpenRevertMessage } from '../utils/perpsErrors'

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
  userOperationHash?: Hex
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
type CommitOrderArgs = readonly [number, bigint, bigint, bigint, boolean]

const TX_HASH_PATTERN = /0x[a-fA-F0-9]{64}/

function isPerpsCommitDebugEnabled(): boolean {
  if (import.meta.env.MODE === 'test') return false
  if (import.meta.env.DEV) return true

  try {
    return globalThis.localStorage.getItem('PLETHER_PERPS_DEBUG') === '1'
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

function requireIncludedTransactionHash(input: {
  transactionHash?: Hex
  userOperationHash: Hex
}): Hex {
  if (!input.transactionHash) {
    throw new Error(
      `Sponsored operation ${input.userOperationHash} was included without a transaction hash. Refresh activity before retrying.`
    )
  }
  return input.transactionHash
}

function readRecordValue(value: unknown, key: string, index: number): unknown {
  if (value && typeof value === 'object' && key in value) {
    return (value as Record<string, unknown>)[key]
  }
  if (Array.isArray(value)) return value[index]
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

function findTransactionHash(value: unknown, depth = 0): Hex | undefined {
  if (depth > 6 || value === undefined || value === null) return undefined
  if (typeof value === 'string') {
    return TX_HASH_PATTERN.exec(value)?.[0] as Hex | undefined
  }
  if (typeof value !== 'object') return undefined

  if (value instanceof Error) {
    return findTransactionHash(value.message, depth + 1) ??
      findTransactionHash((value as Error & { cause?: unknown }).cause, depth + 1)
  }

  for (const nestedValue of Object.values(value as Record<string, unknown>)) {
    const hash = findTransactionHash(nestedValue, depth + 1)
    if (hash) return hash
  }
  return undefined
}

function shouldEnrichCommitFailure(message: string): boolean {
  if (message === COMMIT_UNDECODED_FALLBACK_MESSAGE) return true

  const lowerMessage = message.toLowerCase()
  return lowerMessage === 'transaction failed' ||
    lowerMessage === 'execution reverted' ||
    lowerMessage.includes('missing revert data') ||
    lowerMessage.includes('could not decode') ||
    lowerMessage.includes('did not return a contract error')
}

async function describeCommitFailure({
  client,
  address,
  hash,
  intro,
  args,
  isClose,
  side,
  sizeDelta,
  marginDelta,
  oraclePrice,
}: {
  client: PerpsPublicClient
  address: Address
  hash?: Hex
  intro: string
  args: CommitOrderArgs
  isClose: boolean
  side: number
  sizeDelta: bigint
  marginDelta: bigint
  oraclePrice: bigint
}): Promise<string> {
  const context: string[] = [
    intro,
    hash === undefined
      ? 'No transaction hash was returned by the wallet/RPC, so no mined transaction could be checked.'
      : `Failed tx: ${hash}`,
  ]
  const hasTransactionHash = hash !== undefined

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
      `Current account state: ${readArrayLength(pendingOrders).toString()}/${maxPendingOrders.toString()} pending orders, equity ${formatPerpsUsdc(equityUsdc)} USDC, free/withdrawable ${formatPerpsUsdc(withdrawableUsdc)} USDC, pending margin ${formatPerpsUsdc(pendingMarginUsdc)} USDC, pending bounty ${formatPerpsUsdc(pendingBountyUsdc)} USDC.`
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
        context.push(`Latest open preview now fails: ${getPerpsOpenRevertMessage(openRevertCode)}`)
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
    if (hasTransactionHash) {
      context.push('A fresh commit simulation still passes, so the mined revert likely came from state changing between simulation and confirmation or from RPC-hidden revert data.')
    } else {
      context.push('A fresh commit simulation still passes, so this looks like a wallet/RPC submission failure rather than a contract rejection. Retry the commit; if your wallet still shows a pending request, reject it first or reconnect the wallet.')
    }
  } catch (simulationError) {
    context.push(`A fresh commit simulation now fails: ${getPerpsErrorMessage(simulationError, 'commit')}`)
  }

  return context.join('\n')
}

export function usePerpsTrading() {
  const identity = usePerpsIdentity()
  const address = identity.accountAddress
  const aaRuntime = usePerpsAaRuntime()
  const publicClient = usePublicClient({ chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID })
  const { signTypedDataAsync } = useSignTypedData()
  const queryClient = useQueryClient()

  const invalidatePerpsReads = useCallback(() => {
    void queryClient.invalidateQueries()
  }, [queryClient])

  const requireSponsoredExecution = useCallback(() => {
    if (!identity.isAaManifestConfigured) {
      throw new Error(
        'Perps is sponsorship-only on testnet. Direct owner-wallet transactions are disabled.'
      )
    }
    if (identity.status !== 'ready' || !identity.accountAddress || !identity.ownerAddress) {
      throw new Error(
        identity.error?.message ??
        'Confirm the Plether Trading Account before submitting this action.'
      )
    }
    if (!identity.manifest) {
      throw new Error('The reviewed gas-sponsorship manifest is unavailable.')
    }
    if (
      identity.manifest.chainId !== PERPS_ARBITRUM_SEPOLIA_CHAIN_ID ||
      !isAddressEqual(identity.manifest.usdc, PERPS_ARBITRUM_SEPOLIA.usdc) ||
      !isAddressEqual(
        identity.manifest.marginClearinghouse,
        PERPS_ARBITRUM_SEPOLIA.marginClearinghouse
      ) ||
      !isAddressEqual(identity.manifest.cfdEngine, PERPS_ARBITRUM_SEPOLIA.cfdEngine) ||
      !isAddressEqual(identity.manifest.orderRouter, PERPS_ARBITRUM_SEPOLIA.orderRouter)
    ) {
      throw new Error(
        'The gas-sponsorship manifest does not match this frontend deployment. Your action was not sent.'
      )
    }
    if (!identity.sponsorshipEnabled) {
      throw new Error(
        'Plether gas sponsorship is temporarily unavailable. Your action was kept under the Trading Account and was not sent.'
      )
    }
    if (!aaRuntime) {
      throw new Error(
        'The reviewed smart-account wallet adapter is unavailable. Your action was not sent.'
      )
    }

    return {
      manifest: identity.manifest,
      ownerAddress: identity.ownerAddress,
      accountAddress: identity.accountAddress,
      runtime: aaRuntime,
    }
  }, [aaRuntime, identity])

  const approveUsdcForMargin = useCallback(async (amount: bigint) => {
    void amount
    throw new Error(
      'Direct USDC approvals are disabled. Sponsored deposits execute atomically through the Trading Account.'
    )
  }, [])

  const depositMargin = useCallback(async (
    amount: bigint,
    _allowance?: bigint,
    source?: 'owner' | 'account'
  ) => {
    try {
      if (!address) {
        throw new Error('Confirm the Plether Trading Account before depositing margin')
      }
      if (amount <= 0n) {
        throw new Error('Deposit amount must be greater than zero')
      }

      const sponsored = requireSponsoredExecution()
      const useOwnerAuthorization = source === 'owner' || (
        source === undefined &&
        sponsored.manifest.smartAccountMode === 'separate-immutable' &&
        sponsored.manifest.usdcSupportsEip3009
      )
      let action: PerpsActionPlan
      if (useOwnerAuthorization) {
        if (
          !sponsored.manifest.usdcSupportsEip3009 ||
          !sponsored.manifest.usdcEip712Name ||
          !sponsored.manifest.usdcEip712Version
        ) {
          throw new Error(
            'Owner-funded onboarding is disabled until this USDC deployment and its exact EIP-712 domain are verified.'
          )
        }
        const authorization = getOrCreateDepositAuthorization({
          chainId: sponsored.manifest.chainId,
          ownerAddress: sponsored.ownerAddress,
          accountAddress: sponsored.accountAddress,
          token: sponsored.manifest.usdc,
          amount,
        })
        const typedData = buildReceiveWithAuthorizationTypedData({
          name: sponsored.manifest.usdcEip712Name,
          version: sponsored.manifest.usdcEip712Version,
          chainId: sponsored.manifest.chainId,
          verifyingContract: sponsored.manifest.usdc,
        }, authorization)
        const authorizationSignature = await signTypedDataAsync({
          ...typedData,
          account: sponsored.ownerAddress,
        })
        action = buildAuthorizedDepositAction({
          account: sponsored.accountAddress,
          usdc: sponsored.manifest.usdc,
          clearinghouse: sponsored.manifest.marginClearinghouse,
          authorization,
          authorizationSignature,
        })
      } else {
        action = buildSmartAccountBalanceDepositAction({
          account: sponsored.accountAddress,
          usdc: sponsored.manifest.usdc,
          clearinghouse: sponsored.manifest.marginClearinghouse,
          amount,
        })
      }
      const result = await executeSponsoredPerpsAction({
        manifest: sponsored.manifest,
        ownerAddress: sponsored.ownerAddress,
        action,
        runtime: sponsored.runtime,
        authorizationTokenToClearOnConfirmation: useOwnerAuthorization
          ? sponsored.manifest.usdc
          : undefined,
      })
      const hash = requireIncludedTransactionHash(result)
      invalidatePerpsReads()
      return hash
    } catch (error) {
      const sponsorError = findSponsorRequestError(error)
      if (sponsorError) throw new Error(sponsorReasonMessage(sponsorError))
      throw new Error(getPerpsErrorMessage(error, 'deposit'))
    }
  }, [address, invalidatePerpsReads, requireSponsoredExecution, signTypedDataAsync])

  const withdrawMargin = useCallback(async (amount: bigint) => {
    try {
      if (!address) {
        throw new Error('Confirm the Plether Trading Account before withdrawing margin')
      }
      if (amount <= 0n) {
        throw new Error('Withdraw amount must be greater than zero')
      }

      const sponsored = requireSponsoredExecution()
      const action = sponsored.manifest.smartAccountMode === 'eip-7702'
        ? buildWithdrawAction({
            account: sponsored.accountAddress,
            clearinghouse: sponsored.manifest.marginClearinghouse,
            amount,
          })
        : buildWithdrawToOwnerAction({
            account: sponsored.accountAddress,
            owner: sponsored.ownerAddress,
            usdc: sponsored.manifest.usdc,
            clearinghouse: sponsored.manifest.marginClearinghouse,
            amount,
        })
      const result = await executeSponsoredPerpsAction({
        manifest: sponsored.manifest,
        ownerAddress: sponsored.ownerAddress,
        action,
        runtime: sponsored.runtime,
      })
      const hash = requireIncludedTransactionHash(result)
      invalidatePerpsReads()
      return hash
    } catch (error) {
      const sponsorError = findSponsorRequestError(error)
      if (sponsorError) throw new Error(sponsorReasonMessage(sponsorError))
      throw new Error(getPerpsErrorMessage(error, 'withdraw'))
    }
  }, [address, invalidatePerpsReads, requireSponsoredExecution])

  const addPositionMargin = useCallback(async (amount: bigint) => {
    try {
      if (!address) {
        throw new Error('Confirm the Plether Trading Account before adding position margin')
      }
      if (amount <= 0n) {
        throw new Error('Position margin amount must be greater than zero')
      }

      const sponsored = requireSponsoredExecution()
      const action = buildAddMarginAction({
        account: sponsored.accountAddress,
        cfdEngine: sponsored.manifest.cfdEngine,
        amount,
      })
      const result = await executeSponsoredPerpsAction({
        manifest: sponsored.manifest,
        ownerAddress: sponsored.ownerAddress,
        action,
        runtime: sponsored.runtime,
      })
      const hash = requireIncludedTransactionHash(result)
      invalidatePerpsReads()
      return hash
    } catch (error) {
      const sponsorError = findSponsorRequestError(error)
      if (sponsorError) throw new Error(sponsorReasonMessage(sponsorError))
      throw new Error(getPerpsErrorMessage(error, 'addPositionMargin'))
    }
  }, [address, invalidatePerpsReads, requireSponsoredExecution])

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
    let diagnosticClient: PerpsPublicClient | undefined
    let diagnosticArgs: CommitOrderArgs | undefined
    let diagnosticSide: number | undefined
    let diagnosticSizeDelta: bigint | undefined
    let diagnosticMarginDelta: bigint | undefined
    let diagnosticHash: Hex | undefined

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
        throw new Error('Confirm the Plether Trading Account before committing an order')
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
      diagnosticArgs = args
      diagnosticSide = side
      diagnosticSizeDelta = sizeDelta
      diagnosticMarginDelta = marginDelta
      debugPerpsCommit('args-ready', {
        side,
        sizeDelta,
        marginDelta,
        targetPrice,
        isClose,
      })
      const client = requireClient(publicClient)
      diagnosticClient = client
      debugPerpsCommit('client-ready')
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
          `You already have ${pendingOrderCount.toString()} pending orders, which is the current account limit. Wait for an existing order to be finalized before committing a new order.`
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
          throw new Error(getPerpsOpenRevertMessage(openRevertCode))
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

      const sponsored = requireSponsoredExecution()
      const action = buildPlaceOrderAction({
        account: sponsored.accountAddress,
        orderRouter: sponsored.manifest.orderRouter,
        side: direction === 'long' ? 'BULL' : 'BEAR',
        sizeDelta,
        marginDelta,
        targetPrice,
        isClose,
      })
      const result = await executeSponsoredPerpsAction({
        manifest: sponsored.manifest,
        ownerAddress: sponsored.ownerAddress,
        action,
        runtime: sponsored.runtime,
        onStatus: (status: SponsoredExecutionStatus) => {
          if (status === 'awaiting-signature') {
            onWalletRequestStart?.()
          }
        },
      })
      const hash = requireIncludedTransactionHash(result)
      diagnosticHash = hash
      const committed = parseEventLogs({
        abi: PERPS_ORDER_ROUTER_ABI,
        eventName: 'OrderCommitted',
        logs: [...(result.receipt?.receipt?.logs ?? [])],
      }).at(0)
      if (committed?.args.orderId === undefined) {
        throw new Error(
          'Sponsored commit was included, but no OrderCommitted event was found. Refresh account state before retrying.'
        )
      }

      invalidatePerpsReads()
      return {
        hash,
        userOperationHash: result.userOperationHash,
        orderId: committed.args.orderId,
      }
    } catch (error) {
      const sponsorError = findSponsorRequestError(error)
      if (sponsorError) {
        throw new Error(sponsorReasonMessage(sponsorError))
      }
      const message = getPerpsErrorMessage(error, 'commit')
      debugPerpsCommit('failed', {
        message: error instanceof Error ? error.message : String(error),
        normalizedMessage: message,
      })
      if (
        shouldEnrichCommitFailure(message) &&
        diagnosticClient !== undefined &&
        address !== undefined &&
        diagnosticArgs !== undefined &&
        diagnosticSide !== undefined &&
        diagnosticSizeDelta !== undefined &&
        diagnosticMarginDelta !== undefined
      ) {
        const failureHash = diagnosticHash ?? findTransactionHash(error)
        throw new Error(await describeCommitFailure({
          client: diagnosticClient,
          address,
          hash: failureHash,
          intro: failureHash === undefined
            ? 'Commit was not submitted, or the wallet/RPC did not return a transaction hash. No order was created.'
            : 'Commit failed before an order was created, and the RPC did not return a decodable contract error.',
          args: diagnosticArgs,
          isClose,
          side: diagnosticSide,
          sizeDelta: diagnosticSizeDelta,
          marginDelta: diagnosticMarginDelta,
          oraclePrice,
        }))
      }

      throw new Error(message)
    }
  }, [address, invalidatePerpsReads, publicClient, requireSponsoredExecution])

  const executeOrder = useCallback(async (orderId: bigint): Promise<ExecuteOrderResult> => {
    void orderId
    throw new Error(
      'Order finalization is keeper-operated for sponsored Trading Accounts and never asks the owner wallet for native gas.'
    )
  }, [])

  const cleanupExpiredOrder = useCallback(async (
    orderId: bigint
  ): Promise<CleanupExpiredOrderResult> => {
    void orderId
    throw new Error(
      'Expired-order cleanup is keeper-operated for sponsored Trading Accounts and never submits from the owner wallet.'
    )
  }, [])

  const settleTraderClaim = useCallback(async () => {
    try {
      if (!address) {
        throw new Error('Confirm the Plether Trading Account before settling a claim')
      }

      const sponsored = requireSponsoredExecution()
      const action = buildSettleTraderClaimAction({
        account: sponsored.accountAddress,
        cfdEngine: sponsored.manifest.cfdEngine,
      })
      const result = await executeSponsoredPerpsAction({
        manifest: sponsored.manifest,
        ownerAddress: sponsored.ownerAddress,
        action,
        runtime: sponsored.runtime,
      })
      const hash = requireIncludedTransactionHash(result)
      invalidatePerpsReads()
      return hash
    } catch (error) {
      const sponsorError = findSponsorRequestError(error)
      if (sponsorError) throw new Error(sponsorReasonMessage(sponsorError))
      throw new Error(getPerpsErrorMessage(error, 'settleClaim'))
    }
  }, [address, invalidatePerpsReads, requireSponsoredExecution])

  const abandonDepositAuthorization = useCallback(() => {
    if (
      !identity.manifest ||
      !identity.ownerAddress ||
      !identity.accountAddress
    ) {
      return
    }
    clearDepositAuthorization({
      chainId: identity.manifest.chainId,
      ownerAddress: identity.ownerAddress,
      accountAddress: identity.accountAddress,
      token: identity.manifest.usdc,
    })
  }, [identity.accountAddress, identity.manifest, identity.ownerAddress])

  return {
    approveUsdcForMargin,
    depositMargin,
    abandonDepositAuthorization,
    withdrawMargin,
    addPositionMargin,
    commitOrder,
    settleTraderClaim,
    executeOrder,
    cleanupExpiredOrder,
  }
}
