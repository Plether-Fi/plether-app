import { useCallback } from 'react'
import { useQueryClient } from '@tanstack/react-query'
import { isAddressEqual, parseEventLogs, type Address, type Hex } from 'viem'
import { usePublicClient, useSignTypedData, useWriteContract } from 'wagmi'
import {
  buildAddMarginAction,
  buildAuthorizedDepositAction,
  buildReceiveWithAuthorizationTypedData,
  buildSettleTraderClaimAction,
  buildSmartAccountBalanceDepositAction,
  buildWithdrawToOwnerAction,
  type PerpsActionPlan,
  type SponsoredExecutionStatus,
} from '@plether/perps-aa-client'
import {
  ERC20_ABI,
  PERPS_CFD_ENGINE_LENS_ABI,
  PERPS_ORDER_LIFECYCLE_BOOK_ABI,
  PERPS_ORDER_ROUTER_ABI,
  PERPS_PUBLIC_LENS_ABI,
} from '../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA, PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import {
  preparePerpsOrderV2,
  PerpsOrderFundingShortfallError,
} from '../contracts/preparePerpsOrderV2'
import {
  PERPS_CLIENT_INTENT_RESOLUTION,
  PERPS_LIFECYCLE_STATUS,
  executionModeFromPinnedMask,
  persistPerpsOrderRequestV2,
  restorePerpsOrderRequestV2,
  type PreparedPerpsOrderV2,
  type PerpsExecutionMode,
  type PerpsFailedConstraint,
  type PerpsLifecycleStatus,
  type PerpsLifecycleOutcomeSnapshot,
  type PerpsOrderRequestV2,
  type PerpsTerminalReason,
} from '../contracts/perpsOrderV2'
import {
  executeSponsoredPerpsAction,
  clearDepositAuthorization,
  getOrCreateDepositAuthorization,
  sponsorReasonMessage,
  findSponsorRequestError,
  SponsoredPreflightError,
  trackSponsoredOperationPreflightFailure,
  type ExecuteSponsoredPerpsActionResult,
  usePerpsAaRuntime,
  usePerpsIdentity,
  useSponsoredOperationStore,
} from '../perps-aa'
import {
  directionToPerpsSide,
  formatPerpsUsdc,
  notionalUsdcToQuantizedSizeDelta,
  quantizePerpsPositionSize,
  type PerpsDirection,
} from '../utils/perps'
import { COMMIT_UNDECODED_FALLBACK_MESSAGE, getPerpsCloseInvalidReasonMessage, getPerpsErrorMessage, getPerpsOpenRevertMessage } from '../utils/perpsErrors'
import { buildPlaceOrderV2Action } from '../perps-aa/orderActionV2'

interface PrepareOrderInput {
  direction: PerpsDirection
  notionalUsdc: bigint
  sizeDelta?: bigint
  marginUsdc: bigint
  oraclePrice: bigint
  slippagePercent: number
  isClose: boolean
  selectedMaxLeverageBps: number
}

interface CommitOrderInput extends PrepareOrderInput {
  preparedOrder: PreparedPerpsOrderV2
  onStatus?: (status: SponsoredExecutionStatus) => void
  onIncluded?: (result: CommitOrderResult) => void
}

export interface CommitOrderResult {
  account: Address
  clientOrderId: Hex
  hash?: Hex
  userOperationHash?: Hex
  orderId: bigint
  replayed: boolean
}

interface ExecuteOrderResult {
  hash: Hex
  executionPrice?: bigint
  failedReason?: number
}

const PERPS_CONTRACT_ADDRESSES = new Set(
  Object.values(PERPS_ARBITRUM_SEPOLIA)
    .map((address) => address.toLowerCase())
)
const PERPS_DYNAMIC_READ_FUNCTIONS = new Set([
  'allowance',
  'balanceOf',
  'getAccountLedgerSnapshot',
  'getActivePositionProtection',
  'getFreeBuyingPowerUsdc',
  'getPendingOrderView',
  'getPendingOrders',
  'pendingPolicy',
  'getPoolLiquidityView',
  'getPosition',
  'getProtocolStatus',
  'getTraderAccount',
  'isFadWindow',
  'positions',
  'previewClose',
  'previewOpen',
  'sides',
])

function isPerpsContractAddress(value: unknown): boolean {
  return typeof value === 'string' && PERPS_CONTRACT_ADDRESSES.has(value.toLowerCase())
}

function isPerpsDynamicContractQuery(queryKey: readonly unknown[]): boolean {
  const [queryType, parameters] = queryKey
  if (queryType !== 'readContract' && queryType !== 'readContracts') return false
  if (typeof parameters !== 'object' || parameters === null) return false

  const queryParameters = parameters as {
    chainId?: unknown
    address?: unknown
    functionName?: unknown
    contracts?: { address?: unknown; functionName?: unknown }[]
  }
  if (
    queryParameters.chainId !== undefined &&
    queryParameters.chainId !== PERPS_ARBITRUM_SEPOLIA_CHAIN_ID
  ) {
    return false
  }
  if (
    isPerpsContractAddress(queryParameters.address) &&
    typeof queryParameters.functionName === 'string' &&
    PERPS_DYNAMIC_READ_FUNCTIONS.has(queryParameters.functionName)
  ) return true
  if (!Array.isArray(queryParameters.contracts)) return false

  return queryParameters.contracts.some(
    (contract) =>
      isPerpsContractAddress(contract.address) &&
      typeof contract.functionName === 'string' &&
      PERPS_DYNAMIC_READ_FUNCTIONS.has(contract.functionName)
  )
}

interface CleanupExpiredOrderResult {
  hash: Hex
}

type PerpsPublicClient = NonNullable<ReturnType<typeof usePublicClient>>
type CommitOrderArgs = readonly [PerpsOrderRequestV2]

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

function commitOrderResult(
  result: ExecuteSponsoredPerpsActionResult,
  expected: {
    accountAddress: Address
    orderRouter: Address
    orderLifecycleBook: Address
    clientOrderId: Hex
  }
): CommitOrderResult {
  const hash = requireIncludedTransactionHash(result)
  if (
    !result.receipt.success ||
    result.receipt.receipt.status !== 'success'
  ) {
    throw new Error(
      'The sponsored commit UserOperation reverted before creating an order.'
    )
  }

  const committed = parseEventLogs({
    abi: PERPS_ORDER_ROUTER_ABI,
    eventName: 'OrderCommitted',
    logs: result.receipt.logs.filter((log) =>
      isAddressEqual(log.address, expected.orderRouter)
    ),
  }).filter((event) =>
    isAddressEqual(event.args.account, expected.accountAddress)
  )
  if (committed.length !== 1) {
    throw new Error(
      'Sponsored commit was included, but no unique matching OrderCommitted event was found. Refresh account state before retrying.'
    )
  }
  const registered = parseEventLogs({
    abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI,
    eventName: 'IntentRegistered',
    logs: result.receipt.logs.filter((log) =>
      isAddressEqual(log.address, expected.orderLifecycleBook)
    ),
  }).filter((event) =>
    isAddressEqual(event.args.account, expected.accountAddress) &&
    event.args.clientOrderId.toLowerCase() === expected.clientOrderId.toLowerCase()
  )
  if (
    registered.length !== 1 ||
    registered[0].args.orderId !== committed[0].args.orderId
  ) {
    throw new Error(
      'Sponsored commit was included, but its matching IntentRegistered lifecycle evidence was not found. Refresh account state before retrying.'
    )
  }

  return {
    account: expected.accountAddress,
    clientOrderId: expected.clientOrderId,
    hash,
    userOperationHash: result.userOperationHash,
    orderId: committed[0].args.orderId,
    replayed: false,
  }
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
  const { writeContractAsync } = useWriteContract()
  const queryClient = useQueryClient()

  const invalidatePerpsReads = useCallback(() => {
    void queryClient.invalidateQueries({
      predicate: (query) => isPerpsDynamicContractQuery(query.queryKey),
    })
  }, [queryClient])

  const requireSponsoredExecution = useCallback(() => {
    if (!identity.isAaManifestConfigured) {
      throw new SponsoredPreflightError({
        reason: 'MANIFEST_NOT_CONFIGURED',
        message:
          'Perps is sponsorship-only on testnet. Direct owner-wallet transactions are disabled.',
      })
    }
    if (identity.status !== 'ready' || !identity.accountAddress || !identity.ownerAddress) {
      throw new SponsoredPreflightError({
        reason: 'IDENTITY_NOT_READY',
        message:
          identity.error?.message ??
          'Confirm the Plether Trading Account before submitting this action.',
        cause: identity.error,
      })
    }
    if (!identity.manifest) {
      throw new SponsoredPreflightError({
        reason: 'MANIFEST_UNAVAILABLE',
        message: 'The reviewed gas-sponsorship manifest is unavailable.',
      })
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
      throw new SponsoredPreflightError({
        reason: 'MANIFEST_MISMATCH',
        message:
          'The gas-sponsorship manifest does not match this frontend deployment. Your action was not sent.',
      })
    }
    if (!identity.sponsorshipEnabled) {
      throw new SponsoredPreflightError({
        reason: 'SPONSORSHIP_DISABLED',
        message:
          'Plether gas sponsorship is temporarily unavailable. Your action was kept under the Trading Account and was not sent.',
      })
    }
    if (!aaRuntime) {
      throw new SponsoredPreflightError({
        reason: 'RUNTIME_UNAVAILABLE',
        message:
          'The reviewed smart-account wallet adapter is unavailable. Your action was not sent.',
      })
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

  const fundTradingAccount = useCallback(async (amount: bigint) => {
    try {
      if (amount <= 0n) {
        throw new Error('Funding amount must be greater than zero')
      }

      const sponsored = requireSponsoredExecution()
      const client = requireClient(publicClient)
      const hash = await writeContractAsync({
        account: sponsored.ownerAddress,
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: sponsored.manifest.usdc,
        abi: ERC20_ABI,
        functionName: 'transfer',
        args: [sponsored.accountAddress, amount],
      })
      const receipt = await client.waitForTransactionReceipt({ hash })
      if (receipt.status === 'reverted') {
        throw new Error('Trading Account funding transaction reverted')
      }

      invalidatePerpsReads()
      return hash
    } catch (error) {
      throw new Error(getPerpsErrorMessage(error, 'fund'))
    }
  }, [
    invalidatePerpsReads,
    publicClient,
    requireSponsoredExecution,
    writeContractAsync,
  ])

  const depositMargin = useCallback(async (
    amount: bigint,
    _allowance?: bigint,
    source?: 'owner' | 'account'
  ) => {
    let executionStarted = false
    try {
      if (!address) {
        throw new SponsoredPreflightError({
          reason: 'TRADING_ACCOUNT_UNAVAILABLE',
          message: 'Confirm the Plether Trading Account before depositing margin',
        })
      }
      if (amount <= 0n) {
        throw new SponsoredPreflightError({
          reason: 'INVALID_AMOUNT',
          message: 'Deposit amount must be greater than zero',
        })
      }

      const sponsored = requireSponsoredExecution()
      const useOwnerAuthorization = source === 'owner' || (
        source === undefined &&
        sponsored.manifest.usdcSupportsEip3009
      )
      let action: PerpsActionPlan
      let authorizationNonce: Hex | undefined
      if (useOwnerAuthorization) {
        if (
          !sponsored.manifest.usdcSupportsEip3009 ||
          !sponsored.manifest.usdcEip712Name ||
          !sponsored.manifest.usdcEip712Version
        ) {
          throw new SponsoredPreflightError({
            reason: 'OWNER_AUTHORIZATION_UNAVAILABLE',
            message:
              'Owner-funded onboarding is disabled until this USDC deployment and its exact EIP-712 domain are verified.',
          })
        }
        try {
          const authorization = getOrCreateDepositAuthorization({
            chainId: sponsored.manifest.chainId,
            ownerAddress: sponsored.ownerAddress,
            accountAddress: sponsored.accountAddress,
            token: sponsored.manifest.usdc,
            amount,
          })
          authorizationNonce = authorization.nonce
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
        } catch (error) {
          throw new SponsoredPreflightError({
            reason: 'OWNER_AUTHORIZATION_FAILED',
            message: error instanceof Error
              ? error.message
              : 'Unable to authorize the owner-funded margin deposit.',
            cause: error,
          })
        }
      } else {
        try {
          action = buildSmartAccountBalanceDepositAction({
            account: sponsored.accountAddress,
            usdc: sponsored.manifest.usdc,
            clearinghouse: sponsored.manifest.marginClearinghouse,
            amount,
          })
        } catch (error) {
          throw new SponsoredPreflightError({
            reason: 'ACTION_BUILD_FAILED',
            message: 'Unable to build the sponsored margin deposit.',
            cause: error,
          })
        }
      }
      executionStarted = true
      const result = await executeSponsoredPerpsAction({
        manifest: sponsored.manifest,
        ownerAddress: sponsored.ownerAddress,
        action,
        runtime: sponsored.runtime,
        authorizationTokenToClearOnConfirmation: useOwnerAuthorization
          ? sponsored.manifest.usdc
          : undefined,
        authorizationNonceToClearOnConfirmation: authorizationNonce,
      })
      const hash = requireIncludedTransactionHash(result)
      invalidatePerpsReads()
      return hash
    } catch (error) {
      if (!executionStarted) {
        trackSponsoredOperationPreflightFailure({
          action: 'deposit',
          manifestVersion: identity.manifest?.version,
          accountMode: identity.manifest?.smartAccountMode,
          walletFamily: aaRuntime?.walletFamily,
          walletVersion: aaRuntime?.walletVersion,
        }, error)
      }
      const sponsorError = findSponsorRequestError(error)
      if (sponsorError) throw new Error(sponsorReasonMessage(sponsorError))
      throw new Error(getPerpsErrorMessage(error, 'deposit'))
    }
  }, [
    aaRuntime,
    address,
    identity.manifest,
    invalidatePerpsReads,
    requireSponsoredExecution,
    signTypedDataAsync,
  ])

  const withdrawMargin = useCallback(async (amount: bigint) => {
    try {
      if (!address) {
        throw new Error('Confirm the Plether Trading Account before withdrawing margin')
      }
      if (amount <= 0n) {
        throw new Error('Withdraw amount must be greater than zero')
      }

      const sponsored = requireSponsoredExecution()
      const action = buildWithdrawToOwnerAction({
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

  const prepareOrder = useCallback(async ({
    direction,
    notionalUsdc,
    sizeDelta: sizeDeltaOverride,
    marginUsdc,
    oraclePrice,
    slippagePercent,
    isClose,
    selectedMaxLeverageBps,
  }: PrepareOrderInput): Promise<PreparedPerpsOrderV2> => {
    try {
      if (!address) {
        throw new Error(
          'Confirm the Plether Trading Account before reviewing an order'
        )
      }
      if (notionalUsdc <= 0n) {
        throw new Error('Order size must be greater than zero')
      }
      if (oraclePrice <= 0n) throw new Error('plDXY Perp price is not available')
      const requestedSizeDelta = sizeDeltaOverride ?? notionalUsdcToQuantizedSizeDelta(
        notionalUsdc,
        oraclePrice
      )
      if (requestedSizeDelta <= 0n) throw new Error('Order size is too small')
      if (quantizePerpsPositionSize(requestedSizeDelta, 'down') !== requestedSizeDelta) {
        throw new Error('Order size must use 100 plDXY increments')
      }
      const sizeDelta = requestedSizeDelta
      const sponsored = requireSponsoredExecution()
      const activeOperation = useSponsoredOperationStore
        .getState()
        .getActiveOperation(sponsored.accountAddress)
      if (activeOperation?.orderRequestV2 !== undefined) {
        const request = restorePerpsOrderRequestV2(
          activeOperation.orderRequestV2
        )
        if (
          request.side !== directionToPerpsSide(direction) ||
          request.sizeDelta !== sizeDelta ||
          request.isClose !== isClose
        ) {
          throw new Error(
            'A different immutable order is already awaiting recovery. Finish or cancel that sponsored operation before reviewing another order.'
          )
        }
        return {
          account: sponsored.accountAddress,
          manifestVersion: activeOperation.manifestVersion,
          orderRouter: sponsored.manifest.orderRouter,
          orderLifecycleBook: sponsored.manifest.orderLifecycleBook,
          request,
          executionBountyUsdc: request.bounds.maxExecutionBountyUsdc,
          reviewedBlockNumber: 0n,
          reviewedBlockHash: `0x${'0'.repeat(64)}`,
          reviewedPrice: oraclePrice,
          protection: {
            validUntil: request.bounds.validUntil,
            executionMode: executionModeFromPinnedMask(
              request.bounds.allowedExecutionModes
            ),
            executionBountyUsdc: request.bounds.maxExecutionBountyUsdc,
          },
        }
      }
      const client = requireClient(publicClient)
      return await preparePerpsOrderV2(client, sponsored.manifest, {
        account: sponsored.accountAddress,
        direction,
        side: directionToPerpsSide(direction),
        sizeDelta,
        marginDelta: isClose ? 0n : marginUsdc,
        slippagePercent,
        isClose,
        selectedMaxLeverageBps,
      })
    } catch (error) {
      if (error instanceof PerpsOrderFundingShortfallError) throw error
      const sponsorError = findSponsorRequestError(error)
      if (sponsorError) throw new Error(sponsorReasonMessage(sponsorError))
      throw new Error(getPerpsErrorMessage(error, 'commit'), { cause: error })
    }
  }, [address, publicClient, requireSponsoredExecution])

  const commitOrder = useCallback(async ({
    direction,
    notionalUsdc,
    sizeDelta: sizeDeltaOverride,
    marginUsdc,
    oraclePrice,
    isClose,
    preparedOrder,
    onStatus,
    onIncluded,
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
        isClose,
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
      })
      if (!address) {
        throw new Error('Confirm the Plether Trading Account before committing an order')
      }
      if (notionalUsdc <= 0n) {
        throw new Error('Order size must be greater than zero')
      }
      const side = directionToPerpsSide(direction)
      const sizeDelta = sizeDeltaOverride ?? preparedOrder.request.sizeDelta
      if (sizeDelta <= 0n) {
        throw new Error('Order size is too small')
      }
      if (quantizePerpsPositionSize(sizeDelta, 'down') !== sizeDelta) {
        throw new Error('Order size must use 100 plDXY increments')
      }
      const request = preparedOrder.request
      const marginDelta = request.marginDelta
      if (
        !isAddressEqual(preparedOrder.account, address) ||
        request.side !== side ||
        request.sizeDelta !== sizeDelta ||
        request.isClose !== isClose
      ) {
        throw new Error(
          'The trade changed after final review. Review fresh execution protections before signing.'
        )
      }
      const args = [request] as const
      diagnosticArgs = args
      diagnosticSide = side
      diagnosticSizeDelta = sizeDelta
      diagnosticMarginDelta = marginDelta
      debugPerpsCommit('args-ready', {
        side,
        sizeDelta,
        marginDelta,
        targetPrice: request.targetPrice,
        clientOrderId: request.clientOrderId,
        validUntil: request.bounds.validUntil,
        isClose,
      })
      const client = requireClient(publicClient)
      diagnosticClient = client
      debugPerpsCommit('client-ready')
      const sponsored = requireSponsoredExecution()
      if (
        sponsored.manifest.version !== preparedOrder.manifestVersion ||
        !isAddressEqual(sponsored.manifest.orderRouter, preparedOrder.orderRouter) ||
        !isAddressEqual(
          sponsored.manifest.orderLifecycleBook,
          preparedOrder.orderLifecycleBook
        )
      ) {
        throw new Error(
          'The reviewed deployment changed. Review the order again before signing.'
        )
      }

      const [resolution, resolvedOrderId] = await client.readContract({
        address: preparedOrder.orderLifecycleBook,
        abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI,
        functionName: 'resolveClientIntent',
        args: [address, request],
      })
      if (resolution === PERPS_CLIENT_INTENT_RESOLUTION.CONFLICT) {
        throw new Error(
          'Order integrity error: this client order ID is already bound to a different immutable request.'
        )
      }
      if (resolution === PERPS_CLIENT_INTENT_RESOLUTION.EXACT_REPLAY) {
        const intent = await client.readContract({
          address: preparedOrder.orderLifecycleBook,
          abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI,
          functionName: 'clientIntent',
          args: [address, request.clientOrderId],
        })
        if (intent.orderId !== resolvedOrderId || resolvedOrderId === 0n) {
          throw new Error(
            'Order integrity error: lifecycle replay records are inconsistent.'
          )
        }
        const replayedResult: CommitOrderResult = {
          account: address,
          clientOrderId: request.clientOrderId,
          orderId: resolvedOrderId,
          replayed: true,
        }
        onIncluded?.(replayedResult)
        invalidatePerpsReads()
        return replayedResult
      }
      if (resolution !== PERPS_CLIENT_INTENT_RESOLUTION.UNUSED) {
        throw new Error('Order integrity error: unknown client-intent resolution')
      }

      await client.simulateContract({
        account: address,
        address: sponsored.manifest.orderRouter,
        abi: PERPS_ORDER_ROUTER_ABI,
        functionName: 'commitOrder',
        args,
      })

      const action = buildPlaceOrderV2Action({
        account: sponsored.accountAddress,
        orderRouter: sponsored.manifest.orderRouter,
        request,
      })
      const expectedCommit = {
        accountAddress: sponsored.accountAddress,
        orderRouter: sponsored.manifest.orderRouter,
        orderLifecycleBook: preparedOrder.orderLifecycleBook,
        clientOrderId: request.clientOrderId,
      }
      const result = await executeSponsoredPerpsAction({
        manifest: sponsored.manifest,
        ownerAddress: sponsored.ownerAddress,
        action,
        runtime: sponsored.runtime,
        orderRequestV2: persistPerpsOrderRequestV2(address, request),
        onStatus,
        onIncluded: (includedResult) => {
          const includedCommit = commitOrderResult(
            includedResult,
            expectedCommit
          )
          diagnosticHash = includedCommit.hash
          onIncluded?.(includedCommit)
        },
      })
      const committedResult = commitOrderResult(result, expectedCommit)
      diagnosticHash = committedResult.hash
      invalidatePerpsReads()
      return committedResult
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
        throw new Error(
          await describeCommitFailure({
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
          }),
          { cause: error }
        )
      }

      throw new Error(message, { cause: error })
    }
  }, [address, invalidatePerpsReads, publicClient, requireSponsoredExecution])

  const executeOrder = useCallback(async (orderId: bigint): Promise<ExecuteOrderResult> => {
    void orderId
    throw new Error(
      'Order finalization is keeper-operated for sponsored Trading Accounts and never asks the owner wallet for native gas.'
    )
  }, [])

  const readOrderLifecycleOutcome = useCallback(async (
    orderId: bigint
  ): Promise<PerpsLifecycleOutcomeSnapshot | undefined> => {
    const client = requireClient(publicClient)
    const sponsored = requireSponsoredExecution()
    const outcome = await client.readContract({
      address: sponsored.manifest.orderLifecycleBook,
      abi: PERPS_ORDER_LIFECYCLE_BOOK_ABI,
      functionName: 'outcome',
      args: [orderId],
    })
    const status = outcome.status
    if (
      status !== PERPS_LIFECYCLE_STATUS.EXECUTED &&
      status !== PERPS_LIFECYCLE_STATUS.FAILED
    ) {
      return undefined
    }

    return {
      orderId,
      account: outcome.account,
      clientOrderId: outcome.clientOrderId,
      status: status as PerpsLifecycleStatus,
      terminalReason: outcome.reason as PerpsTerminalReason,
      executionMode: outcome.executionMode as PerpsExecutionMode,
      terminalBlock: outcome.terminalBlock,
      terminalTime: outcome.terminalTime,
      executionPrice: outcome.executionPrice,
      failedConstraint: outcome.failedConstraint as PerpsFailedConstraint,
      receiptHash: outcome.receiptHash,
    }
  }, [publicClient, requireSponsoredExecution])

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
    fundTradingAccount,
    depositMargin,
    abandonDepositAuthorization,
    withdrawMargin,
    addPositionMargin,
    prepareOrder,
    commitOrder,
    settleTraderClaim,
    readOrderLifecycleOutcome,
    executeOrder,
    cleanupExpiredOrder,
  }
}
