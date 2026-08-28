import { useEffect, useMemo, useState, type KeyboardEvent, type ReactNode } from 'react'
import { useAppKit } from '@reown/appkit/react'
import { formatUnits, parseUnits, zeroAddress, type Address } from 'viem'
import { useAccount, useChainId, useReadContracts } from 'wagmi'
import { Link, useParams } from 'react-router-dom'
import {
  usePerpsVaultHistory,
  type VaultHistory,
  type VaultHistoryPoint,
  type VaultHistoryTranche,
} from '../api'
import { TokenInput } from '../components/TokenInput'
import { PerpsPoolLiquidityDetails } from '../components/PerpsPoolLiquidityDetails'
import { Alert, Badge, Button, Modal, TokenAmount, TokenLabel, Tooltip } from '../components/ui'
import { DOCS_LINKS } from '../config/docs'
import { syncAppKitModalStyleOverrides } from '../config/wagmi'
import {
  ERC20_ABI,
  PERPS_CFD_ENGINE_ABI,
  PERPS_HOUSE_POOL_ABI,
  PERPS_PUBLIC_LENS_ABI,
  TRANCHE_VAULT_READ_ABI,
} from '../contracts/abis'
import {
  PERPS_ARBITRUM_SEPOLIA,
  PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
} from '../contracts/perpsAddresses'
import {
  decodePendingTrancheState,
  decodePoolLiquidityView,
  decodeProtocolStatusView,
  decodeTrancheQueueView,
  decodeTrancheView,
} from '../contracts/vaultViewAdapters'
import { PERPS_POSITION_SIZE_TO_USDC_SCALE } from '../contracts/perpsConstants'
import {
  useSwitchToArbitrumSepolia,
  useVaultRequests,
  useVaultTransactions,
  type VaultDepositRequest,
  type VaultRedeemRequest,
} from '../hooks'
import { dxyExposureFromContractNotional, formatPerpsUsdc } from '../utils/perps'
import { calculatePerpsPoolCapital } from '../utils/perpsPoolCapital'

type TrancheId = 'senior' | 'junior'
type DetailTab = 'overview' | 'performance' | 'risk' | 'activity'
type ActionMode = 'deposit' | 'withdraw'
type DataStatus = 'live' | 'partial' | 'syncing' | 'unavailable'

interface TrancheDefinition {
  id: TrancheId
  name: string
  token: string
  eyebrow: string
  shortDescription: string
  description: string
  returnModel: string
  lossPriority: string
  withdrawalPriority: string
  upside: string
  primaryRisk: string
  riskLabel: string
  riskVariant: 'info' | 'warning'
  targetReturn: string
  markClassName: string
  valueClassName: string
  barClassName: string
  featureItems: {
    label: string
    text: string
  }[]
  riskItems: string[]
  address: Address
}

interface ContractResult {
  status: 'failure' | 'success'
  result?: unknown
}

interface PoolSnapshot {
  totalAssetsUsdc?: bigint
  freeUsdc?: bigint
  withdrawalReservedUsdc?: bigint
  pendingRecapitalizationUsdc?: bigint
  pendingTradingRevenueUsdc?: bigint
  seniorPrincipalUsdc?: bigint
  juniorPrincipalUsdc?: bigint
  seniorHighWaterMarkUsdc?: bigint
  currentTerminalDeficitUsdc?: bigint
  markFresh?: boolean
  oracleFrozen?: boolean
  degradedMode?: boolean
  seniorImpaired?: boolean
  seniorImpairmentGapUsdc?: bigint
  seniorPoolWithdrawCapUsdc?: bigint
  juniorPoolWithdrawCapUsdc?: bigint
  maxSeniorExposureUsdc?: bigint
  maxSeniorShareBps?: bigint
  seniorDepositCapacityUsdc?: bigint
  reservedSeniorDepositAssetsUsdc?: bigint
  seniorReservationsWithinLimits?: boolean
  minTrancheDepositUsdc?: bigint
  markPrice?: bigint
  longOpenCapacityUsdc?: bigint
  shortOpenCapacityUsdc?: bigint
}

interface TrancheLiveData {
  totalAssets?: bigint
  totalSupply?: bigint
  effectiveTotalSupply?: bigint
  pendingMaintenanceFeeShares?: bigint
  maintenanceFeeAprBps?: bigint
  maintenanceFeeRecipient?: Address
  userShares?: bigint
  maxRequestDeposit?: bigint
  maxRequestRedeem?: bigint
  allowance?: bigint
  currentEpoch?: bigint
  nextRequestEpoch?: bigint
  nextRequestCutoffTime?: bigint
  depositBacklog?: boolean
  redeemBacklog?: boolean
  settlementLive?: boolean
  poolPaused?: boolean
  lpEpochSettlementPaused?: boolean
  frozenLpFeeBps?: bigint
  depositEnabled?: boolean
  withdrawEnabled?: boolean
  poolWithdrawCapUsdc?: bigint
  sharePrice?: number
  hasCoreData: boolean
  hasDepositData: boolean
  hasUserData: boolean
}

interface VaultsSnapshot {
  status: DataStatus
  pool: PoolSnapshot
  walletUsdc?: bigint
  hasLivePoolData: boolean
  tranches: Record<TrancheId, TrancheLiveData>
  refresh: () => void
}

const USDC_DECIMALS = 6
const SHARE_DECIMALS = 9
const LENS_SHARE_PRICE_DECIMALS = 18 + USDC_DECIMALS - SHARE_DECIMALS
const SHARE_PRICE_PROBE = 10n ** 27n
const SEVEN_DAYS_SECONDS = 7 * 24 * 60 * 60
const VAULT_EPOCH_DURATION_SECONDS = 60 * 60
const VAULT_PERFORMANCE_CHART_COLOR = '#FFAB96'
const EXPLORER_BASE_URL = 'https://sepolia.arbiscan.io/address'
const DEPOSIT_PROBE_ACCOUNT = '0x000000000000000000000000000000000000dEaD' as Address
const WAD = 10n ** 18n

const TRANCHES: Record<TrancheId, TrancheDefinition> = {
  senior: {
    id: 'senior',
    name: 'Senior Vault',
    token: 'psLP',
    eyebrow: 'Priority capital',
    shortDescription: 'Targeted return with first access to free LP liquidity.',
    description:
      'Senior exchanges residual upside for relative protection. It receives a Junior-funded target coupon, is restored toward its high-water mark before Junior receives new revenue, and absorbs losses only after Junior is exhausted.',
    returnModel: 'Target coupon funded by Junior capital',
    lossPriority: 'Second loss, after Junior',
    withdrawalPriority: 'Matured requests settle before Junior',
    upside: 'Target coupon and restoration priority',
    primaryRisk: 'Coupon can stop and principal can still be impaired',
    riskLabel: 'Lower relative risk',
    riskVariant: 'info',
    targetReturn: 'Target coupon',
    markClassName: 'border-brand-peach/60 bg-brand-peach/10 text-brand-peach',
    valueClassName: 'text-brand-peach',
    barClassName: 'bg-brand-peach',
    featureItems: [
      {
        label: 'Loss order',
        text: 'Junior absorbs realized losses before Senior',
      },
      {
        label: 'Return',
        text: 'Receives its target coupon when available; impairment is restored before Junior receives residual revenue',
      },
      {
        label: 'Withdrawals',
        text: 'Matured Senior withdrawal requests are funded first from available LP liquidity each epoch',
      },
    ],
    riskItems: [
      'The target coupon is not guaranteed and is limited by available Junior capital.',
      'Losses can reach Senior after Junior is fully exhausted.',
      'Trader claims and bounded trading liabilities rank ahead of both LP tranches.',
      'Positive share value does not guarantee immediate withdrawal liquidity.',
    ],
    address: PERPS_ARBITRUM_SEPOLIA.seniorVault,
  },
  junior: {
    id: 'junior',
    name: 'Junior Vault',
    token: 'pjLP',
    eyebrow: 'Residual capital',
    shortDescription: 'First-loss capital with variable residual upside.',
    description:
      'Junior funds the Senior target coupon and absorbs HousePool losses first. In exchange, it receives residual realized trading revenue, including the LP share of liquidation charges, after Senior restoration and coupon obligations are satisfied.',
    returnModel: 'Residual HousePool performance',
    lossPriority: 'First loss',
    withdrawalPriority: 'Remainder after matured Senior requests and the required buffer',
    upside: 'Variable residual trading revenue',
    primaryRisk: 'Can be partially or completely wiped before Senior is impaired',
    riskLabel: 'Higher relative risk',
    riskVariant: 'warning',
    targetReturn: 'Variable residual',
    markClassName: 'border-brand-orange/60 bg-brand-orange/10 text-brand-orange',
    valueClassName: 'text-brand-orange',
    barClassName: 'bg-brand-orange',
    featureItems: [
      {
        label: 'Loss order',
        text: 'Absorbs realized losses first, protecting Senior',
      },
      {
        label: 'Return',
        text: 'Receives residual revenue and the LP share of liquidation charges after Senior obligations',
      },
      {
        label: 'Withdrawals',
        text: 'Matured Junior withdrawals receive eligible liquidity remaining after Senior requests and the required first-loss buffer; they may remain queued',
      },
    ],
    riskItems: [
      'Junior pays the Senior target coupon from its own accounting principal.',
      'Junior absorbs realized HousePool losses before Senior is affected.',
      'Junior withdrawals can be zero while its shares retain positive accounting value.',
      'A sufficiently large loss can wipe the tranche out completely.',
    ],
    address: PERPS_ARBITRUM_SEPOLIA.juniorVault,
  },
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

function asBigInt(value: unknown): bigint | undefined {
  return typeof value === 'bigint' ? value : undefined
}

function asBoolean(value: unknown): boolean | undefined {
  return typeof value === 'boolean' ? value : undefined
}

function openInterestNotionalUsdc(
  openInterest: bigint | undefined,
  markPrice: bigint | undefined
): bigint | undefined {
  if (openInterest === undefined || markPrice === undefined) return undefined
  return (openInterest * markPrice) / PERPS_POSITION_SIZE_TO_USDC_SCALE
}

function openCapacityUsdc({
  selectedOpenInterestUsdc,
  oppositeOpenInterestUsdc,
  poolAssetsUsdc,
  maxSkewRatio,
}: {
  selectedOpenInterestUsdc: bigint | undefined
  oppositeOpenInterestUsdc: bigint | undefined
  poolAssetsUsdc: bigint | undefined
  maxSkewRatio: bigint | undefined
}): bigint | undefined {
  if (
    selectedOpenInterestUsdc === undefined
    || oppositeOpenInterestUsdc === undefined
    || poolAssetsUsdc === undefined
    || maxSkewRatio === undefined
  ) {
    return undefined
  }

  const maxSkewUsdc = (poolAssetsUsdc * maxSkewRatio) / WAD
  return maxSkewUsdc + oppositeOpenInterestUsdc > selectedOpenInterestUsdc
    ? maxSkewUsdc + oppositeOpenInterestUsdc - selectedOpenInterestUsdc
    : 0n
}

function calculateSharePrice(
  totalAssets: bigint | undefined,
  totalSupply: bigint | undefined
): number | undefined {
  if (totalAssets === undefined || totalSupply === undefined || totalSupply === 0n) {
    return undefined
  }
  const assets = Number(formatUnits(totalAssets, USDC_DECIMALS))
  const shares = Number(formatUnits(totalSupply, SHARE_DECIMALS))
  return shares > 0 ? assets / shares : undefined
}

function calculateConvertedSharePrice(convertedAssets: bigint | undefined): number | undefined {
  if (convertedAssets === undefined) return undefined
  const assets = Number(formatUnits(convertedAssets, USDC_DECIMALS))
  const shares = Number(formatUnits(SHARE_PRICE_PROBE, SHARE_DECIMALS))
  return Number.isFinite(assets) && shares > 0 ? assets / shares : undefined
}

function calculateLensSharePrice(sharePrice: bigint | undefined): number | undefined {
  if (sharePrice === undefined) return undefined
  const value = Number(formatUnits(sharePrice, LENS_SHARE_PRICE_DECIMALS))
  return Number.isFinite(value) ? value : undefined
}

function useVaultsSnapshot(address: Address | undefined): VaultsSnapshot {
  const readAccount = address ?? zeroAddress
  const depositReceiver = address ?? DEPOSIT_PROBE_ACCOUNT
  const { data, isLoading, refetch } = useReadContracts({
    contracts: [
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.housePool,
        abi: PERPS_HOUSE_POOL_ABI,
        functionName: 'getPoolLiquidityView',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.seniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'totalAssets',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.seniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'totalSupply',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.seniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'balanceOf',
        args: [readAccount],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.seniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'maxRequestDeposit',
        args: [depositReceiver],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.seniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'maxRequestRedeem',
        args: [readAccount],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.juniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'totalAssets',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.juniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'totalSupply',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.juniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'balanceOf',
        args: [readAccount],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.juniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'maxRequestDeposit',
        args: [depositReceiver],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.juniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'maxRequestRedeem',
        args: [readAccount],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.usdc,
        abi: ERC20_ABI,
        functionName: 'balanceOf',
        args: [readAccount],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.usdc,
        abi: ERC20_ABI,
        functionName: 'allowance',
        args: [readAccount, PERPS_ARBITRUM_SEPOLIA.seniorVault],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.usdc,
        abi: ERC20_ABI,
        functionName: 'allowance',
        args: [readAccount, PERPS_ARBITRUM_SEPOLIA.juniorVault],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.seniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'getRequestEpochWindow',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.juniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'getRequestEpochWindow',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.seniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'convertToAssets',
        args: [SHARE_PRICE_PROBE],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.juniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'convertToAssets',
        args: [SHARE_PRICE_PROBE],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
        abi: PERPS_PUBLIC_LENS_ABI,
        functionName: 'getProtocolStatus',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
        abi: PERPS_CFD_ENGINE_ABI,
        functionName: 'sides',
        args: [0n],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
        abi: PERPS_CFD_ENGINE_ABI,
        functionName: 'sides',
        args: [1n],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.cfdEngine,
        abi: PERPS_CFD_ENGINE_ABI,
        functionName: 'riskParams',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
        abi: PERPS_PUBLIC_LENS_ABI,
        functionName: 'getSeniorTranche',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
        abi: PERPS_PUBLIC_LENS_ABI,
        functionName: 'getJuniorTranche',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
        abi: PERPS_PUBLIC_LENS_ABI,
        functionName: 'getTrancheQueues',
        args: [true],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
        abi: PERPS_PUBLIC_LENS_ABI,
        functionName: 'getTrancheQueues',
        args: [false],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.housePool,
        abi: PERPS_HOUSE_POOL_ABI,
        functionName: 'getPendingTrancheState',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.housePool,
        abi: PERPS_HOUSE_POOL_ABI,
        functionName: 'maxSeniorExposureUsdc',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.housePool,
        abi: PERPS_HOUSE_POOL_ABI,
        functionName: 'maxSeniorShareBps',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.housePool,
        abi: PERPS_HOUSE_POOL_ABI,
        functionName: 'getSeniorDepositCapacity',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.housePool,
        abi: PERPS_HOUSE_POOL_ABI,
        functionName: 'reservedSeniorDepositAssetsUsdc',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.housePool,
        abi: PERPS_HOUSE_POOL_ABI,
        functionName: 'areSeniorDepositReservationsWithinLimits',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.housePool,
        abi: PERPS_HOUSE_POOL_ABI,
        functionName: 'minTrancheDepositUsdc',
      },
    ],
    query: {
      refetchInterval: 60_000,
    },
  })

  return useMemo(() => {
    const results = data as readonly ContractResult[] | undefined
    const poolView = decodePoolLiquidityView(readResult(results, 0))
    const directSeniorAssets = asBigInt(readResult(results, 1))
    const directSeniorSupply = asBigInt(readResult(results, 2))
    const seniorUserShares = asBigInt(readResult(results, 3))
    const seniorMaxRequestDeposit = asBigInt(readResult(results, 4))
    const seniorMaxRequestRedeem = asBigInt(readResult(results, 5))
    const directJuniorAssets = asBigInt(readResult(results, 6))
    const directJuniorSupply = asBigInt(readResult(results, 7))
    const juniorUserShares = asBigInt(readResult(results, 8))
    const juniorMaxRequestDeposit = asBigInt(readResult(results, 9))
    const juniorMaxRequestRedeem = asBigInt(readResult(results, 10))
    const walletUsdc = asBigInt(readResult(results, 11))
    const seniorAllowance = asBigInt(readResult(results, 12))
    const juniorAllowance = asBigInt(readResult(results, 13))
    const seniorRequestWindow = readResult(results, 14)
    const juniorRequestWindow = readResult(results, 15)
    const seniorConvertedAssets = asBigInt(readResult(results, 16))
    const juniorConvertedAssets = asBigInt(readResult(results, 17))
    const protocolStatus = decodeProtocolStatusView(readResult(results, 18))
    const bullSide = readResult(results, 19)
    const bearSide = readResult(results, 20)
    const riskParams = readResult(results, 21)
    const seniorTranche = decodeTrancheView(readResult(results, 22))
    const juniorTranche = decodeTrancheView(readResult(results, 23))
    const seniorQueue = decodeTrancheQueueView(readResult(results, 24))
    const juniorQueue = decodeTrancheQueueView(readResult(results, 25))
    const pendingTrancheState = decodePendingTrancheState(readResult(results, 26))
    const maxSeniorExposureUsdc = asBigInt(readResult(results, 27))
    const maxSeniorShareBps = asBigInt(readResult(results, 28))
    const seniorDepositCapacityUsdc = asBigInt(readResult(results, 29))
    const reservedSeniorDepositAssetsUsdc = asBigInt(readResult(results, 30))
    const seniorReservationsWithinLimits = asBoolean(readResult(results, 31))
    const minTrancheDepositUsdc = asBigInt(readResult(results, 32))
    const totalAssetsUsdc = poolView?.totalAssetsUsdc
    const freeUsdc = poolView?.freeUsdc
    const withdrawalReservedUsdc = poolView?.withdrawalReservedUsdc
    const pendingRecapitalizationUsdc = poolView?.pendingRecapitalizationUsdc
    const pendingTradingRevenueUsdc = poolView?.pendingTradingRevenueUsdc
    const seniorPrincipalUsdc = poolView?.seniorPrincipalUsdc
    const juniorPrincipalUsdc = poolView?.juniorPrincipalUsdc
    const seniorHighWaterMarkUsdc = poolView?.seniorHighWaterMarkUsdc
    const currentTerminalDeficitUsdc = poolView?.currentTerminalDeficitUsdc
    const markFresh = poolView?.markFresh
    const oracleFrozen = poolView?.oracleFrozen
    const degradedMode = poolView?.degradedMode
    const seniorAssets = seniorTranche?.totalAssetsUsdc ?? directSeniorAssets
    const seniorSupply = seniorTranche?.totalShares ?? directSeniorSupply
    const juniorAssets = juniorTranche?.totalAssetsUsdc ?? directJuniorAssets
    const juniorSupply = juniorTranche?.totalShares ?? directJuniorSupply
    const markPrice = protocolStatus?.lastMarkPrice
    const inferredCurrentEpoch = BigInt(Math.floor(Date.now() / 3_600_000))
    const seniorCurrentEpoch = seniorQueue?.currentEpoch ?? inferredCurrentEpoch
    const juniorCurrentEpoch = juniorQueue?.currentEpoch ?? inferredCurrentEpoch
    const seniorNextRequestEpoch = seniorQueue?.nextRequestEpoch
      ?? asBigInt(tupleValue(seniorRequestWindow, 0, 'nextRequestEpoch'))
    const juniorNextRequestEpoch = juniorQueue?.nextRequestEpoch
      ?? asBigInt(tupleValue(juniorRequestWindow, 0, 'nextRequestEpoch'))
    const seniorNextRequestCutoffTime = seniorQueue?.nextRequestCutoffTime
      ?? asBigInt(tupleValue(seniorRequestWindow, 1, 'nextRequestCutoffTime'))
    const juniorNextRequestCutoffTime = juniorQueue?.nextRequestCutoffTime
      ?? asBigInt(tupleValue(juniorRequestWindow, 1, 'nextRequestCutoffTime'))
    const seniorPoolWithdrawCapUsdc = pendingTrancheState?.maxSeniorWithdrawUsdc
      ?? seniorTranche?.maxWithdrawUsdc
    const juniorPoolWithdrawCapUsdc = pendingTrancheState?.maxJuniorWithdrawUsdc
      ?? juniorTranche?.maxWithdrawUsdc
    const bullOpenInterest = asBigInt(tupleValue(bullSide, 1, 'openInterest'))
    const bearOpenInterest = asBigInt(tupleValue(bearSide, 1, 'openInterest'))
    const maxSkewRatio = asBigInt(tupleValue(riskParams, 1, 'maxSkewRatio'))
    const bullOpenInterestUsdc = openInterestNotionalUsdc(bullOpenInterest, markPrice)
    const bearOpenInterestUsdc = openInterestNotionalUsdc(bearOpenInterest, markPrice)
    const longOpenCapacityUsdc = openCapacityUsdc({
      selectedOpenInterestUsdc: bullOpenInterestUsdc,
      oppositeOpenInterestUsdc: bearOpenInterestUsdc,
      poolAssetsUsdc: totalAssetsUsdc,
      maxSkewRatio,
    })
    const shortOpenCapacityUsdc = openCapacityUsdc({
      selectedOpenInterestUsdc: bearOpenInterestUsdc,
      oppositeOpenInterestUsdc: bullOpenInterestUsdc,
      poolAssetsUsdc: totalAssetsUsdc,
      maxSkewRatio,
    })
    const seniorImpaired = seniorPrincipalUsdc !== undefined
      && seniorHighWaterMarkUsdc !== undefined
      ? seniorPrincipalUsdc < seniorHighWaterMarkUsdc
      : undefined
    const seniorImpairmentGapUsdc = seniorImpaired
      && seniorPrincipalUsdc !== undefined
      && seniorHighWaterMarkUsdc !== undefined
      ? seniorHighWaterMarkUsdc - seniorPrincipalUsdc
      : seniorImpaired === false
        ? 0n
        : undefined
    const hasLivePoolData = [
      totalAssetsUsdc,
      freeUsdc,
      withdrawalReservedUsdc,
      pendingRecapitalizationUsdc,
      pendingTradingRevenueUsdc,
      seniorPrincipalUsdc,
      juniorPrincipalUsdc,
      seniorHighWaterMarkUsdc,
      markFresh,
      oracleFrozen,
      degradedMode,
    ].every((value) => value !== undefined)
    const hasSeniorCoreData = seniorAssets !== undefined
      && seniorSupply !== undefined
      && seniorMaxRequestDeposit !== undefined
      && seniorMaxRequestRedeem !== undefined
    const hasJuniorCoreData = juniorAssets !== undefined
      && juniorSupply !== undefined
      && juniorMaxRequestDeposit !== undefined
      && juniorMaxRequestRedeem !== undefined
    const hasAnyLiveData = results?.some((result) => result.status === 'success') ?? false
    const hasCompleteLiveSnapshot = hasLivePoolData && hasSeniorCoreData && hasJuniorCoreData

    return {
      status: hasCompleteLiveSnapshot
        ? 'live'
        : isLoading
          ? 'syncing'
          : hasAnyLiveData
            ? 'partial'
            : 'unavailable',
      pool: {
        totalAssetsUsdc,
        freeUsdc,
        withdrawalReservedUsdc,
        pendingRecapitalizationUsdc,
        pendingTradingRevenueUsdc,
        seniorPrincipalUsdc,
        juniorPrincipalUsdc,
        seniorHighWaterMarkUsdc,
        currentTerminalDeficitUsdc,
        markFresh,
        oracleFrozen,
        degradedMode,
        seniorImpaired,
        seniorImpairmentGapUsdc,
        seniorPoolWithdrawCapUsdc,
        juniorPoolWithdrawCapUsdc,
        maxSeniorExposureUsdc,
        maxSeniorShareBps,
        seniorDepositCapacityUsdc,
        reservedSeniorDepositAssetsUsdc,
        seniorReservationsWithinLimits,
        minTrancheDepositUsdc,
        markPrice,
        longOpenCapacityUsdc,
        shortOpenCapacityUsdc,
      },
      walletUsdc,
      hasLivePoolData,
      tranches: {
        senior: {
          totalAssets: seniorAssets,
          totalSupply: seniorSupply,
          effectiveTotalSupply: seniorTranche?.effectiveTotalShares,
          pendingMaintenanceFeeShares: seniorTranche?.pendingMaintenanceFeeShares,
          maintenanceFeeAprBps: seniorTranche?.maintenanceFeeAprBps,
          maintenanceFeeRecipient: seniorTranche?.maintenanceFeeRecipient,
          userShares: seniorUserShares,
          maxRequestDeposit: seniorMaxRequestDeposit,
          maxRequestRedeem: seniorMaxRequestRedeem,
          allowance: seniorAllowance,
          currentEpoch: seniorCurrentEpoch,
          nextRequestEpoch: seniorNextRequestEpoch,
          nextRequestCutoffTime: seniorNextRequestCutoffTime,
          depositBacklog: seniorQueue?.depositBacklog,
          redeemBacklog: seniorQueue?.redeemBacklog,
          settlementLive: seniorQueue?.settlementLive,
          poolPaused: seniorQueue?.poolPaused,
          lpEpochSettlementPaused: seniorQueue?.lpEpochSettlementPaused
            ?? protocolStatus?.lpEpochSettlementPaused,
          frozenLpFeeBps: seniorTranche?.frozenLpFeeBps,
          depositEnabled: seniorTranche?.depositEnabled,
          withdrawEnabled: seniorTranche?.withdrawEnabled,
          poolWithdrawCapUsdc: seniorPoolWithdrawCapUsdc,
          sharePrice: calculateLensSharePrice(seniorTranche?.sharePrice)
            ?? calculateConvertedSharePrice(seniorConvertedAssets)
            ?? calculateSharePrice(seniorAssets, seniorSupply),
          hasCoreData: hasSeniorCoreData,
          hasDepositData: seniorAllowance !== undefined,
          hasUserData: seniorUserShares !== undefined && seniorMaxRequestRedeem !== undefined,
        },
        junior: {
          totalAssets: juniorAssets,
          totalSupply: juniorSupply,
          effectiveTotalSupply: juniorTranche?.effectiveTotalShares,
          pendingMaintenanceFeeShares: juniorTranche?.pendingMaintenanceFeeShares,
          maintenanceFeeAprBps: juniorTranche?.maintenanceFeeAprBps,
          maintenanceFeeRecipient: juniorTranche?.maintenanceFeeRecipient,
          userShares: juniorUserShares,
          maxRequestDeposit: juniorMaxRequestDeposit,
          maxRequestRedeem: juniorMaxRequestRedeem,
          allowance: juniorAllowance,
          currentEpoch: juniorCurrentEpoch,
          nextRequestEpoch: juniorNextRequestEpoch,
          nextRequestCutoffTime: juniorNextRequestCutoffTime,
          depositBacklog: juniorQueue?.depositBacklog,
          redeemBacklog: juniorQueue?.redeemBacklog,
          settlementLive: juniorQueue?.settlementLive,
          poolPaused: juniorQueue?.poolPaused,
          lpEpochSettlementPaused: juniorQueue?.lpEpochSettlementPaused
            ?? protocolStatus?.lpEpochSettlementPaused,
          frozenLpFeeBps: juniorTranche?.frozenLpFeeBps,
          depositEnabled: juniorTranche?.depositEnabled,
          withdrawEnabled: juniorTranche?.withdrawEnabled,
          poolWithdrawCapUsdc: juniorPoolWithdrawCapUsdc,
          sharePrice: calculateLensSharePrice(juniorTranche?.sharePrice)
            ?? calculateConvertedSharePrice(juniorConvertedAssets)
            ?? calculateSharePrice(juniorAssets, juniorSupply),
          hasCoreData: hasJuniorCoreData,
          hasDepositData: juniorAllowance !== undefined,
          hasUserData: juniorUserShares !== undefined && juniorMaxRequestRedeem !== undefined,
        },
      },
      refresh: () => {
        void refetch()
      },
    }
  }, [data, isLoading, refetch])
}

function formatCompactUsdc(amount: bigint | undefined): string {
  if (amount === undefined) return '--'
  const value = Number(formatUnits(amount, USDC_DECIMALS))
  return new Intl.NumberFormat('en-US', {
    notation: 'compact',
    maximumFractionDigits: 2,
  }).format(value)
}

function formatFullUsdc(amount: bigint | undefined, maximumFractionDigits = 2): string {
  if (amount === undefined) return '--'
  const value = Number(formatUnits(amount, USDC_DECIMALS))
  return new Intl.NumberFormat('en-US', {
    minimumFractionDigits: Math.min(2, maximumFractionDigits),
    maximumFractionDigits,
  }).format(value)
}

function formatCompactUsd(amount: bigint | undefined): ReactNode {
  const formatted = formatCompactUsdc(amount)
  return formatted === '--' ? formatted : <TokenAmount amount={formatted} />
}

function formatFullUsd(amount: bigint | undefined, maximumFractionDigits = 2): ReactNode {
  const formatted = formatFullUsdc(amount, maximumFractionDigits)
  return formatted === '--' ? formatted : <TokenAmount amount={formatted} />
}

function formatPoolCapacity(amount: bigint | undefined, markPrice: bigint | undefined): ReactNode {
  if (amount === undefined) return '--'
  const formatted = formatPerpsUsdc(dxyExposureFromContractNotional(amount, markPrice) ?? amount)
  return <TokenAmount amount={formatted} />
}

function formatVaultLimit(amount: bigint | undefined): ReactNode {
  if (amount === undefined) return '--'
  if (amount >= 2n ** 255n) return 'No contract cap'
  return formatFullUsd(amount)
}

function secondsUntilNextVaultEpoch(nowMs = Date.now()): number {
  const nowSeconds = Math.floor(nowMs / 1_000)
  const secondsIntoEpoch = nowSeconds % VAULT_EPOCH_DURATION_SECONDS
  return secondsIntoEpoch === 0
    ? VAULT_EPOCH_DURATION_SECONDS
    : VAULT_EPOCH_DURATION_SECONDS - secondsIntoEpoch
}

function secondsUntilVaultTimestamp(targetTimestamp: bigint | undefined, nowMs = Date.now()): number {
  if (targetTimestamp === undefined) return secondsUntilNextVaultEpoch(nowMs)
  const nowSeconds = Math.floor(nowMs / 1_000)
  let futureTarget = Number(targetTimestamp)
  while (futureTarget <= nowSeconds) futureTarget += VAULT_EPOCH_DURATION_SECONDS
  return Math.max(0, futureTarget - nowSeconds)
}

function formatEpochCountdown(totalSeconds: number): string {
  const minutes = Math.floor(totalSeconds / 60)
  const seconds = totalSeconds % 60
  return `${String(minutes).padStart(2, '0')}:${String(seconds).padStart(2, '0')}`
}

function formatShares(amount: bigint | undefined): string {
  if (amount === undefined) return '--'
  const value = Number(formatUnits(amount, SHARE_DECIMALS))
  return new Intl.NumberFormat('en-US', {
    maximumFractionDigits: 4,
  }).format(value)
}

function formatSharePrice(value: number | undefined): ReactNode {
  if (value === undefined) return '--'
  return <TokenAmount amount={value.toFixed(4)} />
}

interface VaultChartPoint {
  timestamp: number
  blockNumber: string
  sharePrice: number
}

interface CompleteVaultPerformance {
  apy7d: number
  return7d: number
  periodStart: number
  periodEnd: number
  points: VaultChartPoint[]
}

function formatSignedPercent(value: number): string {
  const percent = value * 100
  const rounded = Math.abs(percent) < 0.005 ? 0 : percent
  const sign = rounded > 0 ? '+' : ''
  return `${sign}${rounded.toFixed(2)}%`
}

function formatHistorySharePrice(value: number): ReactNode {
  const formatted = new Intl.NumberFormat('en-US', {
    minimumFractionDigits: 4,
    maximumFractionDigits: 6,
  }).format(value)
  return <TokenAmount amount={formatted} />
}

function performanceTone(value: number): 'default' | 'positive' | 'negative' {
  if (Math.abs(value) < 0.00005) return 'default'
  return value > 0 ? 'positive' : 'negative'
}

function performanceValueClassName(value: number): string {
  const tone = performanceTone(value)
  if (tone === 'positive') return 'text-positive'
  if (tone === 'negative') return 'text-brand-orange'
  return 'text-content-primary'
}

function normalizeHistoryPoints(points: VaultHistoryPoint[]): VaultChartPoint[] {
  const byTimestamp = new Map<number, VaultChartPoint>()

  points.forEach((point) => {
    let sharePrice: number
    try {
      sharePrice = Number(formatUnits(BigInt(point.sharePrice), 18))
    } catch {
      return
    }
    if (!Number.isFinite(point.timestamp) || !Number.isFinite(sharePrice) || sharePrice < 0) return
    byTimestamp.set(point.timestamp, {
      timestamp: point.timestamp,
      blockNumber: point.blockNumber,
      sharePrice,
    })
  })

  return [...byTimestamp.values()].sort((left, right) => left.timestamp - right.timestamp)
}

function historyMatchesConfiguredDeployment(history: VaultHistory): boolean {
  return history.deployment.chainId === PERPS_ARBITRUM_SEPOLIA_CHAIN_ID
    && history.deployment.housePool.toLowerCase() === PERPS_ARBITRUM_SEPOLIA.housePool.toLowerCase()
    && history.deployment.seniorVault.toLowerCase() === PERPS_ARBITRUM_SEPOLIA.seniorVault.toLowerCase()
    && history.deployment.juniorVault.toLowerCase() === PERPS_ARBITRUM_SEPOLIA.juniorVault.toLowerCase()
}

function getCompleteVaultPerformance(
  history: VaultHistory | undefined,
  trancheId: TrancheId
): CompleteVaultPerformance | undefined {
  if (!history
    || !history.coverage.complete
    || !historyMatchesConfiguredDeployment(history)
    || history.coverage.start === null
    || history.coverage.end === null) {
    return undefined
  }

  const tranche: VaultHistoryTranche = history[trancheId]
  if (tranche.apy7d === null || tranche.return7d === null) return undefined
  const points = normalizeHistoryPoints(tranche.points)
  if (points.length < 2 || points[0].sharePrice <= 0) return undefined

  return {
    apy7d: tranche.apy7d,
    return7d: tranche.return7d,
    periodStart: history.coverage.start,
    periodEnd: history.coverage.end,
    points,
  }
}

function parseUsdc(value: string): bigint {
  try {
    return value.trim() ? parseUnits(value, USDC_DECIMALS) : 0n
  } catch {
    return 0n
  }
}

function getDepositMode(liveData: TrancheLiveData): string {
  if (liveData.poolPaused === true) return 'Pool paused'
  if (liveData.depositEnabled === false) return 'Deposits paused'
  if (liveData.maxRequestDeposit === undefined) {
    return 'Availability unavailable'
  }
  if (liveData.maxRequestDeposit > 0n) {
    return 'Queued deposits open'
  }
  return 'At current capacity'
}

function formatAddress(address: Address): string {
  return `${address.slice(0, 6)}...${address.slice(-4)}`
}

function StatLabel({
  children,
  tooltip,
}: {
  children: string
  tooltip?: ReactNode
}) {
  return (
    <dt className="flex items-center gap-1.5 text-xs font-medium uppercase tracking-[0.14em] text-content-secondary">
      <span>{children}</span>
      {tooltip ? (
        <Tooltip content={tooltip} className="max-w-72 whitespace-normal">
          <span
            aria-label={`${children} details`}
            className="inline-flex h-4 w-4 items-center justify-center rounded-full border border-current text-[10px] normal-case tracking-normal text-content-secondary/80"
            tabIndex={0}
          >
            i
          </span>
        </Tooltip>
      ) : null}
    </dt>
  )
}

function PoolStat({
  label,
  value,
  subvalue,
  tooltip,
  valueClassName = 'text-content-primary',
  stackedOnMobile = false,
  startsTabletRow = false,
}: {
  label: string
  value: ReactNode
  subvalue?: ReactNode
  tooltip?: ReactNode
  valueClassName?: string
  stackedOnMobile?: boolean
  startsTabletRow?: boolean
}) {
  const separatorClassName = stackedOnMobile
    ? startsTabletRow
      ? 'sm:border-l-0 sm:pl-0 lg:border-l lg:border-brand-border/25 lg:pl-4'
      : 'sm:border-l sm:border-brand-border/25 sm:pl-4 sm:first:border-l-0 sm:first:pl-0'
    : 'border-l border-brand-border/25 pl-4 first:border-l-0 first:pl-0'

  return (
    <div className={`min-w-0 ${separatorClassName}`}>
      <StatLabel tooltip={tooltip}>{label}</StatLabel>
      <dd className={`mt-2 text-2xl font-semibold ${valueClassName}`}>{value}</dd>
      {subvalue ? <p className="mt-1 text-xs text-content-secondary">{subvalue}</p> : null}
    </div>
  )
}

function VaultEpochCountdown({ targetTimestamp }: { targetTimestamp?: bigint }) {
  const [remainingSeconds, setRemainingSeconds] = useState(() => (
    secondsUntilVaultTimestamp(targetTimestamp)
  ))

  useEffect(() => {
    const interval = window.setInterval(() => {
      setRemainingSeconds(secondsUntilVaultTimestamp(targetTimestamp))
    }, 1_000)

    return () => {
      window.clearInterval(interval)
    }
  }, [targetTimestamp])

  return (
    <time
      dateTime={`PT${String(remainingSeconds)}S`}
      aria-label={`${String(remainingSeconds)} seconds until the vault request cutoff`}
      className="font-mono tabular-nums"
    >
      {formatEpochCountdown(remainingSeconds)}
    </time>
  )
}

function TrancheMark({ tranche, size = 'lg' }: { tranche: TrancheDefinition; size?: 'md' | 'lg' }) {
  const senior = tranche.id === 'senior'

  return (
    <div
      className={`flex shrink-0 items-center justify-center border ${tranche.markClassName} ${
        size === 'lg' ? 'h-14 w-14' : 'h-11 w-11'
      }`}
      aria-hidden="true"
    >
      <svg
        viewBox="0 0 28 28"
        className={size === 'lg' ? 'h-8 w-8' : 'h-7 w-7'}
        fill="none"
        role="presentation"
      >
        <rect
          x="6"
          y="4.5"
          width="16"
          height="7"
          fill={senior ? 'currentColor' : 'none'}
          stroke="currentColor"
          strokeWidth="1.5"
          opacity={senior ? 1 : 0.45}
        />
        <rect
          x="3"
          y="16.5"
          width="22"
          height="7"
          fill={senior ? 'none' : 'currentColor'}
          stroke="currentColor"
          strokeWidth="1.5"
          opacity={senior ? 0.45 : 1}
        />
      </svg>
    </div>
  )
}

function chartYDomain(points: VaultChartPoint[]): { min: number; max: number } {
  const prices = points.map((point) => point.sharePrice)
  const minimum = Math.min(...prices)
  const maximum = Math.max(...prices)
  if (minimum !== maximum) {
    const padding = (maximum - minimum) * 0.12
    return { min: Math.max(0, minimum - padding), max: maximum + padding }
  }

  const padding = Math.max(Math.abs(minimum) * 0.005, 0.000001)
  return { min: Math.max(0, minimum - padding), max: maximum + padding }
}

function chartReturnDomain(points: VaultChartPoint[], startingPrice: number): { min: number; max: number } {
  const returns = points.map((point) => point.sharePrice / startingPrice - 1)
  const minimum = Math.min(0, ...returns)
  const maximum = Math.max(0, ...returns)

  if (minimum === maximum) {
    return { min: -0.00005, max: 0.00005 }
  }

  return {
    min: minimum < 0 ? minimum * 1.12 : 0,
    max: maximum > 0 ? maximum * 1.12 : 0,
  }
}

function chartPointX(timestamp: number, start: number, end: number, left: number, width: number) {
  if (end <= start) return left
  return left + ((timestamp - start) / (end - start)) * width
}

function chartPointY(value: number, min: number, max: number, top: number, height: number) {
  if (max <= min) return top + height / 2
  return top + (1 - (value - min) / (max - min)) * height
}

function MiniPerformanceChart({
  trancheName,
  performance,
}: {
  trancheName: string
  performance: CompleteVaultPerformance
}) {
  const [activeIndex, setActiveIndex] = useState<number | null>(null)
  const width = 600
  const height = 132
  const plot = { left: 8, right: 60, top: 8, bottom: 26 }
  const plotWidth = width - plot.left - plot.right
  const plotHeight = height - plot.top - plot.bottom
  const domainStart = performance.periodEnd - SEVEN_DAYS_SECONDS
  const startingPrice = performance.points[0].sharePrice
  const domain = chartReturnDomain(performance.points, startingPrice)
  const coordinates = performance.points.map((point) => ({
    point,
    x: chartPointX(point.timestamp, domainStart, performance.periodEnd, plot.left, plotWidth),
    y: chartPointY(point.sharePrice / startingPrice - 1, domain.min, domain.max, plot.top, plotHeight),
  }))
  const path = coordinates.map(({ x, y }, index) => (
    `${index === 0 ? 'M' : 'L'} ${x.toFixed(2)} ${y.toFixed(2)}`
  )).join(' ')
  const last = coordinates[coordinates.length - 1]
  const active = activeIndex === null ? undefined : coordinates[activeIndex]
  const yTicks = [domain.max, (domain.min + domain.max) / 2, domain.min]
  const xTicks = [domainStart, domainStart + SEVEN_DAYS_SECONDS / 2, performance.periodEnd]

  function selectNearestPoint(clientX: number, element: SVGSVGElement) {
    const bounds = element.getBoundingClientRect()
    const viewBoxX = bounds.width > 0
      ? ((clientX - bounds.left) / bounds.width) * width
      : clientX
    let nearestIndex = 0
    let nearestDistance = Number.POSITIVE_INFINITY
    coordinates.forEach(({ x }, index) => {
      const distance = Math.abs(x - viewBoxX)
      if (distance < nearestDistance) {
        nearestDistance = distance
        nearestIndex = index
      }
    })
    setActiveIndex(nearestIndex)
  }

  return (
    <div className="relative h-[8.25rem] overflow-hidden">
      <svg
        viewBox="0 0 600 132"
        className="h-[8.25rem] w-full"
        preserveAspectRatio="none"
        aria-label={`${trancheName} seven-day share price chart`}
        role="img"
        onPointerMove={(event) => {
          selectNearestPoint(event.clientX, event.currentTarget)
        }}
        onPointerLeave={() => {
          setActiveIndex(null)
        }}
      >
        <title>{trancheName} USDC share price over the last seven days</title>
        {yTicks.map((tick) => {
          const y = chartPointY(tick, domain.min, domain.max, plot.top, plotHeight)
          return (
            <g key={tick}>
              <line
                x1={plot.left}
                y1={y}
                x2={width - plot.right}
                y2={y}
                stroke="rgba(255,171,150,0.12)"
                strokeWidth="1"
              />
              <text
                data-vault-chart-y-tick
                x={width - plot.right + 7}
                y={y + 3.5}
                fill="rgba(244,235,239,0.62)"
                fontSize="9.5"
                textAnchor="start"
              >
                {formatSignedPercent(tick)}
              </text>
            </g>
          )
        })}
        <line
          data-vault-chart-axis="y"
          x1={width - plot.right}
          y1={plot.top}
          x2={width - plot.right}
          y2={plot.top + plotHeight}
          stroke="rgba(255,171,150,0.32)"
          strokeWidth="1"
        />
        <line
          data-vault-chart-axis="x"
          x1={plot.left}
          y1={plot.top + plotHeight}
          x2={width - plot.right}
          y2={plot.top + plotHeight}
          stroke="rgba(255,171,150,0.32)"
          strokeWidth="1"
        />
        {xTicks.map((tick, index) => (
          <text
            key={tick}
            x={chartPointX(tick, domainStart, performance.periodEnd, plot.left, plotWidth)}
            y={height - 7}
            fill="rgba(244,235,239,0.62)"
            fontSize="9.5"
            textAnchor={index === 0 ? 'start' : index === xTicks.length - 1 ? 'end' : 'middle'}
          >
            {new Date(tick * 1_000).toLocaleDateString('en-US', { month: 'short', day: 'numeric' })}
          </text>
        ))}
        <path
          data-vault-performance-series
          d={path}
          fill="none"
          stroke={VAULT_PERFORMANCE_CHART_COLOR}
          strokeLinecap="round"
          strokeLinejoin="round"
          strokeWidth="2.5"
          vectorEffect="non-scaling-stroke"
        />
        <circle cx={last.x} cy={last.y} r="3" fill={VAULT_PERFORMANCE_CHART_COLOR} />
        {active ? (
          <g aria-hidden="true">
            <line
              x1={active.x}
              y1={plot.top}
              x2={active.x}
              y2={plot.top + plotHeight}
              stroke={VAULT_PERFORMANCE_CHART_COLOR}
              strokeDasharray="3 4"
              strokeOpacity="0.65"
            />
            <circle
              cx={active.x}
              cy={active.y}
              r="4"
              fill={VAULT_PERFORMANCE_CHART_COLOR}
              stroke="#2A0613"
              strokeWidth="2"
            />
          </g>
        ) : null}
      </svg>
      {active ? (
        <div
          data-vault-chart-tooltip
          className="pointer-events-none absolute top-1 z-10 min-w-36 -translate-x-1/2 border border-brand-border/40 bg-app-bg px-2.5 py-2 shadow-xl"
          style={{
            left: `${String((Math.min(width - 76, Math.max(76, active.x)) / width) * 100)}%`,
          }}
          role="status"
        >
          <p className="text-[9px] font-semibold uppercase tracking-[0.1em] text-content-secondary">
            {formatChartTimestamp(active.point.timestamp)}
          </p>
          <p className="mt-1 text-xs font-semibold text-content-primary">
            {formatHistorySharePrice(active.point.sharePrice)}
          </p>
        </div>
      ) : null}
    </div>
  )
}

function TrancheCard({
  tranche,
  liveData,
  performance,
}: {
  tranche: TrancheDefinition
  liveData: TrancheLiveData
  performance?: CompleteVaultPerformance
}) {
  return (
    <Link
      to={`/vaults/${tranche.id}`}
      aria-label={`View ${tranche.name}`}
      className="group block h-full border border-brand-border/30 bg-surface-panel transition-colors hover:border-brand-peach/70 focus-visible:border-brand-peach focus-visible:outline-none"
    >
      <article className="flex h-full flex-col">
        <div className="flex items-start gap-4 border-b border-brand-border/25 p-5">
          <div className="flex min-w-0 items-start gap-3">
            <TrancheMark tranche={tranche} size="md" />
            <div className="min-w-0">
              <p className="text-xs font-semibold uppercase tracking-[0.16em] text-content-secondary">
                {tranche.eyebrow}
              </p>
              <h2 className="mt-1 text-2xl font-semibold text-content-primary">{tranche.name}</h2>
            </div>
          </div>
        </div>

        <div className="flex-1 space-y-5 p-5">
          <p className="min-h-12 text-sm leading-6 text-content-secondary">
            {tranche.shortDescription}
          </p>

          <dl className={`grid gap-3 ${performance ? 'grid-cols-3' : 'grid-cols-2'}`}>
            <div>
              <dt
                className="text-xs font-medium uppercase tracking-[0.14em] text-content-secondary"
                title="Current USDC accounting value attributed to this tranche. It need not sum to physical HousePool assets."
              >
                TVL / NAV
              </dt>
              <dd className="mt-2 text-xl font-semibold text-content-primary">
                {formatCompactUsd(liveData.totalAssets)}
              </dd>
            </div>
            {performance ? (
              <div>
                <dt
                  className="text-xs font-medium uppercase tracking-[0.14em] text-content-secondary"
                  title="Annualized return calculated from the last seven days of vault share-price history."
                >
                  7d APY
                </dt>
                <dd className={`mt-2 text-xl font-semibold ${performanceValueClassName(performance.apy7d)}`}>
                  {formatSignedPercent(performance.apy7d)}
                </dd>
              </div>
            ) : null}
            <div>
              <dt
                className="text-xs font-medium uppercase tracking-[0.14em] text-content-secondary"
                title="Current USDC accounting value per active vault share."
              >
                Share price
              </dt>
              <dd className="mt-2 text-xl font-semibold text-content-primary">
                {formatSharePrice(liveData.sharePrice)}
              </dd>
            </div>
          </dl>

          {performance ? (
            <div className="border-y border-brand-border/20 py-2">
              <MiniPerformanceChart trancheName={tranche.name} performance={performance} />
              <span className="sr-only">
                {tranche.name} seven-day share price changed {formatSignedPercent(performance.return7d)},
                with a realized APY of {formatSignedPercent(performance.apy7d)}.
              </span>
            </div>
          ) : null}

          <dl className="grid grid-cols-[6.5rem_minmax(0,1fr)] gap-x-4 gap-y-3">
            {tranche.featureItems.map((item) => (
              <div key={item.label} className="contents">
                <dt className="pt-0.5 text-[10px] font-semibold uppercase tracking-[0.14em] text-content-secondary">
                  {item.label}
                </dt>
                <dd className="text-sm leading-5 text-content-secondary">{item.text}</dd>
              </div>
            ))}
          </dl>
        </div>

        <div className="flex items-center justify-between border-t border-brand-border/25 px-5 py-4 text-sm font-semibold text-content-primary transition-colors group-hover:text-brand-peach group-focus-visible:text-brand-peach">
          <span className="group-hover:underline group-hover:underline-offset-4 group-focus-visible:underline group-focus-visible:underline-offset-4">
            Explore {tranche.name}
          </span>
          <span className="material-symbols-outlined transition-transform group-hover:translate-x-1">
            arrow_forward
          </span>
        </div>
      </article>
    </Link>
  )
}

function VaultsOverview({
  snapshot,
  history,
}: {
  snapshot: VaultsSnapshot
  history?: VaultHistory
}) {
  const pool = snapshot.pool
  const seniorPerformance = getCompleteVaultPerformance(history, 'senior')
  const juniorPerformance = getCompleteVaultPerformance(history, 'junior')
  const poolCapital = calculatePerpsPoolCapital({
    juniorPrincipalUsdc: pool.juniorPrincipalUsdc,
    seniorPrincipalUsdc: pool.seniorPrincipalUsdc,
    seniorHighWaterMarkUsdc: pool.seniorHighWaterMarkUsdc,
  })
  const freeLiquidityRatio = pool.totalAssetsUsdc !== undefined
    && pool.totalAssetsUsdc > 0n
    && pool.freeUsdc !== undefined
    ? (Number(pool.freeUsdc) / Number(pool.totalAssetsUsdc)) * 100
    : undefined
  return (
    <div className="space-y-8">
      <section className="border border-brand-border/30 bg-surface-panel">
        <div className="flex flex-col gap-6 border-b border-brand-border/25 p-6 lg:flex-row lg:items-end lg:justify-between">
          <div className="max-w-3xl">
            <p className="text-xs font-semibold uppercase tracking-[0.2em] text-brand-peach">
              Plether HousePool
            </p>
            <h1 className="mt-3 text-3xl font-semibold tracking-tight text-content-primary sm:text-4xl">
              Supply the balance sheet behind the market.
            </h1>
            <p className="mt-3 max-w-2xl text-base leading-7 text-content-secondary">
              Deposit USDC into Senior or Junior vault shares. Both tranches underwrite the same
              HousePool, but they take different positions in the loss, revenue, and withdrawal
              waterfall.
            </p>
          </div>

          <a
            href="https://docs.plether.com/get-started/liquidity-provider-quickstart"
            target="_blank"
            rel="noreferrer"
            className="group inline-flex self-start items-center gap-2 border border-brand-border/40 px-4 py-2 text-sm font-semibold text-content-primary transition-colors hover:border-brand-peach hover:text-brand-peach"
          >
            <span className="group-hover:underline group-hover:underline-offset-4">
              Read the LP guide
            </span>
            <span className="material-symbols-outlined text-lg">open_in_new</span>
          </a>
        </div>

        <dl className="grid grid-cols-1 gap-x-4 gap-y-6 p-6 sm:grid-cols-2 lg:grid-cols-4">
          <PoolStat
            label="HousePool assets"
            value={formatCompactUsd(pool.totalAssetsUsdc)}
            tooltip="Canonical physical HousePool assets. This can differ from the sum of tranche accounting NAV."
            stackedOnMobile
          />
          <PoolStat
            label="Withdrawal reserve"
            value={formatCompactUsd(pool.withdrawalReservedUsdc)}
            subvalue="Trader liabilities protected first"
            tooltip="Capital reserved for bounded trader liability, claims, and other protected amounts."
            stackedOnMobile
          />
          <PoolStat
            label="Free liquidity"
            value={formatCompactUsd(pool.freeUsdc)}
            subvalue={freeLiquidityRatio === undefined ? 'Live value unavailable' : `${freeLiquidityRatio.toFixed(1)}% of total assets`}
            tooltip="Physical USDC remaining after protected withdrawal reserves. This is not the same as total tranche NAV."
            stackedOnMobile
            startsTabletRow
          />
          <PoolStat
            label="Request cutoff"
            value={(
              <VaultEpochCountdown
                targetTimestamp={snapshot.tranches.senior.nextRequestCutoffTime}
              />
            )}
            subvalue="Requests after this timer join the following hourly batch"
            tooltip="The shared deposit and withdrawal queue closes five minutes before each hourly settlement boundary."
            stackedOnMobile
          />
        </dl>
      </section>

      {snapshot.tranches.senior.lpEpochSettlementPaused === true
        || snapshot.tranches.junior.lpEpochSettlementPaused === true ? (
          <Alert variant="warning" title="Epoch settlement paused">
            New requests, already-funded claims, eligible cancellations, and refunds remain
            available. Deposit requests will not activate and redemption requests will not receive
            new funding until governance resumes hourly settlement.
          </Alert>
        ) : null}

      <section>
        <div className="mb-4">
          <p className="text-xs font-semibold uppercase tracking-[0.18em] text-content-secondary">
            Choose a tranche
          </p>
          <h2 className="mt-1 text-2xl font-semibold text-content-primary">USDC vaults</h2>
        </div>

        <div className="grid gap-6 lg:grid-cols-2">
          <TrancheCard
            tranche={TRANCHES.senior}
            liveData={snapshot.tranches.senior}
            performance={seniorPerformance}
          />
          <TrancheCard
            tranche={TRANCHES.junior}
            liveData={snapshot.tranches.junior}
            performance={juniorPerformance}
          />
        </div>
      </section>

      <section
        aria-labelledby="pool-liquidity-heading"
      >
        <div className="mb-4">
          <p className="text-xs font-semibold uppercase tracking-[0.18em] text-content-secondary">
            HousePool liquidity
          </p>
          <h2
            id="pool-liquidity-heading"
            className="mt-1 text-2xl font-semibold text-content-primary"
          >
            Capacity and capital waterfall
          </h2>
        </div>
        <div className="border border-brand-border/30 bg-surface-panel p-5">
          <PerpsPoolLiquidityDetails
            longCapacity={formatPoolCapacity(pool.longOpenCapacityUsdc, pool.markPrice)}
            shortCapacity={formatPoolCapacity(pool.shortOpenCapacityUsdc, pool.markPrice)}
            juniorPrincipal={formatCompactUsd(pool.juniorPrincipalUsdc)}
            seniorPrincipal={formatCompactUsd(pool.seniorPrincipalUsdc)}
            juniorSharePercent={poolCapital?.juniorSharePercent}
            seniorSharePercent={poolCapital?.seniorSharePercent}
            seniorStatus={poolCapital?.seniorStatus}
            seniorImpairment={formatCompactUsd(poolCapital?.seniorImpairmentUsdc)}
            isJuniorExhausted={poolCapital?.isJuniorExhausted}
            isEmpty={poolCapital?.isEmpty}
            isLoading={snapshot.status === 'syncing'}
            docsLink={DOCS_LINKS.poolLiquidity}
          />
        </div>
      </section>

    </div>
  )
}

function DetailMetric({
  label,
  value,
  detail,
  tone = 'default',
}: {
  label: string
  value: ReactNode
  detail?: ReactNode
  tone?: 'default' | 'positive' | 'warning' | 'negative'
}) {
  const toneClass = tone === 'positive'
    ? 'text-positive'
    : tone === 'warning'
      ? 'text-warning'
      : tone === 'negative'
        ? 'text-brand-orange'
        : 'text-content-primary'

  return (
    <div className="border border-brand-border/25 bg-app-bg p-4">
      <p className="text-xs font-medium uppercase tracking-[0.12em] text-content-secondary">{label}</p>
      <p className={`mt-2 text-xl font-semibold ${toneClass}`}>{value}</p>
      {detail ? <p className="mt-1 text-xs leading-5 text-content-secondary">{detail}</p> : null}
    </div>
  )
}

function DetailRow({
  label,
  value,
  valueClassName = 'text-content-primary',
}: {
  label: ReactNode
  value: ReactNode
  valueClassName?: string
}) {
  return (
    <div className="grid grid-cols-[minmax(0,1fr)_auto] items-start gap-4 border-b border-brand-border/20 py-3 last:border-b-0">
      <dt className="text-sm leading-5 text-content-secondary">{label}</dt>
      <dd className={`max-w-64 text-right text-sm font-semibold leading-5 ${valueClassName}`}>{value}</dd>
    </div>
  )
}

function OverviewTab({
  tranche,
  liveData,
  snapshot,
  isConnected,
}: {
  tranche: TrancheDefinition
  liveData: TrancheLiveData
  snapshot: VaultsSnapshot
  isConnected: boolean
}) {
  const pool = snapshot.pool
  const positionValue = liveData.userShares !== undefined && liveData.sharePrice !== undefined
    ? Number(formatUnits(liveData.userShares, SHARE_DECIMALS)) * liveData.sharePrice
    : undefined
  const poolWithdrawCap = tranche.id === 'senior'
    ? pool.seniorPoolWithdrawCapUsdc
    : pool.juniorPoolWithdrawCapUsdc
  const depositMode = getDepositMode(liveData)
  const depositState = pool.currentTerminalDeficitUsdc !== undefined
    && pool.currentTerminalDeficitUsdc > 0n
    ? 'Terminal deficit'
    : depositMode

  return (
    <div className="space-y-6">
      <div className="grid gap-3 sm:grid-cols-2 xl:grid-cols-4">
        <DetailMetric
          label="Your position"
          value={isConnected && positionValue !== undefined
            ? <TokenAmount amount={positionValue.toLocaleString('en-US', { maximumFractionDigits: 2 })} />
            : '--'}
          detail={isConnected ? `${formatShares(liveData.userShares)} ${tranche.token}` : 'Connect a wallet to view'}
        />
        <DetailMetric
          label="Requestable shares"
          value={isConnected
            ? `${formatShares(liveData.maxRequestRedeem)} ${tranche.token}`
            : '--'}
          detail="Shares that can enter the withdrawal queue now"
          tone={(liveData.maxRequestRedeem ?? 0n) > 0n ? 'positive' : 'default'}
        />
        <DetailMetric
          label="Pool funding capacity"
          value={formatCompactUsd(poolWithdrawCap)}
          detail={tranche.id === 'senior'
            ? 'Canonical Senior capacity for the next settlement'
            : 'Canonical Junior capacity after Senior priority'}
        />
        <DetailMetric
          label="Deposit availability"
          value={depositState}
          detail={depositMode === 'Queued deposits open'
            ? (
              <span>
                Current request window closes in{' '}
                <VaultEpochCountdown targetTimestamp={liveData.nextRequestCutoffTime} />
              </span>
            )
            : 'The contract is not accepting new deposit requests'}
          tone={depositState === 'Terminal deficit' ? 'negative' : 'warning'}
        />
      </div>

      <div className="grid gap-6 xl:grid-cols-2">
        <section className="border border-brand-border/30 bg-surface-panel p-5">
          <h3 className="text-lg font-semibold text-content-primary">Vault configuration</h3>
          <dl className="mt-3">
            <DetailRow label="Asset" value="USDC" />
            <DetailRow label="Vault share" value={tranche.token} />
            <DetailRow label="Vault standard" value="ERC-4626 shares + async epoch queue" />
            <DetailRow label="Network" value="Arbitrum Sepolia" />
            <DetailRow label="Deposit path" value={depositMode} />
            <DetailRow label="Settlement cadence" value="Hourly shared batch" />
            <DetailRow label="Request cutoff" value="5 minutes before each hour" />
            <DetailRow
              label="Current target batch"
              value={liveData.nextRequestEpoch === undefined
                ? 'Unavailable'
                : new Date(Number(liveData.nextRequestEpoch * 3_600n) * 1_000).toLocaleString()}
            />
            <DetailRow
              label="Request fee"
              value={liveData.frozenLpFeeBps === undefined
                ? 'Unavailable'
                : `${(Number(liveData.frozenLpFeeBps) / 100).toFixed(2)}%`}
            />
            {tranche.id === 'junior' ? (
              <>
                <DetailRow
                  label="Maintenance fee APR"
                  value={liveData.maintenanceFeeAprBps === undefined
                    ? 'Unavailable'
                    : `${(Number(liveData.maintenanceFeeAprBps) / 100).toFixed(2)}%`}
                />
                <DetailRow
                  label="Pending maintenance-fee shares"
                  value={liveData.pendingMaintenanceFeeShares === undefined
                    ? 'Unavailable'
                    : `${formatShares(liveData.pendingMaintenanceFeeShares)} ${tranche.token}`}
                />
                <DetailRow
                  label="Maintenance-fee recipient"
                  value={liveData.maintenanceFeeRecipient === undefined
                    ? 'Unavailable'
                    : (
                      <a
                        href={`${EXPLORER_BASE_URL}/${liveData.maintenanceFeeRecipient}`}
                        target="_blank"
                        rel="noreferrer"
                        className="group inline-flex items-center gap-1 text-brand-peach"
                      >
                        <span className="group-hover:underline">
                          {formatAddress(liveData.maintenanceFeeRecipient)}
                        </span>
                        <span className="material-symbols-outlined text-sm">open_in_new</span>
                      </a>
                    )}
                />
              </>
            ) : null}
            {tranche.id === 'senior' ? (
              <>
                <DetailRow
                  label="Senior deposit capacity"
                  value={formatVaultLimit(pool.seniorDepositCapacityUsdc)}
                />
                <DetailRow
                  label="Absolute Senior cap"
                  value={formatVaultLimit(pool.maxSeniorExposureUsdc)}
                />
                <DetailRow
                  label="Maximum Senior share"
                  value={pool.maxSeniorShareBps === undefined
                    ? 'Unavailable'
                    : `${(Number(pool.maxSeniorShareBps) / 100).toFixed(2)}%`}
                />
                <DetailRow
                  label="Reserved queued Senior deposits"
                  value={formatFullUsd(pool.reservedSeniorDepositAssetsUsdc)}
                />
                <DetailRow
                  label="Queued deposits within limits"
                  value={pool.seniorReservationsWithinLimits === undefined
                    ? 'Unavailable'
                    : pool.seniorReservationsWithinLimits ? 'Yes' : 'No'}
                  valueClassName={pool.seniorReservationsWithinLimits === undefined
                    ? 'text-content-secondary'
                    : pool.seniorReservationsWithinLimits ? 'text-positive' : 'text-brand-orange'}
                />
              </>
            ) : null}
            <DetailRow
              label="Deposit queue backlog"
              value={liveData.depositBacklog === undefined
                ? 'Unavailable'
                : liveData.depositBacklog ? 'Yes' : 'No'}
              valueClassName={liveData.depositBacklog === undefined
                ? 'text-content-secondary'
                : liveData.depositBacklog ? 'text-warning' : 'text-positive'}
            />
            <DetailRow
              label="Withdrawal queue backlog"
              value={liveData.redeemBacklog === undefined
                ? 'Unavailable'
                : liveData.redeemBacklog ? 'Yes' : 'No'}
              valueClassName={liveData.redeemBacklog === undefined
                ? 'text-content-secondary'
                : liveData.redeemBacklog ? 'text-warning' : 'text-positive'}
            />
            <DetailRow
              label="Vault contract"
              value={(
                <a
                  href={`${EXPLORER_BASE_URL}/${tranche.address}`}
                  target="_blank"
                  rel="noreferrer"
                  className="group inline-flex items-center gap-1 text-brand-peach"
                >
                  <span className="group-hover:underline">{formatAddress(tranche.address)}</span>
                  <span className="material-symbols-outlined text-sm">open_in_new</span>
                </a>
              )}
            />
          </dl>
        </section>

        <section className="border border-brand-border/30 bg-surface-panel p-5">
          <h3 className="text-lg font-semibold text-content-primary">Live HousePool state</h3>
          <dl className="mt-3">
            <DetailRow label="Total HousePool assets" value={formatFullUsd(pool.totalAssetsUsdc, 0)} />
            <DetailRow label="Free LP liquidity" value={formatFullUsd(pool.freeUsdc, 0)} />
            <DetailRow label="Protected withdrawal reserve" value={formatFullUsd(pool.withdrawalReservedUsdc, 0)} />
            <DetailRow label="Pending trading revenue" value={formatFullUsd(pool.pendingTradingRevenueUsdc)} />
            <DetailRow label="Pending recapitalization" value={formatFullUsd(pool.pendingRecapitalizationUsdc)} />
            <DetailRow
              label="Terminal deficit"
              value={formatFullUsd(pool.currentTerminalDeficitUsdc)}
              valueClassName={(pool.currentTerminalDeficitUsdc ?? 0n) > 0n
                ? 'text-brand-orange'
                : 'text-positive'}
            />
            <DetailRow
              label="Oracle mark"
              value={pool.markFresh === undefined ? 'Unavailable' : pool.markFresh ? 'Fresh' : 'Stale'}
              valueClassName={pool.markFresh === undefined ? 'text-content-secondary' : pool.markFresh ? 'text-positive' : 'text-brand-orange'}
            />
            <DetailRow
              label="Oracle frozen"
              value={pool.oracleFrozen === undefined ? 'Unavailable' : pool.oracleFrozen ? 'Yes' : 'No'}
              valueClassName={pool.oracleFrozen === undefined ? 'text-content-secondary' : pool.oracleFrozen ? 'text-warning' : 'text-positive'}
            />
            <DetailRow
              label="Protocol mode"
              value={pool.degradedMode === undefined ? 'Unavailable' : pool.degradedMode ? 'Degraded' : 'Normal'}
              valueClassName={pool.degradedMode === undefined ? 'text-content-secondary' : pool.degradedMode ? 'text-brand-orange' : 'text-positive'}
            />
            <DetailRow
              label="Pool paused"
              value={liveData.poolPaused === undefined ? 'Unavailable' : liveData.poolPaused ? 'Yes' : 'No'}
              valueClassName={liveData.poolPaused === undefined
                ? 'text-content-secondary'
                : liveData.poolPaused ? 'text-warning' : 'text-positive'}
            />
            <DetailRow
              label="Epoch settlement paused"
              value={liveData.lpEpochSettlementPaused === undefined
                ? 'Unavailable'
                : liveData.lpEpochSettlementPaused ? 'Yes' : 'No'}
              valueClassName={liveData.lpEpochSettlementPaused === undefined
                ? 'text-content-secondary'
                : liveData.lpEpochSettlementPaused ? 'text-warning' : 'text-positive'}
            />
            <DetailRow
              label="Withdrawal funding"
              value={liveData.settlementLive === undefined
                ? 'Unavailable'
                : liveData.settlementLive ? 'Live' : 'Deferred'}
              valueClassName={liveData.settlementLive === undefined
                ? 'text-content-secondary'
                : liveData.settlementLive ? 'text-positive' : 'text-warning'}
            />
          </dl>
        </section>
      </div>

      {tranche.id === 'junior' ? (
        <Alert variant="info" title="How the Junior maintenance fee works">
          The fee is paid by minting shares to the configured recipient, which dilutes existing
          Junior shares. Pending dilution is already included in the effective supply used for the
          displayed share price and in realized APY.
        </Alert>
      ) : null}

      {tranche.id === 'senior' ? (
        <section className="border border-brand-border/30 bg-surface-panel p-5">
          <div className="flex flex-col gap-4 sm:flex-row sm:items-start sm:justify-between">
            <div>
              <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
                Senior protection account
              </p>
              <h3 className="mt-1 text-lg font-semibold text-content-primary">High-water mark</h3>
              <p className="mt-2 max-w-2xl text-sm leading-6 text-content-secondary">
                Paid coupons and restored principal increase the protected Senior claim. If Senior
                falls below this mark, future pool revenue restores the gap before Junior receives
                residual upside.
              </p>
            </div>
            <Badge variant={pool.seniorImpaired === undefined ? 'default' : pool.seniorImpaired ? 'danger' : 'success'}>
              {pool.seniorImpaired === undefined
                ? 'Impairment status unavailable'
                : pool.seniorImpaired
                  ? 'Senior impaired'
                  : 'Senior not impaired'}
            </Badge>
          </div>
          <div className="mt-5 grid gap-3 sm:grid-cols-3">
            <DetailMetric label="Senior principal" value={formatCompactUsd(pool.seniorPrincipalUsdc)} />
            <DetailMetric label="High-water mark" value={formatCompactUsd(pool.seniorHighWaterMarkUsdc)} />
            <DetailMetric
              label="Impairment gap"
              value={formatCompactUsd(pool.seniorImpairmentGapUsdc)}
              tone={pool.seniorImpaired === true ? 'negative' : pool.seniorImpaired === false ? 'positive' : 'default'}
            />
          </div>
        </section>
      ) : (
        <section className="border border-brand-border/30 bg-surface-panel p-5">
          <div className="flex flex-col gap-4 sm:flex-row sm:items-start sm:justify-between">
            <div>
              <p className="text-xs font-semibold uppercase tracking-[0.14em] text-positive">
                Junior protection account
              </p>
              <h3 className="mt-1 text-lg font-semibold text-content-primary">First-loss buffer</h3>
              <p className="mt-2 max-w-2xl text-sm leading-6 text-content-secondary">
                Junior principal is the buffer protecting Senior. It funds the Senior target
                coupon and absorbs realized losses before Senior principal is reduced.
              </p>
            </div>
            <Badge variant="warning">Subordinated capital</Badge>
          </div>
          <div className="mt-5 grid gap-3 sm:grid-cols-3">
            <DetailMetric label="Junior principal" value={formatCompactUsd(pool.juniorPrincipalUsdc)} />
            <DetailMetric label="Senior claim ahead" value={formatCompactUsd(pool.seniorPrincipalUsdc)} />
            <DetailMetric
              label="Pool max withdraw"
              value={formatCompactUsd(pool.juniorPoolWithdrawCapUsdc)}
              tone={(pool.juniorPoolWithdrawCapUsdc ?? 0n) > 0n ? 'positive' : 'warning'}
            />
          </div>
        </section>
      )}

      <section className="grid gap-px border border-brand-border/30 bg-brand-border/20 md:grid-cols-2">
        <div className="bg-surface-panel p-5">
          <p className="text-xs font-semibold uppercase tracking-[0.14em] text-content-secondary">
            Return position
          </p>
          <h3 className={`mt-2 text-xl font-semibold ${tranche.valueClassName}`}>
            {tranche.returnModel}
          </h3>
          <p className="mt-2 text-sm leading-6 text-content-secondary">{tranche.upside}</p>
        </div>
        <div className="bg-surface-panel p-5">
          <p className="text-xs font-semibold uppercase tracking-[0.14em] text-content-secondary">
            Risk position
          </p>
          <h3 className="mt-2 text-xl font-semibold text-content-primary">{tranche.lossPriority}</h3>
          <p className="mt-2 text-sm leading-6 text-content-secondary">{tranche.primaryRisk}</p>
        </div>
      </section>
    </div>
  )
}

function formatChartTimestamp(timestamp: number): string {
  return new Date(timestamp * 1_000).toLocaleString('en-US', {
    month: 'short',
    day: 'numeric',
    hour: '2-digit',
    minute: '2-digit',
  })
}

function PerformanceChart({
  tranche,
  performance,
}: {
  tranche: TrancheDefinition
  performance: CompleteVaultPerformance
}) {
  const [activeIndex, setActiveIndex] = useState<number | null>(null)
  const width = 640
  const height = 240
  const plot = { left: 62, right: 18, top: 18, bottom: 38 }
  const plotWidth = width - plot.left - plot.right
  const plotHeight = height - plot.top - plot.bottom
  const domainStart = performance.periodEnd - SEVEN_DAYS_SECONDS
  const domain = chartYDomain(performance.points)
  const coordinates = performance.points.map((point) => ({
    point,
    x: chartPointX(point.timestamp, domainStart, performance.periodEnd, plot.left, plotWidth),
    y: chartPointY(point.sharePrice, domain.min, domain.max, plot.top, plotHeight),
  }))
  const path = coordinates.map(({ x, y }, index) => (
    `${index === 0 ? 'M' : 'L'} ${x.toFixed(2)} ${y.toFixed(2)}`
  )).join(' ')
  const active = activeIndex === null ? undefined : coordinates[activeIndex]
  const startingPrice = performance.points[0].sharePrice
  const activeReturn = active ? active.point.sharePrice / startingPrice - 1 : undefined
  const yTicks = [domain.max, (domain.min + domain.max) / 2, domain.min]
  const xTicks = [domainStart, domainStart + SEVEN_DAYS_SECONDS / 2, performance.periodEnd]

  function selectNearestPoint(clientX: number, element: SVGSVGElement) {
    const bounds = element.getBoundingClientRect()
    const viewBoxX = bounds.width > 0
      ? ((clientX - bounds.left) / bounds.width) * width
      : clientX
    let nearestIndex = 0
    let nearestDistance = Number.POSITIVE_INFINITY
    coordinates.forEach(({ x }, index) => {
      const distance = Math.abs(x - viewBoxX)
      if (distance < nearestDistance) {
        nearestDistance = distance
        nearestIndex = index
      }
    })
    setActiveIndex(nearestIndex)
  }

  function handleChartKeyDown(event: KeyboardEvent<SVGSVGElement>) {
    if (event.key === 'ArrowLeft') {
      event.preventDefault()
      setActiveIndex((current) => Math.max(0, (current ?? coordinates.length - 1) - 1))
    } else if (event.key === 'ArrowRight') {
      event.preventDefault()
      setActiveIndex((current) => Math.min(coordinates.length - 1, (current ?? 0) + 1))
    } else if (event.key === 'Home') {
      event.preventDefault()
      setActiveIndex(0)
    } else if (event.key === 'End') {
      event.preventDefault()
      setActiveIndex(coordinates.length - 1)
    }
  }

  return (
    <figure className="border border-brand-border/30 bg-surface-panel">
      <div className="flex flex-col gap-4 border-b border-brand-border/25 p-5 sm:flex-row sm:items-start sm:justify-between">
        <div>
          <h3 className="text-lg font-semibold text-content-primary">Seven-day share price</h3>
          <p className="mt-1 text-sm text-content-secondary">
            Actual USDC accounting value per vault share at hourly checkpoints.
          </p>
        </div>
        <span className="self-start border border-brand-border/30 bg-app-bg px-3 py-1.5 text-xs font-semibold uppercase text-content-secondary">
          7 days
        </span>
      </div>

      <div className="relative p-3 sm:p-5">
        <svg
          viewBox="0 0 640 240"
          className="h-56 w-full sm:h-64"
          aria-label={`${tranche.name} interactive seven-day share price chart`}
          aria-describedby={`${tranche.id}-performance-summary`}
          aria-keyshortcuts="ArrowLeft ArrowRight Home End"
          role="img"
          tabIndex={0}
          onFocus={() => {
            setActiveIndex((current) => current ?? coordinates.length - 1)
          }}
          onBlur={() => {
            setActiveIndex(null)
          }}
          onKeyDown={handleChartKeyDown}
          onPointerDown={(event) => {
            selectNearestPoint(event.clientX, event.currentTarget)
          }}
          onPointerMove={(event) => {
            if (event.pointerType === 'mouse' || event.buttons > 0) {
              selectNearestPoint(event.clientX, event.currentTarget)
            }
          }}
          onPointerLeave={(event) => {
            if (event.pointerType === 'mouse') setActiveIndex(null)
          }}
          style={{ touchAction: 'pan-y' }}
        >
          <title>{tranche.name} USDC share price over the last seven days</title>
          {yTicks.map((tick) => {
            const y = chartPointY(tick, domain.min, domain.max, plot.top, plotHeight)
            return (
              <g key={tick}>
                <line
                  x1={plot.left}
                  y1={y}
                  x2={width - plot.right}
                  y2={y}
                  stroke="rgba(255,171,150,0.12)"
                  strokeWidth="1"
                />
                <text
                  x={plot.left - 8}
                  y={y + 4}
                  fill="rgba(244,235,239,0.62)"
                  fontSize="11"
                  textAnchor="end"
                >
                  {tick.toFixed(4)}
                </text>
              </g>
            )
          })}
          <line
            data-vault-chart-axis="y"
            x1={plot.left}
            y1={plot.top}
            x2={plot.left}
            y2={plot.top + plotHeight}
            stroke="rgba(255,171,150,0.32)"
            strokeWidth="1"
          />
          <line
            data-vault-chart-axis="x"
            x1={plot.left}
            y1={plot.top + plotHeight}
            x2={width - plot.right}
            y2={plot.top + plotHeight}
            stroke="rgba(255,171,150,0.32)"
            strokeWidth="1"
          />
          {xTicks.map((tick, index) => (
            <text
              key={tick}
              x={chartPointX(tick, domainStart, performance.periodEnd, plot.left, plotWidth)}
              y={height - 10}
              fill="rgba(244,235,239,0.62)"
              fontSize="11"
              textAnchor={index === 0 ? 'start' : index === xTicks.length - 1 ? 'end' : 'middle'}
            >
              {new Date(tick * 1_000).toLocaleDateString('en-US', { month: 'short', day: 'numeric' })}
            </text>
          ))}
          <path
            data-vault-performance-series
            d={path}
            fill="none"
            stroke={VAULT_PERFORMANCE_CHART_COLOR}
            strokeLinecap="round"
            strokeLinejoin="round"
            strokeWidth="2.5"
          />
          {active ? (
            <g aria-hidden="true">
              <line
                x1={active.x}
                y1={plot.top}
                x2={active.x}
                y2={plot.top + plotHeight}
                stroke={VAULT_PERFORMANCE_CHART_COLOR}
                strokeDasharray="4 5"
                strokeOpacity="0.6"
              />
              <circle
                cx={active.x}
                cy={active.y}
                r="5"
                fill={VAULT_PERFORMANCE_CHART_COLOR}
                stroke="#2A0613"
                strokeWidth="2"
              />
            </g>
          ) : null}
        </svg>
        {active ? (
          <div
            className="pointer-events-none absolute top-3 z-10 min-w-44 -translate-x-1/2 border border-brand-border/40 bg-app-bg px-3 py-2 shadow-xl sm:top-5"
            style={{
              left: `${String((Math.min(width - 90, Math.max(90, active.x)) / width) * 100)}%`,
            }}
            role="status"
            aria-live="polite"
          >
            <p className="text-[10px] font-semibold uppercase tracking-[0.12em] text-content-secondary">
              {formatChartTimestamp(active.point.timestamp)}
            </p>
            <p className="mt-1 text-sm font-semibold text-content-primary">
              {formatHistorySharePrice(active.point.sharePrice)}
            </p>
            <p className={`mt-1 text-xs font-semibold ${
              performanceTone(activeReturn ?? 0) === 'positive'
                ? 'text-positive'
                : performanceTone(activeReturn ?? 0) === 'negative'
                  ? 'text-brand-orange'
                  : 'text-content-secondary'
            }`}>
              {formatSignedPercent(activeReturn ?? 0)} since start
            </p>
          </div>
        ) : null}
      </div>
      <figcaption id={`${tranche.id}-performance-summary`} className="sr-only">
        {tranche.name} share price moved from {startingPrice.toFixed(6)} USDC to{' '}
        {performance.points[performance.points.length - 1].sharePrice.toFixed(6)} USDC over seven
        days, a return of {formatSignedPercent(performance.return7d)} and a realized APY of{' '}
        {formatSignedPercent(performance.apy7d)}. Focus the chart and use the arrow, Home, or End keys
        to inspect hourly checkpoints.
      </figcaption>
    </figure>
  )
}

function PerformanceTab({
  tranche,
  performance,
}: {
  tranche: TrancheDefinition
  performance: CompleteVaultPerformance
}) {
  const firstPoint = performance.points[0]
  const lastPoint = performance.points[performance.points.length - 1]

  return (
    <div className="space-y-6">
      <PerformanceChart tranche={tranche} performance={performance} />

      <div className="grid gap-3 sm:grid-cols-2 xl:grid-cols-4">
        <DetailMetric
          label="7d realized APY"
          value={formatSignedPercent(performance.apy7d)}
          detail="Annualized historical result"
          tone={performanceTone(performance.apy7d)}
        />
        <DetailMetric
          label="7d return"
          value={formatSignedPercent(performance.return7d)}
          detail="Actual share-price change"
          tone={performanceTone(performance.return7d)}
        />
        <DetailMetric
          label="Start share price"
          value={formatHistorySharePrice(firstPoint.sharePrice)}
          detail={formatChartTimestamp(firstPoint.timestamp)}
        />
        <DetailMetric
          label="Current share price"
          value={formatHistorySharePrice(lastPoint.sharePrice)}
          detail={formatChartTimestamp(lastPoint.timestamp)}
        />
      </div>

      <div className="grid gap-6 xl:grid-cols-2">
        <section className="border border-brand-border/30 bg-surface-panel p-5">
          <h3 className="text-lg font-semibold text-content-primary">What can increase share value</h3>
          <ul className="mt-4 space-y-3">
            {(
              tranche.id === 'senior'
                ? [
                    'Target coupon actually transferred from Junior principal',
                    'Restoration from future realized pool revenue after impairment',
                    'Tranche-retained frozen-oracle surcharges, when active',
                  ]
                : [
                    'Collected trader losses',
                    'Realized carry paid for LP-backed exposure',
                    'Positive VPI and other trader-to-pool price adjustments',
                    'Residual revenue after Senior restoration and coupon allocation',
                  ]
            ).map((item) => (
              <li key={item} className="flex gap-2 text-sm leading-6 text-content-secondary">
                <span className="material-symbols-outlined mt-0.5 text-lg text-positive">add_circle</span>
                <span>{item}</span>
              </li>
            ))}
          </ul>
        </section>

        <section className="border border-brand-border/30 bg-surface-panel p-5">
          <h3 className="text-lg font-semibold text-content-primary">What can reduce share value</h3>
          <ul className="mt-4 space-y-3">
            {(
              tranche.id === 'senior'
                ? [
                    'Trader profits and rebates after Junior is exhausted',
                    'Bad debt or operational loss that reaches Senior',
                    'Other realized HousePool losses that reach Senior principal',
                  ]
                : [
                    'Profitable trader settlements and VPI rebates',
                    'The Senior target coupon',
                    'Liquidation shortfalls, bad debt, and first-loss absorption',
                    'Oracle, smart-contract, or stablecoin failure',
                  ]
            ).map((item) => (
              <li key={item} className="flex gap-2 text-sm leading-6 text-content-secondary">
                <span className="material-symbols-outlined mt-0.5 text-lg text-brand-orange">remove_circle</span>
                <span>{item}</span>
              </li>
            ))}
          </ul>
        </section>
      </div>

      <Alert variant="info" title="How performance is calculated">
        Seven-day realized APY annualizes the actual change between indexed vault share-price
        checkpoints. It is historical, can be negative, and is not a forecast or guaranteed return.
      </Alert>
    </div>
  )
}

function RiskTab({ tranche }: { tranche: TrancheDefinition }) {
  return (
    <div className="space-y-6">
      <Alert variant="warning" title={`${tranche.name} is not principal-protected`}>
        {tranche.primaryRisk}. Trader claims rank ahead of both tranches, and vault share value can
        fall to zero in severe conditions.
      </Alert>

      <section className="border border-brand-border/30 bg-surface-panel p-5">
        <div className="flex flex-col gap-3 sm:flex-row sm:items-start sm:justify-between">
          <div>
            <p className="text-xs font-semibold uppercase tracking-[0.14em] text-content-secondary">
              Relative risk
            </p>
            <h3 className="mt-1 text-xl font-semibold text-content-primary">{tranche.riskLabel}</h3>
          </div>
          <Badge variant={tranche.riskVariant}>{tranche.lossPriority}</Badge>
        </div>
        <div className="mt-5 grid gap-3 sm:grid-cols-2">
          {tranche.riskItems.map((item, index) => (
            <div key={item} className="border border-brand-border/25 bg-app-bg p-4">
              <span className="text-xs font-semibold text-brand-peach">0{index + 1}</span>
              <p className="mt-2 text-sm leading-6 text-content-secondary">{item}</p>
            </div>
          ))}
        </div>
      </section>

      <section className="overflow-hidden border border-brand-border/30 bg-surface-panel">
        <div className="border-b border-brand-border/25 p-5">
          <h3 className="text-lg font-semibold text-content-primary">Senior vs Junior</h3>
          <p className="mt-1 text-sm text-content-secondary">
            The distinction is internal economic priority, not a legal guarantee.
          </p>
        </div>
        <div className="overflow-x-auto">
          <table className="min-w-[720px] w-full text-left text-sm">
            <thead className="bg-app-bg text-xs uppercase tracking-[0.1em] text-content-secondary">
              <tr>
                <th className="px-5 py-3 font-medium">Dimension</th>
                <th className="px-5 py-3 font-medium text-brand-peach">Senior</th>
                <th className="px-5 py-3 font-medium text-positive">Junior</th>
              </tr>
            </thead>
            <tbody className="divide-y divide-brand-border/20">
              {[
                ['Return', 'Target coupon funded by Junior', 'Residual pool performance'],
                ['Loss order', 'After Junior is exhausted', 'First loss'],
                ['Revenue order', 'Restored to high-water mark first', 'Residual after Senior'],
                ['Withdrawal priority', 'Matured requests settle first', 'Remainder after Senior requests and buffer'],
                ['Can lose principal?', 'Yes', 'Yes'],
                ['Can be wiped out?', 'Yes', 'Yes'],
              ].map(([dimension, senior, junior]) => (
                <tr key={dimension}>
                  <th className="px-5 py-4 font-medium text-content-primary">{dimension}</th>
                  <td className="px-5 py-4 text-content-secondary">{senior}</td>
                  <td className="px-5 py-4 text-content-secondary">{junior}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </section>

      <section className="grid gap-4 md:grid-cols-2">
        {[
          {
            icon: 'code',
            title: 'Smart-contract risk',
            text: 'Vault, HousePool, oracle, and trading contracts may contain defects or behave unexpectedly.',
          },
          {
            icon: 'currency_exchange',
            title: 'USDC risk',
            text: 'The vault is denominated in USDC and depends on the stablecoin remaining transferable and near its intended value.',
          },
          {
            icon: 'sensors_off',
            title: 'Oracle and market-state risk',
            text: 'Frozen or stale oracle conditions can change fees, deposit availability, and withdrawal behavior.',
          },
          {
            icon: 'water_drop',
            title: 'Withdrawal liquidity risk',
            text: 'Accounting value can exceed immediately withdrawable USDC because trader liabilities are protected first.',
          },
        ].map((item) => (
          <div key={item.title} className="border border-brand-border/30 bg-surface-panel p-5">
            <span className="material-symbols-outlined text-3xl text-warning">{item.icon}</span>
            <h3 className="mt-3 text-lg font-semibold text-content-primary">{item.title}</h3>
            <p className="mt-2 text-sm leading-6 text-content-secondary">{item.text}</p>
          </div>
        ))}
      </section>
    </div>
  )
}

function ActivityTab({
  tranche,
  liveData,
  snapshot,
  isConnected,
  isWrongNetwork,
  depositRequests,
  redeemRequests,
  requestsLoading,
  requestDiscoveryError,
  onRefreshRequests,
  onSwitchNetwork,
}: {
  tranche: TrancheDefinition
  liveData: TrancheLiveData
  snapshot: VaultsSnapshot
  isConnected: boolean
  isWrongNetwork: boolean
  depositRequests: VaultDepositRequest[]
  redeemRequests: VaultRedeemRequest[]
  requestsLoading: boolean
  requestDiscoveryError: boolean
  onRefreshRequests: () => void
  onSwitchNetwork: () => void
}) {
  const positionValue = liveData.userShares !== undefined && liveData.sharePrice !== undefined
    ? Number(formatUnits(liveData.userShares, SHARE_DECIMALS)) * liveData.sharePrice
    : undefined
  const hasUserBalance = isConnected && liveData.userShares !== undefined
  const claimableUsdc = redeemRequests.reduce(
    (total, request) => total + request.claimableAssets,
    0n
  )
  const vaultTransactions = useVaultTransactions({
    vaultAddress: tranche.address,
    allowance: liveData.allowance,
    onSuccess: () => {
      snapshot.refresh()
      onRefreshRequests()
    },
  })

  function settlementLabel(timestamp: number): string {
    return new Date(timestamp * 1_000).toLocaleString('en-US', {
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
    })
  }

  function prepareRequestAction(action: () => void) {
    vaultTransactions.reset()
    action()
  }

  return (
    <div className="space-y-6">
      <section className="border border-brand-border/30 bg-surface-panel p-5">
        <div>
          <p className="text-xs font-semibold uppercase tracking-[0.14em] text-content-secondary">
            Your active position
          </p>
          <h3 className="mt-1 text-xl font-semibold text-content-primary">
            {hasUserBalance
              ? `${formatShares(liveData.userShares)} ${tranche.token}`
              : isConnected
                ? 'Balance unavailable'
                : 'Wallet not connected'}
          </h3>
        </div>

        <div className="mt-5 grid gap-3 sm:grid-cols-3">
          <DetailMetric
            label="Current value"
            value={isConnected && positionValue !== undefined
              ? (
                <TokenAmount
                  amount={positionValue.toLocaleString('en-US', { maximumFractionDigits: 2 })}
                />
              )
              : '--'}
          />
          <DetailMetric
            label="Requestable shares"
            value={isConnected
              ? `${formatShares(liveData.maxRequestRedeem)} ${tranche.token}`
              : '--'}
            tone={(liveData.maxRequestRedeem ?? 0n) > 0n ? 'positive' : 'default'}
          />
          <DetailMetric
            label="Claimable withdrawal"
            value={isConnected ? formatFullUsd(claimableUsdc) : '--'}
            tone={claimableUsdc > 0n ? 'positive' : 'default'}
          />
        </div>
      </section>

      <section className="border border-brand-border/30 bg-surface-panel">
        <div className="border-b border-brand-border/25 p-5">
          <div className="flex flex-wrap items-start justify-between gap-3">
            <div>
              <h3 className="text-lg font-semibold text-content-primary">Deposit requests</h3>
              <p className="mt-1 text-sm text-content-secondary">
                Funded USDC waits in escrow until the protocol settles its hourly batch.
              </p>
            </div>
            {depositRequests.length > 0 ? (
              <Badge variant="info">
                {depositRequests.length} active {depositRequests.length === 1 ? 'request' : 'requests'}
              </Badge>
            ) : null}
          </div>
        </div>

        {depositRequests.length > 0 ? (
          <div className="divide-y divide-brand-border/25">
            {depositRequests.map((request) => {
              const statusLabel = request.refundableAssets > 0n
                ? 'Refund available'
                : request.claimableShares > 0n
                  ? 'Shares ready'
                  : request.matured
                    ? 'Awaiting settlement'
                    : 'Queued'
              const statusVariant = request.refundableAssets > 0n
                ? 'warning'
                : request.claimableShares > 0n
                  ? 'success'
                  : request.matured
                    ? 'info'
                    : 'warning'
              const displayedAssets = request.pendingAssets > 0n
                ? request.pendingAssets
                : request.refundableAssets > 0n
                  ? request.refundableAssets
                  : request.claimableAssets

              return (
                <article key={String(request.requestId)} className="space-y-5 p-5">
                  <div className="flex flex-col gap-3 sm:flex-row sm:items-start sm:justify-between">
                    <div>
                      <p className="text-xs font-semibold uppercase tracking-[0.14em] text-content-secondary">
                        Request {String(request.requestId)}
                      </p>
                      <h4 className="mt-1 text-xl font-semibold text-content-primary">
                        {formatFullUsd(displayedAssets)} deposited
                      </h4>
                    </div>
                    <Badge variant={statusVariant}>{statusLabel}</Badge>
                  </div>

                  <dl className="grid gap-3 sm:grid-cols-2">
                    <DetailRow
                      label="Target settlement"
                      value={settlementLabel(request.targetTimestamp)}
                    />
                    <DetailRow
                      label="Current share estimate"
                      value={`${formatShares(request.pendingSharesEstimate)} ${tranche.token}`}
                    />
                    {request.claimableShares > 0n ? (
                      <DetailRow
                        label="Claimable shares"
                        value={`${formatShares(request.claimableShares)} ${tranche.token}`}
                        valueClassName="text-positive"
                      />
                    ) : null}
                    {request.refundableAssets > 0n ? (
                      <DetailRow
                        label="Recoverable USDC"
                        value={formatFullUsd(request.refundableAssets)}
                        valueClassName="text-warning"
                      />
                    ) : null}
                  </dl>

                  <p className="text-sm leading-6 text-content-secondary">
                    {request.refundableAssets > 0n
                      ? 'The batch did not activate this deposit. Recover the escrowed USDC.'
                      : request.claimableShares > 0n
                        ? 'The protocol settled the batch. Claim the allocated shares into your wallet.'
                        : request.matured
                          ? 'The target time has passed, but the protocol has not settled this batch yet.'
                          : 'You can cancel before settlement. The final share amount is fixed only when the batch settles.'}
                  </p>

                  {isWrongNetwork ? (
                    <Button type="button" variant="secondary" onClick={onSwitchNetwork}>
                      Switch to Arbitrum Sepolia
                    </Button>
                  ) : (
                    <div className="flex flex-wrap gap-3">
                      {request.claimableShares > 0n ? (
                        <Button
                          type="button"
                          disabled={vaultTransactions.isRunning}
                          onClick={() => {
                            prepareRequestAction(() => {
                              vaultTransactions.claimDepositShares(request.requestId)
                            })
                          }}
                        >
                          Claim shares
                        </Button>
                      ) : null}
                      {request.refundableAssets > 0n ? (
                        <Button
                          type="button"
                          variant="secondary"
                          disabled={vaultTransactions.isRunning}
                          onClick={() => {
                            prepareRequestAction(() => {
                              vaultTransactions.cancelPendingDeposit(request.requestId)
                            })
                          }}
                        >
                          Recover USDC
                        </Button>
                      ) : null}
                      {request.pendingAssets > 0n && !request.matured ? (
                        <Button
                          type="button"
                          variant="secondary"
                          disabled={vaultTransactions.isRunning}
                          onClick={() => {
                            prepareRequestAction(() => {
                              vaultTransactions.cancelPendingDeposit(request.requestId)
                            })
                          }}
                        >
                          Cancel request
                        </Button>
                      ) : null}
                    </div>
                  )}
                </article>
              )
            })}
          </div>
        ) : (
          <div className="px-6 py-8 text-center">
            <p className="text-sm text-content-secondary">No active deposit requests.</p>
          </div>
        )}
      </section>

      <section className="border border-brand-border/30 bg-surface-panel">
        <div className="border-b border-brand-border/25 p-5">
          <div className="flex flex-wrap items-start justify-between gap-3">
            <div>
              <h3 className="text-lg font-semibold text-content-primary">Withdrawal requests</h3>
              <p className="mt-1 text-sm text-content-secondary">
                Shares stay exposed to vault performance until their request is funded.
              </p>
            </div>
            {redeemRequests.length > 0 ? (
              <Badge variant="info">
                {redeemRequests.length} active {redeemRequests.length === 1 ? 'request' : 'requests'}
              </Badge>
            ) : null}
          </div>
        </div>

        {redeemRequests.length > 0 ? (
          <div className="divide-y divide-brand-border/25">
            {redeemRequests.map((request) => {
              const actionReady = request.claimableAssets > 0n || request.refundPending
              const statusLabel = request.claimableAssets > 0n
                ? 'USDC ready'
                : request.refundPending
                  ? 'Shares recoverable'
                  : request.matured
                    ? 'Awaiting funding'
                    : 'Queued'
              const displayedShares = request.pendingShares > 0n
                ? request.pendingShares
                : request.claimableShares > 0n
                  ? request.claimableShares
                  : request.refundableShares

              return (
                <article key={String(request.requestId)} className="space-y-5 p-5">
                  <div className="flex flex-col gap-3 sm:flex-row sm:items-start sm:justify-between">
                    <div>
                      <p className="text-xs font-semibold uppercase tracking-[0.14em] text-content-secondary">
                        Request {String(request.requestId)}
                      </p>
                      <h4 className="mt-1 text-xl font-semibold text-content-primary">
                        {formatShares(displayedShares)} {tranche.token} queued
                      </h4>
                    </div>
                    <Badge variant={actionReady ? 'success' : request.matured ? 'info' : 'warning'}>
                      {statusLabel}
                    </Badge>
                  </div>

                  <dl className="grid gap-3 sm:grid-cols-2">
                    <DetailRow
                      label="Target settlement"
                      value={settlementLabel(request.targetTimestamp)}
                    />
                    <DetailRow
                      label="Current USDC estimate"
                      value={formatFullUsd(request.pendingAssetsEstimate)}
                    />
                    {request.claimableAssets > 0n ? (
                      <DetailRow
                        label="Claimable USDC"
                        value={formatFullUsd(request.claimableAssets)}
                        valueClassName="text-positive"
                      />
                    ) : null}
                    {request.refundableShares > 0n ? (
                      <DetailRow
                        label="Recoverable shares"
                        value={`${formatShares(request.refundableShares)} ${tranche.token}`}
                        valueClassName="text-warning"
                      />
                    ) : null}
                  </dl>

                  <p className="text-sm leading-6 text-content-secondary">
                    {request.claimableAssets > 0n
                      ? 'This portion has been funded and can be claimed as USDC.'
                      : request.refundPending
                        ? 'This request was not funded. Reclaim the remaining escrowed shares.'
                        : request.matured
                          ? tranche.id === 'senior'
                            ? 'The request is eligible and remains queued until settlement liquidity is available.'
                            : 'The request is eligible. Senior withdrawals are funded first, so Junior may remain queued.'
                          : 'You can cancel before settlement. The shares continue to gain or lose value while queued.'}
                  </p>

                  {isWrongNetwork ? (
                    <Button type="button" variant="secondary" onClick={onSwitchNetwork}>
                      Switch to Arbitrum Sepolia
                    </Button>
                  ) : (
                    <div className="flex flex-wrap gap-3">
                      {request.claimableAssets > 0n && request.claimableShares > 0n ? (
                        <Button
                          type="button"
                          disabled={vaultTransactions.isRunning}
                          onClick={() => {
                            prepareRequestAction(() => {
                              vaultTransactions.claimRedeem(
                                request.requestId,
                                request.claimableShares
                              )
                            })
                          }}
                        >
                          Claim USDC
                        </Button>
                      ) : null}
                      {request.refundPending ? (
                        <Button
                          type="button"
                          variant="secondary"
                          disabled={vaultTransactions.isRunning}
                          onClick={() => {
                            prepareRequestAction(() => {
                              vaultTransactions.claimRedeemRefund(request.requestId)
                            })
                          }}
                        >
                          Reclaim shares
                        </Button>
                      ) : null}
                      {request.pendingShares > 0n && !request.matured ? (
                        <Button
                          type="button"
                          variant="secondary"
                          disabled={vaultTransactions.isRunning}
                          onClick={() => {
                            prepareRequestAction(() => {
                              vaultTransactions.cancelRedeemRequest(request.requestId)
                            })
                          }}
                        >
                          Cancel request
                        </Button>
                      ) : null}
                    </div>
                  )}
                </article>
              )
            })}
          </div>
        ) : (
          <div className="px-6 py-8 text-center">
            <p className="text-sm text-content-secondary">No active withdrawal requests.</p>
          </div>
        )}
      </section>

      {requestDiscoveryError ? (
        <Alert variant="warning" title="Older request history is unavailable">
          Current request IDs are still checked onchain. Retry to restore older unclaimed requests
          from explorer event history.
          <Button
            type="button"
            variant="secondary"
            className="mt-3"
            onClick={onRefreshRequests}
          >
            Retry history
          </Button>
        </Alert>
      ) : null}

      {requestsLoading && depositRequests.length === 0 && redeemRequests.length === 0 ? (
        <p className="text-sm text-content-secondary">Checking your vault requests…</p>
      ) : null}

      {vaultTransactions.error ? (
        <p className="text-sm text-brand-orange">{vaultTransactions.error}</p>
      ) : null}

      <section className="border border-brand-border/30 bg-surface-panel p-5">
        <h3 className="text-lg font-semibold text-content-primary">Contract activity</h3>
        <p className="mt-1 text-sm text-content-secondary">
          Full transaction history remains available on the block explorer.
        </p>
        <a
          href={`${EXPLORER_BASE_URL}/${tranche.address}`}
          target="_blank"
          rel="noreferrer"
          className="group mt-5 inline-flex items-center gap-2 border border-brand-border/40 px-4 py-2 text-sm font-semibold text-brand-peach hover:border-brand-peach"
        >
          <span className="group-hover:underline group-hover:underline-offset-4">
            View contract activity
          </span>
          <span className="material-symbols-outlined text-lg">open_in_new</span>
        </a>
      </section>
    </div>
  )
}

function PreviewRow({
  label,
  value,
  valueClassName = 'text-content-primary',
}: {
  label: string
  value: ReactNode
  valueClassName?: string
}) {
  return (
    <div className="flex items-start justify-between gap-4 text-sm">
      <span className="text-content-secondary">{label}</span>
      <span className={`max-w-56 text-right font-semibold ${valueClassName}`}>{value}</span>
    </div>
  )
}

function VaultPreviewModal({
  isOpen,
  onClose,
  mode,
  tranche,
  amount,
  estimatedShares,
  depositMode,
  sharePrice,
  performance,
  oracleFrozen,
  pendingActivationTimestamp,
  quoteCapturedAt,
  canSubmit,
  needsApproval,
  isSubmitting,
  onSubmit,
  submissionError,
}: {
  isOpen: boolean
  onClose: () => void
  mode: ActionMode
  tranche: TrancheDefinition
  amount: string
  estimatedShares?: number
  depositMode: string
  sharePrice?: number
  performance?: CompleteVaultPerformance
  oracleFrozen?: boolean
  pendingActivationTimestamp?: number
  quoteCapturedAt?: number
  canSubmit: boolean
  needsApproval: boolean
  isSubmitting: boolean
  onSubmit: () => void
  submissionError?: string | null
}) {
  const submitLabel = mode === 'withdraw'
    ? 'Queue withdrawal'
    : needsApproval
      ? 'Approve & queue'
      : 'Queue deposit'

  return (
    <Modal
      isOpen={isOpen}
      onClose={onClose}
      title={`${mode === 'deposit' ? 'Deposit' : 'Withdrawal'} preview`}
      size="md"
      bodyClassName="p-0"
      inertBackground
    >
      <div className="space-y-5 p-6">
        <div className="flex items-center gap-3 border border-brand-border/30 bg-app-bg p-4">
          <TrancheMark tranche={tranche} size="md" />
          <div>
            <p className="text-xs uppercase tracking-[0.12em] text-content-secondary">
              Selected tranche
            </p>
            <p className="mt-1 font-semibold text-content-primary">{tranche.name}</p>
          </div>
        </div>

        <div className="space-y-3">
          <PreviewRow
            label={mode === 'deposit' ? 'USDC deposited' : 'USDC requested'}
            value={<TokenAmount amount={amount || '0.00'} />}
          />
          <PreviewRow
            label={
              mode === 'withdraw'
                ? 'Estimated shares burned'
                : 'Current indicative shares'
            }
            value={
              estimatedShares === undefined
                ? 'Live quote unavailable'
                : `${estimatedShares.toLocaleString('en-US', { maximumFractionDigits: 6 })} ${tranche.token}`
            }
          />
          <PreviewRow label="Current share price" value={formatSharePrice(sharePrice)} />
          {performance ? (
            <PreviewRow
              label="7d realized APY"
              value={formatSignedPercent(performance.apy7d)}
              valueClassName={performanceValueClassName(performance.apy7d)}
            />
          ) : null}
          <PreviewRow
            label={mode === 'deposit' ? 'Deposit path' : 'Settlement'}
            value={mode === 'deposit' ? depositMode : 'Hourly withdrawal queue'}
          />
          <PreviewRow
            label="Target settlement"
            value={
              pendingActivationTimestamp === undefined
                ? 'Next eligible hourly batch'
                : new Date(pendingActivationTimestamp * 1_000).toLocaleString('en-US', {
                    month: 'short',
                    day: 'numeric',
                    hour: '2-digit',
                    minute: '2-digit',
                  })
            }
          />
          <PreviewRow
            label="Frozen-oracle surcharge"
            value={oracleFrozen === undefined ? 'State unavailable' : oracleFrozen ? 'Included in live vault quote where supported' : 'Inactive'}
          />
          <PreviewRow label="Network" value="Arbitrum Sepolia" />
          <PreviewRow label="Relative risk" value={tranche.riskLabel} />
          <PreviewRow
            label="Quote refreshed"
            value={
              quoteCapturedAt === undefined
                ? 'Unavailable'
                : new Date(quoteCapturedAt).toLocaleTimeString('en-US', {
                    hour: '2-digit',
                    minute: '2-digit',
                    second: '2-digit',
                  })
            }
          />
        </div>

        <Alert variant="info" title="The final amount is set at settlement">
            This is a current estimate, not a guaranteed batch outcome. {mode === 'deposit'
              ? 'The final shares are calculated when the deposit batch settles.'
              : 'Queued shares remain exposed to gains and losses until the withdrawal is funded.'}
          </Alert>

        {canSubmit ? (
          <Alert variant="info" title="What happens next">
            Confirming starts {needsApproval ? 'an exact USDC approval followed by ' : ''}
            {mode === 'deposit'
              ? 'a funded vault-deposit request'
              : 'a vault-share withdrawal request'}.
            The app simulates each transaction before asking your wallet to submit it.
          </Alert>
        ) : (
          <Alert variant="warning" title="Action unavailable">
            The live vault gates do not currently permit this action.
          </Alert>
        )}

        {submissionError ? (
          <p className="text-sm leading-6 text-brand-orange">{submissionError}</p>
        ) : null}
      </div>

      <div className="grid grid-cols-2 gap-3 border-t border-brand-border/30 p-4">
        <Button
          type="button"
          variant="secondary"
          className="w-full"
          onClick={onClose}
          analyticsId="vault_preview_closed"
          analyticsSurface="vaults"
        >
          Cancel
        </Button>
        <Button
          type="button"
          variant={mode === 'withdraw' ? 'secondary' : 'primary'}
          className="w-full"
          disabled={!canSubmit}
          isLoading={isSubmitting}
          onClick={onSubmit}
          analyticsId={`vault_${mode}_submitted`}
          analyticsSurface="vaults"
          analyticsProperties={{ tranche: tranche.id }}
        >
          {canSubmit ? submitLabel : 'Unavailable'}
        </Button>
      </div>
    </Modal>
  )
}

function VaultActionPanel({
  tranche,
  liveData,
  snapshot,
  performance,
  isConnected,
  isWrongNetwork,
  onConnect,
  onSwitchNetwork,
  isSwitchingNetwork,
  switchError,
  depositRequests,
  redeemRequests,
  onRefreshRequests,
  onViewRequests,
}: {
  tranche: TrancheDefinition
  liveData: TrancheLiveData
  snapshot: VaultsSnapshot
  performance?: CompleteVaultPerformance
  isConnected: boolean
  isWrongNetwork: boolean
  onConnect: () => void
  onSwitchNetwork: () => void
  isSwitchingNetwork: boolean
  switchError?: string
  depositRequests: VaultDepositRequest[]
  redeemRequests: VaultRedeemRequest[]
  onRefreshRequests: () => void
  onViewRequests: () => void
}) {
  const [mode, setMode] = useState<ActionMode>('deposit')
  const [amount, setAmount] = useState('')
  const [showPreview, setShowPreview] = useState(false)
  const [reviewQuote, setReviewQuote] = useState<{
    estimatedShares: number
    capturedAt: number
  }>()
  const [isRefreshingQuote, setIsRefreshingQuote] = useState(false)
  const [quoteRefreshError, setQuoteRefreshError] = useState<string>()
  const amountRaw = parseUsdc(amount)
  const depositMode = getDepositMode(liveData)
  const pendingActivationTimestamp = liveData.nextRequestEpoch !== undefined
    ? Number(liveData.nextRequestEpoch * 3_600n)
    : undefined
  const vaultTransactions = useVaultTransactions({
    vaultAddress: tranche.address,
    allowance: liveData.allowance,
    onSuccess: () => {
      setAmount('')
      setShowPreview(false)
      setReviewQuote(undefined)
      snapshot.refresh()
      onRefreshRequests()
    },
  })
  const {
    data: quoteData,
    isLoading: isQuoteLoading,
    isFetching: isQuoteFetching,
    refetch: refetchQuote,
  } = useReadContracts({
    contracts: [
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: tranche.address,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'estimateDepositShares',
        args: [amountRaw],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: tranche.address,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'estimateWithdrawShares',
        args: [amountRaw],
      },
    ],
    query: {
      enabled: amountRaw > 0n,
    },
  })
  const quoteResults = quoteData as readonly ContractResult[] | undefined
  const isQuotePending = isQuoteLoading || isQuoteFetching
  const quotedSharesRaw = asBigInt(readResult(quoteResults, mode === 'deposit' ? 0 : 1))
  const estimatedShares = quotedSharesRaw === undefined
    ? undefined
    : Number(formatUnits(quotedSharesRaw, SHARE_DECIMALS))
  const positionValue = liveData.userShares !== undefined && liveData.sharePrice !== undefined
    ? Number(formatUnits(liveData.userShares, SHARE_DECIMALS)) * liveData.sharePrice
    : undefined
  const estimatedPositionUsdc = positionValue !== undefined && Number.isFinite(positionValue)
    ? parseUnits(positionValue.toFixed(USDC_DECIMALS), USDC_DECIMALS)
    : undefined
  const maxAmount = mode === 'deposit' ? snapshot.walletUsdc : estimatedPositionUsdc
  const exceedsAvailable = isConnected && maxAmount !== undefined && amountRaw > maxAmount
  const liveDepositLimit = depositMode === 'Queued deposits open'
    ? liveData.maxRequestDeposit
    : 0n
  const depositLimitExceeded = mode === 'deposit'
    && liveDepositLimit !== undefined
    && amountRaw > liveDepositLimit
  const invalidAmount = amountRaw <= 0n
  const belowMinimumDeposit = mode === 'deposit'
    && amountRaw > 0n
    && snapshot.pool.minTrancheDepositUsdc !== undefined
    && amountRaw < snapshot.pool.minTrancheDepositUsdc
  const redeemLimitExceeded = mode === 'withdraw'
    && quotedSharesRaw !== undefined
    && liveData.maxRequestRedeem !== undefined
    && quotedSharesRaw > liveData.maxRequestRedeem
  const actionDataUnavailable = !snapshot.hasLivePoolData
    || !liveData.hasCoreData
    || (
      mode === 'deposit'
        ? snapshot.walletUsdc === undefined || !liveData.hasDepositData
        : !liveData.hasUserData
    )
  const safetyBlocked = mode === 'deposit' && (
    liveData.poolPaused === true
    || liveData.depositEnabled === false
    || (snapshot.pool.currentTerminalDeficitUsdc ?? 0n) > 0n
  )
  const depositUnavailable = mode === 'deposit'
    && depositMode !== 'Queued deposits open'
  const quoteUnavailable = amountRaw > 0n && !isQuotePending && estimatedShares === undefined
  const actionBlocked = actionDataUnavailable || safetyBlocked || depositUnavailable
  const needsApproval = mode === 'deposit'
    && liveData.allowance !== undefined
    && liveData.allowance < amountRaw
  const formInvalid = invalidAmount
    || exceedsAvailable
    || depositLimitExceeded
    || belowMinimumDeposit
    || redeemLimitExceeded
    || actionBlocked
    || quoteUnavailable
  const canSubmitTransaction = isConnected
    && !isWrongNetwork
    && !formInvalid
    && (mode === 'withdraw' || depositMode === 'Queued deposits open')
  const inputError = exceedsAvailable
    ? `Exceeds available ${mode === 'deposit' ? 'balance' : 'withdrawal limit'}.`
    : depositLimitExceeded
      ? 'Exceeds the live deposit-request maximum.'
      : belowMinimumDeposit
        ? `The minimum vault deposit is ${formatFullUsdc(snapshot.pool.minTrancheDepositUsdc)} USDC.`
        : redeemLimitExceeded
          ? 'Exceeds the number of shares currently eligible for a withdrawal request.'
          : undefined

  const buttonLabel = !isConnected
    ? 'Connect wallet'
    : isWrongNetwork
      ? isSwitchingNetwork
        ? 'Switching network...'
        : 'Switch to Arbitrum Sepolia'
      : `Review ${mode}`

  async function handlePrimaryAction() {
    if (!isConnected) {
      onConnect()
      return
    }

    if (isWrongNetwork) {
      onSwitchNetwork()
      return
    }

    setIsRefreshingQuote(true)
    setQuoteRefreshError(undefined)

    try {
      const refreshedQuote = await refetchQuote()
      const refreshedResults = refreshedQuote.data as readonly ContractResult[] | undefined
      const refreshedSharesRaw = asBigInt(
        readResult(refreshedResults, mode === 'deposit' ? 0 : 1)
      )

      if (refreshedSharesRaw === undefined) {
        setQuoteRefreshError('The live vault quote could not be refreshed. Please try again.')
        return
      }

      setReviewQuote({
        estimatedShares: Number(formatUnits(refreshedSharesRaw, SHARE_DECIMALS)),
        capturedAt: Date.now(),
      })
      setShowPreview(true)
    } catch {
      setQuoteRefreshError('The live vault quote could not be refreshed. Please try again.')
    } finally {
      setIsRefreshingQuote(false)
    }
  }

  function handleTransactionSubmit() {
    if (!canSubmitTransaction) return

    setShowPreview(false)
    vaultTransactions.reset()
    if (mode === 'deposit') {
      vaultTransactions.requestDeposit(amountRaw)
    } else {
      if (quotedSharesRaw === undefined) return
      vaultTransactions.requestRedeem(quotedSharesRaw)
    }
  }

  return (
    <>
      <aside className="border border-brand-border/30 bg-surface-panel">
        <div className="border-b border-brand-border/25 p-5">
          <div>
            <h2 className="text-xl font-semibold text-content-primary">{mode === 'deposit' ? 'Deposit USDC' : 'Withdraw USDC'}</h2>
            <p className="mt-1 text-sm text-content-secondary">{tranche.name}</p>
          </div>
        </div>

        <div className="space-y-5 p-5">
          <div className="grid grid-cols-2 border border-brand-border/30 bg-app-bg p-1">
            {(['deposit', 'withdraw'] as const).map((item) => (
              <button
                key={item}
                type="button"
                aria-pressed={mode === item}
                onClick={() => {
                  setMode(item)
                  setAmount('')
                  setReviewQuote(undefined)
                  setQuoteRefreshError(undefined)
                }}
                className={`px-4 py-2 text-sm font-semibold capitalize transition-colors ${
                  mode === item
                    ? 'bg-surface-panel text-content-primary'
                    : 'text-content-secondary hover:text-brand-peach'
                }`}
              >
                {item}
              </button>
            ))}
          </div>

          <TokenInput
            value={amount}
            onChange={(nextAmount) => {
              setAmount(nextAmount)
              setReviewQuote(undefined)
              setQuoteRefreshError(undefined)
            }}
            token={{ symbol: 'USDC', decimals: USDC_DECIMALS }}
            balance={isConnected ? maxAmount : undefined}
            balanceLabel={mode === 'deposit' ? 'Wallet balance:' : 'Estimated position value:'}
            label={mode === 'deposit' ? 'Amount to deposit' : 'Amount to withdraw'}
            error={inputError}
          />

          <div className="space-y-3 border border-brand-border/25 bg-app-bg p-4">
            <PreviewRow label="Share price" value={formatSharePrice(liveData.sharePrice)} />
            <PreviewRow
              label={
                mode === 'withdraw'
                  ? 'Shares burned'
                  : 'Current indicative shares'
              }
              value={
                isQuotePending
                  ? 'Loading quote...'
                  : estimatedShares === undefined
                    ? '--'
                    : `${estimatedShares.toLocaleString('en-US', { maximumFractionDigits: 6 })} ${tranche.token}`
              }
            />
            {mode === 'deposit' ? (
              <>
                <PreviewRow label="Deposit path" value={depositMode} />
                <PreviewRow
                  label="Target settlement"
                  value={
                    pendingActivationTimestamp === undefined
                      ? 'Next eligible hourly batch'
                      : new Date(pendingActivationTimestamp * 1_000).toLocaleString('en-US', {
                          month: 'short',
                          day: 'numeric',
                          hour: '2-digit',
                          minute: '2-digit',
                        })
                  }
                />
              </>
            ) : (
              <>
                <PreviewRow
                  label="Position value"
                  value={positionValue === undefined
                    ? '--'
                    : <TokenAmount amount={positionValue.toLocaleString('en-US', { maximumFractionDigits: 2 })} />}
                />
                <PreviewRow
                  label="Current estimated receipt"
                  value={<TokenAmount amount={amount || '0.00'} />}
                />
                <PreviewRow label="Settlement" value="Queued; Senior requests are funded first" />
              </>
            )}
            {performance ? (
              <PreviewRow
                label="7d realized APY"
                value={formatSignedPercent(performance.apy7d)}
                valueClassName={performanceValueClassName(performance.apy7d)}
              />
            ) : null}
            <PreviewRow
              label="Oracle surcharge"
              value={snapshot.pool.oracleFrozen === undefined ? 'State unavailable' : snapshot.pool.oracleFrozen ? 'Reflected by vault preview where supported' : 'Inactive'}
            />
          </div>

          {mode === 'deposit' && depositMode === 'Queued deposits open' ? (
            <Alert variant="info" title="This deposit will be queued">
              USDC moves into vault escrow now. You can cancel before settlement. After the
              protocol settles the batch, claim the shares from Your position.
            </Alert>
          ) : null}

          {mode === 'deposit' && depositMode !== 'Queued deposits open' ? (
            <Alert variant="warning" title="Deposits unavailable">
              The vault is not accepting new funded deposit requests right now.
            </Alert>
          ) : null}

          {mode === 'withdraw' && liveData.maxRequestRedeem === 0n && isConnected ? (
            <Alert variant="warning" title="No shares can be queued right now">
              The contract currently reports zero eligible shares for a new withdrawal request.
            </Alert>
          ) : null}

          <Button
            type="button"
            className="w-full"
            variant={mode === 'withdraw' ? 'secondary' : 'primary'}
            onClick={() => {
              void handlePrimaryAction()
            }}
            isLoading={
              (isSwitchingNetwork && isWrongNetwork)
              || (isConnected && !isWrongNetwork && (isQuotePending || isRefreshingQuote))
            }
            disabled={
              isConnected &&
              !isWrongNetwork &&
              formInvalid
            }
            analyticsId={`vault_${mode}_reviewed`}
            analyticsSurface="vaults"
            analyticsProperties={{ tranche: tranche.id }}
          >
            {buttonLabel}
          </Button>

          {safetyBlocked ? (
            <p className="text-xs leading-5 text-brand-orange">
              New deposits are paused by the current pool or vault safety state. Existing
              withdrawal requests can still be queued and managed.
            </p>
          ) : null}
          {actionDataUnavailable ? (
            <p className="text-xs leading-5 text-brand-orange">
              Live HousePool, vault, or wallet data is incomplete, so financial previews are
              disabled.
            </p>
          ) : null}
          {mode === 'deposit' && snapshot.pool.degradedMode === true && snapshot.pool.seniorImpaired !== true ? (
            <p className="text-xs leading-5 text-warning">
              Degraded mode is active. Deposit availability still depends on the vault&apos;s
              separate live safety gates.
            </p>
          ) : null}
          {switchError ? <p className="text-xs leading-5 text-brand-orange">{switchError}</p> : null}
          {quoteRefreshError ? (
            <p className="text-xs leading-5 text-brand-orange">{quoteRefreshError}</p>
          ) : null}
          {vaultTransactions.error ? (
            <p className="text-xs leading-5 text-brand-orange">{vaultTransactions.error}</p>
          ) : null}

          <p className="text-xs leading-5 text-content-secondary">
            Vault shares can rise or fall in value. A displayed position is not a promise of
            immediate redemption, and recent APY is not a forecast.
          </p>
        </div>

        {depositRequests.length + redeemRequests.length > 0 ? (
          <div className="border-t border-brand-border/30 p-5">
            <div className="flex items-start justify-between gap-3">
              <div>
                <p className="text-xs font-semibold uppercase tracking-[0.14em] text-content-secondary">
                  Your request queue
                </p>
                <p className="mt-1 text-lg font-semibold text-content-primary">
                  {depositRequests.length + redeemRequests.length} active{' '}
                  {depositRequests.length + redeemRequests.length === 1 ? 'request' : 'requests'}
                </p>
                <p className="mt-1 text-xs leading-5 text-content-secondary">
                  {depositRequests.length} deposit · {redeemRequests.length} withdrawal
                </p>
              </div>
              <Badge variant={depositRequests.some(({ claimableShares }) => claimableShares > 0n)
                || redeemRequests.some(({ claimableAssets, refundPending }) => (
                  claimableAssets > 0n || refundPending
                )) ? 'success' : 'warning'}>
                {depositRequests.some(({ claimableShares }) => claimableShares > 0n)
                  || redeemRequests.some(({ claimableAssets, refundPending }) => (
                    claimableAssets > 0n || refundPending
                  ))
                  ? 'Action ready'
                  : 'In progress'}
              </Badge>
            </div>
            <Button
              type="button"
              variant="secondary"
              className="mt-4 w-full"
              onClick={onViewRequests}
            >
              View & manage
            </Button>
          </div>
        ) : null}
      </aside>

      <VaultPreviewModal
        isOpen={showPreview}
        onClose={() => {
          setShowPreview(false)
        }}
        mode={mode}
        tranche={tranche}
        amount={amount}
        estimatedShares={reviewQuote?.estimatedShares}
        depositMode={depositMode}
        sharePrice={liveData.sharePrice}
        performance={performance}
        oracleFrozen={snapshot.pool.oracleFrozen}
        pendingActivationTimestamp={pendingActivationTimestamp}
        quoteCapturedAt={reviewQuote?.capturedAt}
        canSubmit={canSubmitTransaction}
        needsApproval={needsApproval}
        isSubmitting={vaultTransactions.isRunning}
        onSubmit={handleTransactionSubmit}
        submissionError={vaultTransactions.error}
      />
    </>
  )
}

function VaultDetail({
  tranche,
  snapshot,
  history,
  ownerAddress,
  isConnected,
  isWrongNetwork,
  onConnect,
  onSwitchNetwork,
  isSwitchingNetwork,
  switchError,
}: {
  tranche: TrancheDefinition
  snapshot: VaultsSnapshot
  history?: VaultHistory
  ownerAddress?: Address
  isConnected: boolean
  isWrongNetwork: boolean
  onConnect: () => void
  onSwitchNetwork: () => void
  isSwitchingNetwork: boolean
  switchError?: string
}) {
  const [activeTab, setActiveTab] = useState<DetailTab>('overview')
  const liveData = snapshot.tranches[tranche.id]
  const performance = getCompleteVaultPerformance(history, tranche.id)
  const hasPerformance = performance !== undefined
  const vaultRequests = useVaultRequests({
    controller: ownerAddress,
    isSenior: tranche.id === 'senior',
    currentEpoch: liveData.currentEpoch,
  })
  const poolWithdrawCap = tranche.id === 'senior'
    ? snapshot.pool.seniorPoolWithdrawCapUsdc
    : snapshot.pool.juniorPoolWithdrawCapUsdc
  const tabs: { id: DetailTab; label: string }[] = [
    { id: 'overview', label: 'Overview' },
    ...(hasPerformance ? [{ id: 'performance' as const, label: 'Performance' }] : []),
    { id: 'risk', label: 'Risk' },
    { id: 'activity', label: 'Your position' },
  ]
  const displayedTab = activeTab === 'performance' && !hasPerformance ? 'overview' : activeTab

  function handleTabKeyDown(event: KeyboardEvent<HTMLButtonElement>, index: number) {
    let nextIndex: number | undefined

    if (event.key === 'ArrowRight') nextIndex = (index + 1) % tabs.length
    if (event.key === 'ArrowLeft') nextIndex = (index - 1 + tabs.length) % tabs.length
    if (event.key === 'Home') nextIndex = 0
    if (event.key === 'End') nextIndex = tabs.length - 1
    if (nextIndex === undefined) return

    event.preventDefault()
    const nextTab = tabs[nextIndex]
    setActiveTab(nextTab.id)
    window.requestAnimationFrame(() => {
      document.getElementById(`vault-tab-${tranche.id}-${nextTab.id}`)?.focus()
    })
  }

  return (
    <div className="space-y-6">
      <Link
        to="/vaults"
        className="group inline-flex items-center gap-2 text-sm font-semibold text-content-secondary transition-colors hover:text-brand-peach"
      >
        <span className="material-symbols-outlined text-lg">arrow_back</span>
        <span className="group-hover:underline group-hover:underline-offset-4">All vaults</span>
      </Link>

      <section className="border border-brand-border/30 bg-surface-panel">
        <div className="flex flex-col gap-6 border-b border-brand-border/25 p-6 lg:flex-row lg:items-start lg:justify-between">
          <div className="flex min-w-0 items-start gap-4">
            <TrancheMark tranche={tranche} />
            <div className="min-w-0">
              <div className="flex flex-wrap items-center gap-2">
                <p className="text-xs font-semibold uppercase tracking-[0.16em] text-content-secondary">
                  {tranche.eyebrow}
                </p>
              </div>
              <h1 className="mt-2 text-3xl font-semibold text-content-primary">{tranche.name}</h1>
              <p className="mt-3 max-w-3xl text-sm leading-6 text-content-secondary">
                {tranche.description}
              </p>
              <div className="mt-4 flex flex-wrap items-center gap-3 text-xs text-content-secondary">
                <TokenLabel token="USDC" />
                <a
                  href={`${EXPLORER_BASE_URL}/${tranche.address}`}
                  target="_blank"
                  rel="noreferrer"
                  className="group inline-flex items-center gap-1.5 border border-brand-border/30 bg-app-bg px-2 py-1 text-brand-peach hover:border-brand-peach"
                >
                  <span className="group-hover:underline">{formatAddress(tranche.address)}</span>
                  <span className="material-symbols-outlined text-sm">open_in_new</span>
                </a>
              </div>
            </div>
          </div>
        </div>

        <dl className={`grid grid-cols-2 gap-x-4 gap-y-6 p-6 ${
          hasPerformance ? 'lg:grid-cols-5' : 'lg:grid-cols-4'
        }`}>
          <PoolStat
            label="Tranche TVL / NAV"
            value={formatCompactUsd(liveData.totalAssets)}
            subvalue={tranche.id === 'senior'
              ? (
                <span>
                  Current Senior capacity: {formatVaultLimit(
                    snapshot.pool.seniorDepositCapacityUsdc
                  )}
                </span>
              )
              : liveData.maxRequestDeposit === undefined
                ? 'Current request limit unavailable'
                : (
                  <span>
                    Current request limit: {formatVaultLimit(liveData.maxRequestDeposit)}
                  </span>
                )}
            tooltip="Current ERC-4626 totalAssets accounting value. This can rise or fall and is not cumulative deposits."
          />
          {performance ? (
            <PoolStat
              label="7d realized APY"
              value={formatSignedPercent(performance.apy7d)}
              subvalue={`${formatSignedPercent(performance.return7d)} actual 7d return`}
              valueClassName={performanceValueClassName(performance.apy7d)}
              tooltip="Annualized historical return calculated from seven days of indexed share-price checkpoints."
            />
          ) : null}
          <PoolStat
            label="Share price"
            value={formatSharePrice(liveData.sharePrice)}
            subvalue={`1 ${tranche.token}`}
          />
          <PoolStat
            label="Pool withdrawal cap"
            value={formatCompactUsd(poolWithdrawCap)}
            subvalue={tranche.withdrawalPriority}
          />
          <PoolStat
            label="Return model"
            value={tranche.targetReturn}
            subvalue={tranche.returnModel}
            valueClassName="text-warning"
          />
        </dl>
      </section>

      {liveData.lpEpochSettlementPaused === true ? (
        <Alert variant="warning" title="Epoch settlement paused">
          You can still submit requests, claim already-funded assets or shares, and use any
          cancellation or refund action offered for your request. Deposit requests will not
          activate and redemption requests will not receive new funding until governance resumes
          hourly settlement.
        </Alert>
      ) : null}

      <div className="grid items-start gap-6 lg:grid-cols-[minmax(0,1fr)_360px]">
        <div className="min-w-0 space-y-6">
          <div
            role="tablist"
            aria-label={`${tranche.name} details`}
            className="flex overflow-x-auto border border-brand-border/30 bg-surface-panel p-1"
          >
            {tabs.map((tab, index) => (
              <button
                key={tab.id}
                id={`vault-tab-${tranche.id}-${tab.id}`}
                type="button"
                role="tab"
                aria-selected={displayedTab === tab.id}
                aria-controls={`vault-panel-${tranche.id}`}
                tabIndex={displayedTab === tab.id ? 0 : -1}
                onClick={() => {
                  setActiveTab(tab.id)
                }}
                onKeyDown={(event) => {
                  handleTabKeyDown(event, index)
                }}
                className={`shrink-0 px-4 py-2 text-sm font-semibold transition-colors ${
                  displayedTab === tab.id
                    ? 'bg-app-bg text-content-primary'
                    : 'text-content-secondary hover:text-brand-peach'
                }`}
              >
                {tab.label}
              </button>
            ))}
          </div>

          <div
            id={`vault-panel-${tranche.id}`}
            role="tabpanel"
            aria-labelledby={`vault-tab-${tranche.id}-${displayedTab}`}
          >
            {displayedTab === 'overview' ? (
              <OverviewTab
                tranche={tranche}
                liveData={liveData}
                snapshot={snapshot}
                isConnected={isConnected}
              />
            ) : null}
            {displayedTab === 'performance' && performance ? (
              <PerformanceTab tranche={tranche} performance={performance} />
            ) : null}
            {displayedTab === 'risk' ? <RiskTab tranche={tranche} /> : null}
            {displayedTab === 'activity' ? (
              <ActivityTab
                tranche={tranche}
                liveData={liveData}
                snapshot={snapshot}
                isConnected={isConnected}
                isWrongNetwork={isWrongNetwork}
                depositRequests={vaultRequests.depositRequests}
                redeemRequests={vaultRequests.redeemRequests}
                requestsLoading={vaultRequests.isLoading}
                requestDiscoveryError={vaultRequests.discoveryError}
                onRefreshRequests={vaultRequests.refresh}
                onSwitchNetwork={onSwitchNetwork}
              />
            ) : null}
          </div>
        </div>

        <div className="lg:sticky lg:top-32">
          <VaultActionPanel
            key={tranche.id}
            tranche={tranche}
            liveData={liveData}
            snapshot={snapshot}
            performance={performance}
            isConnected={isConnected}
            isWrongNetwork={isWrongNetwork}
            onConnect={onConnect}
            onSwitchNetwork={onSwitchNetwork}
            isSwitchingNetwork={isSwitchingNetwork}
            switchError={switchError}
            depositRequests={vaultRequests.depositRequests}
            redeemRequests={vaultRequests.redeemRequests}
            onRefreshRequests={vaultRequests.refresh}
            onViewRequests={() => {
              setActiveTab('activity')
            }}
          />
        </div>
      </div>
    </div>
  )
}

function InvalidVault() {
  return (
    <div className="mx-auto max-w-2xl border border-brand-border/30 bg-surface-panel p-8 text-center">
      <span className="material-symbols-outlined text-5xl text-brand-orange">search_off</span>
      <h1 className="mt-4 text-2xl font-semibold text-content-primary">Vault not found</h1>
      <p className="mt-2 text-sm leading-6 text-content-secondary">
        Plether currently exposes the Senior and Junior USDC vaults.
      </p>
      <Link
        to="/vaults"
        className="mt-6 inline-flex items-center gap-2 border border-brand-peach bg-brand-peach px-4 py-2 text-sm font-semibold text-app-bg hover:bg-app-bg hover:text-brand-peach hover:underline hover:underline-offset-4"
      >
        View all vaults
      </Link>
    </div>
  )
}

export function Vaults() {
  const { trancheId } = useParams()
  const { address, isConnected } = useAccount()
  const chainId = useChainId()
  const { open } = useAppKit()
  const {
    switchToArbitrumSepolia,
    isSwitching,
    switchError,
    clearSwitchError,
  } = useSwitchToArbitrumSepolia()
  const snapshot = useVaultsSnapshot(address)
  const vaultHistoryQuery = usePerpsVaultHistory()
  const vaultHistory = vaultHistoryQuery.data?.data
  const selectedTranche = trancheId === 'senior' || trancheId === 'junior'
    ? TRANCHES[trancheId]
    : undefined
  const isWrongNetwork = isConnected && chainId !== PERPS_ARBITRUM_SEPOLIA_CHAIN_ID

  function openWallet() {
    clearSwitchError()
    syncAppKitModalStyleOverrides()
    void open()
    syncAppKitModalStyleOverrides()
  }

  if (trancheId && !selectedTranche) {
    return <InvalidVault />
  }

  if (!selectedTranche) {
    return <VaultsOverview snapshot={snapshot} history={vaultHistory} />
  }

  return (
    <VaultDetail
      tranche={selectedTranche}
      snapshot={snapshot}
      history={vaultHistory}
      ownerAddress={address}
      isConnected={isConnected}
      isWrongNetwork={isWrongNetwork}
      onConnect={openWallet}
      onSwitchNetwork={() => {
        void switchToArbitrumSepolia()
      }}
      isSwitchingNetwork={isSwitching}
      switchError={switchError ?? undefined}
    />
  )
}

export default Vaults
