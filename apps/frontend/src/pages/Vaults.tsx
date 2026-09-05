import {
  useEffect,
  useLayoutEffect,
  useMemo,
  useRef,
  useState,
  type KeyboardEvent,
  type ReactNode,
} from 'react'
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
import { JuniorMarketExposure } from '../components/JuniorMarketExposure'
import { Alert, Badge, Button, DocsLink, InfoTooltip, Modal, Spinner, SuccessIcon, TokenAmount, TokenLabel, Tooltip, type TooltipDocsLink } from '../components/ui'
import { DOCS_LINKS } from '../config/docs'
import { openAppKit } from '../config/wagmi'
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
  useVaultActivity,
  useVaultRequests,
  useVaultTransactions,
  type VaultActivityTranche,
  type VaultDepositRequest,
  type VaultHolderDistribution,
  type VaultOverviewActivityItem,
  type VaultRedeemRequest,
} from '../hooks'
import { dxyExposureFromContractNotional, formatPerpsUsdc } from '../utils/perps'
import { calculatePerpsPoolCapital } from '../utils/perpsPoolCapital'

type TrancheId = 'senior' | 'junior'
type DetailSectionId = 'overview' | 'market-exposure' | 'performance' | 'position' | 'activity'
type ActionMode = 'deposit' | 'withdraw'
type DataStatus = 'live' | 'partial' | 'syncing' | 'unavailable'

type VaultRequestAction =
  | { kind: 'cancel-deposit'; requestId: bigint; assets: bigint }
  | { kind: 'recover-deposit'; requestId: bigint; assets: bigint }
  | { kind: 'claim-deposit'; requestId: bigint; shares: bigint }
  | { kind: 'cancel-withdrawal'; requestId: bigint; shares: bigint }
  | { kind: 'claim-withdrawal'; requestId: bigint; shares: bigint; assets: bigint }
  | { kind: 'reclaim-withdrawal'; requestId: bigint; shares: bigint }

const DEFAULT_STICKY_HEADER_HEIGHT_PX = 144
const SECTION_NAV_HEIGHT_PX = 56
const STICKY_ELEMENT_GAP_PX = 16

function useStickyHeaderHeight() {
  const [height, setHeight] = useState(DEFAULT_STICKY_HEADER_HEIGHT_PX)

  useEffect(() => {
    const header = document.querySelector<HTMLElement>('[data-app-sticky-header]')
    if (!header) return

    const updateHeight = () => {
      const measuredHeight = Math.ceil(header.getBoundingClientRect().height)
      if (measuredHeight > 0) {
        setHeight((currentHeight) => (
          currentHeight === measuredHeight ? currentHeight : measuredHeight
        ))
      }
    }

    updateHeight()
    window.addEventListener('resize', updateHeight)

    const resizeObserver = typeof ResizeObserver === 'undefined'
      ? undefined
      : new ResizeObserver(updateHeight)
    resizeObserver?.observe(header)

    return () => {
      window.removeEventListener('resize', updateHeight)
      resizeObserver?.disconnect()
    }
  }, [])

  return height
}

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

interface ShareValueFactor {
  label: string
  tooltip: string
  docsLink: TooltipDocsLink
}

const SENIOR_SHARE_VALUE_FACTORS: {
  increase: ShareValueFactor[]
  reduce: ShareValueFactor[]
} = {
  increase: [
    {
      label: 'Targeted return funded by Junior',
      tooltip: 'The Senior target coupon transfers available Junior principal to Senior. It is capped by what Junior can fund, and any unpaid amount does not accrue as debt.',
      docsLink: DOCS_LINKS.poolLiquidity,
    },
    {
      label: 'Recovery of earlier Senior losses',
      tooltip: 'After Senior has been impaired, future reconciled LP-owned value restores it toward its protected high-water mark before Junior receives residual value.',
      docsLink: DOCS_LINKS.poolLiquidity,
    },
    {
      label: 'Frozen-price withdrawal surcharges',
      tooltip: 'When an eligible withdrawal is funded using the permitted stored price while the oracle is frozen, the surcharge stays in Senior and benefits the remaining shares. It does not go to Plether or a keeper.',
      docsLink: DOCS_LINKS.withdrawLiquidity,
    },
  ],
  reduce: [
    {
      label: 'Trader profits and rebates after Junior is exhausted',
      tooltip: 'Junior absorbs reconciled pool losses first. Senior principal falls only for the remainder left after Junior reaches zero.',
      docsLink: DOCS_LINKS.poolLiquidity,
    },
    {
      label: 'Liquidation shortfalls and bad debt after Junior is exhausted',
      tooltip: 'Senior is exposed only when a reconciled loss is larger than the Junior principal available to absorb it.',
      docsLink: DOCS_LINKS.lpReturns,
    },
    {
      label: 'Oracle, contract, or stablecoin losses after Junior is exhausted',
      tooltip: 'A failure affects Senior share value only when it causes a recognized pool loss that remains after Junior has been exhausted.',
      docsLink: DOCS_LINKS.lpRisks,
    },
  ],
}

const JUNIOR_SHARE_VALUE_FACTORS: {
  increase: ShareValueFactor[]
  reduce: ShareValueFactor[]
} = {
  increase: [
    {
      label: 'Collectible marked and collected trader losses',
      tooltip: 'A collateral-capped collectible marked loss can increase accounting value before collection. Once collected, it also adds physical USDC to the pool.',
      docsLink: DOCS_LINKS.lpReturns,
    },
    {
      label: 'Carry paid by traders to LPs',
      tooltip: 'Carry compensates LPs for keeping bounded payout capacity available while a position remains open. It counts as LP value when realized and collected.',
      docsLink: DOCS_LINKS.lpReturns,
    },
    {
      label: 'Positive VPI and paid frozen-close spreads',
      tooltip: 'Positive VPI charges traders for increasing directional imbalance. A paid frozen-close spread compensates LPs for eligible closes while the oracle is frozen.',
      docsLink: DOCS_LINKS.lpReturns,
    },
    {
      label: 'LP share of collected liquidation fees',
      tooltip: 'The liquidation keeper receives the bounty. The remaining collected liquidation charge is LP-owned and follows the waterfall, restoring any Senior impairment before Junior receives the residual.',
      docsLink: DOCS_LINKS.lpReturns,
    },
    {
      label: 'Residual LP-owned revenue',
      tooltip: 'Reconciled LP-owned value first restores an impaired Senior tranche toward its protected high-water mark. The remaining ordinary value becomes Junior principal.',
      docsLink: DOCS_LINKS.poolLiquidity,
    },
    {
      label: 'Frozen-price withdrawal surcharges',
      tooltip: 'When an eligible withdrawal is funded using the permitted stored price while the oracle is frozen, the surcharge stays in Junior and benefits the remaining shares. It does not go to Plether or a keeper.',
      docsLink: DOCS_LINKS.withdrawLiquidity,
    },
  ],
  reduce: [
    {
      label: 'Trader profits paid or owed',
      tooltip: 'Marked trader profits reduce distributable LP value as liabilities even before they are paid. Funded payouts and trader claims remain senior to both vaults.',
      docsLink: DOCS_LINKS.lpReturns,
    },
    {
      label: 'VPI rebates funded by the pool',
      tooltip: 'When VPI favors the trader, the funded rebate is a liquidity-pool cost and can reduce Junior value through the waterfall.',
      docsLink: DOCS_LINKS.lpReturns,
    },
    {
      label: 'Senior targeted return',
      tooltip: 'The coupon reallocates available Junior principal to Senior. It is capped by what Junior can fund, but it can reduce Junior to zero.',
      docsLink: DOCS_LINKS.poolLiquidity,
    },
    {
      label: 'Liquidation shortfalls and bad debt',
      tooltip: 'Junior absorbs reconciled pool losses before Senior. Uncollectible marked trader losses that were never counted as LP value are written off rather than charged to Junior.',
      docsLink: DOCS_LINKS.lpReturns,
    },
    {
      label: 'Annual maintenance fee dilution',
      tooltip: 'The fee is paid by adding fee shares to effective Junior supply. This dilutes each existing holder without transferring USDC out of Junior principal.',
      docsLink: DOCS_LINKS.lpReturns,
    },
    {
      label: 'Oracle, contract, or stablecoin losses',
      tooltip: 'These failures reduce share value only when they cause a recognized loss. As the first-loss tranche, Junior absorbs that loss before Senior.',
      docsLink: DOCS_LINKS.lpRisks,
    },
  ],
}

const VAULT_GOVERNANCE_TIMELOCKS = [
  {
    mechanism: 'Pool risk settings',
    delay: '48 hours',
    effect: 'Changes to the Senior target rate, deposit limits, temporary pricing fees, and price-data requirements.',
  },
  {
    mechanism: 'Junior fee settings',
    delay: '48 hours',
    effect: 'Changes to the Junior annual fee or the wallet that receives it.',
  },
  {
    mechanism: 'Trading and pricing settings',
    delay: '48 hours',
    effect: 'Changes to market pricing inputs and trade processing that can affect hourly vault processing.',
  },
] as const

interface PoolSnapshot {
  longOpenInterest?: bigint
  shortOpenInterest?: bigint
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
  seniorRateBps?: bigint
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
  withdrawalCooldownEndsAt?: bigint
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

interface VaultActivityViewState {
  holders: VaultHolderDistribution[]
  activity: VaultOverviewActivityItem[]
  isLoading: boolean
  isError: boolean
  isStale?: boolean
}

interface VaultRequestsViewState {
  depositRequests: VaultDepositRequest[]
  redeemRequests: VaultRedeemRequest[]
  isLoading: boolean
  discoveryError: boolean
  discoveryStale: boolean
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
const EXPLORER_TX_BASE_URL = 'https://sepolia.arbiscan.io/tx'
const DEPOSIT_PROBE_ACCOUNT = '0x000000000000000000000000000000000000dEaD' as Address
const WAD = 10n ** 18n

const TRANCHES: Record<TrancheId, TrancheDefinition> = {
  senior: {
    id: 'senior',
    name: 'Senior Vault',
    token: 'psLP',
    eyebrow: 'More protected option',
    shortDescription: 'A targeted return with first priority when withdrawal funds are allocated.',
    description:
      'Senior gives up some upside for greater protection. It receives a targeted return funded by Junior, recovers prior losses before Junior receives new earnings, and takes losses only after Junior is exhausted.',
    returnModel: 'Targeted return funded by Junior',
    lossPriority: 'Second loss, after Junior',
    withdrawalPriority: 'Senior withdrawals are funded before Junior',
    upside: 'Targeted return and recovery priority',
    primaryRisk: 'The targeted return can stop and the vault can still lose value',
    riskLabel: 'Lower relative risk',
    riskVariant: 'info',
    targetReturn: 'Target return',
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
        text: 'Receives its targeted return when funds are available; prior losses are recovered before Junior receives new earnings',
      },
      {
        label: 'Withdrawals',
        text: 'Senior withdrawals are funded before Junior whenever USDC is available',
      },
    ],
    riskItems: [
      'The targeted return is not guaranteed and is limited by available Junior capital.',
      'Losses can reach Senior after Junior is fully exhausted.',
      'Amounts owed to traders are paid before either vault can withdraw.',
      'Positive share value does not guarantee immediate withdrawal liquidity.',
    ],
    address: PERPS_ARBITRUM_SEPOLIA.seniorVault,
  },
  junior: {
    id: 'junior',
    name: 'Junior Vault',
    token: 'pjLP',
    eyebrow: 'Higher-risk option',
    shortDescription: 'Takes losses first in exchange for more variable return potential.',
    description:
      'Junior funds the Senior targeted return and absorbs losses from the shared trading pool first. In exchange, it receives the trading earnings left after Senior is paid, including a share of fees from forced position closures.',
    returnModel: 'Variable return from trading activity',
    lossPriority: 'First loss',
    withdrawalPriority: 'Available after Senior withdrawals and the required safety buffer',
    upside: 'Variable return from trading activity',
    primaryRisk: 'Can lose some or all of its value before Senior begins taking losses',
    riskLabel: 'Higher relative risk',
    riskVariant: 'warning',
    targetReturn: 'Variable return',
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
        text: 'Receives remaining trading revenue and the vault share of liquidation charges after Senior obligations',
      },
      {
        label: 'Withdrawals',
        text: 'Junior withdrawals use the cash remaining after Senior withdrawals and the required safety buffer; they may take longer',
      },
    ],
    riskItems: [
      'Junior funds the Senior targeted return from its own capital.',
      'Junior absorbs realized losses from the shared trading pool before Senior is affected.',
      'Junior withdrawals can be unavailable even while its shares still have value.',
      'A sufficiently large loss can wipe out the vault completely.',
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
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.seniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'lastDepositTime',
        args: [readAccount],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.seniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'DEPOSIT_COOLDOWN',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.juniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'lastDepositTime',
        args: [readAccount],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.juniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'DEPOSIT_COOLDOWN',
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.housePool,
        abi: PERPS_HOUSE_POOL_ABI,
        functionName: 'seniorRateBps',
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
    const seniorLastDepositTime = asBigInt(readResult(results, 33))
    const seniorWithdrawalCooldown = asBigInt(readResult(results, 34))
    const juniorLastDepositTime = asBigInt(readResult(results, 35))
    const juniorWithdrawalCooldown = asBigInt(readResult(results, 36))
    const seniorRateBps = asBigInt(readResult(results, 37))
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
        longOpenInterest: bullOpenInterest,
        shortOpenInterest: bearOpenInterest,
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
        seniorRateBps,
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
          withdrawalCooldownEndsAt: seniorLastDepositTime !== undefined
            && seniorWithdrawalCooldown !== undefined
            ? seniorLastDepositTime + seniorWithdrawalCooldown
            : undefined,
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
          withdrawalCooldownEndsAt: juniorLastDepositTime !== undefined
            && juniorWithdrawalCooldown !== undefined
            ? juniorLastDepositTime + juniorWithdrawalCooldown
            : undefined,
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
    maximumFractionDigits: 3,
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
  if (amount >= 2n ** 255n) return 'No fixed limit'
  return formatFullUsd(amount)
}

function secondsUntilNextVaultEpoch(nowMs = Date.now()): number {
  const nowSeconds = Math.floor(nowMs / 1_000)
  const secondsIntoEpoch = nowSeconds % VAULT_EPOCH_DURATION_SECONDS
  return secondsIntoEpoch === 0
    ? VAULT_EPOCH_DURATION_SECONDS
    : VAULT_EPOCH_DURATION_SECONDS - secondsIntoEpoch
}

function formatEpochCountdown(totalSeconds: number): string {
  const minutes = Math.floor(totalSeconds / 60)
  const seconds = totalSeconds % 60
  return `${String(minutes).padStart(2, '0')}:${String(seconds).padStart(2, '0')}`
}

function secondsUntilTimestamp(timestamp: bigint | undefined): number {
  if (timestamp === undefined) return 0
  return Math.max(0, Number(timestamp) - Math.floor(Date.now() / 1_000))
}

function useWithdrawalCooldownRemaining(endsAt: bigint | undefined): number {
  const [remainingSeconds, setRemainingSeconds] = useState(() => secondsUntilTimestamp(endsAt))

  useEffect(() => {
    const update = () => {
      setRemainingSeconds(secondsUntilTimestamp(endsAt))
    }

    update()
    if (endsAt === undefined) return undefined

    const interval = window.setInterval(update, 1_000)
    return () => {
      window.clearInterval(interval)
    }
  }, [endsAt])

  return remainingSeconds
}

function WithdrawalCooldownCountdown({ remainingSeconds }: { remainingSeconds: number }) {
  return (
    <time
      dateTime={`PT${String(remainingSeconds)}S`}
      aria-label={`${String(remainingSeconds)} seconds until withdrawals are available`}
      className="font-mono font-semibold tabular-nums text-brand-peach"
    >
      {formatEpochCountdown(remainingSeconds)}
    </time>
  )
}

function formatShares(amount: bigint | undefined): string {
  if (amount === undefined) return '--'
  const value = Number(formatUnits(amount, SHARE_DECIMALS))
  return new Intl.NumberFormat('en-US', {
    maximumFractionDigits: 4,
  }).format(value)
}

function formatCompactShares(amount: bigint | undefined): string {
  if (amount === undefined) return '--'
  const value = Number(formatUnits(amount, SHARE_DECIMALS))
  return new Intl.NumberFormat('en-US', {
    notation: 'compact',
    maximumFractionDigits: 3,
  }).format(value)
}

function formatPositionValue(value: number | undefined): ReactNode {
  if (value === undefined) return '--'
  const exactAmount = value.toLocaleString('en-US', { maximumFractionDigits: 2 })
  const compactAmount = new Intl.NumberFormat('en-US', {
    notation: 'compact',
    maximumFractionDigits: 3,
  }).format(value)

  return (
    <span title={`${exactAmount} USDC`} aria-label={`${exactAmount} USDC`}>
      <TokenAmount amount={compactAmount} />
    </span>
  )
}

function formatPositionShares(amount: bigint | undefined, token: string): ReactNode {
  if (amount === undefined) return '--'
  const exactAmount = formatShares(amount)
  return (
    <span title={`${exactAmount} ${token}`} aria-label={`${exactAmount} ${token}`}>
      <TokenAmount amount={formatCompactShares(amount)} token={token} />
    </span>
  )
}

function formatSharePrice(value: number | undefined): ReactNode {
  if (value === undefined) return '--'
  return <TokenAmount amount={value.toFixed(4)} />
}

interface VaultChartPoint {
  timestamp: number
  blockNumber: string
  markFresh: boolean
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
      markFresh: point.markFresh,
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
  if (liveData.poolPaused === true) return 'Safety pause active'
  if (liveData.depositEnabled === false) return 'Deposits paused'
  if (liveData.maxRequestDeposit === undefined) {
    return 'Deposit status unavailable'
  }
  if (liveData.maxRequestDeposit > 0n) {
    return 'Open for deposits'
  }
  return 'No deposit capacity right now'
}

interface DepositUnavailableStatus {
  reason: string
  availability: string
}

const HOUR_MILLISECONDS = 60 * 60 * 1_000
const NEW_YORK_MARKET_TIME = new Intl.DateTimeFormat('en-US', {
  timeZone: 'America/New_York',
  weekday: 'short',
  hour: '2-digit',
  minute: '2-digit',
  hourCycle: 'h23',
})

function newYorkMarketParts(timestamp: number): {
  weekday: string
  hour: number
  minute: number
} {
  const parts = Object.fromEntries(
    NEW_YORK_MARKET_TIME.formatToParts(new Date(timestamp)).map((part) => [part.type, part.value])
  )
  return {
    weekday: parts.weekday,
    hour: Number(parts.hour),
    minute: Number(parts.minute),
  }
}

function scheduledOracleReopenTime(now = Date.now()): number | undefined {
  const current = newYorkMarketParts(now)
  const inRecurringWeekendClosure = current.weekday === 'Sat'
    || (current.weekday === 'Fri' && current.hour >= 17)
    || (current.weekday === 'Sun' && current.hour < 17)

  if (!inRecurringWeekendClosure) return undefined

  const firstWholeHour = Math.floor(now / HOUR_MILLISECONDS) * HOUR_MILLISECONDS
  for (let offset = 1; offset <= 72; offset += 1) {
    const candidate = firstWholeHour + offset * HOUR_MILLISECONDS
    const parts = newYorkMarketParts(candidate)
    if (parts.weekday === 'Sun' && parts.hour === 17 && parts.minute === 0) {
      return candidate
    }
  }

  return undefined
}

function formatReopenTime(timestamp: number): string {
  return new Intl.DateTimeFormat('en-US', {
    weekday: 'long',
    month: 'short',
    day: 'numeric',
    hour: 'numeric',
    minute: '2-digit',
    timeZoneName: 'short',
  }).format(new Date(timestamp))
}

function getDepositUnavailableStatus(
  tranche: TrancheDefinition,
  liveData: TrancheLiveData,
  pool: PoolSnapshot
): DepositUnavailableStatus {
  if (liveData.poolPaused === true) {
    return {
      reason: 'New deposits have been paused as a safety measure.',
      availability: 'There is no automatic reopening time. Deposits return when the safety pause is lifted.',
    }
  }

  if (pool.oracleFrozen === true) {
    const reopenTime = scheduledOracleReopenTime()
    return {
      reason: 'The live FX market is closed, so new vault shares cannot be priced safely.',
      availability: reopenTime === undefined
        ? 'After the market reopens and a fresh price is available.'
        : `After ${formatReopenTime(reopenTime)} and the first fresh live price is published.`,
    }
  }

  if ((pool.currentTerminalDeficitUsdc ?? 0n) > 0n) {
    return {
      reason: 'The shared trading pool has an unresolved shortfall, so it cannot accept new deposits.',
      availability: 'After the shortfall is resolved and balances are verified. There is no fixed reopening time.',
    }
  }

  if (pool.degradedMode === true) {
    return {
      reason: 'Extra safety restrictions are active, which blocks new vault deposits.',
      availability: 'After normal operation resumes and balances are verified. There is no fixed reopening time.',
    }
  }

  if (pool.markFresh === false) {
    return {
      reason: 'The latest market price is too old to price a new vault deposit safely.',
      availability: 'After fresh market pricing is available. This is normally automatic, but no exact time is guaranteed.',
    }
  }

  if (pool.seniorImpaired === true) {
    return {
      reason: 'The Senior vault has unrecovered losses, so new deposits are paused.',
      availability: 'After the Senior vault recovers those losses and balances are verified. There is no fixed reopening time.',
    }
  }

  if (tranche.id === 'senior' && liveData.maxRequestDeposit === 0n) {
    return {
      reason: 'The Senior vault has reached its current deposit limit.',
      availability: 'When existing capacity is released, more Junior capital is added, or the deposit limit is raised.',
    }
  }

  if (liveData.maxRequestDeposit === undefined || liveData.depositEnabled === undefined) {
    return {
      reason: 'The app cannot currently verify the vault\'s live deposit limit.',
      availability: 'After the live data connection recovers and the vault reports that deposits are open.',
    }
  }

  return {
    reason: 'The shared trading pool is not ready to accept new vault deposits.',
    availability: 'After its balances are verified and deposits reopen. There is no fixed reopening time.',
  }
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

function VaultEpochCountdown({
  fixedRemainingSeconds,
}: {
  fixedRemainingSeconds?: number
} = {}) {
  const [liveRemainingSeconds, setLiveRemainingSeconds] = useState(() => (
    secondsUntilNextVaultEpoch()
  ))

  useEffect(() => {
    if (fixedRemainingSeconds !== undefined) return undefined

    const interval = window.setInterval(() => {
      setLiveRemainingSeconds(secondsUntilNextVaultEpoch())
    }, 1_000)

    return () => {
      window.clearInterval(interval)
    }
  }, [fixedRemainingSeconds])

  const remainingSeconds = fixedRemainingSeconds ?? liveRemainingSeconds

  return (
    <time
      dateTime={`PT${String(remainingSeconds)}S`}
      aria-label={`${String(remainingSeconds)} seconds until the next hourly processing time`}
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
  const activeTooltipPosition = active
    ? chartTooltipPosition(active.x, active.y, width, height)
    : undefined
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
      {active && activeTooltipPosition ? (
        <div
          data-vault-chart-tooltip
          data-placement={activeTooltipPosition.placement}
          className="pointer-events-none absolute z-10 min-w-36 max-w-[calc(100%-1rem)] border border-brand-border/40 bg-app-bg px-2.5 py-2 shadow-xl"
          style={{
            left: activeTooltipPosition.left,
            top: activeTooltipPosition.top,
            transform: activeTooltipPosition.transform,
          }}
          role="status"
          aria-live="polite"
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
                title="The current total value of this vault, including gains and losses."
              >
                Vault value
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
                title="The current value of one vault share."
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
            <div className="contents">
              <dt className="pt-0.5 text-[10px] font-semibold uppercase tracking-[0.14em] text-content-secondary">
                Fee
              </dt>
              <dd className="text-sm leading-5 text-content-secondary">
                {tranche.id === 'senior'
                  ? 'Zero fees'
                  : liveData.maintenanceFeeAprBps === undefined
                    ? '--'
                    : `${(Number(liveData.maintenanceFeeAprBps) / 100).toFixed(2)}% annual maintenance fee, paid by issuing new shares`}
              </dd>
            </div>
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

const VAULT_ACTIVITY_PAGE_SIZE = 5

function AddressAvatar({ address }: { address: Address }) {
  const seed = Number.parseInt(address.slice(2, 8), 16)
  const colors = ['#FFAB96', '#FF5738', '#29F29A', '#FFE07D']
  const firstColor = colors[seed % colors.length]
  const secondColor = colors[Math.floor(seed / colors.length) % colors.length]

  return (
    <span
      aria-hidden="true"
      className="inline-block size-7 shrink-0 rounded-full border border-brand-border/40"
      style={{
        background: `conic-gradient(${firstColor} 0 25%, ${secondColor} 0 50%, ${firstColor} 0 75%, ${secondColor} 0)`,
      }}
    />
  )
}

function DistributionRing({ percentage }: { percentage: number }) {
  const boundedPercentage = Math.max(0, Math.min(100, percentage))
  return (
    <span
      aria-hidden="true"
      className="inline-flex size-7 shrink-0 items-center justify-center rounded-full"
      style={{
        background: `conic-gradient(#FFAB96 ${String(boundedPercentage)}%, rgba(255, 171, 150, 0.16) 0)`,
      }}
    >
      <span className="size-4 rounded-full bg-surface-panel" />
    </span>
  )
}

function ActivityPager({
  currentPage,
  pageCount,
  label,
  onPageChange,
}: {
  currentPage: number
  pageCount: number
  label: string
  onPageChange: (page: number) => void
}) {
  if (pageCount <= 1) return null
  return (
    <nav className="flex items-center justify-center gap-3 border-t border-brand-border/20 px-4 py-3" aria-label={label}>
      <button
        type="button"
        className="inline-flex size-9 items-center justify-center border border-brand-border/35 text-content-primary transition-colors hover:border-brand-peach hover:text-brand-peach disabled:cursor-not-allowed disabled:opacity-35"
        disabled={currentPage === 0}
        aria-label="Previous page"
        onClick={() => { onPageChange(currentPage - 1) }}
      >
        <span className="material-symbols-outlined text-lg">arrow_back</span>
      </button>
      <span className="min-w-20 text-center text-xs text-content-secondary">
        {currentPage + 1} of {pageCount}
      </span>
      <button
        type="button"
        className="inline-flex size-9 items-center justify-center border border-brand-border/35 text-content-primary transition-colors hover:border-brand-peach hover:text-brand-peach disabled:cursor-not-allowed disabled:opacity-35"
        disabled={currentPage >= pageCount - 1}
        aria-label="Next page"
        onClick={() => { onPageChange(currentPage + 1) }}
      >
        <span className="material-symbols-outlined text-lg">arrow_forward</span>
      </button>
    </nav>
  )
}

function formatActivityDate(timestamp: string): string {
  const date = new Date(timestamp)
  if (Number.isNaN(date.getTime())) return '--'
  return new Intl.DateTimeFormat('en-US', {
    month: 'short',
    day: 'numeric',
    year: 'numeric',
    hour: 'numeric',
    minute: '2-digit',
  }).format(date)
}

function activityAmount(activity: VaultOverviewActivityItem): ReactNode {
  if (activity.amountUsdc !== undefined) {
    const amount = formatFullUsdc(activity.amountUsdc, 2)
    return (
      <span
        title={activity.amountIsEstimate ? 'Current USDC estimate; the final value is set when this withdrawal is processed.' : undefined}
        className="inline-flex items-baseline gap-1"
      >
        {activity.amountIsEstimate ? <span aria-hidden="true">≈</span> : null}
        <TokenAmount amount={amount} />
      </span>
    )
  }
  if (activity.shares !== undefined) {
    return <TokenAmount amount={formatShares(activity.shares)} token={activity.tranche === 'senior' ? 'psLP' : 'pjLP'} />
  }
  return '--'
}

function ActivityTypeLabel({ activity }: { activity: VaultOverviewActivityItem }) {
  const isDeposit = activity.kind === 'deposit'
  return (
    <span className="inline-flex items-center gap-2 font-medium text-content-primary">
      <span className={`material-symbols-outlined text-lg ${isDeposit ? 'text-positive' : 'text-brand-orange'}`}>
        {isDeposit ? 'south_west' : 'north_east'}
      </span>
      {isDeposit ? 'Deposit submitted' : 'Withdrawal submitted'}
    </span>
  )
}

function VaultActivitySection({
  holders,
  activity,
  tranche,
  scrollMarginTop,
  isLoading,
  isError,
  isStale,
}: {
  holders: VaultHolderDistribution[]
  activity: VaultOverviewActivityItem[]
  tranche: VaultActivityTranche
  scrollMarginTop?: number
  isLoading: boolean
  isError: boolean
  isStale?: boolean
}) {
  const [holderPage, setHolderPage] = useState(0)
  const [activityPage, setActivityPage] = useState(0)
  const trancheName = tranche === 'senior' ? 'Senior' : 'Junior'
  const scopedHolders = useMemo(() => {
    const attributedHolders = holders.flatMap((holder) => {
      const currentNavUsdc = tranche === 'senior'
        ? holder.seniorNavUsdc
        : holder.juniorNavUsdc
      const attributedPercentage = tranche === 'senior'
        ? holder.seniorShareOfAttributedValue
        : holder.juniorShareOfAttributedValue
      return currentNavUsdc > 0n ? [{ ...holder, currentNavUsdc, attributedPercentage }] : []
    })
    const attributedNavUsdc = attributedHolders.reduce(
      (total, holder) => total + holder.currentNavUsdc,
      0n,
    )

    return attributedHolders.map((holder) => ({
      ...holder,
      shareOfVaultNav: holder.attributedPercentage ?? (
        attributedNavUsdc > 0n
          ? Number(holder.currentNavUsdc * 1_000_000n / attributedNavUsdc) / 10_000
          : 0
      ),
    })).sort((left, right) => (
      left.currentNavUsdc > right.currentNavUsdc
        ? -1
        : left.currentNavUsdc < right.currentNavUsdc ? 1 : 0
    ))
  }, [holders, tranche])
  const scopedActivity = useMemo(() => (
    activity.filter((item) => item.tranche === tranche)
  ), [activity, tranche])
  const holderPageCount = Math.max(1, Math.ceil(scopedHolders.length / VAULT_ACTIVITY_PAGE_SIZE))
  const safeHolderPage = Math.min(holderPage, holderPageCount - 1)
  const pagedHolders = scopedHolders.slice(
    safeHolderPage * VAULT_ACTIVITY_PAGE_SIZE,
    (safeHolderPage + 1) * VAULT_ACTIVITY_PAGE_SIZE,
  )
  const activityPageCount = Math.max(1, Math.ceil(scopedActivity.length / VAULT_ACTIVITY_PAGE_SIZE))
  const safeActivityPage = Math.min(activityPage, activityPageCount - 1)
  const pagedActivity = scopedActivity.slice(
    safeActivityPage * VAULT_ACTIVITY_PAGE_SIZE,
    (safeActivityPage + 1) * VAULT_ACTIVITY_PAGE_SIZE,
  )

  return (
    <section
      id="activity"
      data-vault-detail-section="activity"
      aria-labelledby="vault-activity-heading"
      style={{ scrollMarginTop }}
    >
      <div className="mb-4">
        <p className="text-xs font-semibold uppercase tracking-[0.18em] text-content-secondary">
          Vault activity
        </p>
        <h2 id="vault-activity-heading" className="mt-1 text-2xl font-semibold text-content-primary">
          Holders and recent activity
        </h2>
      </div>

      <div className="space-y-6">
        {isStale ? (
          <p className="border border-brand-orange/40 bg-brand-orange/10 px-4 py-3 text-sm text-content-secondary">
            Vault activity is temporarily stale. The last confirmed holder and request data remains visible while the backend catches up.
          </p>
        ) : null}
        <div className="border border-brand-border/30 bg-surface-panel">
          <div className="flex flex-col gap-2 border-b border-brand-border/25 p-5 sm:flex-row sm:items-start sm:justify-between">
            <div>
              <h3 className="text-lg font-semibold text-content-primary">Holder distribution</h3>
              <p className="mt-1 max-w-2xl text-sm leading-5 text-content-secondary">
                Share of the {trancheName} Vault represented by shares held in each wallet or
                attributed to its deposit and withdrawal requests. Pending and refundable redeem
                shares remain attributed until they become an asset claim; deposits awaiting
                settlement and refundable deposit assets are not included.
              </p>
            </div>
            <span className="text-xs text-content-secondary">Largest wallet positions</span>
          </div>

          {isLoading && scopedHolders.length === 0 ? (
            <div className="flex min-h-36 items-center justify-center"><Spinner /></div>
          ) : scopedHolders.length === 0 ? (
            <p className="p-6 text-sm text-content-secondary">
              {isError ? 'Holder data is temporarily unavailable.' : 'No attributed vault share positions are available yet.'}
            </p>
          ) : (
            <>
              <div className="hidden overflow-x-auto md:block">
                <table className="w-full min-w-[44rem] border-collapse text-left">
                  <caption className="sr-only">Vault holder distribution</caption>
                  <thead className="text-xs uppercase tracking-[0.12em] text-content-secondary">
                    <tr>
                      <th scope="col" className="px-5 py-3 font-medium">Holder</th>
                      <th scope="col" className="px-5 py-3 font-medium">Current value</th>
                      <th scope="col" className="px-5 py-3 font-medium">% of attributed value</th>
                    </tr>
                  </thead>
                  <tbody>
                    {pagedHolders.map((holder) => (
                      <tr key={holder.address} className="border-t border-brand-border/20">
                        <td className="px-5 py-4">
                          <a
                            href={`${EXPLORER_BASE_URL}/${holder.address}`}
                            target="_blank"
                            rel="noreferrer"
                            className="group inline-flex items-center gap-3 text-sm font-medium text-content-primary hover:text-brand-peach"
                          >
                            <AddressAvatar address={holder.address} />
                            <span>
                              <span className="group-hover:underline group-hover:underline-offset-4">{formatAddress(holder.address)}</span>
                            </span>
                          </a>
                        </td>
                        <td className="px-5 py-4 text-sm font-semibold text-content-primary">
                          <TokenAmount amount={formatFullUsdc(holder.currentNavUsdc, 2)} />
                        </td>
                        <td className="px-5 py-4">
                          <span className="inline-flex items-center gap-3 text-sm font-semibold text-content-primary">
                            <DistributionRing percentage={holder.shareOfVaultNav} />
                            {holder.shareOfVaultNav.toFixed(2)}%
                          </span>
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>

              <ul className="divide-y divide-brand-border/20 md:hidden">
                {pagedHolders.map((holder) => (
                  <li key={holder.address} className="space-y-4 p-5">
                    <a
                      href={`${EXPLORER_BASE_URL}/${holder.address}`}
                      target="_blank"
                      rel="noreferrer"
                      className="inline-flex items-center gap-3 text-sm font-medium text-content-primary hover:text-brand-peach hover:underline hover:underline-offset-4"
                    >
                      <AddressAvatar address={holder.address} />
                      {formatAddress(holder.address)}
                    </a>
                    <div className="flex items-end justify-between gap-4">
                      <div>
                        <p className="text-[10px] uppercase tracking-[0.12em] text-content-secondary">Current value</p>
                        <p className="mt-1 font-semibold text-content-primary"><TokenAmount amount={formatFullUsdc(holder.currentNavUsdc, 2)} /></p>
                      </div>
                      <span className="inline-flex items-center gap-2 text-sm font-semibold text-content-primary">
                        <DistributionRing percentage={holder.shareOfVaultNav} />
                        {holder.shareOfVaultNav.toFixed(2)}%
                      </span>
                    </div>
                  </li>
                ))}
              </ul>
              <ActivityPager
                currentPage={safeHolderPage}
                pageCount={holderPageCount}
                label="Holder distribution pages"
                onPageChange={setHolderPage}
              />
            </>
          )}
        </div>

        <div className="border border-brand-border/30 bg-surface-panel">
          <div className="flex flex-col gap-4 border-b border-brand-border/25 p-5 sm:flex-row sm:items-center sm:justify-between">
            <div>
              <h3 className="text-lg font-semibold text-content-primary">Recent deposits and withdrawals</h3>
              <p className="mt-1 text-sm text-content-secondary">The latest activity submitted to the {trancheName} Vault.</p>
            </div>
            <span className="inline-flex items-center gap-2 text-xs text-content-secondary">
              <span className="material-symbols-outlined text-base">calendar_today</span>
              Latest submissions
            </span>
          </div>

          {isLoading && scopedActivity.length === 0 ? (
            <div className="flex min-h-36 items-center justify-center"><Spinner /></div>
          ) : pagedActivity.length === 0 ? (
            <p className="p-6 text-sm text-content-secondary">
              {isError ? 'Recent activity is temporarily unavailable.' : `No ${trancheName} Vault activity found yet.`}
            </p>
          ) : (
            <>
              <div className="hidden overflow-x-auto lg:block">
                <table className="w-full min-w-[62rem] border-collapse text-left">
                  <caption className="sr-only">Recent vault activity</caption>
                  <thead className="text-xs uppercase tracking-[0.12em] text-content-secondary">
                    <tr>
                      <th scope="col" className="px-5 py-3 font-medium">Date</th>
                      <th scope="col" className="px-5 py-3 font-medium">Type</th>
                      <th scope="col" className="px-5 py-3 font-medium">Amount</th>
                      <th scope="col" className="px-5 py-3 font-medium">User</th>
                      <th scope="col" className="px-5 py-3 font-medium">Transaction</th>
                    </tr>
                  </thead>
                  <tbody>
                    {pagedActivity.map((item) => (
                      <tr key={item.id} className="border-t border-brand-border/20 text-sm">
                        <td className="whitespace-nowrap px-5 py-4 text-content-secondary">{formatActivityDate(item.timestamp)}</td>
                        <td className="whitespace-nowrap px-5 py-4"><ActivityTypeLabel activity={item} /></td>
                        <td className="whitespace-nowrap px-5 py-4 font-semibold text-content-primary">{activityAmount(item)}</td>
                        <td className="px-5 py-4">
                          <a
                            href={`${EXPLORER_BASE_URL}/${item.account}`}
                            target="_blank"
                            rel="noreferrer"
                            className="inline-flex items-center gap-2 text-content-primary hover:text-brand-peach hover:underline hover:underline-offset-4"
                          >
                            <AddressAvatar address={item.account} />
                            {formatAddress(item.account)}
                          </a>
                        </td>
                        <td className="px-5 py-4">
                          <a
                            href={`${EXPLORER_TX_BASE_URL}/${item.transactionHash}`}
                            target="_blank"
                            rel="noreferrer"
                            className="inline-flex items-center gap-1 text-content-primary hover:text-brand-peach hover:underline hover:underline-offset-4"
                          >
                            {item.transactionHash.slice(0, 6)}…{item.transactionHash.slice(-4)}
                            <span className="material-symbols-outlined text-base">open_in_new</span>
                          </a>
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>

              <ul className="divide-y divide-brand-border/20 lg:hidden">
                {pagedActivity.map((item) => (
                  <li key={item.id} className="space-y-4 p-5">
                    <div className="flex items-start justify-between gap-4">
                      <ActivityTypeLabel activity={item} />
                    </div>
                    <div className="flex items-baseline justify-between gap-4">
                      <span className="text-xs text-content-secondary">Amount</span>
                      <span className="font-semibold text-content-primary">{activityAmount(item)}</span>
                    </div>
                    <div className="flex items-center justify-between gap-4 border-t border-brand-border/20 pt-3 text-xs">
                      <span className="text-content-secondary">{formatActivityDate(item.timestamp)}</span>
                      <a
                        href={`${EXPLORER_TX_BASE_URL}/${item.transactionHash}`}
                        target="_blank"
                        rel="noreferrer"
                        className="inline-flex items-center gap-1 text-content-primary hover:text-brand-peach hover:underline hover:underline-offset-4"
                      >
                        View transaction
                        <span className="material-symbols-outlined text-base">open_in_new</span>
                      </a>
                    </div>
                  </li>
                ))}
              </ul>
              <ActivityPager
                currentPage={safeActivityPage}
                pageCount={activityPageCount}
            label="Recent activity pages"
                onPageChange={setActivityPage}
              />
            </>
          )}
        </div>
      </div>
    </section>
  )
}

export function VaultsOverview({
  snapshot,
  history,
  epochCountdownSeconds,
}: {
  snapshot: VaultsSnapshot
  history?: VaultHistory
  epochCountdownSeconds?: number
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
              Plether vaults
            </p>
            <h1 className="mt-3 text-3xl font-semibold tracking-tight text-content-primary sm:text-4xl">
              Provide liquidity that keeps the market running.
            </h1>
            <p className="mt-3 max-w-2xl text-base leading-7 text-content-secondary">
              Deposit USDC into the Senior or Junior Vault. Both supply the same trading pool, but
              they take different positions when profits, losses, and withdrawal funds are shared.
            </p>
          </div>

          <a
            href="https://docs.plether.com/get-started/liquidity-provider-quickstart"
            target="_blank"
            rel="noreferrer"
            className="group inline-flex self-start items-center gap-2 border border-brand-border/40 px-4 py-2 text-sm font-semibold text-content-primary transition-colors hover:border-brand-peach hover:text-brand-peach"
          >
            <span className="group-hover:underline group-hover:underline-offset-4">
              Learn how the vaults work
            </span>
            <span className="material-symbols-outlined text-lg">open_in_new</span>
          </a>
        </div>

        <dl className="grid grid-cols-1 gap-x-4 gap-y-6 p-6 sm:grid-cols-2 lg:grid-cols-4">
          <PoolStat
            label="Total pool funds"
            value={formatCompactUsd(pool.totalAssetsUsdc)}
            tooltip="The total USDC currently held by the shared trading pool."
            stackedOnMobile
          />
          <PoolStat
            label="Reserved funds"
            value={formatCompactUsd(pool.withdrawalReservedUsdc)}
            subvalue="Set aside for trader payouts"
            tooltip="USDC reserved for trader withdrawals and other protected payments."
            stackedOnMobile
          />
          <PoolStat
            label="Available liquidity"
            value={formatCompactUsd(pool.freeUsdc)}
            subvalue={freeLiquidityRatio === undefined ? 'Live value unavailable' : `${freeLiquidityRatio.toFixed(1)}% of pool funds`}
            tooltip="USDC available after amounts reserved for trader withdrawals and other protected payments."
            stackedOnMobile
            startsTabletRow
          />
          <PoolStat
            label="Next processing time in"
            value={<VaultEpochCountdown fixedRemainingSeconds={epochCountdownSeconds} />}
            subvalue="Deposits and withdrawals submitted during the final five minutes are processed the following hour."
            tooltip="Deposits and withdrawals are processed on the hour. Submit at least five minutes beforehand to join that processing time."
            stackedOnMobile
          />
        </dl>
      </section>

      {snapshot.tranches.senior.lpEpochSettlementPaused === true
        || snapshot.tranches.junior.lpEpochSettlementPaused === true ? (
          <Alert variant="warning" title="Hourly processing paused">
            You can still submit deposits or withdrawals, move ready funds to your wallet, cancel
            pending activity, and return available funds or shares. New deposits will not start
            earning and withdrawals will not receive new funds until hourly processing resumes.
          </Alert>
        ) : null}

      <section>
        <div className="mb-4">
          <p className="text-xs font-semibold uppercase tracking-[0.18em] text-content-secondary">
            Choose a vault
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
            Shared pool liquidity
          </p>
          <h2
            id="pool-liquidity-heading"
            className="mt-1 text-2xl font-semibold text-content-primary"
          >
            Trading capacity and loss protection
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

function RequestMetric({
  label,
  value,
  tone = 'default',
}: {
  label: string
  value: ReactNode
  tone?: 'default' | 'positive' | 'warning'
}) {
  const valueClassName = tone === 'positive'
    ? 'text-positive'
    : tone === 'warning'
      ? 'text-warning'
      : 'text-content-primary'

  return (
    <div className="min-w-0 bg-app-bg p-4">
      <dt className="text-[10px] font-semibold uppercase tracking-[0.14em] text-content-secondary">
        {label}
      </dt>
      <dd className={`mt-1 truncate text-base font-semibold ${valueClassName}`} title={typeof value === 'string' ? value : undefined}>
        {value}
      </dd>
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

export function OverviewTab({
  tranche,
  liveData,
  snapshot,
  isConnected,
  epochCountdownSeconds,
}: {
  tranche: TrancheDefinition
  liveData: TrancheLiveData
  snapshot: VaultsSnapshot
  isConnected: boolean
  epochCountdownSeconds?: number
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
    ? 'Pool shortfall'
    : depositMode
  const withdrawalCooldownRemaining = useWithdrawalCooldownRemaining(
    liveData.withdrawalCooldownEndsAt
  )
  const withdrawalCooldownActive = isConnected
    && (liveData.userShares ?? 0n) > 0n
    && withdrawalCooldownRemaining > 0

  return (
    <div className="space-y-6">
      <div className="grid gap-3 sm:grid-cols-2 xl:grid-cols-4">
        <DetailMetric
          label="Your position"
          value={isConnected && positionValue !== undefined
            ? formatPositionValue(positionValue)
            : '--'}
          detail={isConnected ? formatPositionShares(liveData.userShares, tranche.token) : 'Connect a wallet to view'}
        />
        <DetailMetric
          label="Shares available to withdraw"
          value={isConnected
            ? formatPositionShares(liveData.maxRequestRedeem, tranche.token)
            : '--'}
          detail={withdrawalCooldownActive
            ? (
              <span>
                Available in{' '}
                <WithdrawalCooldownCountdown remainingSeconds={withdrawalCooldownRemaining} />
              </span>
            )
            : 'Shares currently available to withdraw'}
          tone={(liveData.maxRequestRedeem ?? 0n) > 0n ? 'positive' : 'default'}
        />
        <DetailMetric
          label="Available withdrawal liquidity"
          value={formatCompactUsd(poolWithdrawCap)}
          detail={tranche.id === 'senior'
            ? 'The most Senior withdrawals that can be funded at the next processing time'
            : 'The most Junior withdrawals that can be funded after Senior priority'}
        />
        <DetailMetric
          label="Deposit availability"
          value={depositState}
          detail={depositMode === 'Open for deposits'
            ? (
              <span>
                Current hourly window ends in{' '}
                <VaultEpochCountdown fixedRemainingSeconds={epochCountdownSeconds} />
              </span>
            )
            : 'New deposits are not available right now'}
          tone={depositState === 'Pool shortfall' ? 'negative' : 'warning'}
        />
      </div>

      <div className="grid gap-x-3 gap-y-6 xl:grid-cols-2">
        <section className="border border-brand-border/30 bg-surface-panel p-5">
          <h3 className="text-lg font-semibold text-content-primary">How this vault works</h3>
          <dl className="mt-3">
            <DetailRow label="Asset" value="USDC" />
            <DetailRow label="Vault share symbol" value={<TokenLabel token={tranche.token} />} />
            <DetailRow label="Processing" value="Every hour" />
            <DetailRow label="Network" value="Arbitrum Sepolia" />
            <DetailRow label="Deposits" value={depositMode} />
            <DetailRow label="Submission deadline" value="5 minutes before each hour" />
            <DetailRow
              label="Next processing time"
              value={liveData.nextRequestEpoch === undefined
                ? 'Unavailable'
                : new Date(Number(liveData.nextRequestEpoch * 3_600n) * 1_000).toLocaleString()}
            />
            <DetailRow
              label={(
                <span>
                  <span className="block">Temporary pricing fee</span>
                  <span className="mt-1 block max-w-md text-xs leading-5 text-content-secondary">
                    {pool.oracleFrozen === true
                      ? 'Active while live market pricing is unavailable. Wait for pricing to resume before withdrawing when possible.'
                      : 'Charged only when live market pricing is temporarily unavailable.'}
                  </span>
                </span>
              )}
              value={liveData.frozenLpFeeBps === undefined
                ? 'Unavailable'
                : pool.oracleFrozen === true
                  ? `${(Number(liveData.frozenLpFeeBps) / 100).toFixed(2)}% active`
                  : `Inactive · ${(Number(liveData.frozenLpFeeBps) / 100).toFixed(2)}%`}
              valueClassName={pool.oracleFrozen === true ? 'text-brand-orange' : 'text-content-primary'}
            />
            {tranche.id === 'junior' ? (
              <>
                <DetailRow
                  label="Annual vault fee"
                  value={liveData.maintenanceFeeAprBps === undefined
                    ? 'Unavailable'
                    : `${(Number(liveData.maintenanceFeeAprBps) / 100).toFixed(2)}%`}
                />
                <DetailRow
                  label="Accrued fee shares"
                  value={liveData.pendingMaintenanceFeeShares === undefined
                    ? 'Unavailable'
                    : (
                      <TokenAmount
                        amount={formatShares(liveData.pendingMaintenanceFeeShares)}
                        token={tranche.token}
                      />
                    )}
                />
                <DetailRow
                  label="Fee recipient"
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
                  label="Target nominal APR"
                  value={pool.seniorRateBps === undefined
                    ? 'Unavailable'
                    : `${(Number(pool.seniorRateBps) / 100).toFixed(2)}%`}
                />
                <DetailRow
                  label="Remaining Senior capacity"
                  value={formatVaultLimit(pool.seniorDepositCapacityUsdc)}
                />
                <DetailRow
                  label="Maximum Senior value"
                  value={formatVaultLimit(pool.maxSeniorExposureUsdc)}
                />
                <DetailRow
                  label="Maximum Senior share of pool capital"
                  value={pool.maxSeniorShareBps === undefined
                    ? 'Unavailable'
                    : `${(Number(pool.maxSeniorShareBps) / 100).toFixed(2)}%`}
                />
                <DetailRow
                  label="Amount reserved for pending deposits"
                  value={formatFullUsd(pool.reservedSeniorDepositAssetsUsdc)}
                />
                <DetailRow
                  label="Pending deposits within current limits"
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
              label="Deposits past their expected processing time"
              value={liveData.depositBacklog === undefined
                ? 'Unavailable'
                : liveData.depositBacklog ? 'Yes' : 'No'}
              valueClassName={liveData.depositBacklog === undefined
                ? 'text-content-secondary'
                : liveData.depositBacklog ? 'text-warning' : 'text-positive'}
            />
            <DetailRow
              label="Withdrawals past their expected processing time"
              value={liveData.redeemBacklog === undefined
                ? 'Unavailable'
                : liveData.redeemBacklog ? 'Yes' : 'No'}
              valueClassName={liveData.redeemBacklog === undefined
                ? 'text-content-secondary'
                : liveData.redeemBacklog ? 'text-warning' : 'text-positive'}
            />
            <DetailRow
              label="Vault address"
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
          <h3 className="text-lg font-semibold text-content-primary">Shared pool status</h3>
          <dl className="mt-3">
            <DetailRow label="Total pool funds" value={formatFullUsd(pool.totalAssetsUsdc, 0)} />
            <DetailRow label="Available liquidity" value={formatFullUsd(pool.freeUsdc, 0)} />
            <DetailRow label="Reserved for trader withdrawals" value={formatFullUsd(pool.withdrawalReservedUsdc, 0)} />
            <DetailRow label="Trading revenue awaiting distribution" value={formatFullUsd(pool.pendingTradingRevenueUsdc)} />
            <DetailRow label="Funds awaiting loss recovery" value={formatFullUsd(pool.pendingRecapitalizationUsdc)} />
            <DetailRow
              label="Unresolved pool shortfall"
              value={formatFullUsd(pool.currentTerminalDeficitUsdc)}
              valueClassName={(pool.currentTerminalDeficitUsdc ?? 0n) > 0n
                ? 'text-brand-orange'
                : 'text-positive'}
            />
            <DetailRow
              label="Market price"
              value={pool.markFresh === undefined ? 'Unavailable' : pool.markFresh ? 'Up to date' : 'Out of date'}
              valueClassName={pool.markFresh === undefined ? 'text-content-secondary' : pool.markFresh ? 'text-positive' : 'text-brand-orange'}
            />
            <DetailRow
              label="Live pricing available"
              value={pool.oracleFrozen === undefined ? 'Unavailable' : pool.oracleFrozen ? 'No' : 'Yes'}
              valueClassName={pool.oracleFrozen === undefined ? 'text-content-secondary' : pool.oracleFrozen ? 'text-warning' : 'text-positive'}
            />
            <DetailRow
              label="Safety restrictions"
              value={pool.degradedMode === undefined ? 'Unavailable' : pool.degradedMode ? 'Active' : 'None'}
              valueClassName={pool.degradedMode === undefined ? 'text-content-secondary' : pool.degradedMode ? 'text-brand-orange' : 'text-positive'}
            />
            <DetailRow
              label="New deposits paused"
              value={liveData.poolPaused === undefined ? 'Unavailable' : liveData.poolPaused ? 'Yes' : 'No'}
              valueClassName={liveData.poolPaused === undefined
                ? 'text-content-secondary'
                : liveData.poolPaused ? 'text-warning' : 'text-positive'}
            />
            <DetailRow
              label="Hourly processing paused"
              value={liveData.lpEpochSettlementPaused === undefined
                ? 'Unavailable'
                : liveData.lpEpochSettlementPaused ? 'Yes' : 'No'}
              valueClassName={liveData.lpEpochSettlementPaused === undefined
                ? 'text-content-secondary'
                : liveData.lpEpochSettlementPaused ? 'text-warning' : 'text-positive'}
            />
            <DetailRow
              label="New withdrawal funding"
              value={liveData.settlementLive === undefined
                ? 'Unavailable'
                : liveData.settlementLive ? 'Available' : 'Waiting'}
              valueClassName={liveData.settlementLive === undefined
                ? 'text-content-secondary'
                : liveData.settlementLive ? 'text-positive' : 'text-warning'}
            />
          </dl>
        </section>
      </div>

      {tranche.id === 'senior' ? (
        <section className="border border-brand-border/30 bg-surface-panel p-5">
          <div>
            <p className="text-xs font-semibold uppercase tracking-[0.14em] text-brand-peach">
              Senior protection
            </p>
            <h3 className="mt-1 text-lg font-semibold text-content-primary">Protected balance</h3>
            <p className="mt-2 max-w-2xl text-sm leading-6 text-content-secondary">
              Earned returns and recovered losses increase Senior's protected balance. If the
              vault falls below it, future pool revenue fills the gap before Junior receives new
              earnings.
            </p>
          </div>
          <div className="mt-5 grid gap-3 sm:grid-cols-3">
            <DetailMetric label="Current Senior capital" value={formatCompactUsd(pool.seniorPrincipalUsdc)} />
            <DetailMetric label="Protected balance" value={formatCompactUsd(pool.seniorHighWaterMarkUsdc)} />
            <DetailMetric
              label="Amount still to recover"
              value={formatCompactUsd(pool.seniorImpairmentGapUsdc)}
              tone={pool.seniorImpaired === true ? 'negative' : pool.seniorImpaired === false ? 'positive' : 'default'}
            />
          </div>
        </section>
      ) : (
        <section className="border border-brand-border/30 bg-surface-panel p-5">
          <div>
            <p className="text-xs font-semibold uppercase tracking-[0.14em] text-positive">
              Senior loss protection
            </p>
            <h3 className="mt-1 text-lg font-semibold text-content-primary">Junior loss buffer</h3>
            <p className="mt-2 max-w-2xl text-sm leading-6 text-content-secondary">
              Junior capital protects Senior. It funds the Senior targeted return and absorbs
              realized losses before the Senior Vault loses value.
            </p>
          </div>
          <div className="mt-5 grid gap-3 sm:grid-cols-3">
            <DetailMetric label="Junior capital" value={formatCompactUsd(pool.juniorPrincipalUsdc)} />
            <DetailMetric label="Senior priority balance" value={formatCompactUsd(pool.seniorPrincipalUsdc)} />
            <DetailMetric
              label="Available for Junior withdrawals"
              value={formatCompactUsd(pool.juniorPoolWithdrawCapUsdc)}
              tone={(pool.juniorPoolWithdrawCapUsdc ?? 0n) > 0n ? 'positive' : 'warning'}
            />
          </div>
        </section>
      )}

      <section className="border border-brand-border/30 bg-surface-panel">
        <div className="border-b border-brand-border/25 p-5">
          <p className="text-xs font-semibold uppercase tracking-[0.14em] text-content-secondary">
            Change safeguards
          </p>
          <h3
            id={`vault-timelocks-${tranche.id}`}
            className="mt-1 text-lg font-semibold text-content-primary"
          >
            Delayed settings changes
          </h3>
          <p className="mt-2 max-w-3xl text-sm leading-6 text-content-secondary">
            Important rules for this vault cannot change immediately. Every change below must be
            announced 48 hours before it can take effect.
          </p>
        </div>
        <div className="overflow-x-auto">
          <table
            aria-labelledby={`vault-timelocks-${tranche.id}`}
            className="w-full min-w-[480px] text-left"
          >
            <thead className="bg-app-bg text-[10px] font-semibold uppercase tracking-[0.14em] text-content-secondary">
              <tr>
                <th scope="col" className="px-5 py-3">Setting</th>
                <th scope="col" className="px-5 py-3">Notice period</th>
              </tr>
            </thead>
            <tbody className="divide-y divide-brand-border/20">
              {VAULT_GOVERNANCE_TIMELOCKS.map((timelock) => (
                <tr key={timelock.mechanism}>
                  <th scope="row" className="px-5 py-4 text-left">
                    <span className="block text-sm font-semibold text-content-primary">
                      {timelock.mechanism}
                    </span>
                    <span className="mt-1 block max-w-3xl text-sm font-normal leading-6 text-content-secondary">
                      {timelock.effect}
                    </span>
                  </th>
                  <td className="w-36 whitespace-nowrap px-5 py-4 align-top font-mono text-sm font-semibold text-brand-peach">
                    {timelock.delay}
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
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

type ChartTooltipPlacement = 'above' | 'below' | 'left' | 'right'

function chartViewportTransform(
  renderedWidth: number,
  renderedHeight: number,
  viewBoxWidth: number,
  viewBoxHeight: number,
): { scale: number; xInset: number; yInset: number } {
  const scale = Math.min(renderedWidth / viewBoxWidth, renderedHeight / viewBoxHeight)
  return {
    scale,
    xInset: (renderedWidth - viewBoxWidth * scale) / 2,
    yInset: (renderedHeight - viewBoxHeight * scale) / 2,
  }
}

function chartTooltipPosition(
  x: number,
  y: number,
  width: number,
  height: number,
): { placement: ChartTooltipPlacement; left: string; top: string; transform: string } {
  const horizontalPosition = x / width
  const verticalPosition = y / height
  const left = `${String(horizontalPosition * 100)}%`
  const top = `${String(verticalPosition * 100)}%`

  if (horizontalPosition <= 0.34) {
    return {
      placement: 'right',
      left,
      top,
      transform: `translate(12px, ${verticalPosition <= 0.5 ? '0' : '-100%'})`,
    }
  }

  if (horizontalPosition >= 0.66) {
    return {
      placement: 'left',
      left,
      top,
      transform: `translate(calc(-100% - 12px), ${verticalPosition <= 0.5 ? '0' : '-100%'})`,
    }
  }

  return verticalPosition <= 0.5
    ? {
        placement: 'below',
        left,
        top,
        transform: 'translate(-50%, 12px)',
      }
    : {
        placement: 'above',
        left,
        top,
        transform: 'translate(-50%, calc(-100% - 12px))',
      }
}

function PerformanceChart({
  tranche,
  performance,
}: {
  tranche: TrancheDefinition
  performance: CompleteVaultPerformance
}) {
  const [activeIndex, setActiveIndex] = useState<number | null>(null)
  const [tooltipAnchor, setTooltipAnchor] = useState<{
    activeIndex: number
    x: number
    y: number
    width: number
    height: number
  } | null>(null)
  const chartContainerRef = useRef<HTMLDivElement>(null)
  const chartSvgRef = useRef<SVGSVGElement>(null)
  const height = 240
  const [width, setWidth] = useState(640)
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
  const activeX = active?.x
  const activeY = active?.y
  const measuredTooltipAnchor = tooltipAnchor?.activeIndex === activeIndex
    ? tooltipAnchor
    : undefined
  const activeTooltipPosition = active
    ? chartTooltipPosition(
        measuredTooltipAnchor?.x ?? active.x,
        measuredTooltipAnchor?.y ?? active.y,
        measuredTooltipAnchor?.width ?? width,
        measuredTooltipAnchor?.height ?? height,
      )
    : undefined
  const startingPrice = performance.points[0].sharePrice
  const activeReturn = active ? active.point.sharePrice / startingPrice - 1 : undefined
  const yTicks = [domain.max, (domain.min + domain.max) / 2, domain.min]
  const xTicks = [domainStart, domainStart + SEVEN_DAYS_SECONDS / 2, performance.periodEnd]

  useLayoutEffect(() => {
    const updateViewBoxWidth = () => {
      const svg = chartSvgRef.current
      if (!svg) return
      const bounds = svg.getBoundingClientRect()
      if (bounds.width <= 0 || bounds.height <= 0) return

      const nextWidth = Math.round((height * bounds.width / bounds.height) * 100) / 100
      setWidth((current) => Math.abs(current - nextWidth) < 0.01 ? current : nextWidth)
    }

    updateViewBoxWidth()
    window.addEventListener('resize', updateViewBoxWidth)
    const resizeObserver = typeof ResizeObserver === 'undefined'
      ? undefined
      : new ResizeObserver(updateViewBoxWidth)
    if (resizeObserver && chartSvgRef.current) resizeObserver.observe(chartSvgRef.current)

    return () => {
      window.removeEventListener('resize', updateViewBoxWidth)
      resizeObserver?.disconnect()
    }
  }, [height])

  useLayoutEffect(() => {
    if (activeX === undefined || activeY === undefined || activeIndex === null) return

    const updateTooltipAnchor = () => {
      const container = chartContainerRef.current
      const svg = chartSvgRef.current
      if (!container || !svg) return

      const containerBounds = container.getBoundingClientRect()
      const svgBounds = svg.getBoundingClientRect()
      if (
        containerBounds.width <= 0
        || containerBounds.height <= 0
        || svgBounds.width <= 0
        || svgBounds.height <= 0
      ) return

      const viewport = chartViewportTransform(svgBounds.width, svgBounds.height, width, height)

      setTooltipAnchor({
        activeIndex,
        x: svgBounds.left - containerBounds.left + viewport.xInset + activeX * viewport.scale,
        y: svgBounds.top - containerBounds.top + viewport.yInset + activeY * viewport.scale,
        width: containerBounds.width,
        height: containerBounds.height,
      })
    }

    updateTooltipAnchor()
    window.addEventListener('resize', updateTooltipAnchor)
    const resizeObserver = typeof ResizeObserver === 'undefined'
      ? undefined
      : new ResizeObserver(updateTooltipAnchor)
    if (resizeObserver) {
      if (chartContainerRef.current) resizeObserver.observe(chartContainerRef.current)
      if (chartSvgRef.current) resizeObserver.observe(chartSvgRef.current)
    }

    return () => {
      window.removeEventListener('resize', updateTooltipAnchor)
      resizeObserver?.disconnect()
    }
  }, [activeIndex, activeX, activeY, width])

  function selectNearestPoint(clientX: number, element: SVGSVGElement) {
    const bounds = element.getBoundingClientRect()
    const viewport = chartViewportTransform(bounds.width, bounds.height, width, height)
    const viewBoxX = viewport.scale > 0
      ? (clientX - bounds.left - viewport.xInset) / viewport.scale
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
            The value of one vault share, recorded every hour.
          </p>
        </div>
        <span className="self-start border border-brand-border/30 bg-app-bg px-3 py-1.5 text-xs font-semibold uppercase text-content-secondary">
          7 days
        </span>
      </div>

      <div ref={chartContainerRef} className="relative p-3 sm:p-5">
        <svg
          ref={chartSvgRef}
          viewBox={`0 0 ${String(width)} ${String(height)}`}
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
        {active && activeTooltipPosition ? (
          <div
            className="pointer-events-none absolute z-10 w-44 max-w-[calc(100%-1.5rem)] border border-brand-border/40 bg-app-bg px-3 py-2 shadow-xl"
            style={{
              left: activeTooltipPosition.left,
              top: activeTooltipPosition.top,
              transform: activeTooltipPosition.transform,
            }}
            data-vault-chart-tooltip
            data-placement={activeTooltipPosition.placement}
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
  const shareValueFactors = tranche.id === 'senior'
    ? SENIOR_SHARE_VALUE_FACTORS
    : JUNIOR_SHARE_VALUE_FACTORS

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

      <div className="grid gap-3 xl:grid-cols-2">
        <section className="border border-brand-border/30 bg-surface-panel p-5">
          <h3 className="text-lg font-semibold text-content-primary">What can increase share value</h3>
          <ul className="mt-4 space-y-3">
            {shareValueFactors.increase.map((item) => (
              <li key={item.label} className="flex gap-2 text-sm leading-6 text-content-secondary">
                <span
                  aria-hidden="true"
                  className="material-symbols-outlined mt-0.5 text-lg text-positive"
                >
                  add_circle
                </span>
                <span className="min-w-0">
                  {item.label}{' '}
                  <InfoTooltip
                    ariaLabel={`Learn more about ${item.label}`}
                    content={item.tooltip}
                    docsLink={item.docsLink}
                  />
                </span>
              </li>
            ))}
          </ul>
        </section>

        <section className="border border-brand-border/30 bg-surface-panel p-5">
          <h3 className="text-lg font-semibold text-content-primary">What can reduce share value</h3>
          <ul className="mt-4 space-y-3">
            {shareValueFactors.reduce.map((item) => (
              <li key={item.label} className="flex gap-2 text-sm leading-6 text-content-secondary">
                <span
                  aria-hidden="true"
                  className="material-symbols-outlined mt-0.5 text-lg text-brand-orange"
                >
                  remove_circle
                </span>
                <span className="min-w-0">
                  {item.label}{' '}
                  <InfoTooltip
                    ariaLabel={`Learn more about ${item.label}`}
                    content={item.tooltip}
                    docsLink={item.docsLink}
                  />
                </span>
              </li>
            ))}
          </ul>
        </section>
      </div>

      <Alert variant="info" title="How performance is calculated">
        Seven-day realized APY turns the vault's actual seven-day share-price change into an annual
        rate for easier comparison. It is historical, can be negative, and is not a forecast or
        guaranteed return.
      </Alert>
    </div>
  )
}

export function ActivityTab({
  tranche,
  liveData,
  snapshot,
  isConnected,
  isWrongNetwork,
  depositRequests,
  redeemRequests,
  requestsLoading,
  requestDiscoveryError,
  requestDiscoveryStale,
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
  requestDiscoveryStale: boolean
  onRefreshRequests: () => void
  onSwitchNetwork: () => void
}) {
  const [requestAction, setRequestAction] = useState<VaultRequestAction>()
  const positionValue = liveData.userShares !== undefined && liveData.sharePrice !== undefined
    ? Number(formatUnits(liveData.userShares, SHARE_DECIMALS)) * liveData.sharePrice
    : undefined
  const hasUserBalance = isConnected && liveData.userShares !== undefined
  const withdrawalCooldownRemaining = useWithdrawalCooldownRemaining(
    liveData.withdrawalCooldownEndsAt
  )
  const withdrawalCooldownActive = isConnected
    && (liveData.userShares ?? 0n) > 0n
    && withdrawalCooldownRemaining > 0
  const claimableUsdc = redeemRequests.reduce(
    (total, request) => total + request.claimableAssets,
    0n
  )
  const vaultTransactions = useVaultTransactions({
    vaultAddress: tranche.address,
    allowance: liveData.allowance,
    showTransactionModal: false,
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

  function openRequestAction(action: VaultRequestAction) {
    vaultTransactions.reset()
    setRequestAction(action)
  }

  function submitRequestAction() {
    if (!requestAction) return
    switch (requestAction.kind) {
      case 'cancel-deposit':
      case 'recover-deposit':
        vaultTransactions.cancelPendingDeposit(requestAction.requestId)
        break
      case 'claim-deposit':
        vaultTransactions.claimDepositShares(requestAction.requestId)
        break
      case 'cancel-withdrawal':
        vaultTransactions.cancelRedeemRequest(requestAction.requestId)
        break
      case 'claim-withdrawal':
        vaultTransactions.claimRedeem(requestAction.requestId, requestAction.shares)
        break
      case 'reclaim-withdrawal':
        vaultTransactions.claimRedeemRefund(requestAction.requestId)
        break
    }
  }

  function closeRequestAction() {
    if (vaultTransactions.isRunning) return
    vaultTransactions.reset()
    setRequestAction(undefined)
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
              ? formatPositionShares(liveData.userShares, tranche.token)
              : isConnected
                ? 'Balance unavailable'
                : 'Wallet not connected'}
          </h3>
        </div>

        <div className="mt-5 grid gap-3 sm:grid-cols-3">
          <DetailMetric
            label="Current value"
            value={isConnected && positionValue !== undefined
              ? formatPositionValue(positionValue)
              : '--'}
          />
          <DetailMetric
            label="Shares available to withdraw"
            value={isConnected
              ? formatPositionShares(liveData.maxRequestRedeem, tranche.token)
              : '--'}
            detail={withdrawalCooldownActive
              ? (
                <span>
                  Available in{' '}
                  <WithdrawalCooldownCountdown remainingSeconds={withdrawalCooldownRemaining} />
                </span>
              )
              : undefined}
            tone={(liveData.maxRequestRedeem ?? 0n) > 0n ? 'positive' : 'default'}
          />
          <DetailMetric
            label="USDC ready for wallet"
            value={isConnected ? formatFullUsd(claimableUsdc) : '--'}
            tone={claimableUsdc > 0n ? 'positive' : 'default'}
          />
        </div>
      </section>

      <section className="border border-brand-border/30 bg-surface-panel">
        <div className="border-b border-brand-border/25 p-5">
          <div className="flex flex-wrap items-start justify-between gap-3">
            <div>
              <h3 className="text-lg font-semibold text-content-primary">Pending deposits</h3>
              <p className="mt-1 text-sm text-content-secondary">
                The vault holds your USDC until the next eligible hourly processing time.
              </p>
            </div>
            {depositRequests.length > 0 ? (
              <Badge variant="info">
                {depositRequests.length} pending {depositRequests.length === 1 ? 'deposit' : 'deposits'}
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
                    ? 'Waiting for processing'
                    : 'Pending'
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
              const hasProcessedDeposit = request.claimableShares > 0n || request.refundableAssets > 0n

              return (
                <article key={String(request.requestId)} className="space-y-4 p-5">
                  <div className="flex flex-col gap-3 sm:flex-row sm:items-start sm:justify-between">
                    <div>
                      <p className="text-xs font-semibold uppercase tracking-[0.14em] text-content-secondary">
                        Deposit reference #{String(request.requestId)}
                      </p>
                      <h4 className="mt-1 text-xl font-semibold text-content-primary">
                        {formatFullUsd(displayedAssets)} deposited
                      </h4>
                    </div>
                    <Badge variant={statusVariant}>{statusLabel}</Badge>
                  </div>

                  <dl className="grid gap-px border border-brand-border/20 bg-brand-border/20 sm:grid-cols-2">
                    <RequestMetric
                      label={hasProcessedDeposit ? 'Eligible since' : 'Expected processing'}
                      value={settlementLabel(request.targetTimestamp)}
                    />
                    {!hasProcessedDeposit ? (
                      <RequestMetric
                        label="Estimated shares"
                        value={formatPositionShares(request.pendingSharesEstimate, tranche.token)}
                      />
                    ) : null}
                    {request.claimableShares > 0n ? (
                      <RequestMetric
                        label="Shares ready for wallet"
                        value={formatPositionShares(request.claimableShares, tranche.token)}
                        tone="positive"
                      />
                    ) : null}
                    {request.refundableAssets > 0n ? (
                      <RequestMetric
                        label="USDC ready to return"
                        value={formatFullUsd(request.refundableAssets)}
                        tone="warning"
                      />
                    ) : null}
                  </dl>

                  <div className="flex flex-col gap-4 border-t border-brand-border/20 pt-4 sm:flex-row sm:items-center sm:justify-between">
                    <p className="max-w-3xl text-sm leading-6 text-content-secondary">
                      {request.refundableAssets > 0n
                        ? 'This deposit could not be completed. Return the held USDC to your wallet.'
                        : request.claimableShares > 0n
                          ? `Your deposit is active and already participates in vault performance. Moving the shares to your wallet starts or restarts a one-hour withdrawal cooldown for your entire ${tranche.name} position.`
                          : request.matured
                            ? 'The expected time has passed, but this deposit has not been processed yet.'
                            : 'You can cancel before processing. The final number of shares is set when the deposit is processed.'}
                    </p>

                    {isWrongNetwork ? (
                      <Button type="button" variant="secondary" className="shrink-0" onClick={onSwitchNetwork}>
                        Switch to Arbitrum Sepolia
                      </Button>
                    ) : (
                      <div className="flex shrink-0 flex-wrap gap-3">
                      {request.claimableShares > 0n ? (
                        <Button
                          type="button"
                          disabled={vaultTransactions.isRunning}
                          onClick={() => {
                            openRequestAction({
                              kind: 'claim-deposit',
                              requestId: request.requestId,
                              shares: request.claimableShares,
                            })
                          }}
                        >
                          Move shares to wallet
                        </Button>
                      ) : null}
                      {request.refundableAssets > 0n ? (
                        <Button
                          type="button"
                          variant="secondary"
                          disabled={vaultTransactions.isRunning}
                          onClick={() => {
                            openRequestAction({
                              kind: 'recover-deposit',
                              requestId: request.requestId,
                              assets: request.refundableAssets,
                            })
                          }}
                        >
                          Return USDC to wallet
                        </Button>
                      ) : null}
                      {request.pendingAssets > 0n && !request.matured ? (
                        <Button
                          type="button"
                          variant="secondary"
                          disabled={vaultTransactions.isRunning}
                          onClick={() => {
                            openRequestAction({
                              kind: 'cancel-deposit',
                              requestId: request.requestId,
                              assets: request.pendingAssets,
                            })
                          }}
                        >
                          Cancel deposit
                        </Button>
                      ) : null}
                      </div>
                    )}
                  </div>
                </article>
              )
            })}
          </div>
        ) : (
          <div className="px-6 py-8 text-center">
            <p className="text-sm text-content-secondary">No pending deposits.</p>
          </div>
        )}
      </section>

      <section className="border border-brand-border/30 bg-surface-panel">
        <div className="border-b border-brand-border/25 p-5">
          <div className="flex flex-wrap items-start justify-between gap-3">
            <div>
              <h3 className="text-lg font-semibold text-content-primary">Pending withdrawals</h3>
              <p className="mt-1 text-sm text-content-secondary">
                Your shares can change in value until enough USDC is available for the withdrawal.
              </p>
            </div>
            {redeemRequests.length > 0 ? (
              <Badge variant="info">
                {redeemRequests.length} pending {redeemRequests.length === 1 ? 'withdrawal' : 'withdrawals'}
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
                  ? 'Shares ready to return'
                  : request.matured
                    ? 'Waiting for USDC'
                    : 'Pending'
              const displayedShares = request.pendingShares > 0n
                ? request.pendingShares
                : request.claimableShares > 0n
                  ? request.claimableShares
                  : request.refundableShares
              const hasProcessedWithdrawal = request.claimableAssets > 0n || request.refundableShares > 0n

              return (
                <article key={String(request.requestId)} className="space-y-4 p-5">
                  <div className="flex flex-col gap-3 sm:flex-row sm:items-start sm:justify-between">
                    <div>
                      <p className="text-xs font-semibold uppercase tracking-[0.14em] text-content-secondary">
                        Withdrawal reference #{String(request.requestId)}
                      </p>
                      <h4 className="mt-1 text-xl font-semibold text-content-primary">
                        {formatPositionShares(displayedShares, tranche.token)} requested for withdrawal
                      </h4>
                    </div>
                    <Badge variant={actionReady ? 'success' : request.matured ? 'info' : 'warning'}>
                      {statusLabel}
                    </Badge>
                  </div>

                  <dl className="grid gap-px border border-brand-border/20 bg-brand-border/20 sm:grid-cols-2">
                    <RequestMetric
                      label={hasProcessedWithdrawal ? 'Eligible since' : 'Expected processing'}
                      value={settlementLabel(request.targetTimestamp)}
                    />
                    {!hasProcessedWithdrawal ? (
                      <RequestMetric
                        label="Estimated USDC"
                        value={formatFullUsd(request.pendingAssetsEstimate)}
                      />
                    ) : null}
                    {request.claimableAssets > 0n ? (
                      <RequestMetric
                        label="USDC ready for wallet"
                        value={formatFullUsd(request.claimableAssets)}
                        tone="positive"
                      />
                    ) : null}
                    {request.refundableShares > 0n ? (
                      <RequestMetric
                        label="Shares ready to return"
                        value={formatPositionShares(request.refundableShares, tranche.token)}
                        tone="warning"
                      />
                    ) : null}
                  </dl>

                  <div className="flex flex-col gap-4 border-t border-brand-border/20 pt-4 sm:flex-row sm:items-center sm:justify-between">
                    <p className="max-w-3xl text-sm leading-6 text-content-secondary">
                      {request.claimableAssets > 0n
                        ? 'USDC is ready and can now be moved to your wallet.'
                        : request.refundPending
                          ? 'This withdrawal could not be funded. Return the remaining shares to your wallet.'
                          : request.matured
                            ? tranche.id === 'senior'
                              ? 'The withdrawal is ready to process and is waiting for enough available USDC.'
                              : 'The withdrawal is ready to process. Senior is funded first, so Junior may wait longer.'
                            : 'You can cancel before processing. The shares continue to gain or lose value while waiting.'}
                    </p>

                    {isWrongNetwork ? (
                      <Button type="button" variant="secondary" className="shrink-0" onClick={onSwitchNetwork}>
                        Switch to Arbitrum Sepolia
                      </Button>
                    ) : (
                      <div className="flex shrink-0 flex-wrap gap-3">
                      {request.claimableAssets > 0n && request.claimableShares > 0n ? (
                        <Button
                          type="button"
                          disabled={vaultTransactions.isRunning}
                          onClick={() => {
                            openRequestAction({
                              kind: 'claim-withdrawal',
                              requestId: request.requestId,
                              shares: request.claimableShares,
                              assets: request.claimableAssets,
                            })
                          }}
                        >
                          Move USDC to wallet
                        </Button>
                      ) : null}
                      {request.refundPending ? (
                        <Button
                          type="button"
                          variant="secondary"
                          disabled={vaultTransactions.isRunning}
                          onClick={() => {
                            openRequestAction({
                              kind: 'reclaim-withdrawal',
                              requestId: request.requestId,
                              shares: request.refundableShares,
                            })
                          }}
                        >
                          Return shares to wallet
                        </Button>
                      ) : null}
                      {request.pendingShares > 0n && !request.matured ? (
                        <Button
                          type="button"
                          variant="secondary"
                          disabled={vaultTransactions.isRunning}
                          onClick={() => {
                            openRequestAction({
                              kind: 'cancel-withdrawal',
                              requestId: request.requestId,
                              shares: request.pendingShares,
                            })
                          }}
                        >
                          Cancel withdrawal
                        </Button>
                      ) : null}
                      </div>
                    )}
                  </div>
                </article>
              )
            })}
          </div>
        ) : (
          <div className="px-6 py-8 text-center">
            <p className="text-sm text-content-secondary">No pending withdrawals.</p>
          </div>
        )}
      </section>

      {requestDiscoveryError || requestDiscoveryStale ? (
        <Alert
          variant="warning"
          title={requestDiscoveryError ? 'Older activity could not refresh' : 'Older activity is temporarily stale'}
        >
          {requestDiscoveryError
            ? 'The app keeps the last discovered request IDs and still checks nearby epochs. Retry to refresh older unfinished deposits and withdrawals.'
            : 'The app is showing the last confirmed request IDs while Plether’s vault index catches up.'}
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
        <p className="text-sm text-content-secondary">Checking your pending activity…</p>
      ) : null}

      <VaultRequestActionModal
        action={requestAction}
        tranche={tranche}
        transactionStatus={vaultTransactions.status}
        transactionPhase={vaultTransactions.phase}
        transactionSteps={vaultTransactions.steps}
        currentTransactionStep={vaultTransactions.currentStepIndex}
        transactionHash={vaultTransactions.hash}
        submissionError={vaultTransactions.error}
        onClose={closeRequestAction}
        onReset={vaultTransactions.reset}
        onSubmit={submitRequestAction}
      />
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

export type VaultLifecycleStep = 'review' | 'wallet' | 'queued'
type VaultTransactionStatus = 'idle' | 'running' | 'success' | 'error'
type VaultTransactionPhase = 'idle' | 'awaiting_wallet' | 'confirming_onchain' | 'complete' | 'error'

const VAULT_LIFECYCLE_STEPS: { id: VaultLifecycleStep; label: string }[] = [
  { id: 'review', label: 'Review' },
  { id: 'wallet', label: 'Wallet' },
  { id: 'queued', label: 'Submitted' },
]

export function VaultLifecycleSteps({
  currentStep,
  finalLabel = 'Submitted',
}: {
  currentStep: VaultLifecycleStep
  finalLabel?: string
}) {
  const currentIndex = VAULT_LIFECYCLE_STEPS.findIndex(({ id }) => id === currentStep)
  const steps = VAULT_LIFECYCLE_STEPS.map((step) => (
    step.id === 'queued' ? { ...step, label: finalLabel } : step
  ))

  return (
    <div className="relative">
      <div
        className="absolute top-[7px] h-px bg-brand-border/35"
        style={{ left: 'calc(16.666667% + 0.5rem)', width: 'calc(33.333333% - 1rem)' }}
      />
      <div
        className="absolute top-[7px] h-px bg-brand-border/35"
        style={{ left: 'calc(50% + 0.5rem)', width: 'calc(33.333333% - 1rem)' }}
      />
      <ol className="relative grid grid-cols-3 gap-2">
        {steps.map((step, index) => {
          const isCurrent = index === currentIndex
          const isFuture = index > currentIndex
          const dotClassName = isCurrent
            ? 'border-brand-peach bg-brand-peach'
            : isFuture
              ? 'border-brand-border/30 bg-surface-panel'
              : 'border-content-secondary/50 bg-content-secondary/50'
          const labelClassName = isCurrent
            ? 'text-brand-peach'
            : isFuture
              ? 'text-content-secondary/50'
              : 'text-content-secondary'

          return (
            <li
              key={step.id}
              className="relative min-w-0 text-center"
              aria-current={isCurrent ? 'step' : undefined}
            >
              <div className="flex justify-center">
                <span className={`relative z-10 h-3.5 w-3.5 rounded-full border-2 ${dotClassName}`} />
              </div>
              <div className="mt-3 min-w-0">
                <div className={`text-base font-semibold ${labelClassName}`}>{step.label}</div>
              </div>
            </li>
          )
        })}
      </ol>
    </div>
  )
}

export function VaultRequestQueuedState({
  mode,
  targetSettlement,
  transactionHash,
  onClose,
  onViewRequest,
}: {
  mode: ActionMode
  targetSettlement: string
  transactionHash?: string | null
  onClose: () => void
  onViewRequest: () => void
}) {
  return (
    <div className="space-y-5 text-center">
      <SuccessIcon className="mx-auto" />
      <div>
        <h2 className="text-2xl font-semibold text-content-primary">
          {mode === 'deposit' ? 'Deposit submitted' : 'Withdrawal submitted'}
        </h2>
        <p className="mt-2 text-sm leading-6 text-content-secondary">
          Expected processing: {targetSettlement}. Track it from Your position.
        </p>
      </div>
      {transactionHash ? (
        <a
          href={`${EXPLORER_TX_BASE_URL}/${transactionHash}`}
          target="_blank"
          rel="noopener noreferrer"
          className="group inline-flex items-center gap-2 text-sm font-semibold text-brand-peach"
        >
          <span className="group-hover:underline group-hover:underline-offset-4">View transaction</span>
          <span className="material-symbols-outlined text-lg">open_in_new</span>
        </a>
      ) : null}
      <div className="grid grid-cols-2 gap-3 pt-2">
        <Button type="button" variant="secondary" className="w-full" onClick={onClose}>
          Done
        </Button>
        <Button type="button" className="w-full" onClick={onViewRequest}>
          View activity
        </Button>
      </div>
    </div>
  )
}

function VaultTransactionSteps({
  steps,
  currentStepIndex,
  phase,
}: {
  steps: string[]
  currentStepIndex: number
  phase: VaultTransactionPhase
}) {
  return (
    <ol className="space-y-4 border border-brand-border/25 bg-app-bg p-4">
      {steps.map((step, index) => {
        const isComplete = index < currentStepIndex || phase === 'complete'
        const isCurrent = index === currentStepIndex && !isComplete
        const isError = isCurrent && phase === 'error'

        return (
          <li key={`${step}-${String(index)}`} className="flex items-center gap-3">
            {isComplete ? (
              <span className="material-symbols-outlined flex h-6 w-6 items-center justify-center rounded-full bg-positive text-base text-app-bg">
                check
              </span>
            ) : isError ? (
              <span className="material-symbols-outlined flex h-6 w-6 items-center justify-center rounded-full bg-brand-orange text-base text-app-bg">
                close
              </span>
            ) : isCurrent ? (
              <Spinner size="md" variant={phase === 'confirming_onchain' ? 'confirming' : 'default'} />
            ) : (
              <span className="h-6 w-6 rounded-full border-2 border-content-secondary/40" />
            )}
            <div className="min-w-0">
              <p className={`text-sm font-semibold ${isError ? 'text-brand-orange' : isCurrent ? 'text-content-primary' : 'text-content-secondary'}`}>
                {step}
              </p>
              {isCurrent && !isError ? (
                <p className="mt-0.5 text-xs text-content-secondary">
                  {phase === 'confirming_onchain'
                    ? 'Submitted — waiting for network confirmation'
                    : 'Confirm this transaction in your wallet'}
                </p>
              ) : null}
            </div>
          </li>
        )
      })}
    </ol>
  )
}

function VaultRequestActionModal({
  action,
  tranche,
  transactionStatus,
  transactionPhase,
  transactionSteps,
  currentTransactionStep,
  transactionHash,
  submissionError,
  onClose,
  onReset,
  onSubmit,
}: {
  action?: VaultRequestAction
  tranche: TrancheDefinition
  transactionStatus: VaultTransactionStatus
  transactionPhase: VaultTransactionPhase
  transactionSteps: string[]
  currentTransactionStep: number
  transactionHash?: string | null
  submissionError?: string | null
  onClose: () => void
  onReset: () => void
  onSubmit: () => void
}) {
  if (!action) return null

  const isRunning = transactionStatus === 'running'
  const lifecycleStep: VaultLifecycleStep = transactionStatus === 'idle'
    ? 'review'
    : transactionStatus === 'success'
      ? 'queued'
      : 'wallet'
  const shareAmount = (shares: bigint) => (
    <span className="inline-flex items-baseline gap-1.5 whitespace-nowrap">
      <span>{formatShares(shares)}</span>
      <TokenLabel token={tranche.token} />
    </span>
  )
  const copy = (() => {
    switch (action.kind) {
      case 'cancel-deposit':
        return {
          title: 'Cancel this deposit?',
          description: 'The deposit is still pending. Cancelling returns the USDC held by the vault to your wallet.',
          amountLabel: 'USDC returned',
          amount: formatFullUsd(action.assets),
          confirmLabel: 'Cancel deposit',
          confirmVariant: 'danger' as const,
          successTitle: 'Deposit cancelled',
          successDescription: <>{formatFullUsd(action.assets)} held by the vault has been returned to your wallet.</>,
        }
      case 'recover-deposit':
        return {
          title: 'Return this deposit?',
          description: 'The deposit could not be completed. Return the refundable USDC held by the vault to your wallet.',
          amountLabel: 'USDC returned',
          amount: formatFullUsd(action.assets),
          confirmLabel: 'Return USDC',
          confirmVariant: 'primary' as const,
          successTitle: 'USDC returned',
          successDescription: <>{formatFullUsd(action.assets)} has been returned to your wallet.</>,
        }
      case 'claim-deposit':
        return {
          title: 'Move your vault shares to your wallet?',
          description: `These shares already participate in vault performance. Moving them to your wallet starts or restarts a one-hour cooldown for every ${tranche.token} share in your wallet. Until it ends, those shares cannot be transferred or used for a withdrawal request.`,
          amountLabel: 'Shares moved',
          amount: shareAmount(action.shares),
          confirmLabel: 'Move shares',
          confirmVariant: 'primary' as const,
          successTitle: 'Vault shares moved',
          successDescription: <>{shareAmount(action.shares)} is now in your wallet. Your one-hour withdrawal cooldown has started.</>,
        }
      case 'cancel-withdrawal':
        return {
          title: 'Cancel this withdrawal?',
          description: 'This withdrawal is still waiting for USDC. Cancelling returns the shares held by the vault to your wallet and restarts the one-hour withdrawal cooldown for your entire position.',
          amountLabel: 'Shares returned',
          amount: shareAmount(action.shares),
          confirmLabel: 'Cancel withdrawal',
          confirmVariant: 'danger' as const,
          successTitle: 'Withdrawal cancelled',
          successDescription: <>{shareAmount(action.shares)} has been returned to your wallet.</>,
        }
      case 'claim-withdrawal':
        return {
          title: 'Move your USDC to your wallet?',
          description: 'USDC has been allocated to this withdrawal and is ready to move to your wallet.',
          amountLabel: 'USDC moved',
          amount: formatFullUsd(action.assets),
          confirmLabel: 'Move USDC',
          confirmVariant: 'primary' as const,
          successTitle: 'USDC moved to wallet',
          successDescription: <>{formatFullUsd(action.assets)} has been transferred to your wallet.</>,
        }
      case 'reclaim-withdrawal':
        return {
          title: 'Return your unfunded shares?',
          description: `USDC could not be allocated to this part of the withdrawal. Returning the remaining shares to your wallet restarts the one-hour withdrawal cooldown for every ${tranche.token} share in your wallet.`,
          amountLabel: 'Shares returned',
          amount: shareAmount(action.shares),
          confirmLabel: 'Return shares',
          confirmVariant: 'primary' as const,
          successTitle: 'Vault shares returned',
          successDescription: <>{shareAmount(action.shares)} has been returned to your wallet.</>,
        }
    }
  })()

  return (
    <Modal
      isOpen
      onClose={onClose}
      ariaLabel={`${copy.confirmLabel} flow`}
      headerContent={<VaultLifecycleSteps currentStep={lifecycleStep} finalLabel="Complete" />}
      showCloseButton={false}
      closeOnBackdrop={!isRunning}
      closeOnEscape={!isRunning}
      size="lg"
      inertBackground
      analyticsId="vault_request_action_flow"
      analyticsSurface="vaults"
      analyticsProperties={{ tranche: tranche.id, action: action.kind }}
    >
      {transactionStatus === 'idle' ? (
        <div className="space-y-5">
          <div>
            <p className="text-xs font-semibold uppercase tracking-[0.14em] text-content-secondary">
              Reference {String(action.requestId)}
            </p>
            <h2 className="mt-1 text-xl font-semibold text-content-primary">{copy.title}</h2>
            <p className="mt-2 text-sm leading-6 text-content-secondary">{copy.description}</p>
          </div>
          <section className="border border-brand-border/25 bg-app-bg p-4">
            <PreviewRow label={copy.amountLabel} value={copy.amount} />
          </section>
          <div className="grid grid-cols-2 gap-3">
            <Button type="button" variant="secondary" className="w-full" onClick={onClose}>
              Back
            </Button>
            <Button
              type="button"
              variant={copy.confirmVariant}
              className="w-full"
              onClick={onSubmit}
            >
              {copy.confirmLabel}
            </Button>
          </div>
        </div>
      ) : null}

      {transactionStatus === 'running' ? (
        <div className="space-y-5">
          <div>
            <h2 className="text-xl font-semibold text-content-primary">
              {transactionPhase === 'confirming_onchain'
                ? 'Waiting for network confirmation'
                : 'Confirm in your wallet'}
            </h2>
            <p className="mt-2 text-sm leading-6 text-content-secondary">
              Keep this window open until this step finishes.
            </p>
          </div>
          <VaultTransactionSteps
            steps={transactionSteps}
            currentStepIndex={currentTransactionStep}
            phase={transactionPhase}
          />
        </div>
      ) : null}

      {transactionStatus === 'error' ? (
        <div className="space-y-5">
          <div>
            <h2 className="text-xl font-semibold text-brand-orange">The action did not complete</h2>
            <p className="mt-2 text-sm leading-6 text-content-secondary">
              Nothing else will run until you retry.
            </p>
          </div>
          <VaultTransactionSteps
            steps={transactionSteps}
            currentStepIndex={currentTransactionStep}
            phase="error"
          />
          {submissionError ? (
            <div className="border border-brand-orange/30 bg-brand-orange/10 p-4 text-sm leading-6 text-brand-orange">
              {submissionError}
            </div>
          ) : null}
          <div className="grid grid-cols-2 gap-3">
            <Button type="button" variant="secondary" className="w-full" onClick={onReset}>
              Back to review
            </Button>
            <Button type="button" className="w-full" onClick={onSubmit}>
              Try again
            </Button>
          </div>
        </div>
      ) : null}

      {transactionStatus === 'success' ? (
        <div className="space-y-5 text-center">
          <SuccessIcon className="mx-auto" />
          <div>
            <h2 className="text-2xl font-semibold text-content-primary">{copy.successTitle}</h2>
            <p className="mt-2 text-sm leading-6 text-content-secondary">{copy.successDescription}</p>
          </div>
          {transactionHash ? (
            <a
              href={`${EXPLORER_TX_BASE_URL}/${transactionHash}`}
              target="_blank"
              rel="noopener noreferrer"
              className="group inline-flex items-center gap-2 text-sm font-semibold text-brand-peach"
            >
              <span className="group-hover:underline group-hover:underline-offset-4">View transaction</span>
              <span className="material-symbols-outlined text-lg">open_in_new</span>
            </a>
          ) : null}
          <Button type="button" className="w-full" onClick={onClose}>
            Done
          </Button>
        </div>
      ) : null}
    </Modal>
  )
}

export function VaultPreviewModal({
  isOpen,
  onClose,
  onReset,
  onViewRequest,
  mode,
  tranche,
  amount,
  estimatedShares,
  depositMode,
  sharePrice,
  performance,
  oracleFrozen,
  pendingActivationTimestamp,
  canSubmit,
  needsApproval,
  transactionStatus,
  transactionPhase,
  transactionSteps,
  currentTransactionStep,
  transactionHash,
  onSubmit,
  submissionError,
}: {
  isOpen: boolean
  onClose: () => void
  onReset: () => void
  onViewRequest: () => void
  mode: ActionMode
  tranche: TrancheDefinition
  amount: string
  estimatedShares?: number
  depositMode: string
  sharePrice?: number
  performance?: CompleteVaultPerformance
  oracleFrozen?: boolean
  pendingActivationTimestamp?: number
  canSubmit: boolean
  needsApproval: boolean
  transactionStatus: VaultTransactionStatus
  transactionPhase: VaultTransactionPhase
  transactionSteps: string[]
  currentTransactionStep: number
  transactionHash?: string | null
  onSubmit: () => void
  submissionError?: string | null
}) {
  const isRunning = transactionStatus === 'running'
  const lifecycleStep: VaultLifecycleStep = transactionStatus === 'idle'
    ? 'review'
    : transactionStatus === 'success'
      ? 'queued'
      : 'wallet'
  const actionName = mode === 'deposit' ? 'deposit' : 'withdrawal'
  const targetSettlement = pendingActivationTimestamp === undefined
    ? 'Next processing time'
    : new Date(pendingActivationTimestamp * 1_000).toLocaleString('en-US', {
        month: 'short',
        day: 'numeric',
        hour: '2-digit',
        minute: '2-digit',
      })

  return (
    <Modal
      isOpen={isOpen}
      onClose={onClose}
      ariaLabel={`${mode === 'deposit' ? 'Deposit' : 'Withdrawal'} flow`}
      headerContent={<VaultLifecycleSteps currentStep={lifecycleStep} />}
      showCloseButton={false}
      closeOnBackdrop={!isRunning}
      closeOnEscape={!isRunning}
      size="lg"
      inertBackground
      analyticsId={`vault_${mode}_flow`}
      analyticsSurface="vaults"
      analyticsProperties={{ tranche: tranche.id }}
    >
      {transactionStatus === 'idle' ? (
        <div className="space-y-5">
          <p className="text-xl font-semibold leading-7 text-content-primary">
            You are {mode === 'deposit' ? 'depositing' : 'withdrawing'}{' '}
            <TokenAmount amount={amount || '0.00'} /> {mode === 'deposit' ? 'into' : 'from'}{' '}
            {tranche.name}.
          </p>

          <section className="border border-brand-border/25 bg-app-bg p-4">
            <p className="mb-4 text-xs font-semibold uppercase tracking-[0.14em] text-content-secondary">
              {mode === 'deposit' ? 'Deposit' : 'Withdrawal'} preview
            </p>
            <div className="space-y-3">
              <PreviewRow
                label={mode === 'deposit' ? 'USDC to deposit' : 'USDC to withdraw'}
                value={<TokenAmount amount={amount || '0.00'} />}
              />
              <PreviewRow
                label={mode === 'withdraw' ? 'Estimated shares used' : 'Estimated shares received'}
                value={estimatedShares === undefined
                  ? 'Latest estimate unavailable'
                  : (
                    <TokenAmount
                      amount={estimatedShares.toLocaleString('en-US', { maximumFractionDigits: 6 })}
                      token={tranche.token}
                    />
                  )}
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
                label="Processing"
                value={mode === 'deposit' ? depositMode : 'Processed hourly when USDC is available'}
              />
              <PreviewRow label="Expected processing" value={targetSettlement} />
              {oracleFrozen === true ? (
                <PreviewRow label="Temporary pricing fee" value="May apply when processed" />
              ) : null}
            </div>
          </section>

          <div className="border border-brand-peach/30 bg-brand-peach/10 p-4 text-sm leading-6 text-brand-peach">
            <p className="font-semibold">Your final amount is set when processed</p>
            <p className="mt-1 text-content-secondary">
              {mode === 'deposit'
                ? 'The displayed shares are an estimate and may change before processing.'
                : 'Your shares continue gaining or losing value until the withdrawal is funded.'}
            </p>
          </div>

          {!canSubmit ? (
            <Alert variant="warning" title="Action unavailable">
              This action is temporarily unavailable based on the latest vault status.
            </Alert>
          ) : null}

          <div className="grid grid-cols-2 gap-3">
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
              onClick={onSubmit}
              analyticsId={`vault_${mode}_submitted`}
              analyticsSurface="vaults"
              analyticsProperties={{ tranche: tranche.id }}
            >
              {canSubmit ? `Confirm ${actionName}` : 'Unavailable'}
            </Button>
          </div>
          {mode === 'deposit' && needsApproval ? (
            <p className="text-center text-xs leading-5 text-content-secondary">
              Your wallet will first ask you to approve this USDC amount, then confirm the deposit.
            </p>
          ) : null}
        </div>
      ) : null}

      {transactionStatus === 'running' ? (
        <div className="space-y-5">
          <div>
            <h2 className="text-xl font-semibold text-content-primary">
              {transactionPhase === 'confirming_onchain'
                ? 'Waiting for network confirmation'
                : 'Confirm in your wallet'}
            </h2>
            <p className="mt-2 text-sm leading-6 text-content-secondary">
              Keep this window open. Each required transaction appears here in order.
            </p>
          </div>
          <VaultTransactionSteps
            steps={transactionSteps}
            currentStepIndex={currentTransactionStep}
            phase={transactionPhase}
          />
        </div>
      ) : null}

      {transactionStatus === 'error' ? (
        <div className="space-y-5">
          <div>
            <h2 className="text-xl font-semibold text-brand-orange">
              The {mode === 'deposit' ? 'deposit' : 'withdrawal'} was not submitted
            </h2>
            <p className="mt-2 text-sm leading-6 text-content-secondary">
              No later step will run until you retry.
            </p>
          </div>
          <VaultTransactionSteps
            steps={transactionSteps}
            currentStepIndex={currentTransactionStep}
            phase="error"
          />
          {submissionError ? (
            <div className="border border-brand-orange/30 bg-brand-orange/10 p-4 text-sm leading-6 text-brand-orange">
              {submissionError}
            </div>
          ) : null}
          <div className="grid grid-cols-2 gap-3">
            <Button type="button" variant="secondary" className="w-full" onClick={onReset}>
              Back to review
            </Button>
            <Button type="button" className="w-full" onClick={onSubmit}>
              Try again
            </Button>
          </div>
        </div>
      ) : null}

      {transactionStatus === 'success' ? (
        <VaultRequestQueuedState
          mode={mode}
          targetSettlement={targetSettlement}
          transactionHash={transactionHash}
          onClose={onClose}
          onViewRequest={onViewRequest}
        />
      ) : null}
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
    estimatedSharesRaw: bigint
  }>()
  const [isRefreshingQuote, setIsRefreshingQuote] = useState(false)
  const [quoteRefreshError, setQuoteRefreshError] = useState<string>()
  const withdrawalCooldownRemaining = useWithdrawalCooldownRemaining(
    liveData.withdrawalCooldownEndsAt
  )
  const withdrawalCooldownActive = isConnected
    && (liveData.userShares ?? 0n) > 0n
    && withdrawalCooldownRemaining > 0
  const amountRaw = parseUsdc(amount)
  const depositMode = getDepositMode(liveData)
  const pendingActivationTimestamp = liveData.nextRequestEpoch !== undefined
    ? Number(liveData.nextRequestEpoch * 3_600n)
    : undefined
  const vaultTransactions = useVaultTransactions({
    vaultAddress: tranche.address,
    allowance: liveData.allowance,
    showTransactionModal: false,
    onSuccess: () => {
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
  const liveDepositLimit = depositMode === 'Open for deposits'
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
    && depositMode !== 'Open for deposits'
  const depositUnavailableStatus = depositUnavailable
    ? getDepositUnavailableStatus(tranche, liveData, snapshot.pool)
    : undefined
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
    && (mode === 'withdraw' || depositMode === 'Open for deposits')
  const inputError = exceedsAvailable
    ? `Exceeds available ${mode === 'deposit' ? 'balance' : 'withdrawal limit'}.`
    : depositLimitExceeded
      ? 'Amount is above the current deposit limit.'
      : belowMinimumDeposit
        ? `The minimum vault deposit is ${formatFullUsdc(snapshot.pool.minTrancheDepositUsdc)} USDC.`
        : redeemLimitExceeded
          ? 'Amount is above what you can currently withdraw.'
          : undefined

  useEffect(() => {
    if (liveData.withdrawalCooldownEndsAt === undefined) return undefined

    const millisecondsUntilRefresh = (
      Number(liveData.withdrawalCooldownEndsAt) * 1_000
    ) - Date.now()
    if (millisecondsUntilRefresh <= 0) return undefined

    const timeout = window.setTimeout(() => {
      snapshot.refresh()
    }, millisecondsUntilRefresh + 250)

    return () => {
      window.clearTimeout(timeout)
    }
  }, [liveData.withdrawalCooldownEndsAt, snapshot])

  const buttonLabel = !isConnected
    ? 'Connect wallet'
    : isWrongNetwork
      ? isSwitchingNetwork
        ? 'Switching network...'
        : 'Switch to Arbitrum Sepolia'
      : mode === 'deposit' ? 'Review deposit' : 'Review withdrawal'

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
        setQuoteRefreshError('The latest estimate could not be refreshed. Please try again.')
        return
      }

      setReviewQuote({
        estimatedShares: Number(formatUnits(refreshedSharesRaw, SHARE_DECIMALS)),
        estimatedSharesRaw: refreshedSharesRaw,
      })
      vaultTransactions.reset()
      setShowPreview(true)
    } catch {
      setQuoteRefreshError('The latest estimate could not be refreshed. Please try again.')
    } finally {
      setIsRefreshingQuote(false)
    }
  }

  function handleTransactionSubmit() {
    if (!canSubmitTransaction) return

    vaultTransactions.reset()
    if (mode === 'deposit') {
      vaultTransactions.requestDeposit(amountRaw)
    } else {
      if (reviewQuote?.estimatedSharesRaw === undefined) return
      vaultTransactions.requestRedeem(reviewQuote.estimatedSharesRaw)
    }
  }

  function handlePreviewClose() {
    if (vaultTransactions.isRunning) return
    const shouldClearForm = vaultTransactions.isSuccess
    setShowPreview(false)
    vaultTransactions.reset()
    if (shouldClearForm) {
      setAmount('')
      setReviewQuote(undefined)
    }
  }

  function handleViewRequest() {
    if (vaultTransactions.isRunning) return
    setShowPreview(false)
    setAmount('')
    setReviewQuote(undefined)
    vaultTransactions.reset()
    onViewRequests()
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
                    ? 'bg-brand-peach text-app-bg'
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
                  ? 'Estimated shares used'
                  : 'Estimated shares you’ll receive'
              }
              value={
                isQuotePending
                  ? 'Updating estimate...'
                  : estimatedShares === undefined
                    ? '--'
                    : (
                      <TokenAmount
                        amount={estimatedShares.toLocaleString('en-US', { maximumFractionDigits: 6 })}
                        token={tranche.token}
                      />
                    )
              }
            />
            {mode === 'deposit' ? (
              <>
                <PreviewRow label="Deposit status" value={depositMode} />
                <PreviewRow
                  label="Expected processing"
                  value={
                    pendingActivationTimestamp === undefined
                      ? 'Next processing time'
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
                  label="Estimated USDC you’ll receive"
                  value={<TokenAmount amount={amount || '0.00'} />}
                />
                <PreviewRow label="Processing" value="Processed hourly; Senior withdrawals are funded first" />
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
              label="Temporary pricing fee"
              value={liveData.frozenLpFeeBps === undefined || snapshot.pool.oracleFrozen === undefined
                ? 'State unavailable'
                : snapshot.pool.oracleFrozen
                  ? `${(Number(liveData.frozenLpFeeBps) / 100).toFixed(2)}% active`
                  : 'Inactive'}
              valueClassName={snapshot.pool.oracleFrozen === true ? 'text-brand-orange' : 'text-content-primary'}
            />
          </div>

          {mode === 'deposit' && depositMode === 'Open for deposits' ? (
            <Alert variant="info" title="This deposit is processed hourly">
              The vault holds your USDC immediately. You can cancel before it is processed.
              Once your shares are ready, move them to your wallet from Your position.
            </Alert>
          ) : null}

          {depositUnavailableStatus ? (
            <Alert variant="warning" title="Deposits unavailable">
              <p>{depositUnavailableStatus.reason}</p>
              <p className="mt-2">
                <span className="font-semibold">Available again:</span>{' '}
                {depositUnavailableStatus.availability}
              </p>
            </Alert>
          ) : null}

          {mode === 'withdraw' && liveData.maxRequestRedeem === 0n && isConnected ? (
            <Alert
              variant="warning"
              title={withdrawalCooldownActive
                ? 'Withdrawal cooldown active'
                : 'Withdrawals are temporarily unavailable'}
            >
              {withdrawalCooldownActive ? (
                <>
                  You can request a withdrawal in{' '}
                  <WithdrawalCooldownCountdown remainingSeconds={withdrawalCooldownRemaining} />.{' '}
                  Receiving more {tranche.token} shares in your wallet restarts this one-hour cooldown
                  for your entire {tranche.name} position.
                </>
              ) : (
                'None of your shares are currently available to withdraw.'
              )}
            </Alert>
          ) : null}

          {mode === 'withdraw'
            && snapshot.pool.oracleFrozen === true
            && liveData.frozenLpFeeBps !== undefined ? (
              <Alert variant="warning" title="Temporary withdrawal surcharge active">
                A temporary {(Number(liveData.frozenLpFeeBps) / 100).toFixed(2)}% fee is active
                because live market pricing is unavailable. If it is still active when your
                withdrawal is processed, more shares will be needed. Unless the withdrawal is
                urgent, wait for live pricing to return.
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

          {actionDataUnavailable && mode !== 'deposit' ? (
            <p className="text-xs leading-5 text-brand-orange">
              Some live pool, vault, or wallet data is unavailable, so the preview is disabled.
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
            an immediate withdrawal, and recent APY is not a forecast.
          </p>
        </div>

        {depositRequests.length + redeemRequests.length > 0 ? (
          <div className="border-t border-brand-border/30 p-5">
            <div className="flex items-start justify-between gap-3">
              <div>
                <p className="text-xs font-semibold uppercase tracking-[0.14em] text-content-secondary">
                  Your pending activity
                </p>
                <p className="mt-1 text-lg font-semibold text-content-primary">
                  {depositRequests.length + redeemRequests.length} pending{' '}
                  {depositRequests.length + redeemRequests.length === 1 ? 'item' : 'items'}
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
                  ? 'Action needed'
                  : 'In progress'}
              </Badge>
            </div>
            <Button
              type="button"
              variant="secondary"
              className="mt-4 w-full"
              onClick={onViewRequests}
            >
              Review pending activity
            </Button>
          </div>
        ) : null}
      </aside>

      <VaultPreviewModal
        isOpen={showPreview}
        onClose={handlePreviewClose}
        onReset={vaultTransactions.reset}
        onViewRequest={handleViewRequest}
        mode={mode}
        tranche={tranche}
        amount={amount}
        estimatedShares={reviewQuote?.estimatedShares}
        depositMode={depositMode}
        sharePrice={liveData.sharePrice}
        performance={performance}
        oracleFrozen={snapshot.pool.oracleFrozen}
        pendingActivationTimestamp={pendingActivationTimestamp}
        canSubmit={canSubmitTransaction}
        needsApproval={needsApproval}
        transactionStatus={vaultTransactions.status}
        transactionPhase={vaultTransactions.phase}
        transactionSteps={vaultTransactions.steps}
        currentTransactionStep={vaultTransactions.currentStepIndex}
        transactionHash={vaultTransactions.hash}
        onSubmit={handleTransactionSubmit}
        submissionError={vaultTransactions.error}
      />
    </>
  )
}

interface VaultDetailProps {
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
}

type VaultDetailViewProps = Omit<VaultDetailProps, 'ownerAddress'> & {
  vaultActivity: VaultActivityViewState
  vaultRequests: VaultRequestsViewState
  epochCountdownSeconds?: number
}

function VaultDetail({
  ownerAddress,
  ...viewProps
}: VaultDetailProps) {
  const { snapshot, tranche } = viewProps
  const liveData = snapshot.tranches[tranche.id]
  const vaultActivity = useVaultActivity({
    seniorTotalAssets: snapshot.tranches.senior.totalAssets,
    seniorEffectiveSupply: snapshot.tranches.senior.effectiveTotalSupply
      ?? snapshot.tranches.senior.totalSupply,
    juniorTotalAssets: snapshot.tranches.junior.totalAssets,
    juniorEffectiveSupply: snapshot.tranches.junior.effectiveTotalSupply
      ?? snapshot.tranches.junior.totalSupply,
  })
  const vaultRequests = useVaultRequests({
    controller: ownerAddress,
    isSenior: tranche.id === 'senior',
    currentEpoch: liveData.currentEpoch,
  })

  return (
    <VaultDetailView
      {...viewProps}
      vaultActivity={vaultActivity}
      vaultRequests={vaultRequests}
    />
  )
}

export function VaultDetailView({
  tranche,
  snapshot,
  history,
  isConnected,
  isWrongNetwork,
  onConnect,
  onSwitchNetwork,
  isSwitchingNetwork,
  switchError,
  vaultActivity,
  vaultRequests,
  epochCountdownSeconds,
}: VaultDetailViewProps) {
  const [activeSection, setActiveSection] = useState<DetailSectionId>('overview')
  const stickyHeaderHeight = useStickyHeaderHeight()
  const stickyElementTop = stickyHeaderHeight + STICKY_ELEMENT_GAP_PX
  const sectionScrollOffset = stickyElementTop + SECTION_NAV_HEIGHT_PX
  const liveData = snapshot.tranches[tranche.id]
  const performance = getCompleteVaultPerformance(history, tranche.id)
  const hasPerformance = performance !== undefined
  const poolWithdrawCap = tranche.id === 'senior'
    ? snapshot.pool.seniorPoolWithdrawCapUsdc
    : snapshot.pool.juniorPoolWithdrawCapUsdc
  const sections = useMemo<{ id: DetailSectionId; anchor: string; label: string }[]>(() => [
    { id: 'overview', anchor: 'overview', label: 'Overview' },
    ...(tranche.id === 'junior'
      ? [{ id: 'market-exposure' as const, anchor: 'market-exposure', label: 'Market exposure' }]
      : []),
    ...(hasPerformance
      ? [{ id: 'performance' as const, anchor: 'performance', label: 'Performance' }]
      : []),
    { id: 'position', anchor: 'your-position', label: 'Your position' },
    { id: 'activity', anchor: 'activity', label: 'Activity' },
  ], [hasPerformance, tranche.id])

  function scrollToSection(sectionId: DetailSectionId) {
    const section = sections.find((candidate) => candidate.id === sectionId)
    if (!section) return

    setActiveSection(section.id)
    document.getElementById(section.anchor)?.scrollIntoView({
      behavior: 'smooth',
      block: 'start',
    })
    window.history.replaceState(window.history.state, '', `#${section.anchor}`)
  }

  useEffect(() => {
    const updateActiveSection = () => {
      let nextSection = sections[0]
      const currentScrollTop = window.scrollY
      const maximumScrollTop = Math.max(
        document.documentElement.scrollHeight - window.innerHeight,
        0,
      )

      for (const section of sections) {
        const element = document.getElementById(section.anchor)
        if (!element) continue

        const sectionViewportTop = element.getBoundingClientRect().top
        const sectionTop = sectionViewportTop + currentScrollTop
        const activationScrollTop = Math.min(
          Math.max(sectionTop - sectionScrollOffset, 0),
          maximumScrollTop,
        )

        if (maximumScrollTop > 0) {
          if (currentScrollTop < activationScrollTop) break
        } else if (sectionViewportTop > sectionScrollOffset) {
          break
        }
        nextSection = section
      }
      setActiveSection(nextSection.id)
    }

    const initialUpdateFrame = window.requestAnimationFrame(updateActiveSection)
    window.addEventListener('scroll', updateActiveSection, { passive: true })
    window.addEventListener('resize', updateActiveSection)

    return () => {
      window.cancelAnimationFrame(initialUpdateFrame)
      window.removeEventListener('scroll', updateActiveSection)
      window.removeEventListener('resize', updateActiveSection)
    }
  }, [sectionScrollOffset, sections])

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
            label="Current vault value"
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
                ? 'Current deposit limit unavailable'
                : (
                  <span>
                    Current deposit limit: {formatVaultLimit(liveData.maxRequestDeposit)}
                  </span>
                )}
            tooltip="The current estimated value of this vault. It can rise or fall and is not the same as cumulative deposits."
          />
          {performance ? (
            <PoolStat
              label="7d realized APY"
              value={formatSignedPercent(performance.apy7d)}
              subvalue={`${formatSignedPercent(performance.return7d)} actual 7d return`}
              valueClassName={performanceValueClassName(performance.apy7d)}
              tooltip="Annualized historical return calculated from share prices recorded over the last seven days."
            />
          ) : null}
          <PoolStat
            label="Share price"
            value={formatSharePrice(liveData.sharePrice)}
            subvalue={<TokenAmount amount="1" token={tranche.token} />}
          />
          <PoolStat
            label="Estimated withdrawal liquidity"
            value={formatCompactUsd(poolWithdrawCap)}
            subvalue={tranche.withdrawalPriority}
          />
          <PoolStat
            label="How returns work"
            value={tranche.targetReturn}
            subvalue={(
              <>
                {tranche.returnModel}{' '}
                <DocsLink
                  href={DOCS_LINKS.poolLiquidity.href}
                  title={DOCS_LINKS.poolLiquidity.title}
                  className="whitespace-nowrap"
                >
                  Learn more
                </DocsLink>
              </>
            )}
          />
        </dl>
      </section>

      {liveData.lpEpochSettlementPaused === true ? (
        <Alert variant="warning" title="Hourly processing paused">
          You can still submit deposits or withdrawals, move ready funds to your wallet, cancel
          pending activity, and return available funds or shares. New deposits will not start
          earning and withdrawals will not receive new funds until hourly processing resumes.
        </Alert>
      ) : null}

      <div className="grid items-start gap-6 lg:grid-cols-[minmax(0,1fr)_360px]">
        <div className="min-w-0 space-y-6">
          <nav
            aria-label={`${tranche.name} page sections`}
            className="relative sticky z-10 border border-brand-border/30 bg-app-bg px-4 before:pointer-events-none before:absolute before:-left-px before:-right-px before:bottom-[calc(100%+1px)] before:h-4 before:bg-app-bg before:content-['']"
            style={{ top: stickyElementTop }}
          >
            <div className="vault-section-nav flex items-stretch gap-6 overflow-x-auto">
              <span className="hidden shrink-0 items-center border-r border-brand-border/25 pr-6 text-[10px] font-semibold uppercase tracking-[0.16em] text-content-secondary sm:flex">
                On this page
              </span>
              {sections.map((section, index) => (
                <button
                  key={section.id}
                  type="button"
                  aria-label={section.label}
                  aria-current={activeSection === section.id ? 'location' : undefined}
                  aria-controls={section.anchor}
                  onClick={() => {
                    scrollToSection(section.id)
                  }}
                  className={`group relative flex shrink-0 items-center gap-2 py-3 text-sm font-semibold transition-colors after:absolute after:inset-x-0 after:bottom-0 after:h-0.5 after:origin-left after:transition-transform ${
                    activeSection === section.id
                      ? 'text-content-primary after:scale-x-100 after:bg-brand-peach'
                      : 'text-content-secondary after:scale-x-0 after:bg-brand-peach hover:text-brand-peach hover:after:scale-x-100'
                  }`}
                >
                  <span className={`font-mono text-[10px] ${
                    activeSection === section.id ? 'text-brand-peach' : 'text-content-secondary/70'
                  }`}>
                    {String(index + 1).padStart(2, '0')}
                  </span>
                  <span>{section.label}</span>
                </button>
              ))}
            </div>
          </nav>

          <section
            id="overview"
            data-vault-detail-section="overview"
            aria-labelledby={`vault-section-heading-${tranche.id}-overview`}
            className="space-y-4"
            style={{ scrollMarginTop: sectionScrollOffset }}
          >
            <div>
              <p className="text-xs font-semibold uppercase tracking-[0.16em] text-content-secondary">
                Vault details
              </p>
              <h2
                id={`vault-section-heading-${tranche.id}-overview`}
                className="mt-1 text-2xl font-semibold text-content-primary"
              >
                Overview
              </h2>
            </div>
            <OverviewTab
              tranche={tranche}
              liveData={liveData}
              snapshot={snapshot}
              isConnected={isConnected}
              epochCountdownSeconds={epochCountdownSeconds}
            />
          </section>

          {tranche.id === 'junior' && (
            <JuniorMarketExposure pool={snapshot.pool} scrollMarginTop={sectionScrollOffset} />
          )}

          {performance ? (
            <section
              id="performance"
              data-vault-detail-section="performance"
              aria-labelledby={`vault-section-heading-${tranche.id}-performance`}
              className="space-y-4"
              style={{ scrollMarginTop: sectionScrollOffset }}
            >
              <div>
                <p className="text-xs font-semibold uppercase tracking-[0.16em] text-content-secondary">
                  Historical results
                </p>
                <h2
                  id={`vault-section-heading-${tranche.id}-performance`}
                  className="mt-1 text-2xl font-semibold text-content-primary"
                >
                  Performance
                </h2>
              </div>
              <PerformanceTab tranche={tranche} performance={performance} />
            </section>
          ) : null}

          <section
            id="your-position"
            data-vault-detail-section="activity"
            aria-labelledby={`vault-section-heading-${tranche.id}-activity`}
            className="space-y-4"
            style={{ scrollMarginTop: sectionScrollOffset }}
          >
            <div>
              <p className="text-xs font-semibold uppercase tracking-[0.16em] text-content-secondary">
                Wallet and pending activity
              </p>
              <h2
                id={`vault-section-heading-${tranche.id}-activity`}
                className="mt-1 text-2xl font-semibold text-content-primary"
              >
                Your position
              </h2>
            </div>
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
              requestDiscoveryStale={vaultRequests.discoveryStale}
              onRefreshRequests={vaultRequests.refresh}
              onSwitchNetwork={onSwitchNetwork}
            />
          </section>
        </div>

        <div className="lg:sticky" style={{ top: stickyElementTop }}>
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
              scrollToSection('position')
            }}
          />
        </div>
      </div>

      <VaultActivitySection
        holders={vaultActivity.holders}
        activity={vaultActivity.activity}
        tranche={tranche.id}
        scrollMarginTop={sectionScrollOffset}
        isLoading={vaultActivity.isLoading}
        isError={vaultActivity.isError}
        isStale={vaultActivity.isStale}
      />
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
    void openAppKit()
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

export { TRANCHES as VAULT_TRANCHES }
export default Vaults
