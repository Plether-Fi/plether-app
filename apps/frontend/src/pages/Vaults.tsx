import { useMemo, useState, type KeyboardEvent, type ReactNode } from 'react'
import { useAppKit } from '@reown/appkit/react'
import { formatUnits, parseUnits, zeroAddress, type Address } from 'viem'
import { useAccount, useChainId, useReadContracts } from 'wagmi'
import { Link, useParams } from 'react-router-dom'
import { TokenInput } from '../components/TokenInput'
import { Alert, Badge, Button, Modal, Tooltip } from '../components/ui'
import { syncAppKitModalStyleOverrides } from '../config/wagmi'
import { ERC20_ABI, PERPS_HOUSE_POOL_ABI, TRANCHE_VAULT_READ_ABI } from '../contracts/abis'
import {
  PERPS_ARBITRUM_SEPOLIA,
  PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
} from '../contracts/perpsAddresses'
import { useSwitchToArbitrumSepolia, useVaultTransactions } from '../hooks'

type TrancheId = 'senior' | 'junior'
type DetailTab = 'overview' | 'performance' | 'risk' | 'activity'
type ActionMode = 'deposit' | 'withdraw'
type DataStatus = 'live' | 'partial' | 'syncing' | 'unavailable'

interface TrancheDefinition {
  id: TrancheId
  name: string
  token: string
  icon: string
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
  chartColor: string
  markClassName: string
  valueClassName: string
  barClassName: string
  featureItems: string[]
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
  markFresh?: boolean
  oracleFrozen?: boolean
  degradedMode?: boolean
  seniorImpaired?: boolean
  seniorImpairmentGapUsdc?: bigint
  seniorPoolWithdrawCapUsdc?: bigint
  juniorPoolWithdrawCapUsdc?: bigint
}

interface TrancheLiveData {
  totalAssets?: bigint
  totalSupply?: bigint
  userShares?: bigint
  maxDeposit?: bigint
  maxRequestDeposit?: bigint
  maxWithdraw?: bigint
  allowance?: bigint
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
const EXPLORER_BASE_URL = 'https://sepolia.arbiscan.io/address'
const DEPOSIT_PROBE_ACCOUNT = '0x000000000000000000000000000000000000dEaD' as Address

const TRANCHES: Record<TrancheId, TrancheDefinition> = {
  senior: {
    id: 'senior',
    name: 'Senior Vault',
    token: 'psLP',
    icon: 'shield',
    eyebrow: 'Priority capital',
    shortDescription: 'Targeted return with first access to free LP liquidity.',
    description:
      'Senior exchanges residual upside for relative protection. It receives a Junior-funded target coupon, is restored toward its high-water mark before Junior receives new revenue, and absorbs losses only after Junior is exhausted.',
    returnModel: 'Target coupon funded by Junior capital',
    lossPriority: 'Second loss, after Junior',
    withdrawalPriority: 'First LP claim on free liquidity',
    upside: 'Target coupon and restoration priority',
    primaryRisk: 'Coupon can stop and principal can still be impaired',
    riskLabel: 'Lower relative risk',
    riskVariant: 'info',
    targetReturn: 'Target coupon',
    chartColor: '#FFAB96',
    markClassName: 'border-brand-peach/60 bg-brand-peach/10 text-brand-peach',
    valueClassName: 'text-brand-peach',
    barClassName: 'bg-brand-peach',
    featureItems: [
      'Protected by the Junior first-loss buffer',
      'Restored to its high-water mark before Junior receives residual revenue',
      'First LP claim on physically free HousePool liquidity',
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
    icon: 'rocket_launch',
    eyebrow: 'Residual capital',
    shortDescription: 'First-loss capital with variable residual upside.',
    description:
      'Junior funds the Senior target coupon and absorbs HousePool losses first. In exchange, it receives residual realized trading revenue after Senior restoration and coupon obligations are satisfied.',
    returnModel: 'Residual HousePool performance',
    lossPriority: 'First loss',
    withdrawalPriority: 'Liquidity remaining above the Senior claim',
    upside: 'Variable residual trading revenue',
    primaryRisk: 'Can be partially or completely wiped before Senior is impaired',
    riskLabel: 'Higher relative risk',
    riskVariant: 'warning',
    targetReturn: 'Variable residual',
    chartColor: '#00FF99',
    markClassName: 'border-positive/60 bg-positive/10 text-positive',
    valueClassName: 'text-positive',
    barClassName: 'bg-positive',
    featureItems: [
      'Receives residual realized trading revenue',
      'Higher upside participation after Senior obligations',
      'Direct exposure to carry, positive VPI, and collected trader losses',
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

function minBigInt(left: bigint, right: bigint): bigint {
  return left < right ? left : right
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
        functionName: 'maxDeposit',
        args: [depositReceiver],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.seniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'maxWithdraw',
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
        functionName: 'maxDeposit',
        args: [depositReceiver],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: PERPS_ARBITRUM_SEPOLIA.juniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'maxWithdraw',
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
        address: PERPS_ARBITRUM_SEPOLIA.seniorVault,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'maxRequestDeposit',
        args: [depositReceiver],
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
    ],
    query: {
      refetchInterval: 30_000,
    },
  })

  return useMemo(() => {
    const results = data as readonly ContractResult[] | undefined
    const poolResult = readResult(results, 0)
    const totalAssetsUsdc = asBigInt(tupleValue(poolResult, 0, 'totalAssetsUsdc'))
    const freeUsdc = asBigInt(tupleValue(poolResult, 1, 'freeUsdc'))
    const withdrawalReservedUsdc = asBigInt(tupleValue(poolResult, 2, 'withdrawalReservedUsdc'))
    const pendingRecapitalizationUsdc = asBigInt(
      tupleValue(poolResult, 3, 'pendingRecapitalizationUsdc')
    )
    const pendingTradingRevenueUsdc = asBigInt(
      tupleValue(poolResult, 4, 'pendingTradingRevenueUsdc')
    )
    const seniorPrincipalUsdc = asBigInt(tupleValue(poolResult, 5, 'seniorPrincipalUsdc'))
    const juniorPrincipalUsdc = asBigInt(tupleValue(poolResult, 6, 'juniorPrincipalUsdc'))
    const seniorHighWaterMarkUsdc = asBigInt(
      tupleValue(poolResult, 7, 'seniorHighWaterMarkUsdc')
    )
    const markFresh = asBoolean(tupleValue(poolResult, 8, 'markFresh'))
    const oracleFrozen = asBoolean(tupleValue(poolResult, 9, 'oracleFrozen'))
    const degradedMode = asBoolean(tupleValue(poolResult, 10, 'degradedMode'))
    const seniorAssets = asBigInt(readResult(results, 1))
    const seniorSupply = asBigInt(readResult(results, 2))
    const seniorUserShares = asBigInt(readResult(results, 3))
    const seniorMaxDeposit = asBigInt(readResult(results, 4))
    const seniorMaxWithdraw = asBigInt(readResult(results, 5))
    const juniorAssets = asBigInt(readResult(results, 6))
    const juniorSupply = asBigInt(readResult(results, 7))
    const juniorUserShares = asBigInt(readResult(results, 8))
    const juniorMaxDeposit = asBigInt(readResult(results, 9))
    const juniorMaxWithdraw = asBigInt(readResult(results, 10))
    const walletUsdc = asBigInt(readResult(results, 11))
    const seniorMaxRequestDeposit = asBigInt(readResult(results, 12))
    const juniorMaxRequestDeposit = asBigInt(readResult(results, 13))
    const seniorAllowance = asBigInt(readResult(results, 14))
    const juniorAllowance = asBigInt(readResult(results, 15))
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
    const seniorPoolWithdrawCapUsdc = freeUsdc !== undefined && seniorPrincipalUsdc !== undefined
      ? minBigInt(freeUsdc, seniorPrincipalUsdc)
      : undefined
    const freeAboveSenior = freeUsdc !== undefined && seniorPrincipalUsdc !== undefined
      ? freeUsdc > seniorPrincipalUsdc
        ? freeUsdc - seniorPrincipalUsdc
        : 0n
      : undefined
    const juniorPoolWithdrawCapUsdc = juniorPrincipalUsdc !== undefined
      && freeAboveSenior !== undefined
      ? minBigInt(juniorPrincipalUsdc, freeAboveSenior)
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
      && seniorMaxDeposit !== undefined
      && seniorMaxRequestDeposit !== undefined
    const hasJuniorCoreData = juniorAssets !== undefined
      && juniorSupply !== undefined
      && juniorMaxDeposit !== undefined
      && juniorMaxRequestDeposit !== undefined
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
        markFresh,
        oracleFrozen,
        degradedMode,
        seniorImpaired,
        seniorImpairmentGapUsdc,
        seniorPoolWithdrawCapUsdc,
        juniorPoolWithdrawCapUsdc,
      },
      walletUsdc,
      hasLivePoolData,
      tranches: {
        senior: {
          totalAssets: seniorAssets,
          totalSupply: seniorSupply,
          userShares: seniorUserShares,
          maxDeposit: seniorMaxDeposit,
          maxRequestDeposit: seniorMaxRequestDeposit,
          maxWithdraw: seniorMaxWithdraw,
          allowance: seniorAllowance,
          sharePrice: calculateSharePrice(seniorAssets, seniorSupply),
          hasCoreData: hasSeniorCoreData,
          hasDepositData: seniorAllowance !== undefined,
          hasUserData: seniorUserShares !== undefined && seniorMaxWithdraw !== undefined,
        },
        junior: {
          totalAssets: juniorAssets,
          totalSupply: juniorSupply,
          userShares: juniorUserShares,
          maxDeposit: juniorMaxDeposit,
          maxRequestDeposit: juniorMaxRequestDeposit,
          maxWithdraw: juniorMaxWithdraw,
          allowance: juniorAllowance,
          sharePrice: calculateSharePrice(juniorAssets, juniorSupply),
          hasCoreData: hasJuniorCoreData,
          hasDepositData: juniorAllowance !== undefined,
          hasUserData: juniorUserShares !== undefined && juniorMaxWithdraw !== undefined,
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

function formatCompactUsd(amount: bigint | undefined): string {
  const formatted = formatCompactUsdc(amount)
  return formatted === '--' ? formatted : `$${formatted}`
}

function formatFullUsd(amount: bigint | undefined, maximumFractionDigits = 2): string {
  const formatted = formatFullUsdc(amount, maximumFractionDigits)
  return formatted === '--' ? formatted : `$${formatted}`
}

function formatVaultLimit(amount: bigint | undefined): string {
  if (amount === undefined) return '--'
  if (amount >= 2n ** 255n) return 'No contract cap'
  return formatFullUsd(amount)
}

function formatShares(amount: bigint | undefined): string {
  if (amount === undefined) return '--'
  const value = Number(formatUnits(amount, SHARE_DECIMALS))
  return new Intl.NumberFormat('en-US', {
    maximumFractionDigits: 4,
  }).format(value)
}

function formatSharePrice(value: number | undefined): string {
  if (value === undefined) return '--'
  return `$${value.toFixed(4)}`
}

function parseUsdc(value: string): bigint {
  try {
    return value.trim() ? parseUnits(value, USDC_DECIMALS) : 0n
  } catch {
    return 0n
  }
}

function getDepositMode(liveData: TrancheLiveData): string {
  if (liveData.maxDeposit === undefined || liveData.maxRequestDeposit === undefined) {
    return 'Availability unavailable'
  }
  if (liveData.maxDeposit > 0n) {
    return 'Immediate deposit'
  }
  if (liveData.maxRequestDeposit > 0n) {
    return 'Pending deposit epoch'
  }
  return 'Deposit unavailable'
}

function formatAddress(address: Address): string {
  return `${address.slice(0, 6)}...${address.slice(-4)}`
}

function dataStatusBadge(status: DataStatus) {
  if (status === 'live') {
    return (
      <Badge variant="success">
        <span className="mr-1.5 h-1.5 w-1.5 rounded-full bg-current" />
        Live onchain
      </Badge>
    )
  }

  if (status === 'syncing') {
    return (
      <Badge variant="warning">
        <span className="mr-1.5 h-1.5 w-1.5 animate-pulse rounded-full bg-current" />
        Syncing
      </Badge>
    )
  }

  if (status === 'partial') {
    return <Badge variant="warning">Partial onchain data</Badge>
  }

  return <Badge variant="danger">Onchain data unavailable</Badge>
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
}: {
  label: string
  value: ReactNode
  subvalue?: ReactNode
  tooltip?: ReactNode
  valueClassName?: string
}) {
  return (
    <div className="min-w-0 border-l border-brand-border/25 pl-4 first:border-l-0 first:pl-0">
      <StatLabel tooltip={tooltip}>{label}</StatLabel>
      <dd className={`mt-2 truncate text-2xl font-semibold ${valueClassName}`}>{value}</dd>
      {subvalue ? <p className="mt-1 text-xs text-content-secondary">{subvalue}</p> : null}
    </div>
  )
}

function TrancheMark({ tranche, size = 'lg' }: { tranche: TrancheDefinition; size?: 'md' | 'lg' }) {
  return (
    <div
      className={`flex shrink-0 items-center justify-center border ${tranche.markClassName} ${
        size === 'lg' ? 'h-14 w-14' : 'h-11 w-11'
      }`}
      aria-hidden="true"
    >
      <span className={`material-symbols-outlined ${size === 'lg' ? 'text-3xl' : 'text-2xl'}`}>
        {tranche.icon}
      </span>
    </div>
  )
}

function MiniPerformanceChart({ tranche }: { tranche: TrancheDefinition }) {
  return (
    <div
      className="relative flex h-20 items-center justify-center overflow-hidden"
      aria-label={`${tranche.name} seven-day performance history unavailable`}
      role="status"
    >
      <div className="absolute inset-x-0 top-1/2 border-t border-dashed border-brand-border/25" />
      <span className="relative bg-surface-panel px-3 text-xs font-medium uppercase tracking-[0.12em] text-content-secondary">
        7d history unavailable
      </span>
    </div>
  )
}

function TrancheCard({
  tranche,
  liveData,
}: {
  tranche: TrancheDefinition
  liveData: TrancheLiveData
}) {
  return (
    <Link
      to={`/vaults/${tranche.id}`}
      aria-label={`View ${tranche.name}`}
      className="group block border border-brand-border/30 bg-surface-panel transition-colors hover:border-brand-peach/70 focus-visible:border-brand-peach focus-visible:outline-none"
    >
      <article>
        <div className="flex items-start justify-between gap-4 border-b border-brand-border/25 p-5">
          <div className="flex min-w-0 items-start gap-3">
            <TrancheMark tranche={tranche} size="md" />
            <div className="min-w-0">
              <p className="text-xs font-semibold uppercase tracking-[0.16em] text-content-secondary">
                {tranche.eyebrow}
              </p>
              <h2 className="mt-1 text-2xl font-semibold text-content-primary">{tranche.name}</h2>
            </div>
          </div>
          <Badge variant={tranche.riskVariant}>{tranche.riskLabel}</Badge>
        </div>

        <div className="space-y-5 p-5">
          <p className="min-h-12 text-sm leading-6 text-content-secondary">
            {tranche.shortDescription}
          </p>

          <dl className="grid grid-cols-3 gap-3">
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
            <div>
              <dt
                className="text-xs font-medium uppercase tracking-[0.14em] text-content-secondary"
                title="Seven-day annualized performance will appear when the tranche-history indexer is connected."
              >
                7d APY
              </dt>
              <dd className={`mt-2 text-xl font-semibold ${tranche.valueClassName}`}>
                --
              </dd>
              <p className="mt-1 text-[10px] uppercase tracking-[0.12em] text-content-secondary">Not indexed</p>
            </div>
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

          <div className="border-y border-brand-border/20 py-2">
            <MiniPerformanceChart tranche={tranche} />
          </div>

          <ul className="space-y-2">
            {tranche.featureItems.map((item) => (
              <li key={item} className="flex gap-2 text-sm leading-5 text-content-secondary">
                <span className={`material-symbols-outlined mt-0.5 text-base ${tranche.valueClassName}`}>
                  check
                </span>
                <span>{item}</span>
              </li>
            ))}
          </ul>
        </div>

        <div className="flex items-center justify-between border-t border-brand-border/25 px-5 py-4 text-sm font-semibold text-content-primary">
          <span>Explore {tranche.name}</span>
          <span className="material-symbols-outlined transition-transform group-hover:translate-x-1">
            arrow_forward
          </span>
        </div>
      </article>
    </Link>
  )
}

function VaultsOverview({ snapshot }: { snapshot: VaultsSnapshot }) {
  const pool = snapshot.pool
  const totalCapital = pool.seniorPrincipalUsdc !== undefined
    && pool.juniorPrincipalUsdc !== undefined
    ? pool.seniorPrincipalUsdc + pool.juniorPrincipalUsdc
    : undefined
  const seniorShare = totalCapital !== undefined
    && totalCapital > 0n
    && pool.seniorPrincipalUsdc !== undefined
    ? (Number(pool.seniorPrincipalUsdc) / Number(totalCapital)) * 100
    : undefined
  const freeLiquidityRatio = pool.totalAssetsUsdc !== undefined
    && pool.totalAssetsUsdc > 0n
    && pool.freeUsdc !== undefined
    ? (Number(pool.freeUsdc) / Number(pool.totalAssetsUsdc)) * 100
    : undefined
  const seniorMaxDeposit = snapshot.tranches.senior.maxDeposit
  const juniorMaxDeposit = snapshot.tranches.junior.maxDeposit
  const seniorMaxRequestDeposit = snapshot.tranches.senior.maxRequestDeposit
  const juniorMaxRequestDeposit = snapshot.tranches.junior.maxRequestDeposit
  const depositAvailabilityKnown = seniorMaxDeposit !== undefined
    && juniorMaxDeposit !== undefined
    && seniorMaxRequestDeposit !== undefined
    && juniorMaxRequestDeposit !== undefined
  const allImmediate = depositAvailabilityKnown
    && seniorMaxDeposit > 0n
    && juniorMaxDeposit > 0n
  const allPending = depositAvailabilityKnown
    && seniorMaxDeposit === 0n
    && juniorMaxDeposit === 0n
    && seniorMaxRequestDeposit > 0n
    && juniorMaxRequestDeposit > 0n
  const allUnavailable = depositAvailabilityKnown
    && seniorMaxDeposit === 0n
    && juniorMaxDeposit === 0n
    && seniorMaxRequestDeposit === 0n
    && juniorMaxRequestDeposit === 0n
  const depositRoute = !snapshot.hasLivePoolData || !depositAvailabilityKnown
    ? 'Availability unavailable'
    : pool.seniorImpaired === true
      ? 'Unavailable'
      : pool.degradedMode === true || pool.markFresh === false
        ? 'Check live gate'
        : allImmediate
          ? 'Immediate'
          : allPending
            ? 'Pending epoch'
            : allUnavailable
              ? 'Unavailable'
              : 'Varies by tranche'
  const depositRouteDetail = depositRoute === 'Immediate'
    ? 'Both tranches mint shares in the deposit transaction'
    : depositRoute === 'Varies by tranche'
      ? 'Open a tranche to inspect its live entry path'
      : depositRoute === 'Availability unavailable'
        ? 'Awaiting complete HousePool and vault reads'
        : depositRoute === 'Pending epoch'
          ? 'Requests activate in a future epoch; lifecycle controls are not enabled yet'
          : 'Open a tranche to inspect its live entry path'
  const marketState = pool.degradedMode === undefined
    || pool.oracleFrozen === undefined
    || pool.markFresh === undefined
    ? 'Unavailable'
    : pool.degradedMode
      ? 'Degraded'
      : pool.oracleFrozen
        ? 'Oracle frozen'
        : !pool.markFresh
          ? 'Mark stale'
          : 'Operational'

  return (
    <div className="space-y-8">
      <section className="border border-brand-border/30 bg-surface-panel">
        <div className="flex flex-col gap-6 border-b border-brand-border/25 p-6 lg:flex-row lg:items-end lg:justify-between">
          <div className="max-w-3xl">
            <div className="flex flex-wrap items-center gap-3">
              <p className="text-xs font-semibold uppercase tracking-[0.2em] text-brand-peach">
                Plether HousePool
              </p>
              {dataStatusBadge(snapshot.status)}
            </div>
            <h1 className="mt-3 text-3xl font-semibold tracking-tight text-content-primary sm:text-4xl">
              Supply the balance sheet behind the market.
            </h1>
            <p className="mt-3 max-w-2xl text-base leading-7 text-content-secondary">
              Deposit USDC into Senior or Junior vault shares. Both tranches underwrite the same
              HousePool, but they take different positions in the loss, revenue, and withdrawal
              waterfall.
            </p>
          </div>

          <div className="flex flex-wrap gap-3">
            <a
              href="https://docs.plether.com"
              target="_blank"
              rel="noreferrer"
              className="inline-flex items-center gap-2 border border-brand-border/40 px-4 py-2 text-sm font-semibold text-content-primary transition-colors hover:border-brand-peach hover:text-brand-peach hover:underline hover:underline-offset-4"
            >
              Read the LP guide
              <span className="material-symbols-outlined text-lg">open_in_new</span>
            </a>
            <button
              type="button"
              onClick={snapshot.refresh}
              className="inline-flex items-center gap-2 border border-brand-border/40 px-4 py-2 text-sm font-semibold text-content-secondary transition-colors hover:border-brand-peach hover:text-content-primary hover:underline hover:underline-offset-4"
            >
              <span className="material-symbols-outlined text-lg">refresh</span>
              Refresh
            </button>
          </div>
        </div>

        <dl className="grid grid-cols-2 gap-x-4 gap-y-6 p-6 lg:grid-cols-5">
          <PoolStat
            label="HousePool assets"
            value={formatCompactUsd(pool.totalAssetsUsdc)}
            subvalue={`${formatFullUsdc(pool.totalAssetsUsdc, 0)} USDC`}
            tooltip="Canonical physical HousePool assets. This can differ from the sum of tranche accounting NAV."
          />
          <PoolStat
            label="Free liquidity"
            value={formatCompactUsd(pool.freeUsdc)}
            subvalue={freeLiquidityRatio === undefined ? 'Live value unavailable' : `${freeLiquidityRatio.toFixed(1)}% of total assets`}
            tooltip="Physical USDC remaining after protected withdrawal reserves. This is not the same as total tranche NAV."
          />
          <PoolStat
            label="Withdrawal reserve"
            value={formatCompactUsd(pool.withdrawalReservedUsdc)}
            subvalue="Trader liabilities protected first"
            tooltip="Capital reserved for bounded trader liability, claims, and other protected amounts."
          />
          <PoolStat
            label="Deposit route"
            value={depositRoute}
            subvalue={depositRouteDetail}
            valueClassName={pool.seniorImpaired ? 'text-brand-orange' : 'text-warning'}
            tooltip="A zero ERC-4626 maxDeposit means immediate entry is unavailable. It does not prove that a pending epoch request is currently allowed."
          />
          <PoolStat
            label="Market state"
            value={marketState}
            subvalue={pool.markFresh === undefined ? 'Awaiting HousePool read' : pool.markFresh ? 'Oracle mark fresh' : 'Reconciliation may be restricted'}
            valueClassName={
              marketState === 'Operational'
                ? 'text-positive'
                : marketState === 'Unavailable'
                  ? 'text-content-secondary'
                  : 'text-warning'
            }
          />
        </dl>
      </section>

      <section>
        <div className="mb-4 flex flex-col gap-2 sm:flex-row sm:items-end sm:justify-between">
          <div>
            <p className="text-xs font-semibold uppercase tracking-[0.18em] text-content-secondary">
              Choose a tranche
            </p>
            <h2 className="mt-1 text-2xl font-semibold text-content-primary">USDC vaults</h2>
          </div>
          <p className="max-w-xl text-sm leading-5 text-content-secondary">
            Do not choose on APY alone. Senior and Junior are different claims on the same pool,
            not separate yield strategies.
          </p>
        </div>

        <div className="grid gap-6 lg:grid-cols-2">
          <TrancheCard tranche={TRANCHES.senior} liveData={snapshot.tranches.senior} />
          <TrancheCard tranche={TRANCHES.junior} liveData={snapshot.tranches.junior} />
        </div>
      </section>

      <section className="border border-brand-border/30 bg-surface-panel">
        <div className="border-b border-brand-border/25 p-5">
          <p className="text-xs font-semibold uppercase tracking-[0.18em] text-content-secondary">
            Capital structure
          </p>
          <h2 className="mt-1 text-2xl font-semibold text-content-primary">
            One pool, two economic claims
          </h2>
        </div>

        <div className="grid gap-px bg-brand-border/20 lg:grid-cols-3">
          <div className="bg-surface-panel p-5">
            <span className="material-symbols-outlined text-3xl text-brand-orange">trending_down</span>
            <h3 className="mt-3 text-lg font-semibold text-content-primary">When the pool loses</h3>
            <p className="mt-2 text-sm leading-6 text-content-secondary">
              Junior absorbs realized losses first. Senior is affected only after Junior capital
              reaches zero.
            </p>
            <div className="mt-4 flex items-center gap-2 text-xs font-semibold">
              <span className="border border-positive/40 bg-positive/10 px-2 py-1 text-positive">1 Junior</span>
              <span className="material-symbols-outlined text-base text-content-secondary">arrow_forward</span>
              <span className="border border-brand-peach/40 bg-brand-peach/10 px-2 py-1 text-brand-peach">2 Senior</span>
            </div>
          </div>

          <div className="bg-surface-panel p-5">
            <span className="material-symbols-outlined text-3xl text-positive">trending_up</span>
            <h3 className="mt-3 text-lg font-semibold text-content-primary">When the pool earns</h3>
            <p className="mt-2 text-sm leading-6 text-content-secondary">
              Any Senior impairment is restored first. Residual realized revenue then accrues to
              Junior.
            </p>
            <div className="mt-4 flex items-center gap-2 text-xs font-semibold">
              <span className="border border-brand-peach/40 bg-brand-peach/10 px-2 py-1 text-brand-peach">1 Restore Senior</span>
              <span className="material-symbols-outlined text-base text-content-secondary">arrow_forward</span>
              <span className="border border-positive/40 bg-positive/10 px-2 py-1 text-positive">2 Junior residual</span>
            </div>
          </div>

          <div className="bg-surface-panel p-5">
            <span className="material-symbols-outlined text-3xl text-warning">account_balance</span>
            <h3 className="mt-3 text-lg font-semibold text-content-primary">When LPs withdraw</h3>
            <p className="mt-2 text-sm leading-6 text-content-secondary">
              Trader claims and reserved liabilities come first. Senior then has priority over
              Junior for the remaining free LP cash.
            </p>
            <div className="mt-4 flex items-center gap-2 text-xs font-semibold">
              <span className="border border-warning/40 bg-warning-bg px-2 py-1 text-warning">1 Traders</span>
              <span className="material-symbols-outlined text-base text-content-secondary">arrow_forward</span>
              <span className="border border-brand-peach/40 bg-brand-peach/10 px-2 py-1 text-brand-peach">2 Senior</span>
              <span className="material-symbols-outlined text-base text-content-secondary">arrow_forward</span>
              <span className="border border-positive/40 bg-positive/10 px-2 py-1 text-positive">3 Junior</span>
            </div>
          </div>
        </div>

        {seniorShare === undefined ? (
          <div className="border-t border-brand-border/25 p-5 text-sm text-content-secondary">
            Current tranche-capital allocation is unavailable until the HousePool read succeeds.
          </div>
        ) : (
          <div className="space-y-3 border-t border-brand-border/25 p-5">
            <div className="flex items-center justify-between text-xs font-semibold uppercase tracking-[0.12em] text-content-secondary">
              <span>Current tranche capital</span>
              <span>{seniorShare.toFixed(1)}% Senior / {(100 - seniorShare).toFixed(1)}% Junior</span>
            </div>
            <div className="flex h-3 w-full overflow-hidden border border-brand-border/30 bg-app-bg">
              <div className="bg-brand-peach" style={{ width: `${seniorShare.toFixed(2)}%` }} />
              <div className="bg-positive" style={{ width: `${(100 - seniorShare).toFixed(2)}%` }} />
            </div>
            <div className="flex flex-wrap justify-between gap-3 text-sm">
              <span className="text-brand-peach">
                Senior {formatCompactUsd(pool.seniorPrincipalUsdc)}
              </span>
              <span className="text-positive">
                Junior {formatCompactUsd(pool.juniorPrincipalUsdc)}
              </span>
            </div>
          </div>
        )}
      </section>

      <Alert variant="warning" title="Vault shares can lose value">
        Neither tranche is a savings account. Senior changes the order in which risk is absorbed;
        it does not remove smart-contract, stablecoin, oracle, liquidity, or trading-loss risk.
        Seven-day APY is intentionally left unavailable until a live tranche-performance indexer is
        connected.
      </Alert>
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
  const depositState = pool.seniorImpaired === true
    ? 'Unavailable'
    : pool.degradedMode === true || pool.markFresh === false
      ? 'Check live gate'
      : depositMode

  return (
    <div className="space-y-6">
      <div className="grid gap-3 sm:grid-cols-2 xl:grid-cols-4">
        <DetailMetric
          label="Your position"
          value={isConnected && positionValue !== undefined ? `$${positionValue.toLocaleString('en-US', { maximumFractionDigits: 2 })}` : '--'}
          detail={isConnected ? `${formatShares(liveData.userShares)} ${tranche.token}` : 'Connect a wallet to view'}
        />
        <DetailMetric
          label="Max withdraw"
          value={isConnected ? formatCompactUsd(liveData.maxWithdraw) : '--'}
          detail="Holder-level live limit"
          tone={(liveData.maxWithdraw ?? 0n) > 0n ? 'positive' : 'default'}
        />
        <DetailMetric
          label="Pool-level cap"
          value={formatCompactUsd(poolWithdrawCap)}
          detail={tranche.id === 'senior' ? 'Before holder cooldown' : 'After complete Senior claim'}
        />
        <DetailMetric
          label="Deposit mode"
          value={depositState}
          detail={
            depositMode === 'Immediate deposit'
              ? 'Shares mint in the deposit transaction'
              : depositMode === 'Pending deposit epoch'
                ? 'Funded requests activate two epoch IDs ahead'
                : 'Live vault safety gates currently block entry'
          }
          tone={pool.seniorImpaired === true ? 'negative' : 'warning'}
        />
      </div>

      <div className="grid gap-6 xl:grid-cols-2">
        <section className="border border-brand-border/30 bg-surface-panel p-5">
          <h3 className="text-lg font-semibold text-content-primary">Vault configuration</h3>
          <dl className="mt-3">
            <DetailRow label="Asset" value="USDC" />
            <DetailRow label="Vault share" value={tranche.token} />
            <DetailRow label="Vault standard" value="ERC-4626 + Plether epochs" />
            <DetailRow label="Network" value="Arbitrum Sepolia" />
            <DetailRow label="Deposit path" value={depositMode} />
            <DetailRow
              label="Immediate deposit max"
              value={formatVaultLimit(liveData.maxDeposit)}
            />
            <DetailRow
              label="Pending request max"
              value={formatVaultLimit(liveData.maxRequestDeposit)}
            />
            <DetailRow label="Pending epoch length" value="1 hour" />
            <DetailRow label="Request lead time" value="2 epoch IDs" />
            <DetailRow label="Withdrawal cooldown" value="1 hour after deposit/withdraw" />
            <DetailRow
              label="Vault contract"
              value={(
                <a
                  href={`${EXPLORER_BASE_URL}/${tranche.address}`}
                  target="_blank"
                  rel="noreferrer"
                  className="inline-flex items-center gap-1 text-brand-peach hover:underline"
                >
                  {formatAddress(tranche.address)}
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
          </dl>
        </section>
      </div>

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

function PerformanceChart({ tranche }: { tranche: TrancheDefinition }) {
  return (
    <section className="border border-brand-border/30 bg-surface-panel">
      <div className="flex flex-col gap-4 border-b border-brand-border/25 p-5 sm:flex-row sm:items-start sm:justify-between">
        <div>
          <div className="flex items-center gap-2">
            <h3 className="text-lg font-semibold text-content-primary">Return history</h3>
            <Badge variant="default">Indexer unavailable</Badge>
          </div>
          <p className="mt-1 text-sm text-content-secondary">
            Historical share-price checkpoints will populate this chart once the LP performance
            endpoint is connected.
          </p>
        </div>
        <span className="border border-brand-border/30 bg-app-bg px-3 py-1.5 text-xs font-semibold uppercase text-content-secondary">
          7d / 30d
        </span>
      </div>

      <div className="relative p-5">
        <svg
          viewBox="0 0 600 210"
          className="h-64 w-full"
          preserveAspectRatio="none"
          aria-label={`${tranche.name} performance history unavailable`}
          role="img"
        >
          {[46, 92, 138, 184].map((y) => (
            <line
              key={y}
              x1="16"
              y1={y}
              x2="584"
              y2={y}
              stroke="rgba(255,171,150,0.12)"
              strokeWidth="1"
            />
          ))}
          <line
            x1="16"
            y1="105"
            x2="584"
            y2="105"
            stroke={tranche.chartColor}
            strokeDasharray="6 8"
            strokeOpacity="0.35"
            strokeWidth="2"
          />
        </svg>
        <div className="pointer-events-none absolute inset-0 flex items-center justify-center">
          <span className="border border-brand-border/30 bg-app-bg px-4 py-2 text-sm font-medium text-content-secondary">
            No indexed performance data
          </span>
        </div>
      </div>
    </section>
  )
}

function PerformanceTab({
  tranche,
  liveData,
}: {
  tranche: TrancheDefinition
  liveData: TrancheLiveData
}) {
  return (
    <div className="space-y-6">
      <PerformanceChart tranche={tranche} />

      <div className="grid gap-3 sm:grid-cols-2 xl:grid-cols-4">
        <DetailMetric
          label="7d APY"
          value="--"
          detail="Performance indexer unavailable"
        />
        <DetailMetric
          label="30d APY"
          value="--"
          detail="Performance indexer unavailable"
        />
        <DetailMetric
          label="Share price"
          value={formatSharePrice(liveData.sharePrice)}
          detail="Live accounting value"
        />
        <DetailMetric
          label="Return model"
          value={tranche.targetReturn}
          detail={tranche.id === 'senior' ? 'Not guaranteed' : 'After Senior obligations'}
          tone="warning"
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

      <Alert variant="info" title="How performance will be calculated">
        Seven-day and 30-day APY should be annualized from indexed vault share-price checkpoints.
        Actual returns are expressed through changing share value, not a separate reward, and can
        be negative.
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
                ['Withdrawal priority', 'First LP claim on free cash', 'Cash above full Senior claim'],
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
  isConnected,
}: {
  tranche: TrancheDefinition
  liveData: TrancheLiveData
  isConnected: boolean
}) {
  const positionValue = liveData.userShares !== undefined && liveData.sharePrice !== undefined
    ? Number(formatUnits(liveData.userShares, SHARE_DECIMALS)) * liveData.sharePrice
    : undefined
  const hasUserBalance = isConnected && liveData.userShares !== undefined

  return (
    <div className="space-y-6">
      <section className="border border-brand-border/30 bg-surface-panel p-5">
        <div className="flex flex-col gap-4 sm:flex-row sm:items-start sm:justify-between">
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
          {hasUserBalance
            ? <Badge variant="success">Onchain balance</Badge>
            : isConnected
              ? <Badge variant="warning">Balance unavailable</Badge>
              : <Badge>Read-only</Badge>}
        </div>

        <div className="mt-5 grid gap-3 sm:grid-cols-3">
          <DetailMetric
            label="Current value"
            value={isConnected && positionValue !== undefined ? `$${positionValue.toLocaleString('en-US', { maximumFractionDigits: 2 })}` : '--'}
          />
          <DetailMetric
            label="Share price"
            value={formatSharePrice(liveData.sharePrice)}
          />
          <DetailMetric
            label="Withdrawable now"
            value={isConnected ? formatFullUsd(liveData.maxWithdraw) : '--'}
            tone={(liveData.maxWithdraw ?? 0n) > 0n ? 'positive' : 'default'}
          />
        </div>
      </section>

      <section className="border border-brand-border/30 bg-surface-panel">
        <div className="border-b border-brand-border/25 p-5">
          <h3 className="text-lg font-semibold text-content-primary">Vault activity</h3>
          <p className="mt-1 text-sm text-content-secondary">
            Deposits, pending epochs, claims, and withdrawals.
          </p>
        </div>
        <div className="flex min-h-64 flex-col items-center justify-center px-6 py-12 text-center">
          <span className="material-symbols-outlined text-4xl text-content-secondary">receipt_long</span>
          <h4 className="mt-3 text-base font-semibold text-content-primary">Activity indexer not connected</h4>
          <p className="mt-2 max-w-md text-sm leading-6 text-content-secondary">
            Active share balances and withdrawal limits are read directly from the vault. Historical
            deposits and epoch events will appear here when the LP activity endpoint is available.
          </p>
          <a
            href={`${EXPLORER_BASE_URL}/${tranche.address}`}
            target="_blank"
            rel="noreferrer"
            className="mt-5 inline-flex items-center gap-2 border border-brand-border/40 px-4 py-2 text-sm font-semibold text-brand-peach hover:border-brand-peach hover:underline hover:underline-offset-4"
          >
            View contract activity
            <span className="material-symbols-outlined text-lg">open_in_new</span>
          </a>
        </div>
      </section>
    </div>
  )
}

function PreviewRow({ label, value }: { label: string; value: ReactNode }) {
  return (
    <div className="flex items-start justify-between gap-4 text-sm">
      <span className="text-content-secondary">{label}</span>
      <span className="max-w-56 text-right font-semibold text-content-primary">{value}</span>
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
  oracleFrozen,
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
  oracleFrozen?: boolean
  quoteCapturedAt?: number
  canSubmit: boolean
  needsApproval: boolean
  isSubmitting: boolean
  onSubmit: () => void
  submissionError?: string | null
}) {
  const isIndicativePendingQuote = mode === 'deposit' && depositMode !== 'Immediate deposit'
  const isPendingDeposit = mode === 'deposit' && depositMode === 'Pending deposit epoch'
  const submitLabel = mode === 'withdraw'
    ? 'Withdraw USDC'
    : needsApproval
      ? 'Approve & deposit'
      : 'Deposit USDC'

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
          <PreviewRow label={mode === 'deposit' ? 'USDC deposited' : 'USDC requested'} value={`${amount || '0.00'} USDC`} />
          <PreviewRow
            label={
              mode === 'withdraw'
                ? 'Estimated shares burned'
                : isIndicativePendingQuote
                  ? 'Current indicative shares'
                  : 'Estimated shares'
            }
            value={
              estimatedShares === undefined
                ? 'Live quote unavailable'
                : `${estimatedShares.toLocaleString('en-US', { maximumFractionDigits: 6 })} ${tranche.token}`
            }
          />
          <PreviewRow label="Current share price" value={formatSharePrice(sharePrice)} />
          <PreviewRow
            label={mode === 'deposit' ? 'Deposit path' : 'Settlement'}
            value={mode === 'deposit' ? depositMode : 'Synchronous when permitted'}
          />
          {mode === 'deposit' ? (
            <PreviewRow
              label="Expected activation"
              value={depositMode === 'Immediate deposit' ? 'In this transaction' : 'Depends on epoch-request eligibility'}
            />
          ) : (
            <PreviewRow label="Cooldown" value="Live max already reflects holder cooldown" />
          )}
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

        {isPendingDeposit ? (
          <Alert variant="info" title="Pending entry reprices at finalization">
            This is a current ERC-4626 reference quote, not a guaranteed epoch outcome. The batch
            share price and any oracle-frozen surcharge are fixed later, so final shares can differ.
          </Alert>
        ) : null}

        {canSubmit ? (
          <Alert variant="info" title="Onchain action">
            Confirming starts {needsApproval ? 'an exact USDC approval followed by ' : ''}
            {mode === 'deposit' ? 'an immediate vault deposit' : 'a synchronous vault withdrawal'}.
            The app simulates each transaction before asking your wallet to submit it.
          </Alert>
        ) : isPendingDeposit ? (
          <Alert variant="warning" title="Pending lifecycle not enabled">
            This vault can accept a funded epoch request, but this release will not escrow your USDC
            until request discovery, cancellation, finalization, and share claiming are available
            together.
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
          {canSubmit ? submitLabel : isPendingDeposit ? 'Lifecycle coming soon' : 'Unavailable'}
        </Button>
      </div>
    </Modal>
  )
}

function VaultActionPanel({
  tranche,
  liveData,
  snapshot,
  isConnected,
  isWrongNetwork,
  onConnect,
  onSwitchNetwork,
  isSwitchingNetwork,
  switchError,
}: {
  tranche: TrancheDefinition
  liveData: TrancheLiveData
  snapshot: VaultsSnapshot
  isConnected: boolean
  isWrongNetwork: boolean
  onConnect: () => void
  onSwitchNetwork: () => void
  isSwitchingNetwork: boolean
  switchError?: string
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
  const maxAmount = mode === 'deposit' ? snapshot.walletUsdc : liveData.maxWithdraw
  const depositMode = getDepositMode(liveData)
  const vaultTransactions = useVaultTransactions({
    vaultAddress: tranche.address,
    allowance: liveData.allowance,
    onSuccess: () => {
      setAmount('')
      setShowPreview(false)
      setReviewQuote(undefined)
      snapshot.refresh()
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
        functionName: 'previewDeposit',
        args: [amountRaw],
      },
      {
        chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
        address: tranche.address,
        abi: TRANCHE_VAULT_READ_ABI,
        functionName: 'previewWithdraw',
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
  const exceedsAvailable = isConnected && maxAmount !== undefined && amountRaw > maxAmount
  const liveDepositLimit = depositMode === 'Immediate deposit'
    ? liveData.maxDeposit
    : depositMode === 'Pending deposit epoch'
      ? liveData.maxRequestDeposit
      : 0n
  const depositLimitExceeded = mode === 'deposit'
    && liveDepositLimit !== undefined
    && amountRaw > liveDepositLimit
  const positionValue = liveData.userShares !== undefined && liveData.sharePrice !== undefined
    ? Number(formatUnits(liveData.userShares, SHARE_DECIMALS)) * liveData.sharePrice
    : undefined
  const invalidAmount = amountRaw <= 0n
  const belowMinimumDeposit = mode === 'deposit' && amountRaw > 0n && amountRaw < 1_000_000n
  const invalidSubMinimumWithdrawal = mode === 'withdraw'
    && amountRaw > 0n
    && amountRaw < 1_000_000n
    && quotedSharesRaw !== undefined
    && liveData.userShares !== undefined
    && quotedSharesRaw < liveData.userShares
  const actionDataUnavailable = !snapshot.hasLivePoolData
    || !liveData.hasCoreData
    || (
      mode === 'deposit'
        ? snapshot.walletUsdc === undefined || !liveData.hasDepositData
        : !liveData.hasUserData
    )
  const safetyBlocked = mode === 'deposit'
    ? snapshot.pool.seniorImpaired === true
    : snapshot.pool.degradedMode === true
  const depositUnavailable = mode === 'deposit'
    && (depositMode === 'Availability unavailable' || depositMode === 'Deposit unavailable')
  const quoteUnavailable = amountRaw > 0n && !isQuotePending && estimatedShares === undefined
  const actionBlocked = actionDataUnavailable || safetyBlocked || depositUnavailable
  const needsApproval = mode === 'deposit'
    && liveData.allowance !== undefined
    && liveData.allowance < amountRaw
  const formInvalid = invalidAmount
    || exceedsAvailable
    || depositLimitExceeded
    || belowMinimumDeposit
    || invalidSubMinimumWithdrawal
    || actionBlocked
    || quoteUnavailable
  const canSubmitTransaction = isConnected
    && !isWrongNetwork
    && !formInvalid
    && (mode === 'withdraw' || depositMode === 'Immediate deposit')
  const hasExecutablePath = mode === 'withdraw' || depositMode === 'Immediate deposit'
  const inputError = exceedsAvailable
    ? `Exceeds available ${mode === 'deposit' ? 'balance' : 'withdrawal limit'}.`
    : depositLimitExceeded
      ? `Exceeds the live ${depositMode === 'Pending deposit epoch' ? 'request' : 'immediate-deposit'} maximum.`
      : belowMinimumDeposit
        ? 'The minimum vault deposit is 1 USDC.'
        : invalidSubMinimumWithdrawal
          ? 'Withdrawals below 1 USDC are only allowed for a complete residual exit.'
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
      vaultTransactions.deposit(amountRaw)
    } else {
      vaultTransactions.withdraw(amountRaw)
    }
  }

  return (
    <>
      <aside className="border border-brand-border/30 bg-surface-panel">
        <div className="flex items-start justify-between gap-3 border-b border-brand-border/25 p-5">
          <div>
            <h2 className="text-xl font-semibold text-content-primary">{mode === 'deposit' ? 'Deposit USDC' : 'Withdraw USDC'}</h2>
            <p className="mt-1 text-sm text-content-secondary">{tranche.name}</p>
          </div>
          <Badge variant={hasExecutablePath ? 'success' : 'warning'}>
            {mode === 'deposit' && depositMode === 'Pending deposit epoch'
              ? 'Epoch preview'
              : hasExecutablePath
                ? 'Onchain action'
                : 'Read-only'}
          </Badge>
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
            balanceLabel={mode === 'deposit' ? 'Wallet balance:' : 'Withdrawable now:'}
            label={mode === 'deposit' ? 'Amount to deposit' : 'Amount to withdraw'}
            error={inputError}
          />

          <div className="space-y-3 border border-brand-border/25 bg-app-bg p-4">
            <PreviewRow label="Share price" value={formatSharePrice(liveData.sharePrice)} />
            <PreviewRow
              label={
                mode === 'withdraw'
                  ? 'Shares burned'
                  : depositMode === 'Immediate deposit'
                    ? 'Estimated shares'
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
                  label="Activation"
                  value={depositMode === 'Immediate deposit' ? 'In transaction' : 'Epoch eligibility requires a live gate'}
                />
                <PreviewRow label="7d APY" value="Not indexed" />
              </>
            ) : (
              <>
                <PreviewRow
                  label="Position value"
                  value={positionValue === undefined ? '--' : `$${positionValue.toLocaleString('en-US', { maximumFractionDigits: 2 })}`}
                />
                <PreviewRow
                  label="Requested wallet receipt"
                  value={`${amount || '0.00'} USDC`}
                />
                <PreviewRow label="Settlement" value="Immediate if live cap permits" />
              </>
            )}
            <PreviewRow
              label="Oracle surcharge"
              value={snapshot.pool.oracleFrozen === undefined ? 'State unavailable' : snapshot.pool.oracleFrozen ? 'Reflected by vault preview where supported' : 'Inactive'}
            />
          </div>

          {mode === 'deposit' && depositMode === 'Pending deposit epoch' ? (
            <Alert variant="info" title="Pending deposit available">
              Open positions require a funded epoch request. This release previews that path but
              keeps submission disabled until request discovery, cancellation, finalization, and
              share claiming ship together.
            </Alert>
          ) : null}

          {mode === 'deposit' && depositMode === 'Deposit unavailable' ? (
            <Alert variant="warning" title="Deposits unavailable">
              Neither immediate deposits nor funded epoch requests pass the vault&apos;s current
              safety gates.
            </Alert>
          ) : null}

          {mode === 'withdraw' && liveData.maxWithdraw === 0n && isConnected ? (
            <Alert variant="warning" title="Nothing withdrawable right now">
              The holder cooldown, trader reserves, or tranche priority currently reduces the live
              maximum to zero. Share value can remain positive while withdrawals are unavailable.
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
              {mode === 'deposit'
                ? 'Deposits are unavailable while Senior is impaired.'
                : 'Withdrawals are unavailable while the protocol is in degraded mode.'}
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
        oracleFrozen={snapshot.pool.oracleFrozen}
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
  isConnected,
  isWrongNetwork,
  onConnect,
  onSwitchNetwork,
  isSwitchingNetwork,
  switchError,
}: {
  tranche: TrancheDefinition
  snapshot: VaultsSnapshot
  isConnected: boolean
  isWrongNetwork: boolean
  onConnect: () => void
  onSwitchNetwork: () => void
  isSwitchingNetwork: boolean
  switchError?: string
}) {
  const [activeTab, setActiveTab] = useState<DetailTab>('overview')
  const liveData = snapshot.tranches[tranche.id]
  const poolWithdrawCap = tranche.id === 'senior'
    ? snapshot.pool.seniorPoolWithdrawCapUsdc
    : snapshot.pool.juniorPoolWithdrawCapUsdc
  const tabs: { id: DetailTab; label: string }[] = [
    { id: 'overview', label: 'Overview' },
    { id: 'performance', label: 'Performance' },
    { id: 'risk', label: 'Risk' },
    { id: 'activity', label: 'Your position' },
  ]

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
        className="inline-flex items-center gap-2 text-sm font-semibold text-content-secondary transition-colors hover:text-brand-peach hover:underline hover:underline-offset-4"
      >
        <span className="material-symbols-outlined text-lg">arrow_back</span>
        All vaults
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
                <Badge variant={tranche.riskVariant}>{tranche.riskLabel}</Badge>
                {dataStatusBadge(snapshot.status)}
              </div>
              <h1 className="mt-2 text-3xl font-semibold text-content-primary">{tranche.name}</h1>
              <p className="mt-3 max-w-3xl text-sm leading-6 text-content-secondary">
                {tranche.description}
              </p>
              <div className="mt-4 flex flex-wrap items-center gap-3 text-xs text-content-secondary">
                <span className="inline-flex items-center gap-1.5 border border-brand-border/30 bg-app-bg px-2 py-1">
                  <span className="material-symbols-outlined text-sm">token</span>
                  USDC
                </span>
                <span className="inline-flex items-center gap-1.5 border border-brand-border/30 bg-app-bg px-2 py-1">
                  <span className="material-symbols-outlined text-sm">hub</span>
                  Arbitrum Sepolia
                </span>
                <a
                  href={`${EXPLORER_BASE_URL}/${tranche.address}`}
                  target="_blank"
                  rel="noreferrer"
                  className="inline-flex items-center gap-1.5 border border-brand-border/30 bg-app-bg px-2 py-1 text-brand-peach hover:border-brand-peach hover:underline"
                >
                  {formatAddress(tranche.address)}
                  <span className="material-symbols-outlined text-sm">open_in_new</span>
                </a>
              </div>
            </div>
          </div>
        </div>

        <dl className="grid grid-cols-2 gap-x-4 gap-y-6 p-6 lg:grid-cols-5">
          <PoolStat
            label="Tranche TVL / NAV"
            value={formatCompactUsd(liveData.totalAssets)}
            subvalue={`${formatFullUsdc(liveData.totalAssets, 0)} USDC`}
            tooltip="Current ERC-4626 totalAssets accounting value. This can rise or fall and is not cumulative deposits."
          />
          <PoolStat
            label="7d APY"
            value="--"
            subvalue="Performance indexer unavailable"
            valueClassName="text-content-secondary"
            tooltip="Seven-day annualized performance will appear after historical share-price checkpoints are indexed."
          />
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
                aria-selected={activeTab === tab.id}
                aria-controls={`vault-panel-${tranche.id}`}
                tabIndex={activeTab === tab.id ? 0 : -1}
                onClick={() => {
                  setActiveTab(tab.id)
                }}
                onKeyDown={(event) => {
                  handleTabKeyDown(event, index)
                }}
                className={`shrink-0 px-4 py-2 text-sm font-semibold transition-colors ${
                  activeTab === tab.id
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
            aria-labelledby={`vault-tab-${tranche.id}-${activeTab}`}
          >
            {activeTab === 'overview' ? (
              <OverviewTab
                tranche={tranche}
                liveData={liveData}
                snapshot={snapshot}
                isConnected={isConnected}
              />
            ) : null}
            {activeTab === 'performance' ? (
              <PerformanceTab tranche={tranche} liveData={liveData} />
            ) : null}
            {activeTab === 'risk' ? <RiskTab tranche={tranche} /> : null}
            {activeTab === 'activity' ? (
              <ActivityTab tranche={tranche} liveData={liveData} isConnected={isConnected} />
            ) : null}
          </div>
        </div>

        <div className="lg:sticky lg:top-32">
          <VaultActionPanel
            key={tranche.id}
            tranche={tranche}
            liveData={liveData}
            snapshot={snapshot}
            isConnected={isConnected}
            isWrongNetwork={isWrongNetwork}
            onConnect={onConnect}
            onSwitchNetwork={onSwitchNetwork}
            isSwitchingNetwork={isSwitchingNetwork}
            switchError={switchError}
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
    return <VaultsOverview snapshot={snapshot} />
  }

  return (
    <VaultDetail
      tranche={selectedTranche}
      snapshot={snapshot}
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
