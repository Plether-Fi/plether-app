import { useMemo } from 'react'
import { useQuery } from '@tanstack/react-query'
import { Result } from 'better-result'
import type { Address, Hash } from 'viem'
import { perpsApi } from '../api/client'
import type {
  VaultActivity as BackendVaultActivity,
  VaultActivityRow,
  VaultActivityTrancheName,
} from '../api/types'
import {
  PERPS_ARBITRUM_SEPOLIA,
  PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
} from '../contracts/perpsAddresses'

export type VaultActivityTranche = VaultActivityTrancheName
export type VaultActivityKind = 'deposit' | 'withdraw'

const VAULT_DEPLOYMENT_BLOCK = 302_257_125
const ACTIVITY_LIMIT = 250

interface RawVaultHolder {
  address: Address
  balance: bigint
  tranche: VaultActivityTranche
}

interface RawVaultRequest {
  id: string
  kind: VaultActivityKind
  tranche: VaultActivityTranche
  account: Address
  requestId: bigint
  rawAmount: bigint
  timestamp: string
  blockNumber: number
  transactionIndex: number
  logIndex: number
  transactionHash: Hash
}

export interface VaultHolderDistribution {
  address: Address
  currentNavUsdc: bigint
  shareOfVaultNav: number
  seniorNavUsdc: bigint
  juniorNavUsdc: bigint
  seniorShareOfAttributedValue?: number
  juniorShareOfAttributedValue?: number
}

export interface VaultOverviewActivityItem {
  id: string
  kind: VaultActivityKind
  tranche: VaultActivityTranche
  account: Address
  requestId: bigint
  amountUsdc?: bigint
  shares?: bigint
  amountIsEstimate: boolean
  timestamp: string
  blockNumber: number
  transactionHash: Hash
}

interface UseVaultActivityOptions {
  seniorTotalAssets?: bigint
  seniorEffectiveSupply?: bigint
  juniorTotalAssets?: bigint
  juniorEffectiveSupply?: bigint
}

function isAddress(value: string): value is Address {
  return /^0x[0-9a-fA-F]{40}$/.test(value)
}

function isHash(value: string): value is Hash {
  return /^0x[0-9a-fA-F]{64}$/.test(value)
}

function parseUnsignedInteger(value: string | null): bigint | undefined {
  if (typeof value !== 'string' || !/^\d+$/.test(value)) return undefined
  return BigInt(value)
}

function normalizeAddress(address: Address): Address {
  return address.toLowerCase() as Address
}

function parseHolder(
  tranche: VaultActivityTranche,
  address: string,
  shareBalance: string,
  unclaimedDepositShares?: string,
  withdrawalEscrowShares?: string,
  totalAttributedShares?: string,
): RawVaultHolder | undefined {
  const directBalance = parseUnsignedInteger(shareBalance)
  const hasAttributionFields = unclaimedDepositShares !== undefined
    || withdrawalEscrowShares !== undefined
    || totalAttributedShares !== undefined
  if (
    !isAddress(address)
    || directBalance === undefined
    || (hasAttributionFields && (unclaimedDepositShares === undefined || totalAttributedShares === undefined))
  ) return undefined
  const unclaimedBalance = unclaimedDepositShares === undefined
    ? 0n
    : parseUnsignedInteger(unclaimedDepositShares)
  const withdrawalBalance = withdrawalEscrowShares === undefined
    ? 0n
    : parseUnsignedInteger(withdrawalEscrowShares)
  const balance = totalAttributedShares === undefined
    ? directBalance
    : parseUnsignedInteger(totalAttributedShares)
  if (
    unclaimedBalance === undefined
    || withdrawalBalance === undefined
    || balance === undefined
    || balance <= 0n
    || balance !== directBalance + unclaimedBalance + withdrawalBalance
  ) return undefined
  return { address: normalizeAddress(address), balance, tranche }
}

function parseRequest(
  expectedTranche: VaultActivityTranche,
  row: VaultActivityRow,
): RawVaultRequest | undefined {
  const requestId = parseUnsignedInteger(row.requestId)
  const rawAmount = parseUnsignedInteger(row.kind === 'deposit' ? row.rawAssets : row.rawShares)
  if (
    requestId === undefined
    || rawAmount === undefined
    || row.tranche !== expectedTranche
    || !isAddress(row.account)
    || !isHash(row.transactionHash)
    || !Number.isSafeInteger(row.timestamp)
    || !Number.isSafeInteger(row.blockNumber)
    || !Number.isSafeInteger(row.transactionIndex)
    || !Number.isSafeInteger(row.logIndex)
    || row.timestamp < 0
    || row.blockNumber < 0
    || row.transactionIndex < 0
    || row.logIndex < 0
  ) return undefined

  return {
    id: row.id,
    kind: row.kind,
    tranche: row.tranche,
    account: normalizeAddress(row.account),
    requestId,
    rawAmount,
    timestamp: new Date(row.timestamp * 1_000).toISOString(),
    blockNumber: row.blockNumber,
    transactionIndex: row.transactionIndex,
    logIndex: row.logIndex,
    transactionHash: row.transactionHash,
  }
}

function normalizeActivity(payload: BackendVaultActivity): {
  holders: RawVaultHolder[]
  requests: RawVaultRequest[]
  holderShareTotals: Partial<Record<VaultActivityTranche, bigint>>
} {
  const holderShareTotals: Partial<Record<VaultActivityTranche, bigint>> = {}
  const holders = (['senior', 'junior'] as const).flatMap((tranche) => {
    const data = payload[tranche]
    if (
      !Number.isSafeInteger(data.holderCount)
      || data.holderCount < data.holders.length
      || data.holders.length > ACTIVITY_LIMIT
      || data.holdersTruncated !== (data.holderCount > data.holders.length)
    ) {
      throw new Error('Vault activity returned inconsistent holder coverage.')
    }
    const parsed = data.holders.map(({
      address,
      shareBalance,
      unclaimedDepositShares,
      withdrawalEscrowShares,
      totalAttributedShares,
    }) => {
      const holder = parseHolder(
        tranche,
        address,
        shareBalance,
        unclaimedDepositShares,
        withdrawalEscrowShares,
        totalAttributedShares,
      )
      if (!holder) throw new Error('Vault activity returned a malformed holder row.')
      return holder
    })
    if (data.totalAttributedShares !== undefined) {
      const total = parseUnsignedInteger(data.totalAttributedShares)
      const returnedTotal = parsed.reduce((sum, holder) => sum + holder.balance, 0n)
      if (
        total === undefined
        || total < returnedTotal
        || (!data.holdersTruncated && total !== returnedTotal)
      ) {
        throw new Error('Vault activity returned an inconsistent attributed share total.')
      }
      holderShareTotals[tranche] = total
    }
    return parsed
  })
  const requests = (['senior', 'junior'] as const).flatMap((tranche) => {
    const data = payload[tranche]
    if (
      !Number.isSafeInteger(data.activityCount)
      || data.activityCount < data.activity.length
      || data.activity.length > ACTIVITY_LIMIT
      || data.activityTruncated !== (data.activityCount > data.activity.length)
    ) {
      throw new Error('Vault activity returned inconsistent request coverage.')
    }
    return data.activity.map((row) => {
      const request = parseRequest(tranche, row)
      if (
        !request
        || row.id !== `${row.transactionHash}-${String(row.logIndex)}`
        || (row.kind === 'deposit' && row.rawShares !== null)
        || (row.kind === 'withdraw' && row.rawAssets !== null)
      ) {
        throw new Error('Vault activity returned a malformed request row.')
      }
      return request
    })
  })
  return { holders, requests, holderShareTotals }
}

async function fetchVaultActivity(signal: AbortSignal): Promise<BackendVaultActivity> {
  const result = await perpsApi.getPerpsVaultActivity(signal)
  if (Result.isError(result)) throw result.error
  const payload = result.value.data
  if (
    payload.deployment.chainId !== PERPS_ARBITRUM_SEPOLIA_CHAIN_ID
    || payload.deployment.deploymentBlock !== VAULT_DEPLOYMENT_BLOCK
    || payload.deployment.housePool.toLowerCase() !== PERPS_ARBITRUM_SEPOLIA.housePool.toLowerCase()
    || payload.deployment.seniorVault.toLowerCase() !== PERPS_ARBITRUM_SEPOLIA.seniorVault.toLowerCase()
    || payload.deployment.juniorVault.toLowerCase() !== PERPS_ARBITRUM_SEPOLIA.juniorVault.toLowerCase()
  ) {
    throw new Error('Vault activity belongs to a different contract deployment.')
  }
  const coverage = payload.coverage
  const attribution = coverage.shareAttribution ?? coverage.depositShareAttribution
  if (
    !Number.isSafeInteger(coverage.confirmedThroughBlock)
    || !Number.isSafeInteger(coverage.observedSafeHeadBlock)
    || !Number.isSafeInteger(coverage.lagBlocks)
    || !Number.isSafeInteger(coverage.lagSeconds)
    || !Number.isSafeInteger(coverage.lastSuccessfulPoll)
    || coverage.confirmedThroughBlock < VAULT_DEPLOYMENT_BLOCK
    || coverage.observedSafeHeadBlock < coverage.confirmedThroughBlock
    || coverage.lagBlocks !== coverage.observedSafeHeadBlock - coverage.confirmedThroughBlock
    || coverage.lagSeconds < 0
    || coverage.lastSuccessfulPoll < 0
    || coverage.complete !== (coverage.confirmedThroughBlock >= coverage.observedSafeHeadBlock)
    || (coverage.confirmedThroughHash !== null && !isHash(coverage.confirmedThroughHash))
    || (coverage.observedSafeHeadHash !== null && !isHash(coverage.observedSafeHeadHash))
    || (attribution !== undefined && (
      !Number.isSafeInteger(attribution.confirmedThroughBlock)
      || attribution.confirmedThroughBlock < VAULT_DEPLOYMENT_BLOCK
      || attribution.confirmedThroughBlock > coverage.observedSafeHeadBlock
      || !Number.isSafeInteger(attribution.lastSuccessfulPoll)
      || attribution.lastSuccessfulPoll < 0
      || attribution.complete !== (attribution.confirmedThroughBlock >= coverage.observedSafeHeadBlock)
      || (attribution.confirmedThroughHash !== null && !isHash(attribution.confirmedThroughHash))
    ))
  ) {
    throw new Error('Vault activity returned inconsistent canonical coverage.')
  }
  // Validate every row before React Query accepts this payload. A malformed
  // refresh then leaves the last successful canonical dataset visible.
  normalizeActivity(payload)
  return payload
}

function sharesToAssets(
  shares: bigint,
  totalAssets: bigint | undefined,
  effectiveSupply: bigint | undefined,
): bigint | undefined {
  if (totalAssets === undefined || effectiveSupply === undefined || effectiveSupply <= 0n) {
    return undefined
  }
  return shares * totalAssets / effectiveSupply
}

export function useVaultActivity({
  seniorTotalAssets,
  seniorEffectiveSupply,
  juniorTotalAssets,
  juniorEffectiveSupply,
}: UseVaultActivityOptions) {
  const query = useQuery({
    queryKey: [
      'vault-overview-activity',
      PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
      PERPS_ARBITRUM_SEPOLIA.seniorVault,
      PERPS_ARBITRUM_SEPOLIA.juniorVault,
    ],
    queryFn: ({ signal }) => fetchVaultActivity(signal),
    refetchInterval: 60_000,
    staleTime: 30_000,
    retry: 1,
  })
  const normalized = useMemo(
    () => query.data
      ? normalizeActivity(query.data)
      : { holders: [], requests: [], holderShareTotals: {} },
    [query.data],
  )

  const holders = useMemo<VaultHolderDistribution[]>(() => {
    const values = new Map<Address, {
      senior: bigint
      junior: bigint
      seniorShares: bigint
      juniorShares: bigint
    }>()
    for (const holder of normalized.holders) {
      const vaultAddress = holder.tranche === 'senior'
        ? PERPS_ARBITRUM_SEPOLIA.seniorVault
        : PERPS_ARBITRUM_SEPOLIA.juniorVault
      if (holder.address === normalizeAddress(vaultAddress)) continue
      const currentNav = holder.tranche === 'senior'
        ? sharesToAssets(holder.balance, seniorTotalAssets, seniorEffectiveSupply)
        : sharesToAssets(holder.balance, juniorTotalAssets, juniorEffectiveSupply)
      if (currentNav === undefined || currentNav <= 0n) continue
      const existing = values.get(holder.address) ?? {
        senior: 0n,
        junior: 0n,
        seniorShares: 0n,
        juniorShares: 0n,
      }
      existing[holder.tranche] += currentNav
      existing[holder.tranche === 'senior' ? 'seniorShares' : 'juniorShares'] += holder.balance
      values.set(holder.address, existing)
    }

    const totalVaultNav = (seniorTotalAssets ?? 0n) + (juniorTotalAssets ?? 0n)
    const fallbackSeniorShares = [...values.values()].reduce((sum, value) => sum + value.seniorShares, 0n)
    const fallbackJuniorShares = [...values.values()].reduce((sum, value) => sum + value.juniorShares, 0n)
    const seniorAttributedShares = normalized.holderShareTotals.senior ?? fallbackSeniorShares
    const juniorAttributedShares = normalized.holderShareTotals.junior ?? fallbackJuniorShares
    return [...values.entries()].map(([address, value]) => {
      const currentNavUsdc = value.senior + value.junior
      return {
        address,
        currentNavUsdc,
        shareOfVaultNav: totalVaultNav > 0n
          ? Number(currentNavUsdc * 1_000_000n / totalVaultNav) / 10_000
          : 0,
        seniorNavUsdc: value.senior,
        juniorNavUsdc: value.junior,
        seniorShareOfAttributedValue: seniorAttributedShares > 0n
          ? Number(value.seniorShares * 1_000_000n / seniorAttributedShares) / 10_000
          : 0,
        juniorShareOfAttributedValue: juniorAttributedShares > 0n
          ? Number(value.juniorShares * 1_000_000n / juniorAttributedShares) / 10_000
          : 0,
      }
    }).sort((left, right) => (
      left.currentNavUsdc > right.currentNavUsdc
        ? -1
        : left.currentNavUsdc < right.currentNavUsdc ? 1 : 0
    ))
  }, [
    juniorEffectiveSupply,
    juniorTotalAssets,
    normalized.holderShareTotals,
    normalized.holders,
    seniorEffectiveSupply,
    seniorTotalAssets,
  ])

  const activity = useMemo<VaultOverviewActivityItem[]>(() => (
    normalized.requests.map((request) => {
      const isSenior = request.tranche === 'senior'
      const amountUsdc = request.kind === 'deposit'
        ? request.rawAmount
        : sharesToAssets(
            request.rawAmount,
            isSenior ? seniorTotalAssets : juniorTotalAssets,
            isSenior ? seniorEffectiveSupply : juniorEffectiveSupply,
          )
      return {
        ...request,
        amountUsdc,
        shares: request.kind === 'withdraw' ? request.rawAmount : undefined,
        amountIsEstimate: request.kind === 'withdraw',
      }
    }).sort((left, right) => (
      right.blockNumber - left.blockNumber
        || right.transactionIndex - left.transactionIndex
        || right.logIndex - left.logIndex
    ))
  ), [
    juniorEffectiveSupply,
    juniorTotalAssets,
    normalized.requests,
    seniorEffectiveSupply,
    seniorTotalAssets,
  ])

  return {
    holders,
    activity,
    isLoading: query.isPending,
    isError: query.isError && query.data === undefined,
    isStale: Boolean(query.data && (query.data.coverage.stale || query.isError)),
    refetch: query.refetch,
  }
}
