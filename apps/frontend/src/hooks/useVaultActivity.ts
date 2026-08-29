import { useMemo } from 'react'
import { useQuery } from '@tanstack/react-query'
import { keccak256, toBytes, zeroAddress, type Address, type Hash } from 'viem'
import {
  PERPS_ARBITRUM_SEPOLIA,
  PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
} from '../contracts/perpsAddresses'

const BLOCKSCOUT_API_BASE = 'https://arbitrum-sepolia.blockscout.com/api/v2'
const HOLDER_PAGE_LIMIT = 5
const LOG_PAGE_LIMIT = 5
const TRANSFER_FALLBACK_PAGE_LIMIT = 20

const DEPOSIT_REQUEST_TOPIC = keccak256(
  toBytes('DepositRequest(address,address,uint256,address,uint256)')
)
const REDEEM_REQUEST_TOPIC = keccak256(
  toBytes('RedeemRequest(address,address,uint256,address,uint256)')
)

export type VaultActivityTranche = 'senior' | 'junior'
export type VaultActivityKind = 'deposit' | 'withdraw'

interface BlockscoutAddress {
  hash?: string
  is_contract?: boolean
}

interface BlockscoutHolder {
  address?: BlockscoutAddress
  value?: string
}

interface BlockscoutHolderResponse {
  items?: BlockscoutHolder[]
  next_page_params?: Record<string, string | number> | null
}

interface BlockscoutTransfer {
  from?: BlockscoutAddress
  to?: BlockscoutAddress
  total?: {
    value?: string
  }
}

interface BlockscoutTransferResponse {
  items?: BlockscoutTransfer[]
  next_page_params?: Record<string, string | number> | null
}

interface BlockscoutDecodedParameter {
  name?: string
  value?: string
}

interface BlockscoutLog {
  block_number?: number
  block_timestamp?: string
  data?: string
  decoded?: {
    method_call?: string
    parameters?: BlockscoutDecodedParameter[]
  } | null
  index?: number
  topics?: string[]
  transaction_hash?: string
}

interface BlockscoutLogResponse {
  items?: BlockscoutLog[]
  next_page_params?: Record<string, string | number> | null
}

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
  transactionHash: Hash
}

interface RawVaultActivity {
  holders: RawVaultHolder[]
  requests: RawVaultRequest[]
}

export interface VaultHolderDistribution {
  address: Address
  currentNavUsdc: bigint
  shareOfVaultNav: number
  seniorNavUsdc: bigint
  juniorNavUsdc: bigint
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

function isAddress(value: string | undefined): value is Address {
  return typeof value === 'string' && /^0x[0-9a-fA-F]{40}$/.test(value)
}

function isHash(value: string | undefined): value is Hash {
  return typeof value === 'string' && /^0x[0-9a-fA-F]{64}$/.test(value)
}

function parseUnsignedInteger(value: string | undefined): bigint | undefined {
  if (typeof value !== 'string' || !/^\d+$/.test(value)) return undefined
  return BigInt(value)
}

function pageUrl(path: string, pageParams?: Record<string, string | number> | null): string {
  const url = new URL(`${BLOCKSCOUT_API_BASE}${path}`)
  Object.entries(pageParams ?? {}).forEach(([key, value]) => {
    url.searchParams.set(key, String(value))
  })
  return url.toString()
}

async function fetchJson<T>(url: string, signal: AbortSignal): Promise<T> {
  const response = await fetch(url, { signal })
  if (!response.ok) {
    throw new Error(`Vault activity request failed with HTTP ${String(response.status)}.`)
  }
  return response.json() as Promise<T>
}

async function fetchAllPages<T extends { next_page_params?: Record<string, string | number> | null }>(
  path: string,
  signal: AbortSignal,
  pageLimit: number,
): Promise<T[]> {
  const pages: T[] = []
  let nextPage: Record<string, string | number> | null | undefined

  for (let pageIndex = 0; pageIndex < pageLimit; pageIndex += 1) {
    const page = await fetchJson<T>(pageUrl(path, nextPage), signal)
    pages.push(page)
    nextPage = page.next_page_params
    if (!nextPage) break
  }

  return pages
}

function normalizeAddress(address: Address): Address {
  return address.toLowerCase() as Address
}

async function fetchHolders(
  vaultAddress: Address,
  tranche: VaultActivityTranche,
  signal: AbortSignal,
): Promise<RawVaultHolder[]> {
  const holderPages = await fetchAllPages<BlockscoutHolderResponse>(
    `/tokens/${vaultAddress}/holders`,
    signal,
    HOLDER_PAGE_LIMIT,
  )
  const holders = holderPages.flatMap((page) => page.items ?? []).flatMap((holder) => {
    const address = holder.address?.hash
    const balance = parseUnsignedInteger(holder.value)
    if (!isAddress(address) || balance === undefined || balance <= 0n) return []
    return [{ address: normalizeAddress(address), balance, tranche } satisfies RawVaultHolder]
  })

  if (holders.length > 0) return holders

  // Blockscout can briefly report an empty holder index immediately after a
  // deployment. Reconstruct balances from the complete transfer history only
  // while the history is small enough to fetch without truncation.
  const transferPages = await fetchAllPages<BlockscoutTransferResponse>(
    `/tokens/${vaultAddress}/transfers`,
    signal,
    TRANSFER_FALLBACK_PAGE_LIMIT,
  )
  const lastTransferPage = transferPages.at(-1)
  if (lastTransferPage?.next_page_params) return []

  const balances = new Map<Address, bigint>()
  for (const transfer of transferPages.flatMap((page) => page.items ?? []).reverse()) {
    const from = transfer.from?.hash
    const to = transfer.to?.hash
    const amount = parseUnsignedInteger(transfer.total?.value)
    if (amount === undefined) continue
    if (isAddress(from) && normalizeAddress(from) !== zeroAddress) {
      const normalizedFrom = normalizeAddress(from)
      balances.set(normalizedFrom, (balances.get(normalizedFrom) ?? 0n) - amount)
    }
    if (isAddress(to) && normalizeAddress(to) !== zeroAddress) {
      const normalizedTo = normalizeAddress(to)
      balances.set(normalizedTo, (balances.get(normalizedTo) ?? 0n) + amount)
    }
  }

  return [...balances.entries()].flatMap(([address, balance]) => (
    balance > 0n ? [{ address, balance, tranche } satisfies RawVaultHolder] : []
  ))
}

function decodedValue(log: BlockscoutLog, name: string): string | undefined {
  return log.decoded?.parameters?.find((parameter) => parameter.name === name)?.value
}

function topicAddress(topic: string | undefined): Address | undefined {
  if (typeof topic !== 'string' || !/^0x[0-9a-fA-F]{64}$/.test(topic)) return undefined
  return `0x${topic.slice(-40)}`.toLowerCase() as Address
}

function dataWord(data: string | undefined, wordIndex: number): bigint | undefined {
  if (typeof data !== 'string' || !/^0x[0-9a-fA-F]*$/.test(data)) return undefined
  const start = 2 + wordIndex * 64
  const word = data.slice(start, start + 64)
  return word.length === 64 ? BigInt(`0x${word}`) : undefined
}

function decodeRequestLog(
  log: BlockscoutLog,
  tranche: VaultActivityTranche,
): RawVaultRequest | undefined {
  const topic = log.topics?.[0]?.toLowerCase()
  const method = log.decoded?.method_call ?? ''
  const isDeposit = topic === DEPOSIT_REQUEST_TOPIC.toLowerCase()
    || method.startsWith('DepositRequest(')
  const isRedeem = topic === REDEEM_REQUEST_TOPIC.toLowerCase()
    || method.startsWith('RedeemRequest(')
  if (!isDeposit && !isRedeem) return undefined

  const accountValue = decodedValue(log, 'owner')
  const account = isAddress(accountValue) ? normalizeAddress(accountValue) : topicAddress(log.topics?.[2])
  const requestId = parseUnsignedInteger(decodedValue(log, 'requestId'))
    ?? (log.topics?.[3] ? BigInt(log.topics[3]) : undefined)
  const rawAmount = parseUnsignedInteger(decodedValue(log, isDeposit ? 'assets' : 'shares'))
    ?? dataWord(log.data, 1)
  const transactionHash = log.transaction_hash
  if (
    !account
    || requestId === undefined
    || rawAmount === undefined
    || !isHash(transactionHash)
    || typeof log.block_timestamp !== 'string'
    || typeof log.block_number !== 'number'
  ) {
    return undefined
  }

  return {
    id: `${transactionHash}-${String(log.index ?? 0)}`,
    kind: isDeposit ? 'deposit' : 'withdraw',
    tranche,
    account,
    requestId,
    rawAmount,
    timestamp: log.block_timestamp,
    blockNumber: log.block_number,
    transactionHash,
  }
}

async function fetchRequests(
  vaultAddress: Address,
  tranche: VaultActivityTranche,
  signal: AbortSignal,
): Promise<RawVaultRequest[]> {
  const pages = await fetchAllPages<BlockscoutLogResponse>(
    `/addresses/${vaultAddress}/logs`,
    signal,
    LOG_PAGE_LIMIT,
  )
  return pages.flatMap((page) => page.items ?? []).flatMap((log) => {
    const request = decodeRequestLog(log, tranche)
    return request ? [request] : []
  })
}

async function fetchVaultActivity(signal: AbortSignal): Promise<RawVaultActivity> {
  const tasks = await Promise.allSettled([
    fetchHolders(PERPS_ARBITRUM_SEPOLIA.seniorVault, 'senior', signal),
    fetchHolders(PERPS_ARBITRUM_SEPOLIA.juniorVault, 'junior', signal),
    fetchRequests(PERPS_ARBITRUM_SEPOLIA.seniorVault, 'senior', signal),
    fetchRequests(PERPS_ARBITRUM_SEPOLIA.juniorVault, 'junior', signal),
  ])
  if (tasks.every((task) => task.status === 'rejected')) {
    throw new Error('Vault activity is unavailable.')
  }

  const fulfilled = <T,>(index: number): T[] => {
    const task = tasks[index]
    return task.status === 'fulfilled' ? task.value as T[] : []
  }

  return {
    holders: [...fulfilled<RawVaultHolder>(0), ...fulfilled<RawVaultHolder>(1)],
    requests: [...fulfilled<RawVaultRequest>(2), ...fulfilled<RawVaultRequest>(3)],
  }
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
  })

  const holders = useMemo<VaultHolderDistribution[]>(() => {
    const values = new Map<Address, { senior: bigint; junior: bigint }>()
    for (const holder of query.data?.holders ?? []) {
      const vaultAddress = holder.tranche === 'senior'
        ? PERPS_ARBITRUM_SEPOLIA.seniorVault
        : PERPS_ARBITRUM_SEPOLIA.juniorVault
      if (holder.address === normalizeAddress(vaultAddress)) continue
      const currentNav = holder.tranche === 'senior'
        ? sharesToAssets(holder.balance, seniorTotalAssets, seniorEffectiveSupply)
        : sharesToAssets(holder.balance, juniorTotalAssets, juniorEffectiveSupply)
      if (currentNav === undefined || currentNav <= 0n) continue
      const existing = values.get(holder.address) ?? { senior: 0n, junior: 0n }
      existing[holder.tranche] += currentNav
      values.set(holder.address, existing)
    }

    const totalVaultNav = (seniorTotalAssets ?? 0n) + (juniorTotalAssets ?? 0n)
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
      }
    }).sort((left, right) => (
      left.currentNavUsdc > right.currentNavUsdc
        ? -1
        : left.currentNavUsdc < right.currentNavUsdc ? 1 : 0
    ))
  }, [
    juniorEffectiveSupply,
    juniorTotalAssets,
    query.data?.holders,
    seniorEffectiveSupply,
    seniorTotalAssets,
  ])

  const activity = useMemo<VaultOverviewActivityItem[]>(() => (
    (query.data?.requests ?? []).map((request) => {
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
      left.blockNumber === right.blockNumber
        ? right.id.localeCompare(left.id)
        : right.blockNumber - left.blockNumber
    ))
  ), [
    juniorEffectiveSupply,
    juniorTotalAssets,
    query.data?.requests,
    seniorEffectiveSupply,
    seniorTotalAssets,
  ])

  return {
    holders,
    activity,
    isLoading: query.isLoading,
    isError: query.isError,
    refetch: query.refetch,
  }
}
