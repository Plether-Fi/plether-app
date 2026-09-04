import { useCallback, useEffect, useMemo, useState } from 'react'
import { Result } from 'better-result'
import { zeroAddress, type Address } from 'viem'
import { useReadContracts } from 'wagmi'
import { perpsApi } from '../api/client'
import { PERPS_PUBLIC_LENS_ABI } from '../contracts/abis'
import {
  PERPS_ARBITRUM_SEPOLIA,
  PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
} from '../contracts/perpsAddresses'

const NEARBY_EPOCH_LOOKBACK = 4n

interface ContractResult {
  status: 'failure' | 'success'
  result?: unknown
}

export interface VaultDepositRequest {
  requestId: bigint
  targetTimestamp: number
  activationTimestamp?: number
  cooldownEndsAt?: bigint
  directRedeemableShares?: bigint
  directRedeemSupported?: boolean
  pendingAssets: bigint
  pendingSharesEstimate: bigint
  claimableAssets: bigint
  claimableShares: bigint
  refundableAssets: bigint
  matured: boolean
}

export interface VaultRedeemRequest {
  requestId: bigint
  targetTimestamp: number
  pendingShares: bigint
  pendingAssetsEstimate: bigint
  claimableShares: bigint
  claimableAssets: bigint
  refundableShares: bigint
  refundPending: boolean
  matured: boolean
}

interface UseVaultRequestsOptions {
  controller?: Address
  isSenior: boolean
  currentEpoch?: bigint
}

function readResult(data: readonly ContractResult[] | undefined, index: number): unknown {
  const item = data?.[index]
  return item?.status === 'success' ? item.result : undefined
}

function tupleValue(value: unknown, index: number, key: string): unknown {
  if (value && typeof value === 'object' && key in value) {
    return (value as Record<string, unknown>)[key]
  }
  return Array.isArray(value) ? value[index] : undefined
}

function asBigInt(value: unknown): bigint {
  return typeof value === 'bigint' ? value : 0n
}

function asBoolean(value: unknown): boolean {
  return typeof value === 'boolean' ? value : false
}

function cacheKey(controller: Address, isSenior: boolean): string {
  const vault = isSenior ? PERPS_ARBITRUM_SEPOLIA.seniorVault : PERPS_ARBITRUM_SEPOLIA.juniorVault
  return [
    'plether:vault-request-epochs',
    String(PERPS_ARBITRUM_SEPOLIA_CHAIN_ID),
    PERPS_ARBITRUM_SEPOLIA.perpsPublicLens.toLowerCase(),
    vault.toLowerCase(),
    controller.toLowerCase(),
  ].join(':')
}

function readCachedRequestIds(controller: Address, isSenior: boolean): bigint[] {
  if (typeof window === 'undefined') return []
  try {
    const parsed = JSON.parse(window.localStorage.getItem(cacheKey(controller, isSenior)) ?? '[]') as unknown
    if (!Array.isArray(parsed)) return []
    return parsed
      .filter((value): value is string => typeof value === 'string' && /^\d+$/.test(value))
      .map(BigInt)
  } catch {
    return []
  }
}

function writeCachedRequestIds(controller: Address, isSenior: boolean, requestIds: bigint[]) {
  if (typeof window === 'undefined') return
  try {
    const uniqueIds = [...new Set(requestIds.map(String))]
    window.localStorage.setItem(cacheKey(controller, isSenior), JSON.stringify(uniqueIds))
  } catch {
    // Direct lens reads remain authoritative when local persistence is unavailable.
  }
}

async function discoverRequestIds(
  controller: Address,
  isSenior: boolean,
  signal: AbortSignal
): Promise<{ requestIds: bigint[]; stale: boolean }> {
  const requestIds: bigint[] = []
  const seenCursors = new Set<string>()
  let stale = false
  let cursor: string | undefined
  do {
    const result = await perpsApi.getPerpsVaultRequestIds(
      isSenior ? 'senior' : 'junior',
      controller,
      cursor,
      250,
      signal,
    )
    if (Result.isError(result)) throw result.error
    const page = result.value.data
    if (
      page.tranche !== (isSenior ? 'senior' : 'junior')
      || page.account.toLowerCase() !== controller.toLowerCase()
      || !Number.isSafeInteger(page.confirmedThroughBlock)
      || page.confirmedThroughBlock < 302_257_125
      || typeof page.stale !== 'boolean'
      || (page.nextCursor !== null && !/^\d+$/.test(page.nextCursor))
      || page.requestIds.length > 250
    ) {
      throw new Error('Vault-request discovery returned inconsistent coverage.')
    }
    let previousId: bigint | undefined
    for (const value of page.requestIds) {
      if (!/^\d+$/.test(value)) throw new Error('Vault-request discovery returned an invalid request ID.')
      const requestId = BigInt(value)
      if (
        (previousId !== undefined && requestId >= previousId)
        || (cursor !== undefined && requestId >= BigInt(cursor))
      ) {
        throw new Error('Vault-request discovery returned out-of-order request IDs.')
      }
      requestIds.push(requestId)
      previousId = requestId
    }
    if (
      page.nextCursor !== null
      && (
        page.requestIds.length === 0
        || page.nextCursor !== page.requestIds.at(-1)
        || seenCursors.has(page.nextCursor)
      )
    ) {
      throw new Error('Vault-request discovery returned an invalid pagination cursor.')
    }
    stale ||= page.stale
    if (page.nextCursor !== null) seenCursors.add(page.nextCursor)
    cursor = page.nextCursor ?? undefined
  } while (cursor !== undefined)
  return { requestIds, stale }
}

export function useVaultRequests({
  controller,
  isSenior,
  currentEpoch,
}: UseVaultRequestsOptions) {
  const [cachedRequestIds, setCachedRequestIds] = useState<bigint[]>([])
  const [eventRequestIds, setEventRequestIds] = useState<bigint[]>([])
  const [discoveryStatus, setDiscoveryStatus] = useState<'idle' | 'loading' | 'success' | 'error'>('idle')
  const [discoveryStale, setDiscoveryStale] = useState(false)
  const [discoveryNonce, setDiscoveryNonce] = useState(0)

  useEffect(() => {
    setCachedRequestIds(controller ? readCachedRequestIds(controller, isSenior) : [])
    setEventRequestIds([])
    setDiscoveryStatus(controller ? 'loading' : 'idle')
    setDiscoveryStale(false)
  }, [controller, isSenior])

  useEffect(() => {
    if (!controller) return
    const abortController = new AbortController()
    setDiscoveryStatus('loading')

    void discoverRequestIds(controller, isSenior, abortController.signal)
      .then(({ requestIds, stale }) => {
        setEventRequestIds(requestIds)
        setDiscoveryStale(stale)
        setCachedRequestIds((currentRequestIds) => {
          const merged = [...currentRequestIds, ...requestIds]
          writeCachedRequestIds(controller, isSenior, merged)
          return [...new Set(merged.map(String))].map(BigInt)
        })
        setDiscoveryStatus('success')
      })
      .catch((error: unknown) => {
        if (error instanceof DOMException && error.name === 'AbortError') return
        setDiscoveryStatus('error')
      })

    return () => {
      abortController.abort()
    }
  }, [controller, discoveryNonce, isSenior])

  const requestIds = useMemo(() => {
    const ids = new Set([...cachedRequestIds, ...eventRequestIds].map(String))
    if (currentEpoch !== undefined) {
      const firstNearbyEpoch = currentEpoch > NEARBY_EPOCH_LOOKBACK
        ? currentEpoch - NEARBY_EPOCH_LOOKBACK
        : 0n
      for (let requestId = firstNearbyEpoch; requestId <= currentEpoch + 2n; requestId += 1n) {
        ids.add(String(requestId))
      }
    }
    return [...ids].map(BigInt).sort((left, right) => left > right ? -1 : left < right ? 1 : 0)
  }, [cachedRequestIds, currentEpoch, eventRequestIds])

  const readController = controller ?? zeroAddress
  const contracts = useMemo(() => [
    ...requestIds.map((requestId) => ({
      chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID as 421614,
      address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
      abi: PERPS_PUBLIC_LENS_ABI,
      functionName: 'getLpRequestState' as const,
      args: [isSenior, requestId, readController] as const,
    })),
    ...requestIds.map((requestId) => ({
      chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID as 421614,
      address: PERPS_ARBITRUM_SEPOLIA.perpsPublicLens,
      abi: PERPS_PUBLIC_LENS_ABI,
      functionName: 'getLpDepositCooldownState' as const,
      args: [isSenior, requestId, readController] as const,
    })),
  ], [isSenior, readController, requestIds])

  const { data, isLoading, refetch } = useReadContracts({
    contracts,
    allowFailure: true,
    query: {
      enabled: Boolean(controller) && contracts.length > 0,
      refetchInterval: 60_000,
    },
  })

  const { depositRequests, redeemRequests } = useMemo(() => {
    const results = data as readonly ContractResult[] | undefined
    const deposits: VaultDepositRequest[] = []
    const redeems: VaultRedeemRequest[] = []

    requestIds.forEach((requestId, index) => {
      const result = readResult(results, index)
      if (result === undefined) return
      const cooldownResult = readResult(results, requestIds.length + index)
      const pendingDepositAssets = asBigInt(tupleValue(result, 3, 'pendingDepositAssets'))
      const pendingDepositSharesEstimate = asBigInt(tupleValue(result, 4, 'pendingDepositSharesEstimate'))
      const claimableDepositAssets = asBigInt(tupleValue(result, 5, 'claimableDepositAssets'))
      const claimableDepositShares = asBigInt(tupleValue(result, 6, 'claimableDepositShares'))
      const pendingRedeemShares = asBigInt(tupleValue(result, 7, 'pendingRedeemShares'))
      const pendingRedeemAssetsEstimate = asBigInt(tupleValue(result, 8, 'pendingRedeemAssetsEstimate'))
      const claimableRedeemShares = asBigInt(tupleValue(result, 9, 'claimableRedeemShares'))
      const claimableRedeemAssets = asBigInt(tupleValue(result, 10, 'claimableRedeemAssets'))
      const refundableDepositAssets = asBigInt(tupleValue(result, 11, 'refundableDepositAssets'))
      const refundableRedeemShares = asBigInt(tupleValue(result, 12, 'refundableRedeemShares'))
      const redeemRefundPending = asBoolean(tupleValue(result, 13, 'redeemRefundPending'))
      const activationTime = asBigInt(tupleValue(cooldownResult, 3, 'activationTime'))
      const cooldownEnd = asBigInt(tupleValue(cooldownResult, 4, 'cooldownEnd'))
      const directRedeemableShares = asBigInt(tupleValue(
        cooldownResult,
        6,
        'directRedeemableShares',
      ))
      const matured = currentEpoch !== undefined && currentEpoch >= requestId
      const targetTimestamp = Number(requestId * 3_600n)

      if (
        pendingDepositAssets > 0n
        || claimableDepositAssets > 0n
        || claimableDepositShares > 0n
        || refundableDepositAssets > 0n
      ) {
        deposits.push({
          requestId,
          targetTimestamp,
          activationTimestamp: activationTime > 0n ? Number(activationTime) : undefined,
          cooldownEndsAt: cooldownEnd > 0n ? cooldownEnd : undefined,
          directRedeemableShares,
          directRedeemSupported: cooldownResult !== undefined,
          pendingAssets: pendingDepositAssets,
          pendingSharesEstimate: pendingDepositSharesEstimate,
          claimableAssets: claimableDepositAssets,
          claimableShares: claimableDepositShares,
          refundableAssets: refundableDepositAssets,
          matured,
        })
      }

      if (
        pendingRedeemShares > 0n
        || claimableRedeemShares > 0n
        || claimableRedeemAssets > 0n
        || refundableRedeemShares > 0n
        || redeemRefundPending
      ) {
        redeems.push({
          requestId,
          targetTimestamp,
          pendingShares: pendingRedeemShares,
          pendingAssetsEstimate: pendingRedeemAssetsEstimate,
          claimableShares: claimableRedeemShares,
          claimableAssets: claimableRedeemAssets,
          refundableShares: refundableRedeemShares,
          refundPending: redeemRefundPending,
          matured,
        })
      }
    })

    return { depositRequests: deposits, redeemRequests: redeems }
  }, [currentEpoch, data, requestIds])

  useEffect(() => {
    if (!controller || (depositRequests.length === 0 && redeemRequests.length === 0)) return
    const activeIds = [
      ...depositRequests.map(({ requestId }) => requestId),
      ...redeemRequests.map(({ requestId }) => requestId),
    ]
    writeCachedRequestIds(controller, isSenior, [...cachedRequestIds, ...eventRequestIds, ...activeIds])
  }, [cachedRequestIds, controller, depositRequests, eventRequestIds, isSenior, redeemRequests])

  const refresh = useCallback(() => {
    setDiscoveryNonce((value) => value + 1)
    void refetch()
  }, [refetch])

  return {
    depositRequests,
    redeemRequests,
    isLoading: Boolean(controller) && (isLoading || discoveryStatus === 'loading'),
    discoveryError: discoveryStatus === 'error',
    discoveryStale,
    refresh,
  }
}
