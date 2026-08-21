import { useCallback, useEffect, useMemo, useState } from 'react'
import { keccak256, padHex, toBytes, zeroAddress, type Address } from 'viem'
import { useReadContracts } from 'wagmi'
import { TRANCHE_VAULT_READ_ABI } from '../contracts/abis'
import { PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'

const BLOCKSCOUT_LOGS_URL = 'https://arbitrum-sepolia.blockscout.com/api'
const VAULT_DEPLOYMENT_BLOCK = 288_439_939
const DEPOSIT_REQUESTED_TOPIC = keccak256(
  toBytes('DepositRequested(address,address,uint256,uint256)')
)
const NEARBY_EPOCH_LOOKBACK = 4n

interface ContractResult {
  status: 'failure' | 'success'
  result?: unknown
}

interface BlockscoutLog {
  topics?: string[]
}

interface BlockscoutLogsResponse {
  status?: string
  result?: BlockscoutLog[] | string
}

export type PendingVaultDepositStatus = 'waiting' | 'ready' | 'claimable'

export interface PendingVaultDeposit {
  epochId: bigint
  assets: bigint
  epochAssets: bigint
  epochShares: bigint
  claimedAssets: bigint
  claimedShares: bigint
  claimableShares?: bigint
  activationTimestamp: number
  finalized: boolean
  status: PendingVaultDepositStatus
}

interface UsePendingVaultDepositsOptions {
  owner?: Address
  vaultAddress: Address
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

function asBigInt(value: unknown): bigint | undefined {
  return typeof value === 'bigint' ? value : undefined
}

function asBoolean(value: unknown): boolean | undefined {
  return typeof value === 'boolean' ? value : undefined
}

function cacheKey(owner: Address, vaultAddress: Address): string {
  return `plether:vault-deposit-epochs:${String(PERPS_ARBITRUM_SEPOLIA_CHAIN_ID)}:${vaultAddress.toLowerCase()}:${owner.toLowerCase()}`
}

function readCachedEpochIds(owner: Address, vaultAddress: Address): bigint[] {
  if (typeof window === 'undefined') return []
  try {
    const parsed = JSON.parse(
      window.localStorage.getItem(cacheKey(owner, vaultAddress)) ?? '[]'
    ) as unknown
    if (!Array.isArray(parsed)) return []
    return parsed
      .filter((value): value is string => typeof value === 'string' && /^\d+$/.test(value))
      .map((value) => BigInt(value))
  } catch {
    return []
  }
}

function writeCachedEpochIds(owner: Address, vaultAddress: Address, epochIds: bigint[]) {
  if (typeof window === 'undefined') return
  try {
    const uniqueIds = [...new Set(epochIds.map(String))]
    window.localStorage.setItem(cacheKey(owner, vaultAddress), JSON.stringify(uniqueIds))
  } catch {
    // Event discovery and direct onchain reads remain authoritative if storage is unavailable.
  }
}

async function discoverDepositEpochIds(
  owner: Address,
  vaultAddress: Address,
  signal: AbortSignal
): Promise<bigint[]> {
  const params = new URLSearchParams({
    module: 'logs',
    action: 'getLogs',
    fromBlock: String(VAULT_DEPLOYMENT_BLOCK),
    toBlock: 'latest',
    address: vaultAddress,
    topic0: DEPOSIT_REQUESTED_TOPIC,
    topic2: padHex(owner, { size: 32 }),
    topic0_2_opr: 'and',
  })
  const response = await fetch(`${BLOCKSCOUT_LOGS_URL}?${params}`, { signal })
  if (!response.ok) {
    throw new Error(`Pending-deposit discovery failed with HTTP ${String(response.status)}.`)
  }

  const payload = await response.json() as BlockscoutLogsResponse
  if (!Array.isArray(payload.result)) {
    if (payload.status === '0' && typeof payload.result === 'string') return []
    throw new Error('Pending-deposit discovery returned an invalid response.')
  }

  return payload.result.flatMap((log) => {
    const epochTopic = log.topics?.[3]
    if (!epochTopic || !/^0x[0-9a-fA-F]{64}$/.test(epochTopic)) return []
    return [BigInt(epochTopic)]
  })
}

export function usePendingVaultDeposits({
  owner,
  vaultAddress,
  currentEpoch,
}: UsePendingVaultDepositsOptions) {
  const [cachedEpochIds, setCachedEpochIds] = useState<bigint[]>([])
  const [eventEpochIds, setEventEpochIds] = useState<bigint[]>([])
  const [discoveryStatus, setDiscoveryStatus] = useState<'idle' | 'loading' | 'success' | 'error'>('idle')
  const [discoveryNonce, setDiscoveryNonce] = useState(0)
  const [now, setNow] = useState(() => Date.now())

  useEffect(() => {
    setCachedEpochIds(owner ? readCachedEpochIds(owner, vaultAddress) : [])
    setEventEpochIds([])
    setDiscoveryStatus(owner ? 'loading' : 'idle')
  }, [owner, vaultAddress])

  useEffect(() => {
    if (!owner) return
    const controller = new AbortController()
    setDiscoveryStatus('loading')

    void discoverDepositEpochIds(owner, vaultAddress, controller.signal)
      .then((epochIds) => {
        const merged = [...cachedEpochIds, ...epochIds]
        setEventEpochIds(epochIds)
        writeCachedEpochIds(owner, vaultAddress, merged)
        setDiscoveryStatus('success')
      })
      .catch((error: unknown) => {
        if (error instanceof DOMException && error.name === 'AbortError') return
        setDiscoveryStatus('error')
      })

    return () => {
      controller.abort()
    }
  }, [cachedEpochIds, discoveryNonce, owner, vaultAddress])

  useEffect(() => {
    const interval = window.setInterval(() => {
      setNow(Date.now())
    }, 60_000)
    return () => {
      window.clearInterval(interval)
    }
  }, [])

  const epochIds = useMemo(() => {
    const ids = new Set<string>()
    for (const epochId of [...cachedEpochIds, ...eventEpochIds]) ids.add(String(epochId))

    if (currentEpoch !== undefined) {
      const firstNearbyEpoch = currentEpoch > NEARBY_EPOCH_LOOKBACK
        ? currentEpoch - NEARBY_EPOCH_LOOKBACK
        : 0n
      for (let epochId = firstNearbyEpoch; epochId <= currentEpoch + 2n; epochId += 1n) {
        ids.add(String(epochId))
      }
    }

    return [...ids].map(BigInt).sort((left, right) => left > right ? -1 : left < right ? 1 : 0)
  }, [cachedEpochIds, currentEpoch, eventEpochIds])

  const readAccount = owner ?? zeroAddress
  const contracts = useMemo(() => epochIds.flatMap((epochId) => [
    {
      chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID as 421614,
      address: vaultAddress,
      abi: TRANCHE_VAULT_READ_ABI,
      functionName: 'pendingDepositAssets' as const,
      args: [readAccount, epochId] as const,
    },
    {
      chainId: PERPS_ARBITRUM_SEPOLIA_CHAIN_ID as 421614,
      address: vaultAddress,
      abi: TRANCHE_VAULT_READ_ABI,
      functionName: 'depositEpochs' as const,
      args: [epochId] as const,
    },
  ]), [epochIds, readAccount, vaultAddress])

  const { data, isLoading, refetch } = useReadContracts({
    contracts,
    query: {
      enabled: Boolean(owner) && contracts.length > 0,
      refetchInterval: 60_000,
    },
  })

  const deposits = useMemo(() => {
    const results = data as readonly ContractResult[] | undefined
    const nowSeconds = Math.floor(now / 1_000)

    return epochIds.flatMap((epochId, index): PendingVaultDeposit[] => {
      const assets = asBigInt(readResult(results, index * 2))
      const epochResult = readResult(results, index * 2 + 1)
      const epochAssets = asBigInt(tupleValue(epochResult, 0, 'assets'))
      const epochShares = asBigInt(tupleValue(epochResult, 1, 'shares'))
      const claimedAssets = asBigInt(tupleValue(epochResult, 2, 'claimedAssets'))
      const claimedShares = asBigInt(tupleValue(epochResult, 3, 'claimedShares'))
      const finalized = asBoolean(tupleValue(epochResult, 4, 'finalized'))

      if (
        assets === undefined
        || assets === 0n
        || epochAssets === undefined
        || epochShares === undefined
        || claimedAssets === undefined
        || claimedShares === undefined
        || finalized === undefined
      ) {
        return []
      }

      const activationTimestamp = Number(epochId * 3_600n)
      let claimableShares: bigint | undefined
      if (finalized && epochAssets > 0n) {
        const remainingAssets = epochAssets - claimedAssets
        claimableShares = assets === remainingAssets
          ? epochShares - claimedShares
          : assets * epochShares / epochAssets
      }

      return [{
        epochId,
        assets,
        epochAssets,
        epochShares,
        claimedAssets,
        claimedShares,
        claimableShares,
        activationTimestamp,
        finalized,
        status: finalized
          ? 'claimable'
          : nowSeconds >= activationTimestamp
            ? 'ready'
            : 'waiting',
      }]
    })
  }, [data, epochIds, now])

  useEffect(() => {
    if (!owner || deposits.length === 0) return
    const merged = [...cachedEpochIds, ...eventEpochIds, ...deposits.map(({ epochId }) => epochId)]
    writeCachedEpochIds(owner, vaultAddress, merged)
  }, [cachedEpochIds, deposits, eventEpochIds, owner, vaultAddress])

  const refresh = useCallback(() => {
    setDiscoveryNonce((value) => value + 1)
    void refetch()
  }, [refetch])

  return {
    deposits,
    isLoading: Boolean(owner) && (isLoading || discoveryStatus === 'loading'),
    discoveryError: discoveryStatus === 'error',
    refresh,
  }
}
