import { useInfiniteQuery, useQuery } from '@tanstack/react-query'
import { getScopedApiBaseUrl } from '../api/client'
import { usePerpsIdentity } from '../perps-aa'
import { parseProtectionExecutionReport } from '../utils/protectionExecution'

export function useProtectionExecution(protectionId: bigint | undefined, enabled: boolean) {
  const { manifest, accountAddress } = usePerpsIdentity()
  return useQuery({
    queryKey: ['perps', 'protection-execution', manifest?.positionProtectionBook, accountAddress, protectionId?.toString()],
    enabled: enabled && Boolean(manifest && accountAddress && protectionId),
    queryFn: async ({ signal }) => {
      if (!manifest || !protectionId) throw new Error('Protection deployment is unavailable')
      const params = new URLSearchParams({ book: manifest.positionProtectionBook })
      const response = await fetch(`${getScopedApiBaseUrl('perps').replace(/\/$/, '')}/perps/protections/${protectionId.toString()}/execution?${params}`, { signal, cache: 'no-store' })
      if (!response.ok) throw new Error('Execution status is temporarily unavailable')
      const body: unknown = await response.json()
      return parseProtectionExecutionReport(body && typeof body === 'object' && 'data' in body ? body.data : undefined)
    },
    refetchInterval: 5_000,
    retry: 1,
  })
}

export interface ProtectionHistoryRecord {
  protectionId: string; parentOrderId: string; linkedOrderId: string; account: string; side: number; size: string
  takeProfitTriggerPrice: string; stopLossTriggerPrice: string; triggerBountyUsdc: string; executionBountyUsdc: string
  armedAt: string; armedBlock: string; triggerMarkPrice: string; triggerPublishTime: string; triggeredLeg: number; status: number
  statusName: string; triggeredLegName: string; updatedBlock: string
}
export interface ProtectionHistoryEvent {
  event: string; args: Partial<Record<string, string | number | boolean>>; blockNumber: string; logIndex: string; blockHash: string; transactionHash: string
}
export function useProtectionEvents(protectionId: string, enabled: boolean) {
  const { manifest } = usePerpsIdentity()
  return useInfiniteQuery({
    queryKey: ['perps', 'protection-events', manifest?.positionProtectionBook, protectionId],
    enabled: enabled && Boolean(manifest),
    initialPageParam: undefined as string | undefined,
    queryFn: async ({ pageParam, signal }) => {
      if (!manifest) throw new Error('Protection deployment is unavailable')
      const params = new URLSearchParams({ limit: '25', book: manifest.positionProtectionBook })
      if (pageParam) params.set('cursor', pageParam)
      const response = await fetch(`${getScopedApiBaseUrl('perps').replace(/\/$/, '')}/perps/protections/${protectionId}/events?${params}`, { signal })
      if (!response.ok) throw new Error('Protection events are temporarily unavailable')
      return (await response.json() as { data: { events: ProtectionHistoryEvent[]; nextCursor?: string } }).data
    },
    getNextPageParam: page => page.nextCursor,
    refetchInterval: 15_000,
  })
}
export function useProtectionHistory(enabled: boolean) {
  const { accountAddress, manifest } = usePerpsIdentity()
  return useInfiniteQuery({
    queryKey: ['perps', 'protections', manifest?.positionProtectionBook, accountAddress],
    enabled: enabled && Boolean(accountAddress && manifest),
    initialPageParam: undefined as string | undefined,
    queryFn: async ({ pageParam, signal }) => {
      if (!manifest || !accountAddress) throw new Error('Confirm the Trading Account before loading protections')
      const params = new URLSearchParams({ limit: '25', book: manifest.positionProtectionBook })
      if (pageParam) params.set('cursor', pageParam)
      const response = await fetch(`${getScopedApiBaseUrl('perps').replace(/\/$/, '')}/perps/accounts/${accountAddress}/protections?${params}`, { signal })
      if (!response.ok) throw new Error('Protection history is temporarily unavailable')
      const body = await response.json() as { data: { protections: ProtectionHistoryRecord[]; nextCursor?: string; indexedThroughBlock?: string } }
      return body.data
    },
    getNextPageParam: page => page.nextCursor,
    refetchInterval: 15_000,
  })
}
