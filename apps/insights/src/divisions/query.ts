import { useQuery } from '@tanstack/react-query'
import type { DivisionPublication } from './types'
import { isPublication } from './schema'

export async function getDivisions(signal?: AbortSignal): Promise<DivisionPublication | null> {
  const response = await fetch('/divisions/current.json', { signal, cache: 'no-store', headers: { Accept: 'application/json' } })
  if (response.status === 404) return null
  if (!response.ok) throw new Error(`Divisions standings are unavailable (${String(response.status)})`)
  // Older Pages deployments can return the SPA HTML with 200 for a missing file.
  if (response.headers.get('Content-Type')?.toLowerCase().includes('text/html')) return null
  const data: unknown = await response.json().catch(() => { throw new Error('Divisions publication is invalid') })
  if (!isPublication(data) || Date.parse(data.publishedAt) > Date.now()) throw new Error('Divisions publication is invalid')
  return data
}

export function useDivisions() {
  return useQuery({ queryKey: ['insights', 'divisions'], queryFn: ({ signal }) => getDivisions(signal), staleTime: 30_000, refetchInterval: 60_000 })
}

export interface TradedWallet {
  address: string
  executedTrades: number
  lastTradeAt: string
}

export interface TradedWallets {
  traders: TradedWallet[]
  releaseRouter: string
  indexedThroughBlock: string
}

export async function getTradedWallets(signal?: AbortSignal): Promise<TradedWallets> {
  const response = await fetch('/api/insights/v1/divisions/traders', { signal, headers: { Accept: 'application/json' } })
  if (!response.ok) throw new Error(`Traded wallets are unavailable (${String(response.status)})`)
  const body: unknown = await response.json().catch(() => { throw new Error('Traded wallet feed is invalid') })
  if (typeof body !== 'object' || body === null || !('data' in body)) throw new Error('Traded wallet feed is invalid')
  const data = body.data
  if (typeof data !== 'object' || data === null || !('traders' in data) || !Array.isArray(data.traders)
    || !('releaseRouter' in data) || typeof data.releaseRouter !== 'string'
    || !/^0x[0-9a-fA-F]{40}$/.test(data.releaseRouter)
    || !('indexedThroughBlock' in data) || typeof data.indexedThroughBlock !== 'string'
    || !/^\d+$/.test(data.indexedThroughBlock)
    || !data.traders.every((row: unknown) => typeof row === 'object' && row !== null
      && 'address' in row && typeof row.address === 'string' && /^0x[0-9a-fA-F]{40}$/.test(row.address)
      && 'executedTrades' in row && typeof row.executedTrades === 'number' && Number.isSafeInteger(row.executedTrades) && row.executedTrades > 0
      && 'lastTradeAt' in row && typeof row.lastTradeAt === 'string' && Number.isFinite(Date.parse(row.lastTradeAt)))) {
    throw new Error('Traded wallet feed is invalid')
  }
  return data as TradedWallets
}

export function useTradedWallets() {
  return useQuery({ queryKey: ['insights', 'divisions', 'traded-wallets'], queryFn: ({ signal }) => getTradedWallets(signal), staleTime: 30_000, refetchInterval: 60_000 })
}

export interface LiveStanding {
  address: string
  discordUsername?: string | null
  rank: number | null
  weeklyReturnBps: number | null
  activeDays: number
}

export interface LiveDivisions {
  status: 'pending' | 'ready'
  weekStartsAt: string
  snapshotAt?: string
  snapshotBlock?: string
  walletCount?: number
  standings?: LiveStanding[]
}

export async function getLiveDivisions(signal?: AbortSignal): Promise<LiveDivisions> {
  const response = await fetch('/api/insights/v1/divisions/live', { signal, headers: { Accept: 'application/json' } })
  if (!response.ok) throw new Error(`Live Divisions are unavailable (${String(response.status)})`)
  const body: unknown = await response.json().catch(() => { throw new Error('Live Divisions response is invalid') })
  if (typeof body !== 'object' || body === null || !('data' in body)) throw new Error('Live Divisions response is invalid')
  const data = body.data
  if (typeof data !== 'object' || data === null || !('status' in data) || !('weekStartsAt' in data)
    || typeof data.weekStartsAt !== 'string' || !Number.isFinite(Date.parse(data.weekStartsAt))) {
    throw new Error('Live Divisions response is invalid')
  }
  if (data.status === 'pending') return data as LiveDivisions
  if (data.status !== 'ready' || !('snapshotAt' in data) || typeof data.snapshotAt !== 'string'
    || !Number.isFinite(Date.parse(data.snapshotAt)) || !('snapshotBlock' in data) || typeof data.snapshotBlock !== 'string'
    || !/^\d+$/.test(data.snapshotBlock) || !('walletCount' in data) || typeof data.walletCount !== 'number'
    || !Number.isSafeInteger(data.walletCount) || data.walletCount < 0 || !('standings' in data)
    || !Array.isArray(data.standings) || data.standings.length !== data.walletCount
    || !data.standings.every((row: unknown) => typeof row === 'object' && row !== null
      && 'address' in row && typeof row.address === 'string' && /^0x[0-9a-fA-F]{40}$/.test(row.address)
      && (!('discordUsername' in row) || row.discordUsername === null || typeof row.discordUsername === 'string' && row.discordUsername.length > 0)
      && 'rank' in row && (row.rank === null || typeof row.rank === 'number' && Number.isSafeInteger(row.rank) && row.rank > 0)
      && 'weeklyReturnBps' in row && (row.weeklyReturnBps === null || typeof row.weeklyReturnBps === 'number' && Number.isSafeInteger(row.weeklyReturnBps))
      && 'activeDays' in row && typeof row.activeDays === 'number' && Number.isSafeInteger(row.activeDays) && row.activeDays >= 0)) {
    throw new Error('Live Divisions response is invalid')
  }
  return data as LiveDivisions
}

export function useLiveDivisions() {
  return useQuery({ queryKey: ['insights', 'divisions', 'live'], queryFn: ({ signal }) => getLiveDivisions(signal), staleTime: 30_000, refetchInterval: 60_000 })
}
