import { useQuery } from '@tanstack/react-query'
import type { DivisionPublication } from './types'
import { isPublication } from './schema'

export async function getDivisions(signal?: AbortSignal): Promise<DivisionPublication | null> {
  const response = await fetch('/divisions/current.json', { signal, cache: 'no-store', headers: { Accept: 'application/json' } })
  if (response.status === 404) return null
  if (!response.ok) throw new Error(`Divisions standings are unavailable (${String(response.status)})`)
  const data: unknown = await response.json()
  if (!isPublication(data) || Date.parse(data.publishedAt) > Date.now()) throw new Error('Divisions publication is invalid')
  return data
}

export function useDivisions() {
  return useQuery({ queryKey: ['insights', 'divisions'], queryFn: ({ signal }) => getDivisions(signal), staleTime: 30_000, refetchInterval: 60_000 })
}
