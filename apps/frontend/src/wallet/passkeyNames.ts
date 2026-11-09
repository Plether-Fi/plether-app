import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import { getScopedApiBaseUrl } from '../api/client'

export const MAX_PASSKEY_NAME_LENGTH = 64
export type PasskeyNames = Record<string, string>

async function request(userId: string, token: string | null, signal?: AbortSignal, update?: { credentialId: string; name: string }) {
  if (!token) throw new Error('Sign in again to manage passkey names.')
  const response = await fetch(`${getScopedApiBaseUrl('perps')}/user/passkey-names${update ? '' : `?userId=${encodeURIComponent(userId)}`}`, {
    method: update ? 'PUT' : 'GET', credentials: 'omit', cache: 'no-store',
    headers: { Authorization: `Bearer ${token}`, ...(update ? { 'Content-Type': 'application/json' } : {}) },
    body: update ? JSON.stringify({ userId, ...update }) : undefined,
    signal: signal ? AbortSignal.any([signal, AbortSignal.timeout(10000)]) : AbortSignal.timeout(10000),
  })
  if (!response.ok) throw new Error(response.status === 401
    ? 'The signed-in account changed. Sign in again to manage passkey names.'
    : 'Passkey names are temporarily unavailable. Try again.')
  const data: unknown = await response.json()
  if (!data || typeof data !== 'object' || !('userId' in data) || data.userId !== userId) {
    throw new Error('Could not verify the account for these passkey names.')
  }
  return data
}

export async function loadPasskeyNames(userId: string, token: string | null, signal?: AbortSignal): Promise<PasskeyNames> {
  const data = await request(userId, token, signal)
  if (!('names' in data) || !Array.isArray(data.names) || data.names.length > 200) throw new Error('Could not load passkey names.')
  const names: PasskeyNames = Object.create(null) as PasskeyNames
  for (const item of data.names as unknown[]) {
    if (!item || typeof item !== 'object' || !('credentialId' in item) || typeof item.credentialId !== 'string'
      || !('name' in item) || typeof item.name !== 'string' || item.name.length > MAX_PASSKEY_NAME_LENGTH) throw new Error('Could not load passkey names.')
    names[item.credentialId] = item.name
  }
  return names
}

export function usePasskeyNames(userId: string, getAccessToken: () => Promise<string | null>) {
  const client = useQueryClient()
  const queryKey = ['passkey-names', userId]
  const query = useQuery({ queryKey, retry: false, staleTime: 0, refetchOnMount: 'always',
    queryFn: async ({ signal }) => loadPasskeyNames(userId, await getAccessToken(), signal) })
  const mutation = useMutation({
    mutationFn: async ({ credentialId, name }: { credentialId: string; name: string }) => {
      const data = await request(userId, await getAccessToken(), undefined, { credentialId, name: name.trim() })
      if (!('name' in data) || typeof data.name !== 'string' || data.name !== name.trim()) throw new Error('Could not save the name. Try again.')
      return { credentialId, name: data.name }
    },
    onSuccess: ({ credentialId, name }) => {
      client.setQueryData<PasskeyNames>(queryKey, previous => ({ ...previous, [credentialId]: name }))
      void client.invalidateQueries({ queryKey })
    },
  })
  return {
    names: query.data ?? {}, loading: query.isPending, error: query.error?.message ?? null,
    retry: () => { void query.refetch() }, saving: mutation.isPending,
    rename: async (credentialId: string, name: string) => {
      if (name.trim().length > MAX_PASSKEY_NAME_LENGTH) return 'Use 64 characters or fewer.'
      try { await mutation.mutateAsync({ credentialId, name }); return null }
      catch (error) { return error instanceof Error ? error.message : 'Could not save the name. Try again.' }
    },
  }
}
