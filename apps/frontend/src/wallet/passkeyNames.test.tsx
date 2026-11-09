import { act, renderHook, waitFor } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import type { ReactNode } from 'react'
import { afterEach, beforeEach, expect, it, vi } from 'vitest'
import { loadPasskeyNames, usePasskeyNames } from './passkeyNames'

const server = new Map<string, Record<string, string>>()
const fetchMock = vi.fn<typeof fetch>()
let failSave = false
beforeEach(() => {
  server.clear()
  failSave = false
  fetchMock.mockReset().mockImplementation(async (input, init) => {
    const url = new URL(String(input), 'https://app.example')
    const userId = init?.method === 'PUT' ? JSON.parse(init.body as string).userId as string : url.searchParams.get('userId')!
    if (init?.method === 'PUT') {
      if (failSave) return Response.json({}, { status: 503 })
      const { credentialId, name } = JSON.parse(init.body as string) as { credentialId: string; name: string }
      server.set(userId, { ...server.get(userId), [credentialId]: name })
      return Response.json({ userId, name })
    }
    return Response.json({ userId, names: Object.entries(server.get(userId) ?? {}).map(([credentialId, name]) => ({ credentialId, name })) })
  })
  vi.stubGlobal('fetch', fetchMock)
})
afterEach(() => { vi.unstubAllGlobals() })

function device(userId = 'did:privy:user-a', getToken = vi.fn().mockResolvedValue('access-token')) {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } })
  return renderHook(() => usePasskeyNames(userId, getToken), {
    wrapper: ({ children }: { children: ReactNode }) => <QueryClientProvider client={client}>{children}</QueryClientProvider>,
  })
}

it('authenticates requests and shares a saved label with a separate device/query cache', async () => {
  const laptop = device()
  await waitFor(() => expect(laptop.result.current.loading).toBe(false))
  await act(async () => { expect(await laptop.result.current.rename('public-security-key', '  YubiKey  ')).toBeNull() })
  await waitFor(() => expect(laptop.result.current.names['public-security-key']).toBe('YubiKey'))
  laptop.unmount()
  const phone = device()
  await waitFor(() => expect(phone.result.current.names['public-security-key']).toBe('YubiKey'))
  const otherAccount = device('did:privy:user-b')
  await waitFor(() => expect(otherAccount.result.current.loading).toBe(false))
  expect(otherAccount.result.current.names['public-security-key']).toBeUndefined()
  expect(fetchMock.mock.calls.every(([, init]) => init?.headers && new Headers(init.headers).get('Authorization') === 'Bearer access-token')).toBe(true)
  expect(fetchMock.mock.calls.every(([, init]) => init?.credentials === 'omit' && init.cache === 'no-store')).toBe(true)
})

it('clears one name without replacing another and requests a fresh token when saving', async () => {
  server.set('did:privy:user-a', { key1: 'YubiKey', key2: 'Phone' })
  const token = vi.fn().mockResolvedValueOnce('initial-token').mockResolvedValue('fresh-token')
  const view = device('did:privy:user-a', token)
  await waitFor(() => expect(view.result.current.names.key2).toBe('Phone'))
  await act(async () => { expect(await view.result.current.rename('key1', '')).toBeNull() })
  await waitFor(() => expect(view.result.current.names.key1).toBe(''))
  expect(view.result.current.names.key2).toBe('Phone')
  const write = fetchMock.mock.calls.find(([, init]) => init?.method === 'PUT')!
  expect(new Headers(write[1]!.headers).get('Authorization')).toBe('Bearer fresh-token')
})

it('keeps the old label on a failed save and permits retry', async () => {
  server.set('did:privy:user-a', { key1: 'Old name' })
  const view = device()
  await waitFor(() => expect(view.result.current.names.key1).toBe('Old name'))
  failSave = true
  await act(async () => { expect(await view.result.current.rename('key1', 'YubiKey')).toContain('temporarily unavailable') })
  expect(view.result.current.names.key1).toBe('Old name')
  failSave = false
  await act(async () => { expect(await view.result.current.rename('key1', 'YubiKey')).toBeNull() })
  await waitFor(() => expect(view.result.current.names.key1).toBe('YubiKey'))
})

it('rejects a response for another account instead of caching its labels', async () => {
  fetchMock.mockResolvedValueOnce(Response.json({ userId: 'did:privy:other', names: [{ credentialId: 'key1', name: 'Private name' }] }))
  await expect(loadPasskeyNames('did:privy:user-a', 'token')).rejects.toThrow('Could not verify the account')
})

it('does not send a request without authentication or use a browser-only fallback', async () => {
  await expect(loadPasskeyNames('did:privy:user-a', null)).rejects.toThrow('Sign in again')
  expect(fetchMock).not.toHaveBeenCalled()
})

it('rejects malformed responses and expired authentication', async () => {
  fetchMock.mockResolvedValueOnce(Response.json({ userId: 'did:privy:user-a', names: [{ name: 'Missing credential' }] }))
  await expect(loadPasskeyNames('did:privy:user-a', 'token')).rejects.toThrow('Could not load')
  fetchMock.mockResolvedValueOnce(Response.json({}, { status: 401 }))
  await expect(loadPasskeyNames('did:privy:user-a', 'expired-token')).rejects.toThrow('signed-in account changed')
})
