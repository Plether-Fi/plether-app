import type { EIP1193Provider } from '@privy-io/react-auth'
import { describe, expect, it, vi } from 'vitest'
import { createSigningGuard } from './signingGuard'

const first = '0x1111111111111111111111111111111111111111'
const second = '0x2222222222222222222222222222222222222222'

function wallet(address = first) {
  const request = vi.fn(async () => '0xsignature')
  const provider = { request, on: vi.fn(), removeListener: vi.fn() } satisfies EIP1193Provider
  return { address, provider, request, getEthereumProvider: async (): Promise<EIP1193Provider> => provider }
}

function deferredProvider() {
  let resolveProvider: (provider: EIP1193Provider) => void = () => { throw new Error('Not initialized') }
  const pending = new Promise<EIP1193Provider>((resolve) => { resolveProvider = resolve })
  return { pending, resolveProvider }
}

describe('owner signing authority', () => {
  it.each([second, null])('blocks a late provider after selection changes to %s', async (nextOwner) => {
    const source = wallet()
    const { pending, resolveProvider } = deferredProvider()
    const guard = createSigningGuard(first)
    guard.setReady(true)
    const oldWallet = guard.wrapWallet({ address: first, getEthereumProvider: () => pending })
    const lateProvider = oldWallet.getEthereumProvider()
    guard.select(nextOwner)
    resolveProvider(source.provider)
    const provider = await lateProvider
    await expect(provider.request({ method: 'personal_sign', params: ['message', first] })).rejects.toThrow(/selected wallet changed/)
    expect(source.request).not.toHaveBeenCalled()
    await expect(provider.request({ method: 'eth_chainId' })).resolves.toBe('0xsignature')
  })

  it('keeps the active connector usable when the same owner is selected again', async () => {
    const source = wallet()
    const guard = createSigningGuard(first)
    guard.setReady(true)
    const provider = await guard.wrapWallet(source).getEthereumProvider()
    guard.select(first)
    await expect(provider.request({ method: 'personal_sign' })).resolves.toBe('0xsignature')
  })

  it('keeps a cached connector blocked until its owner is explicitly selected again', async () => {
    const source = wallet()
    const guard = createSigningGuard(first)
    guard.setReady(true)
    const oldProvider = await guard.wrapWallet(source).getEthereumProvider()
    guard.select(null)
    await expect(oldProvider.request({ method: 'eth_sendTransaction' })).rejects.toThrow(/selected wallet changed/)
    guard.select(first)
    const currentProvider = await guard.wrapWallet(source).getEthereumProvider()
    await expect(oldProvider.request({ method: 'eth_sendTransaction' })).resolves.toBe('0xsignature')
    await expect(currentProvider.request({ method: 'eth_sendTransaction' })).resolves.toBe('0xsignature')
    guard.select(second)
    await expect(oldProvider.request({ method: 'personal_sign' })).rejects.toThrow(/selected wallet changed/)
    guard.select(first)
    await expect(oldProvider.request({ method: 'personal_sign' })).resolves.toBe('0xsignature')
  })

  it.each(['eth_signTypedData_v4', 'eth_sendRawTransaction', 'wallet_sendCalls', 'wallet_sign', 'wallet_switchEthereumChain', 'wallet_addEthereumChain'])('guards %s before it reaches the wallet', async (method) => {
    const source = wallet()
    const guard = createSigningGuard(first)
    guard.setReady(true)
    const provider = await guard.wrapWallet(source).getEthereumProvider()
    guard.select(second)
    await expect(provider.request({ method })).rejects.toThrow(/selected wallet changed/)
    expect(source.request).not.toHaveBeenCalled()
  })

  it('blocks signing while SDK initialization is incomplete and resumes for the unchanged owner', async () => {
    const source = wallet()
    const guard = createSigningGuard(first)
    const provider = await guard.wrapWallet(source).getEthereumProvider()
    await expect(provider.request({ method: 'personal_sign' })).rejects.toThrow(/not ready/)
    guard.setReady(true)
    await expect(provider.request({ method: 'personal_sign' })).resolves.toBe('0xsignature')
  })

  it('preserves provider method binding for event listeners', async () => {
    const listeners = new Set<string>()
    const source = {
      address: first,
      getEthereumProvider: async () => ({
        request: async () => '0xsignature',
        listeners,
        on(this: { listeners: Set<string> }, event: string) { this.listeners.add(event) },
        removeListener(this: { listeners: Set<string> }, event: string | symbol) { this.listeners.delete(String(event)) },
      }),
    }
    const guard = createSigningGuard(first)
    const provider: EIP1193Provider = await guard.wrapWallet(source).getEthereumProvider()
    provider.on('accountsChanged', () => {})
    expect(listeners.has('accountsChanged')).toBe(true)
    provider.removeListener('accountsChanged', () => {})
    expect(listeners.has('accountsChanged')).toBe(false)
  })

  it('gives accounts in the same extension distinct bridge identities and signs only for the selected one', async () => {
    const source = wallet()
    const meta = { id: 'io.metamask', name: 'MetaMask' }
    const firstAccount = { ...source, meta, walletClientType: 'metamask' }
    const secondAccount = { ...source, address: second, meta, walletClientType: 'metamask' }
    const guard = createSigningGuard(first)
    guard.setReady(true)
    const wrappedFirst = guard.wrapWallet(firstAccount)
    const wrappedSecond = guard.wrapWallet(secondAccount)
    expect(wrappedFirst.meta.id).not.toBe(wrappedSecond.meta.id)
    expect(wrappedFirst.meta.name).toBe('MetaMask')
    expect(firstAccount.meta.id).toBe('io.metamask')
    const firstProvider = await wrappedFirst.getEthereumProvider()
    const secondProvider = await wrappedSecond.getEthereumProvider()
    guard.select(second)
    await expect(firstProvider.request({ method: 'personal_sign' })).rejects.toThrow(/selected wallet changed/)
    await expect(secondProvider.request({ method: 'personal_sign' })).resolves.toBe('0xsignature')
    guard.select(first)
    await expect(firstProvider.request({ method: 'personal_sign' })).resolves.toBe('0xsignature')
  })

  it('retains Privy embedded wallet metadata because its bridge identity already includes its address', () => {
    const source = { ...wallet(), walletClientType: 'privy', meta: { id: 'io.privy.wallet', name: 'Privy' } }
    const guard = createSigningGuard(first)
    expect(guard.wrapWallet(source).meta).toBe(source.meta)
  })
})
