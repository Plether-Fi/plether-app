import { test as base, expect, type Page } from '@playwright/test'
import { isAddressEqual, isHex, type Hex } from 'viem'
import { mnemonicToAccount } from 'viem/accounts'

// Public Anvil/Hardhat test mnemonic. Never use these accounts with real funds.
const TEST_MNEMONIC = 'test test test test test test test test test test test junk'
const testAccounts = [0, 1].map(addressIndex => mnemonicToAccount(TEST_MNEMONIC, { addressIndex }))
export const TEST_WALLET_ADDRESSES = testAccounts.map(account => account.address)
export const TEST_WALLET_CHAIN_ID = 11155111

interface WalletRequest {
  method: string
  params?: unknown[]
}

interface SigningRequest extends WalletRequest {
  address: string
}

type SigningResult = { result: Hex } | { error: { code: number; message: string } }

interface BrowserWalletControls {
  setAccount(index: number): void
  setChain(chainId: number): void
  disconnect(): void
  rejectNextRequest(method: string): void
}

export interface TestWalletController {
  setAccount(index: number): Promise<void>
  setChain(chainId: number): Promise<void>
  disconnect(): Promise<void>
  rejectNextRequest(method: string): Promise<void>
}

interface TestWalletWindow extends Window {
  __pletherTestWalletSign(input: SigningRequest): Promise<SigningResult>
  __pletherTestWallet: BrowserWalletControls
}

async function signRequest({ method, address, params = [] }: SigningRequest): Promise<SigningResult> {
  const account = testAccounts.find(candidate => isAddressEqual(candidate.address, address as Hex))
  if (!account) return { error: { code: 4100, message: 'Unknown test wallet account' } }
  try {
    if (method === 'personal_sign') {
      const message = params[0]
      if (typeof message !== 'string') throw new Error('personal_sign requires a message')
      return { result: await account.signMessage({ message: isHex(message) ? { raw: message } : message }) }
    }
    if (method === 'eth_signTypedData_v4') {
      const raw = params[1]
      const typedData = typeof raw === 'string' ? JSON.parse(raw) : raw
      return { result: await account.signTypedData(typedData as Parameters<typeof account.signTypedData>[0]) }
    }
    return { error: { code: 4200, message: `Unsupported signing method: ${method}` } }
  } catch (error) {
    return { error: { code: -32602, message: error instanceof Error ? error.message : 'Invalid signing request' } }
  }
}

/** Inject a signing wallet without contacting an RPC, OAuth service, or Privy. */
export async function installTestWallet(page: Page): Promise<TestWalletController> {
  await page.exposeBinding('__pletherTestWalletSign', (_source, request: SigningRequest) => signRequest(request))
  await page.addInitScript(({ addresses, initialChainId }) => {
    const testWindow = window as unknown as TestWalletWindow
    const listeners = new Map<string, Set<(payload: unknown) => void>>()
    const rejectedMethods = new Set<string>()
    const chains = new Set([1, 11155111, 421614, 31337])
    let currentAddress = addresses[0]
    let chainId = initialChainId
    let authorized = true
    const rpcError = (code: number, message: string) => Object.assign(new Error(message), { code })
    const emit = (event: string, payload: unknown) => {
      for (const listener of [...(listeners.get(event) ?? [])]) listener(payload)
    }
    const requireAuthorized = (address: unknown) => {
      if (!authorized || typeof address !== 'string' || address.toLowerCase() !== currentAddress.toLowerCase()) {
        throw rpcError(4100, 'The requested account is not authorized')
      }
    }
    const updateChain = (nextChainId: number) => {
      if (!chains.has(nextChainId)) throw rpcError(4902, 'Unrecognized chain; add it before switching')
      if (chainId === nextChainId) return
      chainId = nextChainId
      provider.chainId = `0x${chainId.toString(16)}`
      provider.networkVersion = String(chainId)
      emit('chainChanged', provider.chainId)
    }
    const disconnect = () => {
      authorized = false
      provider.selectedAddress = null
      emit('accountsChanged', [])
      emit('disconnect', { code: 4900, message: 'Test wallet disconnected' })
    }
    const provider = {
      isMetaMask: true,
      selectedAddress: currentAddress as string | null,
      chainId: `0x${chainId.toString(16)}`,
      networkVersion: String(chainId),
      isConnected: () => authorized,
      _metamask: { isUnlocked: async () => authorized },
      async request({ method, params = [] }: WalletRequest): Promise<unknown> {
        if (rejectedMethods.delete(method)) throw rpcError(4001, 'User rejected the request')
        switch (method) {
          case 'eth_accounts': return authorized ? [currentAddress] : []
          case 'eth_requestAccounts':
          case 'wallet_requestPermissions': {
            if (!authorized) {
              authorized = true
              provider.selectedAddress = currentAddress
              emit('connect', { chainId: provider.chainId })
              emit('accountsChanged', [currentAddress])
            }
            return method === 'eth_requestAccounts' ? [currentAddress] : [{ parentCapability: 'eth_accounts' }]
          }
          case 'wallet_getPermissions': return authorized ? [{ parentCapability: 'eth_accounts' }] : []
          case 'wallet_revokePermissions': disconnect(); return null
          case 'eth_chainId': return provider.chainId
          case 'net_version': return provider.networkVersion
          case 'wallet_addEthereumChain':
          case 'wallet_switchEthereumChain': {
            const next = params[0] as { chainId?: unknown } | undefined
            if (typeof next?.chainId !== 'string' || !/^0x[0-9a-f]+$/i.test(next.chainId)) {
              throw rpcError(-32602, 'A hexadecimal chainId is required')
            }
            const nextChainId = Number.parseInt(next.chainId, 16)
            if (method === 'wallet_addEthereumChain') chains.add(nextChainId)
            else updateChain(nextChainId)
            return null
          }
          case 'personal_sign':
          case 'eth_signTypedData_v4': {
            requireAuthorized(params[method === 'personal_sign' ? 1 : 0])
            const signed = await testWindow.__pletherTestWalletSign({ method, params, address: currentAddress })
            if ('error' in signed) throw rpcError(signed.error.code, signed.error.message)
            return signed.result
          }
          default: throw rpcError(4200, `Unsupported test wallet method: ${method}`)
        }
      },
      on(event: string, callback: (payload: unknown) => void) {
        if (!listeners.has(event)) listeners.set(event, new Set())
        listeners.get(event)!.add(callback)
        return provider
      },
      removeListener(event: string, callback: (payload: unknown) => void) {
        listeners.get(event)?.delete(callback)
        return provider
      },
      removeAllListeners(event?: string) {
        if (event) listeners.delete(event)
        else listeners.clear()
        return provider
      },
    }
    testWindow.__pletherTestWallet = {
      setAccount(index) {
        const nextAddress = addresses[index]
        if (!nextAddress) throw new Error('Unknown test account index')
        const wasAuthorized = authorized
        const changed = currentAddress !== nextAddress || !authorized
        currentAddress = nextAddress
        authorized = true
        provider.selectedAddress = currentAddress
        if (!wasAuthorized) emit('connect', { chainId: provider.chainId })
        if (changed) emit('accountsChanged', [currentAddress])
      },
      setChain: updateChain,
      disconnect,
      rejectNextRequest: method => { rejectedMethods.add(method) },
    }
    Object.defineProperty(window, 'ethereum', { value: provider, configurable: true })
    const announce = () => window.dispatchEvent(new CustomEvent('eip6963:announceProvider', {
      detail: Object.freeze({
        info: {
          uuid: '0b201946-592a-4ebd-a7df-316e1ca5f040',
          name: 'Plether Test Wallet',
          icon: 'data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" width="32" height="32"><rect fill="%23FF572D" width="32" height="32"/></svg>',
          rdns: 'com.plether.test-wallet',
        },
        provider,
      }),
    }))
    window.addEventListener('eip6963:requestProvider', announce)
    announce()
  }, { addresses: TEST_WALLET_ADDRESSES, initialChainId: TEST_WALLET_CHAIN_ID })

  return {
    setAccount: index => page.evaluate(value => (window as unknown as TestWalletWindow).__pletherTestWallet.setAccount(value), index),
    setChain: chainId => page.evaluate(value => (window as unknown as TestWalletWindow).__pletherTestWallet.setChain(value), chainId),
    disconnect: () => page.evaluate(() => (window as unknown as TestWalletWindow).__pletherTestWallet.disconnect()),
    rejectNextRequest: method => page.evaluate(value => (window as unknown as TestWalletWindow).__pletherTestWallet.rejectNextRequest(value), method),
  }
}

export const test = base.extend<{ mockConnectedWallet: () => Promise<TestWalletController> }>({
  mockConnectedWallet: async ({ page }, use) => {
    let installed: Promise<TestWalletController> | undefined
    await use(() => installed ??= installTestWallet(page))
  },
})

export { expect }

export async function waitForPageLoad(page: Page) {
  await page.waitForLoadState('networkidle')
}

export async function clickConnectWallet(page: Page) {
  const connectButton = page.getByRole('button', { name: /connect wallet/i }).first()
  if (await connectButton.isVisible() && await connectButton.isEnabled()) await connectButton.click()
}
