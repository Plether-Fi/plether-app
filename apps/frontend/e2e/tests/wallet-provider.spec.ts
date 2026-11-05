import { type Page } from '@playwright/test'
import { stringToHex, verifyMessage, verifyTypedData, type Hex } from 'viem'
import { test, expect, TEST_WALLET_ADDRESSES } from '../fixtures/signingWallet'

interface TestProviderWindow extends Window {
  ethereum: {
    request(input: { method: string; params?: unknown[] }): Promise<unknown>
    on(event: string, callback: (payload: unknown) => void): void
    removeListener(event: string, callback: (payload: unknown) => void): void
    selectedAddress: string | null
  }
  walletEvents: { event: string; payload: unknown }[]
}

async function request<T>(page: Page, method: string, params: unknown[] = []): Promise<T> {
  return page.evaluate(input => (window as unknown as TestProviderWindow).ethereum.request(input), { method, params }) as Promise<T>
}

async function requestError(page: Page, method: string, params: unknown[] = []) {
  return page.evaluate(async input => {
    try {
      await (window as unknown as TestProviderWindow).ethereum.request(input)
      return null
    } catch (error) {
      const caught = error as { code: number; message: string }
      return { code: caught.code, message: caught.message }
    }
  }, { method, params })
}

test.describe('Deterministic EIP-1193 wallet', () => {
  test.beforeEach(async ({ page, mockConnectedWallet }) => {
    await mockConnectedWallet()
    await page.goto('about:blank')
  })

  test('discovers the same provider through EIP-6963 and window.ethereum', async ({ page }) => {
    const discovered = await page.evaluate(() => {
      let result: { name: string; sameProvider: boolean } | undefined
      window.addEventListener('eip6963:announceProvider', event => {
        const detail = (event as CustomEvent).detail
        result = { name: detail.info.name, sameProvider: detail.provider === (window as unknown as TestProviderWindow).ethereum }
      })
      window.dispatchEvent(new Event('eip6963:requestProvider'))
      return result
    })
    expect(discovered).toEqual({ name: 'Plether Test Wallet', sameProvider: true })
    expect(await request(page, 'eth_accounts')).toEqual([TEST_WALLET_ADDRESSES[0]])
    expect(await request(page, 'eth_chainId')).toBe('0xaa36a7')
  })

  test('produces recoverable personal signatures and signs with the selected account', async ({ page, mockConnectedWallet }) => {
    const wallet = await mockConnectedWallet()
    const message = 'Verify the owner wallet for Plether; no transaction is submitted.'
    const signature = await request<Hex>(page, 'personal_sign', [stringToHex(message), TEST_WALLET_ADDRESSES[0]])
    expect(signature).toMatch(/^0x[0-9a-f]{130}$/)
    expect(await verifyMessage({ address: TEST_WALLET_ADDRESSES[0], message, signature })).toBe(true)
    expect(await verifyMessage({ address: TEST_WALLET_ADDRESSES[1], message, signature })).toBe(false)

    await wallet.setAccount(1)
    expect(await requestError(page, 'personal_sign', [stringToHex(message), TEST_WALLET_ADDRESSES[0]])).toMatchObject({ code: 4100 })
    const nextSignature = await request<Hex>(page, 'personal_sign', [stringToHex(message), TEST_WALLET_ADDRESSES[1]])
    expect(await verifyMessage({ address: TEST_WALLET_ADDRESSES[1], message, signature: nextSignature })).toBe(true)
  })

  test('produces a verifiable EIP-712 signature rather than a fabricated result', async ({ page }) => {
    const typedData = {
      domain: { name: 'Plether Wallet Test', version: '1', chainId: 421614 },
      types: { Authorization: [{ name: 'owner', type: 'address' }, { name: 'nonce', type: 'uint256' }] },
      primaryType: 'Authorization',
      message: { owner: TEST_WALLET_ADDRESSES[0], nonce: '42' },
    } as const
    const signature = await request<Hex>(page, 'eth_signTypedData_v4', [TEST_WALLET_ADDRESSES[0], JSON.stringify(typedData)])
    expect(await verifyTypedData({
      address: TEST_WALLET_ADDRESSES[0],
      ...typedData,
      message: { ...typedData.message, nonce: 42n },
      signature,
    })).toBe(true)
    expect(await verifyTypedData({
      address: TEST_WALLET_ADDRESSES[0],
      ...typedData,
      message: { ...typedData.message, nonce: 43n },
      signature,
    })).toBe(false)
  })

  test('switches the actual reported network and removes event listeners', async ({ page, mockConnectedWallet }) => {
    const wallet = await mockConnectedWallet()
    await page.evaluate(() => {
      const target = window as unknown as TestProviderWindow
      target.walletEvents = []
      const removedListener = () => { throw new Error('Removed listener was called') }
      target.ethereum.on('chainChanged', removedListener)
      target.ethereum.removeListener('chainChanged', removedListener)
      target.ethereum.on('chainChanged', payload => target.walletEvents.push({ event: 'chainChanged', payload }))
    })
    expect(await request(page, 'wallet_switchEthereumChain', [{ chainId: '0x66eee' }])).toBeNull()
    expect(await request(page, 'eth_chainId')).toBe('0x66eee')
    expect(await request(page, 'net_version')).toBe('421614')
    await wallet.setChain(421614)
    expect(await page.evaluate(() => (window as unknown as TestProviderWindow).walletEvents)).toEqual([
      { event: 'chainChanged', payload: '0x66eee' },
    ])
    expect(await requestError(page, 'wallet_switchEthereumChain', [{ chainId: '0x123456' }])).toMatchObject({ code: 4902 })
    expect(await requestError(page, 'wallet_switchEthereumChain', [{ chainId: 421614 }])).toMatchObject({ code: -32602 })
    expect(await request(page, 'eth_chainId')).toBe('0x66eee')
  })

  test('emits real account/disconnect/reconnect changes and blocks disconnected signing', async ({ page, mockConnectedWallet }) => {
    const wallet = await mockConnectedWallet()
    await page.evaluate(() => {
      const target = window as unknown as TestProviderWindow
      target.walletEvents = []
      for (const event of ['accountsChanged', 'disconnect', 'connect']) {
        target.ethereum.on(event, payload => target.walletEvents.push({ event, payload }))
      }
    })
    await wallet.setAccount(1)
    expect(await request(page, 'eth_accounts')).toEqual([TEST_WALLET_ADDRESSES[1]])
    await wallet.disconnect()
    expect(await request(page, 'eth_accounts')).toEqual([])
    expect(await requestError(page, 'personal_sign', ['0x00', TEST_WALLET_ADDRESSES[1]])).toMatchObject({ code: 4100 })
    expect(await request(page, 'eth_requestAccounts')).toEqual([TEST_WALLET_ADDRESSES[1]])
    expect(await page.evaluate(() => (window as unknown as TestProviderWindow).walletEvents)).toEqual([
      { event: 'accountsChanged', payload: [TEST_WALLET_ADDRESSES[1]] },
      { event: 'accountsChanged', payload: [] },
      { event: 'disconnect', payload: { code: 4900, message: 'Test wallet disconnected' } },
      { event: 'connect', payload: { chainId: '0xaa36a7' } },
      { event: 'accountsChanged', payload: [TEST_WALLET_ADDRESSES[1]] },
    ])
  })

  test('can reject a request once and never claims to submit a transaction', async ({ page, mockConnectedWallet }) => {
    const wallet = await mockConnectedWallet()
    await wallet.rejectNextRequest('personal_sign')
    expect(await requestError(page, 'personal_sign', ['0x00', TEST_WALLET_ADDRESSES[0]])).toMatchObject({ code: 4001 })
    expect(await request<Hex>(page, 'personal_sign', ['0x00', TEST_WALLET_ADDRESSES[0]])).toMatch(/^0x[0-9a-f]{130}$/)
    expect(await requestError(page, 'eth_sendTransaction', [{ from: TEST_WALLET_ADDRESSES[0] }])).toMatchObject({ code: 4200 })
    expect(await requestError(page, 'eth_signTypedData_v4', [TEST_WALLET_ADDRESSES[0], '{invalid'])).toMatchObject({ code: -32602 })
  })
})
