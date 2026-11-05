import { test as base, expect, type Page } from '@playwright/test'
import { installSpotApi, SPOT_WALLET_ADDRESS } from './spotApi'

export interface WalletFixtures {
  /** Connect the injected test provider through the real wagmi action after navigation.
   * This establishes account state; it does not test the third-party wallet chooser.
   */
  mockConnectedWallet: () => Promise<void>
  spotEnvironment: void
}

export const test = base.extend<WalletFixtures>({
  spotEnvironment: [async ({ page }, use) => {
    // Keep these UI tests deterministic and offline. Requests to the app itself
    // still load its real source; only external services and HTTP API data are isolated.
    await page.route(/^https?:\/\/(?!127\.0\.0\.1(?::|\/))/, route => route.abort())
    await installSpotApi(page)
    await page.addInitScript((address) => {
      localStorage.clear()
      localStorage.setItem('plether_settings', JSON.stringify({
        state: { slippage: 0.5, maxPriceImpact: 2, riskAccepted: true, sepoliaWelcomeDismissed: true }, version: 0,
      }))
      let authorized = false
      let chainId = '0x1'
      const listeners = new Map<string, Set<(value: unknown) => void>>()
      Object.defineProperty(window, 'ethereum', {
        configurable: true,
        value: {
          isMetaMask: true,
          request: async ({ method, params }: { method: string; params?: { chainId?: string }[] }) => {
            switch (method) {
              case 'eth_requestAccounts': authorized = true; return [address]
              case 'eth_accounts': return authorized ? [address] : []
              case 'eth_chainId': return chainId
              case 'net_version': return String(Number(chainId))
              case 'wallet_getPermissions': return authorized ? [{ parentCapability: 'eth_accounts' }] : []
              case 'wallet_requestPermissions': authorized = true; return [{ parentCapability: 'eth_accounts' }]
              case 'wallet_switchEthereumChain':
                if (!params?.[0]?.chainId) throw new Error('Missing requested chain')
                chainId = params[0].chainId
                listeners.get('chainChanged')?.forEach(listener => listener(chainId))
                return null
              default: throw new Error(`Unsupported E2E wallet method: ${method}`)
            }
          },
          on: (event: string, listener: (value: unknown) => void) => {
            const callbacks = listeners.get(event) ?? new Set()
            callbacks.add(listener)
            listeners.set(event, callbacks)
          },
          removeListener: (event: string, listener: (value: unknown) => void) => listeners.get(event)?.delete(listener),
        },
      })
    }, SPOT_WALLET_ADDRESS)
    await use()
  }, { auto: true }],
  mockConnectedWallet: async ({ page }, use) => {
    await use(async () => {
      await page.evaluate(async () => {
        const configPath = '/src/config/wagmi.ts'
        const actionPath = '/node_modules/@wagmi/core/dist/esm/actions/connect.js'
        const injectedPath = '/node_modules/@wagmi/core/dist/esm/connectors/injected.js'
        const [{ config }, { connect }, { injected }] = await Promise.all([
          import(configPath), import(actionPath), import(injectedPath),
        ])
        const connector = config._internal.connectors.setup(injected())
        await connect(config, { connector, chainId: 1 })
      })
      await expect(page.getByRole('button', { name: 'Open wallet account 0x1234...7890', exact: true })).toBeVisible()
    })
  },
})

export { expect }

export async function waitForPageLoad(page: Page) {
  await expect(page.getByRole('main')).toBeVisible()
  await expect(page.getByRole('main').getByRole('heading', { level: 1 })).toBeVisible()
}

export async function clickConnectWallet(page: Page) {
  const connectButton = page.getByRole('button', { name: 'Connect Wallet', exact: true })
  await expect(connectButton).toBeVisible()
  await connectButton.click()
}
