import { test as base, expect, type Page } from '@playwright/test'

export interface WalletFixtures {
  mockConnectedWallet: () => Promise<void>
}

export const test = base.extend<WalletFixtures>({
  mockConnectedWallet: async ({ page }, use) => {
    const mockWallet = async () => {
      await page.addInitScript(() => {
        const MOCK_ADDRESS = '0x1234567890123456789012345678901234567890'

        Object.defineProperty(window, 'ethereum', {
          value: {
            isMetaMask: true,
            selectedAddress: MOCK_ADDRESS,
            chainId: '0xaa36a7',
            networkVersion: '11155111',
            request: async ({ method }: { method: string; params?: unknown[] }) => {
              switch (method) {
                case 'eth_requestAccounts':
                case 'eth_accounts':
                  return [MOCK_ADDRESS]
                case 'eth_chainId':
                  return '0xaa36a7'
                case 'net_version':
                  return '11155111'
                case 'wallet_switchEthereumChain':
                  return null
                default:
                  console.warn(`Unhandled method: ${method}`)
                  return null
              }
            },
            on: () => {},
            removeListener: () => {},
          },
          writable: true,
        })
      })
    }

    await use(mockWallet)
  },
})

export { expect }

export async function waitForPageLoad(page: Page) {
  await page.waitForLoadState('networkidle')
}

export async function clickConnectWallet(page: Page) {
  const connectButton = page.getByRole('button', { name: /connect wallet/i })
  if (await connectButton.isVisible()) {
    await connectButton.click()
  }
}
