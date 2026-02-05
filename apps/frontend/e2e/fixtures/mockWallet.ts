/**
 * Mock wallet injection for Playwright MCP testing.
 *
 * Usage with Playwright MCP:
 *
 * 1. First, inject the mock wallet (run ONCE per browser session):
 *    mcp__playwright__browser_run_code({ code: MOCK_WALLET_INIT_SCRIPT })
 *
 * 2. Then navigate to your page (wallet will auto-connect):
 *    mcp__playwright__browser_navigate({ url: "http://localhost:5173" })
 *
 * The mock provides:
 * - Auto-connection via EIP-6963 wallet discovery
 * - Connected wallet state with Anvil test address
 * - Sepolia network (chainId: 11155111)
 * - Basic RPC method responses
 */

import { test as base, expect, type Page } from '@playwright/test'

export const MOCK_ADDRESS = '0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266' // Anvil account 0
export const MOCK_CHAIN_ID = 11155111 // Sepolia
export const MOCK_CHAIN_ID_HEX = '0xaa36a7'

export interface WalletFixtures {
  mockConnectedWallet: () => Promise<void>
}

/**
 * Playwright MCP init script - copy this into browser_run_code.
 * Must be run BEFORE navigating to the page.
 */
/**
 * Anvil-proxied mock wallet - proxies contract reads to local Anvil node.
 * Use this when you need real contract state (balances, allowances, etc.)
 */
export const MOCK_WALLET_ANVIL_SCRIPT = `async (page) => {
  await page.addInitScript(() => {
    const MOCK_ADDRESS = '0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266';
    const CHAIN_ID = '0x7a69'; // 31337 in hex (Anvil)
    const ANVIL_RPC = 'http://127.0.0.1:8545';

    async function proxyToAnvil(method, params) {
      const res = await fetch(ANVIL_RPC, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ jsonrpc: '2.0', id: 1, method, params }),
      });
      const data = await res.json();
      if (data.error) throw new Error(data.error.message);
      return data.result;
    }

    const mockProvider = {
      isMetaMask: true,
      selectedAddress: MOCK_ADDRESS,
      chainId: CHAIN_ID,
      networkVersion: '31337',
      _metamask: { isUnlocked: () => Promise.resolve(true) },
      request: async ({ method, params }) => {
        console.log('[MockWallet]', method, params);
        switch (method) {
          case 'eth_requestAccounts':
          case 'eth_accounts':
            return [MOCK_ADDRESS];
          case 'eth_chainId':
            return CHAIN_ID;
          case 'net_version':
            return '31337';
          case 'wallet_switchEthereumChain':
          case 'wallet_addEthereumChain':
            return null;
          case 'wallet_getPermissions':
          case 'wallet_requestPermissions':
            return [{ parentCapability: 'eth_accounts' }];
          case 'eth_call':
          case 'eth_getBalance':
          case 'eth_estimateGas':
          case 'eth_gasPrice':
          case 'eth_blockNumber':
          case 'eth_getTransactionCount':
          case 'eth_getCode':
          case 'eth_getLogs':
          case 'eth_sendRawTransaction':
          case 'eth_sendTransaction':
          case 'eth_getTransactionReceipt':
            return proxyToAnvil(method, params);
          default:
            console.warn('[MockWallet] Unhandled:', method);
            return null;
        }
      },
      on: (event, cb) => {
        if (event === 'accountsChanged') setTimeout(() => cb([MOCK_ADDRESS]), 100);
        if (event === 'chainChanged') setTimeout(() => cb(CHAIN_ID), 100);
        if (event === 'connect') setTimeout(() => cb({ chainId: CHAIN_ID }), 100);
      },
      removeListener: () => {},
      removeAllListeners: () => {},
    };

    Object.defineProperty(window, 'ethereum', {
      value: mockProvider,
      writable: false,
      configurable: true,
    });

    const announceProvider = () => {
      window.dispatchEvent(new CustomEvent('eip6963:announceProvider', {
        detail: Object.freeze({
          info: {
            uuid: 'mock-wallet-anvil',
            name: 'MetaMask',
            icon: 'data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" width="32" height="32"><rect fill="%23f6851b" width="32" height="32" rx="6"/></svg>',
            rdns: 'io.metamask',
          },
          provider: mockProvider,
        }),
      }));
    };

    window.addEventListener('eip6963:requestProvider', announceProvider);
    announceProvider();
    setTimeout(announceProvider, 100);
    setTimeout(announceProvider, 500);

    console.log('[MockWallet] Anvil-proxied wallet ready');
  });
  return 'Anvil-proxied mock wallet ready - navigate to page now';
}`

export const MOCK_WALLET_INIT_SCRIPT = `async (page) => {
  await page.addInitScript(() => {
    const MOCK_ADDRESS = '0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266';
    const CHAIN_ID = '0xaa36a7';

    const mockProvider = {
      isMetaMask: true,
      selectedAddress: MOCK_ADDRESS,
      chainId: CHAIN_ID,
      networkVersion: '11155111',
      _metamask: { isUnlocked: () => Promise.resolve(true) },
      request: async ({ method, params }) => {
        console.log('[MockWallet]', method, params);
        switch (method) {
          case 'eth_requestAccounts':
          case 'eth_accounts':
            return [MOCK_ADDRESS];
          case 'eth_chainId':
            return CHAIN_ID;
          case 'net_version':
            return '11155111';
          case 'eth_getBalance':
            return '0x8ac7230489e80000'; // 10 ETH
          case 'eth_call':
            return '0x' + '0'.repeat(64);
          case 'wallet_switchEthereumChain':
          case 'wallet_addEthereumChain':
            return null;
          case 'eth_estimateGas':
            return '0x5208';
          case 'eth_gasPrice':
            return '0x3b9aca00';
          case 'eth_blockNumber':
            return '0x1234567';
          case 'wallet_getPermissions':
          case 'wallet_requestPermissions':
            return [{ parentCapability: 'eth_accounts' }];
          default:
            console.warn('[MockWallet] Unhandled:', method);
            return null;
        }
      },
      on: (event, cb) => {
        if (event === 'accountsChanged') setTimeout(() => cb([MOCK_ADDRESS]), 100);
        if (event === 'chainChanged') setTimeout(() => cb(CHAIN_ID), 100);
        if (event === 'connect') setTimeout(() => cb({ chainId: CHAIN_ID }), 100);
      },
      removeListener: () => {},
      removeAllListeners: () => {},
    };

    Object.defineProperty(window, 'ethereum', {
      value: mockProvider,
      writable: false,
      configurable: true,
    });

    // EIP-6963: Announce as MetaMask for auto-detection
    const announceProvider = () => {
      window.dispatchEvent(new CustomEvent('eip6963:announceProvider', {
        detail: Object.freeze({
          info: {
            uuid: 'mock-wallet-12345',
            name: 'MetaMask',
            icon: 'data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" width="32" height="32"><rect fill="%23f6851b" width="32" height="32" rx="6"/></svg>',
            rdns: 'io.metamask',
          },
          provider: mockProvider,
        }),
      }));
    };

    window.addEventListener('eip6963:requestProvider', announceProvider);
    announceProvider();
    setTimeout(announceProvider, 100);
    setTimeout(announceProvider, 500);

    console.log('[MockWallet] Injected with EIP-6963');
  });
  return 'Mock wallet ready - navigate to page now';
}`

/**
 * Playwright test fixture for e2e tests.
 */
export const test = base.extend<WalletFixtures>({
  mockConnectedWallet: async ({ page }, use) => {
    const mockWallet = async () => {
      await page.addInitScript(() => {
        const MOCK_ADDRESS = '0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266'
        const CHAIN_ID = '0xaa36a7'

        const mockProvider = {
          isMetaMask: true,
          selectedAddress: MOCK_ADDRESS,
          chainId: CHAIN_ID,
          networkVersion: '11155111',
          _metamask: { isUnlocked: () => Promise.resolve(true) },
          request: async ({ method }: { method: string; params?: unknown[] }) => {
            switch (method) {
              case 'eth_requestAccounts':
              case 'eth_accounts':
                return [MOCK_ADDRESS]
              case 'eth_chainId':
                return CHAIN_ID
              case 'net_version':
                return '11155111'
              case 'eth_getBalance':
                return '0x8ac7230489e80000'
              case 'eth_call':
                return '0x' + '0'.repeat(64)
              case 'wallet_switchEthereumChain':
              case 'wallet_addEthereumChain':
                return null
              case 'eth_estimateGas':
                return '0x5208'
              case 'eth_gasPrice':
                return '0x3b9aca00'
              case 'eth_blockNumber':
                return '0x1234567'
              case 'wallet_getPermissions':
              case 'wallet_requestPermissions':
                return [{ parentCapability: 'eth_accounts' }]
              default:
                return null
            }
          },
          on: (event: string, cb: (arg: unknown) => void) => {
            if (event === 'accountsChanged') setTimeout(() => cb([MOCK_ADDRESS]), 100)
            if (event === 'chainChanged') setTimeout(() => cb(CHAIN_ID), 100)
            if (event === 'connect') setTimeout(() => cb({ chainId: CHAIN_ID }), 100)
          },
          removeListener: () => {},
          removeAllListeners: () => {},
        }

        Object.defineProperty(window, 'ethereum', {
          value: mockProvider,
          writable: false,
          configurable: true,
        })

        const announceProvider = () => {
          window.dispatchEvent(
            new CustomEvent('eip6963:announceProvider', {
              detail: Object.freeze({
                info: {
                  uuid: 'mock-wallet-12345',
                  name: 'MetaMask',
                  icon: 'data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" width="32" height="32"><rect fill="%23f6851b" width="32" height="32" rx="6"/></svg>',
                  rdns: 'io.metamask',
                },
                provider: mockProvider,
              }),
            })
          )
        }

        window.addEventListener('eip6963:requestProvider', announceProvider)
        announceProvider()
        setTimeout(announceProvider, 100)
        setTimeout(announceProvider, 500)
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
