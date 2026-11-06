import { test, expect, type Page } from '@playwright/test'
import { stringToHex, verifyMessage, type Hex } from 'viem'
import publicConfig from '../fixtures/privy-public-config.json' with { type: 'json' }
import { installTestWallet, TEST_WALLET_ADDRESSES, type TestWalletController } from '../fixtures/signingWallet'

const selectionKey = `plether.privy.${publicConfig.id}.owner`
const staleOwner = '0x1111111111111111111111111111111111111111'
const message = 'Plether wallet integration test; no transaction is submitted.'
const connectButton = (page: Page) => page.getByRole('banner').getByRole('button', { name: 'Connect Wallet', exact: true })
const accountButton = (page: Page) => page.getByRole('banner').getByRole('button', { name: /^Open wallet account / })

async function openApp(page: Page, selection = 'disconnected'): Promise<TestWalletController> {
  const wallet = await installTestWallet(page)
  await page.addInitScript(({ key, initialSelection }) => {
    localStorage.setItem('plether_settings', JSON.stringify({ state: { riskAccepted: true, sepoliaWelcomeDismissed: true }, version: 0 }))
    if (sessionStorage.getItem(key) === null) sessionStorage.setItem(key, initialSelection)
  }, { key: selectionKey, initialSelection: selection })
  // Keep the SDK and application real, but make this suite deterministic and
  // offline. Unexpected external requests fail closed instead of reaching RPC,
  // auth, analytics, or the embedded-wallet service.
  await page.route('**/*', route => {
    const url = new URL(route.request().url())
    if (url.hostname === '127.0.0.1' || url.hostname === 'localhost') {
      if (url.pathname.startsWith('/api/')) return route.fulfill({ status: 503, json: {} })
      return route.continue()
    }
    if (url.hostname === 'auth.privy.io' && url.pathname === `/api/v1/apps/${publicConfig.id}`) {
      return route.fulfill({ json: publicConfig, headers: {
        'access-control-allow-origin': route.request().headers().origin,
        'access-control-allow-credentials': 'true',
      } })
    }
    if (url.hostname === 'explorer-api.walletconnect.com' && url.pathname === '/v3/wallets') {
      return route.fulfill({ json: { listings: {}, total: 0 }, headers: { 'access-control-allow-origin': '*' } })
    }
    if (url.hostname === 'auth.privy.io' && url.pathname.endsWith('/embedded-wallets')) {
      // Readiness is the only iframe interaction these external-wallet tests
      // need. Fail other calls explicitly rather than fabricating wallet keys,
      // authentication, signatures, or a successfully created embedded wallet.
      return route.fulfill({ contentType: 'text/html', body: `<!doctype html><html><body><script>
        addEventListener('message', event => {
          const input = event.data;
          if (!input || typeof input.event !== 'string' || !input.event.startsWith('privy:')) return;
          const output = { id: input.id, event: input.event };
          if (input.event === 'privy:iframe:ready' || input.event === 'privy:mfa:clear') output.data = {};
          else output.error = { type: 'unsupported_test_operation', message: 'Embedded wallet operations are not implemented in this external-wallet fixture' };
          event.source.postMessage(output, event.origin);
        });
      </script></body></html>` })
    }
    return route.abort()
  })
  await page.routeWebSocket(/^wss:/, socket => socket.close())
  await page.goto('/privacy')
  await expect(connectButton(page)).toBeEnabled()
  return wallet
}

async function openPicker(page: Page) {
  await connectButton(page).click()
  await expect(page.getByRole('button', { name: 'Log in with Email', exact: true })).toBeVisible()
}

async function approveConnection(page: Page) {
  await openPicker(page)
  await page.locator('#privy-dialog').getByRole('button', { name: 'Plether Test Wallet', exact: true }).click()
  await expect(accountButton(page)).toBeVisible()
  await expect(connectButton(page)).not.toBeVisible()
}

async function selectedOwner(page: Page) {
  return page.evaluate(key => sessionStorage.getItem(key), selectionKey)
}

async function signThroughBridge(page: Page) {
  const result = await page.evaluate(async ({ modulePath, data }) => {
    const { config } = await import(modulePath)
    const connection = config.state.connections.get(config.state.current)
    if (!connection) throw new Error('No active Wagmi connection')
    const provider = await connection.connector.getProvider()
    const [address] = await provider.request({ method: 'eth_accounts' })
    const signature = await provider.request({ method: 'personal_sign', params: [data, address] })
    return { address: address as Hex, signature: signature as Hex }
  }, { modulePath: '/src/config/wagmi.ts', data: stringToHex(message) })
  expect(await verifyMessage({ ...result, message })).toBe(true)
  return result.address
}

test('external approval replaces an unavailable saved owner and signs through the real bridge', async ({ page }) => {
  const wallet = await openApp(page, staleOwner)
  await wallet.disconnect()
  await approveConnection(page)
  expect(await selectedOwner(page)).toBe(TEST_WALLET_ADDRESSES[0].toLowerCase())
  expect(await signThroughBridge(page)).toBe(TEST_WALLET_ADDRESSES[0])
})

test('disconnect, reconnect, and returning sessions keep the actual selected signer', async ({ page }) => {
  await openApp(page)
  await approveConnection(page)
  await page.getByRole('button', { name: 'Disconnect wallet', exact: true }).click()
  await expect(connectButton(page)).toBeVisible()
  expect(await selectedOwner(page)).toBe('disconnected')
  await approveConnection(page)
  expect(await signThroughBridge(page)).toBe(TEST_WALLET_ADDRESSES[0])
  await page.reload()
  await expect(accountButton(page)).toBeVisible()
  expect(await signThroughBridge(page)).toBe(TEST_WALLET_ADDRESSES[0])
})

test('a rejected wallet request leaves the app disconnected and can be retried', async ({ page }) => {
  const wallet = await openApp(page)
  await wallet.disconnect()
  await wallet.rejectNextRequest('eth_requestAccounts')
  await openPicker(page)
  await page.locator('#privy-dialog').getByRole('button', { name: 'Plether Test Wallet', exact: true }).click()
  await expect(page.locator('#privy-dialog').getByRole('button', { name: 'Retry', exact: true })).toBeVisible()
  await page.locator('#privy-dialog').getByRole('button', { name: 'close modal', exact: true }).click()
  await expect(connectButton(page)).toBeVisible()
  expect(await selectedOwner(page)).toBe('disconnected')
  await approveConnection(page)
})

for (const dismissal of ['close', 'escape', 'backdrop'] as const) {
  test(`canceling email with ${dismissal} does not select an already available wallet`, async ({ page }) => {
    await openApp(page)
    await openPicker(page)
    await page.getByRole('button', { name: 'Log in with Email', exact: true }).click()
    await expect(page.getByPlaceholder('your@email.com')).toBeVisible()
    if (dismissal === 'close') await page.locator('#privy-dialog').getByRole('button', { name: 'close modal', exact: true }).click()
    else if (dismissal === 'escape') await page.keyboard.press('Escape')
    else await page.mouse.click(5, 5)
    await expect(page.getByPlaceholder('your@email.com')).not.toBeVisible()
    await expect(connectButton(page)).toBeVisible()
    expect(await selectedOwner(page)).toBe('disconnected')
    await expect(page.getByRole('banner').getByRole('alert')).not.toBeVisible()
    await approveConnection(page)
    expect(await signThroughBridge(page)).toBe(TEST_WALLET_ADDRESSES[0])
  })
}

test('an extension account change cannot sign as the saved owner and reconnect selects the new account', async ({ page }) => {
  const wallet = await openApp(page)
  await approveConnection(page)
  await wallet.setAccount(1)
  await expect(connectButton(page)).toBeVisible()
  expect(await selectedOwner(page)).toBe(TEST_WALLET_ADDRESSES[0].toLowerCase())
  await approveConnection(page)
  expect(await selectedOwner(page)).toBe(TEST_WALLET_ADDRESSES[1].toLowerCase())
  expect(await signThroughBridge(page)).toBe(TEST_WALLET_ADDRESSES[1])
})
