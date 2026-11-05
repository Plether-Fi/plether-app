import { test, expect } from '../fixtures/wallet'

test('missing Privy configuration keeps public pages readable and wallet actions unavailable', async ({ page, baseURL, mockConnectedWallet }, testInfo) => {
  test.skip(testInfo.project.name !== 'privy-unconfigured', 'Run with e2e/privy.playwright.config.ts to force an empty Privy app ID')
  const errors: string[] = []
  const remoteWalletRequests: string[] = []
  page.on('pageerror', error => errors.push(error.message))
  await page.route('**/*', route => {
    const url = new URL(route.request().url())
    if (url.origin === baseURL && url.pathname.startsWith('/api/')) {
      return route.fulfill({ status: 503, contentType: 'application/json', body: JSON.stringify({ error: 'Backend intentionally unavailable in this smoke test' }) })
    }
    if (url.origin === baseURL) return route.continue()
    if (/(^|\.)(privy\.io|walletconnect\.(org|com)|reown\.com)$/.test(url.hostname)) remoteWalletRequests.push(url.href)
    return route.abort()
  })
  await mockConnectedWallet()
  await page.addInitScript(() => {
    localStorage.setItem('plether_settings', JSON.stringify({
      state: { riskAccepted: true, sepoliaWelcomeDismissed: true },
      version: 0,
    }))
  })

  await page.goto('/privacy')
  await expect(page.getByRole('heading', { name: 'Privacy Policy', exact: true })).toBeVisible()
  const header = page.getByRole('banner')
  await expect(header.getByRole('button', { name: 'Connect Wallet', exact: true })).toBeDisabled()
  await expect(header.getByRole('status')).toHaveText('Wallet login is not configured for this deployment.')

  await header.getByRole('link', { name: 'Perps', exact: true }).click()
  await expect(page).toHaveURL('/')
  await expect(page.getByRole('main')).toBeVisible()
  await expect(page.getByRole('main').getByText('Wallet connection is not configured for this deployment.', { exact: true })).toBeVisible()
  await expect(header.getByRole('button', { name: 'Connect Wallet', exact: true })).toBeDisabled()
  await expect(header.getByRole('status')).toHaveText('Wallet login is not configured for this deployment.')
  await page.screenshot({ path: testInfo.outputPath('unconfigured-home.png'), fullPage: true })
  expect(remoteWalletRequests).toEqual([])
  expect(errors).toEqual([])
})
