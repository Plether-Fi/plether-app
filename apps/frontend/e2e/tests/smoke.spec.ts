import { test, expect, waitForPageLoad } from '../fixtures/wallet'
import { readFileSync } from 'node:fs'

const release = JSON.parse(readFileSync(new URL('../../../../config/perps/mock-usdg-prepared.json', import.meta.url), 'utf8'))

const perpsHeading = release.release.phase === 'prepared'
  ? `${release.settlementToken.symbol} perps are being prepared`
  : 'plDXY Perp'

test.describe('Application smoke', () => {
  test('homepage renders the configured perps release state', async ({ page }) => {
    await page.goto('/')
    await waitForPageLoad(page)
    await expect(page).toHaveTitle(/plether/i)
    await expect(page.getByRole('heading', { level: 1, name: perpsHeading, exact: true })).toBeVisible()
  })

  test('displays the real primary navigation links', async ({ page }) => {
    await page.goto('/spot')
    await waitForPageLoad(page)
    const banner = page.getByRole('banner')
    await expect(banner.getByRole('link', { name: 'Plether', exact: true })).toBeVisible()
    await expect(banner.getByRole('link', { name: 'Perps', exact: true })).toHaveAttribute('href', '/')
    await expect(banner.getByRole('link', { name: 'Vaults', exact: true })).toHaveAttribute('href', '/vaults')
    await expect(banner.getByRole('link', { name: 'Leaderboard', exact: true })).toHaveAttribute('href', 'https://insights.plether.com')
  })

  test('the connected spot route exposes its required trade controls', async ({ page, mockConnectedWallet }) => {
    await page.goto('/spot')
    await mockConnectedWallet()
    await expect(page.getByRole('heading', { name: 'Dashboard', exact: true })).toBeVisible()
    await expect(page.getByRole('button', { name: 'Buy', exact: true })).toBeEnabled()
    await expect(page.getByLabel('You pay', { exact: true })).toBeEditable()
  })

  test('shows wallet connection prompts when disconnected', async ({ page }) => {
    await page.goto('/spot')
    await waitForPageLoad(page)
    await expect(page.getByRole('banner').getByRole('button', { name: 'Connect Wallet', exact: true })).toBeVisible()
    await expect(page.getByRole('button', { name: 'Connect Your Wallet', exact: true })).toBeVisible()
    await expect(page.getByLabel('You pay', { exact: true })).toHaveCount(0)
  })

  test('displays a numeric quote and price impact from market data', async ({ page, mockConnectedWallet }) => {
    await page.goto('/spot')
    await mockConnectedWallet()
    await page.getByLabel('You pay', { exact: true }).fill('100')
    await expect(page.getByText('200 plDXY-BULL', { exact: true })).toBeVisible()
    await page.getByRole('button', { name: /Swap details/ }).click()
    await expect(page.getByText('Price Impact', { exact: true }).locator('..')).toHaveText('Price Impact0.50%')
  })

  test('offers the required perps network switch for a connected mainnet wallet', async ({ page, mockConnectedWallet }) => {
    await page.goto('/spot')
    await mockConnectedWallet()
    await page.getByRole('banner').getByRole('link', { name: 'Perps', exact: true }).click()
    await expect(page).toHaveURL(/\/$/)
    await expect(page.getByRole('button', { name: 'Switch wallet network to Arbitrum Sepolia', exact: true })).toBeEnabled()
  })
})

test('dashboard controls navigate between spot, leverage and lending routes', async ({ page, mockConnectedWallet }) => {
  await page.goto('/spot')
  await mockConnectedWallet()
  await page.getByRole('button', { name: /Leverage.*Margin trading/ }).click()
  await expect(page).toHaveURL(/\/leverage$/)
  await expect(page.getByLabel('Collateral (USDC)', { exact: true })).toBeVisible()
  await page.getByRole('button', { name: /Lending.*Supply and borrow USDC/ }).click()
  await expect(page).toHaveURL(/\/lending$/)
  await expect(page.getByRole('heading', { name: 'BEAR Market', exact: true })).toBeVisible()
  await expect(page.getByRole('heading', { name: 'BULL Market', exact: true })).toBeVisible()
  await page.getByRole('button', { name: /Dollar Hedge.*Spot trading/ }).click()
  await expect(page).toHaveURL(/\/spot$/)
  await expect(page.getByLabel('You pay', { exact: true })).toBeEditable()
})
