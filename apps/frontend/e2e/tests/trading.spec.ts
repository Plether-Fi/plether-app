import { test, expect, waitForPageLoad } from '../fixtures/wallet'

// These cover the real spot UI and API-client/query integration over a controlled
// HTTP backend and injected wallet. They do not claim an on-chain swap succeeds.
test.beforeEach(async ({ page, mockConnectedWallet }) => {
  await page.goto('/spot')
  await waitForPageLoad(page)
  await mockConnectedWallet()
  await expect(page.getByLabel('You pay', { exact: true })).toBeVisible()
})

test.describe('Trading Page', () => {
  test('displays the required buy/sell controls and trade input', async ({ page }) => {
    await expect(page.getByRole('button', { name: 'Buy', exact: true })).toBeVisible()
    await expect(page.getByRole('button', { name: 'Sell', exact: true })).toBeVisible()
    await expect(page.getByLabel('You pay', { exact: true })).toBeEditable()
  })

  test('buy and sell modes change the input asset, label, and submission action', async ({ page }) => {
    await page.getByRole('button', { name: 'Sell', exact: true }).click()
    await expect(page.getByLabel('You sell', { exact: true })).toBeVisible()
    await expect(page.getByRole('button', { name: 'Sell plDXY-BULL', exact: true })).toBeDisabled()
    await expect(page.getByLabel('You pay', { exact: true })).toHaveCount(0)

    await page.getByRole('button', { name: 'Buy', exact: true }).click()
    await expect(page.getByLabel('You pay', { exact: true })).toBeVisible()
    await expect(page.getByRole('button', { name: 'Buy plDXY-BULL', exact: true })).toBeDisabled()
  })

  test('selecting BEAR and BULL changes the quoted asset and action', async ({ page }) => {
    await page.getByRole('button', { name: /plDXY-BEAR Bearish on USD/ }).click()
    await expect(page.getByRole('button', { name: 'Buy plDXY-BEAR', exact: true })).toBeDisabled()
    await page.getByLabel('You pay', { exact: true }).fill('100')
    await expect(page.getByText('200 plDXY-BEAR', { exact: true })).toBeVisible()

    await page.getByRole('button', { name: /plDXY-BULL Bullish on USD/ }).click()
    await expect(page.getByText('200 plDXY-BULL', { exact: true })).toBeVisible()
    await expect(page.getByRole('button', { name: 'Approve & Buy', exact: true })).toBeEnabled()
  })

  test('the numeric text input accepts a decimal amount', async ({ page }) => {
    const input = page.getByLabel('You pay', { exact: true })
    await input.fill('100.25')
    await expect(input).toHaveValue('100.25')
    await expect(page.getByText('200.5 plDXY-BULL', { exact: true })).toBeVisible()
  })

  test('swap details expose the selected route and numeric price impact', async ({ page }) => {
    await page.getByLabel('You pay', { exact: true }).fill('100')
    await expect(page.getByText('200 plDXY-BULL', { exact: true })).toBeVisible()
    await page.getByRole('button', { name: 'Swap details', exact: false }).click()
    await expect(page.getByText('USDC → ZapRouter → plDXY-BULL', { exact: true })).toBeVisible()
    await expect(page.getByText('Price Impact', { exact: true }).locator('..')).toHaveText('Price Impact0.50%')
  })

  test('shows the configured slippage value', async ({ page }) => {
    await expect(page.getByText('0.5% slippage', { exact: true })).toBeVisible()
  })

  test('requires a positive affordable amount and identifies approval before submission', async ({ page }) => {
    const input = page.getByLabel('You pay', { exact: true })
    await expect(page.getByRole('button', { name: 'Buy plDXY-BULL', exact: true })).toBeDisabled()
    await input.fill('0')
    await expect(page.getByRole('button', { name: 'Buy plDXY-BULL', exact: true })).toBeDisabled()
    await input.fill('100')
    await expect(page.getByText('200 plDXY-BULL', { exact: true })).toBeVisible()
    await expect(page.getByRole('button', { name: 'Approve & Buy', exact: true })).toBeEnabled()
    await input.fill('1001')
    await expect(page.getByRole('button', { name: 'Insufficient USDC', exact: true })).toBeDisabled()
    await input.clear()
    await expect(page.getByRole('button', { name: 'Buy plDXY-BULL', exact: true })).toBeDisabled()
  })
})

test.describe('Trading quote flow', () => {
  test('changing the input updates the numeric quoted output through the HTTP API', async ({ page }) => {
    const input = page.getByLabel('You pay', { exact: true })
    await expect(page.getByText('0 plDXY-BULL', { exact: true })).toBeVisible()
    const firstRequest = page.waitForRequest(request => {
      const url = new URL(request.url())
      return url.pathname.endsWith('/quotes/zap') && url.searchParams.get('amount') === '100000000'
    })
    await input.fill('100')
    await firstRequest
    await expect(page.getByText('200 plDXY-BULL', { exact: true })).toBeVisible()
    await input.fill('25')
    await expect(page.getByText('50 plDXY-BULL', { exact: true })).toBeVisible()
    await expect(page.getByText('200 plDXY-BULL', { exact: true })).toHaveCount(0)
  })

  test('MAX fills the loaded USDC balance and refreshes the quote', async ({ page }) => {
    await page.getByRole('button', { name: 'MAX', exact: true }).click()
    await expect(page.getByLabel('You pay', { exact: true })).toHaveValue('1000')
    await expect(page.getByText('2,000 plDXY-BULL', { exact: true })).toBeVisible()
    await expect(page.getByRole('button', { name: 'Approve & Buy', exact: true })).toBeEnabled()
  })
})
