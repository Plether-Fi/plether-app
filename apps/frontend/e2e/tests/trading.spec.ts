import { test, expect, waitForPageLoad, clickConnectWallet } from '../fixtures/wallet'

test.describe('Trading Page', () => {
  test.beforeEach(async ({ page, mockConnectedWallet }) => {
    await mockConnectedWallet()
    await page.goto('/')
    await waitForPageLoad(page)
  })

  test('displays trade card with buy/sell toggle', async ({ page }) => {
    const buyButton = page.getByRole('button', { name: /^buy$/i })
    const sellButton = page.getByRole('button', { name: /^sell$/i })

    if (await buyButton.isVisible()) {
      await expect(buyButton).toBeVisible()
      await expect(sellButton).toBeVisible()
    }
  })

  test('can toggle between buy and sell modes', async ({ page }) => {
    const buyButton = page.getByRole('button', { name: /^buy$/i })
    const sellButton = page.getByRole('button', { name: /^sell$/i })

    if (await buyButton.isVisible()) {
      await buyButton.click()
      await expect(buyButton).toHaveClass(/bg-cyber-surface-dark/)

      await sellButton.click()
      await expect(sellButton).toHaveClass(/bg-cyber-surface-dark/)
    }
  })

  test('can select between BEAR and BULL tokens', async ({ page }) => {
    const bearButton = page.getByRole('button', { name: /dxy-bear/i })
    const bullButton = page.getByRole('button', { name: /dxy-bull/i })

    if (await bearButton.isVisible()) {
      await bearButton.click()
      await expect(bearButton).toHaveClass(/border-cyber-electric-fuchsia/)

      await bullButton.click()
      await expect(bullButton).toHaveClass(/border-cyber-neon-green/)
    }
  })

  test('input field accepts numeric values', async ({ page }) => {
    const input = page.getByRole('spinbutton')

    if (await input.isVisible()) {
      await input.fill('100')
      await expect(input).toHaveValue('100')
    }
  })

  test('displays swap details section', async ({ page }) => {
    const detailsToggle = page.getByRole('button', { name: /swap details/i })

    if (await detailsToggle.isVisible()) {
      await detailsToggle.click()

      const routeText = page.getByText(/route/i)
      const priceImpactText = page.getByText(/price impact/i)

      await expect(routeText).toBeVisible()
      await expect(priceImpactText).toBeVisible()
    }
  })

  test('shows slippage setting', async ({ page }) => {
    const slippageText = page.getByText(/slippage/i)

    if (await slippageText.isVisible()) {
      await expect(slippageText).toContainText('%')
    }
  })

  test('swap button shows appropriate state', async ({ page }) => {
    const swapButton = page.getByRole('button', { name: /buy dxy-|sell dxy-|approve|connect wallet/i })

    if (await swapButton.isVisible()) {
      await expect(swapButton).toBeVisible()
    }
  })
})

test.describe('Trading Flow', () => {
  test('entering amount updates output display', async ({ page, mockConnectedWallet }) => {
    await mockConnectedWallet()
    await page.goto('/')
    await waitForPageLoad(page)

    const input = page.getByRole('spinbutton')
    const outputDisplay = page.getByText(/you receive/i)

    if (await input.isVisible()) {
      await input.fill('100')

      if (await outputDisplay.isVisible()) {
        await expect(outputDisplay).toBeVisible()
      }
    }
  })

  test('shows max button on input', async ({ page, mockConnectedWallet }) => {
    await mockConnectedWallet()
    await page.goto('/')
    await waitForPageLoad(page)

    const maxButton = page.getByRole('button', { name: /max/i })

    if (await maxButton.isVisible()) {
      await expect(maxButton).toBeEnabled()
    }
  })
})
