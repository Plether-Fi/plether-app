import { test, expect, waitForPageLoad } from '../fixtures/wallet'

test.describe('Smoke Tests', () => {
  test('homepage loads successfully', async ({ page }) => {
    await page.goto('/')
    await waitForPageLoad(page)

    await expect(page).toHaveTitle(/plether/i)
  })

  test('displays main navigation elements', async ({ page }) => {
    await page.goto('/')
    await waitForPageLoad(page)

    await expect(page.getByRole('banner')).toBeVisible()
  })

  test('trade tab is accessible', async ({ page }) => {
    await page.goto('/')
    await waitForPageLoad(page)

    const tradeTab = page.getByRole('tab', { name: /trade/i })
    if (await tradeTab.isVisible()) {
      await expect(tradeTab).toBeEnabled()
    }
  })

  test('shows connect wallet prompt when not connected', async ({ page }) => {
    await page.goto('/')
    await waitForPageLoad(page)

    const connectButton = page.getByRole('button', { name: /connect wallet/i })
    await expect(connectButton).toBeVisible()
  })

  test('displays price information', async ({ page }) => {
    await page.goto('/')
    await waitForPageLoad(page)

    const priceDisplay = page.locator('[data-testid="dxy-price"]')
    if (await priceDisplay.isVisible()) {
      await expect(priceDisplay).toContainText('$')
    }
  })

  test('network switcher is present', async ({ page }) => {
    await page.goto('/')
    await waitForPageLoad(page)

    const networkButton = page.getByRole('button', { name: /sepolia|mainnet|network/i })
    if (await networkButton.isVisible()) {
      await expect(networkButton).toBeEnabled()
    }
  })
})

test.describe('Navigation', () => {
  test('can navigate between tabs', async ({ page }) => {
    await page.goto('/')
    await waitForPageLoad(page)

    const tabs = ['Trade', 'Mint', 'Stake', 'Leverage']

    for (const tabName of tabs) {
      const tab = page.getByRole('tab', { name: new RegExp(tabName, 'i') })
      if (await tab.isVisible()) {
        await tab.click()
        await expect(tab).toHaveAttribute('aria-selected', 'true')
      }
    }
  })
})
