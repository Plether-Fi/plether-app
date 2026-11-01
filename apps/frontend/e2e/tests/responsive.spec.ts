import type { Locator, Page } from '@playwright/test'
import { readFileSync } from 'node:fs'
import { expect, test } from '../fixtures/wallet'

const release = JSON.parse(readFileSync(new URL('../../../../config/perps/arbitrum-sepolia-v2.0.0.json', import.meta.url), 'utf8'))
const isPreparedRelease = release.release.phase === 'prepared'
const perpsHeading = isPreparedRelease
  ? `${release.settlementToken.symbol} perps are being prepared`
  : 'plDXY Perp'

const viewports = [
  { name: 'compact mobile', width: 320, height: 568 },
  { name: 'small mobile', width: 360, height: 800 },
  { name: 'tablet', width: 768, height: 1024 },
  { name: 'small desktop', width: 1024, height: 768 },
  { name: 'desktop breakpoint', width: 1280, height: 800 },
  { name: 'large desktop', width: 1440, height: 900 },
] as const

const primaryRoutes = [
  { path: '/', heading: perpsHeading },
  { path: '/spot', heading: 'Dashboard' },
  { path: '/leverage', heading: 'Dashboard' },
  { path: '/lending', heading: 'Dashboard' },
  { path: '/mint', heading: 'Mint & Redeem' },
  { path: '/stake', heading: 'Stake' },
  { path: '/history', heading: 'Transaction History' },
] as const

async function expectWithinViewport(
  page: Page,
  locator: Locator,
  label: string
) {
  await expect(locator, `${label} should be visible`).toBeVisible()
  await expect(locator, `${label} should intersect the viewport`).toBeInViewport()

  const box = await locator.boundingBox()
  const viewport = page.viewportSize()

  expect(box, `${label} should have a bounding box`).not.toBeNull()
  expect(viewport, 'the page should have a configured viewport').not.toBeNull()

  if (!box || !viewport) return

  expect(box.x, `${label} should not extend past the left edge`).toBeGreaterThanOrEqual(-1)
  expect(
    box.x + box.width,
    `${label} should not extend past the right edge`
  ).toBeLessThanOrEqual(viewport.width + 1)
}

async function expectNoDocumentHorizontalOverflow(page: Page) {
  const dimensions = await page.evaluate(() => ({
    clientWidth: document.documentElement.clientWidth,
    scrollWidth: document.documentElement.scrollWidth,
  }))

  expect(
    dimensions.scrollWidth,
    `document scroll width ${dimensions.scrollWidth.toString()}px should fit client width ${dimensions.clientWidth.toString()}px`
  ).toBeLessThanOrEqual(dimensions.clientWidth)
}

async function expectPerpsLayout(page: Page, viewportWidth: number) {
  const sections = page.locator('main section')
  const instrument = sections.filter({
    has: page.getByRole('heading', { name: 'plDXY Perp', exact: true }),
  })
  const tradeTicket = sections.filter({
    has: page.getByRole('button', { name: 'Long plDXY Perp', exact: true }),
  })
  const chart = page.getByRole('region', {
    name: 'Interactive TradingView chart for the plDXY perpetual market', exact: true,
  })
  const account = sections.filter({
    has: page.getByRole('button', { name: 'Open Orders', exact: true }),
  })

  await Promise.all([
    expect(instrument).toHaveCount(1),
    expect(tradeTicket).toHaveCount(1),
    expect(chart).toHaveCount(1),
    expect(account).toHaveCount(1),
  ])

  const [instrumentBox, tradeTicketBox, chartBox, accountBox] = await Promise.all([
    instrument.boundingBox(),
    tradeTicket.boundingBox(),
    chart.boundingBox(),
    account.boundingBox(),
  ])

  expect(
    instrumentBox,
    'instrument panel should have a bounding box'
  ).not.toBeNull()
  expect(
    tradeTicketBox,
    'trade ticket should have a bounding box'
  ).not.toBeNull()
  expect(chartBox, 'chart panel should have a bounding box').not.toBeNull()
  expect(accountBox, 'account panel should have a bounding box').not.toBeNull()

  if (!instrumentBox || !tradeTicketBox || !chartBox || !accountBox) return

  const instrumentMetrics = instrument.locator('[data-perps-instrument-stat]')
  await expect(instrumentMetrics).toHaveCount(6)

  if (viewportWidth < 640) {
    const metricsBox = await instrument.locator('[data-perps-instrument-metrics]').boundingBox()
    expect(metricsBox).not.toBeNull()
    if (!metricsBox) return
    // The deployed instrument uses two columns below 22rem of available
    // panel width, then three columns. Both layouts must keep rows aligned.
    const columns = metricsBox.width >= 352 ? 3 : 2
    const boxes = await Promise.all(
      [0, 1, 2, 3].map(index => instrumentMetrics.nth(index).boundingBox())
    )
    for (const box of boxes) expect(box).not.toBeNull()
    const first = boxes[0]
    const lastInRow = boxes[columns - 1]
    const nextRow = boxes[columns]
    if (first && lastInRow && nextRow) {
      expect(Math.abs(first.y - lastInRow.y)).toBeLessThanOrEqual(1)
      expect(lastInRow.x).toBeGreaterThan(first.x)
      expect(nextRow.y).toBeGreaterThan(first.y)
      expect(Math.abs(nextRow.x - first.x)).toBeLessThanOrEqual(1)
    }
  }

  if (viewportWidth < 1280) {
    expect(instrumentBox.y + instrumentBox.height).toBeLessThanOrEqual(
      tradeTicketBox.y
    )
    expect(tradeTicketBox.y + tradeTicketBox.height).toBeLessThanOrEqual(
      chartBox.y
    )
    expect(chartBox.y + chartBox.height).toBeLessThanOrEqual(accountBox.y)
    return
  }

  expect(Math.abs(instrumentBox.x - chartBox.x)).toBeLessThanOrEqual(1)
  expect(Math.abs(chartBox.x - accountBox.x)).toBeLessThanOrEqual(1)
  expect(Math.abs(instrumentBox.width - chartBox.width)).toBeLessThanOrEqual(1)
  expect(Math.abs(chartBox.width - accountBox.width)).toBeLessThanOrEqual(1)
  expect(tradeTicketBox.x).toBeGreaterThan(instrumentBox.x + instrumentBox.width)

  const instrumentToChartGap =
    chartBox.y - (instrumentBox.y + instrumentBox.height)
  const chartToAccountGap = accountBox.y - (chartBox.y + chartBox.height)

  expect(Math.abs(instrumentToChartGap - 24)).toBeLessThanOrEqual(2)
  expect(Math.abs(chartToAccountGap - 24)).toBeLessThanOrEqual(2)
}

async function expectConfiguredPerpsLayout(page: Page, viewportWidth: number) {
  if (!isPreparedRelease) {
    await expectPerpsLayout(page, viewportWidth)
    return
  }

  // A prepared deployment intentionally has no live ticket or chart. Require
  // its explicit release state instead of silently accepting missing panels.
  const main = page.getByRole('main')
  const pending = main.getByRole('status')
  await expectWithinViewport(page, pending, 'prepared perps status')
  await expect(pending.getByRole('heading', { name: perpsHeading, exact: true })).toBeVisible()
  await expect(pending).toContainText('Trading, testnet funding, and vault deposits will be available after deployment.')
  await expect(main.getByRole('button', { name: 'Long plDXY Perp', exact: true })).toHaveCount(0)
  await expect(main.getByRole('button', { name: 'Short plDXY Perp', exact: true })).toHaveCount(0)
  await expect(main.getByRole('button', { name: 'Open Orders', exact: true })).toHaveCount(0)
}

for (const { name, width, height } of viewports) {
  test.describe(`${name} (${width.toString()}x${height.toString()})`, () => {
    test.use({ viewport: { width, height } })

    test('keeps the application shell usable without page-level horizontal overflow', async ({
      page,
    }) => {
      await page.goto('/', { waitUntil: 'domcontentloaded' })

      const banner = page.getByRole('banner')
      const logo = page.getByRole('link', { name: 'Plether', exact: true })
      const connectWallet = banner.getByRole('button', { name: /connect wallet/i })
      const main = page.getByRole('main')

      await expectWithinViewport(page, banner, 'header')
      await expectWithinViewport(page, logo, 'Plether logo')
      await expectWithinViewport(page, connectWallet, 'Connect Wallet button')
      await expectWithinViewport(page, main, 'main content')
      await expect(page.getByRole('heading', { name: perpsHeading, exact: true })).toBeVisible()

      await expectNoDocumentHorizontalOverflow(page)
      await expectConfiguredPerpsLayout(page, width)
    })

    test('keeps primary application routes free of horizontal overflow', async ({
      page,
    }) => {
      for (const route of primaryRoutes) {
        await page.goto(route.path, { waitUntil: 'domcontentloaded' })
        await expect(
          page.getByRole('heading', { name: route.heading, exact: true }),
          `${route.path} should finish rendering`
        ).toBeVisible()
        await expectNoDocumentHorizontalOverflow(page)
      }
    })
  })
}
