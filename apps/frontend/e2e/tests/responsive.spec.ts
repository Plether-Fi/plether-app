import { expect, test, type Locator, type Page } from '@playwright/test'

const viewports = [
  { name: 'compact mobile', width: 320, height: 568 },
  { name: 'small mobile', width: 360, height: 800 },
  { name: 'tablet', width: 768, height: 1024 },
  { name: 'small desktop', width: 1024, height: 768 },
  { name: 'desktop breakpoint', width: 1280, height: 800 },
  { name: 'large desktop', width: 1440, height: 900 },
] as const

const primaryRoutes = [
  { path: '/', heading: 'plDXY Perp' },
  { path: '/spot', heading: 'Dashboard' },
  { path: '/leverage', heading: 'Dashboard' },
  { path: '/lending', heading: 'Dashboard' },
  { path: '/mint', heading: 'Mint & Redeem' },
  { path: '/stake', heading: 'Stake' },
  { path: '/history', heading: 'Transaction History' },
] as const

const dismissedWelcomeSettings = JSON.stringify({
  state: {
    sepoliaWelcomeDismissed: true,
  },
  version: 0,
})

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
  const chart = sections
    .filter({ has: page.getByText('plDXY Perp Price', { exact: true }) })
    .filter({
      has: page.getByRole('button', {
        name: '1 minute interval',
        exact: true,
      }),
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

test.beforeEach(async ({ page }) => {
  await page.addInitScript((settings) => {
    window.localStorage.setItem('plether_settings', settings)
  }, dismissedWelcomeSettings)
})

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
      await expect(page.getByRole('heading', { name: 'plDXY Perp' })).toBeVisible()

      await expectNoDocumentHorizontalOverflow(page)
      await expectPerpsLayout(page, width)
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
