import { describe, expect, it } from 'vitest'
import { DOCS_LINKS } from './docs'

describe('documentation links', () => {
  it('links the market cost of carry tooltip to the general carry explanation', () => {
    expect(DOCS_LINKS.marketCostOfCarry).toEqual({
      href: 'https://docs.plether.com/how-plether-works/trading-costs-fees-carry-and-vpi#cost-of-carry',
      title: 'Cost of carry',
    })
  })

  it('links the focused trading tooltips to their exact documentation sections', () => {
    expect(DOCS_LINKS).toMatchObject({
      perpsPrice: {
        href: 'https://docs.plether.com/welcome/understanding-the-plether-dollar-index#the-mark-and-execution-economics-can-differ',
        title: 'The mark and execution economics can differ',
      },
      direction: {
        href: 'https://docs.plether.com/welcome/understanding-the-plether-dollar-index#why-the-raw-basket-moves-opposite-to-the-dollar',
        title: 'Why the raw basket moves opposite to the dollar',
      },
      contractNotional: {
        href: 'https://docs.plether.com/how-plether-works/margin-leverage-and-liquidation#the-notional-used-for-margin',
        title: 'The notional used for margin',
      },
      executionLimit: {
        href: 'https://docs.plether.com/how-plether-works/how-orders-execute#acceptable-price-protection',
        title: 'Acceptable-price protection',
      },
      maintenanceMargin: {
        href: 'https://docs.plether.com/how-plether-works/margin-leverage-and-liquidation#maintenance-margin',
        title: 'Maintenance margin',
      },
      executionReward: {
        href: 'https://docs.plether.com/how-plether-works/trading-costs-fees-carry-and-vpi#the-order-execution-reward',
        title: 'The order execution reward',
      },
      manualFinalization: {
        href: 'https://docs.plether.com/how-plether-works/how-orders-execute#who-finalizes-the-order',
        title: 'Who finalizes the order?',
      },
    })
  })
})
