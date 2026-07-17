import { describe, expect, it } from 'vitest'
import { DOCS_LINKS } from './docs'

describe('documentation links', () => {
  it('links the market cost of carry tooltip to the general carry explanation', () => {
    expect(DOCS_LINKS.marketCostOfCarry).toEqual({
      href: 'https://docs.plether.com/how-plether-works/trading-costs-fees-carry-and-vpi#cost-of-carry',
      title: 'Cost of carry',
    })
  })
})
