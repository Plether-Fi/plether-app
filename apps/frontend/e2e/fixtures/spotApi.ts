import type { Page } from '@playwright/test'
import type { ProtocolStatus, TradeQuote, UserDashboard, ZapQuote } from '../../src/api/types'

export const SPOT_WALLET_ADDRESS = '0x1234567890123456789012345678901234567890'

const dashboard: UserDashboard = {
  balances: {
    usdc: '1000000000', bear: '250000000000000000000', bull: '500000000000000000000',
    stakedBear: '0', stakedBull: '0', stakedBearAssets: '0', stakedBullAssets: '0',
  },
  leverage: { bear: null, bull: null }, lending: { bear: null, bull: null },
  allowances: {
    usdc: { splitter: '0', zap: '0', morphoBear: '0', morphoBull: '0', curvePool: '0', leverageRouter: '0', bullLeverageRouter: '0' },
    bear: { splitter: '0', staking: '0', leverageRouter: '0', curvePool: '0' },
    bull: { splitter: '0', staking: '0', leverageRouter: '0', zapRouter: '0' },
  },
  authorization: { bearLeverageRouter: false, bullLeverageRouter: false },
}

const protocol: ProtocolStatus = {
  prices: { bear: '50000000', bull: '150000000', cap: '200000000', priceChange24h: { bear: -0.01, bull: 0.01 } },
  status: 'ACTIVE', oracle: { price: '100000000', updatedAt: 1_800_000_000, decimals: 8 },
  staking: {
    bear: { totalAssets: '0', totalShares: '0', exchangeRate: '1000000000000000000' },
    bull: { totalAssets: '0', totalShares: '0', exchangeRate: '1000000000000000000' },
    apy7d: { bear: 0.02, bull: 0.03 },
  },
  apy: { bear: { supply: 0.02, borrow: 0.03, utilization: 0.5 }, bull: { supply: 0.04, borrow: 0.05, utilization: 0.5 } },
  timestamp: 1_800_000_000,
}

/** Controlled HTTP boundary only: components, query hooks and API client stay real.
 * Quotes use an explicit fixture rate: 1 USDC buys 2 synthetic tokens; selling
 * 2 tokens returns 1 USDC. The browser assertions state concrete independent
 * examples (100 → 200, 25 → 50), not values read back from this implementation.
 */
export async function installSpotApi(page: Page) {
  await page.route('**/api/spot/v1/**', async route => {
    const url = new URL(route.request().url())
    const path = url.pathname.slice('/api/spot/v1'.length)
    const envelope = (data: unknown) => ({ data, meta: { cached: false, blockNumber: 123, chainId: 1 } })
    if (path === '/protocol/status') return route.fulfill({ json: envelope(protocol) })
    if (path === `/user/${SPOT_WALLET_ADDRESS}/dashboard`) return route.fulfill({ json: envelope(dashboard) })
    if (path === '/quotes/zap') {
      const direction = url.searchParams.get('direction')
      const amount = BigInt(url.searchParams.get('amount') ?? '0')
      if (direction !== 'buy' && direction !== 'sell') throw new Error(`Unexpected quote direction: ${direction}`)
      const output = direction === 'buy' ? amount * 2n * 10n ** 12n : amount / (2n * 10n ** 12n)
      const quote: ZapQuote = {
        direction, input: { token: direction === 'buy' ? 'usdc' : 'bull', amount: String(amount) },
        output: { token: direction === 'buy' ? 'bull' : 'usdc', amount: String(output), minAmount: String(output * 995n / 1000n) },
        priceImpact: '50', route: ['USDC', 'ZapRouter', 'plDXY-BULL'],
      }
      return route.fulfill({ json: envelope(quote) })
    }
    if (path === '/quotes/trade') {
      const from = url.searchParams.get('from')
      const amount = BigInt(url.searchParams.get('amount') ?? '0')
      if (from !== 'usdc' && from !== 'bear') throw new Error(`Unexpected quote token: ${from}`)
      const output = from === 'usdc' ? amount * 2n * 10n ** 12n : amount / (2n * 10n ** 12n)
      const quote: TradeQuote = {
        from, to: from === 'usdc' ? 'bear' : 'usdc', amountIn: String(amount), amountOut: String(output),
        minAmountOut: String(output * 995n / 1000n), spotPrice: '50000000', priceImpact: '50', fee: '0',
      }
      return route.fulfill({ json: envelope(quote) })
    }
    // An unmodeled API must not silently receive invented successful data.
    return route.fulfill({ status: 501, json: { error: { code: 'INTERNAL_ERROR', message: `No E2E fixture for ${path}` } } })
  })
}
