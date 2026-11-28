import type { Meta, StoryObj } from '@storybook/react-vite'
import { ShareCardModal, type ShareCardOption } from '../../../insights/src/components/ShareCardModal'
import { buildDefaultOgSvg, buildOgCardSvg } from '../../../insights/src/og/ogCardSvg.js'
import {
  buildShareCardSvg,
  svgToDataUrl,
  type ShareCardData,
} from '../../../insights/src/utils/shareCard'
import './InsightsPositionBreakdown.css'

const TRADER_A = '0x07f5bdb61891a09d58e0ee2c373c287e9526386b'
const TRADER_B = '0x8b4c92a71d0e5f3a61b8c9d4e2f7a1b3c5d9e0f2'
const TRADER_C = '0x31a9f0c2e4b7d81256a3c9e0b1f4d7a2c8e6b301'
const TX_HASH_A = `0x${'a1b2c3d4'.repeat(8)}`
const TX_HASH_B = `0x${'f9e8d7c6'.repeat(8)}`

interface GalleryEntry {
  id: string
  category: string
  title: string
  subtitle: string
  svg: string
  cardData?: ShareCardData
}

const DIVISION_LIVE_IRON: ShareCardData = {
  kind: 'division',
  traderLabel: '@godwynmets',
  account: TRADER_A,
  networkName: 'Arbitrum Sepolia',
  tier: 'Iron',
  rank: 7,
  weeklyReturnLabel: '+3.40%',
  activeDays: 0,
  weekLabel: 'Week of 22 Nov 2026',
  provisional: true,
}

const DIVISION_PUBLISHED_GOLD: ShareCardData = {
  kind: 'division',
  traderLabel: '@pavilion6018',
  account: TRADER_B,
  networkName: 'Arbitrum Sepolia',
  tier: 'Gold',
  rank: 1,
  weeklyReturnLabel: '+29.67%',
  weeklyPnlAssets: '11688270000',
  activeDays: 4,
  movement: 'promoted',
  weekLabel: 'Ended 20 Nov 2026',
  provisional: false,
}

const DIVISION_DIAMOND_5_OF_5: ShareCardData = {
  kind: 'division',
  traderLabel: '@fx_whale',
  account: TRADER_C,
  networkName: 'Arbitrum One',
  tier: 'Diamond',
  rank: 1,
  weeklyReturnLabel: '+42.15%',
  weeklyPnlAssets: '48920500000',
  activeDays: 5,
  movement: 'unchanged',
  weekLabel: 'Ended 20 Nov 2026',
  provisional: false,
}

const DIVISION_PENDING_BADGE: ShareCardData = {
  kind: 'division',
  traderLabel: '@new_entrant',
  account: TRADER_B,
  networkName: 'Arbitrum Sepolia',
  tier: null,
  rank: 14,
  weeklyReturnLabel: '+6.82%',
  activeDays: 2,
  weekLabel: 'Week of 22 Nov 2026',
  provisional: true,
}

const ACCOUNT_OVERVIEW: ShareCardData = {
  kind: 'account',
  traderLabel: '@mnitarik',
  account: TRADER_B,
  networkName: 'Arbitrum Sepolia',
  releaseVersion: 'v2.0.0',
  accountBalanceAssets: '146935680000',
  realizedPnlAssets: '71572430000',
  volumeAssets: '731020000000',
  totalTrades: 42,
  activeDays: 11,
  firstActivityAt: '2026-11-10T10:00:00Z',
  tier: 'Silver',
  balancePoints: [
    { timestamp: 1794300000, balanceAssets: '75000000000' },
    { timestamp: 1794600000, balanceAssets: '84200000000' },
    { timestamp: 1794900000, balanceAssets: '96500000000' },
    { timestamp: 1795150000, balanceAssets: '91000000000' },
    { timestamp: 1795400000, balanceAssets: '122000000000' },
    { timestamp: 1795690000, balanceAssets: '146935680000' },
  ],
}

const TRADE_CLOSED_PROFIT: ShareCardData = {
  kind: 'trade',
  traderLabel: '@alice_fx',
  account: TRADER_A,
  networkName: 'Arbitrum Sepolia',
  releaseVersion: 'v2.0.0',
  market: 'plDXY-PERP',
  side: 'long',
  realizedPnlAssets: '4205000000',
  price: '94756243',
  sizeDelta: '50000000000000000000000',
  notionalAssets: '50000000000',
  occurredAt: '2026-11-26T12:00:00Z',
  txHash: TX_HASH_A,
}

const TRADE_CLOSED_LOSS: ShareCardData = {
  kind: 'trade',
  traderLabel: '@short_dxy',
  account: TRADER_C,
  networkName: 'Arbitrum Sepolia',
  releaseVersion: 'v2.0.0',
  market: 'plDXY-PERP',
  side: 'short',
  realizedPnlAssets: '-1280400000',
  price: '95412000',
  sizeDelta: '30000000000000000000000',
  notionalAssets: '31376400000',
  occurredAt: '2026-11-25T16:45:00Z',
  txHash: TX_HASH_B,
}

const TRADE_OPENED_SHORT: ShareCardData = {
  kind: 'open_trade',
  traderLabel: '@alice_fx',
  account: TRADER_A,
  networkName: 'Arbitrum Sepolia',
  releaseVersion: 'v2.0.0',
  market: 'plDXY-PERP',
  side: 'short',
  price: '94589000',
  sizeDelta: '47433000000000000000000',
  notionalAssets: '50000000000',
  occurredAt: '2026-11-23T11:53:00Z',
  txHash: TX_HASH_B,
}

const POSITION_LIVE_HEALTHY: ShareCardData = {
  kind: 'position',
  traderLabel: '@macro_long',
  account: TRADER_B,
  networkName: 'Arbitrum One',
  releaseVersion: 'v2.0.0',
  market: 'plDXY-PERP',
  side: 'long',
  size: '85000000000000000000000',
  entryPrice: '95820000',
  marginAssets: '6000000000',
  unrealizedPnlAssets: '1480000000',
  liquidatable: false,
}

const POSITION_LIVE_LIQUIDATABLE: ShareCardData = {
  kind: 'position',
  traderLabel: '@degen_500x',
  account: TRADER_C,
  networkName: 'Arbitrum Sepolia',
  releaseVersion: 'v2.0.0',
  market: 'plDXY-PERP',
  side: 'short',
  size: '120000000000000000000000',
  entryPrice: '94100000',
  marginAssets: '3500000000',
  unrealizedPnlAssets: '-3120000000',
  liquidatable: true,
}

const LIFECYCLE_CLOSED_PROFIT: ShareCardData = {
  kind: 'position_lifecycle',
  traderLabel: '@alice_fx',
  account: TRADER_A,
  networkName: 'Arbitrum Sepolia',
  releaseVersion: 'v2.0.0',
  positionId: 'pos-0x1111:0',
  market: 'plDXY-PERP',
  side: 'long',
  status: 'closed',
  netPnlAssets: '1250400000',
  roiBps: 1840,
  entryPrice: '95800000',
  exitOrMarkPrice: '93500000',
  peakNotionalAssets: '50000000000',
  maxLeverageLabel: '7.4x',
  mfePnlAssets: '1500000000',
  maePnlAssets: '-210000000',
  liqBufferLabel: '14.2%',
  durationLabel: '3h 42m',
}

const LIFECYCLE_LIQUIDATED: ShareCardData = {
  kind: 'position_lifecycle',
  traderLabel: '@degen_500x',
  account: TRADER_C,
  networkName: 'Arbitrum Sepolia',
  releaseVersion: 'v2.0.0',
  positionId: 'pos-0x31a9:4',
  market: 'plDXY-PERP',
  side: 'short',
  status: 'liquidated',
  netPnlAssets: '-2450000000',
  roiBps: -8920,
  entryPrice: '94900000',
  exitOrMarkPrice: '96850000',
  peakNotionalAssets: '110000000000',
  maxLeverageLabel: '40.0x',
  mfePnlAssets: '180000000',
  maePnlAssets: '-2450000000',
  liqBufferLabel: '0.00% (Liq)',
  durationLabel: '41m',
}

const LIQUIDITY_EFFICIENCY: ShareCardData = {
  kind: 'liquidity_efficiency',
  networkName: 'Arbitrum Sepolia',
  releaseVersion: 'v2.0.0',
  poolTvlAssets: '266324039691594',
  seniorTvlAssets: '40098400866815',
  juniorTvlAssets: '226225638824779',
  volume24hAssets: '176322889629990',
  volume7dAssets: '1968245457410846',
  turnover24hX100: 66,
  turnover7dAvgDailyX100: 117,
  turnover30dAvgDailyX100: 104,
  effectiveTakeRate7dBpsX100: 1252,
  annualizedFeeYield7dBps: 5344,
  currentLongOiAssets: '119554364913940',
  currentShortOiAssets: '105460606838158',
  currentGrossOiAssets: '225014971752098',
  currentNetSkewAssets: '14093758075782',
  grossOiToPoolBps: 8449,
  netSkewToPoolBps: 529,
  turnoverSeriesX100: [
    72, 118, 165, 134, 98, 210, 184, 152, 88, 145, 232, 194, 128, 112, 167, 262, 171, 71, 29, 71,
    109, 66,
  ],
  observedAt: '2026-11-28T14:21:14Z',
}

function entryFor(
  id: string,
  category: string,
  title: string,
  subtitle: string,
  cardData: ShareCardData,
): GalleryEntry {
  return {
    id,
    category,
    title,
    subtitle,
    svg: buildShareCardSvg(cardData),
    cardData,
  }
}

const ALL_ENTRIES: GalleryEntry[] = [
  entryFor(
    'division-live-iron',
    'Divisions',
    'Divisions — Live Provisional (Iron, 0/5 FX days)',
    '5-pip FX session strip, Fri 21:00 UTC cutoff, and live qualification status (zero duplicate tier/status)',
    DIVISION_LIVE_IRON,
  ),
  entryFor(
    'division-published-gold',
    'Divisions',
    'Divisions — Published & Promoted (Gold, 4/5 FX days)',
    'Published weekly PnL, official Gold crest, and Verified & ranked audit status',
    DIVISION_PUBLISHED_GOLD,
  ),
  entryFor(
    'division-diamond',
    'Divisions',
    'Divisions — Diamond Crest (5/5 FX days, Rank #1)',
    'Full 5/5 green FX session pips with Diamond crest',
    DIVISION_DIAMOND_5_OF_5,
  ),
  entryFor(
    'division-pending',
    'Divisions',
    'Divisions — New Entrant (Pending Monday Badge)',
    'No crest yet; Column 3 shows Pending Monday without empty placeholder artwork',
    DIVISION_PENDING_BADGE,
  ),
  entryFor(
    'account-overview',
    'Trading Account',
    'Trading Account — All-Time Overview & Balance Curve',
    'Step-line lifetime balance curve paired with unified 4-column hairline ribbon (no duplicate balance line)',
    ACCOUNT_OVERVIEW,
  ),
  entryFor(
    'trade-closed-profit',
    'Trade Receipts',
    'Closed Position Receipt — Profitable Long',
    'Realized PnL hero + ROE on notional + 4-column hairline execution ribbon',
    TRADE_CLOSED_PROFIT,
  ),
  entryFor(
    'trade-closed-loss',
    'Trade Receipts',
    'Closed Position Receipt — Realized Loss Short',
    'Negative PnL treatment with brand-orange glow and 4-column hairline ribbon',
    TRADE_CLOSED_LOSS,
  ),
  entryFor(
    'trade-opened-short',
    'Trade Receipts',
    'Opened Position Receipt — Short Exposure',
    'Deduplicated 3-column hairline ribbon (Entry price, Quantity opened, Executed UTC)',
    TRADE_OPENED_SHORT,
  ),
  entryFor(
    'position-live-healthy',
    'Positions & Lifecycles',
    'Live Perpetual Position — Healthy Long',
    'Unrealized PnL + ROE + 4-column hairline ribbon with green Healthy status dot',
    POSITION_LIVE_HEALTHY,
  ),
  entryFor(
    'position-live-liquidatable',
    'Positions & Lifecycles',
    'Live Perpetual Position — Liquidatable Short',
    'Danger state glow + orange Liquidatable status dot',
    POSITION_LIVE_LIQUIDATABLE,
  ),
  entryFor(
    'lifecycle-closed-profit',
    'Positions & Lifecycles',
    'Position Lifecycle Audit — Closed Long (MFE/MAE Bar)',
    'Includes proportional green/orange MFE vs. MAE excursion micro-bar inside the ribbon',
    LIFECYCLE_CLOSED_PROFIT,
  ),
  entryFor(
    'lifecycle-liquidated',
    'Positions & Lifecycles',
    'Position Lifecycle Audit — Liquidated Short',
    'High-drawdown excursion micro-bar and 0.00% (Liq) buffer treatment',
    LIFECYCLE_LIQUIDATED,
  ),
  entryFor(
    'liquidity-efficiency',
    'Liquidity Pool',
    'LP Pool Capital Efficiency — Vol / Pool & Open Interest',
    'Daily Vol/Pool turnover velocity bar strip, Long/Short OI split, Senior/Junior TVL micro-bar, and 7d fee APR ribbon',
    LIQUIDITY_EFFICIENCY,
  ),
  {
    id: 'og-liquidity',
    category: 'Default OG',
    title: 'Server OG Card — /og/liquidity.png (ogCardSvg.js)',
    subtitle: 'Server-rendered 1200×630 Open Graph card for insights.plether.com/liquidity',
    svg: buildOgCardSvg(LIQUIDITY_EFFICIENCY as unknown as Record<string, unknown>),
  },
  {
    id: 'og-default',
    category: 'Default OG',
    title: 'Default Protocol OG Card (insights.plether.com)',
    subtitle: 'Server-rendered 1200×630 Open Graph fallback card from ogCardSvg.js',
    svg: buildDefaultOgSvg(),
  },
]

const MODAL_OPTIONS: ShareCardOption[] = ALL_ENTRIES.filter((e) => e.cardData != null).map((e) => ({
  id: e.id,
  label: e.title.replace(/^[^—]+—\s*/, ''),
  data: e.cardData!,
}))

function ShareCardsGallery({
  entries,
  columns = 2,
  showModalTrigger = true,
}: {
  entries: GalleryEntry[]
  columns?: 1 | 2
  showModalTrigger?: boolean
}) {
  return (
    <div className="min-h-screen bg-app-bg p-6 text-content-primary sm:p-8">
      <div className="mx-auto max-w-[1440px] space-y-6">
        <header className="flex flex-wrap items-end justify-between gap-4 border-b border-brand-border/25 pb-5">
          <div className="space-y-1.5">
            <p className="font-mono text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">
              Plether Insights · Social &amp; OG Cards
            </p>
            <h1 className="text-2xl font-semibold tracking-tight sm:text-3xl">
              Shareable Card Gallery ({String(entries.length)})
            </h1>
            <p className="text-sm text-content-secondary">
              1200×630 SVG templates (exported at 2400×1260 retina PNG) with unified hairline metric
              ribbons, domain micro-visuals, and deduplicated stats.
            </p>
          </div>
          {showModalTrigger ? (
            <ShareCardModal cards={MODAL_OPTIONS} triggerLabel="Open Interactive Share Modal" />
          ) : null}
        </header>

        <div
          className={
            columns === 1
              ? 'mx-auto grid max-w-4xl grid-cols-1 gap-8'
              : 'grid grid-cols-1 gap-6 xl:grid-cols-2'
          }
        >
          {entries.map((entry) => (
            <article
              key={entry.id}
              className="flex flex-col overflow-hidden border border-brand-border/25 bg-surface-panel"
            >
              <div className="flex items-baseline justify-between gap-3 border-b border-brand-border/20 px-4 py-3">
                <div className="min-w-0">
                  <h2 className="truncate text-sm font-semibold text-content-primary">
                    {entry.title}
                  </h2>
                  <p className="mt-0.5 text-xs text-content-tertiary">{entry.subtitle}</p>
                </div>
                <span className="shrink-0 border border-brand-border/30 bg-app-bg px-2 py-0.5 font-mono text-[10px] uppercase tracking-wider text-brand-peach">
                  {entry.category}
                </span>
              </div>
              <div className="bg-app-bg/70 p-3 sm:p-4">
                <img
                  src={svgToDataUrl(entry.svg)}
                  alt={entry.title}
                  className="block w-full border border-brand-border/20 shadow-lg"
                />
              </div>
            </article>
          ))}
        </div>
      </div>
    </div>
  )
}

const meta: Meta<typeof ShareCardsGallery> = {
  title: 'Insights/Share Cards — Social & OG Gallery',
  component: ShareCardsGallery,
  parameters: { layout: 'fullscreen' },
}

export default meta
type Story = StoryObj<typeof meta>

export const AllCardsGallery: Story = {
  args: {
    entries: ALL_ENTRIES,
    columns: 2,
    showModalTrigger: true,
  },
}

export const DivisionsVariants: Story = {
  args: {
    entries: ALL_ENTRIES.filter((e) => e.category === 'Divisions'),
    columns: 2,
    showModalTrigger: false,
  },
}

export const TradingAccountCard: Story = {
  args: {
    entries: ALL_ENTRIES.filter((e) => e.category === 'Trading Account'),
    columns: 1,
    showModalTrigger: false,
  },
}

export const TradeReceipts: Story = {
  args: {
    entries: ALL_ENTRIES.filter((e) => e.category === 'Trade Receipts'),
    columns: 1,
    showModalTrigger: false,
  },
}

export const PositionsAndLifecycles: Story = {
  args: {
    entries: ALL_ENTRIES.filter((e) => e.category === 'Positions & Lifecycles'),
    columns: 2,
    showModalTrigger: false,
  },
}

export const DefaultProtocolOgCard: Story = {
  args: {
    entries: ALL_ENTRIES.filter((e) => e.category === 'Default OG'),
    columns: 1,
    showModalTrigger: false,
  },
}
