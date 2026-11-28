import { getOfficialBadgePaths } from '../divisions/badgeArtwork'
import type { Tier } from '../divisions/rules'
import { formatPlDxyQuantity } from '../api/wallets'
import { buildAddressAvatarInnerSvg } from './addressAvatar'
import { formatAssets, formatCompactAssets, formatPrice, formatUtc, shortAddress } from './format'

export const SHARE_CARD_WIDTH = 1200
export const SHARE_CARD_HEIGHT = 630

export interface TradeShareCardData {
  kind: 'trade'
  traderLabel: string
  account: string
  networkName: string
  releaseVersion?: string | null
  market: string
  side: 'long' | 'short' | null
  realizedPnlAssets: string
  price?: string | null
  sizeDelta?: string | null
  notionalAssets?: string | null
  occurredAt: string
  txHash?: string | null
}

export interface OpenTradeShareCardData {
  kind: 'open_trade'
  traderLabel: string
  account: string
  networkName: string
  releaseVersion?: string | null
  market: string
  side: 'long' | 'short' | null
  price?: string | null
  sizeDelta?: string | null
  notionalAssets?: string | null
  occurredAt: string
  txHash?: string | null
}

export interface PositionShareCardData {
  kind: 'position'
  traderLabel: string
  account: string
  networkName: string
  releaseVersion?: string | null
  market: string
  side: 'long' | 'short' | 'unknown'
  size: string
  entryPrice: string
  marginAssets: string
  unrealizedPnlAssets: string
  liquidatable: boolean
}

export interface DivisionShareCardData {
  kind: 'division'
  traderLabel: string
  account: string
  networkName: string
  tier?: Tier | null
  rank: number | null
  weeklyReturnLabel: string
  weeklyPnlAssets?: string | null
  activeDays: number
  movement?: string | null
  weekLabel: string
  provisional: boolean
}

export interface AccountBalancePoint {
  timestamp: number
  balanceAssets: string
}

export interface AccountShareCardData {
  kind: 'account'
  traderLabel: string
  account: string
  networkName: string
  releaseVersion?: string | null
  netEquityAssets?: string | null
  accountBalanceAssets?: string | null
  realizedPnlAssets?: string | null
  volumeAssets?: string | null
  totalTrades?: number | null
  activeDays?: number | null
  firstActivityAt?: string | null
  tier?: Tier | null
  balancePoints?: AccountBalancePoint[]
  sparklineValues?: number[]
}

export interface PositionLifecycleShareCardData {
  kind: 'position_lifecycle'
  traderLabel: string
  account: string
  networkName: string
  releaseVersion?: string | null
  market: string
  side: 'long' | 'short' | 'unknown'
  status: 'open' | 'closed' | 'liquidated'
  positionId: string
  netPnlAssets: string
  roiBps?: number | null
  entryPrice: string
  exitOrMarkPrice?: string | null
  peakNotionalAssets: string
  maxLeverageLabel: string
  mfePnlAssets: string
  maePnlAssets: string
  liqBufferLabel: string
  durationLabel: string
}

export interface LiquidityEfficiencyShareCardData {
  kind: 'liquidity_efficiency'
  networkName: string
  releaseVersion?: string | null
  poolTvlAssets?: string | null
  seniorTvlAssets?: string | null
  juniorTvlAssets?: string | null
  volume24hAssets: string
  volume7dAssets: string
  turnover24hX100?: number | null
  turnover7dAvgDailyX100?: number | null
  turnover30dAvgDailyX100?: number | null
  effectiveTakeRate7dBpsX100?: number | null
  annualizedFeeYield7dBps?: number | null
  currentLongOiAssets: string
  currentShortOiAssets: string
  currentGrossOiAssets: string
  currentNetSkewAssets: string
  grossOiToPoolBps?: number | null
  netSkewToPoolBps?: number | null
  turnoverSeriesX100?: (number | null)[]
  observedAt?: string | null
}

export type ShareCardData =
  | TradeShareCardData
  | OpenTradeShareCardData
  | PositionShareCardData
  | PositionLifecycleShareCardData
  | DivisionShareCardData
  | AccountShareCardData
  | LiquidityEfficiencyShareCardData

const TIER_COLORS: Record<string, { primary: string; secondary: string; glow: string }> = {
  iron: { primary: '#FFAB96', secondary: '#F2CF71', glow: '#FFAB96' },
  bronze: { primary: '#F2CF71', secondary: '#FF572D', glow: '#FFAB96' },
  silver: { primary: '#FFAB96', secondary: '#FF572D', glow: '#FF572D' },
  gold: { primary: '#F2CF71', secondary: '#FF572D', glow: '#F2CF71' },
  platinum: { primary: '#F2CF71', secondary: '#FF572D', glow: '#FF572D' },
  diamond: { primary: '#F2CF71', secondary: '#FF572D', glow: '#FF572D' },
}

const DEFAULT_TIER_PALETTE = { primary: '#FFAB96', secondary: '#FF572D', glow: '#FF572D' }

function resolveTierPalette(tier: string | null | undefined): {
  primary: string
  secondary: string
  glow: string
} {
  if (!tier) return DEFAULT_TIER_PALETTE
  return TIER_COLORS[tier.trim().toLowerCase()] ?? DEFAULT_TIER_PALETTE
}

export function escapeXml(value: string): string {
  return value
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;')
    .replaceAll("'", '&apos;')
}

function formatSignedUsdg(raw: string): { text: string; tone: 'positive' | 'negative' | 'neutral' } {
  if (!/^-?\d+$/.test(raw)) return { text: 'Unavailable', tone: 'neutral' }
  const val = BigInt(raw)
  const formatted = formatAssets(val < 0n ? (-val).toString() : val.toString())
  if (val > 0n) return { text: `+${formatted}`, tone: 'positive' }
  if (val < 0n) return { text: `−${formatted}`, tone: 'negative' }
  return { text: formatted, tone: 'neutral' }
}

function computeRoePercent(pnlRaw: string, baseRaw: string | null | undefined): string | null {
  if (!baseRaw || !/^-?\d+$/.test(pnlRaw) || !/^\d+$/.test(baseRaw)) return null
  const base = BigInt(baseRaw)
  if (base <= 0n) return null
  const pct = (Number(BigInt(pnlRaw)) / Number(base)) * 100
  if (!Number.isFinite(pct)) return null
  return `${pct >= 0 ? '+' : ''}${pct.toFixed(2)}%`
}

function toneHex(tone: 'positive' | 'negative' | 'neutral'): string {
  if (tone === 'positive') return '#00ff99'
  if (tone === 'negative') return '#ff572d'
  return '#fff5f9'
}

function renderPletherLogo(x: number, y: number, scale: number): string {
  return `<g transform="translate(${String(x)}, ${String(y)}) scale(${scale.toFixed(4)})">
    <path d="M178.41 206.91C194.343 206.91 207.26 193.993 207.26 178.06C207.26 162.127 194.343 149.21 178.41 149.21C162.477 149.21 149.56 162.127 149.56 178.06C149.56 193.993 162.477 206.91 178.41 206.91Z" fill="#FFF5F9"/>
    <path d="M65.13 72.82H0.27V137.64H65.13V72.82Z" fill="#FFF5F9"/>
    <path d="M65.4 145.64H0V210.47H65.4V145.64Z" fill="#FFF5F9"/>
    <path d="M137.99 145.64H73.13V210.47H137.99V145.64Z" fill="#FFF5F9"/>
    <path d="M137.98 0H73.13V64.82H137.98V0Z" fill="#FFF5F9"/>
    <path d="M65.13 0H0.28V64.82H65.13V0Z" fill="#FFF5F9"/>
    <path d="M210.84 0H145.98V64.82H210.84V0Z" fill="#FFF5F9"/>
    <path d="M210.84 72.82H145.98V137.64H210.84V72.82Z" fill="#FFF5F9"/>
    <path d="M73.13 72.82V137.64H137.99V72.82H73.13ZM105.56 134.08C89.63 134.08 76.71 121.17 76.71 105.23C76.71 89.29 89.63 76.39 105.56 76.39C121.49 76.39 134.41 89.3 134.41 105.23C134.41 121.16 121.49 134.08 105.56 134.08Z" fill="#FFF5F9"/>
  </g>`
}

function pillWidth(label: string): number {
  return Math.round(Math.max(96, label.length * 10.5 + 32))
}

function renderPill(x: number, y: number, label: string, bg: string, border: string, fg: string): string {
  const width = pillWidth(label)
  return `<g>
    <rect x="${String(x)}" y="${String(y)}" width="${String(width)}" height="34" fill="${bg}" stroke="${border}" stroke-width="1.2" />
    <text x="${String(Math.round(x + width / 2))}" y="${String(y + 22)}" text-anchor="middle" fill="${fg}" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="14" font-weight="700" letter-spacing="1.2">${escapeXml(label)}</text>
  </g>`
}

function formatShareCardUtc(iso: string | null | undefined): string {
  if (!iso) return '—'
  const date = new Date(iso)
  if (Number.isNaN(date.getTime())) return formatUtc(iso).replace(/\s+UTC$/, '')
  const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
  const day = String(date.getUTCDate()).padStart(2, '0')
  const mon = months[date.getUTCMonth()]
  const year = String(date.getUTCFullYear())
  const hh = String(date.getUTCHours()).padStart(2, '0')
  const mm = String(date.getUTCMinutes()).padStart(2, '0')
  return `${day} ${mon} ${year}, ${hh}:${mm}`
}

interface RibbonItem {
  label: string
  value: string
  accent?: string
  /** Optional status dot rendered to the left of the value. */
  dotColor?: string
  /** Optional domain-native SVG micro-visual rendered below/beside the value inside the column. */
  renderVisual?: (x: number, y: number, colW: number) => string
}

function fitStatFontSize(value: string, colW: number, maxSize: number): number {
  const usableW = Math.max(80, colW - 48)
  const charCount = Math.max(1, value.length)
  const fitted = Math.floor(usableW / (charCount * 0.64))
  return Math.min(maxSize, Math.max(14, fitted))
}

/**
 * Unified borderless hairline metric ribbon replacing separated boxed KPI cards.
 * Uses a single translucent band with subtle top/bottom hairlines and vertical column dividers.
 */
function renderMetricRibbon(items: RibbonItem[], y: number): string {
  const totalW = 1080
  const height = 96
  const count = items.length
  const colW = totalW / count

  const colsSvg = items
    .map((item, idx) => {
      const x = Math.round(60 + idx * colW)
      const width = Math.round(colW)
      const divider =
        idx > 0
          ? `<line x1="${String(x)}" y1="${String(y + 18)}" x2="${String(x)}" y2="${String(y + height - 18)}" stroke="#ffab96" stroke-opacity="0.2" stroke-width="1" />`
          : ''
      const labelSvg = `<text x="${String(x + 24)}" y="${String(y + 30)}" fill="#aa98a0" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="12" font-weight="600" letter-spacing="1.2">${escapeXml(item.label.toUpperCase())}</text>`

      if (item.renderVisual) {
        return `<g>${divider}${labelSvg}${item.renderVisual(x + 24, y + 44, width - 48)}</g>`
      }

      const fontSize = fitStatFontSize(item.value, width - (item.dotColor ? 24 : 0), 23)
      const valueX = item.dotColor ? x + 44 : x + 24
      const dotSvg = item.dotColor
        ? `<circle cx="${String(x + 29)}" cy="${String(y + 60)}" r="4.5" fill="${item.dotColor}" />`
        : ''
      return `<g>
        ${divider}
        ${labelSvg}
        ${dotSvg}
        <text x="${String(valueX)}" y="${String(y + 68)}" fill="${item.accent ?? '#fff5f9'}" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="${String(fontSize)}" font-weight="700">${escapeXml(item.value)}</text>
      </g>`
    })
    .join('')

  return `<g>
    <rect x="60" y="${String(y)}" width="${String(totalW)}" height="${String(height)}" fill="#3b212d" fill-opacity="0.36" />
    <line x1="60" y1="${String(y)}" x2="1140" y2="${String(y)}" stroke="#ffab96" stroke-opacity="0.28" stroke-width="1" />
    <line x1="60" y1="${String(y + height)}" x2="1140" y2="${String(y + height)}" stroke="#ffab96" stroke-opacity="0.16" stroke-width="1" />
    ${colsSvg}
  </g>`
}

function renderFxSessionPips(activeDays: number, activeColor: string): (x: number, y: number, w: number) => string {
  const clamped = Math.max(0, Math.min(5, Math.round(activeDays)))
  const labels = ['M', 'T', 'W', 'T', 'F']
  return (x, y) => {
    const pipW = 24
    const pipH = 22
    const gap = 6
    const pips = labels
      .map((day, idx) => {
        const px = x + idx * (pipW + gap)
        const active = idx < clamped
        return `<g>
          <rect x="${String(px)}" y="${String(y + 4)}" width="${String(pipW)}" height="${String(pipH)}" fill="${active ? activeColor : '#250917'}" fill-opacity="${active ? '0.2' : '0.6'}" stroke="${active ? activeColor : '#ffab96'}" stroke-opacity="${active ? '0.85' : '0.24'}" stroke-width="1.2" />
          <text x="${String(px + pipW / 2)}" y="${String(y + 19)}" text-anchor="middle" fill="${active ? activeColor : '#aa98a0'}" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="11" font-weight="700">${day}</text>
        </g>`
      })
      .join('')
    const textX = x + 5 * (pipW + gap) + 10
    return `${pips}<text x="${String(textX)}" y="${String(y + 21)}" fill="#fff5f9" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="20" font-weight="700">${String(clamped)} / 5</text>`
  }
}

function renderExcursionMicroBar(
  mfeRaw: string,
  maeRaw: string,
  mfeText: string,
  maeText: string,
): (x: number, y: number, w: number) => string {
  const mfeMag = /^-?\d+$/.test(mfeRaw) ? Math.abs(Number(BigInt(mfeRaw))) : 0
  const maeMag = /^-?\d+$/.test(maeRaw) ? Math.abs(Number(BigInt(maeRaw))) : 0
  const total = mfeMag + maeMag
  const mfeRatio = total > 0 ? Math.max(0.08, Math.min(0.92, mfeMag / total)) : 0.5
  return (x, y, w) => {
    const barW = Math.max(80, w)
    const greenW = Math.round(barW * mfeRatio)
    const orangeW = Math.max(0, barW - greenW)
    const combined = `${mfeText} / ${maeText}`
    const fontSize = fitStatFontSize(combined, w + 48, 17)
    return `
      <text x="${String(x)}" y="${String(y + 16)}" fill="#fff5f9" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="${String(fontSize)}" font-weight="700">${escapeXml(combined)}</text>
      <rect x="${String(x)}" y="${String(y + 27)}" width="${String(barW)}" height="6" fill="#250917" stroke="#ffab96" stroke-opacity="0.22" stroke-width="0.8" />
      <rect x="${String(x)}" y="${String(y + 27)}" width="${String(greenW)}" height="6" fill="#00ff99" fill-opacity="0.85" />
      <rect x="${String(x + greenW)}" y="${String(y + 27)}" width="${String(orangeW)}" height="6" fill="#ff572d" fill-opacity="0.85" />
    `
  }
}

function renderTierBadge(tier: Tier | null | undefined, cx: number, cy: number): string {
  const officialPaths = getOfficialBadgePaths(tier)
  if (!officialPaths || !tier) return ''

  const palette = resolveTierPalette(tier)
  const size = 188
  const scale = size / 166
  const left = Math.round(cx - size / 2)
  const top = Math.round(cy - size / 2)

  return `<g>
    <circle cx="${String(cx)}" cy="${String(cy)}" r="108" fill="${palette.glow}" fill-opacity="0.14" />
    <circle cx="${String(cx)}" cy="${String(cy)}" r="98" fill="none" stroke="${palette.primary}" stroke-opacity="0.45" stroke-width="1.5" />
    <g transform="translate(${String(left)}, ${String(top)}) scale(${scale.toFixed(4)})">
      ${officialPaths}
    </g>
    <text x="${String(cx)}" y="${String(cy + 126)}" text-anchor="middle" fill="${palette.primary}" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="16" font-weight="700" letter-spacing="2.4">${escapeXml(tier.toUpperCase())} DIVISION</text>
  </g>`
}

function formatCompactUsdValue(dollars: number): string {
  if (!Number.isFinite(dollars)) return '—'
  const abs = Math.abs(dollars)
  const sign = dollars < 0 ? '-' : ''
  if (abs >= 1_000_000_000) return `${sign}${(abs / 1_000_000_000).toFixed(2)}B`
  if (abs >= 1_000_000) return `${sign}${(abs / 1_000_000).toFixed(2)}M`
  if (abs >= 1_000) return `${sign}${(abs / 1_000).toFixed(2)}K`
  return `${sign}${abs.toFixed(2)}`
}

function formatMonthDayYear(isoOrUnix: string | number | null | undefined): string | null {
  if (isoOrUnix == null) return null
  const date = typeof isoOrUnix === 'number' ? new Date(isoOrUnix * 1000) : new Date(isoOrUnix)
  if (Number.isNaN(date.getTime())) return null
  const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
  return `${String(date.getUTCDate())} ${months[date.getUTCMonth()]} ${String(date.getUTCFullYear())}`
}

function renderLifetimeBalanceChart(
  balancePoints: AccountBalancePoint[] | undefined,
  fallbackValues: number[] | undefined,
): { svg: string; accent: string; startLabel: string | null } {
  const boxX = 532
  const boxY = 132
  const boxW = 608
  const boxH = 252
  const plotX = boxX + 22
  const plotY = boxY + 52
  const plotW = boxW - 44
  const plotH = 146

  const normalized: { ts: number; val: number }[] =
    balancePoints && balancePoints.length > 0
      ? [...balancePoints]
          .map((pt) => ({ ts: pt.timestamp, val: Number(pt.balanceAssets) / 1_000_000 }))
          .filter((pt) => Number.isFinite(pt.ts) && Number.isFinite(pt.val))
          .sort((a, b) => a.ts - b.ts)
      : (fallbackValues ?? [])
          .filter((v) => Number.isFinite(v))
          .map((val, idx) => ({ ts: idx, val }))

  if (normalized.length < 2) {
    const emptySvg = `<g>
      <rect x="${String(boxX)}" y="${String(boxY)}" width="${String(boxW)}" height="${String(boxH)}" fill="#3b212d" fill-opacity="0.52" stroke="#ffab96" stroke-opacity="0.26" stroke-width="1" />
      <text x="${String(plotX)}" y="${String(boxY + 30)}" fill="#aa98a0" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="13" font-weight="700" letter-spacing="1.3">LIFE-LONG BALANCE</text>
      <text x="${String(Math.round(boxX + boxW / 2))}" y="${String(Math.round(boxY + boxH / 2 + 10))}" text-anchor="middle" fill="#aa98a0" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="15" font-weight="500">Awaiting life-long balance history</text>
    </g>`
    return { svg: emptySvg, accent: '#ffab96', startLabel: null }
  }

  const start = normalized[0]
  const end = normalized[normalized.length - 1]
  const stroke = end.val < start.val ? '#ff572d' : '#00ff99'
  const vals = normalized.map((pt) => pt.val)
  const rawMin = Math.min(...vals)
  const rawMax = Math.max(...vals)
  const span = rawMax - rawMin
  const pad = span === 0 ? Math.max(rawMax * 0.08, 10) : span * 0.14
  const minVal = Math.max(0, rawMin - pad)
  const maxVal = rawMax + pad
  const rangeVal = Math.max(maxVal - minVal, 1)

  const minTs = start.ts
  const maxTs = end.ts
  const tsSpan = Math.max(maxTs - minTs, 1)
  const useTimestamps = balancePoints != null && balancePoints.length >= 2 && maxTs > minTs

  const coords = normalized.map((pt, idx) => {
    const xRatio = useTimestamps ? (pt.ts - minTs) / tsSpan : idx / (normalized.length - 1)
    const x = plotX + xRatio * plotW
    const y = plotY + (1 - (pt.val - minVal) / rangeVal) * plotH
    return { x, y }
  })

  const linePath = coords
    .map((pt, idx) =>
      idx === 0
        ? `M${pt.x.toFixed(1)},${pt.y.toFixed(1)}`
        : `H${pt.x.toFixed(1)} V${pt.y.toFixed(1)}`,
    )
    .join(' ')
  const areaPath = `${linePath} L${coords[coords.length - 1].x.toFixed(1)},${String(plotY + plotH)} L${coords[0].x.toFixed(1)},${String(plotY + plotH)} Z`
  const baselineY = plotY + (1 - (start.val - minVal) / rangeVal) * plotH
  const lastPt = coords[coords.length - 1]
  const pointDots =
    coords.length <= 45
      ? coords
          .slice(0, -1)
          .map(
            (pt) =>
              `<circle cx="${pt.x.toFixed(1)}" cy="${pt.y.toFixed(1)}" r="2.6" fill="#ffab96" fill-opacity="0.8" stroke="#250917" stroke-width="1" />`,
          )
          .join('')
      : ''

  const delta = end.val - start.val
  const pctStr =
    start.val > 0
      ? `${delta >= 0 ? '+' : ''}${((delta / start.val) * 100).toFixed(2)}%`
      : null
  const rangeHeader = `${formatCompactUsdValue(start.val)} → ${formatCompactUsdValue(end.val)} USDG`
  const headerRight = pctStr ? `${pctStr} · ${rangeHeader}` : rangeHeader

  const startDate = useTimestamps ? formatMonthDayYear(start.ts) : null
  const endDate = useTimestamps ? formatMonthDayYear(end.ts) : null

  const svg = `<g>
    <defs>
      <linearGradient id="acct-bal-area" x1="0" y1="0" x2="0" y2="1">
        <stop offset="0%" stop-color="${stroke}" stop-opacity="0.28" />
        <stop offset="100%" stop-color="${stroke}" stop-opacity="0.02" />
      </linearGradient>
    </defs>
    <rect x="${String(boxX)}" y="${String(boxY)}" width="${String(boxW)}" height="${String(boxH)}" fill="#3b212d" fill-opacity="0.52" stroke="#ffab96" stroke-opacity="0.26" stroke-width="1" />
    <text x="${String(plotX)}" y="${String(boxY + 30)}" fill="#aa98a0" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="13" font-weight="700" letter-spacing="1.3">LIFE-LONG BALANCE</text>
    <text x="${String(plotX + plotW)}" y="${String(boxY + 30)}" text-anchor="end" fill="${stroke}" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="13" font-weight="700">${escapeXml(headerRight)}</text>
    <line x1="${String(plotX)}" y1="${String(plotY)}" x2="${String(plotX + plotW)}" y2="${String(plotY)}" stroke="#ffab96" stroke-opacity="0.1" stroke-width="1" />
    <line x1="${String(plotX)}" y1="${String(Math.round(plotY + plotH / 2))}" x2="${String(plotX + plotW)}" y2="${String(Math.round(plotY + plotH / 2))}" stroke="#ffab96" stroke-opacity="0.1" stroke-width="1" />
    <line x1="${String(plotX)}" y1="${String(plotY + plotH)}" x2="${String(plotX + plotW)}" y2="${String(plotY + plotH)}" stroke="#ffab96" stroke-opacity="0.16" stroke-width="1" />
    <line x1="${String(plotX)}" y1="${baselineY.toFixed(1)}" x2="${String(plotX + plotW)}" y2="${baselineY.toFixed(1)}" stroke="#aa98a0" stroke-opacity="0.38" stroke-width="1" stroke-dasharray="4 4" />
    <path d="${areaPath}" fill="url(#acct-bal-area)" />
    <path d="${linePath}" fill="none" stroke="${stroke}" stroke-width="2.6" stroke-linejoin="round" stroke-linecap="round" />
    ${pointDots}
    <circle cx="${lastPt.x.toFixed(1)}" cy="${lastPt.y.toFixed(1)}" r="9" fill="${stroke}" fill-opacity="0.22" />
    <circle cx="${lastPt.x.toFixed(1)}" cy="${lastPt.y.toFixed(1)}" r="4.5" fill="${stroke}" />
    <text x="${String(plotX)}" y="${String(plotY + 14)}" fill="#aa98a0" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="11">High ${escapeXml(formatCompactUsdValue(rawMax))}</text>
    <text x="${String(plotX)}" y="${String(plotY + plotH - 6)}" fill="#aa98a0" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="11">Low ${escapeXml(formatCompactUsdValue(rawMin))}</text>
    <text x="${String(plotX)}" y="${String(boxY + boxH - 18)}" fill="#aa98a0" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="12">${escapeXml(startDate ?? 'Start')}</text>
    <text x="${String(plotX + plotW)}" y="${String(boxY + boxH - 18)}" text-anchor="end" fill="#aa98a0" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="12">${escapeXml(endDate ? `${endDate} · Live` : 'Live')}</text>
  </g>`

  return { svg, accent: stroke, startLabel: startDate }
}

function renderFrame(
  accentGlow: string,
  eyebrow: string,
  networkName: string,
  releaseVersion: string | null | undefined,
  traderLabel: string,
  account: string,
  footerRight: string,
  bodySvg: string,
  protocolFooterSubtitle?: string,
): string {
  const networkTag = releaseVersion ? `${networkName} · ${releaseVersion}` : networkName
  const footerLeftSvg = protocolFooterSubtitle
    ? `${renderPletherLogo(60, 558, 0.12)}
  <text x="96" y="576" fill="#fff5f9" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="18" font-weight="700">${escapeXml(traderLabel)}</text>
  <text x="${String(96 + Math.min(400, traderLabel.length * 10.5 + 22))}" y="576" fill="#aa98a0" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="14">${escapeXml(protocolFooterSubtitle)}</text>`
    : `<g transform="translate(60, 558)">${buildAddressAvatarInnerSvg(account)}</g>
  <text x="96" y="576" fill="#fff5f9" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="18" font-weight="700">${escapeXml(traderLabel)}</text>
  <text x="${String(96 + Math.min(400, traderLabel.length * 11 + 24))}" y="576" fill="#aa98a0" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="15">${escapeXml(shortAddress(account))}</text>`
  return `<svg xmlns="http://www.w3.org/2000/svg" width="${String(SHARE_CARD_WIDTH)}" height="${String(SHARE_CARD_HEIGHT)}" viewBox="0 0 ${String(SHARE_CARD_WIDTH)} ${String(SHARE_CARD_HEIGHT)}">
  <defs>
    <linearGradient id="bg-grad" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0%" stop-color="#19040f" />
      <stop offset="55%" stop-color="#250917" />
      <stop offset="100%" stop-color="#2f0d1e" />
    </linearGradient>
    <radialGradient id="ambient-glow" cx="80%" cy="18%" r="55%">
      <stop offset="0%" stop-color="${accentGlow}" stop-opacity="0.26" />
      <stop offset="100%" stop-color="${accentGlow}" stop-opacity="0" />
    </radialGradient>
    <pattern id="grid-lines" width="48" height="48" patternUnits="userSpaceOnUse">
      <path d="M 48 0 L 0 0 0 48" fill="none" stroke="#ffab96" stroke-opacity="0.06" stroke-width="1" />
    </pattern>
  </defs>
  <rect width="${String(SHARE_CARD_WIDTH)}" height="${String(SHARE_CARD_HEIGHT)}" fill="url(#bg-grad)" />
  <rect width="${String(SHARE_CARD_WIDTH)}" height="${String(SHARE_CARD_HEIGHT)}" fill="url(#grid-lines)" />
  <rect width="${String(SHARE_CARD_WIDTH)}" height="${String(SHARE_CARD_HEIGHT)}" fill="url(#ambient-glow)" />
  <rect x="20" y="20" width="1160" height="590" fill="none" stroke="#ffab96" stroke-opacity="0.28" stroke-width="1.5" />

  ${renderPletherLogo(60, 48, 0.21)}
  <text x="118" y="69" fill="#fff5f9" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="22" font-weight="700" letter-spacing="0.4">Plether Insights</text>
  <text x="118" y="90" fill="#ffab96" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="13" font-weight="600" letter-spacing="1.6">${escapeXml(eyebrow.toUpperCase())}</text>

  <text x="1140" y="75" text-anchor="end" fill="#d8cbd0" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="14" font-weight="600">${escapeXml(networkTag)}</text>

  ${bodySvg}

  <line x1="60" y1="538" x2="1140" y2="538" stroke="#ffab96" stroke-opacity="0.24" stroke-width="1" />
  ${footerLeftSvg}
  <text x="1140" y="576" text-anchor="end" fill="#ffab96" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="15" font-weight="600">${escapeXml(footerRight)}</text>
</svg>`
}

export function buildShareCardSvg(data: ShareCardData): string {
  if (data.kind === 'trade') {
    const pnl = formatSignedUsdg(data.realizedPnlAssets)
    const color = toneHex(pnl.tone)
    const roi = computeRoePercent(data.realizedPnlAssets, data.notionalAssets)
    const sideText = data.side ? data.side.toUpperCase() : 'PERP'
    const sideBg = data.side === 'short' ? '#ff572d26' : '#00ff9922'
    const sideFg = data.side === 'short' ? '#ff572d' : '#00ff99'
    const stats: RibbonItem[] = [
      { label: 'Close price', value: data.price ? formatPrice(data.price) : '—' },
      { label: 'Quantity closed', value: data.sizeDelta ? formatPlDxyQuantity(data.sizeDelta) : '—' },
      { label: 'Notional', value: data.notionalAssets ? formatCompactAssets(data.notionalAssets) : '—' },
      { label: 'Executed (UTC)', value: formatShareCardUtc(data.occurredAt) },
    ]
    const body = `
      ${renderPill(60, 134, data.market, '#3b212d', '#ffab9666', '#fff5f9')}
      ${renderPill(218, 134, `CLOSED ${sideText}`, sideBg, `${sideFg}88`, sideFg)}
      <text x="60" y="220" fill="#aa98a0" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="16" font-weight="600" letter-spacing="1.4">REALIZED PNL</text>
      <text x="60" y="308" fill="${color}" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="76" font-weight="800">${escapeXml(pnl.text)}</text>
      ${roi ? `<text x="60" y="358" fill="${color}" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="24" font-weight="700">${escapeXml(roi)} on notional</text>` : ''}
      ${renderMetricRibbon(stats, 412)}
    `
    const footerRight = data.txHash
      ? `tx ${shortAddress(data.txHash)} · insights.plether.com`
      : 'insights.plether.com'
    return renderFrame(color, 'Closed Position Receipt', data.networkName, data.releaseVersion, data.traderLabel, data.account, footerRight, body)
  }

  if (data.kind === 'open_trade') {
    const sideText = data.side ? data.side.toUpperCase() : 'PERP'
    const sideFg = data.side === 'short' ? '#ff572d' : '#00ff99'
    const sideBg = data.side === 'short' ? '#ff572d26' : '#00ff9922'
    const heroText = data.notionalAssets
      ? formatAssets(data.notionalAssets)
      : data.sizeDelta
        ? formatPlDxyQuantity(data.sizeDelta)
        : 'Position Opened'
    const heroFontSize = heroText.length <= 15 ? 72 : heroText.length <= 20 ? 58 : 48
    const stats: RibbonItem[] = [
      { label: 'Entry price', value: data.price ? formatPrice(data.price) : '—' },
      { label: 'Quantity opened', value: data.sizeDelta ? formatPlDxyQuantity(data.sizeDelta) : '—' },
      { label: 'Executed (UTC)', value: formatShareCardUtc(data.occurredAt) },
    ]
    const body = `
      ${renderPill(60, 134, data.market, '#3b212d', '#ffab9666', '#fff5f9')}
      ${renderPill(218, 134, `OPENED ${sideText}`, sideBg, `${sideFg}88`, sideFg)}
      <text x="60" y="220" fill="#aa98a0" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="16" font-weight="600" letter-spacing="1.4">${data.notionalAssets ? 'NOTIONAL EXPOSURE' : 'OPENED POSITION'}</text>
      <text x="60" y="308" fill="${sideFg}" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="${String(heroFontSize)}" font-weight="800">${escapeXml(heroText)}</text>
      <text x="60" y="358" fill="#d8cbd0" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="20" font-weight="600">Verified on-chain perpetual fill</text>
      ${renderMetricRibbon(stats, 412)}
    `
    const footerRight = data.txHash
      ? `tx ${shortAddress(data.txHash)} · insights.plether.com`
      : 'insights.plether.com'
    return renderFrame(sideFg, 'Opened Position Receipt', data.networkName, data.releaseVersion, data.traderLabel, data.account, footerRight, body)
  }

  if (data.kind === 'position') {
    const upnl = formatSignedUsdg(data.unrealizedPnlAssets)
    const color = data.liquidatable ? '#ff572d' : toneHex(upnl.tone)
    const roe = computeRoePercent(data.unrealizedPnlAssets, data.marginAssets)
    const sideUpper = data.side.toUpperCase()
    const sideFg = data.side === 'short' ? '#ff572d' : '#00ff99'
    const healthColor = data.liquidatable ? '#ff572d' : '#00ff99'
    const stats: RibbonItem[] = [
      { label: 'Position size', value: formatPlDxyQuantity(data.size) },
      { label: 'Entry price', value: formatPrice(data.entryPrice) },
      { label: 'Margin posted', value: formatCompactAssets(data.marginAssets) },
      {
        label: 'Health',
        value: data.liquidatable ? 'Liquidatable' : 'Healthy',
        accent: healthColor,
        dotColor: healthColor,
      },
    ]
    const body = `
      ${renderPill(60, 134, data.market, '#3b212d', '#ffab9666', '#fff5f9')}
      ${renderPill(218, 134, `OPEN ${sideUpper}`, `${sideFg}22`, `${sideFg}88`, sideFg)}
      <text x="60" y="220" fill="#aa98a0" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="16" font-weight="600" letter-spacing="1.4">UNREALIZED PNL</text>
      <text x="60" y="308" fill="${color}" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="76" font-weight="800">${escapeXml(upnl.text)}</text>
      ${roe ? `<text x="60" y="358" fill="${color}" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="26" font-weight="700">${escapeXml(roe)} ROE</text>` : ''}
      ${renderMetricRibbon(stats, 412)}
    `
    return renderFrame(color, 'Live Perpetual Position', data.networkName, data.releaseVersion, data.traderLabel, data.account, 'CfdEngineAccountLens · insights.plether.com', body)
  }

  if (data.kind === 'position_lifecycle') {
    const netPnl = formatSignedUsdg(data.netPnlAssets)
    const color = data.status === 'liquidated' ? '#ff572d' : toneHex(netPnl.tone)
    const sideUpper = data.side.toUpperCase()
    const statusUpper = data.status.toUpperCase()
    const sideFg = data.side === 'short' ? '#ff572d' : '#00ff99'
    const roiText =
      data.roiBps != null && Number.isFinite(data.roiBps)
        ? `${data.roiBps > 0 ? '+' : ''}${(data.roiBps / 100).toFixed(2)}% ROI`
        : null
    const subParts: string[] = [
      `${formatPrice(data.entryPrice)} → ${data.exitOrMarkPrice ? formatPrice(data.exitOrMarkPrice) : 'Open'}`,
      `Held ${data.durationLabel}`,
    ]
    if (roiText) subParts.unshift(roiText)
    const mfeCompact = `+${formatCompactAssets(data.mfePnlAssets).replace(' USDG', '')}`
    const maeCompact = formatCompactAssets(data.maePnlAssets)
    const stats: RibbonItem[] = [
      { label: 'Peak notional', value: formatCompactAssets(data.peakNotionalAssets) },
      { label: 'Max leverage', value: data.maxLeverageLabel, accent: '#ffab96' },
      {
        label: 'Excursion (MFE / MAE)',
        value: `${mfeCompact} / ${maeCompact}`,
        renderVisual: renderExcursionMicroBar(data.mfePnlAssets, data.maePnlAssets, mfeCompact, maeCompact),
      },
      {
        label: data.status === 'open' ? 'Liq buffer' : 'Closest liq',
        value: data.liqBufferLabel,
        accent: data.status === 'liquidated' ? '#ff572d' : undefined,
      },
    ]
    const pnlFontSize = netPnl.text.length <= 14 ? 72 : netPnl.text.length <= 18 ? 58 : 48
    const sidePillText = `${statusUpper} ${sideUpper}`
    const body = `
      ${renderPill(60, 134, data.market, '#3b212d', '#ffab9666', '#fff5f9')}
      ${renderPill(218, 134, sidePillText, `${sideFg}22`, `${sideFg}88`, sideFg)}
      <text x="60" y="220" fill="#aa98a0" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="16" font-weight="600" letter-spacing="1.4">${data.status === 'open' ? 'NET PNL (INCL. UNREALIZED)' : 'POSITION LIFECYCLE NET PNL'}</text>
      <text x="60" y="304" fill="${color}" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="${String(pnlFontSize)}" font-weight="800">${escapeXml(netPnl.text)}</text>
      <text x="60" y="356" fill="#d8cbd0" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="22" font-weight="600">${escapeXml(subParts.join(' · '))}</text>
      ${renderMetricRibbon(stats, 412)}
    `
    return renderFrame(
      color,
      'Position Lifecycle Audit',
      data.networkName,
      data.releaseVersion,
      data.traderLabel,
      data.account,
      `${data.positionId} · insights.plether.com`,
      body,
    )
  }

  if (data.kind === 'division') {
    const tierPalette = resolveTierPalette(data.tier)
    const rankHeadline = data.rank !== null ? `RANK #${String(data.rank)}` : 'ACTIVE TRADER'
    const returnColor = data.weeklyReturnLabel.startsWith('-') || data.weeklyReturnLabel.startsWith('−')
      ? '#ff572d'
      : '#00ff99'
    const pnlText = data.weeklyPnlAssets ? formatSignedUsdg(data.weeklyPnlAssets).text : null
    const qualified = data.activeDays >= 1
    const thirdCol: RibbonItem = data.tier
      ? !data.provisional
        ? {
            label: 'Result audit',
            value: data.rank !== null ? 'Verified & ranked' : 'Audited',
            accent: '#00ff99',
            dotColor: '#00ff99',
          }
        : {
            label: 'Qualification',
            value: qualified ? 'Qualified' : 'In progress',
            accent: qualified ? '#00ff99' : '#ffab96',
            dotColor: qualified ? '#00ff99' : '#ffab96',
          }
      : {
          label: 'Division badge',
          value: 'Pending Monday',
          accent: tierPalette.primary,
        }
    const stats: RibbonItem[] = [
      {
        label: 'Active FX sessions',
        value: `${String(data.activeDays)} / 5`,
        renderVisual: renderFxSessionPips(data.activeDays, qualified ? '#00ff99' : '#ffab96'),
      },
      pnlText
        ? { label: 'Weekly PnL', value: pnlText, accent: returnColor }
        : { label: 'Scoring cutoff', value: 'Fri 21:00 UTC' },
      thirdCol,
    ]
    const badgeTag = data.movement && data.movement !== 'unchanged'
      ? data.movement.toUpperCase()
      : data.provisional
        ? 'LIVE WEEK'
        : 'PUBLISHED'
    const body = `
      ${renderPill(60, 134, data.weekLabel, '#3b212d', '#ffab9666', '#fff5f9')}
      ${renderPill(60 + pillWidth(data.weekLabel) + 14, 134, badgeTag, `${tierPalette.primary}22`, `${tierPalette.primary}88`, tierPalette.primary)}
      <text x="60" y="226" fill="#aa98a0" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="16" font-weight="600" letter-spacing="1.4">PLETHER DIVISIONS</text>
      <text x="60" y="308" fill="#fff5f9" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="74" font-weight="800">${escapeXml(rankHeadline)}</text>
      <text x="60" y="358" fill="${returnColor}" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="28" font-weight="700">${escapeXml(data.weeklyReturnLabel)} weekly return</text>
      ${renderTierBadge(data.tier, 990, 244)}
      ${renderMetricRibbon(stats, 412)}
    `
    const eyebrow = data.tier ? 'Plether Divisions Badge' : 'Plether Divisions Live Standing'
    return renderFrame(tierPalette.glow, eyebrow, data.networkName, undefined, data.traderLabel, data.account, 'insights.plether.com/divisions', body)
  }

  if (data.kind === 'liquidity_efficiency') {
    const parseAmt = (raw: string | null | undefined): number => {
      if (!raw || !/^-?\d+$/.test(raw)) return 0
      const n = Number(raw) / 1_000_000
      return Number.isFinite(n) ? n : 0
    }
    const formatTurnover = (x100: number | null | undefined): string => {
      if (x100 == null || !Number.isFinite(x100)) return '—'
      const x = x100 / 100
      if (x >= 100) return `${x.toFixed(0)}×`
      if (x >= 10) return `${x.toFixed(1)}×`
      return `${x.toFixed(2)}×`
    }
    const formatBpsPct = (bps: number | null | undefined): string =>
      bps != null && Number.isFinite(bps) ? `${(bps / 100).toFixed(2)}%` : '—'

    const primaryIs24h = data.turnover24hX100 != null
    const heroRatio = formatTurnover(primaryIs24h ? data.turnover24hX100 : data.turnover7dAvgDailyX100)
    const heroLabel = primaryIs24h ? '24H VOLUME / POOL' : '7D AVG DAILY VOL / POOL'
    const avg7dText = data.turnover7dAvgDailyX100 != null ? `${formatTurnover(data.turnover7dAvgDailyX100)} / d` : '—'
    const takeRateText =
      data.effectiveTakeRate7dBpsX100 != null
        ? `${(data.effectiveTakeRate7dBpsX100 / 100).toFixed(2)} bps`
        : '—'
    const yieldText =
      data.annualizedFeeYield7dBps != null
        ? `${(data.annualizedFeeYield7dBps / 100).toFixed(2)}% APR`
        : '—'

    const grossOiPct = data.grossOiToPoolBps != null ? data.grossOiToPoolBps / 100 : null
    const oiBadgeTone =
      grossOiPct == null
        ? { bg: '#3b212d', border: '#ffab9666', fg: '#d8cbd0' }
        : grossOiPct > 150
          ? { bg: '#ff572d22', border: '#ff572d99', fg: '#ff572d' }
          : grossOiPct > 100
            ? { bg: '#f2cf7122', border: '#f2cf7199', fg: '#f2cf71' }
            : { bg: '#00ff991f', border: '#00ff9988', fg: '#00ff99' }
    const oiBadgeLabel =
      grossOiPct != null ? `GROSS OI ${grossOiPct.toFixed(1)}% OF POOL` : 'OPEN INTEREST LIVE'

    const longNum = parseAmt(data.currentLongOiAssets)
    const shortNum = parseAmt(data.currentShortOiAssets)
    const grossNum = Math.max(0, longNum + shortNum)
    const longShare = grossNum > 0 ? Math.max(0.04, Math.min(0.96, longNum / grossNum)) : 0.5
    const netSkewNum = parseAmt(data.currentNetSkewAssets)
    const skewDir = netSkewNum > 0 ? 'Long' : netSkewNum < 0 ? 'Short' : 'Balanced'
    const skewLabel =
      data.netSkewToPoolBps != null
        ? `${netSkewNum >= 0 ? '+' : '−'}${(data.netSkewToPoolBps / 100).toFixed(2)}% ${skewDir}`
        : skewDir

    const senNum = parseAmt(data.seniorTvlAssets)
    const junNum = parseAmt(data.juniorTvlAssets)
    const tvlSum = senNum + junNum
    const senShare = tvlSum > 0 ? Math.max(0.06, Math.min(0.94, senNum / tvlSum)) : 0.5
    const poolTvlText = data.poolTvlAssets ? formatCompactAssets(data.poolTvlAssets) : '—'

    // Right visual panel: 7d turnover bars + Open Interest Long/Short split
    const boxX = 540
    const boxY = 126
    const boxW = 600
    const boxH = 256
    const plotX = boxX + 24
    const plotY = boxY + 46
    const plotW = boxW - 48
    const plotH = 104

    const rawSeries = (data.turnoverSeriesX100 ?? []).map((v) => (v != null && Number.isFinite(v) && v >= 0 ? v / 100 : 0))
    const bars = rawSeries.length > 0 ? rawSeries.slice(-28) : [0.4, 0.8, 1.1, 0.9, 1.4, 1.2, 0.7]
    const peakTurn = Math.max(1, ...bars)
    const refOneY = plotY + plotH - (1 / (peakTurn * 1.12)) * plotH
    const stepX = plotW / bars.length
    const barW = Math.max(4, Math.min(18, stepX * 0.62))
    const barsSvg = bars
      .map((val, idx) => {
        const h = Math.max(2, (val / (peakTurn * 1.12)) * plotH)
        const bx = plotX + idx * stepX + (stepX - barW) / 2
        const by = plotY + plotH - h
        const isLast = idx === bars.length - 1
        return `<rect x="${bx.toFixed(1)}" y="${by.toFixed(1)}" width="${barW.toFixed(1)}" height="${h.toFixed(1)}" fill="${isLast ? '#ff572d' : '#ffab96'}" fill-opacity="${isLast ? '0.95' : '0.62'}" />`
      })
      .join('')

    const oiBarY = boxY + 206
    const oiLongW = Math.round(plotW * longShare)
    const oiShortW = Math.max(0, plotW - oiLongW)
    const oiHeaderRight = `${formatCompactAssets(data.currentGrossOiAssets)} · ${skewLabel}`

    const visualPanelSvg = `<g>
      <rect x="${String(boxX)}" y="${String(boxY)}" width="${String(boxW)}" height="${String(boxH)}" fill="#3b212d" fill-opacity="0.46" stroke="#ffab96" stroke-opacity="0.24" stroke-width="1" />
      <text x="${String(plotX)}" y="${String(boxY + 28)}" fill="#aa98a0" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="12" font-weight="700" letter-spacing="1.3">DAILY TURNOVER VELOCITY (VOL / POOL)</text>
      <text x="${String(plotX + plotW)}" y="${String(boxY + 28)}" text-anchor="end" fill="#ffab96" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="12" font-weight="700">Peak ${escapeXml(peakTurn.toFixed(2))}×</text>
      <line x1="${String(plotX)}" y1="${refOneY.toFixed(1)}" x2="${String(plotX + plotW)}" y2="${refOneY.toFixed(1)}" stroke="#f2cf71" stroke-opacity="0.45" stroke-width="1" stroke-dasharray="4 4" />
      <text x="${String(plotX + plotW)}" y="${(refOneY - 4).toFixed(1)}" text-anchor="end" fill="#f2cf71" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="10" font-weight="700">1.0×/d</text>
      <line x1="${String(plotX)}" y1="${String(plotY + plotH)}" x2="${String(plotX + plotW)}" y2="${String(plotY + plotH)}" stroke="#ffab96" stroke-opacity="0.2" stroke-width="1" />
      ${barsSvg}
      <line x1="${String(plotX)}" y1="${String(boxY + 170)}" x2="${String(plotX + plotW)}" y2="${String(boxY + 170)}" stroke="#ffab96" stroke-opacity="0.16" stroke-width="1" />
      <text x="${String(plotX)}" y="${String(boxY + 194)}" fill="#aa98a0" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="12" font-weight="700" letter-spacing="1.2">OPEN INTEREST SPLIT</text>
      <text x="${String(plotX + plotW)}" y="${String(boxY + 194)}" text-anchor="end" fill="#fff5f9" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="12" font-weight="700">${escapeXml(oiHeaderRight)}</text>
      <rect x="${String(plotX)}" y="${String(oiBarY)}" width="${String(plotW)}" height="12" fill="#250917" stroke="#ffab96" stroke-opacity="0.22" stroke-width="0.8" />
      <rect x="${String(plotX)}" y="${String(oiBarY)}" width="${String(oiLongW)}" height="12" fill="#00ff99" fill-opacity="0.85" />
      <rect x="${String(plotX + oiLongW)}" y="${String(oiBarY)}" width="${String(oiShortW)}" height="12" fill="#ff572d" fill-opacity="0.85" />
      <text x="${String(plotX)}" y="${String(oiBarY + 30)}" fill="#00ff99" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="12" font-weight="700">Long ${escapeXml(formatCompactAssets(data.currentLongOiAssets))} (${String(Math.round(longShare * 100))}%)</text>
      <text x="${String(plotX + plotW)}" y="${String(oiBarY + 30)}" text-anchor="end" fill="#ff572d" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="12" font-weight="700">Short ${escapeXml(formatCompactAssets(data.currentShortOiAssets))} (${String(100 - Math.round(longShare * 100))}%)</text>
    </g>`

    const renderPoolTvlVisual = (x: number, y: number, w: number): string => {
      const barW = Math.max(80, w)
      const senW = Math.round(barW * senShare)
      const junW = Math.max(0, barW - senW)
      const fontSize = fitStatFontSize(poolTvlText, w + 48, 20)
      return `
        <text x="${String(x)}" y="${String(y + 16)}" fill="#fff5f9" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="${String(fontSize)}" font-weight="700">${escapeXml(poolTvlText)}</text>
        <rect x="${String(x)}" y="${String(y + 27)}" width="${String(barW)}" height="5" fill="#250917" />
        <rect x="${String(x)}" y="${String(y + 27)}" width="${String(senW)}" height="5" fill="#f2cf71" fill-opacity="0.85" />
        <rect x="${String(x + senW)}" y="${String(y + 27)}" width="${String(junW)}" height="5" fill="#ffab96" fill-opacity="0.85" />
      `
    }

    const stats: RibbonItem[] = [
      { label: 'Pool TVL (Sr / Jr)', value: poolTvlText, renderVisual: renderPoolTvlVisual },
      { label: '7d avg Vol / Pool', value: avg7dText, accent: '#ffab96' },
      { label: '7d fee yield', value: yieldText, accent: '#00ff99' },
      { label: '7d take rate', value: takeRateText },
    ]

    const subline = `24h Vol ${formatCompactAssets(data.volume24hAssets)} · 7d Vol ${formatCompactAssets(data.volume7dAssets)}`
    const body = `
      ${renderPill(60, 132, 'LP POOL EFFICIENCY', '#3b212d', '#ffab9666', '#fff5f9')}
      ${renderPill(60 + pillWidth('LP POOL EFFICIENCY') + 12, 132, oiBadgeLabel, oiBadgeTone.bg, oiBadgeTone.border, oiBadgeTone.fg)}
      <text x="60" y="218" fill="#aa98a0" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="15" font-weight="600" letter-spacing="1.4">${heroLabel}</text>
      <text x="60" y="302" fill="#fff5f9" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="78" font-weight="800">${escapeXml(heroRatio)}</text>
      <text x="60" y="346" fill="#d8cbd0" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="16" font-weight="500">${escapeXml(subline)}</text>
      <text x="60" y="372" fill="#aa98a0" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="13">OI / Pool ${escapeXml(formatBpsPct(data.grossOiToPoolBps))} · Net skew ${escapeXml(skewLabel)}</text>
      ${visualPanelSvg}
      ${renderMetricRibbon(stats, 412)}
    `
    return renderFrame(
      '#ff572d',
      'LP Pool Capital Efficiency',
      data.networkName,
      data.releaseVersion,
      'Plether LP House Pool',
      '',
      'insights.plether.com/liquidity',
      body,
      'Senior & Junior Vaults',
    )
  }

  // Account overview card (distinct from Divisions card — centers life-long balance curve)
  const pnl = data.realizedPnlAssets ? formatSignedUsdg(data.realizedPnlAssets) : { text: 'Unavailable', tone: 'neutral' as const }
  const pnlColor = toneHex(pnl.tone)
  const chart = renderLifetimeBalanceChart(data.balancePoints, data.sparklineValues)
  const glowAccent = pnl.tone !== 'neutral' ? pnlColor : chart.accent
  const effectiveBalance =
    data.accountBalanceAssets ??
    (data.netEquityAssets && data.netEquityAssets !== '0' ? data.netEquityAssets : null) ??
    data.balancePoints?.[data.balancePoints.length - 1]?.balanceAssets ??
    data.netEquityAssets ??
    null
  const stats: RibbonItem[] = [
    { label: 'Balance', value: effectiveBalance ? formatCompactAssets(effectiveBalance) : 'Unavailable' },
    { label: 'Volume', value: data.volumeAssets ? formatCompactAssets(data.volumeAssets) : 'Unavailable' },
    { label: 'Executed fills', value: data.totalTrades != null ? data.totalTrades.toLocaleString('en-US') : 'Unavailable' },
    { label: 'Active days', value: data.activeDays != null ? String(data.activeDays) : 'Unavailable' },
  ]
  const accountTierPalette = resolveTierPalette(data.tier)
  const pnlFontSize = pnl.text.length <= 13 ? 48 : pnl.text.length <= 16 ? 42 : 36
  const sinceText = formatMonthDayYear(data.firstActivityAt) ?? chart.startLabel
  const body = `
    ${renderPill(60, 132, 'TRADING ACCOUNT', '#3b212d', '#ffab9666', '#fff5f9')}
    ${data.tier ? renderPill(264, 132, `${data.tier.toUpperCase()} TIER`, `${accountTierPalette.primary}22`, `${accountTierPalette.primary}88`, accountTierPalette.primary) : ''}
    <text x="60" y="216" fill="#aa98a0" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="15" font-weight="600" letter-spacing="1.4">ALL-TIME REALIZED PNL</text>
    <text x="60" y="282" fill="${pnlColor}" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="${String(pnlFontSize)}" font-weight="800">${escapeXml(pnl.text)}</text>
    <text x="60" y="330" fill="#d8cbd0" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="17" font-weight="500">${escapeXml(sinceText ? `Life-long history since ${sinceText}` : 'Life-long on-chain perps performance')}</text>
    ${chart.svg}
    ${renderMetricRibbon(stats, 412)}
  `
  return renderFrame(glowAccent, 'Trading Account Summary', data.networkName, data.releaseVersion, data.traderLabel, data.account, 'insights.plether.com/wallets', body)
}

export function svgToDataUrl(svg: string): string {
  return `data:image/svg+xml;charset=utf-8,${encodeURIComponent(svg)}`
}

export function shareCardFilename(data: ShareCardData): string {
  if (data.kind === 'liquidity_efficiency') {
    return 'plether-liquidity-efficiency.png'
  }
  const safeTrader = data.traderLabel.replace(/[^a-zA-Z0-9_-]/g, '').toLowerCase() || shortAddress(data.account)
  return `plether-${data.kind}-${safeTrader}.png`
}

export function buildShareTweetUrl(data: ShareCardData, pageUrl: string): string {
  let headline: string
  if (data.kind === 'liquidity_efficiency') {
    const turn24 = data.turnover24hX100 != null ? `${(data.turnover24hX100 / 100).toFixed(2)}x` : null
    const turn7 = data.turnover7dAvgDailyX100 != null ? `${(data.turnover7dAvgDailyX100 / 100).toFixed(2)}x/d` : null
    const oiPct = data.grossOiToPoolBps != null ? `${(data.grossOiToPoolBps / 100).toFixed(1)}%` : null
    const apr = data.annualizedFeeYield7dBps != null ? `${(data.annualizedFeeYield7dBps / 100).toFixed(1)}% APR` : null
    const parts = [
      turn24 ? `${turn24} 24h Vol/Pool` : null,
      turn7 ? `${turn7} 7d avg` : null,
      oiPct ? `${oiPct} OI/Pool` : null,
      apr ? `${apr} 7d fee yield` : null,
    ].filter((p): p is string => Boolean(p))
    headline =
      parts.length > 0
        ? `Plether LP Pool Capital Efficiency: ${parts.join(' · ')} on Plether Insights.`
        : 'Live LP Pool Capital Efficiency & Open Interest on Plether Insights.'
  } else if (data.kind === 'trade') {
    const pnl = formatSignedUsdg(data.realizedPnlAssets).text
    headline = `Closed ${data.side ? data.side.toUpperCase() + ' ' : ''}${data.market} on Plether: ${pnl} realized PnL.`
  } else if (data.kind === 'open_trade') {
    const notionalPart = data.notionalAssets ? ` (${formatCompactAssets(data.notionalAssets)})` : ''
    const pricePart = data.price ? ` @ ${formatPrice(data.price)}` : ''
    headline = `Opened ${data.side ? data.side.toUpperCase() + ' ' : ''}${data.market}${notionalPart}${pricePart} on Plether.`
  } else if (data.kind === 'position') {
    const upnl = formatSignedUsdg(data.unrealizedPnlAssets).text
    headline = `Live ${data.side.toUpperCase()} ${data.market} position on Plether: ${upnl} unrealized PnL.`
  } else if (data.kind === 'position_lifecycle') {
    const netPnl = formatSignedUsdg(data.netPnlAssets).text
    const stateLabel =
      data.status === 'open' ? 'Live' : data.status === 'liquidated' ? 'Liquidated' : 'Closed'
    headline = `${stateLabel} ${data.side.toUpperCase()} ${data.market} lifecycle on Plether: ${netPnl} net PnL (${data.maxLeverageLabel} max lev, held ${data.durationLabel}).`
  } else if (data.kind === 'division') {
    const tierPart = data.tier ? `${data.tier} Division` : 'Plether Divisions'
    const rankPart = data.rank !== null ? `Rank #${String(data.rank)} ` : ''
    headline = `${rankPart}in ${tierPart} (${data.weeklyReturnLabel} weekly return) on Plether Insights.`
  } else {
    const pnl = data.realizedPnlAssets ? formatSignedUsdg(data.realizedPnlAssets).text : null
    headline = pnl
      ? `Verified on-chain Plether Perps performance: ${pnl} realized PnL.`
      : 'Verified on-chain Plether Trading Account on Plether Insights.'
  }
  const params = new URLSearchParams({
    text: headline,
    url: pageUrl,
  })
  return `https://x.com/intent/tweet?${params.toString()}`
}

export async function renderShareCardPng(svg: string, scale = 2): Promise<Blob> {
  const width = SHARE_CARD_WIDTH * scale
  const height = SHARE_CARD_HEIGHT * scale
  const dataUrl = svgToDataUrl(svg)

  return new Promise<Blob>((resolve, reject) => {
    const img = new Image()
    img.onload = () => {
      const canvas = document.createElement('canvas')
      canvas.width = width
      canvas.height = height
      const ctx = canvas.getContext('2d')
      if (!ctx) {
        reject(new Error('Canvas 2D context unavailable'))
        return
      }
      ctx.drawImage(img, 0, 0, width, height)
      canvas.toBlob(
        (blob) => {
          if (blob) resolve(blob)
          else reject(new Error('Failed to encode PNG image'))
        },
        'image/png',
        1,
      )
    }
    img.onerror = () => {
      reject(new Error('Failed to load share card graphic'))
    }
    img.src = dataUrl
  })
}
