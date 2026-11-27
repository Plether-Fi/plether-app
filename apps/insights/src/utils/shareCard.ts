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

export type ShareCardData =
  | TradeShareCardData
  | PositionShareCardData
  | DivisionShareCardData
  | AccountShareCardData

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

function fitStatFontSize(value: string, boxW: number, maxSize: number): number {
  const usableW = Math.max(80, boxW - 40)
  const charCount = Math.max(1, value.length)
  const fitted = Math.floor(usableW / (charCount * 0.64))
  return Math.min(maxSize, Math.max(14, fitted))
}

function renderStatBoxes(stats: { label: string; value: string; accent?: string }[], y: number): string {
  const gap = 18
  const totalW = 1080
  const count = stats.length
  const boxW = Math.floor((totalW - gap * (count - 1)) / count)
  return stats
    .map((stat, idx) => {
      const x = 60 + idx * (boxW + gap)
      const fontSize = fitStatFontSize(stat.value, boxW, 24)
      return `<g>
        <rect x="${String(x)}" y="${String(y)}" width="${String(boxW)}" height="92" fill="#3b212d" fill-opacity="0.72" stroke="#ffab96" stroke-opacity="0.28" stroke-width="1" />
        <text x="${String(x + 20)}" y="${String(y + 30)}" fill="#aa98a0" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="13" font-weight="600" letter-spacing="1.1">${escapeXml(stat.label.toUpperCase())}</text>
        <text x="${String(x + 20)}" y="${String(y + 66)}" fill="${stat.accent ?? '#fff5f9'}" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="${String(fontSize)}" font-weight="700">${escapeXml(stat.value)}</text>
      </g>`
    })
    .join('')
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
): string {
  const networkTag = releaseVersion ? `${networkName} · ${releaseVersion}` : networkName
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
  <g transform="translate(60, 558)">${buildAddressAvatarInnerSvg(account)}</g>
  <text x="96" y="576" fill="#fff5f9" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="18" font-weight="700">${escapeXml(traderLabel)}</text>
  <text x="${String(96 + Math.min(400, traderLabel.length * 11 + 24))}" y="576" fill="#aa98a0" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="15">${escapeXml(shortAddress(account))}</text>
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
    const stats = [
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
      ${renderStatBoxes(stats, 408)}
    `
    const footerRight = data.txHash
      ? `tx ${shortAddress(data.txHash)} · insights.plether.com`
      : 'insights.plether.com'
    return renderFrame(color, 'Closed Position Receipt', data.networkName, data.releaseVersion, data.traderLabel, data.account, footerRight, body)
  }

  if (data.kind === 'position') {
    const upnl = formatSignedUsdg(data.unrealizedPnlAssets)
    const color = data.liquidatable ? '#ff572d' : toneHex(upnl.tone)
    const roe = computeRoePercent(data.unrealizedPnlAssets, data.marginAssets)
    const sideUpper = data.side.toUpperCase()
    const sideFg = data.side === 'short' ? '#ff572d' : '#00ff99'
    const stats = [
      { label: 'Position size', value: formatPlDxyQuantity(data.size) },
      { label: 'Entry price', value: formatPrice(data.entryPrice) },
      { label: 'Margin posted', value: formatCompactAssets(data.marginAssets) },
      { label: 'Status', value: data.liquidatable ? 'Liquidatable' : 'Healthy', accent: data.liquidatable ? '#ff572d' : '#00ff99' },
    ]
    const body = `
      ${renderPill(60, 134, data.market, '#3b212d', '#ffab9666', '#fff5f9')}
      ${renderPill(218, 134, `OPEN ${sideUpper}`, `${sideFg}22`, `${sideFg}88`, sideFg)}
      <text x="60" y="220" fill="#aa98a0" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="16" font-weight="600" letter-spacing="1.4">UNREALIZED PNL</text>
      <text x="60" y="308" fill="${color}" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="76" font-weight="800">${escapeXml(upnl.text)}</text>
      ${roe ? `<text x="60" y="358" fill="${color}" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="26" font-weight="700">${escapeXml(roe)} ROE</text>` : ''}
      ${renderStatBoxes(stats, 408)}
    `
    return renderFrame(color, 'Live Perpetual Position', data.networkName, data.releaseVersion, data.traderLabel, data.account, 'CfdEngineAccountLens · insights.plether.com', body)
  }

  if (data.kind === 'division') {
    const tierPalette = resolveTierPalette(data.tier)
    const rankHeadline = data.rank !== null ? `RANK #${String(data.rank)}` : 'ACTIVE TRADER'
    const returnColor = data.weeklyReturnLabel.startsWith('-') || data.weeklyReturnLabel.startsWith('−')
      ? '#ff572d'
      : '#00ff99'
    const pnlText = data.weeklyPnlAssets ? formatSignedUsdg(data.weeklyPnlAssets).text : null
    const stats = [
      pnlText
        ? { label: 'Weekly PnL', value: pnlText, accent: returnColor }
        : { label: 'Status', value: data.provisional ? 'Provisional' : 'Published' },
      { label: 'Active days', value: `${String(data.activeDays)} / 5` },
      { label: 'Tier standing', value: data.tier ?? 'Not yet awarded', accent: tierPalette.primary },
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
      ${renderStatBoxes(stats, 408)}
    `
    const eyebrow = data.tier ? 'Plether Divisions Badge' : 'Plether Divisions Live Standing'
    return renderFrame(tierPalette.glow, eyebrow, data.networkName, undefined, data.traderLabel, data.account, 'insights.plether.com/divisions', body)
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
  const stats = [
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
    <text x="60" y="328" fill="#d8cbd0" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif" font-size="17" font-weight="500">${escapeXml(sinceText ? `Life-long history since ${sinceText}` : 'Life-long on-chain perps performance')}</text>
    ${effectiveBalance ? `<text x="60" y="362" fill="#aa98a0" font-family="ui-monospace, SFMono-Regular, Menlo, monospace" font-size="14" font-weight="600">Current balance · ${escapeXml(formatCompactAssets(effectiveBalance))}</text>` : ''}
    ${chart.svg}
    ${renderStatBoxes(stats, 412)}
  `
  return renderFrame(glowAccent, 'Trading Account Summary', data.networkName, data.releaseVersion, data.traderLabel, data.account, 'insights.plether.com/wallets', body)
}

export function svgToDataUrl(svg: string): string {
  return `data:image/svg+xml;charset=utf-8,${encodeURIComponent(svg)}`
}

export function shareCardFilename(data: ShareCardData): string {
  const safeTrader = data.traderLabel.replace(/[^a-zA-Z0-9_-]/g, '').toLowerCase() || shortAddress(data.account)
  return `plether-${data.kind}-${safeTrader}.png`
}

export function buildShareTweetUrl(data: ShareCardData, pageUrl: string): string {
  let headline: string
  if (data.kind === 'trade') {
    const pnl = formatSignedUsdg(data.realizedPnlAssets).text
    headline = `Closed ${data.side ? data.side.toUpperCase() + ' ' : ''}${data.market} on Plether: ${pnl} realized PnL.`
  } else if (data.kind === 'position') {
    const upnl = formatSignedUsdg(data.unrealizedPnlAssets).text
    headline = `Live ${data.side.toUpperCase()} ${data.market} position on Plether: ${upnl} unrealized PnL.`
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
