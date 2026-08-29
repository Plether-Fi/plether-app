const USDC_DECIMALS = 6
const ADDRESS_PATTERN = /^0x[a-fA-F0-9]{40}$/
const X_USERNAME_PATTERN = /^[A-Za-z0-9_]{1,15}$/

function parseUnits(value: string): bigint | null {
  if (!/^-?\d+$/.test(value)) return null
  try {
    return BigInt(value)
  } catch {
    return null
  }
}

function formatWhole(value: bigint): string {
  return new Intl.NumberFormat('en-US', { maximumFractionDigits: 0 }).format(value)
}

export function formatUsdc(value: string | null | undefined): string {
  if (value == null) return '—'
  const units = parseUnits(value)
  if (units == null) return '—'

  const negative = units < 0n
  const absolute = negative ? -units : units
  const divisor = 10n ** BigInt(USDC_DECIMALS - 2)
  const rounded = (absolute + divisor / 2n) / divisor
  const scale = 100n
  const whole = rounded / scale
  const fraction = rounded % scale
  const fractionText = `.${fraction.toString().padStart(2, '0')}`
  return `${negative ? '-' : ''}${formatWhole(whole)}${fractionText} USDC`
}

export function formatSignedUsdc(value: string | null | undefined): string {
  const units = value == null ? null : parseUnits(value)
  const formatted = formatUsdc(value)
  if (units == null || units <= 0n) return formatted
  return `+${formatted}`
}

export function formatCompactUsdc(value: string | null | undefined): string {
  if (value == null) return '—'
  const units = parseUnits(value)
  if (units == null) return '—'
  const absolute = units < 0n ? -units : units
  const thresholds = [
    { units: 1_000_000_000_000_000n, suffix: 'B' },
    { units: 1_000_000_000_000n, suffix: 'M' },
    { units: 1_000_000_000n, suffix: 'K' },
  ]
  const threshold = thresholds.find((item) => absolute >= item.units)
  if (!threshold) return formatUsdc(value)

  const hundredths = (absolute * 100n + threshold.units / 2n) / threshold.units
  const sign = units < 0n ? '-' : ''
  const whole = hundredths / 100n
  const fraction = hundredths % 100n
  return `${sign}${whole.toString()}.${fraction.toString().padStart(2, '0')}${threshold.suffix} USDC`
}

export function formatRoi(roiBps: number | null | undefined): string {
  if (roiBps == null || !Number.isFinite(roiBps)) return '—'
  const percent = roiBps / 100
  const sign = percent > 0 ? '+' : ''
  return `${sign}${percent.toFixed(2)}%`
}

export function shortAddress(address: string): string {
  if (address.length < 12) return address
  return `${address.slice(0, 6)}…${address.slice(-4)}`
}

export function isWalletAddress(value: string): boolean {
  return ADDRESS_PATTERN.test(value.trim())
}

export function xProfileUrl(value: string | null | undefined): string | null {
  if (!value) return null
  const username = value.trim().replace(/^@/, '')
  if (!X_USERNAME_PATTERN.test(username)) return null
  return `https://x.com/${username}`
}

export function formatUtc(value: string | null | undefined): string {
  if (!value) return 'Not available'
  const date = new Date(value)
  if (Number.isNaN(date.getTime())) return 'Not available'
  return new Intl.DateTimeFormat('en-GB', {
    day: 'numeric',
    month: 'short',
    year: 'numeric',
    hour: '2-digit',
    minute: '2-digit',
    timeZone: 'UTC',
    timeZoneName: 'short',
  }).format(date)
}

export function formatCountdown(target: string, now: number): string {
  const targetTime = new Date(target).getTime()
  const remaining = Number.isFinite(targetTime) ? Math.max(0, targetTime - now) : 0
  const seconds = Math.floor(remaining / 1_000)
  const days = Math.floor(seconds / 86_400)
  const hours = Math.floor((seconds % 86_400) / 3_600)
  const minutes = Math.floor((seconds % 3_600) / 60)
  const finalSeconds = seconds % 60
  return `${String(days)}d ${hours.toString().padStart(2, '0')}h ${minutes.toString().padStart(2, '0')}m ${finalSeconds.toString().padStart(2, '0')}s`
}

export function formatPrice(value: string | null | undefined): string {
  if (!value) return '—'
  const parsed = Number(value)
  if (!Number.isFinite(parsed)) return value
  return parsed.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 5 })
}
