const USDC_DECIMALS = 6
const ADDRESS_PATTERN = /^0x[a-fA-F0-9]{40}$/

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

export function formatUsdc(value: string | null | undefined, displayDecimals = 2): string {
  if (value == null) return '—'
  const units = parseUnits(value)
  if (units == null) return '—'

  const negative = units < 0n
  const absolute = negative ? -units : units
  const decimals = Math.max(0, Math.min(USDC_DECIMALS, displayDecimals))
  const divisor = 10n ** BigInt(USDC_DECIMALS - decimals)
  const rounded = (absolute + divisor / 2n) / divisor
  const scale = 10n ** BigInt(decimals)
  const whole = rounded / scale
  const fraction = rounded % scale
  const fractionText = decimals > 0 ? `.${fraction.toString().padStart(decimals, '0')}` : ''
  return `${negative ? '-' : ''}${formatWhole(whole)}${fractionText} USDC`
}

export function formatSignedUsdc(value: string | null | undefined): string {
  const units = value == null ? null : parseUnits(value)
  // P&L drives prize eligibility, so never round away meaningful sub-cent units.
  // This prevents 999.999999 USDC from being presented as 1,000.00 USDC.
  const absolute = units == null || units >= 0n ? units : -units
  const displayDecimals = absolute != null && absolute % 10_000n !== 0n ? USDC_DECIMALS : 2
  const formatted = formatUsdc(value, displayDecimals)
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
  if (!threshold) return formatUsdc(value, 0)

  const tenths = (absolute * 10n + threshold.units / 2n) / threshold.units
  const sign = units < 0n ? '-' : ''
  const whole = tenths / 10n
  const decimal = tenths % 10n
  return `${sign}${whole.toString()}${decimal === 0n ? '' : `.${decimal.toString()}`}${threshold.suffix} USDC`
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

export function formatPrice(value: string | null | undefined): string {
  if (!value) return '—'
  const parsed = Number(value)
  if (!Number.isFinite(parsed)) return value
  return parsed.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 5 })
}
