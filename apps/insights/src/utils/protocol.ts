export function readRecord(value: unknown): Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value)
    ? value as Record<string, unknown>
    : {}
}

export function readString(value: unknown): string | null {
  return typeof value === 'string' || typeof value === 'number' ? String(value) : null
}

export function displayText(value: unknown, fallback = 'Unavailable'): string {
  if (typeof value === 'string') return value
  if (typeof value === 'number' || typeof value === 'bigint' || typeof value === 'boolean') return String(value)
  return fallback
}

export function humanize(value: string): string {
  return value
    .replaceAll('_', ' ')
    .replace(/([a-z])([A-Z])/g, '$1 $2')
    .replace(/\b\w/g, (letter) => letter.toUpperCase())
}

export function dedupeBy<T>(values: T[], key: (value: T) => string): T[] {
  const seen = new Set<string>()
  return values.filter((value) => {
    const identity = key(value)
    if (seen.has(identity)) return false
    seen.add(identity)
    return true
  })
}

export function formatTimestamp(timestamp: number | string | null | undefined): string {
  if (timestamp === null || timestamp === undefined) return 'Unavailable'
  const numeric = Number(timestamp)
  if (!Number.isFinite(numeric) || numeric <= 0) return 'Unavailable'
  return new Intl.DateTimeFormat('en-GB', {
    day: '2-digit',
    month: 'short',
    year: 'numeric',
    hour: '2-digit',
    minute: '2-digit',
    second: '2-digit',
    timeZone: 'UTC',
    timeZoneName: 'short',
  }).format(new Date(numeric * 1000))
}
