export function formatNativeWei(value: string | null | undefined): string {
  if (value === null || value === undefined || !/^\d+$/.test(value)) return 'Unavailable'
  const wei = BigInt(value)
  const scale = 10n ** 18n
  if (wei > 0n && wei < scale / 1_000_000n) return '<0.000001 ETH'
  const whole = wei / scale
  const fraction = ((wei % scale) * 1_000_000n / scale).toString().padStart(6, '0')
  return `${whole.toString()}.${fraction} ETH`
}

export function isOracleUpdaterRole(role: string): boolean {
  const normalized = role.toLowerCase().replaceAll('-', '_').replaceAll(' ', '_')
  return normalized.includes('oracle') && normalized.includes('updat')
}

export function walletStatusTone(
  status: string,
): 'positive' | 'warning' | 'critical' | 'default' {
  switch (status.toLowerCase()) {
    case 'healthy':
      return 'positive'
    case 'warning':
    case 'no_cost_baseline':
      return 'warning'
    case 'critical':
    case 'depleted':
      return 'critical'
    default:
      return 'default'
  }
}

export function updaterTelemetryIsUnattributable(field: string, reason: string): boolean {
  const normalized = `${field} ${reason}`.toLowerCase()
  return normalized.includes('oracle')
    && normalized.includes('updater')
    && (
      normalized.includes('unattribut')
      || normalized.includes('not_attribut')
      || (
        normalized.includes('unavailable')
        && (normalized.includes('activity') || normalized.includes('outlay'))
      )
    )
}
