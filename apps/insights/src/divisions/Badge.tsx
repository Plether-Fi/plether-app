import type { Tier } from './rules'

export function TierBadge({ tier, size = 'small', showLabel = false }: {
  tier: Tier
  size?: 'small' | 'large'
  showLabel?: boolean
}) {
  const dimension = size === 'large' ? 72 : 32
  return <span className={`inline-flex shrink-0 ${size === 'large' ? 'flex-col items-start gap-2' : 'items-center gap-2'}`} title={tier}>
    <img src={`/divisions/badges/${tier.toLowerCase()}.svg`} alt={showLabel ? '' : tier} width={dimension} height={dimension} className="shrink-0" />
    {showLabel && <span className="font-semibold text-content-primary">{tier}</span>}
  </span>
}
