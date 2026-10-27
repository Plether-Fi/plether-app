import { TierBadge } from './Badge'
import type { Tier } from './rules'

export function CurrentBadge({ tier, loading, unavailable }: { tier: Tier | undefined; loading: boolean; unavailable: boolean }) {
  if (tier) return <TierBadge tier={tier} showLabel />
  return <span className="text-xs text-content-tertiary">{loading ? 'Loading badge…' : unavailable ? 'Badge unavailable' : 'Not yet awarded'}</span>
}

