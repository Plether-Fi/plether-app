import { Link } from 'react-router-dom'
import { shortAddress } from '../utils/format'
import { CopyButton } from './ActionButtons'
import { AddressAvatar } from './AddressAvatar'

export interface TraderIdentityProps {
  address: string
  discordUsername?: string | null
  to?: string
  avatarSize?: number
  showCopy?: boolean
  className?: string
}

/**
 * Unified trader identity cell: deterministic avatar + @discord handle (when
 * linked) + truncated 0x address + optional fixed-slot hover-expanding copy button.
 */
export function TraderIdentity({
  address,
  discordUsername,
  to,
  avatarSize = 20,
  showCopy = false,
  className = '',
}: TraderIdentityProps) {
  const href = to ?? `/wallets/${address}`
  const handle = discordUsername?.trim() ? `@${discordUsername.trim().replace(/^@/, '')}` : null

  return (
    <div className={`inline-flex min-w-0 items-center gap-2 ${className}`}>
      <AddressAvatar address={address} size={avatarSize} />
      <div className="min-w-0 leading-tight">
        <Link
          to={href}
          title={address}
          className={`block truncate hover:underline focus-visible:underline ${
            handle
              ? 'font-sans text-sm font-semibold text-brand-peach'
              : 'font-mono text-xs text-brand-peach'
          }`}
        >
          {handle ?? shortAddress(address)}
        </Link>
        {handle ? (
          <span className="block truncate font-mono text-[11px] text-content-tertiary">
            {shortAddress(address)}
          </span>
        ) : null}
      </div>
      {showCopy ? <CopyButton value={address} /> : null}
    </div>
  )
}
