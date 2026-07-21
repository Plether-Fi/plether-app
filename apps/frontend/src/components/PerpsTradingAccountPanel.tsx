import { type ReactNode } from 'react'
import { TokenAmount } from './ui'

export type SponsorshipStatus =
  | 'available'
  | 'checking'
  | 'unavailable'
  | 'rate-limited'
  | 'bundler-rejected'
  | 'user-operation-dropped'

interface PerpsTradingAccountPanelProps {
  ownerWalletAddress: string
  tradingAccountAddress: string
  accountModel: 'same-address' | 'smart-account'
  marginAccountUsdc: string
  sponsorshipStatus?: SponsorshipStatus
  retryAt?: string
  supportedAlternative?: string
}

interface OperationStateCardProps {
  title: string
  stage: string
  message: string
  tone?: 'neutral' | 'pending' | 'success' | 'error'
  identifierLabel?: string
  identifier?: string
  action?: ReactNode
}

function truncateAddress(value: string): string {
  if (value.length <= 18) return value
  return `${value.slice(0, 8)}...${value.slice(-6)}`
}

function sponsorshipCopy(status: SponsorshipStatus): {
  label: string
  message: string
  tone: string
} {
  switch (status) {
    case 'available':
      return {
        label: 'Sponsored',
        message: 'Eligible Trading Account operations can be submitted without owner-wallet native gas.',
        tone: 'border-positive/40 bg-positive/10 text-positive',
      }
    case 'checking':
      return {
        label: 'Checking sponsorship',
        message: 'Plether is evaluating the prepared operation against the active sponsorship policy.',
        tone: 'border-warning/40 bg-warning/10 text-warning',
      }
    case 'unavailable':
      return {
        label: 'Sponsor unavailable',
        message: 'No sponsored operation has been accepted. No Trading Account action was submitted.',
        tone: 'border-brand-orange/40 bg-brand-orange/10 text-brand-peach',
      }
    case 'rate-limited':
      return {
        label: 'Sponsor rate-limited',
        message: 'Wait for the displayed retry time before requesting a newly prepared operation.',
        tone: 'border-brand-orange/40 bg-brand-orange/10 text-brand-peach',
      }
    case 'bundler-rejected':
      return {
        label: 'Bundler rejected',
        message: 'The UserOperation failed bundler simulation or policy checks before onchain submission.',
        tone: 'border-brand-orange/40 bg-brand-orange/10 text-brand-peach',
      }
    case 'user-operation-dropped':
      return {
        label: 'UserOperation dropped',
        message: 'Check for a transaction hash and onchain state before requesting a replacement.',
        tone: 'border-brand-orange/40 bg-brand-orange/10 text-brand-peach',
      }
  }
}

function DetailRow({ label, value }: { label: string; value: ReactNode }) {
  return (
    <div className="grid grid-cols-[minmax(0,1fr)_minmax(0,1.5fr)] gap-4 border-b border-brand-border/15 py-3 last:border-b-0">
      <dt className="text-sm text-content-secondary">{label}</dt>
      <dd className="min-w-0 text-right text-sm font-semibold text-content-primary">{value}</dd>
    </div>
  )
}

export function PerpsTradingAccountPanel({
  ownerWalletAddress,
  tradingAccountAddress,
  accountModel,
  marginAccountUsdc,
  sponsorshipStatus = 'available',
  retryAt,
  supportedAlternative,
}: PerpsTradingAccountPanelProps) {
  const sponsorship = sponsorshipCopy(sponsorshipStatus)

  return (
    <section className="border border-brand-border/30 bg-surface-panel">
      <div className="border-b border-brand-border/20 px-5 py-4">
        <div className="text-xs font-medium uppercase tracking-wide text-content-secondary">Account context</div>
        <h2 className="mt-1 text-xl font-semibold text-content-primary">Plether Trading Account</h2>
      </div>

      <dl className="px-5">
        <DetailRow
          label="Owner Wallet"
          value={<span title={ownerWalletAddress}>{truncateAddress(ownerWalletAddress)}</span>}
        />
        <DetailRow
          label="Trading Account"
          value={<span title={tradingAccountAddress}>{truncateAddress(tradingAccountAddress)}</span>}
        />
        <DetailRow
          label="Account model"
          value={accountModel === 'same-address' ? 'Same-address EIP-7702' : 'Separate smart account'}
        />
        <DetailRow label="Margin Account" value={<TokenAmount amount={marginAccountUsdc} />} />
      </dl>

      {sponsorshipStatus !== 'available' ? (
        <div className="p-5 pt-2">
          <div className={`border px-4 py-3 ${sponsorship.tone}`}>
            <div className="flex items-center justify-between gap-3">
              <span className="font-semibold">{sponsorship.label}</span>
              {retryAt ? <span className="text-xs font-medium">Retry {retryAt}</span> : null}
            </div>
            <p className="mt-1 text-sm leading-5 text-content-secondary">{sponsorship.message}</p>
            {supportedAlternative ? (
              <p className="mt-2 border-t border-current/20 pt-2 text-xs leading-5 text-content-primary">
                Supported alternative: {supportedAlternative}
              </p>
            ) : null}
          </div>
        </div>
      ) : null}
    </section>
  )
}

function operationToneClass(tone: NonNullable<OperationStateCardProps['tone']>): string {
  switch (tone) {
    case 'success':
      return 'border-positive/40 bg-positive/10'
    case 'pending':
      return 'border-warning/40 bg-warning/10'
    case 'error':
      return 'border-brand-orange/40 bg-brand-orange/10'
    case 'neutral':
      return 'border-brand-border/30 bg-surface-panel'
  }
}

export function OperationStateCard({
  title,
  stage,
  message,
  tone = 'neutral',
  identifierLabel,
  identifier,
  action,
}: OperationStateCardProps) {
  return (
    <article className={`border p-4 ${operationToneClass(tone)}`}>
      <div className="flex items-start justify-between gap-3">
        <div>
          <div className="text-xs font-medium uppercase tracking-wide text-content-secondary">{stage}</div>
          <h3 className="mt-1 text-base font-semibold text-content-primary">{title}</h3>
        </div>
        <span className={`mt-1 h-2.5 w-2.5 shrink-0 rounded-full ${
          tone === 'success'
            ? 'bg-positive'
            : tone === 'pending'
              ? 'bg-warning'
              : tone === 'error'
                ? 'bg-brand-orange'
                : 'bg-content-secondary'
        }`} />
      </div>
      <p className="mt-3 text-sm leading-5 text-content-secondary">{message}</p>
      {identifierLabel && identifier ? (
        <div className="mt-3 border-t border-brand-border/20 pt-3">
          <div className="text-xs text-content-secondary">{identifierLabel}</div>
          <div className="mt-1 break-all font-mono text-xs text-content-primary">{identifier}</div>
        </div>
      ) : null}
      {action ? <div className="mt-4">{action}</div> : null}
    </article>
  )
}
