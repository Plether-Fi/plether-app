import { type ReactNode, useState } from 'react'
import { Button, Modal, TokenAmount } from './ui'

export type TraderClaimStatus = 'waiting' | 'available' | 'settled'

interface PerpsClaimPanelProps {
  claimUsdc: string
  status: TraderClaimStatus
  tradingAccountAddress: string
  marginAccountUsdc?: string
  aggregateClaimsUsdc?: string
  housePoolAssetsUsdc?: string
  coverageRatio?: string
  settlementDestination?: string
  initialConfirmationOpen?: boolean
  settledCreditUsdc?: string
}

function truncateAddress(value: string): string {
  if (value.length <= 18) return value
  return `${value.slice(0, 8)}...${value.slice(-6)}`
}

function ClaimRow({ label, value }: { label: string; value: ReactNode }) {
  return (
    <div className="grid min-w-0 grid-cols-1 gap-1 border-b border-brand-border/15 py-3 last:border-b-0 min-[400px]:grid-cols-[minmax(0,1fr)_auto] min-[400px]:items-center min-[400px]:gap-4">
      <dt className="text-sm text-content-secondary">{label}</dt>
      <dd className="min-w-0 text-sm font-semibold text-content-primary min-[400px]:text-right">{value}</dd>
    </div>
  )
}

export function PerpsClaimPanel({
  claimUsdc,
  status,
  tradingAccountAddress,
  marginAccountUsdc,
  aggregateClaimsUsdc,
  housePoolAssetsUsdc,
  coverageRatio,
  settlementDestination = 'Margin Account',
  initialConfirmationOpen = false,
  settledCreditUsdc,
}: PerpsClaimPanelProps) {
  const [isConfirmationOpen, setIsConfirmationOpen] = useState(initialConfirmationOpen)
  const isAvailable = status === 'available'
  const isSettled = status === 'settled'
  const statusLabel = status === 'waiting'
    ? 'Settlement unavailable'
    : status === 'available'
      ? 'Settlement available'
      : 'Settled'

  return (
    <>
      <section className="border border-brand-border/30 bg-surface-panel">
        <div className="border-b border-brand-border/20 panel-padding-x py-4">
          <div className="text-xs font-medium uppercase tracking-wide text-content-secondary">Margin Account</div>
          <h2 className="mt-1 text-xl font-semibold text-content-primary">Trader claim</h2>
        </div>

        <dl className="panel-padding-x">
          <ClaimRow label="Claim owner" value={<span title={tradingAccountAddress}>{truncateAddress(tradingAccountAddress)}</span>} />
          <ClaimRow label="Account claim" value={<TokenAmount amount={claimUsdc} />} />
          {aggregateClaimsUsdc && housePoolAssetsUsdc ? (
            <ClaimRow
              label="Aggregate claim coverage"
              value={(
                <span className="flex flex-col items-end gap-1">
                  <span className={isAvailable || isSettled ? 'text-positive' : 'text-brand-peach'}>
                    {isAvailable || isSettled ? 'Covered' : 'Under-covered'}
                    {coverageRatio ? ` · ${coverageRatio}` : ''}
                  </span>
                  <span className="text-xs font-normal text-content-secondary">
                    <TokenAmount amount={housePoolAssetsUsdc} /> assets / <TokenAmount amount={aggregateClaimsUsdc} /> claims
                  </span>
                </span>
              )}
            />
          ) : null}
          <ClaimRow
            label="Settlement availability"
            value={(
              <span className={
                isSettled
                  ? 'text-positive'
                  : isAvailable
                    ? 'text-warning'
                    : 'text-brand-peach'
              }>
                {statusLabel}
              </span>
            )}
          />
          <ClaimRow label="Settlement destination" value={settlementDestination} />
          {marginAccountUsdc ? (
            <ClaimRow label="Current Margin Account" value={<TokenAmount amount={marginAccountUsdc} />} />
          ) : null}
          {settledCreditUsdc ? (
            <ClaimRow
              label="Latest settlement credit"
              value={<span className="text-positive">+<TokenAmount amount={settledCreditUsdc} /></span>}
            />
          ) : null}
        </dl>

        <div className="panel-padding-x pb-4 sm:pb-6 pt-2">
          {status === 'waiting' ? (
            <p className="border border-brand-orange/30 bg-brand-orange/10 px-4 py-3 text-sm leading-5 text-content-secondary">
              Aggregate HousePool cash does not yet cover all outstanding trader claims. The claim still supports the same account's position price risk, but cannot be spent as free funds or withdrawn.
            </p>
          ) : null}
          {status === 'available' ? (
            <Button className="w-full" onClick={() => { setIsConfirmationOpen(true) }}>
              Settle claim
            </Button>
          ) : null}
          {status === 'settled' ? (
            <p className="border border-positive/30 bg-positive/10 px-4 py-3 text-sm leading-5 text-content-secondary">
              The complete claim was credited to position margin if a position remained open, or account funds if flat. Wallet withdrawal remains a separate sponsored operation.
            </p>
          ) : null}
        </div>
      </section>

      <Modal
        isOpen={isConfirmationOpen}
        onClose={() => { setIsConfirmationOpen(false) }}
        title="Settle trader claim"
        size="md"
        footer={
          <div className="grid grid-cols-1 gap-3 sm:grid-cols-2">
            <Button variant="secondary" onClick={() => { setIsConfirmationOpen(false) }}>
              Cancel
            </Button>
            <Button>Authorize settlement</Button>
          </div>
        }
      >
        <div className="space-y-5">
          <p className="text-sm leading-6 text-content-secondary">
            Your connected owner wallet authorizes settlement for the claim-owning Trading Account. Settlement converts the claim into position margin while open, or account funds while flat.
          </p>
          <dl className="border border-brand-border/20 bg-app-bg px-4">
            <ClaimRow label="Complete claim" value={<TokenAmount amount={claimUsdc} />} />
            <ClaimRow label="Destination" value={settlementDestination} />
            <ClaimRow label="Network gas" value={<span className="text-positive">Sponsored</span>} />
          </dl>
        </div>
      </Modal>
    </>
  )
}
