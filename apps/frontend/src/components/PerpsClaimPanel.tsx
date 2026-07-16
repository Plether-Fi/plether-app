import { type ReactNode, useState } from 'react'
import { Button, Modal, TokenAmount } from './ui'

export type TraderClaimStatus = 'waiting' | 'available' | 'settled'

interface PerpsClaimPanelProps {
  claimUsdc: string
  status: TraderClaimStatus
  tradingAccountAddress: string
  marginAccountUsdc?: string
  initialConfirmationOpen?: boolean
  settledCreditUsdc?: string
}

function truncateAddress(value: string): string {
  if (value.length <= 18) return value
  return `${value.slice(0, 8)}...${value.slice(-6)}`
}

function ClaimRow({ label, value }: { label: string; value: ReactNode }) {
  return (
    <div className="flex items-center justify-between gap-4 border-b border-brand-border/15 py-3 last:border-b-0">
      <dt className="text-sm text-content-secondary">{label}</dt>
      <dd className="text-right text-sm font-semibold text-content-primary">{value}</dd>
    </div>
  )
}

export function PerpsClaimPanel({
  claimUsdc,
  status,
  tradingAccountAddress,
  marginAccountUsdc,
  initialConfirmationOpen = false,
  settledCreditUsdc,
}: PerpsClaimPanelProps) {
  const [isConfirmationOpen, setIsConfirmationOpen] = useState(initialConfirmationOpen)
  const isAvailable = status === 'available'
  const isSettled = status === 'settled'
  const statusLabel = status === 'waiting'
    ? 'Waiting for settlement liquidity'
    : status === 'available'
      ? 'Available to settle'
      : 'Settled'

  return (
    <>
      <section className="border border-brand-border/30 bg-surface-panel">
        <div className="border-b border-brand-border/20 px-5 py-4">
          <div className="text-xs font-medium uppercase tracking-wide text-content-secondary">Margin Account</div>
          <h2 className="mt-1 text-xl font-semibold text-content-primary">Trader claim</h2>
        </div>

        <dl className="px-5">
          <ClaimRow label="Claim owner" value={<span title={tradingAccountAddress}>{truncateAddress(tradingAccountAddress)}</span>} />
          <ClaimRow label="Claim balance" value={<TokenAmount amount={claimUsdc} />} />
          <ClaimRow
            label="Settlement status"
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
          {marginAccountUsdc ? (
            <ClaimRow label="Margin Account" value={<TokenAmount amount={marginAccountUsdc} />} />
          ) : null}
          {settledCreditUsdc ? (
            <ClaimRow
              label="Latest settlement credit"
              value={<span className="text-positive">+<TokenAmount amount={settledCreditUsdc} /></span>}
            />
          ) : null}
        </dl>

        <div className="px-5 pb-5 pt-2">
          {status === 'waiting' ? (
            <p className="border border-brand-orange/30 bg-brand-orange/10 px-4 py-3 text-sm leading-5 text-content-secondary">
              Aggregate HousePool assets do not yet cover all outstanding trader claims. The claim remains recorded but cannot be used as margin or withdrawn.
            </p>
          ) : null}
          {status === 'available' ? (
            <Button className="w-full" onClick={() => { setIsConfirmationOpen(true) }}>
              Settle Claim
            </Button>
          ) : null}
          {status === 'settled' ? (
            <p className="border border-positive/30 bg-positive/10 px-4 py-3 text-sm leading-5 text-content-secondary">
              The complete claim was credited to the Trading Account&apos;s Margin Account. Wallet withdrawal remains a separate sponsored operation.
            </p>
          ) : null}
        </div>
      </section>

      <Modal
        isOpen={isConfirmationOpen}
        onClose={() => { setIsConfirmationOpen(false) }}
        title="Settle Trader Claim"
        size="md"
      >
        <div className="space-y-5">
          <p className="text-sm leading-6 text-content-secondary">
            Your connected owner wallet authorizes settlement for the claim-owning Trading Account. Plether submits the eligible sponsored operation.
          </p>
          <dl className="border border-brand-border/20 bg-app-bg px-4">
            <ClaimRow label="Complete claim" value={<TokenAmount amount={claimUsdc} />} />
            <ClaimRow label="Destination" value="Margin Account" />
            <ClaimRow label="Network gas" value={<span className="text-positive">Sponsored</span>} />
          </dl>
          <div className="grid grid-cols-2 gap-3">
            <Button variant="secondary" onClick={() => { setIsConfirmationOpen(false) }}>
              Cancel
            </Button>
            <Button>Authorize Settlement</Button>
          </div>
        </div>
      </Modal>
    </>
  )
}
