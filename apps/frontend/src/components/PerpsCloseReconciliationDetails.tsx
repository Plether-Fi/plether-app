import type { ReactNode } from 'react'
import { formatPerpsUsdc, formatSignedPerpsUsdc } from '../utils/perps'
import type { PerpsCloseReconciliation } from '../utils/perpsCloseReconciliation'
import { TokenAmount } from './ui'

type AmountTone = 'default' | 'positive' | 'negative' | 'warning' | 'muted'

interface ReconciliationRowProps {
  label: string
  amount: bigint
  signed?: boolean
  tone?: AmountTone
  emphasized?: boolean
}

function amountToneClass(tone: AmountTone): string {
  if (tone === 'positive') return 'text-positive'
  if (tone === 'negative') return 'text-brand-peach'
  if (tone === 'warning') return 'text-warning'
  if (tone === 'muted') return 'text-content-secondary'
  return 'text-content-primary'
}

function signedTone(amount: bigint): AmountTone {
  if (amount > 0n) return 'positive'
  if (amount < 0n) return 'negative'
  return 'muted'
}

function ReconciliationRow({
  label,
  amount,
  signed = true,
  tone = signed ? signedTone(amount) : 'default',
  emphasized = false,
}: ReconciliationRowProps) {
  return (
    <div className={`flex min-h-7 items-start justify-between gap-4 py-1.5 ${
      emphasized ? 'mt-2 border-t border-brand-border/30 pt-3' : ''
    }`}>
      <dt className={emphasized ? 'font-semibold text-content-primary' : 'text-content-secondary'}>
        {label}
      </dt>
      <dd className={`shrink-0 text-right ${emphasized ? 'font-semibold' : ''} ${amountToneClass(tone)}`}>
        <TokenAmount amount={signed ? formatSignedPerpsUsdc(amount) : formatPerpsUsdc(amount)} />
      </dd>
    </div>
  )
}

function ReconciliationSection({
  title,
  children,
}: {
  title: string
  children: ReactNode
}) {
  return (
    <section className="border border-brand-border/20 bg-app-bg p-4">
      <h3 className="mb-2 text-xs font-medium uppercase tracking-wide text-content-secondary">
        {title}
      </h3>
      <dl className="text-sm">{children}</dl>
    </section>
  )
}

export function PerpsCloseReconciliationDetails({
  reconciliation,
}: {
  reconciliation: PerpsCloseReconciliation
}) {
  const vpiEffectUsdc = -reconciliation.vpiUsdc
  const carryEffectUsdc = -reconciliation.carryUsdc
  const executionFeeEffectUsdc = -reconciliation.executionFeeUsdc
  const frozenSpreadEffectUsdc = -reconciliation.frozenSpreadPaidUsdc
  const traderClaimLabel = reconciliation.traderClaimChangeUsdc > 0n
    ? 'Trader claim created'
    : reconciliation.traderClaimChangeUsdc < 0n
      ? 'Trader claim consumed'
      : 'Trader claim change'

  return (
    <div className="space-y-3" data-testid="close-reconciliation">
      <ReconciliationSection title="Close result">
        <ReconciliationRow label="Realized PnL" amount={reconciliation.realizedPnlUsdc} />
        <ReconciliationRow label="Carry" amount={carryEffectUsdc} />
        <ReconciliationRow label="Protocol execution fee" amount={executionFeeEffectUsdc} />
        <ReconciliationRow
          label={reconciliation.vpiUsdc > 0n
            ? 'VPI charge'
            : reconciliation.vpiUsdc < 0n
              ? 'VPI rebate'
              : 'VPI'}
          amount={vpiEffectUsdc}
        />
        <ReconciliationRow label="Frozen spread paid" amount={frozenSpreadEffectUsdc} />
        {reconciliation.frozenSpreadWaivedUsdc > 0n ? (
          <>
            <ReconciliationRow
              label="Frozen spread assessed"
              amount={reconciliation.frozenSpreadAssessedUsdc}
              signed={false}
              tone="muted"
            />
            <ReconciliationRow
              label="Frozen spread waived"
              amount={reconciliation.frozenSpreadWaivedUsdc}
              signed={false}
              tone="muted"
            />
          </>
        ) : null}
        <ReconciliationRow
          label="Net close result"
          amount={reconciliation.netCloseResultUsdc}
          emphasized
        />
      </ReconciliationSection>

      <ReconciliationSection title="Account outcome">
        <ReconciliationRow
          label="Margin Account balance change"
          amount={reconciliation.marginAccountChangeUsdc}
        />
        <ReconciliationRow
          label={traderClaimLabel}
          amount={reconciliation.traderClaimChangeUsdc}
        />
        {reconciliation.releasedPositionMarginUsdc !== undefined ? (
          <ReconciliationRow
            label="Position margin released"
            amount={reconciliation.releasedPositionMarginUsdc}
            signed={false}
          />
        ) : null}
        {reconciliation.postPositionSize > 0n ? (
          <ReconciliationRow
            label="Remaining position margin"
            amount={reconciliation.postPositionMarginUsdc}
            signed={false}
          />
        ) : null}
        {reconciliation.uncoveredLossUsdc > 0n ? (
          <ReconciliationRow
            label="Uncovered loss (bad debt)"
            amount={reconciliation.uncoveredLossUsdc}
            signed={false}
            tone="warning"
          />
        ) : null}
      </ReconciliationSection>

      {reconciliation.releasedPositionMarginUsdc !== undefined ? (
        <p className="text-xs leading-5 text-content-secondary">
          Released margin is existing collateral becoming unlocked; it is not PnL and is not added to the net close result.
        </p>
      ) : null}
    </div>
  )
}
