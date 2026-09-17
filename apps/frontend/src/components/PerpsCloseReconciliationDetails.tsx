import { type ReactNode, useId, useState } from 'react'
import { formatPerpsUsdc, formatSignedPerpsUsdc } from '../utils/perps'
import type { PerpsCloseReconciliation } from '../utils/perpsCloseReconciliation'
import { TokenAmount } from './ui'

type AmountTone = 'default' | 'positive' | 'negative' | 'muted'

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
  const executionRewardEffectUsdc = -reconciliation.executionBountyUsdc
  const frozenSpreadEffectUsdc = -reconciliation.frozenSpreadChargedUsdc
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
        <ReconciliationRow label="Execution reward" amount={executionRewardEffectUsdc} />
        <ReconciliationRow
          label={reconciliation.vpiUsdc > 0n
            ? 'VPI charge'
            : reconciliation.vpiUsdc < 0n
              ? 'VPI rebate'
              : 'VPI'}
          amount={vpiEffectUsdc}
        />
        <ReconciliationRow label="Frozen spread charged" amount={frozenSpreadEffectUsdc} />
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
          label="Close result before settlement adjustments"
          amount={reconciliation.netCloseResultUsdc}
          emphasized
        />
      </ReconciliationSection>

      {reconciliation.frozenSpreadAssessedUsdc > 0n && reconciliation.vpiUsdc < 0n ? (
        <p className="text-xs leading-5 text-content-secondary">
          The VPI rebate is shown separately and offsets the charges above. Charges withheld from profit are not waived.
        </p>
      ) : null}

      {reconciliation.settlementAdjustmentUsdc !== 0n ? (
        <>
          <ReconciliationSection title="Settlement">
            <ReconciliationRow
              label="Settlement adjustment"
              amount={reconciliation.settlementAdjustmentUsdc}
              tone="default"
            />
          </ReconciliationSection>
          <p className="text-xs leading-5 text-content-secondary">
            The difference between the calculated close result and the recorded change in your account balance and trader claims. This amount is not classified as bad debt.
          </p>
        </>
      ) : null}

      <ReconciliationSection title="Account outcome">
        <ReconciliationRow
          label="Margin Account balance change"
          amount={reconciliation.marginAccountChangeUsdc}
        />
        <ReconciliationRow
          label={traderClaimLabel}
          amount={reconciliation.traderClaimChangeUsdc}
        />
        <ReconciliationRow
          label="Actual account change"
          amount={reconciliation.actualAccountChangeUsdc}
          emphasized
        />
      </ReconciliationSection>

      <p className="text-xs leading-5 text-content-secondary">
        Actual account change includes changes to your Margin Account balance and trader claims.
      </p>

      {reconciliation.releasedPositionMarginUsdc !== undefined || reconciliation.postPositionSize > 0n ? (
        <ReconciliationSection title="Position margin">
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
        </ReconciliationSection>
      ) : null}

      {reconciliation.releasedPositionMarginUsdc !== undefined ? (
        <p className="text-xs leading-5 text-content-secondary">
          Released margin is existing collateral becoming unlocked; it is not PnL and is not added to the actual account change.
        </p>
      ) : null}
    </div>
  )
}

export function PerpsCloseReconciliationDisclosure({
  reconciliation,
  initiallyExpanded = false,
}: {
  reconciliation: PerpsCloseReconciliation
  initiallyExpanded?: boolean
}) {
  const [isExpanded, setIsExpanded] = useState(initiallyExpanded)
  const detailsId = useId()
  const actualTone = signedTone(reconciliation.actualAccountChangeUsdc)

  return (
    <div data-testid="close-reconciliation-disclosure">
      <button
        type="button"
        aria-expanded={isExpanded}
        aria-controls={detailsId}
        className="flex min-h-12 w-full cursor-pointer flex-wrap items-center justify-between gap-4 border border-brand-border/20 bg-surface-panel px-4 py-3 text-left transition-colors hover:border-brand-border/40 hover:bg-[#3B212D] focus-visible:border-brand-border/50"
        onClick={() => {
          setIsExpanded((expanded) => !expanded)
        }}
      >
        <span className="text-sm font-semibold text-content-primary">
          Detailed close accounting
        </span>
        <span className="flex shrink-0 items-center gap-2 text-sm">
          <span className="text-xs text-content-secondary">Actual account change</span>
          <span className={amountToneClass(actualTone)}>
            <TokenAmount amount={formatSignedPerpsUsdc(reconciliation.actualAccountChangeUsdc)} />
          </span>
          <span aria-hidden="true" className="w-3 text-center text-content-secondary">
            {isExpanded ? '−' : '+'}
          </span>
        </span>
      </button>
      {isExpanded ? (
        <div id={detailsId} className="mt-3">
          <PerpsCloseReconciliationDetails reconciliation={reconciliation} />
        </div>
      ) : null}
    </div>
  )
}
