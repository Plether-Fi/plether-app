import { PERPS_SETTLEMENT_TOKEN_LABEL } from '../contracts/perpsAddresses'
import { type ReactNode, useId } from 'react'
import { formatPerpsPositionSize, formatPerpsAssets, formatSignedPerpsAssets } from '../utils/perps'
import type { PerpsCloseReconciliation } from '../utils/perpsCloseReconciliation'
import { TokenAmount, Tooltip } from './ui'

type AmountTone = 'default' | 'positive' | 'negative' | 'muted'

interface ReconciliationRowProps {
  label: string
  amount: bigint | undefined
  description?: string
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
  description,
  signed = true,
  tone = amount === undefined ? 'muted' : signed ? signedTone(amount) : 'default',
  emphasized = false,
}: ReconciliationRowProps) {
  return (
    <div className={`flex min-h-7 items-start justify-between gap-4 py-1.5 ${
      emphasized ? 'mt-2 border-t border-brand-border/30 pt-3' : ''
    }`}>
      <dt className={emphasized ? 'font-semibold text-content-primary' : 'text-content-secondary'}>
        {label}
        {description ? (
          <Tooltip content={description}>
            <button type="button" aria-label={`${label} info`} className="ml-1 inline-flex h-5 w-5 cursor-help items-center justify-center text-xs text-content-secondary">ⓘ</button>
          </Tooltip>
        ) : null}
      </dt>
      <dd className={`shrink-0 text-right ${emphasized ? 'font-semibold' : ''} ${amountToneClass(tone)}`}>
        {amount === undefined ? 'Unavailable' : <TokenAmount token={PERPS_SETTLEMENT_TOKEN_LABEL} amount={signed ? formatSignedPerpsAssets(amount) : formatPerpsAssets(amount)} />}
      </dd>
    </div>
  )
}

function ReconciliationSection({
  title,
  description,
  children,
}: {
  title: string
  description: string
  children: ReactNode
}) {
  const titleId = useId()
  return (
    <section aria-labelledby={titleId} className="border border-brand-border/20 bg-app-bg p-4">
      <h3 id={titleId} className="text-base font-semibold text-content-primary">{title}</h3>
      <p className="mb-4 mt-1 text-xs leading-5 text-content-secondary">{description}</p>
      {children}
    </section>
  )
}

function CloseAccountingDetails({
  reconciliation,
}: {
  reconciliation: PerpsCloseReconciliation
}) {
  const vpiEffectAssets = -reconciliation.vpiAssets
  const traderClaimLabel = reconciliation.traderClaimChangeAssets > 0n
    ? 'Trader claim created'
    : reconciliation.traderClaimChangeAssets < 0n
      ? 'Trader claim consumed'
      : 'Trader claim change'

  return (
    <div className="space-y-3" data-testid="close-reconciliation">
      <ReconciliationSection title="This close" description="Profit, costs, and adjustments from this transaction.">
        <dl className="text-sm">
          <ReconciliationRow label="Realized PnL" amount={reconciliation.realizedPnlAssets}
            description="Price-only profit or loss on the quantity closed in this transaction, before carry, fees, VPI, and settlement adjustments." />
          <ReconciliationRow label="Carry" amount={-reconciliation.carryAssets} />
          <ReconciliationRow label="Protocol execution fee" amount={-reconciliation.executionFeeAssets} />
          <ReconciliationRow label="Execution reward" amount={-reconciliation.executionBountyAssets} />
          <ReconciliationRow
            label={reconciliation.vpiAssets > 0n ? 'VPI charge' : reconciliation.vpiAssets < 0n ? 'VPI rebate' : 'VPI'}
            amount={vpiEffectAssets}
          />
          <ReconciliationRow label="Frozen spread charged" amount={-reconciliation.frozenSpreadChargedAssets} />
          {reconciliation.frozenSpreadWaivedAssets > 0n ? (
            <>
              <ReconciliationRow label="Frozen spread assessed" amount={reconciliation.frozenSpreadAssessedAssets} signed={false} tone="muted" />
              <ReconciliationRow label="Frozen spread waived" amount={reconciliation.frozenSpreadWaivedAssets} signed={false} tone="muted" />
            </>
          ) : null}
          <ReconciliationRow label="Close result before settlement adjustments" amount={reconciliation.netCloseResultAssets} />
          {reconciliation.settlementAdjustmentAssets !== 0n ? (
            <ReconciliationRow
              label="Settlement adjustment"
              amount={reconciliation.settlementAdjustmentAssets}
              tone="default"
              description="The difference between the calculated close result and the recorded change in your account balance and trader claims. This amount is not classified as bad debt."
            />
          ) : null}
          <ReconciliationRow label="Net result of this close" amount={reconciliation.actualAccountChangeAssets} emphasized />
        </dl>
        {reconciliation.frozenSpreadAssessedAssets > 0n && reconciliation.vpiAssets < 0n ? (
          <p className="mt-3 text-xs leading-5 text-content-secondary">
            The VPI rebate is shown separately and offsets the charges above. Charges withheld from profit are not waived.
          </p>
        ) : null}
      </ReconciliationSection>

      <ReconciliationSection title="Position lifetime" description="Realized result since opening, including this close.">
        <dl className="text-sm">
          {reconciliation.lifetime ? (
            <>
              <ReconciliationRow label="Previous trades net result" amount={reconciliation.lifetime.previousTradesResultAssets} />
              <ReconciliationRow label="Other account changes" amount={reconciliation.lifetime.betweenTradesAccountChangeAssets}
                description="Account changes between executions, excluding deposits and withdrawals. Includes carry paid between trades and any other account credits or charges." />
              <ReconciliationRow label="This close’s net result" amount={reconciliation.actualAccountChangeAssets} />
            </>
          ) : null}
          <ReconciliationRow label="Total realized result since opening" amount={reconciliation.lifetime?.netResultAssets} emphasized />
        </dl>
        {!reconciliation.lifetime ? (
          <p className="mt-2 text-xs text-content-secondary">Complete position history unavailable</p>
        ) : null}
        <div className="mt-4 border border-brand-border/20 bg-surface-panel p-3">
          <h4 className="text-xs font-medium uppercase tracking-wide text-content-secondary">VPI breakdown</h4>
          <p className="mb-2 mt-1 text-xs leading-5 text-content-secondary">Already included in the results above.</p>
          <dl className="text-sm">
            <ReconciliationRow label="Previous net VPI" amount={reconciliation.totalPositionVpiAssets === undefined
              ? undefined : -(reconciliation.totalPositionVpiAssets - reconciliation.vpiAssets)} />
            <ReconciliationRow label="This close’s VPI" amount={vpiEffectAssets} />
            <ReconciliationRow label="Total VPI balance" amount={reconciliation.totalPositionVpiAssets === undefined
              ? undefined : -reconciliation.totalPositionVpiAssets}
              description="Net VPI across this position, including this close. Negative means net paid; positive means net credited. This total is not an additional charge."
              emphasized />
          </dl>
        </div>
      </ReconciliationSection>

      <ReconciliationSection title="Collateral & balances" description="Collateral released and where this close’s result was recorded.">
        <h4 className="mb-2 text-xs font-medium uppercase tracking-wide text-content-secondary">Position collateral</h4>
        <dl className="text-sm">
          <ReconciliationRow label="Position margin released" amount={reconciliation.releasedPositionMarginAssets} signed={false} />
          {reconciliation.postPositionSize > 0n ? (
            <div className="flex items-start justify-between gap-4 py-1.5">
              <dt className="text-content-secondary">Remaining position quantity</dt>
              <dd><TokenAmount amount={formatPerpsPositionSize(reconciliation.postPositionSize, 6)} token="plDXY" /></dd>
            </div>
          ) : null}
          <ReconciliationRow label="Remaining position margin" amount={reconciliation.postPositionMarginAssets} signed={false} />
        </dl>
        <p className="mt-2 text-xs leading-5 text-content-secondary">
          Released margin is collateral, not profit. Some may cover costs or losses.
        </p>
        <div className="mt-4 border-t border-brand-border/20 pt-4">
          <h4 className="mb-2 text-xs font-medium uppercase tracking-wide text-content-secondary">Account settlement</h4>
          <dl className="text-sm">
            <ReconciliationRow label="Margin Account balance change" amount={reconciliation.marginAccountChangeAssets} />
            <ReconciliationRow label={traderClaimLabel} amount={reconciliation.traderClaimChangeAssets} />
            <ReconciliationRow label="Total account change" amount={reconciliation.actualAccountChangeAssets} emphasized />
          </dl>
          <p className="mt-2 text-xs leading-5 text-content-secondary">Matches the net result of this close.</p>
        </div>
      </ReconciliationSection>
    </div>
  )
}

interface CloseReconciliationProps {
  reconciliation: PerpsCloseReconciliation
  executionDetails?: ReactNode
}

export function PerpsCloseReconciliationDetails({
  reconciliation,
  executionDetails,
}: CloseReconciliationProps) {
  return (
    <div className="space-y-3" data-testid="close-reconciliation-disclosure">
      <section className="border border-brand-border/20 bg-app-bg p-4" data-testid="close-reconciliation-summary">
        <h3 className="mb-3 text-base font-semibold text-content-primary">
          {reconciliation.postPositionSize === 0n ? 'Position fully closed' : 'Position partially closed'}
        </h3>
        <dl className="text-sm">
          <ReconciliationRow label="Net result of this close" amount={reconciliation.actualAccountChangeAssets}
            description="The change in your Margin Account balance plus trader claims from this close, after costs and settlement adjustments."
            emphasized />
          <ReconciliationRow label="Total realized result since opening" amount={reconciliation.lifetime?.netResultAssets}
            description="Realized account result since this position opened, including trading costs, carry paid, and other account credits or charges. Excludes deposits, withdrawals, remaining unrealized PnL, and wallet gas. Unavailable when position history is incomplete."
            emphasized />
          <ReconciliationRow label="Position margin released" amount={reconciliation.releasedPositionMarginAssets}
            description="Reduction in assigned collateral. Some may cover close costs or losses; this is not additional profit or a guaranteed withdrawable amount."
            signed={false} />
          {reconciliation.postPositionSize > 0n ? (
            <>
              <div className="flex items-start justify-between gap-4 py-1.5">
                <dt className="text-content-secondary">Remaining position quantity</dt>
                <dd><TokenAmount amount={formatPerpsPositionSize(reconciliation.postPositionSize, 6)} token="plDXY" /></dd>
              </div>
              <ReconciliationRow label="Remaining position margin" amount={reconciliation.postPositionMarginAssets} signed={false} />
            </>
          ) : null}
        </dl>
      </section>
      <CloseAccountingDetails reconciliation={reconciliation} />
      {executionDetails}
    </div>
  )
}

export function PerpsCloseReconciliationDisclosure(props: CloseReconciliationProps) {
  return <PerpsCloseReconciliationDetails {...props} />
}
