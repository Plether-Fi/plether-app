import type { PerpsAnalyticsProperties } from '../analytics/perps'
import type { PerpsReviewChange } from '../utils/perpsReviewChanges'
import { Button } from './ui'

interface PerpsReviewFooterProps {
  preparing: boolean
  refreshing: boolean
  slow: boolean
  error?: string
  changes: PerpsReviewChange[]
  canConfirm: boolean
  direction: 'long' | 'short'
  onConfirm: () => void
  onCancel: () => void
  onRetry?: () => void
  analyticsProperties?: PerpsAnalyticsProperties
}

export function PerpsReviewFooter({ preparing, refreshing, slow, error, changes, canConfirm, direction,
  onConfirm, onCancel, onRetry, analyticsProperties }: PerpsReviewFooterProps) {
  return (
    <div className="space-y-3">
      <div role="status" aria-live="polite" aria-atomic="true" className="text-sm text-content-secondary">
        {preparing ? (
          <p>{slow ? 'Network is taking longer than usual.' : refreshing
            ? 'Updating review. Previous values remain visible until checks finish.'
            : 'Checking your order before confirmation.'}</p>
        ) : error ? <p className="text-brand-orange">{error}</p> : changes.length > 0 ? (
          <>
            <p className="font-semibold text-content-primary">Order updated. Review the changes before confirming.</p>
            <ul tabIndex={0} aria-label="Updated order terms" className="mt-2 max-h-40 space-y-1 overflow-y-auto">
              {changes.map(change => <li key={change.label} className="break-words">{change.label}: {change.before} → {change.after}</li>)}
            </ul>
          </>
        ) : null}
      </div>
      {!preparing && error && onRetry ? <Button size="sm" variant="secondary" onClick={onRetry}>Retry review</Button> : null}
      <div className="flex gap-3">
        <Button className="flex-1 !border-[#FFAB96]/40 !bg-[#250917] !text-[#FFF5F9] enabled:hover:!border-[#FFAB96] enabled:hover:!bg-[#3B212D]"
          variant="secondary" analyticsId="cancel_review" analyticsProperties={analyticsProperties} onClick={onCancel}>Cancel</Button>
        <Button className="flex-1" variant={direction === 'short' ? 'danger' : 'primary'}
          isLoading={preparing} aria-busy={preparing} disabled={!canConfirm || !!error}
          analyticsId="confirm_commit" analyticsProperties={analyticsProperties} onClick={onConfirm}>
          {preparing ? refreshing ? 'Updating review…' : 'Checking order…' : changes.length > 0 ? 'Confirm updated order' : 'Confirm Commit'}
        </Button>
      </div>
    </div>
  )
}
