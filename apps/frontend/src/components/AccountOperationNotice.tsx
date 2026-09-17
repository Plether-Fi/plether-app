import type { SponsoredOperation } from '../perps-aa/operationStore'
import { accountOperationGuidance } from '../utils/accountOperationGuidance'
import { sponsoredOperationActionLabel } from '../utils/sponsoredOperation'
import { Button } from './ui/Button'

export function AccountOperationNotice({ operation, now, onOpen }: {
  operation: SponsoredOperation
  now: number
  onOpen: () => void
}) {
  const guidance = accountOperationGuidance(operation, now)
  const started = new Date(operation.createdAt)
  const age = Math.max(0, Math.floor((now - operation.createdAt) / 60_000))
  const startedLabel = age < 1 ? 'Started just now'
    : age < 60 ? `Started ${String(age)} min ago`
    : `Started ${String(Math.floor(age / 60))}h${age % 60 ? ` ${String(age % 60)}m` : ''} ago`
  return <section aria-label="Pending Trading Account action"
    className={`border p-4 ${guidance.attention ? 'border-warning/40 bg-warning-bg' : 'border-brand-peach/30 bg-brand-peach/5'}`}>
    <div className="flex items-start gap-3">
      <span aria-hidden="true" className={`material-symbols-outlined mt-0.5 ${guidance.attention ? 'text-warning' : 'text-brand-peach'}`}>
        {guidance.attention ? 'info' : 'hourglass_top'}
      </span>
      <div className="min-w-0 flex-1">
        <div role="status" className="text-sm font-semibold text-content-primary">{guidance.title}</div>
        <p className="mt-1 text-xs text-content-secondary">
          {sponsoredOperationActionLabel(operation.action)} · <time dateTime={started.toISOString()} title={started.toLocaleString()}>{startedLabel}</time>
        </p>
        <p className="mt-2 text-sm leading-5 text-content-secondary">{guidance.description}</p>
        <Button type="button" className="mt-3 min-h-11 w-full" size="sm" variant="secondary"
          analyticsId="open_account_operation" analyticsProperties={{ operation_status: operation.status, action_kind: operation.action }} onClick={onOpen}>
          {guidance.action}
        </Button>
      </div>
    </div>
  </section>
}
