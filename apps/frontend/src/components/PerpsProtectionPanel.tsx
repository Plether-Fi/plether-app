import { useEffect, useRef, useState, type ReactNode } from 'react'
import { formatUnits } from 'viem'
import { ProtectionInputs, ProtectionPriceSummary } from './ProtectionInputs'
import { EMPTY_PROTECTION_DRAFT, type ProtectionDraft, protectionParamsFromInputs, type PositionProtection, type PositionProtectionParams } from '../contracts/positionProtection'
import { useProtectionEvents, useProtectionHistory, useProtectionExecution, type ProtectionHistoryEvent, type ProtectionHistoryRecord } from '../hooks/useProtectionHistory'
import { usePerpsTrading } from '../hooks/usePerpsTrading'
import type { ProtectionConfiguration } from '../hooks/useProtectionConfiguration'
import type { PerpsPosition } from '../hooks/usePerpsAccount'
import { formatPerpsPositionSize, formatPerpsUsdc, perpsSideToDirection } from '../utils/perps'
import { protectionPrice, protectionStatusLabel, protectionStateDescription } from '../utils/positionProtection'
import { ProtectionExecutionNotice } from './ProtectionExecutionNotice'
import type { ProtectionExecutionReport } from '../utils/protectionExecution'
import { PERPS_TERMINAL_REASON_LABELS } from '../contracts/perpsOrderV2'
import { Button } from './ui'
import { usePerpsIdentity } from '../perps-aa'
import { getExplorerTxUrl } from '../utils/explorer'

interface ProtectionPanelProps {
  protection?: PositionProtection
  position?: PerpsPosition
  rawMark?: bigint
  cap?: bigint
  configuration: ProtectionConfiguration
  pendingOrders: number
  onRefresh: () => void
}
export interface ProtectionManagementRequest {
  action: 'create' | 'replace' | 'cancel'
  protectionId?: bigint
  params?: PositionProtectionParams
}
interface ProtectionReview extends ProtectionManagementRequest {
  params: PositionProtectionParams
  cap: bigint
  rawMark: bigint
  reward: bigint
}

const ACTION_CLASS = '!border-[#FFAB96] !bg-[#FFAB96] !text-app-bg enabled:hover:!bg-[#FF572D]'
const STATE_CHANGED = 'Your account, position, or TP/SL changed. Go back and review it again.'

export function PerpsProtectionPanel(props: ProtectionPanelProps) {
  const { managePositionProtection } = usePerpsTrading()
  const { accountAddress } = usePerpsIdentity()
  const execution = useProtectionExecution(props.protection?.protectionId, [2, 8].includes(props.protection?.status ?? 0))
  return <PositionProtectionManager {...props} accountAddress={accountAddress}
    executionReport={execution.data} executionLoading={execution.isFetching && !execution.data} executionError={execution.isError}
    onRefreshExecution={() => { void execution.refetch() }}
    onManage={async request => { await managePositionProtection(request); props.onRefresh() }}
    history={<ProtectionHistory activeProtectionId={props.protection?.protectionId} />}
  />
}

/** The same stateful UI is used by the app and the transaction-free Storybook demo. */
export function PositionProtectionManager({ protection, position, rawMark, cap, configuration, pendingOrders, accountAddress, onManage, history, executionReport, executionLoading, executionError, onRefreshExecution }: Omit<ProtectionPanelProps, 'onRefresh'> & {
  accountAddress?: string
  onManage: (request: ProtectionManagementRequest) => Promise<void>
  history?: ReactNode
  executionReport?: ProtectionExecutionReport
  executionLoading?: boolean
  executionError?: boolean
  onRefreshExecution?: () => void
}) {
  const [view, setView] = useState<'overview' | 'edit' | 'review' | 'remove'>('overview')
  const [draft, setDraft] = useState<ProtectionDraft>(EMPTY_PROTECTION_DRAFT)
  const [pending, setPending] = useState(false)
  const [error, setError] = useState<string>()
  const [success, setSuccess] = useState<string>()
  const [review, setReview] = useState<ProtectionReview>()
  const heading = useRef<HTMLHeadingElement>(null)
  const previousView = useRef(view)
  const targetKey = [accountAddress, position?.exists, position?.direction, position?.size, protection?.protectionId, protection?.status, protection?.takeProfitTriggerPrice, protection?.stopLossTriggerPrice].join(':')
  const [reviewTarget, setReviewTarget] = useState<string>()
  const targetChanged = reviewTarget !== undefined && reviewTarget !== targetKey
  const editable = protection !== undefined && [1, 2].includes(protection.status)
  const creatable = !protection && position?.exists && pendingOrders === 0
  const direction = position?.exists ? position.direction : perpsSideToDirection(protection?.side)
  const reward = (configuration.triggerBountyUsdc ?? 0n) + (configuration.executionBountyUsdc ?? 0n)
  const reviewChanged = targetChanged || (review?.action === 'create' && (review.reward !== reward || pendingOrders > 0))
  const delayed = protection?.status === 8
  const closing = protection?.status === 3 || delayed
  const currentSize = position?.exists ? position.size : protection?.size
  const reserve = protection ? protection.triggerBountyUsdc + protection.executionBountyUsdc : reward

  useEffect(() => {
    if (previousView.current !== view) heading.current?.focus()
    previousView.current = view
  }, [view])

  function edit() {
    const display = (price?: bigint) => price && cap ? formatUnits(cap - price, 8) : ''
    setDraft({ mode: 'price', takeProfit: display(protection?.takeProfitTriggerPrice), stopLoss: display(protection?.stopLossTriggerPrice) })
    setError(undefined)
    setSuccess(undefined)
    setReviewTarget(targetKey)
    setReview(undefined)
    setView('edit')
  }
  function reviewChanges() {
    setError(undefined)
    try {
      if (targetChanged) throw new Error(STATE_CHANGED)
      if (!rawMark || !cap) throw new Error('Waiting for a current market price. Please try again.')
      const params = protectionParamsFromInputs({ ...draft, direction, rawMark, cap })
      setReview({ action: protection ? 'replace' : 'create', protectionId: protection?.protectionId, params, rawMark, cap, reward })
      setView('review')
    } catch (cause) { setError(cause instanceof Error ? cause.message : 'Check your TP/SL prices') }
  }
  async function submit(remove = false) {
    if (pending) return
    setPending(true)
    setError(undefined)
    try {
      if (remove ? targetChanged : reviewChanged) throw new Error(STATE_CHANGED)
      // Do not recompute percentage inputs against a moving mark after review.
      let request: ProtectionManagementRequest
      if (remove) {
        if (!editable) throw new Error(STATE_CHANGED)
        request = { action: 'cancel', protectionId: protection.protectionId }
      } else {
        if (!review || !configuration.enabled) throw new Error('Review your TP/SL again before confirming.')
        request = { action: review.action, protectionId: review.protectionId, params: review.params }
      }
      await onManage(request)
      setView('overview')
      setReview(undefined)
      setSuccess(remove ? 'TP/SL removed. No position-close order was submitted.' : 'TP/SL saved.')
    } catch (cause) { setError(cause instanceof Error ? cause.message : 'We could not update your TP/SL. Please try again.') }
    finally { setPending(false) }
  }

  return <div className="space-y-5 text-sm text-content-primary">
    <section className="border border-brand-border/20 bg-app-bg">
      <header className="flex flex-wrap items-start justify-between gap-3 border-b border-brand-border/20 p-4 sm:p-5">
        <div>
          <p className="mb-1 text-[10px] font-medium uppercase tracking-[0.16em] text-content-secondary">Position exits</p>
          <h3 ref={heading} tabIndex={-1} className="text-lg font-semibold focus:outline-none">{view === 'edit' ? 'Set your TP/SL' : view === 'review' ? 'Review your TP/SL' : view === 'remove' ? 'Remove TP/SL?' : 'Take profit & stop loss'}</h3>
          <p className="mt-1 text-xs text-content-secondary">{protection || position?.exists ? <>{direction === 'long' ? 'Long' : 'Short'} · plDXY Perp · {currentSize ? `${formatPerpsPositionSize(currentSize)} plDXY · ` : ''}Full position</> : 'plDXY Perp · No open position'}</p>
        </div>
        <span className={`inline-flex items-center gap-2 border px-2.5 py-1 text-xs ${delayed ? 'border-[#F7D977]/40 text-[#F7D977]' : protection?.status === 2 ? 'border-positive/30 text-positive' : 'border-brand-border/30 text-content-secondary'}`}>
          <span className="h-1.5 w-1.5 rounded-full bg-current" aria-hidden="true" />{protectionStatusLabel(protection?.status)}
        </span>
      </header>

      <div className="space-y-4 p-4 sm:p-5">
        {view === 'overview' ? <>
          {protection ? <>
            <ProtectionPriceSummary params={protection} cap={cap} rawMark={rawMark} />
            {protection.status === 1 ? <p className="text-sm leading-6 text-content-secondary">Your triggers will become active after opening order #{protection.parentOrderId.toString()} fills. They are not monitoring a position yet.</p> : null}
            {protection.status === 2 ? <p className="text-sm leading-6 text-content-secondary">The first trigger reached queues a close for your full position. The other trigger is then cancelled.</p> : null}
            {closing ? <div className={`border-l-2 p-3 ${delayed ? 'border-[#F7D977] bg-[#F7D977]/5' : 'border-[#FFAB96] bg-[#FFAB96]/5'}`}>
              <p className="font-semibold">{delayed ? 'Your position is still open' : `${protection.triggeredLeg === 1 ? 'Take profit' : 'Stop loss'} reached`}</p>
              <p className="mt-1 text-sm leading-6 text-content-secondary">{delayed
                ? 'The triggered close did not complete. Your original trigger remains binding, even if the price moves back. TP/SL cannot be changed or removed while this close is unresolved.'
                : `Close order #${protection.linkedOrderId.toString()} is waiting for execution. The final fill price may differ from your trigger. TP/SL can no longer be changed or removed.`}</p>
              {protection.triggerMarkPrice > 0n ? <p className="mt-2 text-xs text-content-secondary">{protection.triggeredLeg === 1 ? 'Take profit' : 'Stop loss'} triggered at {protectionPrice(protection.triggerMarkPrice, cap)} USDC{delayed ? ` · latest close #${protection.linkedOrderId.toString()}` : ''}</p> : null}
              <p className="mt-2 text-xs text-content-secondary">You can still add margin from the Position tab.</p>
            </div> : null}
            {[4, 5, 6, 7].includes(protection.status) ? <p className="text-sm leading-6 text-content-secondary">{protectionStateDescription(protection.status)}</p> : null}
            {[2, 8].includes(protection.status) ? <ProtectionExecutionNotice protection={protection} report={executionReport} loading={executionLoading} error={executionError} onRefresh={onRefreshExecution} /> : null}
          </> : <div className="py-3">
            <p className="font-medium">Choose when to exit</p>
            <p className="mt-2 max-w-lg text-sm leading-6 text-content-secondary">Set a take-profit price, a stop-loss price, or both. Each applies to your full position.</p>
            {!position?.exists ? <p className="mt-2 text-xs text-content-secondary">Open a position first, or add TP/SL to a new order in the trade ticket.</p> : pendingOrders > 0 ? <p className="mt-2 text-xs text-[#F7D977]">Wait for your pending orders to finish before adding TP/SL.</p> : null}
          </div>}
          {!configuration.enabled ? <p className="text-xs leading-5 text-content-secondary">Adding and editing TP/SL is temporarily unavailable.{editable ? ' You can still remove your existing triggers.' : ''}</p> : null}
          <div className="flex flex-wrap gap-2">
            {(creatable || editable) && configuration.enabled ? <Button size="sm" className={ACTION_CLASS} onClick={edit}>{protection ? 'Edit TP/SL' : 'Add TP/SL'}</Button> : null}
            {editable ? <Button size="sm" variant="secondary" onClick={() => { setReviewTarget(targetKey); setView('remove'); setError(undefined); setSuccess(undefined) }}>Remove TP/SL</Button> : null}
          </div>
        </> : view === 'edit' ? <>
          <ProtectionInputs value={draft} onChange={setDraft} disabled={pending || targetChanged} direction={direction} rawMark={rawMark} cap={cap} />
          <p className="text-xs text-content-secondary">{protection ? 'Your current triggers stay in place until the update is confirmed.' : `${formatPerpsUsdc(reward)} USDC will be reserved from free margin to pay for triggering and executing the close.`}</p>
          <div className="flex flex-wrap gap-2">
            <Button size="sm" className={ACTION_CLASS} disabled={pending || targetChanged || !configuration.enabled || (!draft.takeProfit && !draft.stopLoss)} onClick={reviewChanges}>Review TP/SL</Button>
            <Button size="sm" variant="secondary" onClick={() => { setView('overview'); setError(undefined) }}>Back</Button>
          </div>
        </> : view === 'review' && review ? <>
          <ProtectionPriceSummary params={review.params} cap={review.cap} rawMark={review.rawMark} />
          <dl className="space-y-2 border-y border-brand-border/20 py-3 text-xs">
            <div className="flex justify-between gap-4"><dt className="text-content-secondary">Amount to close</dt><dd>100% of the position</dd></div>
            <div className="flex justify-between gap-4"><dt className="text-content-secondary">Execution reserve</dt><dd>{formatPerpsUsdc(protection ? reserve : review.reward)} USDC {protection ? '· already reserved' : '· from free margin'}</dd></div>
            <div className="flex justify-between gap-4"><dt className="text-content-secondary">Active from</dt><dd>{protection?.status === 1 ? 'Opening order execution' : 'Update confirmation'}</dd></div>
          </dl>
          <p className="text-xs leading-5 text-content-secondary">These trigger prices are fixed for confirmation. Reaching one queues a close; it does not guarantee that fill price.</p>
          <div className="flex flex-wrap gap-2">
            <Button size="sm" className={ACTION_CLASS} isLoading={pending} disabled={reviewChanged || !configuration.enabled} onClick={() => void submit()}>Confirm TP/SL</Button>
            <Button size="sm" variant="secondary" disabled={pending} onClick={() => { setView('edit'); setError(undefined) }}>Back to edit</Button>
          </div>
        </> : view === 'remove' ? <>
          <p className="text-sm leading-6">{protection?.status === 1 ? 'Your opening order will remain committed and can still fill without TP/SL.' : 'Your position will stay open without take-profit or stop-loss triggers.'}</p>
          <p className="text-xs leading-5 text-content-secondary">Removing TP/SL does not close your position. It releases the unpaid execution reserve of {formatPerpsUsdc(reserve)} USDC.</p>
          <div className="flex flex-wrap gap-2">
            <Button size="sm" variant="danger" isLoading={pending} disabled={targetChanged} onClick={() => void submit(true)}>Confirm removal</Button>
            <Button size="sm" variant="secondary" disabled={pending} onClick={() => { setView('overview'); setError(undefined) }}>Keep TP/SL</Button>
          </div>
        </> : null}
        {pending ? <p role="status" className="text-xs text-content-secondary">Confirm in your wallet, then wait for the update.</p> : null}
        {view !== 'overview' && (targetChanged || (view === 'review' && reviewChanged)) ? <p role="alert" className="text-xs text-brand-orange">{STATE_CHANGED}</p> : null}
        {error ? <p role="alert" className="break-words text-sm text-brand-orange">{error}</p> : null}
        {success ? <p role="status" className="text-xs text-positive">{success}</p> : null}
      </div>
      {protection && view === 'overview' ? <details className="border-t border-brand-border/20 px-4 py-3 text-xs text-content-secondary sm:px-5">
        <summary className="cursor-pointer hover:text-content-primary">Execution details</summary>
        <dl className="mt-3 space-y-2">
          <div className="flex flex-wrap justify-between gap-2"><dt>Execution reserve remaining</dt><dd>{formatPerpsUsdc(reserve)} USDC</dd></div>
          <div className="flex flex-wrap justify-between gap-2"><dt>Protection reference</dt><dd>#{protection.protectionId.toString()}</dd></div>
          {protection.linkedOrderId > 0n ? <div className="flex flex-wrap justify-between gap-2"><dt>Latest close order</dt><dd>#{protection.linkedOrderId.toString()}</dd></div> : null}
        </dl>
        <p className="mt-3 leading-5">You can still add margin. To place a discretionary order, remove TP/SL first if it has not triggered. Execution depends on the oracle and order queue.</p>
      </details> : null}
    </section>
    {history ? <section><h3 className="mb-3 text-sm font-semibold">TP/SL activity</h3>{history}</section> : null}
  </div>
}

function ProtectionHistory({ activeProtectionId }: { activeProtectionId?: bigint }) {
  const history = useProtectionHistory(true)
  if (history.isPending) return <p className="text-xs text-content-secondary">Loading TP/SL activity…</p>
  if (history.isError) return <div className="text-xs text-content-secondary"><p>Activity is unavailable. Your current TP/SL is shown above.</p><button type="button" className="mt-2 underline" onClick={() => void history.refetch()}>Try again</button></div>
  const rows = history.data.pages.flatMap(page => page.protections)
  return <div className="space-y-2">
    {rows.map((row, index) => {
      const initiallyExpanded = !activeProtectionId && index === 0 && [4, 5, 6, 7].includes(row.status)
      return <ConnectedProtectionHistoryRow key={`${row.protectionId}:${initiallyExpanded ? 'terminal' : 'default'}`} row={row} initiallyExpanded={initiallyExpanded} />
    })}
    {!rows.length ? <p className="text-xs text-content-secondary">Your confirmed TP/SL updates will appear here. Recent changes may take a moment to show.</p> : null}
    {history.hasNextPage ? <Button size="sm" variant="secondary" disabled={history.isFetchingNextPage} onClick={() => void history.fetchNextPage()}>Earlier activity</Button> : null}
  </div>
}

function ConnectedProtectionHistoryRow({ row, initiallyExpanded = false }: { row: ProtectionHistoryRecord; initiallyExpanded?: boolean }) {
  const [expanded, setExpanded] = useState(initiallyExpanded)
  const events = useProtectionEvents(row.protectionId, expanded)
  return <ProtectionHistoryRow row={row} initiallyExpanded={initiallyExpanded} onToggle={setExpanded}
    events={events.data?.pages.flatMap(page => page.events)}
    pending={events.isPending} error={events.isError}
    more={events.hasNextPage ? <Button size="sm" variant="secondary" disabled={events.isFetchingNextPage} onClick={() => void events.fetchNextPage()}>Earlier events</Button> : undefined}
  />
}

export function ProtectionHistoryRow({ row, events, pending, error, more, onToggle, initiallyExpanded = false }: {
  row: ProtectionHistoryRecord
  events?: ProtectionHistoryEvent[]
  pending?: boolean
  error?: boolean
  more?: ReactNode
  onToggle?: (expanded: boolean) => void
  initiallyExpanded?: boolean
}) {
  const [expanded, setExpanded] = useState(initiallyExpanded)
  const labels: Record<string, string> = {
    PositionProtectionCreated: 'TP/SL added', PositionProtectionArmed: 'Triggers became active',
    PositionProtectionReplaced: 'Trigger prices updated', PositionProtectionCancelled: 'TP/SL removed',
    PositionProtectionTriggered: 'Exit price reached', PositionProtectionCloseAttemptQueued: 'Close order queued',
    PositionProtectionCloseAttemptFailed: 'Close did not complete', PositionProtectionTerminal: 'Protection finished',
  }
  return <details open={expanded} className="border border-brand-border/20 bg-app-bg px-4 py-3" onToggle={event => { setExpanded(event.currentTarget.open); onToggle?.(event.currentTarget.open) }}>
    <summary className="cursor-pointer text-sm">
      <span className="font-medium">{protectionStatusLabel(row.status)}</span><span className="ml-2 text-xs text-content-secondary">{row.side === 0 ? 'Long' : 'Short'} · #{row.protectionId}</span>
    </summary>
    <p className="mt-3 text-xs leading-5 text-content-secondary">{protectionStateDescription(row.status)}</p>
    <p className="mt-2 text-xs text-content-secondary">{row.parentOrderId !== '0' ? `Opening order #${row.parentOrderId} · ` : ''}{row.linkedOrderId !== '0' ? `Latest close #${row.linkedOrderId}` : 'No close order queued'}{row.status === 5 ? ' · See Order history for the order outcome.' : ''}</p>
    <p className="mt-3 text-xs text-content-secondary">TP {row.takeProfitTriggerPrice === '0' ? 'not set' : protectionPrice(BigInt(row.takeProfitTriggerPrice), 200_000_000n)} · SL {row.stopLossTriggerPrice === '0' ? 'not set' : protectionPrice(BigInt(row.stopLossTriggerPrice), 200_000_000n)} USDC</p>
    {pending ? <p className="mt-3 text-xs">Loading activity…</p> : error ? <p className="mt-3 text-xs text-content-secondary">Event details are temporarily unavailable.</p> : <ol className="mt-4 space-y-3 border-l border-brand-border/30 pl-4">
      {events?.map(event => <li key={`${event.blockHash}:${event.logIndex}`} className="text-xs">
        <a className="text-content-primary underline decoration-brand-border/40 underline-offset-4 hover:text-[#FFAB96]" href={getExplorerTxUrl(421614, event.transactionHash)} target="_blank" rel="noopener noreferrer">{event.event === 'PositionProtectionTerminal' ? `TP/SL ${protectionStatusLabel(Number(event.args.status ?? row.status)).toLowerCase()}` : event.event === 'PositionProtectionTriggered' ? `${Number(event.args.leg ?? row.triggeredLeg) === 1 ? 'Take profit' : 'Stop loss'} reached` : labels[event.event] ?? 'TP/SL update'} ↗</a>
        <p className="mt-1 text-content-secondary">{event.args.linkedOrderId && event.args.linkedOrderId !== '0' ? `Close #${String(event.args.linkedOrderId)} · ` : ''}Block {event.blockNumber}</p>
        {event.args.reason !== undefined ? <p className="mt-1 text-content-secondary">{PERPS_TERMINAL_REASON_LABELS[Number(event.args.reason)] ?? 'Execution needs review'}{event.args.relatched === true ? ' · the original trigger remained binding after this attempt' : ''}</p> : null}
      </li>)}
    </ol>}
    {!pending && !error && events?.length === 0 ? <p className="mt-3 text-xs text-content-secondary">No event details are available yet. Recent updates may still be indexing.</p> : null}
    {more}
  </details>
}
