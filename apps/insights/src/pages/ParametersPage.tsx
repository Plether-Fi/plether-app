import { useEffect, useState, type ReactNode } from 'react'
import { Link, useSearchParams } from 'react-router-dom'
import type { AvailabilityReason, ParameterChangesResponse, ParametersResponse } from '../api'
import { useCurrentProtocolRelease, useParameterChanges, useParameters } from '../api'
import { AvailabilityList, DisplayValue, EvidenceBadge, PageTitle, ProtocolMeta, RawEvidence, Section } from '../components/Protocol'
import { EmptyState, ErrorState, LoadingState, Panel } from '../components/ui'
import { dedupeBy, formatTimestamp, humanize, readString } from '../utils/protocol'

const TABS = ['current', 'pending', 'history'] as const
type Tab = typeof TABS[number]

export function ParametersPage() {
  const release = useCurrentProtocolRelease()
  const releaseId = release.data?.releaseId ?? ''
  const query = useParameters(releaseId)
  const historyQuery = useParameterChanges(releaseId)
  const [searchParams, setSearchParams] = useSearchParams()
  const requested = searchParams.get('view')
  const tab: Tab = TABS.includes(requested as Tab) ? requested as Tab : 'current'
  const historyPages = historyQuery.data?.pages ?? []
  const historyAnchor = historyPages.at(0)
  const historyValues = dedupeBy(
    historyPages.flatMap((page) => page.parameterChanges.items),
    parameterChangeIdentity,
  )
  const historyAvailability = dedupeBy(
    historyPages.flatMap((page) => page.availability),
    (item) => `${item.field}:${item.reason}`,
  )

  if (release.isLoading || query.isLoading) return <Panel><LoadingState rows={10} /></Panel>
  if (query.isError) return <ErrorState title="Protocol parameters unavailable" message={query.error.message} onRetry={() => void query.refetch()} />
  const data = query.data
  if (!data) return <ErrorState title="Protocol parameters unavailable" />

  return (
    <div className="space-y-7">
      <PageTitle title="Parameters and governance" description="Current values are read at one confirmed block. The catalog explains units, risk direction, mutability, timelock policy, and source getter. Pending and historical actions never masquerade as active values." />
      <ProtocolMeta data={data} />
      <AvailabilityList items={data.availability} />
      <div className="flex border-b border-brand-border/25" role="tablist" aria-label="Parameter views">
        {TABS.map((item) => <button id={`parameters-tab-${item}`} role="tab" aria-controls={`parameters-panel-${item}`} aria-selected={tab === item} key={item} type="button" onClick={() => { setSearchParams({ view: item }); }} className={`border-b-2 px-5 py-3 text-sm font-semibold capitalize ${tab === item ? 'border-brand-orange text-content-primary' : 'border-transparent text-content-secondary hover:text-content-primary'}`}>{item}</button>)}
      </div>
      <div id={`parameters-panel-${tab}`} role="tabpanel" aria-labelledby={`parameters-tab-${tab}`} tabIndex={0}>
        <TrackedParameterView
          key={releaseId}
          releaseId={releaseId}
          parameters={data.parameters}
          tab={tab}
          history={{
            anchor: historyAnchor,
            firstPageValues: historyAnchor?.parameterChanges.items ?? [],
            values: historyValues,
            availability: historyAvailability,
            isLoading: historyQuery.isLoading,
            isError: historyQuery.isError,
            errorMessage: historyQuery.error?.message,
            refetch: () => { void historyQuery.refetch(); },
            hasNextPage: historyQuery.hasNextPage,
            isFetchingNextPage: historyQuery.isFetchingNextPage,
            fetchNextPage: () => { void historyQuery.fetchNextPage(); },
            pageCount: historyPages.length,
          }}
        />
      </div>
    </div>
  )
}

interface ParameterHistoryFeed {
  anchor: ParameterChangesResponse | undefined
  firstPageValues: Record<string, unknown>[]
  values: Record<string, unknown>[]
  availability: AvailabilityReason[]
  isLoading: boolean
  isError: boolean
  errorMessage: string | undefined
  refetch: () => void
  hasNextPage: boolean
  isFetchingNextPage: boolean
  fetchNextPage: () => void
  pageCount: number
}

function TrackedParameterView({
  releaseId,
  parameters,
  tab,
  history,
}: {
  releaseId: string
  parameters: ParametersResponse['parameters']
  tab: Tab
  history: ParameterHistoryFeed
}) {
  const storageKey = `plether.insights.protocol.parameter-changes.seen.v1.${releaseId}`
  const [previousIdentities] = useState(() => readSeenChanges(storageKey))
  const currentIdentities = [...parameters.pending, ...history.firstPageValues]
    .map(changeIdentity)
    .filter((value): value is string => value !== null)
  const serializedIdentities = JSON.stringify([...new Set(currentIdentities)])
  const newChangeIds = previousIdentities === null
    ? new Set<string>()
    : new Set(currentIdentities.filter((identity) => !previousIdentities.has(identity)))

  useEffect(() => {
    if (history.anchor !== undefined) {
      void writeSeenChanges(storageKey, serializedIdentities)
    }
  }, [history.anchor, serializedIdentities, storageKey])

  return (
    <>
      {newChangeIds.size > 0 ? (
        <div role="status" className="border border-brand-peach/30 bg-brand-peach/10 px-4 py-3 text-sm text-brand-peach">
          {newChangeIds.size} governance change{newChangeIds.size === 1 ? '' : 's'} observed since your previous view.
        </div>
      ) : null}
      {tab === 'current' ? <CurrentParameters values={parameters.current} /> : null}
      {tab === 'pending' ? <ChangeList title="Pending timelock actions" values={parameters.pending} empty="No active onchain timelock activation was found at the confirmed block." newChangeIds={newChangeIds} releaseId={releaseId} /> : null}
      {tab === 'history' ? <ParameterHistory history={history} newChangeIds={newChangeIds} releaseId={releaseId} /> : null}
    </>
  )
}

function ParameterHistory({
  history,
  newChangeIds,
  releaseId,
}: {
  history: ParameterHistoryFeed
  newChangeIds: ReadonlySet<string>
  releaseId: string
}) {
  if (history.isLoading) return <Panel><LoadingState rows={8} /></Panel>
  if (history.isError) {
    return <ErrorState title="Governance history unavailable" message={history.errorMessage} onRetry={history.refetch} />
  }
  return (
    <div className="space-y-5">
      {history.anchor ? (
        <div aria-label="Governance history pagination anchor" className="space-y-2">
          <p className="text-xs text-content-tertiary">All loaded governance changes remain anchored to this first page’s confirmed block.</p>
          <ProtocolMeta data={history.anchor} />
        </div>
      ) : null}
      <AvailabilityList items={history.availability} />
      <ChangeList
        title="Governance history"
        values={history.values}
        empty="No proposal, cancellation, supersession, execution, role, dependency, ownership, or pause change has been indexed for this release."
        newChangeIds={newChangeIds}
        releaseId={releaseId}
      />
      {history.hasNextPage ? (
        <div className="text-center">
          <button
            type="button"
            onClick={history.fetchNextPage}
            disabled={history.isFetchingNextPage}
            className="border border-brand-border/40 px-5 py-2.5 text-sm font-semibold hover:border-brand-orange disabled:cursor-wait disabled:opacity-60"
          >
            {history.isFetchingNextPage ? 'Loading governance history…' : 'Load more governance history'}
          </button>
        </div>
      ) : null}
      <p className="text-center text-xs text-content-tertiary">{history.values.length} unique changes loaded across {history.pageCount} anchored page{history.pageCount === 1 ? '' : 's'}.</p>
    </div>
  )
}

function CurrentParameters({ values }: { values: ParametersResponse['parameters']['current'] }) {
  const groups = values.reduce((result, item) => {
    const group = readString(item.definition.group) ?? 'Other'
    const items = result.get(group) ?? []
    items.push(item)
    result.set(group, items)
    return result
  }, new Map<string, ParametersResponse['parameters']['current']>())
  return (
    <div className="space-y-5">
      {[...groups.entries()].map(([group, items]) => (
        <Section key={group} title={group}>
          <div className="overflow-x-auto">
            <table className="w-full min-w-[1120px] text-left text-sm">
              <thead><tr className="border-b border-brand-border/20 text-xs uppercase tracking-wide text-content-tertiary"><th className="px-5 py-3">Parameter</th><th className="px-5 py-3">Active value</th><th className="px-5 py-3">Meaning</th><th className="px-5 py-3">Risk direction</th><th className="px-5 py-3">Source</th></tr></thead>
              <tbody>{items.map((item) => {
                const definition = item.definition
                const key = readString(definition.key) ?? 'unknown'
                const rawScale = readString(definition.rawScale)
                const displayUnit = readString(definition.displayUnit)
                const mutability = readString(definition.mutability)
                const timelockPolicy = readString(definition.timelockPolicy)
                const sourceContract = readString(definition.sourceContract)
                const getter = readString(definition.getter)
                return (
                  <tr key={key} className="border-b border-brand-border/10 align-top">
                    <td className="px-5 py-4"><p className="font-semibold">{humanize(key.split('.').at(-1) ?? key)}</p><code className="mt-1 block text-xs text-content-tertiary">{key}</code></td>
                    <td className="px-5 py-4">
                      <p className="font-semibold tabular-nums"><DisplayValue value={item.formattedValue} /></p>
                      <dl className="mt-2 grid gap-1 text-xs text-content-tertiary">
                        <div><dt className="inline font-semibold">Raw value:</dt> <dd className="inline font-mono">{item.rawValue ?? 'Unavailable'}</dd></div>
                        <div><dt className="inline font-semibold">Raw scale:</dt> <dd className="inline font-mono">{rawScale ?? 'Unavailable'}</dd></div>
                        <div><dt className="inline font-semibold">Display unit:</dt> <dd className="inline">{displayUnit ?? 'Unavailable'}</dd></div>
                      </dl>
                      <div className="mt-2"><EvidenceBadge level={item.evidence} /></div>
                      {item.availability.length > 0 ? (
                        <ul aria-label={`${key} availability`} className="mt-2 space-y-1 text-xs text-brand-yellow">
                          {item.availability.map((reason) => (
                            <li key={`${reason.field}:${reason.reason}`}>
                              {reason.field}: <code>{reason.reason}</code>
                            </li>
                          ))}
                        </ul>
                      ) : null}
                    </td>
                    <td className="max-w-xs px-5 py-4 leading-6 text-content-secondary">{readString(definition.description)}</td>
                    <td className="max-w-xs px-5 py-4 leading-6 text-content-secondary">{readString(definition.riskInterpretation)}</td>
                    <td className="min-w-72 px-5 py-4">
                      <code className="break-all text-xs text-brand-peach">
                        {sourceContract ?? 'Unknown contract'}.{getter ?? 'unknown getter'}
                      </code>
                      <dl className="mt-3 grid gap-2 text-xs text-content-tertiary">
                        <div>
                          <dt className="font-semibold text-content-secondary">Source address</dt>
                          <dd className="mt-0.5 break-all font-mono">{item.sourceAddress ?? 'Unavailable'}</dd>
                        </div>
                        <div><dt className="inline font-semibold text-content-secondary">Effective block:</dt> <dd className="inline"> {item.effectiveBlock}</dd></div>
                        <div><dt className="inline font-semibold text-content-secondary">Mutability:</dt> <dd className="inline"> {mutability ? humanize(mutability) : 'Unavailable'}</dd></div>
                        <div><dt className="inline font-semibold text-content-secondary">Timelock:</dt> <dd className="inline"> {timelockPolicy ? humanize(timelockPolicy) : 'Unavailable'}</dd></div>
                      </dl>
                      <div className="mt-3">
                        <DocumentationLink value={definition.documentationLink} />
                      </div>
                    </td>
                  </tr>
                )
              })}</tbody>
            </table>
          </div>
        </Section>
      ))}
    </div>
  )
}

function ChangeList({
  title,
  values,
  empty,
  newChangeIds,
  releaseId,
}: {
  title: string
  values: Record<string, unknown>[]
  empty: string
  newChangeIds: ReadonlySet<string>
  releaseId: string
}) {
  return (
    <Section title={title}>
      {values.length === 0 ? <EmptyState title="Nothing to show" message={empty} /> : (
        <div className="divide-y divide-brand-border/15">{values.map((value, index) => {
          const identity = changeIdentity(value)
          const isNew = identity !== null && newChangeIds.has(identity)
          const txHash = readString(value.txHash)
          const status = readString(value.status)
          const evidence = value.evidence ?? value.provenance
          return (
            <div key={identity ?? String(index)} className={isNew ? 'bg-brand-peach/[0.04]' : undefined}>
              <div className="px-5 py-4">
                <div className="flex flex-wrap items-center gap-2">
                  <p className="font-semibold">{readString(value.parameterKey) ?? 'Governance action'}</p>
                  <GovernanceStatusBadge status={status} />
                  <span className="text-xs text-content-tertiary">Evidence</span>
                  <EvidenceBadge level={evidence} />
                  {isNew ? <span className="rounded-full border border-brand-peach/35 px-2 py-0.5 text-[11px] font-semibold text-brand-peach">New since last view</span> : null}
                </div>
                <dl className="mt-4 grid gap-x-6 gap-y-3 text-sm sm:grid-cols-2 xl:grid-cols-4">
                  <ChangeField label="Old value"><DisplayValue value={value.oldValue} /></ChangeField>
                  <ChangeField label="New value"><DisplayValue value={value.newValue} /></ChangeField>
                  <ChangeField label="Proposer"><DisplayValue value={value.proposer} /></ChangeField>
                  <ChangeField label="Executor"><DisplayValue value={value.executor} /></ChangeField>
                  <ChangeField label="Proposed at">{formatTimestamp(readString(value.proposedAt))}</ChangeField>
                  <ChangeField label="ETA">{formatTimestamp(readString(value.eta))}</ChangeField>
                  <ChangeField label="Timelock countdown"><TimelockCountdown value={value.countdownSeconds} /></ChangeField>
                  <ChangeField label="Executed at">{formatTimestamp(readString(value.executedAt) ?? readString(value.terminalAt))}</ChangeField>
                  {value.provenance !== undefined ? (
                    <ChangeField label="Provenance"><DisplayValue value={value.provenance} /></ChangeField>
                  ) : null}
                  <ChangeField label="Transaction">
                    {isTransactionHash(txHash)
                      ? <Link className="break-all font-mono text-brand-peach hover:underline" to={`/transactions/${encodeURIComponent(txHash)}?release=${encodeURIComponent(releaseId)}`}>{txHash}</Link>
                      : <span className="text-content-tertiary">Unavailable</span>}
                  </ChangeField>
                </dl>
              </div>
              <RawEvidence value={value} title="Change evidence" />
            </div>
          )
        })}</div>
      )}
    </Section>
  )
}

function ChangeField({ label, children }: { label: string; children: ReactNode }) {
  return (
    <div>
      <dt className="text-xs uppercase tracking-wide text-content-tertiary">{label}</dt>
      <dd className="mt-1 break-words text-content-secondary">{children}</dd>
    </div>
  )
}

function GovernanceStatusBadge({ status }: { status: string | null }) {
  const normalized = status?.toLowerCase() ?? 'unavailable'
  const className = normalized === 'executed'
    ? 'border-positive/35 bg-positive/10 text-positive'
    : ['pending', 'proposed', 'ready', 'overdue'].includes(normalized)
      ? 'border-brand-yellow/40 bg-brand-yellow/10 text-brand-yellow'
      : normalized === 'cancelled' || normalized === 'superseded'
        ? 'border-brand-orange/35 bg-brand-orange/10 text-brand-orange'
        : 'border-content-tertiary/30 bg-white/5 text-content-tertiary'
  return (
    <span
      aria-label={`Status: ${humanize(normalized)}`}
      className={`inline-flex rounded-full border px-2 py-0.5 text-[11px] font-semibold ${className}`}
    >
      {humanize(normalized)}
    </span>
  )
}

function TimelockCountdown({ value }: { value: unknown }) {
  const text = readString(value)
  if (text === null || !/^\d+$/.test(text)) {
    return <span className="text-content-tertiary">Unavailable</span>
  }
  const seconds = Number(text)
  if (!Number.isSafeInteger(seconds)) {
    return <span className="font-mono">{text} seconds</span>
  }
  if (seconds === 0) {
    return (
      <>
        <span className="font-semibold text-brand-yellow">Ready now</span>
        <span className="mt-0.5 block text-xs text-content-tertiary">0 seconds at confirmed block</span>
      </>
    )
  }
  return (
    <>
      <span>{formatDuration(seconds)} remaining</span>
      <span className="mt-0.5 block text-xs text-content-tertiary">{seconds.toLocaleString()} seconds at confirmed block</span>
    </>
  )
}

function formatDuration(totalSeconds: number): string {
  const days = Math.floor(totalSeconds / 86_400)
  const hours = Math.floor((totalSeconds % 86_400) / 3_600)
  const minutes = Math.floor((totalSeconds % 3_600) / 60)
  const seconds = totalSeconds % 60
  return [
    days > 0 ? `${String(days)}d` : null,
    hours > 0 ? `${String(hours)}h` : null,
    minutes > 0 ? `${String(minutes)}m` : null,
    seconds > 0 ? `${String(seconds)}s` : null,
  ].filter((value): value is string => value !== null).join(' ')
}

function DocumentationLink({ value }: { value: unknown }) {
  const href = safeDocumentationHref(value)
  if (href === null) {
    return <span className="text-xs text-content-tertiary">Documentation unavailable</span>
  }
  const className = 'text-xs font-semibold text-brand-peach hover:underline'
  if (href.startsWith('/') || href.startsWith('#')) {
    return <Link className={className} to={href}>Parameter documentation</Link>
  }
  return <a className={className} href={href} target="_blank" rel="noreferrer noopener">Parameter documentation</a>
}

function safeDocumentationHref(value: unknown): string | null {
  const href = readString(value)?.trim()
  if (!href || hasAsciiControlCharacter(href)) return null
  if (href.startsWith('/') && !href.startsWith('//')) return href
  if (href.startsWith('#')) return href
  try {
    const parsed = new URL(href)
    return parsed.protocol === 'https:' ? parsed.toString() : null
  } catch {
    return null
  }
}

function hasAsciiControlCharacter(value: string): boolean {
  for (let index = 0; index < value.length; index += 1) {
    const codeUnit = value.charCodeAt(index)
    if (codeUnit <= 31 || codeUnit === 127) return true
  }
  return false
}

function isTransactionHash(value: string | null): value is string {
  return value !== null && /^0x[a-fA-F0-9]{64}$/.test(value)
}

function changeIdentity(value: Record<string, unknown>): string | null {
  const explicitId = readString(value.changeId)
  if (explicitId !== null) return explicitId
  const parts = [
    readString(value.parameterKey),
    readString(value.status),
    readString(value.txHash),
    readString(value.eta),
  ]
  return parts.some((part) => part !== null)
    ? parts.map((part) => part ?? '').join('|')
    : null
}

function parameterChangeIdentity(value: Record<string, unknown>): string {
  return changeIdentity(value) ?? JSON.stringify(value)
}

function readSeenChanges(storageKey: string): Set<string> | null {
  try {
    const stored = localStorage.getItem(storageKey)
    if (stored === null) return null
    const parsed = JSON.parse(stored) as unknown
    if (!Array.isArray(parsed) || !parsed.every((value) => typeof value === 'string')) return null
    return new Set(parsed)
  } catch {
    return null
  }
}

function writeSeenChanges(storageKey: string, serializedIdentities: string): boolean {
  try {
    localStorage.setItem(storageKey, serializedIdentities)
    return true
  } catch {
    return false
  }
}
