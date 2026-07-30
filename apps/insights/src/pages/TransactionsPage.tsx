import type { SyntheticEvent } from 'react'
import { useSearchParams } from 'react-router-dom'
import { useCurrentProtocolRelease, useProtocolTransactions } from '../api'
import { ActionTable, AvailabilityList, PageTitle, ProtocolMeta, Section } from '../components/Protocol'
import { ErrorState, LoadingState, Panel } from '../components/ui'

const ACTION_TYPES = [
  ['', 'All actions'],
  ['order_commitment', 'Order commitment'],
  ['order_execution', 'Order execution'],
  ['order_cleanup', 'Order cleanup / failure'],
  ['position_open', 'Position open / increase'],
  ['position_close', 'Position reduction / close'],
  ['liquidation', 'Liquidation'],
  ['margin_add', 'Margin action'],
] as const

export function TransactionsPage() {
  const [searchParams, setSearchParams] = useSearchParams()
  const optionalParam = (name: string) => {
    const value = searchParams.get(name)
    return value === null || value === '' ? undefined : value
  }
  const release = useCurrentProtocolRelease()
  const requestedRelease = optionalParam('release')?.trim()
  const requestedReleaseId = requestedRelease === undefined || requestedRelease.length === 0
    ? 'current'
    : requestedRelease
  const resolvingCurrentRelease = requestedReleaseId === 'current'
  const releaseId = resolvingCurrentRelease
    ? release.data?.releaseId ?? ''
    : requestedReleaseId
  const filterKey = searchParams.toString()
  const filters = {
    actionType: optionalParam('actionType'),
    outcome: optionalParam('outcome'),
    address: optionalParam('address'),
    account: optionalParam('account'),
    keeper: optionalParam('keeper'),
    contract: optionalParam('contract'),
    transactionHash: optionalParam('transactionHash'),
    from: optionalParam('from'),
    to: optionalParam('to'),
    limit: 50,
  }
  const query = useProtocolTransactions(releaseId, filters, filterKey)

  function applyFilters(event: SyntheticEvent<HTMLFormElement>) {
    event.preventDefault()
    const form = new FormData(event.currentTarget)
    const next = new URLSearchParams()
    const rawRelease = form.get('release')
    const selectedRelease = typeof rawRelease === 'string' ? rawRelease.trim() : ''
    if (selectedRelease && selectedRelease !== 'current') next.set('release', selectedRelease)
    for (const key of ['actionType', 'outcome', 'address', 'account', 'keeper', 'contract', 'transactionHash', 'from', 'to']) {
      const rawValue = form.get(key)
      const value = typeof rawValue === 'string' ? rawValue.trim() : ''
      if (value) next.set(key, value)
    }
    setSearchParams(next)
  }

  const actions = query.data?.pages.flatMap((page) => page.transactions.items) ?? []
  const envelope = query.data?.pages[0]

  return (
    <div className="space-y-7">
      <PageTitle title="Transactions and protocol actions" description="Filter the confirmed activity ledger by action, outcome, participant, contract, date, or canonical transaction hash. Batch transactions remain grouped on their transaction detail page." />
      <Panel>
        <form onSubmit={applyFilters} className="grid gap-3 p-4 sm:grid-cols-2 lg:grid-cols-4" aria-label="Transaction filters">
          <Filter label="Release" name="release" defaultValue={requestedReleaseId} placeholder="current or release ID" />
          <Filter label="Action type" name="actionType" type="select" defaultValue={filters.actionType ?? ''} options={ACTION_TYPES} />
          <Filter
            label="Action state"
            name="outcome"
            type="select"
            defaultValue={filters.outcome ?? ''}
            options={[
              ['', 'All action states'],
              ['success', 'Successful onchain action'],
              ['pending', 'Pending commitment'],
            ]}
          />
          <Filter label="Account or keeper" name="address" defaultValue={filters.address ?? ''} placeholder="0x…" />
          <Filter label="Trading account" name="account" defaultValue={filters.account ?? ''} placeholder="0x…" />
          <Filter label="Keeper" name="keeper" defaultValue={filters.keeper ?? ''} placeholder="0x…" />
          <Filter label="Contract" name="contract" defaultValue={filters.contract ?? ''} placeholder="0x…" />
          <Filter label="Transaction hash" name="transactionHash" defaultValue={filters.transactionHash ?? ''} placeholder="0x…" />
          <Filter label="From · Unix seconds" name="from" defaultValue={filters.from ?? ''} placeholder="1785000000" />
          <Filter label="To · Unix seconds" name="to" defaultValue={filters.to ?? ''} placeholder="1786000000" />
          <div className="flex gap-2 sm:col-span-2 lg:col-span-4">
            <button type="submit" className="border border-brand-orange bg-brand-orange px-5 py-2.5 text-sm font-semibold hover:bg-brand-peach hover:text-app-bg">Apply filters</button>
            <button type="button" onClick={() => { setSearchParams({}); }} className="border border-brand-border/35 px-5 py-2.5 text-sm font-semibold hover:border-brand-peach">Clear</button>
          </div>
        </form>
      </Panel>

      {envelope ? <><ProtocolMeta data={envelope} /><AvailabilityList items={envelope.availability} /></> : null}
      {resolvingCurrentRelease && release.isError ? <ErrorState message={release.error.message} /> : query.isLoading ? <Panel><LoadingState rows={8} /></Panel> : query.isError ? (
        <ErrorState title="Activity feed unavailable" message={query.error.message} onRetry={() => void query.refetch()} />
      ) : (
        <Section title="Confirmed activity" description={`${String(actions.length)} action rows loaded. Select a transaction to see its receipt, calldata, raw logs, and every projected action.`}>
          <ActionTable actions={actions} releaseId={envelope?.releaseId ?? releaseId} />
        </Section>
      )}
      {query.hasNextPage ? <div className="text-center"><button type="button" onClick={() => void query.fetchNextPage()} disabled={query.isFetchingNextPage} className="border border-brand-border/40 px-5 py-2.5 text-sm font-semibold hover:border-brand-orange">{query.isFetchingNextPage ? 'Loading…' : 'Load more'}</button></div> : null}
    </div>
  )
}

function Filter({
  label,
  name,
  defaultValue,
  placeholder,
  type,
  options,
}: {
  label: string
  name: string
  defaultValue: string
  placeholder?: string
  type?: 'select'
  options?: readonly (readonly [string, string])[]
}) {
  return (
    <label className="text-xs font-semibold text-content-secondary">
      {label}
      {type === 'select' ? (
        <select key={defaultValue} name={name} defaultValue={defaultValue} className="mt-1.5 w-full border border-brand-border/30 bg-app-bg px-3 py-2.5 text-sm text-content-primary">
          {options?.map(([value, text]) => <option key={value} value={value}>{text}</option>)}
        </select>
      ) : (
        <input key={defaultValue} name={name} defaultValue={defaultValue} placeholder={placeholder} autoComplete="off" className="mt-1.5 w-full border border-brand-border/30 bg-app-bg px-3 py-2.5 font-mono text-sm text-content-primary placeholder:text-content-tertiary" />
      )}
    </label>
  )
}
