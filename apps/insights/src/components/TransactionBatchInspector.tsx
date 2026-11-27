import { useState } from 'react'
import { Link } from 'react-router-dom'
import type {
  FinalizedOrderRecord,
  ProtocolEventRecord,
  TransactionBatchSummary,
  TxLpSettlementRecord,
} from '../api'
import { explorerAddressUrl, explorerTxUrl } from '../utils/chain'
import { formatCompactAssets, formatPrice, formatUtc, shortAddress } from '../utils/format'
import { StatusPill, type StatusTone } from './StatusPill'
import { Panel } from './ui'

function orderStatusTone(status: string): StatusTone {
  const lower = status.toLowerCase()
  if (lower === 'executed') return 'ready'
  if (lower === 'expired' || lower === 'cancelled') return 'degraded'
  if (lower === 'failed' || lower === 'reverted') return 'blocked'
  return 'unknown'
}

function summarizePayload(data: Record<string, unknown>): string {
  const entries = Object.entries(data).slice(0, 4)
  if (entries.length === 0) return '—'
  return entries
    .map(([k, v]) => {
      const rendered =
        v == null
          ? 'null'
          : typeof v === 'string'
            ? v.length > 18 && v.startsWith('0x')
              ? shortAddress(v)
              : v.slice(0, 28)
            : typeof v === 'number' || typeof v === 'boolean'
              ? String(v)
              : '{…}'
      return `${k}=${rendered}`
    })
    .join(' · ')
}

export function BatchFinalizationPanel({
  batchSummary,
  finalizedOrders,
  chainId,
  activeOrderId,
  onInspectOrder,
}: {
  batchSummary: TransactionBatchSummary | null | undefined
  finalizedOrders: FinalizedOrderRecord[]
  chainId: number | null | undefined
  activeOrderId: string | null
  onInspectOrder?: (orderId: string) => void
}) {
  if (finalizedOrders.length === 0) return null

  const isBatch = finalizedOrders.length > 1
  const executed =
    batchSummary?.executedOrders ??
    finalizedOrders.filter((o) => o.terminalStatus === 'Executed').length
  const nonExecuted = finalizedOrders.length - executed

  return (
    <section aria-labelledby="tx-batch-title" className="space-y-4">
      <div className="flex flex-wrap items-center justify-between gap-2">
        <div>
          <h2 id="tx-batch-title" className="text-lg font-semibold text-content-primary">
            {isBatch
              ? `Batch finalization manifest (${String(finalizedOrders.length)} orders)`
              : 'Order finalization manifest'}
          </h2>
          <p className="mt-0.5 text-xs text-content-tertiary">
            Protocol-level OrderRouter finalization trace, commit-to-finalize latency, and Pyth
            oracle settlement window
          </p>
        </div>
        <StatusPill
          tone={nonExecuted === 0 ? 'ready' : executed > 0 ? 'degraded' : 'blocked'}
          label={
            nonExecuted === 0
              ? `${String(executed)}/${String(finalizedOrders.length)} executed`
              : `${String(executed)} executed · ${String(nonExecuted)} non-filled`
          }
        />
      </div>

      {batchSummary ? (
        <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
          <Panel className="p-4">
            <p className="text-xs text-content-tertiary">Orders &amp; accounts</p>
            <p className="mt-1 font-mono text-base font-semibold tabular-nums text-content-primary">
              {batchSummary.executedOrders}/{batchSummary.totalOrders} executed
            </p>
            <p className="mt-1 font-mono text-[11px] text-content-secondary">
              {batchSummary.uniqueAccounts}{' '}
              {batchSummary.uniqueAccounts === 1 ? 'account' : 'accounts'} ·{' '}
              {batchSummary.failedOrExpiredOrders} expired/failed
            </p>
          </Panel>

          <Panel className="p-4">
            <p className="text-xs text-content-tertiary">Commit → Finalize latency</p>
            <p className="mt-1 font-mono text-base font-semibold tabular-nums text-content-primary">
              {batchSummary.medianLatencySeconds != null
                ? `${String(batchSummary.medianLatencySeconds)}s p50`
                : '—'}
            </p>
            <p className="mt-1 font-mono text-[11px] text-content-secondary">
              {batchSummary.minLatencySeconds != null && batchSummary.maxLatencySeconds != null
                ? `min ${String(batchSummary.minLatencySeconds)}s · max ${String(batchSummary.maxLatencySeconds)}s`
                : 'No commit timestamp recorded'}
            </p>
          </Panel>

          <Panel className="p-4">
            <p className="text-xs text-content-tertiary">Pyth oracle settlement</p>
            <p className="mt-1 font-mono text-base font-semibold tabular-nums text-content-primary">
              {batchSummary.oraclePrice ? formatPrice(batchSummary.oraclePrice) : '—'}
            </p>
            <p className="mt-1 font-mono text-[11px] text-content-secondary">
              {batchSummary.oracleFrozen ? 'Oracle frozen' : 'Live mark'}
              {batchSummary.oracleSpreadSeconds != null
                ? ` · ${String(batchSummary.oracleSpreadSeconds)}s feed spread`
                : ''}
            </p>
          </Panel>

          <Panel className="p-4">
            <p className="text-xs text-content-tertiary">Batch VPI collected</p>
            <p className="mt-1 font-mono text-base font-semibold tabular-nums text-content-primary">
              {formatCompactAssets(batchSummary.totalVpiAssets)}
            </p>
            <p className="mt-1 font-mono text-[11px] text-content-secondary">
              {batchSummary.oracleMaxPublishTime
                ? `Oracle ${formatUtc(batchSummary.oracleMaxPublishTime)}`
                : 'Protocol VPI across finalized orders'}
            </p>
          </Panel>
        </div>
      ) : null}

      <Panel className="overflow-x-auto">
        <table className="w-full min-w-[860px] text-left text-xs">
          <thead className="border-b border-brand-border/25 bg-app-bg/40 text-[11px] uppercase tracking-[0.1em] text-content-tertiary">
            <tr>
              <th scope="col" className="px-4 py-3">Order</th>
              <th scope="col" className="px-4 py-3">Account</th>
              <th scope="col" className="px-4 py-3">Side</th>
              <th scope="col" className="px-4 py-3">Outcome</th>
              <th scope="col" className="px-4 py-3">Commit → Finalize</th>
              <th scope="col" className="px-4 py-3">Exec / Oracle price</th>
              <th scope="col" className="px-4 py-3">VPI</th>
              <th scope="col" className="px-4 py-3">Notes / Action</th>
            </tr>
          </thead>
          <tbody className="divide-y divide-brand-border/15">
            {finalizedOrders.map((order) => {
              const isSelected = activeOrderId != null && activeOrderId === order.orderId
              const reasonText =
                order.failedConstraint ?? order.failureReason ?? order.terminalReason ?? null
              return (
                <tr
                  key={order.orderId}
                  className={isSelected ? 'bg-brand-orange/10' : 'hover:bg-app-bg/35'}
                >
                  <td className="px-4 py-3 font-mono font-semibold text-content-primary">
                    #{order.orderId}
                  </td>
                  <td className="px-4 py-3 font-mono">
                    {order.account ? (
                      <Link
                        to={`/wallets/${order.account.toLowerCase()}`}
                        className="text-brand-peach hover:underline"
                      >
                        {shortAddress(order.account)}
                      </Link>
                    ) : (
                      '—'
                    )}
                  </td>
                  <td className="px-4 py-3 font-mono uppercase">
                    {order.side ? (
                      <span
                        className={
                          order.side === 'long'
                            ? 'text-positive'
                            : order.side === 'short'
                              ? 'text-brand-orange'
                              : 'text-content-secondary'
                        }
                      >
                        {order.side}
                      </span>
                    ) : (
                      '—'
                    )}
                  </td>
                  <td className="px-4 py-3">
                    <StatusPill
                      tone={orderStatusTone(order.terminalStatus)}
                      label={order.terminalStatus}
                    />
                  </td>
                  <td className="px-4 py-3 font-mono tabular-nums">
                    <div>
                      {order.latencySeconds != null ? `${String(order.latencySeconds)}s` : '—'}
                      {order.latencyBlocks != null
                        ? ` (${String(order.latencyBlocks)} blk)`
                        : ''}
                    </div>
                    {order.commitTxHash ? (
                      <div className="mt-0.5 text-[11px] text-content-tertiary">
                        Commit{' '}
                        <a
                          href={explorerTxUrl(order.commitTxHash, chainId)}
                          target="_blank"
                          rel="noreferrer"
                          className="text-brand-peach hover:underline"
                        >
                          {shortAddress(order.commitTxHash)} ↗
                        </a>
                      </div>
                    ) : null}
                  </td>
                  <td className="px-4 py-3 font-mono tabular-nums">
                    <div>{order.executionPrice ? formatPrice(order.executionPrice) : '—'}</div>
                    {order.oraclePrice ? (
                      <div className="mt-0.5 text-[11px] text-content-tertiary">
                        Oracle {formatPrice(order.oraclePrice)}
                      </div>
                    ) : null}
                  </td>
                  <td className="px-4 py-3 font-mono tabular-nums text-content-secondary">
                    {order.vpiAssets ? formatCompactAssets(order.vpiAssets) : '—'}
                  </td>
                  <td className="px-4 py-3">
                    <div className="flex flex-wrap items-center gap-2">
                      {reasonText && order.terminalStatus !== 'Executed' ? (
                        <span className="font-mono text-[11px] text-brand-yellow">
                          {reasonText}
                        </span>
                      ) : null}
                      {onInspectOrder && order.terminalStatus === 'Executed' ? (
                        <button
                          type="button"
                          onClick={() => {
                            onInspectOrder(order.orderId)
                          }}
                          title={
                            isSelected
                              ? 'Currently inspecting this fill — click to jump to breakdown above'
                              : `Switch execution breakdown above to Order #${order.orderId}`
                          }
                          className={`cursor-pointer border px-2 py-0.5 text-[11px] font-semibold transition-colors ${
                            isSelected
                              ? 'border-brand-orange bg-brand-orange text-content-primary'
                              : 'border-brand-border/35 text-brand-peach hover:border-brand-peach hover:bg-brand-peach/10'
                          }`}
                        >
                          {isSelected ? 'Viewing ↑' : 'Inspect fill'}
                        </button>
                      ) : null}
                    </div>
                  </td>
                </tr>
              )
            })}
          </tbody>
        </table>
      </Panel>
    </section>
  )
}

export function TxLpSettlementPanel({ settlement }: { settlement: TxLpSettlementRecord }) {
  const flags: string[] = []
  if (settlement.seniorBacklog) flags.push('Senior backlog')
  if (settlement.juniorBacklog) flags.push('Junior backlog')
  if (settlement.entriesDeferred) flags.push('Entries deferred')

  return (
    <section aria-labelledby="tx-lp-settlement-title" className="space-y-3">
      <div className="flex flex-wrap items-center justify-between gap-2">
        <h2 id="tx-lp-settlement-title" className="text-lg font-semibold text-content-primary">
          LP Epoch Settlement
        </h2>
        <Link
          to={`/liquidity/epochs/${String(settlement.epoch)}`}
          className="font-mono text-xs font-semibold text-brand-peach hover:underline"
        >
          Open Epoch #{settlement.epoch.toLocaleString('en-US')} →
        </Link>
      </div>
      <Panel className="grid gap-4 p-5 sm:grid-cols-2 lg:grid-cols-4">
        <div>
          <p className="text-xs text-content-tertiary">Epoch &amp; status</p>
          <p className="mt-1 font-mono text-sm font-semibold text-content-primary">
            #{settlement.epoch.toLocaleString('en-US')}
          </p>
          <div className="mt-1.5">
            <StatusPill
              tone={settlement.status === 'confirmed_success' ? 'ready' : 'degraded'}
              label={settlement.status}
            />
          </div>
        </div>
        <div>
          <p className="text-xs text-content-tertiary">Senior flows (dep / red)</p>
          <p className="mt-1 font-mono text-sm font-semibold tabular-nums text-content-primary">
            {formatCompactAssets(settlement.seniorDepositAssets)} /{' '}
            {formatCompactAssets(settlement.seniorRedeemAssets)}
          </p>
        </div>
        <div>
          <p className="text-xs text-content-tertiary">Junior flows (dep / red)</p>
          <p className="mt-1 font-mono text-sm font-semibold tabular-nums text-content-primary">
            {formatCompactAssets(settlement.juniorDepositAssets)} /{' '}
            {formatCompactAssets(settlement.juniorRedeemAssets)}
          </p>
        </div>
        <div>
          <p className="text-xs text-content-tertiary">Settlement flags</p>
          <p className="mt-1 text-sm text-content-secondary">
            {flags.length > 0 ? flags.join(' · ') : 'Clean settlement'}
          </p>
        </div>
      </Panel>
    </section>
  )
}

export function ProtocolEventTracePanel({
  protocolEvents,
  chainId,
}: {
  protocolEvents: ProtocolEventRecord[]
  chainId: number | null | undefined
}) {
  const [expandedLog, setExpandedLog] = useState<number | null>(null)
  if (protocolEvents.length === 0) return null

  return (
    <section aria-labelledby="tx-protocol-events-title" className="space-y-3">
      <div>
        <h2 id="tx-protocol-events-title" className="text-lg font-semibold text-content-primary">
          On-chain protocol event trace ({protocolEvents.length})
        </h2>
        <p className="mt-0.5 text-xs text-content-tertiary">
          Ordered contract logs emitted inside this transaction by log index
        </p>
      </div>
      <Panel className="overflow-x-auto">
        <table className="w-full min-w-[760px] text-left text-xs">
          <thead className="border-b border-brand-border/25 bg-app-bg/40 text-[11px] uppercase tracking-[0.1em] text-content-tertiary">
            <tr>
              <th scope="col" className="px-4 py-3">Log #</th>
              <th scope="col" className="px-4 py-3">Event</th>
              <th scope="col" className="px-4 py-3">Contract</th>
              <th scope="col" className="px-4 py-3">Account</th>
              <th scope="col" className="px-4 py-3">Order</th>
              <th scope="col" className="px-4 py-3">Decoded data</th>
            </tr>
          </thead>
          <tbody className="divide-y divide-brand-border/15">
            {protocolEvents.map((ev) => {
              const isExpanded = expandedLog === ev.logIndex
              return (
                <tr key={ev.logIndex} className="align-top hover:bg-app-bg/35">
                  <td className="px-4 py-3 font-mono text-content-tertiary">#{ev.logIndex}</td>
                  <td className="px-4 py-3 font-mono font-semibold text-content-primary">
                    {ev.eventName}
                  </td>
                  <td className="px-4 py-3 font-mono">
                    <a
                      href={explorerAddressUrl(ev.contractAddress, chainId)}
                      target="_blank"
                      rel="noreferrer"
                      className="text-brand-peach hover:underline"
                    >
                      {shortAddress(ev.contractAddress)} ↗
                    </a>
                  </td>
                  <td className="px-4 py-3 font-mono">
                    {ev.account ? (
                      <Link
                        to={`/wallets/${ev.account.toLowerCase()}`}
                        className="text-brand-peach hover:underline"
                      >
                        {shortAddress(ev.account)}
                      </Link>
                    ) : (
                      '—'
                    )}
                  </td>
                  <td className="px-4 py-3 font-mono text-content-secondary">
                    {ev.orderId ? `#${ev.orderId}` : '—'}
                  </td>
                  <td className="px-4 py-3 font-mono text-[11px] text-content-secondary">
                    <div className="flex items-start justify-between gap-2">
                      <span className="break-all">{summarizePayload(ev.data)}</span>
                      <button
                        type="button"
                        onClick={() => {
                          setExpandedLog((prev) => (prev === ev.logIndex ? null : ev.logIndex))
                        }}
                        className="shrink-0 text-[10px] font-semibold text-brand-peach hover:underline"
                      >
                        {isExpanded ? 'Hide JSON' : 'JSON'}
                      </button>
                    </div>
                    {isExpanded ? (
                      <pre className="mt-2 max-h-48 overflow-auto border border-brand-border/20 bg-app-bg p-2.5 text-[11px] text-content-primary">
                        {JSON.stringify(ev.data, null, 2)}
                      </pre>
                    ) : null}
                  </td>
                </tr>
              )
            })}
          </tbody>
        </table>
      </Panel>
    </section>
  )
}
