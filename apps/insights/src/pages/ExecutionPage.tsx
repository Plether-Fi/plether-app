import { Link } from 'react-router-dom'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { StatCard } from '../components/StatCard'
import { Panel } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'

export function ExecutionPage() {
  const { chain, releaseVersion, blockNumber, observedAt } = useProtocolContext()

  return (
    <div className="space-y-8">
      <DataFreshnessBanner
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={releaseVersion}
        blockNumber={blockNumber}
        observedAt={observedAt}
        tone="unknown"
        statusLabel="Stage 2 — Execution Quality API pending"
        note="Backed by /api/insights/v1/execution/{quality,trades}"
      />

      <header className="flex flex-col justify-between gap-4 sm:flex-row sm:items-end">
        <div className="max-w-3xl space-y-3">
          <p className="text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">
            Trader Execution &amp; Market Forensics
          </p>
          <h1 className="text-3xl font-semibold tracking-tight sm:text-4xl">
            Order Reliability, Protections, Liquidations &amp; Trade Tape
          </h1>
          <p className="text-sm leading-6 text-content-secondary">
            Protocol-wide execution telemetry beyond the main app’s price chart and trade ticket—
            auditing keeper fill rates, commit-to-execution latency, TP/SL trigger reliability,
            liquidation flow, and receipt-level execution costs across every Trading Account.
          </p>
        </div>
        <a
          href={chain.tradeAppUrl}
          className="shrink-0 border border-brand-border/40 px-4 py-2.5 text-sm font-semibold text-brand-peach hover:bg-brand-peach/10"
        >
          Open Trading App ↗
        </a>
      </header>

      <section aria-labelledby="execution-kpis-title" className="space-y-4">
        <h2 id="execution-kpis-title" className="text-xl font-semibold">
          Execution quality snapshot
        </h2>
        <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">
          <StatCard
            label="Order fill rate (24h)"
            unavailable
            detail="Canonical keeper cohort (shifted 120s for settlement)"
          />
          <StatCard
            label="Execution latency (p50 / p95)"
            unavailable
            detail="Commit block to execution block & seconds"
          />
          <StatCard
            label="TP/SL protection health"
            unavailable
            detail="Active protections, trigger rate & latency"
          />
          <StatCard
            label="Liquidations (24h)"
            unavailable
            detail="Liquidated notional, charges & keeper freshness"
          />
        </div>
      </section>

      <div className="grid gap-4 lg:grid-cols-2">
        <Panel className="p-6">
          <h2 className="text-lg font-semibold">Keeper cohorts &amp; protection reliability</h2>
          <p className="mt-2 text-sm leading-6 text-content-secondary">
            Applies the canonical counting contract from{' '}
            <code className="font-mono text-xs text-brand-peach">perps_keeper_orders</code> and{' '}
            <code className="font-mono text-xs text-brand-peach">perps_protection_*</code> to
            separate genuine order expirations (<code className="font-mono text-xs">terminal_reason = 2</code>),
            pending queue depth, and TP/SL trigger performance without mixing stale cohorts.
          </p>
        </Panel>
        <Panel className="p-6">
          <h2 className="text-lg font-semibold">Protocol-wide receipt trade tape</h2>
          <p className="mt-2 text-sm leading-6 text-content-secondary">
            Streams executed opens, closes, and liquidations across all Trading Accounts with
            expandable receipt breakdowns (VPI charge vs. rebate, protocol fee, waivers, and exact
            account change).
          </p>
          <div className="mt-5 flex flex-wrap gap-4 text-sm font-semibold">
            <Link to="/divisions" className="text-brand-peach hover:underline">
              View live Divisions rankings →
            </Link>
            <Link to="/" className="text-content-secondary hover:text-brand-peach">
              ← Back to Protocol Pulse
            </Link>
          </div>
        </Panel>
      </div>
    </div>
  )
}
