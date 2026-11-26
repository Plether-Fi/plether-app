import { Link } from 'react-router-dom'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { StatCard } from '../components/StatCard'
import { Panel } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'

const ACTION_CARDS = [
  { label: 'Deposit margin', detail: 'Paymaster, Alto bundler & RPC readiness' },
  { label: 'Open position', detail: 'Keeper, Pyth oracle freshness & AA pipeline' },
  { label: 'Close position', detail: 'Keeper & frozen-close fallback readiness' },
  { label: 'TP/SL protection', detail: 'Protection worker & keeper execution path' },
]

export function InfraPage() {
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
        statusLabel="Stage 5 — Infra Status API pending"
        note="Backed by /api/insights/v1/infra/status (public-safe sanitized telemetry)"
      />

      <header className="max-w-3xl space-y-3">
        <p className="text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">
          Infrastructure &amp; Reliability
        </p>
        <h1 className="text-3xl font-semibold tracking-tight sm:text-4xl">
          Action Readiness, Workers, AA &amp; RPC Health
        </h1>
        <p className="text-sm leading-6 text-content-secondary">
          Real-time off-chain health translated into user impact—covering gasless AA execution,
          order keeper, Pyth oracle pipeline, vault &amp; protection workers, indexers, and
          self-hosted Arbitrum RPC nodes.
        </p>
      </header>

      <section aria-labelledby="infra-actions-title" className="space-y-4">
        <h2 id="infra-actions-title" className="text-xl font-semibold">
          User action readiness
        </h2>
        <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">
          {ACTION_CARDS.map((card) => (
            <StatCard key={card.label} label={card.label} unavailable detail={card.detail} />
          ))}
        </div>
      </section>

      <Panel className="p-6">
        <h2 className="text-lg font-semibold">Public-safe operational observability</h2>
        <p className="mt-2 text-sm leading-6 text-content-secondary">
          Stage 5 exposes categorical action readiness, per-indexer block-height lag, Alto bundler
          and Paymaster state, Close-Assistance availability, Pyth feed staleness/confidence, and
          Arbitrum Nitro RPC gateway sync health—strictly excluding signer addresses, balances,
          secrets, client IPs, and raw internal errors.
        </p>
        <Link to="/" className="mt-4 inline-block text-sm font-semibold text-brand-peach hover:underline">
          ← Back to Protocol Pulse
        </Link>
      </Panel>
    </div>
  )
}
