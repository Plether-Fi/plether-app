import { Link } from 'react-router-dom'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { StatCard } from '../components/StatCard'
import { Panel } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'

export function ProtocolPage() {
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
        statusLabel="Stage 4 — Protocol Manifest API pending"
        note="Backed by /api/insights/v1/protocol/manifest"
      />

      <header className="max-w-3xl space-y-3">
        <p className="text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">
          Protocol Mechanics &amp; Verification
        </p>
        <h1 className="text-3xl font-semibold tracking-tight sm:text-4xl">
          Contracts, Parameters &amp; Safety Controls
        </h1>
        <p className="text-sm leading-6 text-content-secondary">
          Complete on-chain contract registry, runtime bytecode verification, live economic
          parameters vs. release manifest defaults, and emergency pause status.
        </p>
      </header>

      <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">
        <StatCard label="Verified contracts" unavailable detail="Runtime code hash checks" />
        <StatCard
          label="Emergency pause state"
          unavailable
          detail="EmergencyPauseCoordinator flags"
        />
        <StatCard
          label="Trader risk parameters"
          unavailable
          detail="IM/MM, VPI factor, max skew, carry, fees"
        />
        <StatCard
          label="LP risk parameters"
          unavailable
          detail="Senior cap, exposure limit, settlement buffer"
        />
      </div>

      <Panel className="p-6">
        <h2 className="text-lg font-semibold">Release &amp; contract provenance</h2>
        <p className="mt-2 text-sm leading-6 text-content-secondary">
          Stage 4 will expose the active Core release manifest alongside live on-chain parameter
          reads and release upgrade history (<code className="font-mono text-xs">v1.2.3 → v1.3.1 → v2.0.0</code>),
          clearly explaining how every parameter impacts traders and LPs.
        </p>
        <Link to="/" className="mt-4 inline-block text-sm font-semibold text-brand-peach hover:underline">
          ← Back to Protocol Pulse
        </Link>
      </Panel>
    </div>
  )
}
