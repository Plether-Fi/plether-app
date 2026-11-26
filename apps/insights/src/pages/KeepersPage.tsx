import { useState } from 'react'
import { Link } from 'react-router-dom'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { StatCard } from '../components/StatCard'
import { EmptyState, Panel } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'

type KeeperRoleFilter =
  | 'all'
  | 'order_execution'
  | 'lp_settlement'
  | 'protection'
  | 'liquidation'
  | 'oracle'

interface RoleSurface {
  id: Exclude<KeeperRoleFilter, 'all'>
  label: string
  contract: string
  detectionRule: string
}

const ROLE_SURFACES: RoleSurface[] = [
  {
    id: 'order_execution',
    label: 'Order execution',
    contract: 'OrderRouter',
    detectionRule: 'tx.from on executed V3 open/close order transactions',
  },
  {
    id: 'lp_settlement',
    label: 'LP settlement',
    contract: 'HousePool',
    detectionRule: 'tx.from on hourly Senior/Junior epoch settlement transactions',
  },
  {
    id: 'protection',
    label: 'TP/SL protection',
    contract: 'PositionProtectionBook',
    detectionRule: 'tx.from on PositionProtectionTriggered executions',
  },
  {
    id: 'liquidation',
    label: 'Liquidation',
    contract: 'CfdEngine',
    detectionRule: 'tx.from on PositionLiquidated transactions + keeperBountyAssets',
  },
  {
    id: 'oracle',
    label: 'Oracle updates',
    contract: 'PletherOracle',
    detectionRule: 'tx.from on on-chain six-feed Pyth FX basket update transactions',
  },
]

export function KeepersPage() {
  const { chain, releaseVersion, blockNumber, observedAt } = useProtocolContext()
  const [roleFilter, setRoleFilter] = useState<KeeperRoleFilter>('all')
  const [search, setSearch] = useState('')

  const selectedSurface = ROLE_SURFACES.find((surface) => surface.id === roleFilter) ?? null

  return (
    <div className="space-y-8">
      <DataFreshnessBanner
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={releaseVersion}
        blockNumber={blockNumber}
        observedAt={observedAt}
        tone="unknown"
        statusLabel="Permissionless Keepers API pending"
        note="Backed by /api/insights/v1/keepers/{status,activity}"
      />

      <header className="max-w-3xl space-y-3">
        <p className="text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">
          Permissionless On-Chain Keepers
        </p>
        <h1 className="text-3xl font-semibold tracking-tight sm:text-4xl">
          Detected Keepers, Latency &amp; ETH Balances
        </h1>
        <p className="text-sm leading-6 text-content-secondary">
          Protocol execution is permissionless—every address discovered on-chain executing orders,
          settling LP epochs, triggering TP/SL protections, running liquidations, or updating
          oracles is tracked identically, with no special treatment for Plether-run signers.
        </p>
      </header>

      <section aria-labelledby="keepers-summary-title" className="space-y-4">
        <h2 id="keepers-summary-title" className="text-xl font-semibold">
          Ecosystem summary
        </h2>
        <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">
          <StatCard
            label="Active keepers (24h)"
            unavailable
            detail="Unique on-chain tx.from executors detected"
          />
          <StatCard
            label="Keeper executions (24h)"
            unavailable
            detail="Orders, epochs, protections, liquidations & oracle updates"
          />
          <StatCard
            label="Execution latency (p50 / p95)"
            unavailable
            detail="Commit, trigger, or epoch boundary → confirmation"
          />
          <StatCard
            label="Bounties paid (24h)"
            unavailable
            detail={`Liquidation & protection trigger payouts in ${chain.defaultAssetSymbol}`}
          />
        </div>
      </section>

      <section aria-labelledby="keepers-detected-title" className="space-y-4">
        <div className="flex flex-col justify-between gap-4 lg:flex-row lg:items-end">
          <div>
            <h2 id="keepers-detected-title" className="text-xl font-semibold">
              Detected on-chain keepers
            </h2>
            <p className="mt-1 text-xs text-content-tertiary">
              Ranked dynamically by on-chain executions · Live ETH balance read via eth_getBalance
            </p>
          </div>

          <div className="w-full sm:max-w-xs">
            <label htmlFor="keeper-address-filter" className="sr-only">
              Filter keeper address
            </label>
            <input
              id="keeper-address-filter"
              type="search"
              value={search}
              onChange={(event) => {
                setSearch(event.target.value)
              }}
              placeholder="Filter keeper address (0x…)"
              className="w-full border border-brand-border/35 bg-app-bg px-4 py-2 text-sm font-mono placeholder:font-sans placeholder:text-content-tertiary"
            />
          </div>
        </div>

        <div className="flex flex-wrap gap-2" role="group" aria-label="Filter keepers by role">
          <button
            type="button"
            aria-pressed={roleFilter === 'all'}
            onClick={() => {
              setRoleFilter('all')
            }}
            className={`border px-3 py-1.5 text-xs font-semibold transition-colors ${
              roleFilter === 'all'
                ? 'border-brand-orange bg-brand-orange text-content-primary'
                : 'border-brand-border/30 bg-surface-panel text-content-secondary hover:border-brand-orange/50 hover:text-content-primary'
            }`}
          >
            All roles
          </button>
          {ROLE_SURFACES.map((surface) => (
            <button
              key={surface.id}
              type="button"
              aria-pressed={roleFilter === surface.id}
              onClick={() => {
                setRoleFilter(surface.id)
              }}
              className={`border px-3 py-1.5 text-xs font-semibold transition-colors ${
                roleFilter === surface.id
                  ? 'border-brand-orange bg-brand-orange text-content-primary'
                  : 'border-brand-border/30 bg-surface-panel text-content-secondary hover:border-brand-orange/50 hover:text-content-primary'
              }`}
            >
              {surface.label}
            </button>
          ))}
        </div>

        <Panel>
          <div className="overflow-x-auto">
            <table className="w-full min-w-[820px] text-left text-sm">
              <caption className="sr-only">Permissionless on-chain keepers detected by the indexer</caption>
              <thead className="border-b border-brand-border/25 text-xs text-content-tertiary">
                <tr>
                  <th scope="col" className="px-4 py-3.5">Keeper address</th>
                  <th scope="col" className="px-4 py-3.5">Observed roles</th>
                  <th scope="col" className="px-4 py-3.5">Executions (24h)</th>
                  <th scope="col" className="px-4 py-3.5">Share</th>
                  <th scope="col" className="px-4 py-3.5">Latency (p50 / p95)</th>
                  <th scope="col" className="px-4 py-3.5">ETH balance</th>
                  <th scope="col" className="px-4 py-3.5">Bounties earned</th>
                  <th scope="col" className="px-4 py-3.5">Last active</th>
                </tr>
              </thead>
              <tbody />
            </table>
          </div>
          <EmptyState
            title="Keeper discovery feed awaiting backend endpoint"
            message={
              selectedSurface
                ? `Once /api/insights/v1/keepers/status is connected, every on-chain address detected executing ${selectedSurface.label.toLowerCase()} (${selectedSurface.contract}) will appear here.`
                : 'Once /api/insights/v1/keepers/status is connected, every on-chain address detected across permissionless protocol keeper surfaces will appear here with live ETH balances and latency.'
            }
          />
        </Panel>
      </section>

      <section aria-labelledby="keepers-surfaces-title" className="space-y-4">
        <div className="flex flex-wrap items-baseline justify-between gap-2">
          <h2 id="keepers-surfaces-title" className="text-xl font-semibold">
            Indexed permissionless surfaces
          </h2>
          <Link to="/infra" className="text-sm font-semibold text-brand-peach hover:underline">
            AA bundler &amp; infra readiness →
          </Link>
        </div>
        <div className="grid gap-3 md:grid-cols-2 lg:grid-cols-5">
          {ROLE_SURFACES.map((surface) => (
            <Panel key={surface.id} className="p-4">
              <p className="font-mono text-xs text-brand-peach">{surface.contract}</p>
              <h3 className="mt-1 text-sm font-semibold">{surface.label}</h3>
              <p className="mt-2 text-xs leading-5 text-content-secondary">
                {surface.detectionRule}
              </p>
            </Panel>
          ))}
        </div>
      </section>

      <section aria-labelledby="keepers-feed-title" className="space-y-4">
        <h2 id="keepers-feed-title" className="text-xl font-semibold">
          Recent permissionless keeper activity
        </h2>
        <Panel>
          <EmptyState
            title="Keeper activity stream awaiting backend endpoint"
            message="Recent on-chain order fills, LP epoch settlements, TP/SL triggers, liquidations, and oracle updates across all detected keepers will stream here with latency, bounties, and explorer links."
          />
        </Panel>
      </section>
    </div>
  )
}
