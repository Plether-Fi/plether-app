import { useMemo, useState } from 'react'
import { Link } from 'react-router-dom'
import {
  deriveInfraBanner,
  ecsTaskSummary,
  formatAgeDuration,
  formatBytes,
  formatInfraReason,
  formatLatencyMs,
  formatUptimeSeconds,
  infraHealthLabel,
  infraHealthTone,
  InsightsApiError,
  useInfraStatus,
  type InfraActionReadiness,
  type InfraServiceCategory,
  type InfraServiceStatus,
} from '../api'
import { DataFreshnessBanner } from '../components/DataFreshnessBanner'
import { StatusPill, type StatusTone } from '../components/StatusPill'
import { EmptyState, Panel } from '../components/ui'
import { useProtocolContext } from '../hooks/useProtocolContext'
import { resolvePerpsChain } from '../utils/chain'
import { formatUtc } from '../utils/format'

const CATEGORY_TABS: { key: InfraServiceCategory; label: string }[] = [
  { key: 'all', label: 'All services' },
  { key: 'execution', label: 'Execution' },
  { key: 'oracle', label: 'Oracle' },
  { key: 'liquidity', label: 'Liquidity' },
  { key: 'sponsorship', label: 'Sponsorship' },
  { key: 'indexing', label: 'Indexing' },
]

const FALLBACK_ACTIONS: { key: string; label: string; detail: string }[] = [
  { key: 'deposit', label: 'Deposit margin', detail: 'PostgreSQL, Alto bundler & RPC readiness' },
  { key: 'open', label: 'Open position', detail: 'Order keeper, Pyth oracle & AA pipeline' },
  { key: 'close', label: 'Close position', detail: 'Order keeper & close oracle policy' },
  { key: 'protection', label: 'TP/SL protection', detail: 'Protection worker & live trigger feeds' },
]

function fundingPill(state: string | null | undefined, reason: string | null | undefined): {
  label: string
  tone: StatusTone
  title: string
} {
  if (!state) {
    return { label: '—', tone: 'unknown', title: 'Service does not submit on-chain transactions' }
  }
  const reasonText = formatInfraReason(reason)
  if (state === 'ready') {
    return { label: 'Funded', tone: 'live', title: reasonText }
  }
  if (state === 'degraded' || state === 'warning') {
    return { label: 'Low ETH', tone: 'stale', title: reasonText }
  }
  if (state === 'blocked') {
    return { label: 'Depleted', tone: 'blocked', title: reasonText }
  }
  return { label: 'Unverified', tone: 'unknown', title: reasonText }
}

function blockBgForStatus(status: string): string {
  switch (status) {
    case 'healthy':
      return 'bg-positive'
    case 'degraded':
    case 'scaled_zero':
      return 'bg-brand-yellow'
    case 'down':
      return 'bg-brand-orange'
    default:
      return 'bg-brand-border/50'
  }
}

function ActionReadinessCell({ action }: { action: InfraActionReadiness }) {
  const tone = infraHealthTone(action.status)
  return (
    <div className="flex flex-col justify-between gap-1.5 px-4 py-3">
      <div className="flex items-center justify-between gap-2">
        <span className="text-xs font-semibold text-content-primary">{action.label}</span>
        <StatusPill tone={tone} label={infraHealthLabel(action.status)} />
      </div>
      <p className="text-[11px] leading-4 text-content-tertiary">{formatInfraReason(action.reason)}</p>
    </div>
  )
}

function ConnectionPoolBar({
  active,
  idle,
  total,
  max,
}: {
  active: number | null
  idle: number | null
  total: number | null
  max: number | null
}) {
  if (total == null || max == null || max <= 0) {
    return <p className="font-mono text-xs text-content-tertiary">Pool metrics unavailable</p>
  }
  const activePct = Math.min(100, Math.max(0, ((active ?? 0) / max) * 100))
  const idlePct = Math.min(100 - activePct, Math.max(0, ((idle ?? 0) / max) * 100))
  const totalPct = Math.min(100, (total / max) * 100)
  const barColor =
    totalPct >= 85 ? 'bg-brand-orange' : totalPct >= 65 ? 'bg-brand-yellow' : 'bg-positive'

  return (
    <div className="space-y-1.5">
      <div className="flex items-center justify-between text-xs">
        <span className="text-content-secondary">Connection pool</span>
        <span className="font-mono text-content-primary">
          {total} / {max} ({totalPct.toFixed(0)}%)
        </span>
      </div>
      <div
        className="flex h-2 w-full overflow-hidden bg-app-bg ring-1 ring-brand-border/30"
        role="img"
        aria-label={`PostgreSQL connections: ${String(total)} of ${String(max)} (${String(active ?? 0)} active, ${String(idle ?? 0)} idle)`}
      >
        <div className={barColor} style={{ width: `${activePct.toFixed(2)}%` }} />
        <div className="bg-brand-peach/45" style={{ width: `${idlePct.toFixed(2)}%` }} />
      </div>
      <div className="flex items-center gap-3 font-mono text-[11px] text-content-tertiary">
        <span>{active ?? 0} active</span>
        <span>·</span>
        <span>{idle ?? 0} idle</span>
      </div>
    </div>
  )
}

function ServiceFleetStrip({
  services,
  selectedCategory,
  onSelectCategory,
}: {
  services: InfraServiceStatus[]
  selectedCategory: InfraServiceCategory
  onSelectCategory: (cat: InfraServiceCategory) => void
}) {
  return (
    <div className="border border-brand-border/25 bg-app-bg p-3.5">
      <div className="flex flex-wrap items-center justify-between gap-2 text-xs">
        <span className="font-medium text-content-secondary">Fleet heartbeat strip</span>
        <div className="flex items-center gap-3 font-mono text-[11px] text-content-tertiary">
          <span className="inline-flex items-center gap-1.5">
            <span className="h-2 w-2 bg-positive" /> Healthy
          </span>
          <span className="inline-flex items-center gap-1.5">
            <span className="h-2 w-2 bg-brand-yellow" /> Degraded
          </span>
          <span className="inline-flex items-center gap-1.5">
            <span className="h-2 w-2 bg-brand-orange" /> Down
          </span>
          <span className="inline-flex items-center gap-1.5">
            <span className="h-2 w-2 bg-brand-border/50" /> Unknown
          </span>
        </div>
      </div>
      <div
        className="mt-2.5 grid gap-1.5"
        style={{ gridTemplateColumns: `repeat(${String(Math.max(1, services.length))}, minmax(0, 1fr))` }}
      >
        {services.map((svc) => {
          const dimmed = selectedCategory !== 'all' && svc.category !== selectedCategory
          const ecsText = svc.ecs
            ? `${String(svc.ecs.runningCount)}/${String(svc.ecs.desiredCount)} ECS`
            : 'ECS n/a'
          return (
            <button
              key={svc.key}
              type="button"
              onClick={() => {
                onSelectCategory(
                  selectedCategory === svc.category
                    ? 'all'
                    : (svc.category as InfraServiceCategory),
                )
              }}
              title={`${svc.name} · ${infraHealthLabel(svc.status)} · ${ecsText}`}
              aria-label={`${svc.name}: ${infraHealthLabel(svc.status)}`}
              className={`group flex flex-col gap-1 text-left transition-opacity ${
                dimmed ? 'opacity-35 hover:opacity-80' : 'opacity-100'
              }`}
            >
              <span className={`block h-3 w-full ${blockBgForStatus(svc.status)}`} />
              <span className="hidden truncate font-mono text-[10px] text-content-tertiary group-hover:text-content-primary md:block">
                {svc.name.replace(/ Worker| Indexer| Keeper/g, '')}
              </span>
            </button>
          )
        })}
      </div>
    </div>
  )
}

export function InfraPage() {
  const { chain: fallbackChain, releaseVersion: fallbackRelease, blockNumber: fallbackBlock, observedAt: fallbackObserved } =
    useProtocolContext()
  const infraQuery = useInfraStatus()
  const [categoryFilter, setCategoryFilter] = useState<InfraServiceCategory>('all')

  const data = infraQuery.data
  const chain = data ? resolvePerpsChain(data.network.chainId) : fallbackChain
  const banner = deriveInfraBanner(data, infraQuery.isLoading, infraQuery.error)
  const isPendingBackend =
    infraQuery.error instanceof InsightsApiError && infraQuery.error.status === 404

  const headBlockNum = data?.aws.node.headBlock ? Number(data.aws.node.headBlock) : fallbackBlock
  const observedAt = data?.observedAt ?? fallbackObserved
  const releaseVersion = data?.network.releaseVersion ?? fallbackRelease

  const filteredServices = useMemo(() => {
    const all = data?.aws.services ?? []
    if (categoryFilter === 'all') return all
    return all.filter((svc) => svc.category === categoryFilter)
  }, [data, categoryFilter])

  const backendEcs = ecsTaskSummary(data?.aws.backend.ecs)
  const gatewayEcs = ecsTaskSummary(data?.aws.node.gatewayEcs)
  const altoEcs = ecsTaskSummary(data?.aws.alto.ecs)
  const altoFunding = fundingPill(data?.aws.alto.fundingState, data?.aws.alto.fundingReason)

  return (
    <div className="space-y-6 sm:space-y-8">
      <DataFreshnessBanner
        networkName={chain.name}
        isTestnet={chain.isTestnet}
        releaseVersion={releaseVersion}
        blockNumber={headBlockNum}
        observedAt={observedAt}
        tone={banner.tone}
        statusLabel={banner.statusLabel}
        note={banner.note}
      />

      <header className="flex flex-col gap-3 sm:flex-row sm:items-end sm:justify-between">
        <div className="max-w-3xl space-y-2">
          <p className="text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">
            Infrastructure &amp; Reliability
          </p>
          <h1 className="text-3xl font-semibold tracking-tight sm:text-4xl">
            AWS Stack, Keepers, Database, Alto &amp; RPC Health
          </h1>
          <p className="text-sm leading-6 text-content-secondary">
            Live runtime probes paired with scoped read-only AWS ECS, RDS, and EC2 control-plane
            health—translated into user action readiness across trading, settlement, and gasless
            execution.
          </p>
        </div>
        {data && (
          <div className="shrink-0">
            <StatusPill
              tone={data.aws.controlPlaneAvailable ? 'live' : 'unknown'}
              label={
                data.aws.controlPlaneAvailable
                  ? 'AWS control plane connected'
                  : 'Runtime probes only'
              }
            />
          </div>
        )}
      </header>

      {isPendingBackend && (
        <Panel className="p-6">
          <h2 className="text-lg font-semibold">Stage 6 backend endpoint awaiting deployment</h2>
          <p className="mt-2 text-sm leading-6 text-content-secondary">
            This environment does not yet serve{' '}
            <code className="font-mono text-xs text-content-primary">
              /api/insights/v1/infra/status
            </code>
            . Deploy the Stage 6 backend &amp; IAM update to stream live AWS ECS, RDS, EC2 Nitro
            node, Alto bundler, and worker heartbeats.
          </p>
          <div className="mt-4 flex flex-wrap gap-4 text-sm font-semibold">
            <Link to="/keepers" className="text-brand-peach hover:underline">
              Inspect on-chain Keepers →
            </Link>
            <Link to="/sponsored" className="text-content-secondary hover:text-content-primary hover:underline">
              Inspect Sponsored AA →
            </Link>
          </div>
        </Panel>
      )}

      {!isPendingBackend && infraQuery.isError && (
        <Panel>
          <EmptyState
            title="Infrastructure telemetry unavailable"
            message="Could not load live infrastructure status from the backend. Retry shortly or verify backend connectivity."
          />
        </Panel>
      )}

      <section aria-labelledby="infra-actions-title">
        <h2 id="infra-actions-title" className="sr-only">
          User action readiness
        </h2>
        <Panel className="overflow-hidden">
          {infraQuery.isLoading ? (
            <div className="grid grid-cols-1 divide-y divide-brand-border/15 sm:grid-cols-2 lg:grid-cols-4 lg:divide-x lg:divide-y-0">
              {FALLBACK_ACTIONS.map((item) => (
                <div key={item.key} className="space-y-2 px-4 py-3">
                  <div className="skeleton h-4 w-28" />
                  <div className="skeleton h-3 w-40" />
                </div>
              ))}
            </div>
          ) : data ? (
            <div className="grid grid-cols-1 divide-y divide-brand-border/15 sm:grid-cols-2 lg:grid-cols-4 lg:divide-x lg:divide-y-0">
              <ActionReadinessCell action={data.actions.deposit} />
              <ActionReadinessCell action={data.actions.open} />
              <ActionReadinessCell action={data.actions.close} />
              <ActionReadinessCell action={data.actions.protection} />
            </div>
          ) : (
            <div className="grid grid-cols-1 divide-y divide-brand-border/15 sm:grid-cols-2 lg:grid-cols-4 lg:divide-x lg:divide-y-0">
              {FALLBACK_ACTIONS.map((item) => (
                <div key={item.key} className="space-y-1.5 px-4 py-3">
                  <div className="flex items-center justify-between gap-2">
                    <span className="text-xs font-semibold text-content-primary">{item.label}</span>
                    <StatusPill tone="unknown" label="Unavailable" />
                  </div>
                  <p className="text-[11px] text-content-tertiary">{item.detail}</p>
                </div>
              ))}
            </div>
          )}
        </Panel>
      </section>

      {data && (
        <>
          <section aria-labelledby="aws-core-title" className="space-y-3">
            <div className="flex flex-wrap items-center justify-between gap-2">
              <h2 id="aws-core-title" className="text-lg font-semibold">
                AWS core infrastructure
              </h2>
              <p className="font-mono text-xs text-content-tertiary">
                Observed {formatUtc(data.observedAt)}
              </p>
            </div>

            <div className="grid gap-4 lg:grid-cols-2">
              {/* Backend API */}
              <Panel className="flex flex-col justify-between gap-4 p-5">
                <div className="flex items-start justify-between gap-3">
                  <div>
                    <p className="font-mono text-[11px] uppercase tracking-[0.14em] text-brand-peach">
                      AWS ECS · plether-api
                    </p>
                    <h3 className="mt-1 text-base font-semibold">Backend API Service</h3>
                  </div>
                  <StatusPill
                    tone={infraHealthTone(data.aws.backend.status)}
                    label={infraHealthLabel(data.aws.backend.status)}
                  />
                </div>

                <dl className="grid grid-cols-2 gap-3 border-t border-brand-border/20 pt-3 text-xs sm:grid-cols-3">
                  <div>
                    <dt className="text-content-tertiary">Process uptime</dt>
                    <dd className="mt-1 font-mono text-sm font-semibold text-content-primary">
                      {formatUptimeSeconds(data.aws.backend.uptimeSeconds)}
                    </dd>
                  </div>
                  <div>
                    <dt className="text-content-tertiary">Core release</dt>
                    <dd className="mt-1 font-mono text-sm font-semibold text-content-primary">
                      {data.aws.backend.releaseVersion}
                    </dd>
                  </div>
                  <div>
                    <dt className="text-content-tertiary">ECS tasks</dt>
                    <dd className="mt-1">
                      <StatusPill tone={backendEcs.tone} label={backendEcs.label} />
                    </dd>
                  </div>
                </dl>

                <div className="flex flex-wrap items-center justify-between gap-2 border-t border-brand-border/15 pt-2.5 font-mono text-[11px] text-content-tertiary">
                  <span>
                    Rollout:{' '}
                    <span className="text-content-secondary">
                      {data.aws.backend.ecs?.rolloutState ?? 'Unreported'}
                    </span>
                  </span>
                  <span>
                    Service state:{' '}
                    <span className="text-content-secondary">
                      {data.aws.backend.ecs?.status ?? 'Live HTTP'}
                    </span>
                  </span>
                </div>
              </Panel>

              {/* PostgreSQL RDS */}
              <Panel className="flex flex-col justify-between gap-4 p-5">
                <div className="flex items-start justify-between gap-3">
                  <div>
                    <p className="font-mono text-[11px] uppercase tracking-[0.14em] text-brand-peach">
                      AWS RDS · PostgreSQL
                    </p>
                    <h3 className="mt-1 text-base font-semibold">Primary Database</h3>
                  </div>
                  <StatusPill
                    tone={data.aws.database.reachable ? 'live' : 'blocked'}
                    label={data.aws.database.reachable ? 'Reachable' : 'Unreachable'}
                  />
                </div>

                <ConnectionPoolBar
                  active={data.aws.database.activeConnections}
                  idle={data.aws.database.idleConnections}
                  total={data.aws.database.totalConnections}
                  max={data.aws.database.maxConnections}
                />

                <dl className="grid grid-cols-2 gap-3 border-t border-brand-border/20 pt-3 text-xs sm:grid-cols-4">
                  <div>
                    <dt className="text-content-tertiary">Ping latency</dt>
                    <dd className="mt-1 font-mono text-sm font-semibold text-content-primary">
                      {formatLatencyMs(data.aws.database.pingLatencyMs)}
                    </dd>
                  </div>
                  <div>
                    <dt className="text-content-tertiary">DB size</dt>
                    <dd className="mt-1 font-mono text-sm font-semibold text-content-primary">
                      {formatBytes(data.aws.database.databaseSizeBytes)}
                    </dd>
                  </div>
                  <div>
                    <dt className="text-content-tertiary">Role</dt>
                    <dd className="mt-1 font-mono text-sm font-semibold text-content-primary">
                      {data.aws.database.inRecovery == null
                        ? '—'
                        : data.aws.database.inRecovery
                          ? 'Replica'
                          : 'Primary'}
                    </dd>
                  </div>
                  <div>
                    <dt className="text-content-tertiary">RDS status</dt>
                    <dd className="mt-1">
                      <StatusPill
                        tone={infraHealthTone(data.aws.database.rds?.status)}
                        label={data.aws.database.rds?.status ?? 'Unverified'}
                      />
                    </dd>
                  </div>
                </dl>

                {data.aws.database.rds && (
                  <div className="flex flex-wrap items-center gap-x-4 gap-y-1 border-t border-brand-border/15 pt-2.5 font-mono text-[11px] text-content-tertiary">
                    {data.aws.database.rds.engineVersion && (
                      <span>PG {data.aws.database.rds.engineVersion}</span>
                    )}
                    {data.aws.database.rds.allocatedStorageGb != null && (
                      <span>{data.aws.database.rds.allocatedStorageGb} GB allocated</span>
                    )}
                    {data.aws.database.rds.multiAz != null && (
                      <span>{data.aws.database.rds.multiAz ? 'Multi-AZ' : 'Single-AZ'}</span>
                    )}
                    {data.aws.database.rds.storageEncrypted != null && (
                      <span>
                        {data.aws.database.rds.storageEncrypted ? 'Encrypted' : 'Unencrypted'}
                      </span>
                    )}
                  </div>
                )}
              </Panel>

              {/* Arbitrum Nitro Node & RPC Gateway */}
              <Panel className="flex flex-col justify-between gap-4 p-5">
                <div className="flex items-start justify-between gap-3">
                  <div>
                    <p className="font-mono text-[11px] uppercase tracking-[0.14em] text-brand-peach">
                      AWS EC2 + ECS · Arbitrum Nitro &amp; Gateway
                    </p>
                    <h3 className="mt-1 text-base font-semibold">Self-Hosted RPC Node</h3>
                  </div>
                  <StatusPill
                    tone={
                      !data.aws.node.reachable
                        ? 'blocked'
                        : data.aws.node.syncing === true ||
                            (data.aws.node.headLagSeconds != null &&
                              data.aws.node.headLagSeconds > 30)
                          ? 'stale'
                          : 'live'
                    }
                    label={
                      !data.aws.node.reachable
                        ? 'Unreachable'
                        : data.aws.node.syncing === true
                          ? 'Syncing'
                          : data.aws.node.headLagSeconds != null &&
                              data.aws.node.headLagSeconds > 30
                            ? 'Lagging'
                            : 'Synced'
                    }
                  />
                </div>

                <dl className="grid grid-cols-2 gap-3 border-t border-brand-border/20 pt-3 text-xs sm:grid-cols-4">
                  <div>
                    <dt className="text-content-tertiary">Head block</dt>
                    <dd className="mt-1 font-mono text-sm font-semibold text-content-primary">
                      {data.aws.node.headBlock
                        ? Number(data.aws.node.headBlock).toLocaleString('en-US')
                        : '—'}
                    </dd>
                  </div>
                  <div>
                    <dt className="text-content-tertiary">Head lag</dt>
                    <dd className="mt-1 font-mono text-sm font-semibold text-content-primary">
                      {data.aws.node.headLagSeconds != null
                        ? `${String(data.aws.node.headLagSeconds)}s`
                        : '—'}
                    </dd>
                  </div>
                  <div>
                    <dt className="text-content-tertiary">RPC latency</dt>
                    <dd className="mt-1 font-mono text-sm font-semibold text-content-primary">
                      {formatLatencyMs(data.aws.node.rpcLatencyMs)}
                    </dd>
                  </div>
                  <div>
                    <dt className="text-content-tertiary">Peers</dt>
                    <dd className="mt-1 font-mono text-sm font-semibold text-content-primary">
                      {data.aws.node.peerCount ?? '—'}
                    </dd>
                  </div>
                </dl>

                <div className="flex flex-wrap items-center justify-between gap-2 border-t border-brand-border/15 pt-2.5 text-xs">
                  <div className="flex flex-wrap items-center gap-2">
                    <span className="font-mono text-[11px] text-content-tertiary">EC2 node:</span>
                    <StatusPill
                      tone={infraHealthTone(data.aws.node.ec2?.instanceState)}
                      label={data.aws.node.ec2?.instanceState ?? 'Unverified'}
                    />
                    {data.aws.node.ec2?.systemStatus && (
                      <span className="font-mono text-[11px] text-content-tertiary">
                        sys:{data.aws.node.ec2.systemStatus} · inst:
                        {data.aws.node.ec2.instanceStatus ?? 'n/a'}
                      </span>
                    )}
                  </div>
                  <div className="flex items-center gap-2">
                    <span className="font-mono text-[11px] text-content-tertiary">Gateway:</span>
                    <StatusPill tone={gatewayEcs.tone} label={gatewayEcs.label} />
                  </div>
                </div>
              </Panel>

              {/* Alto ERC-4337 Bundler */}
              <Panel className="flex flex-col justify-between gap-4 p-5">
                <div className="flex items-start justify-between gap-3">
                  <div>
                    <p className="font-mono text-[11px] uppercase tracking-[0.14em] text-brand-peach">
                      AWS ECS · plether-alto
                    </p>
                    <h3 className="mt-1 text-base font-semibold">Alto ERC-4337 Bundler</h3>
                  </div>
                  <StatusPill
                    tone={
                      !data.aws.alto.configured
                        ? 'unknown'
                        : data.aws.alto.reachable && data.aws.alto.entryPointSupported
                          ? 'live'
                          : 'blocked'
                    }
                    label={
                      !data.aws.alto.configured
                        ? 'Not configured'
                        : data.aws.alto.reachable && data.aws.alto.entryPointSupported
                          ? 'Ready'
                          : data.aws.alto.reachable
                            ? 'EntryPoint mismatch'
                            : 'Unreachable'
                    }
                  />
                </div>

                <dl className="grid grid-cols-2 gap-3 border-t border-brand-border/20 pt-3 text-xs sm:grid-cols-4">
                  <div>
                    <dt className="text-content-tertiary">Probe latency</dt>
                    <dd className="mt-1 font-mono text-sm font-semibold text-content-primary">
                      {formatLatencyMs(data.aws.alto.probeLatencyMs)}
                    </dd>
                  </div>
                  <div>
                    <dt className="text-content-tertiary">EntryPoint</dt>
                    <dd className="mt-1 font-mono text-sm font-semibold text-content-primary">
                      {data.aws.alto.entryPointVersion
                        ? `v${data.aws.alto.entryPointVersion}`
                        : '—'}
                    </dd>
                  </div>
                  <div>
                    <dt className="text-content-tertiary">Executor ETH</dt>
                    <dd className="mt-1" title={altoFunding.title}>
                      <StatusPill tone={altoFunding.tone} label={altoFunding.label} />
                    </dd>
                  </div>
                  <div>
                    <dt className="text-content-tertiary">ECS tasks</dt>
                    <dd className="mt-1">
                      <StatusPill tone={altoEcs.tone} label={altoEcs.label} />
                    </dd>
                  </div>
                </dl>

                <div className="flex items-center justify-between gap-2 border-t border-brand-border/15 pt-2.5 text-xs">
                  <span className="font-mono text-[11px] text-content-tertiary">
                    {altoFunding.title}
                  </span>
                  <Link
                    to="/sponsored"
                    className="font-semibold text-brand-peach hover:underline hover:underline-offset-4"
                  >
                    Paymaster &amp; UserOps →
                  </Link>
                </div>
              </Panel>
            </div>
          </section>

          {/* ECS Keepers, Workers & Indexers Fleet */}
          <section aria-labelledby="workers-fleet-title" className="space-y-3">
            <div className="flex flex-col gap-3 sm:flex-row sm:items-center sm:justify-between">
              <div>
                <h2 id="workers-fleet-title" className="text-lg font-semibold">
                  ECS Keepers, Workers &amp; Indexers
                </h2>
                <p className="text-xs text-content-secondary">
                  {data.aws.summary.healthyServices} of {data.aws.summary.totalServices} services
                  passing ECS task, heartbeat freshness, and funding checks.
                </p>
              </div>
              <div className="flex flex-wrap gap-1.5" role="tablist" aria-label="Service category">
                {CATEGORY_TABS.map((tab) => {
                  const active = categoryFilter === tab.key
                  return (
                    <button
                      key={tab.key}
                      type="button"
                      role="tab"
                      aria-selected={active}
                      onClick={() => {
                        setCategoryFilter(tab.key)
                      }}
                      className={`border px-2.5 py-1 text-xs font-semibold transition-colors ${
                        active
                          ? 'border-brand-orange bg-brand-orange text-content-primary'
                          : 'border-brand-border/35 bg-surface-panel text-content-secondary hover:border-brand-peach/50 hover:text-content-primary'
                      }`}
                    >
                      {tab.label}
                    </button>
                  )
                })}
              </div>
            </div>

            <ServiceFleetStrip
              services={data.aws.services}
              selectedCategory={categoryFilter}
              onSelectCategory={setCategoryFilter}
            />

            <Panel className="overflow-x-auto">
              <table className="w-full min-w-[740px] border-collapse text-left text-xs">
                <thead>
                  <tr className="border-b border-brand-border/25 text-[11px] uppercase tracking-[0.12em] text-content-tertiary">
                    <th className="px-4 py-3 font-semibold">Service</th>
                    <th className="px-4 py-3 font-semibold">Health</th>
                    <th className="px-4 py-3 font-semibold">AWS ECS</th>
                    <th className="px-4 py-3 font-semibold">Scanned Block &amp; Lag</th>
                    <th className="px-4 py-3 font-semibold">Heartbeat</th>
                    <th className="px-4 py-3 font-semibold">ETH Funding</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-brand-border/15">
                  {filteredServices.map((svc) => {
                    const ecs = ecsTaskSummary(svc.ecs)
                    const fund = fundingPill(svc.fundingState, svc.fundingReason)
                    return (
                      <tr key={svc.key} className="hover:bg-brand-border/10">
                        <td className="px-4 py-3">
                          <div className="font-semibold text-content-primary">{svc.name}</div>
                          <div className="mt-0.5 font-mono text-[11px] capitalize text-content-tertiary">
                            {svc.category}
                          </div>
                        </td>
                        <td className="px-4 py-3">
                          <StatusPill
                            tone={infraHealthTone(svc.status)}
                            label={infraHealthLabel(svc.status)}
                          />
                        </td>
                        <td className="px-4 py-3">
                          <div className="flex flex-col gap-1">
                            <StatusPill tone={ecs.tone} label={ecs.label} />
                            {svc.ecs?.rolloutState && (
                              <span className="font-mono text-[10px] text-content-tertiary">
                                {svc.ecs.rolloutState}
                              </span>
                            )}
                          </div>
                        </td>
                        <td className="px-4 py-3 font-mono">
                          {svc.lastBlock ? (
                            <>
                              <div className="text-content-primary">
                                {Number(svc.lastBlock).toLocaleString('en-US')}
                              </div>
                              <div className="text-[11px] text-content-tertiary">
                                {svc.blocksBehind != null
                                  ? svc.blocksBehind === 0
                                    ? 'At head'
                                    : `${svc.blocksBehind.toLocaleString('en-US')} blks behind`
                                  : 'Lag unverified'}
                              </div>
                            </>
                          ) : (
                            <span className="text-content-tertiary">Timestamp-based</span>
                          )}
                        </td>
                        <td className="px-4 py-3 font-mono">
                          <div className="text-content-primary">
                            {formatAgeDuration(svc.heartbeatAgeSeconds)}
                          </div>
                          <div className="text-[11px] text-content-tertiary">
                            Limit {svc.freshThresholdSeconds}s
                          </div>
                        </td>
                        <td className="px-4 py-3" title={fund.title}>
                          {svc.fundingState ? (
                            <StatusPill tone={fund.tone} label={fund.label} />
                          ) : (
                            <span className="font-mono text-content-tertiary">—</span>
                          )}
                        </td>
                      </tr>
                    )
                  })}
                </tbody>
              </table>
            </Panel>
          </section>

          {/* Pyth Oracle Pipeline */}
          <section aria-labelledby="oracle-pipeline-title" className="space-y-3">
            <h2 id="oracle-pipeline-title" className="text-lg font-semibold">
              Pyth oracle pipeline
            </h2>
            <Panel className="p-5">
              <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-6">
                <div>
                  <p className="text-xs text-content-tertiary">Latest publish</p>
                  <p className="mt-1 font-mono text-sm font-semibold text-content-primary">
                    {data.oracle.latestPublishTime
                      ? formatUtc(data.oracle.latestPublishTime)
                      : 'Unavailable'}
                  </p>
                </div>
                <div>
                  <p className="text-xs text-content-tertiary">Oracle age</p>
                  <p className="mt-1 font-mono text-sm font-semibold text-content-primary">
                    {data.oracle.ageSeconds != null ? `${String(data.oracle.ageSeconds)}s` : '—'}
                  </p>
                </div>
                <div>
                  <p className="text-xs text-content-tertiary">6-feed spread</p>
                  <p className="mt-1 font-mono text-sm font-semibold text-content-primary">
                    {data.oracle.feedSpreadSeconds != null
                      ? `${String(data.oracle.feedSpreadSeconds)}s`
                      : '—'}
                  </p>
                </div>
                <div>
                  <p className="text-xs text-content-tertiary">Open policy</p>
                  <div className="mt-1">
                    <StatusPill
                      tone={infraHealthTone(data.oracle.openPolicyStatus)}
                      label={infraHealthLabel(data.oracle.openPolicyStatus)}
                    />
                  </div>
                </div>
                <div>
                  <p className="text-xs text-content-tertiary">Close policy</p>
                  <div className="mt-1">
                    <StatusPill
                      tone={infraHealthTone(data.oracle.closePolicyStatus)}
                      label={infraHealthLabel(data.oracle.closePolicyStatus)}
                    />
                  </div>
                </div>
                <div>
                  <p className="text-xs text-content-tertiary">Protection policy</p>
                  <div className="mt-1">
                    <StatusPill
                      tone={infraHealthTone(data.oracle.protectionPolicyStatus)}
                      label={infraHealthLabel(data.oracle.protectionPolicyStatus)}
                    />
                  </div>
                </div>
              </div>
            </Panel>
          </section>
        </>
      )}

      <Panel className="p-5 text-xs leading-6 text-content-secondary">
        <span className="font-semibold text-content-primary">Public-safe telemetry boundary:</span>{' '}
        AWS ECS, RDS, and EC2 health is read via scoped least-privilege IAM and sanitized before
        leaving the backend. AWS account IDs, ARNs, VPC/subnet/security-group IDs, private IPs,
        EC2 instance IDs, KMS keys, RPC endpoints/tokens, signer keys, and raw exception messages
        are never exposed.
      </Panel>
    </div>
  )
}
