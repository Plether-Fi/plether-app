import { useQuery, useQueryClient } from '@tanstack/react-query'
import type { StatusTone } from '../components/StatusPill'
import { InsightsApiError } from './client'
import type { ApiErrorBody } from './types'

const API_ROOT = '/api/insights/v1'

export type InfraHealthStatus = 'healthy' | 'degraded' | 'down' | 'scaled_zero' | 'unknown'
export type ActionReadinessStatus = 'ready' | 'degraded' | 'blocked' | 'unknown'
export type InfraServiceCategory = 'all' | 'execution' | 'oracle' | 'liquidity' | 'sponsorship' | 'indexing'

export interface InfraNetworkMeta {
  chainId: number
  releaseVersion: string
  settlementAssetSymbol: string
}

export interface AwsEcsStatus {
  status: string
  desiredCount: number
  runningCount: number
  pendingCount: number
  rolloutState: string | null
}

export interface AwsRdsStatus {
  status: string
  engineVersion: string | null
  multiAz: boolean | null
  storageEncrypted: boolean | null
  allocatedStorageGb: number | null
}

export interface AwsEc2Status {
  instanceState: string
  systemStatus: string | null
  instanceStatus: string | null
}

export interface InfraActionReadiness {
  label: string
  status: string
  reason: string
}

export interface InfraBackendStatus {
  status: string
  uptimeSeconds: number
  releaseVersion: string
  ecs: AwsEcsStatus | null
}

export interface InfraDatabaseStatus {
  reachable: boolean
  pingLatencyMs: number | null
  activeConnections: number | null
  idleConnections: number | null
  totalConnections: number | null
  maxConnections: number | null
  databaseSizeBytes: string | null
  inRecovery: boolean | null
  rds: AwsRdsStatus | null
}

export interface InfraNodeStatus {
  reachable: boolean
  rpcLatencyMs: number | null
  headBlock: string | null
  headTimestamp: string | null
  headLagSeconds: number | null
  syncing: boolean | null
  peerCount: number | null
  ec2: AwsEc2Status | null
  gatewayEcs: AwsEcsStatus | null
}

export interface InfraAltoStatus {
  configured: boolean
  reachable: boolean
  probeLatencyMs: number | null
  entryPointVersion: string | null
  entryPointSupported: boolean
  fundingState: string
  fundingReason: string
  ecs: AwsEcsStatus | null
}

export interface InfraServiceStatus {
  key: string
  name: string
  category: string
  status: string
  lastBlock: string | null
  blocksBehind: number | null
  lastHeartbeatAt: string | null
  heartbeatAgeSeconds: number | null
  freshThresholdSeconds: number
  fundingState: string | null
  fundingReason: string | null
  ecs: AwsEcsStatus | null
}

export interface InfraOracleStatus {
  latestPublishTime: string | null
  ageSeconds: number | null
  feedSpreadSeconds: number | null
  openPolicyStatus: string
  closePolicyStatus: string
  protectionPolicyStatus: string
}

export interface InfraStatusResponse {
  network: InfraNetworkMeta
  observedAt: string
  overallStatus: string
  actions: {
    deposit: InfraActionReadiness
    open: InfraActionReadiness
    close: InfraActionReadiness
    protection: InfraActionReadiness
  }
  aws: {
    controlPlaneAvailable: boolean
    backend: InfraBackendStatus
    database: InfraDatabaseStatus
    node: InfraNodeStatus
    alto: InfraAltoStatus
    services: InfraServiceStatus[]
    summary: {
      healthyServices: number
      totalServices: number
    }
  }
  oracle: InfraOracleStatus
}

export interface OverviewResponse {
  network: InfraNetworkMeta
  observedAt: string
  headBlock: string | null
  execution: {
    orders24h: number | null
    executed24h: number | null
    fillRate24hBps: number | null
    latencyP50Seconds: number | null
    latencyP95Seconds: number | null
  }
  liquidity: {
    latestEpoch: number | null
    readiness: string | null
    confirmedSettlements24h: number | null
  }
  sponsored: {
    configured: boolean
    stack: string
    enabled: boolean
    ops24h: number
    successRate24hBps: number | null
  }
  protocol: {
    releaseVersion: string
    deploymentBlock: string
    pinnedContracts: number
  }
  infra: {
    overallStatus: string
    dbReachable: boolean
    dbPingLatencyMs: number | null
    rpcReachable: boolean
    rpcLatencyMs: number | null
    healthyServices: number
    totalServices: number
    awsControlPlaneAvailable: boolean
  }
}

interface Envelope<T> {
  data: T
  blockNumber: number
  chainId: number
}

async function fetchInfraEnvelope<T>(path: string, signal?: AbortSignal): Promise<Envelope<T>> {
  const res = await fetch(`${API_ROOT}${path}`, {
    headers: { Accept: 'application/json' },
    signal,
  })
  const text = await res.text()
  let body: unknown = null
  if (text.length > 0) {
    try {
      body = JSON.parse(text)
    } catch {
      body = null
    }
  }

  if (!res.ok) {
    const errBody = body as ApiErrorBody | null
    throw new InsightsApiError(
      errBody?.error?.message ?? `Request failed (${String(res.status)})`,
      res.status,
      errBody?.error?.code,
    )
  }

  if (!body || typeof body !== 'object' || !('data' in body)) {
    throw new InsightsApiError('Malformed infra response', res.status)
  }

  return body as Envelope<T>
}

export async function getInfraStatus(signal?: AbortSignal): Promise<InfraStatusResponse> {
  const env = await fetchInfraEnvelope<InfraStatusResponse>('/infra/status', signal)
  return env.data
}

export async function getOverview(signal?: AbortSignal): Promise<OverviewResponse> {
  const env = await fetchInfraEnvelope<OverviewResponse>('/overview', signal)
  return env.data
}

export const infraKeys = {
  all: ['insights', 'infra'] as const,
  status: () => [...infraKeys.all, 'status'] as const,
  overview: () => ['insights', 'overview'] as const,
}

export function useInfraStatus() {
  const queryClient = useQueryClient()
  const staticPreview = queryClient.getDefaultOptions().queries?.refetchInterval === false

  return useQuery({
    queryKey: infraKeys.status(),
    queryFn: ({ signal }) => getInfraStatus(signal),
    staleTime: staticPreview ? Infinity : 15_000,
    refetchInterval: staticPreview ? false : 15_000,
    retry: (failureCount, error) => {
      if (error instanceof InsightsApiError && error.status === 404) return false
      return failureCount < 1
    },
  })
}

export function useOverview() {
  const queryClient = useQueryClient()
  const staticPreview = queryClient.getDefaultOptions().queries?.refetchInterval === false

  return useQuery({
    queryKey: infraKeys.overview(),
    queryFn: ({ signal }) => getOverview(signal),
    staleTime: staticPreview ? Infinity : 15_000,
    refetchInterval: staticPreview ? false : 15_000,
    retry: (failureCount, error) => {
      if (error instanceof InsightsApiError && error.status === 404) return false
      return failureCount < 1
    },
  })
}

export function formatUptimeSeconds(seconds: number | null | undefined): string {
  if (seconds == null || !Number.isFinite(seconds) || seconds < 0) return '—'
  const whole = Math.floor(seconds)
  const days = Math.floor(whole / 86400)
  const hours = Math.floor((whole % 86400) / 3600)
  const mins = Math.floor((whole % 3600) / 60)
  if (days > 0) return `${String(days)}d ${String(hours)}h`
  if (hours > 0) return `${String(hours)}h ${String(mins)}m`
  if (mins > 0) return `${String(mins)}m`
  return `${String(whole)}s`
}

export function formatLatencyMs(ms: number | null | undefined): string {
  if (ms == null || !Number.isFinite(ms) || ms < 0) return '—'
  if (ms >= 1000) return `${(ms / 1000).toFixed(2)}s`
  return `${String(Math.round(ms))} ms`
}

export function formatBytes(raw: string | number | null | undefined): string {
  if (raw == null) return '—'
  const bytes = typeof raw === 'number' ? raw : Number(raw)
  if (!Number.isFinite(bytes) || bytes < 0) return '—'
  if (bytes >= 1024 ** 3) return `${(bytes / 1024 ** 3).toFixed(2)} GiB`
  if (bytes >= 1024 ** 2) return `${(bytes / 1024 ** 2).toFixed(1)} MiB`
  if (bytes >= 1024) return `${(bytes / 1024).toFixed(1)} KiB`
  return `${String(Math.round(bytes))} B`
}

export function formatAgeDuration(seconds: number | null | undefined): string {
  if (seconds == null || !Number.isFinite(seconds) || seconds < 0) return '—'
  const whole = Math.floor(seconds)
  if (whole < 60) return `${String(whole)}s ago`
  const mins = Math.floor(whole / 60)
  const rem = whole % 60
  if (mins < 60) return rem > 0 ? `${String(mins)}m ${String(rem)}s ago` : `${String(mins)}m ago`
  const hrs = Math.floor(mins / 60)
  const remMin = mins % 60
  if (hrs < 48) return remMin > 0 ? `${String(hrs)}h ${String(remMin)}m ago` : `${String(hrs)}h ago`
  const days = Math.floor(hrs / 24)
  return `${String(days)}d ago`
}

export function formatInfraReason(code: string | null | undefined): string {
  if (!code) return 'Unverified'
  switch (code) {
    case 'READY':
      return 'All checks healthy'
    case 'DB_UNAVAILABLE':
      return 'Database probe unreachable'
    case 'RPC_NODE_LAG':
      return 'Arbitrum Nitro RPC lagging or syncing'
    case 'BUNDLER_UNAVAILABLE':
      return 'Alto bundler unreachable or EntryPoint mismatch'
    case 'KEEPER_STALE':
      return 'Order keeper heartbeat stale or down'
    case 'PROTECTION_WORKER_LAG':
      return 'Protection worker checkpoint stale'
    case 'ORACLE_UNAVAILABLE':
      return 'Pyth oracle feeds stale or unavailable'
    case 'OPEN_EXECUTION_UNAVAILABLE':
      return 'Open execution blocked by oracle or trading policy'
    case 'PROTECTION_TRIGGER_UNAVAILABLE':
      return 'Oracle frozen for armed protection triggers'
    case 'FUNDING_UNVERIFIED':
      return 'Funding unverified'
    case 'FUNDING_LOW':
      return 'Low ETH runway'
    case 'FUNDING_EMPTY':
      return 'ETH depleted'
    default:
      return code
        .toLowerCase()
        .replaceAll('_', ' ')
        .replace(/^\w/, (ch) => ch.toUpperCase())
  }
}

export function infraHealthTone(status: string | null | undefined): StatusTone {
  switch (status) {
    case 'healthy':
    case 'ready':
    case 'available':
    case 'running':
    case 'ok':
      return 'live'
    case 'degraded':
    case 'scaled_zero':
    case 'warning':
    case 'modifying':
    case 'backing-up':
      return 'stale'
    case 'down':
    case 'blocked':
    case 'critical':
    case 'impaired':
    case 'stopped':
      return 'blocked'
    default:
      return 'unknown'
  }
}

export function infraHealthLabel(status: string | null | undefined): string {
  switch (status) {
    case 'healthy':
      return 'Healthy'
    case 'ready':
      return 'Ready'
    case 'degraded':
      return 'Degraded'
    case 'down':
      return 'Down'
    case 'blocked':
      return 'Blocked'
    case 'scaled_zero':
      return 'Scaled to 0'
    default:
      return 'Unknown'
  }
}

export function ecsTaskSummary(ecs: AwsEcsStatus | null | undefined): {
  label: string
  tone: StatusTone
} {
  if (!ecs) {
    return { label: 'ECS unverified', tone: 'unknown' }
  }
  const base = `${String(ecs.runningCount)}/${String(ecs.desiredCount)} tasks`
  if (ecs.desiredCount === 0) {
    return { label: '0/0 scaled', tone: 'stale' }
  }
  if (ecs.runningCount === 0) {
    return { label: `${base} down`, tone: 'blocked' }
  }
  if (ecs.runningCount < ecs.desiredCount || ecs.pendingCount > 0) {
    return {
      label: ecs.pendingCount > 0 ? `${base} (+${String(ecs.pendingCount)} pending)` : base,
      tone: 'stale',
    }
  }
  return { label: base, tone: 'live' }
}

export function deriveInfraBanner(
  data: InfraStatusResponse | undefined,
  isLoading: boolean,
  error: unknown,
): { tone: StatusTone; statusLabel: string; note: string } {
  if (error instanceof InsightsApiError && error.status === 404) {
    return {
      tone: 'unknown',
      statusLabel: 'Awaiting Stage 6 backend deploy',
      note: 'Endpoint /api/insights/v1/infra/status is not active on this backend yet',
    }
  }
  if (error) {
    return {
      tone: 'blocked',
      statusLabel: 'Infra telemetry unavailable',
      note: 'Unable to load AWS & runtime health probes',
    }
  }
  if (isLoading || !data) {
    return {
      tone: 'unknown',
      statusLabel: 'Checking AWS infrastructure & runtime probes',
      note: 'Querying ECS, RDS, EC2, Alto bundler, PostgreSQL, and Arbitrum Nitro RPC',
    }
  }

  const { healthyServices, totalServices } = data.aws.summary
  const ratio = `${String(healthyServices)}/${String(totalServices)}`
  const cpNote = data.aws.controlPlaneAvailable
    ? 'AWS ECS/RDS/EC2 control-plane + live probes active'
    : 'Live runtime probes active (AWS control-plane API unconfigured)'

  if (data.overallStatus === 'down') {
    return {
      tone: 'blocked',
      statusLabel: `Core infrastructure degraded (${ratio} workers healthy)`,
      note: cpNote,
    }
  }
  if (data.overallStatus === 'degraded') {
    return {
      tone: 'stale',
      statusLabel: `Degraded (${ratio} workers healthy)`,
      note: cpNote,
    }
  }
  return {
    tone: 'live',
    statusLabel: `All AWS & protocol services healthy (${ratio} workers)`,
    note: cpNote,
  }
}
