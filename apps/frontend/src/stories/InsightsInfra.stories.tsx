import type { Meta, StoryObj } from '@storybook/react-vite'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { useMemo } from 'react'
import { MemoryRouter } from 'react-router-dom'
import { infraKeys, type InfraStatusResponse } from '../../../insights/src/api/infra'
import { InfraPage } from '../../../insights/src/pages/InfraPage'
import './InsightsPositionBreakdown.css'

const HEALTHY_INFRA: InfraStatusResponse = {
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  observedAt: '2026-11-26T22:15:00Z',
  overallStatus: 'healthy',
  actions: {
    deposit: { label: 'Deposit margin', status: 'ready', reason: 'READY' },
    open: { label: 'Open position', status: 'ready', reason: 'READY' },
    close: { label: 'Close position', status: 'ready', reason: 'READY' },
    protection: { label: 'TP/SL protection', status: 'ready', reason: 'READY' },
  },
  aws: {
    controlPlaneAvailable: true,
    backend: {
      status: 'healthy',
      uptimeSeconds: 412800,
      releaseVersion: 'v2.0.0',
      ecs: { status: 'ACTIVE', desiredCount: 2, runningCount: 2, pendingCount: 0, rolloutState: 'COMPLETED' },
    },
    database: {
      reachable: true,
      pingLatencyMs: 4,
      activeConnections: 7,
      idleConnections: 21,
      totalConnections: 28,
      maxConnections: 100,
      databaseSizeBytes: '4831838208',
      inRecovery: false,
      rds: {
        status: 'available',
        engineVersion: '16.4',
        multiAz: true,
        storageEncrypted: true,
        allocatedStorageGb: 100,
      },
    },
    node: {
      reachable: true,
      rpcLatencyMs: 9,
      headBlock: '332948912',
      headTimestamp: '2026-11-26T22:14:59Z',
      headLagSeconds: 1,
      syncing: false,
      peerCount: 28,
      ec2: { instanceState: 'running', systemStatus: 'ok', instanceStatus: 'ok' },
      gatewayEcs: { status: 'ACTIVE', desiredCount: 2, runningCount: 2, pendingCount: 0, rolloutState: 'COMPLETED' },
    },
    alto: {
      configured: true,
      reachable: true,
      probeLatencyMs: 14,
      entryPointVersion: '0.9',
      entryPointSupported: true,
      fundingState: 'ready',
      fundingReason: 'READY',
      ecs: { status: 'ACTIVE', desiredCount: 1, runningCount: 1, pendingCount: 0, rolloutState: 'COMPLETED' },
    },
    services: [
      {
        key: 'keeper',
        name: 'Order Execution Keeper',
        category: 'execution',
        status: 'healthy',
        lastBlock: '332948911',
        blocksBehind: 1,
        lastHeartbeatAt: '2026-11-26T22:14:57Z',
        heartbeatAgeSeconds: 3,
        freshThresholdSeconds: 120,
        fundingState: 'ready',
        fundingReason: 'READY',
        ecs: { status: 'ACTIVE', desiredCount: 1, runningCount: 1, pendingCount: 0, rolloutState: 'COMPLETED' },
      },
      {
        key: 'liquidation_worker',
        name: 'Liquidation Worker',
        category: 'execution',
        status: 'healthy',
        lastBlock: '332948910',
        blocksBehind: 2,
        lastHeartbeatAt: '2026-11-26T22:14:52Z',
        heartbeatAgeSeconds: 8,
        freshThresholdSeconds: 180,
        fundingState: 'ready',
        fundingReason: 'READY',
        ecs: { status: 'ACTIVE', desiredCount: 1, runningCount: 1, pendingCount: 0, rolloutState: 'COMPLETED' },
      },
      {
        key: 'protection_worker',
        name: 'TP/SL Protection Worker',
        category: 'execution',
        status: 'healthy',
        lastBlock: '332948909',
        blocksBehind: 3,
        lastHeartbeatAt: '2026-11-26T22:14:54Z',
        heartbeatAgeSeconds: 6,
        freshThresholdSeconds: 180,
        fundingState: 'ready',
        fundingReason: 'READY',
        ecs: { status: 'ACTIVE', desiredCount: 1, runningCount: 1, pendingCount: 0, rolloutState: 'COMPLETED' },
      },
      {
        key: 'basket_worker',
        name: 'Pyth Basket / Oracle Worker',
        category: 'oracle',
        status: 'healthy',
        lastBlock: null,
        blocksBehind: null,
        lastHeartbeatAt: '2026-11-26T22:14:58Z',
        heartbeatAgeSeconds: 2,
        freshThresholdSeconds: 60,
        fundingState: 'ready',
        fundingReason: 'READY',
        ecs: { status: 'ACTIVE', desiredCount: 1, runningCount: 1, pendingCount: 0, rolloutState: 'COMPLETED' },
      },
      {
        key: 'lp_settlement',
        name: 'LP Settlement Worker',
        category: 'liquidity',
        status: 'healthy',
        lastBlock: '332948890',
        blocksBehind: 22,
        lastHeartbeatAt: '2026-11-26T22:14:15Z',
        heartbeatAgeSeconds: 45,
        freshThresholdSeconds: 600,
        fundingState: 'ready',
        fundingReason: 'READY',
        ecs: { status: 'ACTIVE', desiredCount: 1, runningCount: 1, pendingCount: 0, rolloutState: 'COMPLETED' },
      },
      {
        key: 'aa_reconciler',
        name: 'AA Sponsorship Reconciler',
        category: 'sponsorship',
        status: 'healthy',
        lastBlock: '332948900',
        blocksBehind: 12,
        lastHeartbeatAt: '2026-11-26T22:14:49Z',
        heartbeatAgeSeconds: 11,
        freshThresholdSeconds: 600,
        fundingState: null,
        fundingReason: null,
        ecs: { status: 'ACTIVE', desiredCount: 1, runningCount: 1, pendingCount: 0, rolloutState: 'COMPLETED' },
      },
      {
        key: 'perps_indexer',
        name: 'Perps History Indexer',
        category: 'indexing',
        status: 'healthy',
        lastBlock: '332948912',
        blocksBehind: 0,
        lastHeartbeatAt: '2026-11-26T22:14:59Z',
        heartbeatAgeSeconds: 1,
        freshThresholdSeconds: 180,
        fundingState: null,
        fundingReason: null,
        ecs: { status: 'ACTIVE', desiredCount: 1, runningCount: 1, pendingCount: 0, rolloutState: 'COMPLETED' },
      },
      {
        key: 'vault_indexers',
        name: 'Vault Activity & Attribution',
        category: 'indexing',
        status: 'healthy',
        lastBlock: '332948905',
        blocksBehind: 7,
        lastHeartbeatAt: '2026-11-26T22:14:40Z',
        heartbeatAgeSeconds: 20,
        freshThresholdSeconds: 180,
        fundingState: null,
        fundingReason: null,
        ecs: { status: 'ACTIVE', desiredCount: 1, runningCount: 1, pendingCount: 0, rolloutState: 'COMPLETED' },
      },
      {
        key: 'insights_worker',
        name: 'Insights & Divisions Worker',
        category: 'indexing',
        status: 'healthy',
        lastBlock: '332948700',
        blocksBehind: 212,
        lastHeartbeatAt: '2026-11-26T22:12:00Z',
        heartbeatAgeSeconds: 180,
        freshThresholdSeconds: 900,
        fundingState: null,
        fundingReason: null,
        ecs: { status: 'ACTIVE', desiredCount: 1, runningCount: 1, pendingCount: 0, rolloutState: 'COMPLETED' },
      },
    ],
    summary: { healthyServices: 9, totalServices: 9 },
  },
  oracle: {
    latestPublishTime: '2026-11-26T22:14:58Z',
    ageSeconds: 2,
    feedSpreadSeconds: 1,
    openPolicyStatus: 'ready',
    closePolicyStatus: 'ready',
    protectionPolicyStatus: 'ready',
  },
}

const DEGRADED_INFRA: InfraStatusResponse = {
  ...HEALTHY_INFRA,
  overallStatus: 'degraded',
  actions: {
    deposit: { label: 'Deposit margin', status: 'degraded', reason: 'RPC_NODE_LAG' },
    open: { label: 'Open position', status: 'degraded', reason: 'RPC_NODE_LAG' },
    close: { label: 'Close position', status: 'degraded', reason: 'RPC_NODE_LAG' },
    protection: { label: 'TP/SL protection', status: 'degraded', reason: 'PROTECTION_WORKER_LAG' },
  },
  aws: {
    ...HEALTHY_INFRA.aws,
    database: {
      ...HEALTHY_INFRA.aws.database,
      activeConnections: 32,
      idleConnections: 44,
      totalConnections: 76,
    },
    node: {
      ...HEALTHY_INFRA.aws.node,
      rpcLatencyMs: 285,
      headLagSeconds: 48,
      peerCount: 6,
      gatewayEcs: { status: 'ACTIVE', desiredCount: 2, runningCount: 1, pendingCount: 1, rolloutState: 'IN_PROGRESS' },
    },
    services: HEALTHY_INFRA.aws.services.map((svc) =>
      svc.key === 'protection_worker'
        ? {
            ...svc,
            status: 'degraded',
            blocksBehind: 420,
            heartbeatAgeSeconds: 245,
            fundingState: 'degraded',
            fundingReason: 'FUNDING_LOW',
          }
        : svc.key === 'lp_settlement'
          ? {
              ...svc,
              status: 'down',
              ecs: { status: 'ACTIVE', desiredCount: 1, runningCount: 0, pendingCount: 1, rolloutState: 'IN_PROGRESS' },
            }
          : svc,
    ),
    summary: { healthyServices: 7, totalServices: 9 },
  },
  oracle: {
    ...HEALTHY_INFRA.oracle,
    ageSeconds: 14,
    feedSpreadSeconds: 4,
    protectionPolicyStatus: 'degraded',
  },
}

const OUTAGE_INFRA: InfraStatusResponse = {
  ...HEALTHY_INFRA,
  overallStatus: 'down',
  actions: {
    deposit: { label: 'Deposit margin', status: 'blocked', reason: 'DB_UNAVAILABLE' },
    open: { label: 'Open position', status: 'blocked', reason: 'BUNDLER_UNAVAILABLE' },
    close: { label: 'Close position', status: 'blocked', reason: 'KEEPER_STALE' },
    protection: { label: 'TP/SL protection', status: 'unknown', reason: 'ORACLE_UNAVAILABLE' },
  },
  aws: {
    ...HEALTHY_INFRA.aws,
    backend: {
      ...HEALTHY_INFRA.aws.backend,
      status: 'degraded',
      ecs: { status: 'ACTIVE', desiredCount: 2, runningCount: 1, pendingCount: 1, rolloutState: 'IN_PROGRESS' },
    },
    database: {
      reachable: false,
      pingLatencyMs: null,
      activeConnections: null,
      idleConnections: null,
      totalConnections: null,
      maxConnections: null,
      databaseSizeBytes: null,
      inRecovery: null,
      rds: { status: 'modifying', engineVersion: '16.4', multiAz: true, storageEncrypted: true, allocatedStorageGb: 100 },
    },
    node: {
      reachable: false,
      rpcLatencyMs: null,
      headBlock: null,
      headTimestamp: null,
      headLagSeconds: null,
      syncing: null,
      peerCount: null,
      ec2: { instanceState: 'running', systemStatus: 'impaired', instanceStatus: 'ok' },
      gatewayEcs: { status: 'ACTIVE', desiredCount: 2, runningCount: 0, pendingCount: 2, rolloutState: 'FAILED' },
    },
    alto: {
      ...HEALTHY_INFRA.aws.alto,
      reachable: false,
      probeLatencyMs: null,
      entryPointSupported: false,
      fundingState: 'blocked',
      fundingReason: 'FUNDING_EMPTY',
      ecs: { status: 'ACTIVE', desiredCount: 1, runningCount: 0, pendingCount: 1, rolloutState: 'FAILED' },
    },
    services: HEALTHY_INFRA.aws.services.map((svc, idx) =>
      idx < 4
        ? {
            ...svc,
            status: 'down',
            ecs: { status: 'ACTIVE', desiredCount: 1, runningCount: 0, pendingCount: 0, rolloutState: null },
          }
        : { ...svc, status: 'unknown', heartbeatAgeSeconds: null, lastHeartbeatAt: null },
    ),
    summary: { healthyServices: 0, totalServices: 9 },
  },
  oracle: {
    latestPublishTime: null,
    ageSeconds: null,
    feedSpreadSeconds: null,
    openPolicyStatus: 'unknown',
    closePolicyStatus: 'unknown',
    protectionPolicyStatus: 'unknown',
  },
}

const RUNTIME_ONLY_INFRA: InfraStatusResponse = {
  ...HEALTHY_INFRA,
  aws: {
    ...HEALTHY_INFRA.aws,
    controlPlaneAvailable: false,
    backend: { ...HEALTHY_INFRA.aws.backend, ecs: null },
    database: { ...HEALTHY_INFRA.aws.database, rds: null },
    node: { ...HEALTHY_INFRA.aws.node, ec2: null, gatewayEcs: null },
    alto: { ...HEALTHY_INFRA.aws.alto, ecs: null },
    services: HEALTHY_INFRA.aws.services.map((svc) => ({ ...svc, ecs: null })),
  },
}

function InfraStoryHarness({ status }: { status: InfraStatusResponse }) {
  const client = useMemo(() => {
    const qc = new QueryClient({
      defaultOptions: {
        queries: {
          retry: false,
          staleTime: Infinity,
          refetchInterval: false,
        },
      },
    })
    qc.setQueryData(infraKeys.status(), status)
    qc.setQueryData(['insights', 'status'], {
      healthy: true,
      latestIndexedBlock: 332948912,
      latestIndexedAt: status.observedAt,
      chainId: status.network.chainId,
    })
    return qc
  }, [status])

  return (
    <QueryClientProvider client={client}>
      <MemoryRouter initialEntries={['/infra']}>
        <div className="min-h-screen bg-app-bg px-6 py-8 text-content-primary">
          <div className="mx-auto max-w-7xl">
            <InfraPage />
          </div>
        </div>
      </MemoryRouter>
    </QueryClientProvider>
  )
}

const meta: Meta<typeof InfraStoryHarness> = {
  title: 'Insights/Stage 6 — AWS Infrastructure & Health',
  component: InfraStoryHarness,
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta

type Story = StoryObj<typeof InfraStoryHarness>

export const HealthyAwsStack: Story = {
  args: { status: HEALTHY_INFRA },
}

export const DegradedWorkerAndLaggingNode: Story = {
  args: { status: DEGRADED_INFRA },
}

export const CoreOutageRdsAndRpcDown: Story = {
  args: { status: OUTAGE_INFRA },
}

export const RuntimeProbesOnly: Story = {
  args: { status: RUNTIME_ONLY_INFRA },
}
