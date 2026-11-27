import { fireEvent, render, screen } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { InsightsApiError, type InfraStatusResponse } from '../api'
import { InfraPage } from './InfraPage'

const infraQuery = vi.hoisted(() => vi.fn())
const protocolContext = vi.hoisted(() => vi.fn())

vi.mock('../api', async () => {
  const actual = await vi.importActual<typeof import('../api')>('../api')
  return {
    ...actual,
    useInfraStatus: infraQuery,
  }
})

vi.mock('../hooks/useProtocolContext', () => ({
  useProtocolContext: protocolContext,
}))

const SAMPLE_DATA: InfraStatusResponse = {
  network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
  observedAt: '2026-11-26T22:00:00Z',
  overallStatus: 'healthy',
  actions: {
    deposit: { label: 'Deposit margin', status: 'ready', reason: 'READY' },
    open: { label: 'Open position', status: 'ready', reason: 'READY' },
    close: { label: 'Close position', status: 'ready', reason: 'READY' },
    protection: { label: 'TP/SL protection', status: 'degraded', reason: 'PROTECTION_WORKER_LAG' },
  },
  aws: {
    controlPlaneAvailable: true,
    backend: {
      status: 'healthy',
      uptimeSeconds: 93600,
      releaseVersion: 'v2.0.0',
      ecs: { status: 'ACTIVE', desiredCount: 2, runningCount: 2, pendingCount: 0, rolloutState: 'COMPLETED' },
    },
    database: {
      reachable: true,
      pingLatencyMs: 5,
      activeConnections: 4,
      idleConnections: 16,
      totalConnections: 20,
      maxConnections: 100,
      databaseSizeBytes: '2147483648',
      inRecovery: false,
      rds: {
        status: 'available',
        engineVersion: '16.4',
        multiAz: false,
        storageEncrypted: true,
        allocatedStorageGb: 100,
      },
    },
    node: {
      reachable: true,
      rpcLatencyMs: 11,
      headBlock: '332945120',
      headTimestamp: '2026-11-26T21:59:59Z',
      headLagSeconds: 1,
      syncing: false,
      peerCount: 24,
      ec2: { instanceState: 'running', systemStatus: 'ok', instanceStatus: 'ok' },
      gatewayEcs: { status: 'ACTIVE', desiredCount: 2, runningCount: 2, pendingCount: 0, rolloutState: 'COMPLETED' },
    },
    alto: {
      configured: true,
      reachable: true,
      probeLatencyMs: 17,
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
        lastBlock: '332945119',
        blocksBehind: 1,
        lastHeartbeatAt: '2026-11-26T21:59:56Z',
        heartbeatAgeSeconds: 4,
        freshThresholdSeconds: 120,
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
        lastHeartbeatAt: '2026-11-26T21:59:58Z',
        heartbeatAgeSeconds: 2,
        freshThresholdSeconds: 60,
        fundingState: 'ready',
        fundingReason: 'READY',
        ecs: { status: 'ACTIVE', desiredCount: 1, runningCount: 1, pendingCount: 0, rolloutState: 'COMPLETED' },
      },
      {
        key: 'protection_worker',
        name: 'TP/SL Protection Worker',
        category: 'execution',
        status: 'degraded',
        lastBlock: '332944800',
        blocksBehind: 320,
        lastHeartbeatAt: '2026-11-26T21:56:00Z',
        heartbeatAgeSeconds: 240,
        freshThresholdSeconds: 180,
        fundingState: 'degraded',
        fundingReason: 'FUNDING_LOW',
        ecs: { status: 'ACTIVE', desiredCount: 1, runningCount: 1, pendingCount: 0, rolloutState: 'COMPLETED' },
      },
    ],
    summary: { healthyServices: 2, totalServices: 3 },
  },
  oracle: {
    latestPublishTime: '2026-11-26T21:59:58Z',
    ageSeconds: 2,
    feedSpreadSeconds: 1,
    openPolicyStatus: 'ready',
    closePolicyStatus: 'ready',
    protectionPolicyStatus: 'ready',
  },
}

beforeEach(() => {
  protocolContext.mockReturnValue({
    chain: {
      chainId: 421614,
      name: 'Arbitrum Sepolia',
      shortName: 'Arb Sepolia',
      isTestnet: true,
      explorerBaseUrl: 'https://sepolia.arbiscan.io',
      tradeAppUrl: 'https://sepolia.plether.com',
      defaultAssetSymbol: 'USDG',
    },
    releaseVersion: 'v2.0.0',
    blockNumber: 332945120,
    observedAt: '2026-11-26T22:00:00Z',
    assetSymbol: 'USDG',
  })
  infraQuery.mockReturnValue({
    data: SAMPLE_DATA,
    isLoading: false,
    isError: false,
    error: null,
  })
})

function renderPage() {
  return render(
    <MemoryRouter>
      <InfraPage />
    </MemoryRouter>,
  )
}

describe('InfraPage', () => {
  it('renders user action readiness, AWS core infra cards, ECS fleet matrix, and oracle status', () => {
    renderPage()
    expect(screen.getByRole('heading', { name: /AWS Stack, Keepers, Database, Alto & RPC Health/ })).toBeInTheDocument()
    expect(screen.getByText('AWS control plane connected')).toBeInTheDocument()

    // Core AWS cards
    expect(screen.getByText('Backend API Service')).toBeInTheDocument()
    expect(screen.getByText('1d 2h')).toBeInTheDocument()
    expect(screen.getByText('Primary Database')).toBeInTheDocument()
    expect(screen.getByText('20 / 100 (20%)')).toBeInTheDocument()
    expect(screen.getByText('2.00 GiB')).toBeInTheDocument()
    expect(screen.getByText('Self-Hosted RPC Node')).toBeInTheDocument()
    expect(screen.getByText('Alto ERC-4337 Bundler')).toBeInTheDocument()
    expect(screen.getByText('v0.9')).toBeInTheDocument()

    // Workers table & category filtering
    expect(screen.getByText('Order Execution Keeper')).toBeInTheDocument()
    expect(screen.getByText('Pyth Basket / Oracle Worker')).toBeInTheDocument()
    expect(screen.getByText('TP/SL Protection Worker')).toBeInTheDocument()

    fireEvent.click(screen.getByRole('tab', { name: 'Oracle' }))
    expect(screen.getByText('Pyth Basket / Oracle Worker')).toBeInTheDocument()
    expect(screen.queryByText('Order Execution Keeper')).not.toBeInTheDocument()
  })

  it('renders fail-closed Stage 6 pending deploy state cleanly on 404', () => {
    infraQuery.mockReturnValue({
      data: undefined,
      isLoading: false,
      isError: true,
      error: new InsightsApiError('Not found', 404),
    })
    renderPage()
    expect(screen.getByText('Awaiting Stage 6 backend deploy')).toBeInTheDocument()
    expect(screen.getByText('Stage 6 backend endpoint awaiting deployment')).toBeInTheDocument()
    expect(screen.getAllByText('Unavailable')).toHaveLength(4)
  })
})
