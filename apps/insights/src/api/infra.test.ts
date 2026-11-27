import { describe, expect, it } from 'vitest'
import { InsightsApiError } from './client'
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
  type InfraStatusResponse,
} from './infra'

function sampleInfraResponse(
  overrides: Partial<InfraStatusResponse> = {},
): InfraStatusResponse {
  return {
    network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
    observedAt: '2026-11-26T22:00:00Z',
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
        uptimeSeconds: 185400,
        releaseVersion: 'v2.0.0',
        ecs: { status: 'ACTIVE', desiredCount: 2, runningCount: 2, pendingCount: 0, rolloutState: 'COMPLETED' },
      },
      database: {
        reachable: true,
        pingLatencyMs: 4,
        activeConnections: 6,
        idleConnections: 18,
        totalConnections: 24,
        maxConnections: 100,
        databaseSizeBytes: '2684354560',
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
        rpcLatencyMs: 12,
        headBlock: '332945120',
        headTimestamp: '2026-11-26T21:59:59Z',
        headLagSeconds: 1,
        syncing: false,
        peerCount: 26,
        ec2: { instanceState: 'running', systemStatus: 'ok', instanceStatus: 'ok' },
        gatewayEcs: { status: 'ACTIVE', desiredCount: 2, runningCount: 2, pendingCount: 0, rolloutState: 'COMPLETED' },
      },
      alto: {
        configured: true,
        reachable: true,
        probeLatencyMs: 18,
        entryPointVersion: '0.9',
        entryPointSupported: true,
        fundingState: 'ready',
        fundingReason: 'READY',
        ecs: { status: 'ACTIVE', desiredCount: 1, runningCount: 1, pendingCount: 0, rolloutState: 'COMPLETED' },
      },
      services: [],
      summary: { healthyServices: 9, totalServices: 9 },
    },
    oracle: {
      latestPublishTime: '2026-11-26T21:59:58Z',
      ageSeconds: 2,
      feedSpreadSeconds: 1,
      openPolicyStatus: 'ready',
      closePolicyStatus: 'ready',
      protectionPolicyStatus: 'ready',
    },
    ...overrides,
  }
}

describe('infra API helpers', () => {
  it('formats uptime, latency, byte sizes, and ages deterministically', () => {
    expect(formatUptimeSeconds(45)).toBe('45s')
    expect(formatUptimeSeconds(3660)).toBe('1h 1m')
    expect(formatUptimeSeconds(185400)).toBe('2d 3h')
    expect(formatUptimeSeconds(null)).toBe('—')

    expect(formatLatencyMs(4)).toBe('4 ms')
    expect(formatLatencyMs(1250)).toBe('1.25s')
    expect(formatLatencyMs(undefined)).toBe('—')

    expect(formatBytes('2684354560')).toBe('2.50 GiB')
    expect(formatBytes(5242880)).toBe('5.0 MiB')
    expect(formatBytes(null)).toBe('—')

    expect(formatAgeDuration(14)).toBe('14s ago')
    expect(formatAgeDuration(125)).toBe('2m 5s ago')
    expect(formatAgeDuration(7200)).toBe('2h ago')
  })

  it('summarizes ECS task status fail-closed', () => {
    expect(ecsTaskSummary(null)).toEqual({ label: 'ECS unverified', tone: 'unknown' })
    expect(
      ecsTaskSummary({ status: 'ACTIVE', desiredCount: 2, runningCount: 2, pendingCount: 0, rolloutState: 'COMPLETED' }),
    ).toEqual({ label: '2/2 tasks', tone: 'live' })
    expect(
      ecsTaskSummary({ status: 'ACTIVE', desiredCount: 2, runningCount: 1, pendingCount: 1, rolloutState: 'IN_PROGRESS' }),
    ).toEqual({ label: '1/2 tasks (+1 pending)', tone: 'stale' })
    expect(
      ecsTaskSummary({ status: 'ACTIVE', desiredCount: 1, runningCount: 0, pendingCount: 0, rolloutState: null }),
    ).toEqual({ label: '0/1 tasks down', tone: 'blocked' })
    expect(
      ecsTaskSummary({ status: 'ACTIVE', desiredCount: 0, runningCount: 0, pendingCount: 0, rolloutState: null }),
    ).toEqual({ label: '0/0 scaled', tone: 'stale' })
  })

  it('derives banner states for 404 pending deploy, healthy, degraded, and down', () => {
    expect(deriveInfraBanner(undefined, false, new InsightsApiError('Not found', 404)).statusLabel).toBe(
      'Awaiting Stage 6 backend deploy',
    )
    expect(deriveInfraBanner(sampleInfraResponse(), false, null)).toMatchObject({
      tone: 'live',
      statusLabel: 'All AWS & protocol services healthy (9/9 workers)',
    })
    expect(deriveInfraBanner(sampleInfraResponse({ overallStatus: 'degraded' }), false, null).tone).toBe('stale')
    expect(deriveInfraBanner(sampleInfraResponse({ overallStatus: 'down' }), false, null).tone).toBe('blocked')
  })

  it('maps health tones, labels, and sanitized reason codes', () => {
    expect(infraHealthTone('healthy')).toBe('live')
    expect(infraHealthTone('degraded')).toBe('stale')
    expect(infraHealthTone('down')).toBe('blocked')
    expect(infraHealthTone('unknown')).toBe('unknown')

    expect(infraHealthLabel('scaled_zero')).toBe('Scaled to 0')
    expect(formatInfraReason('KEEPER_STALE')).toBe('Order keeper heartbeat stale or down')
    expect(formatInfraReason('CUSTOM_ERROR')).toBe('Custom error')
  })
})
