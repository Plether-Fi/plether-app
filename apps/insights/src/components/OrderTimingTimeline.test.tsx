import { fireEvent, render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import type { FinalizedOrderRecord, ProtocolLatencyBenchmark } from '../api'
import { OrderTimingTimelinePanel } from './OrderTimingTimeline'

const BENCHMARK: ProtocolLatencyBenchmark = {
  windowLabel: '7d',
  sampleCount: 1420,
  minSeconds: 1,
  p25Seconds: 1,
  p50Seconds: 2,
  p75Seconds: 3,
  p95Seconds: 5,
  p99Seconds: 9,
  maxSeconds: 18,
  p50CommitToOracleSeconds: 1,
  p50OracleToFinalizeSeconds: 1,
}

const ORDER_A: FinalizedOrderRecord = {
  orderId: '10421',
  account: '0x1111111111111111111111111111111111111111',
  side: 'long',
  commitTxHash: '0xa1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f60001',
  commitBlock: '333079609',
  commitAt: '2026-11-27T06:22:08Z',
  finalizedBlock: '333079617',
  finalizedAt: '2026-11-27T06:22:10Z',
  oracleMinPublishTime: '2026-11-27T06:22:08Z',
  oracleMaxPublishTime: '2026-11-27T06:22:09Z',
  oracleSpreadSeconds: 1,
  commitToOracleMinSeconds: 0,
  commitToOracleMaxSeconds: 1,
  oracleToFinalizeSeconds: 1,
  latencySeconds: 2,
  latencyBlocks: 8,
  latencyPercentile: 42,
  terminalStatus: 'Executed',
  terminalReason: 'Executed',
  failureReason: null,
  failedConstraint: null,
  executionMode: 'live',
  executionPrice: '94585000',
  oraclePrice: '94589000',
  oracleFrozen: false,
  vpiAssets: '12500000',
  frozenCloseSpreadAssets: null,
}

const ORDER_B: FinalizedOrderRecord = {
  ...ORDER_A,
  orderId: '10422',
  side: 'short',
  commitAt: '2026-11-27T06:22:03Z',
  commitToOracleMaxSeconds: 2,
  oracleToFinalizeSeconds: 5,
  latencySeconds: 7,
  latencyBlocks: 28,
  latencyPercentile: 97,
}

describe('OrderTimingTimelinePanel', () => {
  it('renders Commit -> Oracle Publish -> Finalize timestamps, stage deltas, and protocol percentile gauge', () => {
    render(
      <OrderTimingTimelinePanel
        finalizedOrders={[ORDER_A]}
        activeOrderId="10421"
        latencyBenchmark={BENCHMARK}
        chainId={421614}
      />,
    )

    expect(screen.getByText('Commit → Oracle Publish → Finalize Timeline')).toBeInTheDocument()
    expect(screen.getByText('Order #10421')).toBeInTheDocument()
    expect(screen.getByText('P42 · Typical')).toBeInTheDocument()
    expect(screen.getByText('27 Nov 2026, 06:22:08 UTC')).toBeInTheDocument()
    expect(screen.getByText('06:22:08 → 06:22:09 UTC')).toBeInTheDocument()
    expect(screen.getByText('27 Nov 2026, 06:22:10 UTC')).toBeInTheDocument()
    expect(screen.getByText('(~2,000 ms est.)')).toBeInTheDocument()
    expect(screen.getByText('~2,000 ms (~250ms/blk)')).toBeInTheDocument()
    expect(screen.getByText(/Protocol Latency Position \(7d · 1,420 executed orders\)/i)).toBeInTheDocument()
    expect(screen.getAllByText(/p50 2s/i).length).toBeGreaterThanOrEqual(1)
    expect(screen.getAllByText(/p95 5s/i).length).toBeGreaterThanOrEqual(1)
  })

  it('allows switching between orders in a batch when activeOrderId is not locked', () => {
    render(
      <OrderTimingTimelinePanel
        finalizedOrders={[ORDER_A, ORDER_B]}
        activeOrderId={null}
        latencyBenchmark={BENCHMARK}
        chainId={421614}
      />,
    )

    expect(screen.getByText('P42 · Typical')).toBeInTheDocument()
    fireEvent.click(screen.getByRole('button', { name: '#10422 (7s)' }))
    expect(screen.getByText('P97 · Slow tail (> p95)')).toBeInTheDocument()
  })
})
