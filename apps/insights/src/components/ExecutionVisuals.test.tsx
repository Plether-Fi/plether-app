import { fireEvent, render, screen } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import {
  ExecutionLatencyWhisker,
  OrderOutcomeProportionCard,
} from './ExecutionVisuals'

describe('ExecutionVisuals', () => {
  it('renders chronological time-axis blocks and inspects exact failure windows on click', () => {
    render(
      <OrderOutcomeProportionCard
        cohort={{
          windowSeconds: 86400,
          totalOrders: 100,
          executedOrders: 94,
          expiredOrders: 4,
          otherFailedOrders: 2,
          pendingOrders: 0,
          activeAccounts: 30,
          affectedAccounts: 3,
          repeatAffectedAccounts: 1,
          fillRateBps: 9400,
          expiryRateBps: 400,
          timeline: [
            {
              bucketStart: '2026-11-26T10:00:00Z',
              bucketEnd: '2026-11-26T10:30:00Z',
              totalOrders: 40,
              executedOrders: 40,
              expiredOrders: 0,
              otherFailedOrders: 0,
              pendingOrders: 0,
            },
            {
              bucketStart: '2026-11-26T10:30:00Z',
              bucketEnd: '2026-11-26T11:00:00Z',
              totalOrders: 0,
              executedOrders: 0,
              expiredOrders: 0,
              otherFailedOrders: 0,
              pendingOrders: 0,
            },
            {
              bucketStart: '2026-11-26T11:00:00Z',
              bucketEnd: '2026-11-26T11:30:00Z',
              totalOrders: 20,
              executedOrders: 16,
              expiredOrders: 4,
              otherFailedOrders: 0,
              pendingOrders: 0,
            },
            {
              bucketStart: '2026-11-26T11:30:00Z',
              bucketEnd: '2026-11-26T12:00:00Z',
              totalOrders: 40,
              executedOrders: 38,
              expiredOrders: 0,
              otherFailedOrders: 2,
              pendingOrders: 0,
            },
          ],
        }}
      />,
    )

    expect(screen.getByText('24 hours')).toBeInTheDocument()
    expect(screen.getByText('94.00%')).toBeInTheDocument()
    expect(screen.getByText('24h ago')).toBeInTheDocument()
    expect(screen.getByText('Now')).toBeInTheDocument()
    expect(
      screen.getByRole('group', {
        name: /24 hours order timeline: 94 executed, 4 expired, 2 failed/i,
      }),
    ).toBeInTheDocument()

    const expBlock = screen.getByRole('button', { name: /11:00–11:30 UTC · 20 orders · 16 exec · 4 exp/i })
    fireEvent.click(expBlock)
    expect(screen.getByText('11:00–11:30 UTC · 20 orders · 16 exec · 4 exp')).toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: 'Clear' }))
    expect(screen.queryByText('11:00–11:30 UTC · 20 orders · 16 exec · 4 exp')).not.toBeInTheDocument()
  })

  it('fails closed to aggregate proportion bar without fabricating timestamps when timeline is absent', () => {
    render(
      <OrderOutcomeProportionCard
        cohort={{
          windowSeconds: 86400,
          totalOrders: 1000,
          executedOrders: 940,
          expiredOrders: 45,
          otherFailedOrders: 15,
          pendingOrders: 0,
          activeAccounts: 180,
          affectedAccounts: 12,
          repeatAffectedAccounts: 2,
          fillRateBps: 9400,
          expiryRateBps: 450,
        }}
      />,
    )

    expect(screen.getByText('24 hours')).toBeInTheDocument()
    expect(screen.getByText('94.00%')).toBeInTheDocument()
    expect(screen.getByText('1,000 orders')).toBeInTheDocument()
    expect(
      screen.getByRole('img', {
        name: /24 hours order outcomes: 940 executed, 45 expired, 15 failed/i,
      }),
    ).toBeInTheDocument()
    expect(screen.getByText('12/180 accts affected')).toBeInTheDocument()
  })

  it('renders latency whisker across p50/p95/p99/max and toggles cleanly between seconds and blocks', () => {
    render(
      <ExecutionLatencyWhisker
        seconds={{ sampleCount: 985, p50: 2, p95: 4, p99: 9, max: 18 }}
        blocks={{ sampleCount: 985, p50: 8, p95: 16, p99: 36, max: 72 }}
      />,
    )

    expect(
      screen.getByRole('img', {
        name: /Execution latency whisker \(seconds\): p50 2s, p95 4s, p99 9s, max 18s/i,
      }),
    ).toBeInTheDocument()
    expect(screen.getByText('2s')).toBeInTheDocument()
    expect(screen.getByText('18s')).toBeInTheDocument()

    fireEvent.click(screen.getByRole('button', { name: 'Blocks' }))
    expect(
      screen.getByRole('img', {
        name: /Execution latency whisker \(blocks\): p50 8 blk, p95 16 blk, p99 36 blk, max 72 blk/i,
      }),
    ).toBeInTheDocument()
    expect(screen.getByText('72 blk')).toBeInTheDocument()
  })

  it('fails closed when latency has zero samples', () => {
    render(
      <ExecutionLatencyWhisker
        seconds={{ sampleCount: 0, p50: null, p95: null, p99: null, max: null }}
        blocks={{ sampleCount: 0, p50: null, p95: null, p99: null, max: null }}
      />,
    )

    expect(screen.getByText('Latency distribution unavailable')).toBeInTheDocument()
  })
})
