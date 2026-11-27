import type { Meta, StoryObj } from '@storybook/react-vite'
import type { OrderReliabilityBucket, OrderReliabilityCohort } from '../../../insights/src/api/execution'
import {
  ExecutionLatencyWhisker,
  OrderOutcomeProportionCard,
} from '../../../insights/src/components/ExecutionVisuals'
import { Panel } from '../../../insights/src/components/ui'
import './InsightsPositionBreakdown.css'

function buildSampleTimeline(
  endIso: string,
  windowSeconds: number,
  bucketCount: number,
  incidents: Record<number, { expired?: number; failed?: number; empty?: boolean }>,
): OrderReliabilityBucket[] {
  const endMs = new Date(endIso).getTime()
  const startMs = endMs - windowSeconds * 1000
  const widthMs = (windowSeconds * 1000) / bucketCount
  return Array.from({ length: bucketCount }, (_, i) => {
    const inc = incidents[i]
    const total = inc?.empty ? 0 : 12 + ((i * 7) % 18)
    const exp = inc?.expired ?? 0
    const fail = inc?.failed ?? 0
    const exec = Math.max(0, total - exp - fail)
    return {
      bucketStart: new Date(startMs + i * widthMs).toISOString(),
      bucketEnd: new Date(startMs + (i + 1) * widthMs).toISOString(),
      totalOrders: total,
      executedOrders: exec,
      expiredOrders: exp,
      otherFailedOrders: fail,
      pendingOrders: 0,
    }
  })
}

const END_ISO = '2026-11-26T22:00:00Z'

const COHORT_1H: OrderReliabilityCohort = {
  windowSeconds: 3600,
  totalOrders: 326,
  executedOrders: 325,
  expiredOrders: 1,
  otherFailedOrders: 0,
  pendingOrders: 0,
  activeAccounts: 178,
  affectedAccounts: 1,
  repeatAffectedAccounts: 0,
  fillRateBps: 9969,
  expiryRateBps: 31,
  timeline: buildSampleTimeline(END_ISO, 3600, 30, {
    9: { empty: true },
    18: { expired: 1 },
  }),
}

const COHORT_24H: OrderReliabilityCohort = {
  windowSeconds: 86400,
  totalOrders: 2073,
  executedOrders: 2065,
  expiredOrders: 5,
  otherFailedOrders: 3,
  pendingOrders: 0,
  activeAccounts: 1120,
  affectedAccounts: 5,
  repeatAffectedAccounts: 1,
  fillRateBps: 9961,
  expiryRateBps: 24,
  timeline: buildSampleTimeline(END_ISO, 86400, 48, {
    6: { empty: true },
    7: { empty: true },
    14: { expired: 2 },
    23: { failed: 2 },
    24: { expired: 1 },
    37: { expired: 2 },
    44: { failed: 1 },
  }),
}

function ExecutionTimelineStory() {
  return (
    <div className="min-h-screen bg-app-bg px-6 py-8 text-content-primary">
      <div className="mx-auto grid max-w-6xl gap-4 lg:grid-cols-2">
        <Panel className="space-y-4 p-5">
          <div>
            <h2 className="text-base font-semibold">Order outcome breakdown</h2>
            <p className="mt-1 text-xs text-content-tertiary">
              Chronological order outcome timeline across 1h (2m/block) and 24h (30m/block) cohorts
            </p>
          </div>
          <div className="space-y-3">
            <OrderOutcomeProportionCard cohort={COHORT_1H} />
            <OrderOutcomeProportionCard cohort={COHORT_24H} />
          </div>
        </Panel>
        <ExecutionLatencyWhisker
          seconds={{ sampleCount: 2065, p50: 2, p95: 4, p99: 8, max: 19 }}
          blocks={{ sampleCount: 2065, p50: 8, p95: 16, p99: 32, max: 76 }}
        />
      </div>
    </div>
  )
}

const meta: Meta<typeof ExecutionTimelineStory> = {
  title: 'Insights/Execution — Time-Axis Order Blocks',
  component: ExecutionTimelineStory,
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta

type Story = StoryObj<typeof ExecutionTimelineStory>

export const ChronologicalIncidents: Story = {}
