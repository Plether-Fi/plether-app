import type { Meta, StoryObj } from '@storybook/react-vite'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { useMemo } from 'react'
import { MemoryRouter } from 'react-router-dom'
import type {
  LiquidityEfficiencyResponse,
  LiquidityForensicsResponse,
  LiquiditySettlementsResponse,
  PoolEfficiencyBucket,
} from '../../../insights/src/api/liquidity'
import { PoolEfficiencySection } from '../../../insights/src/components/PoolEfficiencySection'
import { LiquidityOpsPage } from '../../../insights/src/pages/LiquidityOpsPage'
import './InsightsPositionBreakdown.css'

function buildSampleSeries(count: number, stepSeconds: number): PoolEfficiencyBucket[] {
  const endMs = Date.parse('2026-11-28T12:00:00Z')
  const startMs = endMs - count * stepSeconds * 1000
  const isSixHour = stepSeconds === 21600
  return Array.from({ length: count }, (_, i) => {
    const startIso = new Date(startMs + i * stepSeconds * 1000).toISOString()
    const endIso = new Date(startMs + (i + 1) * stepSeconds * 1000).toISOString()
    const wave = Math.sin(i * 0.55) * 0.35 + Math.cos(i * 0.28) * 0.2
    const poolUsd = Math.round(780_000 + i * (isSixHour ? 1_600 : 1_500))
    const juniorUsd = Math.round(poolUsd * 0.25)
    const seniorUsd = poolUsd - juniorUsd
    const dailyVolUsd = Math.round(145_000_000 * (1 + wave) + (i % 5 === 0 ? 48_000_000 : 0))
    const bucketVolUsd = isSixHour ? Math.round(dailyVolUsd / 4) : dailyVolUsd
    const takeRateBps = 6.75 + (i % 4) * 0.14
    const bucketFeesUsd = Math.round((bucketVolUsd * takeRateBps) / 10_000)

    const longOiUsd = Math.round(280_000 + 130_000 * (0.5 + 0.5 * Math.sin(i * 0.42)))
    const shortOiUsd = Math.round(220_000 + 115_000 * (0.5 + 0.5 * Math.cos(i * 0.37)))
    const grossOiUsd = longOiUsd + shortOiUsd
    const netSkewUsd = longOiUsd - shortOiUsd

    const turnoverX100 = Math.round((bucketVolUsd / poolUsd) * 100)
    const annualizedDailyX100 = isSixHour ? turnoverX100 * 4 : turnoverX100

    return {
      bucketStart: startIso,
      bucketEnd: endIso,
      volumeAssets: String(bucketVolUsd * 1_000_000),
      feesAssets: String(bucketFeesUsd * 1_000_000),
      tradesCount: isSixHour ? 320 + (i % 7) * 44 : 1_320 + (i % 6) * 190,
      poolTvlAssets: String(poolUsd * 1_000_000),
      seniorTvlAssets: String(seniorUsd * 1_000_000),
      juniorTvlAssets: String(juniorUsd * 1_000_000),
      turnoverX100,
      annualizedTurnoverDailyX100: annualizedDailyX100,
      takeRateBpsX100: Math.round(takeRateBps * 100),
      longOiAssets: String(longOiUsd * 1_000_000),
      shortOiAssets: String(shortOiUsd * 1_000_000),
      grossOiAssets: String(grossOiUsd * 1_000_000),
      netSkewAssets: String(netSkewUsd * 1_000_000),
      grossOiToPoolBps: Math.round((grossOiUsd / poolUsd) * 10_000),
      netSkewToPoolBps: Math.round((Math.abs(netSkewUsd) / poolUsd) * 10_000),
    }
  })
}

const SAMPLE_EFFICIENCY: LiquidityEfficiencyResponse = {
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  observedAt: '2026-11-28T12:00:00Z',
  summary: {
    poolTvlAssets: '820000000000',
    seniorTvlAssets: '615000000000',
    juniorTvlAssets: '205000000000',
    volume24hAssets: '208460000000000',
    volume7dAssets: '1240000000000000',
    volume30dAssets: '4280000000000000',
    fees24hAssets: '142500000000',
    fees7dAssets: '842000000000',
    turnover24hX100: 25422,
    turnover7dAvgDailyX100: 21602,
    turnover30dAvgDailyX100: 17398,
    effectiveTakeRate7dBpsX100: 679,
    annualizedFeeYield7dBps: 53540,
    currentLongOiAssets: '382000000000',
    currentShortOiAssets: '258000000000',
    currentGrossOiAssets: '640000000000',
    currentNetSkewAssets: '124000000000',
    grossOiToPoolBps: 7805,
    netSkewToPoolBps: 1512,
    netSkewToJuniorBps: 6049,
  },
  series7d: buildSampleSeries(28, 21600),
  series30d: buildSampleSeries(30, 86400),
}

const SAMPLE_SETTLEMENTS: LiquiditySettlementsResponse = {
  network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
  monitorAddress: '0x87d2eae2783fea3055ce142d060e8db14d8e04d8',
  latestObservation: {
    epoch: 496948,
    observedBlock: '333410200',
    observedAt: '2026-11-28T12:00:00Z',
    readiness: 'ready',
    executionPath: 'cached_mark',
    healthState: 'healthy',
    observationComplete: true,
    hasMaturedWork: true,
    paused: false,
    masks: {
      operationalBlocker: { raw: '0', activeBits: [] },
      warning: { raw: '0', activeBits: [] },
      dependencyFailure: { raw: '0', activeBits: [] },
      criticalFault: { raw: '0', activeBits: [] },
    },
  },
  summary: {
    observedEpochs24h: 24,
    confirmedSettlements24h: 24,
    failedOrReverted24h: 0,
    replacedAttempts24h: 1,
    deferredEntryEpochs24h: 0,
    backlogEpochs24h: 0,
  },
  recentSettlements: [
    {
      epoch: 496948,
      cutoffEpoch: '496947',
      status: 'confirmed',
      executionPath: 'cached_mark',
      txHash: `0x${'a'.repeat(64)}`,
      receiptBlock: '333410190',
      createdAt: '2026-11-28T12:00:01Z',
      confirmedAt: '2026-11-28T12:00:04Z',
      confirmationLatencySeconds: 3,
      replacementCount: 0,
      seniorDepositAssets: '50000000000',
      seniorRedeemAssets: '0',
      juniorDepositAssets: '15000000000',
      juniorRedeemAssets: '0',
      seniorBacklog: false,
      juniorBacklog: false,
      entriesDeferred: false,
    },
  ],
}

const SAMPLE_FORENSICS: LiquidityForensicsResponse = {
  network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
  observedAt: '2026-11-28T12:00:00Z',
  snapshotFreshness: {
    window7d: {
      expectedHours: 169,
      observedHours: 169,
      freshHours: 169,
      staleHours: 0,
      latestEpochTimestamp: 1795867200,
      latestBlockNumber: '333410200',
      latestMarkFresh: true,
    },
    window30d: {
      expectedHours: 721,
      observedHours: 721,
      freshHours: 719,
      staleHours: 2,
      latestEpochTimestamp: 1795867200,
      latestBlockNumber: '333410200',
      latestMarkFresh: true,
    },
  },
  attributionWorker: {
    indexedThroughBlock: '333410200',
    recentReports: [
      {
        startBlockNumber: '333390000',
        endBlockNumber: '333410200',
        sourceCommit: '24a7a466',
        schemaVersion: 3,
        updatedAt: '2026-11-28T11:55:00Z',
        sourceBlockCount: 20200,
        reportedReturnPp: 0.412,
        unexplainedPp: 0,
        netReturnUsdg: '845000000',
        unexplainedUsdg: '0',
        liquidationFeesUsdg: '120000000',
        notesCount: 0,
      },
    ],
  },
  vaultIndexers: {
    activity: {
      lastIndexedBlock: '333410200',
      safeHeadBlock: '333410200',
      lagBlocks: 0,
      lagSeconds: 0,
      pollAgeSeconds: 2,
      backfillComplete: true,
    },
    depositAttribution: {
      confirmedThroughBlock: '333410200',
      pollAgeSeconds: 2,
      backfillComplete: true,
    },
  },
}

function LiquidityOpsCanvas({
  efficiency,
  standalone = false,
}: {
  efficiency: LiquidityEfficiencyResponse
  standalone?: boolean
}) {
  const client = useMemo(() => {
    const qc = new QueryClient({
      defaultOptions: {
        queries: {
          enabled: false,
          retry: false,
          staleTime: Number.POSITIVE_INFINITY,
          refetchInterval: false,
          refetchOnMount: false,
          refetchOnWindowFocus: false,
          refetchOnReconnect: false,
        },
      },
    })
    qc.setQueryData(['insights', 'status'], {
      healthy: true,
      latestIndexedBlock: 333410200,
      latestIndexedAt: '2026-11-28T12:00:00Z',
      chainId: 421614,
    })
    qc.setQueryData(['insights', 'divisions', 'live', 'summary'], {
      status: 'ready',
      scoringRelease: { version: 'v2.0.0', assetSymbol: 'USDG' },
      weekStartsAt: '2026-11-22T21:00:00Z',
      snapshotAt: '2026-11-28T12:00:00Z',
      snapshotBlock: '333410200',
      walletCount: 48,
      topStandings: [],
    })
    qc.setQueryData(['insights', 'liquidity', 'settlements'], SAMPLE_SETTLEMENTS)
    qc.setQueryData(['insights', 'liquidity', 'forensics'], SAMPLE_FORENSICS)
    qc.setQueryData(['insights', 'liquidity', 'efficiency'], efficiency)
    return qc
  }, [efficiency])

  return (
    <QueryClientProvider client={client}>
      <MemoryRouter initialEntries={['/liquidity']}>
        <div className="min-h-screen bg-app-bg p-6 text-content-primary">
          <div className="mx-auto max-w-7xl">
            {standalone ? (
              <PoolEfficiencySection
                data={efficiency}
                isLoading={false}
                isError={false}
                error={null}
              />
            ) : (
              <LiquidityOpsPage />
            )}
          </div>
        </div>
      </MemoryRouter>
    </QueryClientProvider>
  )
}

const meta: Meta<typeof LiquidityOpsCanvas> = {
  title: 'Insights/Liquidity Ops — Pool Capital Efficiency & OI',
  component: LiquidityOpsCanvas,
  parameters: { layout: 'fullscreen' },
}

export default meta
type Story = StoryObj<typeof meta>

export const FullLiquidityOpsPage: Story = {
  args: {
    efficiency: SAMPLE_EFFICIENCY,
    standalone: false,
  },
}

export const PoolEfficiencyChartsOnly: Story = {
  args: {
    efficiency: SAMPLE_EFFICIENCY,
    standalone: true,
  },
}
