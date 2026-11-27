import type { Meta, StoryObj } from '@storybook/react-vite'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { useMemo } from 'react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { txKeys, type TransactionDetailsResponse } from '../../../insights/src/api/execution'
import { TransactionPage } from '../../../insights/src/pages/TransactionPage'
import './InsightsPositionBreakdown.css'

const SAMPLE_TX = '0x590a41c8e2d9f7b3a1c5e7d9f2b4a6c8e0d2f4b6a8c0e2d4f6b8a0c2e4d64d4b'
const KEEPER_ADDR = '0x5a71a4094ec81165ada48aa4c27da48ec27e0d6b'
const ACCOUNT_A = '0x1111111111111111111111111111111111111111'
const ACCOUNT_B = '0x2222222222222222222222222222222222222222'
const ACCOUNT_C = '0x3333333333333333333333333333333333333333'
const ROUTER_ADDR = '0x9999999999999999999999999999999999999999'

const BATCH_FINALIZATION: TransactionDetailsResponse = {
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  txHash: SAMPLE_TX,
  txKind: 'batch_finalization',
  blockNumber: '333079617',
  occurredAt: '2026-11-27T06:22:10Z',
  executorAddress: KEEPER_ADDR,
  batchSummary: {
    totalOrders: 3,
    executedOrders: 2,
    failedOrExpiredOrders: 1,
    uniqueAccounts: 3,
    minLatencySeconds: 2,
    medianLatencySeconds: 2,
    maxLatencySeconds: 5,
    totalVpiAssets: '18750000',
    oracleMinPublishTime: '2026-11-27T06:22:08Z',
    oracleMaxPublishTime: '2026-11-27T06:22:09Z',
    oracleSpreadSeconds: 1,
    oraclePrice: '94589000',
    oracleFrozen: false,
  },
  latencyBenchmark: {
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
  },
  finalizedOrders: [
    {
      orderId: '10421',
      account: ACCOUNT_A,
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
      orderProtections: {
        orderId: '10421',
        boundsSource: 'commit_receipt',
        fullBoundsAvailable: true,
        summary: {
          v3EnforcedCount: 5,
          v3PassedCount: 5,
          v3FailedCount: 0,
          v3UnboundedCount: 8,
          hasTakeProfit: true,
          hasStopLoss: true,
          positionProtectionStatus: 'Armed',
        },
        executionChecks: [
          {
            key: 'price_limit',
            category: 'core',
            label: 'Price / slippage limit',
            status: 'passed',
            limitValue: '94500000',
            actualValue: '94585000',
            unit: 'price',
            detail: 'Fill price cleared committed slippage limit',
          },
          {
            key: 'execution_window',
            category: 'core',
            label: 'Execution window',
            status: 'passed',
            limitValue: '60',
            actualValue: '2',
            unit: 'seconds',
            detail: 'Filled in 2s within 60s window',
          },
          {
            key: 'execution_mode',
            category: 'core',
            label: 'Execution regime',
            status: 'passed',
            limitValue: 'Live only',
            actualValue: 'Live',
            unit: 'mode',
            detail: 'Filled in Live regime (Live only)',
          },
          {
            key: 'config_hash',
            category: 'core',
            label: 'Protocol config pin',
            status: 'passed',
            limitValue: '0xff9a3bbf31030658aa5644732816f101ba033ba1b980f84bf30c0fa408072805',
            actualValue: '0xff9a3bbf31030658aa5644732816f101ba033ba1b980f84bf30c0fa408072805',
            unit: 'hash',
            detail: 'Matched pinned config 0xff9a3bbf…072805',
          },
          {
            key: 'max_execution_bounty',
            category: 'core',
            label: 'Max keeper bounty',
            status: 'passed',
            limitValue: '210000',
            actualValue: '200000',
            unit: 'usdg',
            detail: 'Within committed ceiling',
          },
          {
            key: 'max_execution_notional',
            category: 'accounting',
            label: 'Max execution notional',
            status: 'not_set',
            limitValue: null,
            actualValue: '50000000000',
            unit: 'usdg',
            detail: 'Unbounded',
          },
          {
            key: 'max_post_leverage',
            category: 'accounting',
            label: 'Max post-trade leverage',
            status: 'not_set',
            limitValue: null,
            actualValue: '100000',
            unit: 'bps',
            detail: 'Unbounded',
          },
        ],
        positionProtection: {
          protectionId: '42',
          parentOrderId: '10421',
          linkedOrderId: null,
          status: 'Armed',
          triggeredLeg: null,
          takeProfitTriggerPrice: '92500000',
          stopLossTriggerPrice: '96200000',
          triggerMarkPrice: null,
          triggerBountyAssets: '200000',
          armedBlock: '333079617',
          updatedBlock: '333079617',
          txHash: SAMPLE_TX,
        },
      },
    },
    {
      orderId: '10422',
      account: ACCOUNT_B,
      side: 'short',
      commitTxHash: '0xa1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f60002',
      commitBlock: '333079610',
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
      latencyBlocks: 7,
      latencyPercentile: 42,
      terminalStatus: 'Executed',
      terminalReason: 'Executed',
      failureReason: null,
      failedConstraint: null,
      executionMode: 'live',
      executionPrice: '94592000',
      oraclePrice: '94589000',
      oracleFrozen: false,
      vpiAssets: '6250000',
      frozenCloseSpreadAssets: null,
    },
    {
      orderId: '10423',
      account: ACCOUNT_C,
      side: 'long',
      commitTxHash: '0xa1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f60003',
      commitBlock: '333079595',
      commitAt: '2026-11-27T06:22:05Z',
      finalizedBlock: '333079617',
      finalizedAt: '2026-11-27T06:22:10Z',
      oracleMinPublishTime: '2026-11-27T06:22:08Z',
      oracleMaxPublishTime: '2026-11-27T06:22:09Z',
      oracleSpreadSeconds: 1,
      commitToOracleMinSeconds: 3,
      commitToOracleMaxSeconds: 4,
      oracleToFinalizeSeconds: 1,
      latencySeconds: 5,
      latencyBlocks: 22,
      latencyPercentile: 95,
      terminalStatus: 'Expired',
      terminalReason: 'Expired',
      failureReason: '2',
      failedConstraint: 'MAX_SLIPPAGE_EXCEEDED',
      executionMode: 'live',
      executionPrice: null,
      oraclePrice: '94589000',
      oracleFrozen: false,
      vpiAssets: null,
      frozenCloseSpreadAssets: null,
    },
  ],
  protocolEvents: [
    {
      logIndex: 14,
      contractAddress: ROUTER_ADDR,
      eventName: 'PositionOpened',
      account: ACCOUNT_A,
      orderId: '10421',
      side: 'long',
      data: { notionalAssets: '50000000000', executionPrice: '94585000', vpiAssets: '12500000' },
    },
    {
      logIndex: 15,
      contractAddress: ROUTER_ADDR,
      eventName: 'OrderFinalized',
      account: ACCOUNT_A,
      orderId: '10421',
      side: 'long',
      data: { status: '2', terminalReason: 'Executed' },
    },
    {
      logIndex: 19,
      contractAddress: ROUTER_ADDR,
      eventName: 'PositionClosed',
      account: ACCOUNT_B,
      orderId: '10422',
      side: 'short',
      data: { realizedPnlAssets: '111590000', executionPrice: '94592000', vpiAssets: '6250000' },
    },
    {
      logIndex: 20,
      contractAddress: ROUTER_ADDR,
      eventName: 'OrderFinalized',
      account: ACCOUNT_B,
      orderId: '10422',
      side: 'short',
      data: { status: '2', terminalReason: 'Executed' },
    },
    {
      logIndex: 24,
      contractAddress: ROUTER_ADDR,
      eventName: 'OrderFinalized',
      account: ACCOUNT_C,
      orderId: '10423',
      side: 'long',
      data: { status: '3', terminalReason: 'Expired', failureReason: 2 },
    },
  ],
  lpSettlement: null,
  events: [
    {
      id: `${SAMPLE_TX}:14`,
      activityType: 'Open',
      account: ACCOUNT_A,
      orderId: '10421',
      side: 'long',
      price: '94585000',
      sizeDelta: '47431500000000000000000',
      amountAssets: '50000000000',
      pnlAssets: null,
      vpiAssets: '12500000',
      frozenCloseSpreadAssets: null,
      oracleFrozen: false,
      latencySeconds: 2,
      keeperBountyAssets: null,
      txHash: SAMPLE_TX,
      blockNumber: '333079617',
      occurredAt: '2026-11-27T06:22:10Z',
      execution: null,
    },
    {
      id: `${SAMPLE_TX}:19`,
      activityType: 'Close',
      account: ACCOUNT_B,
      orderId: '10422',
      side: 'short',
      price: '94592000',
      sizeDelta: '23717000000000000000000',
      amountAssets: '25000000000',
      pnlAssets: '111590000',
      vpiAssets: '6250000',
      frozenCloseSpreadAssets: null,
      oracleFrozen: false,
      latencySeconds: 2,
      keeperBountyAssets: null,
      txHash: SAMPLE_TX,
      blockNumber: '333079617',
      occurredAt: '2026-11-27T06:22:10Z',
      execution: null,
    },
  ],
}

const LP_SETTLEMENT_TX: TransactionDetailsResponse = {
  network: {
    chainId: 421614,
    releaseVersion: 'v2.0.0',
    settlementAssetSymbol: 'USDG',
  },
  txHash: SAMPLE_TX,
  txKind: 'lp_settlement',
  blockNumber: '332948912',
  occurredAt: '2026-11-26T00:00:12Z',
  executorAddress: KEEPER_ADDR,
  batchSummary: null,
  finalizedOrders: [],
  protocolEvents: [
    {
      logIndex: 5,
      contractAddress: ROUTER_ADDR,
      eventName: 'LpEpochSettled',
      account: null,
      orderId: null,
      side: null,
      data: { epoch: 498776, seniorDepositAssets: '48500000000', juniorDepositAssets: '22000000000' },
    },
  ],
  lpSettlement: {
    epoch: 498776,
    status: 'confirmed_success',
    seniorDepositAssets: '48500000000',
    seniorRedeemAssets: '12000000000',
    juniorDepositAssets: '22000000000',
    juniorRedeemAssets: '5000000000',
    seniorBacklog: false,
    juniorBacklog: false,
    entriesDeferred: false,
  },
  events: [],
}

function TransactionStoryHarness({ data }: { data: TransactionDetailsResponse }) {
  const client = useMemo(() => {
    const qc = new QueryClient({
      defaultOptions: {
        queries: {
          enabled: false,
          retry: false,
          staleTime: Infinity,
          gcTime: Infinity,
          refetchInterval: false,
          refetchOnMount: false,
          refetchOnWindowFocus: false,
          refetchOnReconnect: false,
        },
      },
    })
    qc.setQueryData(txKeys.detail(data.txHash), data)
    qc.setQueryData(['insights', 'status'], {
      healthy: true,
      latestIndexedBlock: Number(data.blockNumber),
      latestIndexedAt: data.occurredAt,
      chainId: data.network.chainId,
    })
    qc.setQueryData(['insights', 'divisions', 'live'], {
      status: 'ready',
      snapshotBlock: data.blockNumber,
      snapshotAt: data.occurredAt,
      scoringRelease: { version: data.network.releaseVersion, assetSymbol: 'USDG' },
      walletCount: 42,
      standings: [],
    })
    return qc
  }, [data])

  return (
    <QueryClientProvider client={client}>
      <MemoryRouter initialEntries={[`/tx/${data.txHash}`]}>
        <div className="min-h-screen bg-app-bg px-6 py-8 text-content-primary">
          <div className="mx-auto max-w-7xl">
            <Routes>
              <Route path="/tx/:txHash" element={<TransactionPage />} />
            </Routes>
          </div>
        </div>
      </MemoryRouter>
    </QueryClientProvider>
  )
}

const meta: Meta<typeof TransactionStoryHarness> = {
  title: 'Insights/Execution — Batch Finalization & Tx Inspector',
  component: TransactionStoryHarness,
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta

type Story = StoryObj<typeof TransactionStoryHarness>

export const BatchFinalizationMultiOrder: Story = {
  args: { data: BATCH_FINALIZATION },
}

export const SlowTailOrderTiming: Story = {
  args: {
    data: {
      ...BATCH_FINALIZATION,
      txKind: 'order_finalization',
      batchSummary: {
        ...BATCH_FINALIZATION.batchSummary!,
        totalOrders: 1,
        executedOrders: 1,
        failedOrExpiredOrders: 0,
        uniqueAccounts: 1,
        minLatencySeconds: 7,
        medianLatencySeconds: 7,
        maxLatencySeconds: 7,
        oracleMinPublishTime: '2026-11-27T06:22:04Z',
        oracleMaxPublishTime: '2026-11-27T06:22:05Z',
        oracleSpreadSeconds: 1,
      },
      finalizedOrders: [
        {
          ...BATCH_FINALIZATION.finalizedOrders![0],
          commitAt: '2026-11-27T06:22:03Z',
          finalizedAt: '2026-11-27T06:22:10Z',
          oracleMinPublishTime: '2026-11-27T06:22:04Z',
          oracleMaxPublishTime: '2026-11-27T06:22:05Z',
          oracleSpreadSeconds: 1,
          commitToOracleMinSeconds: 1,
          commitToOracleMaxSeconds: 2,
          oracleToFinalizeSeconds: 5,
          latencySeconds: 7,
          latencyBlocks: 28,
          latencyPercentile: 97,
        },
      ],
      events: [
        {
          ...BATCH_FINALIZATION.events[0],
          latencySeconds: 7,
        },
      ],
    },
  },
}

export const LpSettlementTransaction: Story = {
  args: { data: LP_SETTLEMENT_TX },
}
