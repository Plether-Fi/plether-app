import { render, screen, within } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { InsightsApiError } from '../api/client'
import { resolvePerpsChain } from '../utils/chain'
import { LiquidityOpsPage } from './LiquidityOpsPage'

const protocolContext = vi.hoisted(() => vi.fn())
const settlementsQuery = vi.hoisted(() => vi.fn())
const forensicsQuery = vi.hoisted(() => vi.fn())

vi.mock('../hooks/useProtocolContext', () => ({ useProtocolContext: protocolContext }))
vi.mock('../api', async (importOriginal) => ({
  ...(await importOriginal<typeof import('../api')>()),
  useLiquiditySettlements: settlementsQuery,
  useLiquidityForensics: forensicsQuery,
}))

beforeEach(() => {
  protocolContext.mockReturnValue({
    chainId: 421614,
    chain: resolvePerpsChain(421614),
    releaseVersion: 'v2.0.0',
    blockNumber: '332879274',
    observedAt: '2026-11-26T11:37:00Z',
  })
  settlementsQuery.mockReturnValue({
    data: {
      network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
      monitorAddress: '0x87d2eae2783fea3055ce142d060e8db14d8e04d8',
      latestObservation: {
        epoch: 496900,
        observedBlock: '325000100',
        observedAt: '2026-11-26T12:00:00Z',
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
        confirmedSettlements24h: 23,
        failedOrReverted24h: 0,
        replacedAttempts24h: 1,
        deferredEntryEpochs24h: 0,
        backlogEpochs24h: 0,
      },
      recentSettlements: [
        {
          epoch: 496900,
          cutoffEpoch: '496899',
          status: 'confirmed_success',
          executionPath: 'cached_mark',
          txHash: `0x${'b'.repeat(64)}`,
          receiptBlock: '325000105',
          createdAt: '2026-11-26T12:00:00Z',
          confirmedAt: '2026-11-26T12:00:04Z',
          confirmationLatencySeconds: 4,
          replacementCount: 0,
          seniorDepositAssets: '5000000000',
          seniorRedeemAssets: '0',
          juniorDepositAssets: '1000000000',
          juniorRedeemAssets: '250000000',
          seniorBacklog: false,
          juniorBacklog: false,
          entriesDeferred: false,
        },
      ],
    },
    isLoading: false,
    isError: false,
  })
  forensicsQuery.mockReturnValue({
    data: {
      network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
      observedAt: '2026-11-26T12:00:00Z',
      snapshotFreshness: {
        window7d: {
          expectedHours: 169,
          observedHours: 169,
          freshHours: 168,
          staleHours: 1,
          latestEpochTimestamp: 1795694400,
          latestBlockNumber: '325000100',
          latestMarkFresh: true,
        },
        window30d: {
          expectedHours: 721,
          observedHours: 700,
          freshHours: 695,
          staleHours: 5,
          latestEpochTimestamp: 1795694400,
          latestBlockNumber: '325000100',
          latestMarkFresh: true,
        },
      },
      attributionWorker: {
        indexedThroughBlock: '325000090',
        recentReports: [
          {
            startBlockNumber: '324000000',
            endBlockNumber: '325000000',
            sourceCommit: '1209ca8',
            schemaVersion: 2,
            updatedAt: '2026-11-26T12:00:00Z',
            sourceBlockCount: 674,
            reportedReturnPp: 0.2997,
            unexplainedPp: 0,
            netReturnUsdg: '18395305533',
            unexplainedUsdg: '0',
            liquidationFeesUsdg: '420000000',
            notesCount: 0,
          },
        ],
      },
      vaultIndexers: {
        activity: {
          lastIndexedBlock: '325000080',
          safeHeadBlock: '325000100',
          lagBlocks: 20,
          lagSeconds: 5,
          pollAgeSeconds: 3,
          backfillComplete: true,
        },
        depositAttribution: {
          confirmedThroughBlock: '325000080',
          pollAgeSeconds: 4,
          backfillComplete: true,
        },
      },
    },
    isLoading: false,
    isError: false,
  })
})

function show() {
  render(
    <MemoryRouter>
      <LiquidityOpsPage />
    </MemoryRouter>,
  )
}

describe('LiquidityOpsPage (Stage 1 LP Ops)', () => {
  it('renders live epoch settlements, clear masks, residual audit, and indexer lag', () => {
    show()
    expect(screen.getByText('#496,900')).toBeInTheDocument()
    expect(screen.getByText('23')).toBeInTheDocument()
    expect(screen.getByText('168/169 hrs')).toBeInTheDocument()
    expect(screen.getAllByText('Clear')).toHaveLength(4)

    const settlementsTable = screen.getByRole('table', { name: /Recent hourly LP epoch settlement/ })
    expect(within(settlementsTable).getByText('#496900')).toBeInTheDocument()
    expect(within(settlementsTable).getByText('4s')).toBeInTheDocument()
    expect(within(settlementsTable).getByText('Clean')).toBeInTheDocument()

    const auditTable = screen.getByRole('table', { name: /Junior vault return attribution residual/ })
    expect(within(auditTable).getByText('Exact (0.00)')).toBeInTheDocument()
    expect(within(auditTable).getByText('+0.2997%')).toBeInTheDocument()
  })

  it('highlights active blocker/fault bitmasks and unexplained residuals', () => {
    settlementsQuery.mockReturnValue({
      data: {
        network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
        monitorAddress: '0x87d2eae2783fea3055ce142d060e8db14d8e04d8',
        latestObservation: {
          epoch: 496901,
          observedBlock: '325000200',
          observedAt: '2026-11-26T13:00:00Z',
          readiness: 'blocked',
          executionPath: 'atomic_oracle_refresh',
          healthState: 'degraded',
          observationComplete: true,
          hasMaturedWork: true,
          paused: false,
          masks: {
            operationalBlocker: { raw: '5', activeBits: [0, 2] },
            warning: { raw: '2', activeBits: [1] },
            dependencyFailure: { raw: '0', activeBits: [] },
            criticalFault: { raw: '0', activeBits: [] },
          },
        },
        summary: {
          observedEpochs24h: 24,
          confirmedSettlements24h: 20,
          failedOrReverted24h: 2,
          replacedAttempts24h: 1,
          deferredEntryEpochs24h: 1,
          backlogEpochs24h: 1,
        },
        recentSettlements: [],
      },
      isLoading: false,
      isError: false,
    })
    show()
    expect(screen.getAllByText('Settlement blocked').length).toBeGreaterThanOrEqual(1)
    expect(screen.getByText('Bits: #0, #2')).toBeInTheDocument()
    expect(screen.getByText('Bits: #1')).toBeInTheDocument()
  })

  it('fails closed cleanly when the backend does not serve Stage 1 endpoints yet (404)', () => {
    const notFound = new InsightsApiError('Not found', 404)
    settlementsQuery.mockReturnValue({ data: undefined, isLoading: false, isError: true, error: notFound })
    forensicsQuery.mockReturnValue({ data: undefined, isLoading: false, isError: true, error: notFound })
    show()
    expect(screen.getByText(/Connected backend does not serve Stage 1 LP endpoints yet/i)).toBeInTheDocument()
    expect(screen.getAllByText('Unavailable').length).toBeGreaterThanOrEqual(4)
  })
})
