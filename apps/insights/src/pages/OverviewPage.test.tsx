import { render, screen, within } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { OverviewPage } from './OverviewPage'
import { ARBITRUM_ONE_CHAIN_ID } from '../utils/chain'
import { account, input } from '../test/divisions'
import { publishWeek } from '../divisions/engine'

const statusQuery = vi.hoisted(() => vi.fn())
const overviewQuery = vi.hoisted(() => vi.fn())
const marketExposureQuery = vi.hoisted(() => vi.fn())
const liveQuery = vi.hoisted(() => vi.fn())
const publicationQuery = vi.hoisted(() => vi.fn())
const statsQuery = vi.hoisted(() => vi.fn())

vi.mock('../api', async () => {
  const actual = await vi.importActual<typeof import('../api')>('../api')
  return {
    ...actual,
    useInsightsStatus: statusQuery,
    useOverview: overviewQuery,
    useMarketExposure: marketExposureQuery,
  }
})
vi.mock('../divisions/query', () => ({
  useLiveDivisions: liveQuery,
  useDivisions: publicationQuery,
  useDivisionsStats: statsQuery,
}))

beforeEach(() => {
  statusQuery.mockReturnValue({
    data: {
      healthy: true,
      latestIndexedBlock: 325000000,
      latestIndexedAt: '2026-11-26T10:00:00Z',
      chainId: 421614,
    },
    isLoading: false,
    isError: false,
  })
  overviewQuery.mockReturnValue({
    data: undefined,
    isLoading: false,
    isError: false,
  })
  marketExposureQuery.mockReturnValue({
    data: undefined,
    isLoading: false,
    isError: false,
  })
  statsQuery.mockReturnValue({ data: { registeredWallets: 88 }, isLoading: false, isError: false })
  publicationQuery.mockReturnValue({ data: publishWeek(input(0, [account(1)]), null), isLoading: false, isError: false })
  liveQuery.mockReturnValue({
    data: {
      status: 'ready',
      scoringRelease: { router: `0x${'2'.repeat(40)}`, version: 'v2.0.0', assetSymbol: 'USDG' },
      weekStartsAt: '2026-11-22T21:00:00Z',
      snapshotAt: '2026-11-26T10:00:00Z',
      snapshotBlock: '325000042',
      walletCount: 2,
      standings: [
        { address: account(1).address, discordUsername: 'alice', rank: 1, weeklyReturnBps: 1450, activeDays: 3 },
        { address: account(2).address, discordUsername: null, rank: 2, weeklyReturnBps: -250, activeDays: 2 },
      ],
    },
    isLoading: false,
    isError: false,
  })
})

function show() {
  render(
    <MemoryRouter>
      <OverviewPage />
    </MemoryRouter>,
  )
}

describe('OverviewPage (Protocol Pulse)', () => {
  it('renders live telemetry, perspective hubs, and links Divisions leaders to Trading Account pages', () => {
    show()
    expect(screen.getByRole('heading', { name: 'Plether Insights' })).toBeInTheDocument()
    expect(screen.getByText('325,000,042')).toBeInTheDocument()
    expect(screen.getByText('v2.0.0')).toBeInTheDocument()
    expect(screen.getByText('88')).toBeInTheDocument()

    const table = screen.getByRole('table', { name: /Top live weekly Divisions traders/ })
    expect(within(table).getByText('@alice')).toBeInTheDocument()
    expect(within(table).getByText('+14.50%')).toBeInTheDocument()
    expect(within(table).getByRole('link', { name: '0x0000…0001' })).toHaveAttribute(
      'href',
      `/wallets/${account(1).address}`,
    )
  })

  it('hydrates cross-pillar metrics when /api/insights/v1/overview is available', () => {
    overviewQuery.mockReturnValue({
      data: {
        network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
        observedAt: '2026-11-26T22:00:00Z',
        headBlock: '332945120',
        execution: {
          orders24h: 2073,
          executed24h: 2068,
          fillRate24hBps: 9976,
          latencyP50Seconds: 2,
          latencyP95Seconds: 4,
        },
        liquidity: {
          latestEpoch: 148,
          readiness: 'healthy',
          confirmedSettlements24h: 24,
        },
        sponsored: {
          configured: true,
          stack: 'kernel',
          enabled: true,
          ops24h: 842,
          successRate24hBps: 9952,
        },
        protocol: {
          releaseVersion: 'v2.0.0',
          deploymentBlock: '320000000',
          pinnedContracts: 30,
        },
        infra: {
          overallStatus: 'healthy',
          dbReachable: true,
          dbPingLatencyMs: 4,
          rpcReachable: true,
          rpcLatencyMs: 11,
          healthyServices: 9,
          totalServices: 9,
          awsControlPlaneAvailable: true,
        },
      },
      isLoading: false,
      isError: false,
    })

    show()
    expect(screen.getByText('332,945,120')).toBeInTheDocument()
    expect(screen.getByText('99.76% fill (24h)')).toBeInTheDocument()
    expect(screen.getByText(/Epoch #148 · 24 confirmed \(24h\)/)).toBeInTheDocument()
    expect(screen.getByText(/9\/9 workers healthy · DB 4 ms/)).toBeInTheDocument()
    expect(screen.getByText(/842 UserOps \(24h\) · 99.52% success/)).toBeInTheDocument()
    expect(screen.getByText('30 contracts')).toBeInTheDocument()
  })

  it('adapts network labels and trading app URL automatically for Arbitrum One', () => {
    statusQuery.mockReturnValue({
      data: {
        healthy: true,
        latestIndexedBlock: 400000000,
        latestIndexedAt: '2026-11-26T10:00:00Z',
        chainId: ARBITRUM_ONE_CHAIN_ID,
      },
      isLoading: false,
      isError: false,
    })
    show()
    expect(screen.getByText('Arbitrum One')).toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'Open trading app ↗' })).toHaveAttribute(
      'href',
      'https://app.plether.com',
    )
  })

  it('renders global market exposure skew and top active positions with lifecycle links', () => {
    marketExposureQuery.mockReturnValue({
      data: {
        network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
        observedAt: '2026-11-26T22:00:00Z',
        currentMarkPrice: '95500000',
        markObservedAt: '2026-11-26T21:59:55Z',
        summary: {
          longOpenInterestAssets: '75000000000',
          shortOpenInterestAssets: '25000000000',
          totalOpenInterestAssets: '100000000000',
          netSkewAssets: '50000000000',
          longShareBps: 7500,
          shortShareBps: 2500,
          openPositionCount: 3,
          longCount: 2,
          shortCount: 1,
          avgLeverageX100: 750,
        },
        positions: [
          {
            account: account(1).address,
            discordUsername: 'alice',
            positionId: 'pos-0xabc:0',
            market: 'plDXY-PERP',
            side: 'long',
            size: '50000000000000000000000',
            notionalAssets: '50000000000',
            marginAssets: '6000000000',
            entryPrice: '96200000',
            markPrice: '95500000',
            liquidationPrice: '105000000',
            distanceToLiquidationBps: 900,
            effectiveLeverageX100: 833,
            unrealizedPnlAssets: '950000000',
            openedAt: '2026-11-26T18:00:00Z',
            durationSeconds: 14400,
          },
        ],
      },
      isLoading: false,
      isError: false,
    })

    show()
    expect(screen.getByText('Live market exposure & top active positions')).toBeInTheDocument()
    expect(screen.getByText(/Long 75\.0%/)).toBeInTheDocument()
    expect(screen.getByText(/Short 25\.0%/)).toBeInTheDocument()
    const posTable = screen.getByRole('table', {
      name: /Largest active plDXY-PERP open positions by notional exposure/i,
    })
    expect(within(posTable).getByText('@alice')).toBeInTheDocument()
    expect(within(posTable).getByRole('link', { name: 'Lifecycle →' })).toHaveAttribute(
      'href',
      `/wallets/${account(1).address}/positions/pos-0xabc%3A0`,
    )
    expect(within(posTable).getByRole('link', { name: '@alice' })).toHaveAttribute(
      'href',
      `/wallets/${account(1).address}?tab=exposure`,
    )
  })

  it('shows explicit unavailable states instead of fake zeros when telemetry fails', () => {
    statusQuery.mockReturnValue({ data: undefined, isLoading: false, isError: true })
    liveQuery.mockReturnValue({ data: undefined, isLoading: false, isError: true })
    statsQuery.mockReturnValue({ data: undefined, isLoading: false, isError: true })
    publicationQuery.mockReturnValue({ data: undefined, isLoading: false, isError: true })
    show()
    expect(screen.getByText('Telemetry unavailable')).toBeInTheDocument()
    expect(screen.getAllByText('Unavailable').length).toBeGreaterThanOrEqual(4)
  })
})
