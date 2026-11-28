import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import {
  InsightsApiError,
  classifyLiquidationBuffer,
  describeDirectionalBias,
  formatDurationCompact,
  formatLeverageX100,
  rawToDisplayPrice,
  type PositionDetailResponse,
  type WalletExposureResponse,
} from '../api'
import { WalletExposureSection } from '../components/WalletExposureSection'
import { resolvePerpsChain } from '../utils/chain'
import { PositionLifecyclePage } from './PositionLifecyclePage'

const protocolContext = vi.hoisted(() => vi.fn())
const positionDetailQuery = vi.hoisted(() => vi.fn())

vi.mock('../hooks/useProtocolContext', () => ({ useProtocolContext: protocolContext }))
vi.mock('../api', async (importOriginal) => ({
  ...(await importOriginal<typeof import('../api')>()),
  useWalletPositionDetail: positionDetailQuery,
}))

const testAccount = `0x${'b'.repeat(40)}`

const sampleExposure: WalletExposureResponse = {
  network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
  account: testAccount,
  observedAt: '2026-11-27T10:00:00Z',
  currentMarkPrice: '94740000',
  markObservedAt: '2026-11-27T09:59:45Z',
  maintenanceMarginBps: 10,
  activeExposure: {
    positionId: 'pos-332900100-4',
    market: 'plDXY-PERP',
    side: 'long',
    size: '25000000000000000000000',
    notionalAssets: '23685000000',
    marginAssets: '1650000000',
    entryPrice: '95100000',
    markPrice: '94740000',
    liquidationPrice: '101400000',
    distanceToLiquidationBps: 632,
    closestLiquidationBps: 410,
    effectiveLeverageX100: 1435,
    unrealizedPnlAssets: '90000000',
    marginUtilizationBps: 4200,
    liquidatable: false,
    openedAt: '2026-11-27T06:00:00Z',
    durationSeconds: 14400,
    mfePnlAssets: '145000000',
    maePnlAssets: '-55000000',
  },
  summary: {
    totalPositions: 2,
    openPositions: 1,
    closedPositions: 1,
    liquidatedPositions: 0,
    winCount: 1,
    lossCount: 0,
    winRateBps: 10000,
    longPositions: 1,
    shortPositions: 1,
    longNotionalAssets: '24000000000',
    shortNotionalAssets: '12000000000',
    longNotionalShareBps: 6667,
    shortNotionalShareBps: 3333,
    longRealizedPnlAssets: '0',
    shortRealizedPnlAssets: '320000000',
    longHoldSeconds: 14400,
    shortHoldSeconds: 21600,
    avgEntryLeverageX100: 1450,
    maxLeverageX100: 1850,
    currentLeverageX100: 1435,
    avgHoldSeconds: 18000,
    medianHoldSeconds: 21600,
    avgNotionalAssets: '18000000000',
    peakNotionalAssets: '24000000000',
    avgMfeAssets: '280000000',
    avgMaeAssets: '-80000000',
    tightestLiquidationBps: 185,
  },
  exposureTimeline: [
    {
      timestamp: 1795650000,
      occurredAt: '2026-11-26T08:00:00Z',
      blockNumber: '332800000',
      signedNotionalAssets: '-12000000000',
      absNotionalAssets: '12000000000',
      marginAssets: '800000000',
      leverageX100: 1500,
      side: 'short',
      eventType: 'Open',
      positionId: 'pos-332800000-2',
    },
    {
      timestamp: 1795671600,
      occurredAt: '2026-11-26T14:00:00Z',
      blockNumber: '332850000',
      signedNotionalAssets: '0',
      absNotionalAssets: '0',
      marginAssets: '0',
      leverageX100: 0,
      side: 'flat',
      eventType: 'Close',
      positionId: 'pos-332800000-2',
    },
    {
      timestamp: 1795759200,
      occurredAt: '2026-11-27T06:00:00Z',
      blockNumber: '332900100',
      signedNotionalAssets: '23685000000',
      absNotionalAssets: '23685000000',
      marginAssets: '1650000000',
      leverageX100: 1435,
      side: 'long',
      eventType: 'Open',
      positionId: 'pos-332900100-4',
    },
  ],
  positions: [
    {
      positionId: 'pos-332900100-4',
      market: 'plDXY-PERP',
      status: 'open',
      side: 'long',
      openedAt: '2026-11-27T06:00:00Z',
      closedAt: null,
      durationSeconds: 14400,
      entryPrice: '95100000',
      exitPrice: null,
      currentSize: '25000000000000000000000',
      peakSize: '25000000000000000000000',
      peakNotionalAssets: '23775000000',
      peakMarginAssets: '1650000000',
      entryLeverageX100: 1440,
      maxLeverageX100: 1480,
      currentLeverageX100: 1435,
      realizedPnlAssets: '0',
      unrealizedPnlAssets: '90000000',
      netPnlAssets: '90000000',
      roiBps: 545,
      totalFeesAssets: '9510000',
      totalVpiAssets: '1200000',
      totalKeeperBountyAssets: '0',
      mfePnlAssets: '145000000',
      mfeBps: 55,
      mfePrice: '94520000',
      mfeOccurredAt: '2026-11-27T08:15:00Z',
      maePnlAssets: '-55000000',
      maeBps: -21,
      maePrice: '95320000',
      maeOccurredAt: '2026-11-27T06:45:00Z',
      liquidationPrice: '101400000',
      closestLiquidationBps: 410,
      closestLiquidationAt: '2026-11-27T06:45:00Z',
      currentLiquidationBps: 632,
      liquidatable: false,
      fillCount: 1,
      scaleInCount: 0,
      partialCloseCount: 0,
      marginAddCount: 0,
      openTxHash: `0x${'1'.repeat(64)}`,
      closeTxHash: null,
      openOrderId: '402',
    },
    {
      positionId: 'pos-332800000-2',
      market: 'plDXY-PERP',
      status: 'closed',
      side: 'short',
      openedAt: '2026-11-26T08:00:00Z',
      closedAt: '2026-11-26T14:00:00Z',
      durationSeconds: 21600,
      entryPrice: '94200000',
      exitPrice: '96700000',
      currentSize: '0',
      peakSize: '12800000000000000000000',
      peakNotionalAssets: '12057600000',
      peakMarginAssets: '800000000',
      entryLeverageX100: 1507,
      maxLeverageX100: 1850,
      currentLeverageX100: 0,
      realizedPnlAssets: '320000000',
      unrealizedPnlAssets: '0',
      netPnlAssets: '320000000',
      roiBps: 4000,
      totalFeesAssets: '9600000',
      totalVpiAssets: '-800000',
      totalKeeperBountyAssets: '0',
      mfePnlAssets: '415000000',
      mfeBps: 306,
      mfePrice: '97440000',
      mfeOccurredAt: '2026-11-26T12:30:00Z',
      maePnlAssets: '-105000000',
      maeBps: -77,
      maePrice: '93380000',
      maeOccurredAt: '2026-11-26T09:15:00Z',
      liquidationPrice: '88400000',
      closestLiquidationBps: 185,
      closestLiquidationAt: '2026-11-26T09:15:00Z',
      currentLiquidationBps: null,
      liquidatable: false,
      fillCount: 2,
      scaleInCount: 0,
      partialCloseCount: 0,
      marginAddCount: 0,
      openTxHash: `0x${'2'.repeat(64)}`,
      closeTxHash: `0x${'3'.repeat(64)}`,
      openOrderId: '389',
    },
  ],
}

const sampleDetail: PositionDetailResponse = {
  network: sampleExposure.network,
  account: testAccount,
  observedAt: '2026-11-27T10:00:00Z',
  identity: { discordUsername: 'macro_fox' },
  currentMarkPrice: '94740000',
  markObservedAt: '2026-11-27T09:59:45Z',
  maintenanceMarginBps: 10,
  candleIntervalSeconds: 300,
  position: {
    ...sampleExposure.positions[1],
    captureRatioBps: 7710,
    remainingMarginAssets: '0',
    entryCostAssets: '12057600000',
  },
  priceSeries: [
    {
      timestamp: 1795650000,
      occurredAt: '2026-11-26T08:00:00Z',
      rawPrice: '94200000',
      rawHigh: '94300000',
      rawLow: '94100000',
      unrealizedPnlAssets: '0',
      positionSize: '12800000000000000000000',
      leverageX100: 1507,
      liquidationPrice: '88400000',
      distanceToLiquidationBps: 548,
    },
    {
      timestamp: 1795654500,
      occurredAt: '2026-11-26T09:15:00Z',
      rawPrice: '93380000',
      rawHigh: '93800000',
      rawLow: '93380000',
      unrealizedPnlAssets: '-105000000',
      positionSize: '12800000000000000000000',
      leverageX100: 1850,
      liquidationPrice: '88400000',
      distanceToLiquidationBps: 185,
    },
    {
      timestamp: 1795666200,
      occurredAt: '2026-11-26T12:30:00Z',
      rawPrice: '97440000',
      rawHigh: '97440000',
      rawLow: '96900000',
      unrealizedPnlAssets: '415000000',
      positionSize: '12800000000000000000000',
      leverageX100: 1490,
      liquidationPrice: '88400000',
      distanceToLiquidationBps: 880,
    },
    {
      timestamp: 1795671600,
      occurredAt: '2026-11-26T14:00:00Z',
      rawPrice: '96700000',
      rawHigh: '96800000',
      rawLow: '96500000',
      unrealizedPnlAssets: '320000000',
      positionSize: '0',
      leverageX100: 0,
      liquidationPrice: '88400000',
      distanceToLiquidationBps: 800,
    },
  ],
  events: [
    {
      id: `0x${'2'.repeat(64)}:2`,
      action: 'Open',
      activityType: 'Open',
      orderId: '389',
      txHash: `0x${'2'.repeat(64)}`,
      blockNumber: '332800000',
      timestamp: 1795650000,
      occurredAt: '2026-11-26T08:00:00Z',
      price: '94200000',
      sizeDelta: '12800000000000000000000',
      resultingSize: '12800000000000000000000',
      resultingEntryPrice: '94200000',
      marginDeltaAssets: '800000000',
      resultingMarginAssets: '800000000',
      resultingNotionalAssets: '12057600000',
      resultingLeverageX100: 1507,
      resultingLiquidationPrice: '88400000',
      pnlAssets: null,
      feeAssets: '4820000',
      vpiAssets: '-800000',
      keeperBountyAssets: null,
      orderProtections: null,
    },
    {
      id: `0x${'3'.repeat(64)}:4`,
      action: 'Close',
      activityType: 'Close',
      orderId: '395',
      txHash: `0x${'3'.repeat(64)}`,
      blockNumber: '332850000',
      timestamp: 1795671600,
      occurredAt: '2026-11-26T14:00:00Z',
      price: '96700000',
      sizeDelta: '-12800000000000000000000',
      resultingSize: '0',
      resultingEntryPrice: '94200000',
      marginDeltaAssets: '-800000000',
      resultingMarginAssets: '0',
      resultingNotionalAssets: '0',
      resultingLeverageX100: 0,
      resultingLiquidationPrice: null,
      pnlAssets: '320000000',
      feeAssets: '4780000',
      vpiAssets: '0',
      keeperBountyAssets: null,
      orderProtections: null,
    },
  ],
  protections: [
    {
      protectionId: '77',
      blockNumber: '332800040',
      logIndex: 5,
      txHash: `0x${'4'.repeat(64)}`,
      eventName: 'ProtectionCreated',
      eventData: {},
      snapshot: { kind: 'take_profit', triggerPrice: '97500000' },
    },
  ],
}

beforeEach(() => {
  protocolContext.mockReturnValue({
    chainId: 421614,
    chain: resolvePerpsChain(421614),
    releaseVersion: 'v2.0.0',
    blockNumber: '332900500',
    observedAt: '2026-11-27T10:00:00Z',
  })
  positionDetailQuery.mockReturnValue({
    data: sampleDetail,
    isLoading: false,
    isError: false,
    error: null,
    refetch: vi.fn(),
  })
})

describe('position exposure helpers', () => {
  it('formats leverage, durations, display prices, liquidation buffers, and bias', () => {
    expect(formatLeverageX100(1436)).toBe('14.4×')
    expect(formatLeverageX100(525)).toBe('5.25×')
    expect(formatLeverageX100(0)).toBe('—')
    expect(formatDurationCompact(45)).toBe('45s')
    expect(formatDurationCompact(14400)).toBe('4h')
    expect(formatDurationCompact(93600)).toBe('1d 2h')
    expect(rawToDisplayPrice('94740000')).toBeCloseTo(1.0526, 4)
    expect(classifyLiquidationBuffer(40)).toMatchObject({ tone: 'negative', label: 'Critical' })
    expect(classifyLiquidationBuffer(185)).toMatchObject({ tone: 'warning', label: 'Tight' })
    expect(classifyLiquidationBuffer(632)).toMatchObject({ tone: 'positive', label: 'Safe' })
    expect(describeDirectionalBias(6667)).toMatchObject({ label: 'Long biased', tone: 'positive' })
    expect(describeDirectionalBias(3000)).toMatchObject({ label: 'Short biased', tone: 'negative' })
    expect(describeDirectionalBias(5000)).toMatchObject({ label: 'Balanced', tone: 'neutral' })
  })
})

describe('WalletExposureSection', () => {
  it('renders directional bias, leverage profile, excursion metrics, chart toggle, and position drill-down links', () => {
    const { container } = render(
      <MemoryRouter>
        <WalletExposureSection
          address={testAccount}
          data={sampleExposure}
          isLoading={false}
          isError={false}
          awaitingDeploy={false}
          releaseVersion="v2.0.0"
          onRetry={vi.fn()}
        />
      </MemoryRouter>,
    )

    expect(screen.getByText('67% Long')).toBeInTheDocument()
    expect(screen.getByText('14.5× avg')).toBeInTheDocument()
    expect(screen.getAllByText('1.85%').length).toBeGreaterThanOrEqual(2)
    expect(screen.getByText('Long vs Short Directional Split')).toBeInTheDocument()
    expect(container.textContent).not.toContain('$')

    // Switch chart from Net Exposure to Leverage
    fireEvent.click(screen.getByRole('button', { name: 'Leverage (×)' }))
    expect(screen.getByRole('img', { name: /Wallet effective leverage over time/i })).toBeInTheDocument()

    // Filter positions table to Short only
    const table = screen.getByRole('table', { name: /Reconstructed Trading Account position lifecycles/i })
    expect(within(table).getAllByRole('link', { name: /Lifecycle →/i })).toHaveLength(2)
    fireEvent.click(screen.getByRole('button', { name: 'Short' }))
    expect(within(table).getAllByRole('link', { name: /Lifecycle →/i })).toHaveLength(1)
  })
})

describe('PositionLifecyclePage', () => {
  function show(path = `/wallets/${testAccount}/positions/pos-332800000-2`) {
    return render(
      <MemoryRouter initialEntries={[path]}>
        <Routes>
          <Route path="/wallets/:address/positions/:positionId" element={<PositionLifecyclePage />} />
        </Routes>
      </MemoryRouter>,
    )
  }

  it('renders full open-to-close trajectory, MFE/MAE anatomy, protections, and timeline without $ prefixes', () => {
    const { container } = show()

    expect(screen.getByText(/plDXY-PERP/)).toBeInTheDocument()
    expect(screen.getByText(/Captured 77\.10% of peak MFE/)).toBeInTheDocument()
    expect(screen.getByText(/#77 · ProtectionCreated/)).toBeInTheDocument()
    const breadcrumb = screen.getByRole('navigation', { name: 'Breadcrumb' })
    expect(within(breadcrumb).getByRole('link', { name: '@macro_fox' })).toHaveAttribute(
      'href',
      `/wallets/${testAccount}`,
    )
    expect(container.textContent).not.toContain('$')

    // Verify split green/orange gradient in Price & Levels mode (Short: orange above entry, green below)
    const priceSvg = screen.getByRole('img', { name: /Position price trajectory chart/i })
    const priceStops = Array.from(priceSvg.querySelectorAll('linearGradient stop')).map((s) =>
      s.getAttribute('stop-color'),
    )
    expect(priceStops).toContain('#ff572d')
    expect(priceStops).toContain('#34d399')

    // Toggle chart modes cleanly
    fireEvent.click(screen.getByRole('button', { name: 'Unrealized PnL' }))
    const pnlSvg = screen.getByRole('img', { name: /Position pnl trajectory chart/i })
    expect(pnlSvg).toBeInTheDocument()
    const pnlStops = Array.from(pnlSvg.querySelectorAll('linearGradient stop')).map((s) =>
      s.getAttribute('stop-color'),
    )
    expect(pnlStops[0]).toBe('#34d399')
    expect(pnlStops[pnlStops.length - 1]).toBe('#ff572d')

    fireEvent.click(screen.getByRole('button', { name: 'Leverage (×)' }))
    expect(screen.getByRole('img', { name: /Position leverage trajectory chart/i })).toBeInTheDocument()

    const timeline = screen.getByRole('table', { name: /Chronological position lifecycle events/i })
    expect(within(timeline).getByText('Open')).toBeInTheDocument()
    expect(within(timeline).getByText('Close')).toBeInTheDocument()
  })

  it('fails closed cleanly when position lifecycle returns 404', () => {
    positionDetailQuery.mockReturnValue({
      data: undefined,
      isLoading: false,
      isError: true,
      error: new InsightsApiError('Position not found', 404),
      refetch: vi.fn(),
    })

    show()
    expect(screen.getByText('Position lifecycle not found')).toBeInTheDocument()
  })
})
