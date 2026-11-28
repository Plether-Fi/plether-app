import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { InsightsApiError, type ProtocolManifestResponse } from '../api'
import { resolvePerpsChain } from '../utils/chain'
import { ProtocolPage } from './ProtocolPage'

const protocolContext = vi.hoisted(() => vi.fn())
const manifestQuery = vi.hoisted(() => vi.fn())

vi.mock('../hooks/useProtocolContext', () => ({ useProtocolContext: protocolContext }))
vi.mock('../api', async (importOriginal) => ({
  ...(await importOriginal<typeof import('../api')>()),
  useProtocolManifest: manifestQuery,
}))

const sampleManifest: ProtocolManifestResponse = {
  network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
  observedAt: '2026-11-26T18:00:00Z',
  rpcAvailable: true,
  verifiedAtBlock: '332900000',
  release: {
    version: 'v2.0.0',
    date: '2026-10-28',
    phase: 'active',
    deploymentBlock: '323658447',
    sourceCommit: '1209ca8a5b94a4fb6323c7c4585846ed93db3932',
    forgeVersion: '1.5.1-stable',
    manifestSha256: '584cdacada35f8225d2a88811edfbd9622764fe0e6da4b275ba8f33d6f8c0c74',
    bundleSha256: '3ea6e372776abb4fe6b7f3c6b2914b381f4f3da3e71b28872a293d6d32cc8ba7',
    pythAddress: '0x0b73614636c855bf23f342f307fb981a3e47f42b',
    schemaVersion: 4,
    orderInterfaceVersion: 3,
    aaManifestVersion: 'v2',
  },
  settlementToken: {
    name: 'Mock USDG',
    symbol: 'USDG',
    decimals: 6,
    isMock: true,
    address: '0x3d051dbbf5ff427a4ae8ad09746902666af8fb9d',
  },
  safety: {
    available: true,
    phase: 'active',
    lastMarkPrice: '94743619',
    lastMarkTime: '2026-11-26T17:59:30Z',
    tradingActive: true,
    withdrawalLive: true,
    lpEpochSettlementPaused: false,
    oracleFrozen: false,
    fadWindow: false,
    housePoolPaused: false,
    degradedMode: false,
  },
  tvl: {
    available: true,
    isComplete: true,
    verifiedAtBlock: '332900000',
    settlementAssetSymbol: 'USDG',
    totalTvlAssets: '1000000000000',
    protocolContractsTvlAssets: '855000000000',
    lpVaultsTvlAssets: '650000000000',
    housePoolAssets: '500000000000',
    seniorVaultAssets: '100000000000',
    juniorVaultAssets: '50000000000',
    marginClearinghouseAssets: '200000000000',
    otherContractsAssets: '5000000000',
    kernelTradingAccountsAssets: '145000000000',
    totalTraderMarginAssets: '345000000000',
    contractsTracked: 3,
    contractsWithBalance: 2,
    contractsFailedReads: 0,
    kernelAccountsAvailable: true,
    kernelAccountsTracked: 12,
    kernelAccountsFunded: 5,
    kernelAccountsFailedReads: 0,
    buckets: [
      {
        key: 'lp_vaults',
        label: 'LP Liquidity Pools',
        detail: 'Settled HousePool liquidity + Senior/Junior vault escrow',
        assets: '650000000000',
        shareBps: 6500,
        addressCount: 3,
      },
      {
        key: 'margin_clearinghouse',
        label: 'Margin Clearinghouse',
        detail: 'Deposited trader collateral, locked position margin & claims',
        assets: '200000000000',
        shareBps: 2000,
        addressCount: 1,
      },
      {
        key: 'kernel_v4_accounts',
        label: 'Kernel v4 Trading Accounts',
        detail: 'User margin held directly in ZeroDev Kernel v4 smart wallets',
        assets: '145000000000',
        shareBps: 1450,
        addressCount: 5,
      },
      {
        key: 'other_contracts',
        label: 'Other Protocol Contracts',
        detail: 'Remaining pinned protocol release contracts',
        assets: '5000000000',
        shareBps: 50,
        addressCount: 1,
      },
    ],
    contractHoldings: [
      {
        key: 'housePool',
        name: 'HousePool',
        category: 'liquidity',
        address: '0xeb5604c393843b2f3c347ec24fa9ddad6ab2131e',
        assets: '500000000000',
        shareBps: 5000,
      },
    ],
    topKernelAccounts: [
      {
        address: '0x1111111111111111111111111111111111111111',
        assets: '95000000000',
        shareBps: 950,
      },
    ],
  },
  verification: {
    totalContracts: 3,
    verifiedCount: 3,
    mismatchCount: 0,
    uncheckedCount: 0,
    matchedParameters: 2,
    driftedParameters: 1,
    unavailableParameters: 0,
  },
  parameters: [
    {
      key: 'initialMarginBps',
      category: 'trader',
      label: 'Initial margin',
      unit: 'bps',
      contract: 'CfdEngine',
      description: 'Minimum collateral ratio required to open or increase a perpetual position.',
      manifestDefault: '20',
      liveValue: '20',
      status: 'match',
    },
    {
      key: 'executionFeeBps',
      category: 'trader',
      label: 'Protocol execution fee',
      unit: 'bps',
      contract: 'CfdEngine',
      description: 'Fee assessed on executed trade notional for opens, increases, and closes.',
      manifestDefault: '4',
      liveValue: '6',
      status: 'drifted',
    },
    {
      key: 'maxSeniorExposureAssets',
      category: 'lp',
      label: 'Max Senior tranche cap',
      unit: 'assets',
      contract: 'HousePool',
      description: 'Hard absolute ceiling on Senior vault USDG deposits accepted by HousePool.',
      manifestDefault: '40000000000000',
      liveValue: '40000000000000',
      status: 'match',
    },
    {
      key: 'lpEpochDurationSeconds',
      category: 'lp',
      label: 'LP settlement epoch duration',
      unit: 'seconds',
      contract: 'HousePool',
      description: 'Cadence at which queued LP requests mature.',
      manifestDefault: null,
      liveValue: '3600',
      status: 'live_only',
    },
  ],
  contracts: [
    {
      key: 'cfdEngine',
      name: 'CfdEngine',
      category: 'core',
      role: 'Core perpetuals accounting, margin, VPI, carry, and PnL settlement engine.',
      address: '0xca18a6783d7542f9d2f28f4f259772b74c4d6baa',
      expectedCodeHash: '0xa0f8e5be6e622a5a83e46e88f6b012757ef924cf613a50cba52a2b5c66b76a6b',
      actualCodeHash: '0xa0f8e5be6e622a5a83e46e88f6b012757ef924cf613a50cba52a2b5c66b76a6b',
      verified: true,
    },
    {
      key: 'housePool',
      name: 'HousePool',
      category: 'liquidity',
      role: 'Counterparty liquidity pool managing Senior/Junior tranche accounting and hourly LP epochs.',
      address: '0xeb5604c393843b2f3c347ec24fa9ddad6ab2131e',
      expectedCodeHash: '0x5e684cc82bfc5b697a24c80ef26910cfbe25efbe6fb2ca184c421808b5ced957',
      actualCodeHash: '0x5e684cc82bfc5b697a24c80ef26910cfbe25efbe6fb2ca184c421808b5ced957',
      verified: true,
      usdgBalanceAssets: '500000000000',
    },
    {
      key: 'emergencyPauseCoordinator',
      name: 'EmergencyPauseCoordinator',
      category: 'safety',
      role: 'Coordinates emergency pause actions across trading and LP settlement surfaces.',
      address: '0x736ead1f7a23f980e9fb51f023822c53d739ca2c',
      expectedCodeHash: '0x5dfb50481c44f5bb0e47c51ccf0d6d9b7b7eb4c93cc5e13ee23062aebe5594f6',
      actualCodeHash: '0x5dfb50481c44f5bb0e47c51ccf0d6d9b7b7eb4c93cc5e13ee23062aebe5594f6',
      verified: true,
    },
  ],
  releaseHistory: [
    {
      version: 'v1.2.3',
      status: 'superseded',
      date: '2026-09-01',
      summary: 'Initial Sepolia Core perpetuals release.',
    },
    {
      version: 'v2.0.0',
      status: 'active',
      date: '2026-10-28',
      deploymentBlock: '323658447',
      summary: 'Schema-4 USDG migration and bounded V3 orders.',
    },
  ],
}

beforeEach(() => {
  protocolContext.mockReturnValue({
    chainId: 421614,
    chain: resolvePerpsChain(421614),
    releaseVersion: 'v2.0.0',
    blockNumber: '332900000',
    observedAt: '2026-11-26T18:00:00Z',
  })
  manifestQuery.mockReturnValue({
    data: sampleManifest,
    isLoading: false,
    isError: false,
    refetch: vi.fn(),
  })
})

function show() {
  render(
    <MemoryRouter>
      <ProtocolPage />
    </MemoryRouter>,
  )
}

describe('ProtocolPage (Stage 4 Contracts, Parameters & Safety Controls)', () => {
  it('renders verified contract count, live safety controls, parameter drift, and provenance', () => {
    show()

    expect(screen.getByText('Parameter drift detected')).toBeInTheDocument()
    expect(screen.getByText('3 / 3 bytecode verified')).toBeInTheDocument()
    expect(screen.getByText('1 drifted')).toBeInTheDocument()
    expect(screen.getByText(/Last on-chain mark: 1\.05256/)).toBeInTheDocument()

    const paramsTable = screen.getByRole('table', {
      name: /Live on-chain economic parameters/i,
    })
    expect(within(paramsTable).getByText('Initial margin')).toBeInTheDocument()
    expect(within(paramsTable).getByText('Up to 500× nominal leverage')).toBeInTheDocument()
    expect(within(paramsTable).getByText('0.06% (6 bps)')).toBeInTheDocument()
    expect(within(paramsTable).getByText('Drifted')).toBeInTheDocument()
    expect(within(paramsTable).getAllByText('40.00M USDG').length).toBeGreaterThanOrEqual(1)
    expect(within(paramsTable).getByText('1h')).toBeInTheDocument()
    expect(paramsTable.textContent).not.toContain('$')

    // Filter to drifted parameters only
    fireEvent.click(screen.getByRole('button', { name: /Filter drifted \(1\)/i }))
    expect(within(paramsTable).getByText('Protocol execution fee')).toBeInTheDocument()
    expect(within(paramsTable).queryByText('Initial margin')).not.toBeInTheDocument()

    // Search verified contracts & verify pinned GitHub links
    const contractsTable = screen.getByRole('table', {
      name: /Verified on-chain protocol contracts/i,
    })
    expect(within(contractsTable).getByText('CfdEngine')).toBeInTheDocument()
    expect(within(contractsTable).getByText('HousePool')).toBeInTheDocument()

    const cfdGitHub = within(contractsTable).getByRole('link', {
      name: /View CfdEngine source on GitHub/i,
    })
    expect(cfdGitHub).toHaveAttribute(
      'href',
      'https://github.com/Plether-Fi/plether-core/blob/1209ca8a5b94a4fb6323c7c4585846ed93db3932/packages/perps/src/CfdEngine.sol',
    )
    const housePoolGitHub = within(contractsTable).getByRole('link', {
      name: /View HousePool source on GitHub/i,
    })
    expect(housePoolGitHub).toHaveAttribute(
      'href',
      'https://github.com/Plether-Fi/plether-core/blob/1209ca8a5b94a4fb6323c7c4585846ed93db3932/packages/perps/src/HousePool.sol',
    )
    expect(
      screen.getByRole('link', { name: /Plether-Fi\/plether-core@1209ca8a/i }),
    ).toHaveAttribute(
      'href',
      'https://github.com/Plether-Fi/plether-core/tree/1209ca8a5b94a4fb6323c7c4585846ed93db3932/packages/perps/src',
    )

    // Verify every external Arbiscan URL appears at most once across the entire Protocol page
    const arbiscanHrefs = screen
      .getAllByRole('link')
      .map((el) => el.getAttribute('href') ?? '')
      .filter((href) => href.includes('arbiscan.io'))
    expect(arbiscanHrefs.length).toBeGreaterThanOrEqual(3)
    expect(new Set(arbiscanHrefs.map((u) => u.toLowerCase())).size).toBe(arbiscanHrefs.length)

    fireEvent.change(screen.getByLabelText(/Filter contracts/i), {
      target: { value: '0xeb56' },
    })
    expect(within(contractsTable).getByText('HousePool')).toBeInTheDocument()
    expect(within(contractsTable).queryByText('CfdEngine')).not.toBeInTheDocument()

    // Release history & provenance
    expect(screen.getByText('Core v1.2.3')).toBeInTheDocument()
    expect(screen.getByText('1.5.1-stable')).toBeInTheDocument()
    expect(
      screen.getByTitle(/Inspect commit 1209ca8a5b94a4fb6323c7c4585846ed93db3932 on GitHub/i),
    ).toHaveAttribute(
      'href',
      'https://github.com/Plether-Fi/plether-core/commit/1209ca8a5b94a4fb6323c7c4585846ed93db3932',
    )

    // Total Protocol TVL including protocol contracts and Kernel v4 trading accounts
    expect(
      screen.getByRole('heading', { name: /Total Protocol TVL/i }),
    ).toBeInTheDocument()
    expect(screen.getByText('1,000,000.00 USDG')).toBeInTheDocument()
    expect(screen.getByText('Complete on-chain sum')).toBeInTheDocument()
    expect(screen.getAllByText('145,000.00 USDG').length).toBeGreaterThanOrEqual(2)
    expect(screen.getByText('345,000.00 USDG')).toBeInTheDocument()
    expect(
      screen.getByRole('link', { name: '0x1111…1111' }),
    ).toHaveAttribute('href', '/trading-accounts/0x1111111111111111111111111111111111111111')
  })

  it('fails closed cleanly when Stage 4 backend endpoint returns 404', () => {
    manifestQuery.mockReturnValue({
      data: undefined,
      isLoading: false,
      isError: true,
      error: new InsightsApiError('Not found', 404),
      refetch: vi.fn(),
    })

    show()

    expect(screen.getByText('Awaiting Stage 4 backend deploy')).toBeInTheDocument()
    expect(screen.getByText('Stage 4 backend deployment pending')).toBeInTheDocument()
    expect(screen.getByText('Params unavailable')).toBeInTheDocument()
  })

  it('shows RPC unavailable state without faking verified counts when rpcAvailable is false', () => {
    manifestQuery.mockReturnValue({
      data: {
        ...sampleManifest,
        rpcAvailable: false,
        safety: {
          ...sampleManifest.safety,
          available: false,
          tradingActive: null,
          withdrawalLive: null,
          lpEpochSettlementPaused: null,
          oracleFrozen: null,
          housePoolPaused: null,
          degradedMode: null,
        },
        verification: {
          ...sampleManifest.verification,
          verifiedCount: 0,
          uncheckedCount: 3,
          matchedParameters: 0,
          driftedParameters: 0,
          unavailableParameters: 4,
        },
      },
      isLoading: false,
      isError: false,
      refetch: vi.fn(),
    })

    show()

    expect(screen.getByText('RPC verification unavailable')).toBeInTheDocument()
    expect(screen.queryByText('3 / 3')).not.toBeInTheDocument()
    expect(screen.getAllByText('Unavailable').length).toBeGreaterThanOrEqual(3)
  })
})
