import { fireEvent, render, screen, within } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { InsightsApiError, type TradingAccountsOverviewResponse } from '../api'
import { resolvePerpsChain } from '../utils/chain'
import { TradingAccountsPage } from './TradingAccountsPage'

const protocolContext = vi.hoisted(() => vi.fn())
const tradingAccountsQuery = vi.hoisted(() => vi.fn())

vi.mock('../hooks/useProtocolContext', () => ({ useProtocolContext: protocolContext }))
vi.mock('../api', async (importOriginal) => ({
  ...(await importOriginal<typeof import('../api')>()),
  useTradingAccountsOverview: tradingAccountsQuery,
}))

const sampleOverview: TradingAccountsOverviewResponse = {
  network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
  observedAt: '2026-11-27T15:00:00Z',
  configured: true,
  status: 'healthy',
  profile: {
    deployment: 'sepolia-kernel-v4-canary',
    accountVersion: 'kernel-v4-uups',
    entryPointVersion: '0.9',
    entryPoint: '0x433709009b8330fda32311df1c2afa402ed8d009',
    entryPointCodeHash: `0x${'1'.repeat(64)}`,
    factory: `0x${'2'.repeat(40)}`,
    factoryCodeHash: `0x${'3'.repeat(64)}`,
    implementation: `0x${'4'.repeat(40)}`,
    implementationCodeHash: `0x${'5'.repeat(64)}`,
    validator: `0x${'6'.repeat(40)}`,
    validatorCodeHash: `0x${'7'.repeat(64)}`,
    accountCodeHash: `0x${'8'.repeat(64)}`,
    paymaster: `0x${'9'.repeat(40)}`,
    policyId: 'kernel-sepolia-v1',
  },
  controls: {
    ethModeEnabled: true,
    sponsorshipEnabled: true,
    protectionCommitsEnabled: true,
    ownerAdmissionMode: 'canary',
    admittedOwnerCount: 12,
  },
  pipeline: {
    bundlerReachable: true,
    bundlerEntryPointSupported: true,
    bundlerStatus: 'ok',
    reconcilerSafeBlock: '333171200',
    reconcilerHeadBlock: '333171209',
    reconcilerBlocksBehind: 9,
    reconcilerLagSeconds: 3,
    reconcilerMaxSafeLagSeconds: 120,
    reconcilerFresh: true,
    reconcilerPausedReason: null,
  },
  adoption: {
    uniqueAccounts: 18,
    uniqueOwners: 18,
    activeAccounts24h: 11,
    totalPreparations: 164,
    totalOperations: 140,
    operations24h: 48,
    sponsoredOpsTotal: 98,
    ethOpsTotal: 42,
    sponsoredOps24h: 34,
    ethOps24h: 14,
    cancellationOpsTotal: 3,
    pendingOpsCount: 1,
    oldestPendingAgeSeconds: 12,
  },
  lifecycle: {
    includedSucceeded: 132,
    includedReverted: 2,
    pending: 1,
    nonceConsumed: 3,
    sponsorshipExpired: 2,
    cancellationsPrepared: 4,
    cancellationsAccepted: 3,
    successRateBps: 9850,
    totalGasCostWei: '14200000000000000',
    sponsoredGasCostWei: '9800000000000000',
    ethGasCostWei: '4400000000000000',
  },
  accounts: [
    {
      sender: `0x${'a'.repeat(40)}`,
      ownerAddress: `0x${'b'.repeat(40)}`,
      discordUsername: 'kernel_pilot',
      sponsoredCount: 14,
      ethCount: 5,
      includedCount: 18,
      pendingCount: 0,
      retiredCount: 1,
      highestNonce: '19',
      lastMode: 'sponsored',
      firstSeenAt: '2026-11-22T10:00:00Z',
      lastActivityAt: '2026-11-27T14:55:00Z',
    },
  ],
  operations: [
    {
      userOpHash: `0x${'c'.repeat(64)}`,
      sender: `0x${'a'.repeat(40)}`,
      ownerAddress: `0x${'b'.repeat(40)}`,
      mode: 'sponsored',
      cancellationOnly: false,
      nonce: '19',
      status: 'included',
      executionSuccess: true,
      txHash: `0x${'d'.repeat(64)}`,
      blockNumber: '333171180',
      actualGasCostWei: '95000000000000',
      cancellationHash: null,
      createdAt: '2026-11-27T14:55:00Z',
    },
    {
      userOpHash: `0x${'e'.repeat(64)}`,
      sender: `0x${'a'.repeat(40)}`,
      ownerAddress: `0x${'b'.repeat(40)}`,
      mode: 'eth',
      cancellationOnly: true,
      nonce: '18',
      status: 'nonce_consumed',
      executionSuccess: null,
      txHash: null,
      blockNumber: null,
      actualGasCostWei: null,
      cancellationHash: `0x${'f'.repeat(64)}`,
      createdAt: '2026-11-27T14:40:00Z',
    },
  ],
}

beforeEach(() => {
  protocolContext.mockReturnValue({
    chainId: 421614,
    chain: resolvePerpsChain(421614),
    releaseVersion: 'v2.0.0',
    blockNumber: '333171209',
    observedAt: '2026-11-27T15:00:00Z',
  })
  tradingAccountsQuery.mockReturnValue({
    data: sampleOverview,
    isLoading: false,
    isError: false,
    error: null,
    refetch: vi.fn(),
  })
})

function show() {
  return render(
    <MemoryRouter>
      <TradingAccountsPage />
    </MemoryRouter>,
  )
}

describe('TradingAccountsPage (ZeroDev Kernel v4 Smart Wallets)', () => {
  it('renders Kernel v4 stack controls, Sponsored vs ETH-paid split, pinned contracts, smart accounts, and filterable operation journal', () => {
    const { container } = show()
    expect(screen.getByRole('heading', { name: /Trading Accounts \(Smart Wallets\)/i })).toBeInTheDocument()
    expect(screen.getByText('Sponsored Gas: On')).toBeInTheDocument()
    expect(screen.getByText('ETH-Paid Gas: On')).toBeInTheDocument()
    expect(screen.getByText('Alto v0.9 SafeMode OK')).toBeInTheDocument()
    expect(screen.getByText('70% / 30%')).toBeInTheDocument()
    expect(screen.getByText('EntryPoint v0.9')).toBeInTheDocument()
    expect(screen.getByText('KernelFactory')).toBeInTheDocument()
    expect(screen.getByText('@kernel_pilot')).toBeInTheDocument()
    expect(container.textContent).not.toContain('$')

    // Filter operation journal to Cancellations
    const opsTable = screen.getByRole('table', { name: /Recent ZeroDev Kernel v4 UserOperations/i })
    expect(within(opsTable).getAllByRole('row')).toHaveLength(3)
    fireEvent.click(screen.getByRole('button', { name: 'Cancellations' }))
    expect(within(opsTable).getAllByRole('row')).toHaveLength(2)
    expect(within(opsTable).getByText('ETH Cancel')).toBeInTheDocument()
  })

  it('fails closed cleanly when Kernel v4 gateway is not configured or awaiting backend deploy', () => {
    tradingAccountsQuery.mockReturnValue({
      data: { ...sampleOverview, configured: false, status: 'unconfigured', profile: null, controls: null, pipeline: null },
      isLoading: false,
      isError: false,
      error: null,
      refetch: vi.fn(),
    })
    show()
    expect(screen.getByText('ZeroDev Kernel v4 gateway not active on this environment')).toBeInTheDocument()
  })

  it('shows awaiting deploy state when backend endpoint returns 404', () => {
    tradingAccountsQuery.mockReturnValue({
      data: undefined,
      isLoading: false,
      isError: true,
      error: new InsightsApiError('Not found', 404),
      refetch: vi.fn(),
    })
    show()
    expect(screen.getByText('Kernel Trading Accounts telemetry awaiting backend deploy')).toBeInTheDocument()
  })
})
