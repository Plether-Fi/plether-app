import { render, screen } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { TransactionDetailPage } from './TransactionDetailPage'

const apiMocks = vi.hoisted(() => ({
  useCurrentProtocolRelease: vi.fn(),
  useProtocolTransaction: vi.fn(),
}))

vi.mock('../api', () => apiMocks)

const txHash = `0x${'1'.repeat(64)}`
const account = '0x2222222222222222222222222222222222222222'

beforeEach(() => {
  vi.clearAllMocks()
  apiMocks.useCurrentProtocolRelease.mockReturnValue({
    data: { releaseId: 'release-1' },
    isError: false,
    isLoading: false,
  })
  apiMocks.useProtocolTransaction.mockReturnValue({
    data: transactionResponse(),
    isError: false,
    isLoading: false,
  })
})

describe('TransactionDetailPage', () => {
  it('renders transaction-level state impact with source and attribution evidence', () => {
    renderPage()

    expect(apiMocks.useProtocolTransaction).toHaveBeenCalledWith('release-1', txHash)
    const stateImpact = sectionNamed('State impact')
    expect(stateImpact).toHaveTextContent('HousePool')
    expect(stateImpact).toHaveTextContent('Free Usdc Delta')
    expect(stateImpact).toHaveTextContent('-2.00 USDC')
    expect(stateImpact).toHaveTextContent('Senior tranche')
    expect(stateImpact).toHaveTextContent('Principal Usdc Delta')
    expect(stateImpact).toHaveTextContent('1.25 USDC')
    expect(stateImpact).toHaveTextContent(account)
    expect(stateImpact).toHaveTextContent('block-level delta')
    expect(stateImpact).toHaveTextContent('protocol.transaction.state-impact.v1')
    expect(stateImpact).toHaveTextContent('beforeBlock')
    expect(screen.getByText('Complete state-impact evidence')).toBeInTheDocument()
    expect(screen.getByRole('link', { name: '← Back to activity' })).toHaveAttribute(
      'href',
      '/transactions?release=release-1',
    )
  })

  it('renders economics and each action-specific analysis with nested availability', () => {
    renderPage()

    const analysis = sectionNamed('Transaction analysis')
    expect(analysis).toHaveTextContent('Protocol Fee Usdc')
    expect(analysis).toHaveTextContent('0.50 USDC')
    expect(analysis).toHaveTextContent('Liquidation analysis · 1')
    expect(analysis).toHaveTextContent('Bounty Usdc')
    expect(analysis).toHaveTextContent('0.75 USDC')
    expect(analysis).toHaveTextContent('Margin actions · 1')
    expect(analysis).toHaveTextContent('Margin Delta Usdc')
    expect(analysis).toHaveTextContent('-1.00 USDC')
    expect(analysis).toHaveTextContent('Tranche actions · 1')
    expect(analysis).toHaveTextContent('Shares')
    expect(analysis).toHaveTextContent('500000000000000000')
    expect(analysis).toHaveTextContent('analysis.oraclePublishTimes')
    expect(analysis).toHaveTextContent('Current Release Telemetry Missing')
    expect(screen.getByText('Complete derived transaction analysis')).toBeInTheDocument()
  })

  it('makes unavailable impact and non-applicable analyses explicit', () => {
    const response = transactionResponse()
    response.transaction.stateImpact = {
      accounts: [],
      housePool: null,
      senior: null,
      junior: null,
      sourceBlocks: {},
      provenance: 'unavailable',
      formula: null,
      evidenceReferences: [],
      availability: [{
        field: 'stateImpact.housePool',
        reason: 'archive_state_unavailable',
      }],
    }
    response.transaction.analysis = {
      economics: {},
      liquidations: [],
      marginActions: [],
      trancheActions: [],
      availability: [{
        field: 'analysis.economics',
        reason: 'current_release_telemetry_missing',
      }],
      provenance: 'best_effort',
    }
    apiMocks.useProtocolTransaction.mockReturnValue({
      data: response,
      isError: false,
      isLoading: false,
    })

    renderPage()

    expect(screen.getByText('No account-level state impact is available for this transaction.')).toBeInTheDocument()
    expect(screen.getByText('HousePool state impact is unavailable for this transaction.')).toBeInTheDocument()
    expect(screen.getByText('No liquidation analysis applies or is reconstructable for this transaction.')).toBeInTheDocument()
    expect(screen.getByText('No margin-action analysis applies or is reconstructable for this transaction.')).toBeInTheDocument()
    expect(screen.getByText('No tranche-action analysis applies or is reconstructable for this transaction.')).toBeInTheDocument()
    expect(screen.getByText((_, element) =>
      element?.tagName === 'LI'
      && element.textContent?.includes('stateImpact.housePool') === true
      && element.textContent?.includes('Archive State Unavailable') === true,
    )).toBeInTheDocument()
    expect(screen.getByText((_, element) =>
      element?.tagName === 'LI'
      && element.textContent?.includes('analysis.economics') === true
      && element.textContent?.includes('Current Release Telemetry Missing') === true,
    )).toBeInTheDocument()
  })

  it('uses the requested release without waiting for the current release', () => {
    apiMocks.useCurrentProtocolRelease.mockReturnValue({
      data: undefined,
      isError: false,
      isLoading: true,
    })

    renderPage(`?release=release-archive`)

    expect(apiMocks.useProtocolTransaction).toHaveBeenCalledWith('release-archive', txHash)
    expect(screen.getByRole('heading', { name: 'Canonical transaction' })).toBeInTheDocument()
  })
})

function renderPage(search = '') {
  render(
    <MemoryRouter initialEntries={[`/transactions/${txHash}${search}`]}>
      <Routes>
        <Route path="/transactions/:txHash" element={<TransactionDetailPage />} />
      </Routes>
    </MemoryRouter>,
  )
}

function sectionNamed(name: string): HTMLElement {
  const heading = screen.getByRole('heading', { name })
  const section = heading.closest('section')
  if (!section) throw new Error(`Section ${name} not found`)
  return section
}

function transactionResponse() {
  return {
    releaseId: 'release-1',
    chainId: '421614',
    confirmedBlock: {
      number: '123',
      hash: `0x${'3'.repeat(64)}`,
      timestamp: 1_785_000_000,
    },
    indexerTimestamp: 1_785_000_010,
    calculationVersion: 'protocol-transparency-v1',
    evidence: {
      transaction: 'exact_receipt',
      actions: 'versioned_projection',
    },
    availability: [],
    transaction: {
      chainTransaction: {
        transactionHash: txHash,
        sender: account,
      },
      actions: [],
      events: [],
      batchActionCount: 0,
      stateImpact: {
        accounts: [{
          account,
          sizeDelta: '-1000000000000000000',
          provenance: 'block-level delta',
        }],
        housePool: {
          freeUsdcDelta: '-2000000',
          before: { freeUsdc: '10000000' },
          after: { freeUsdc: '8000000' },
        },
        senior: {
          principalUsdcDelta: '1250000',
        },
        junior: {
          principalUsdcDelta: '-3250000',
        },
        sourceBlocks: {
          beforeBlock: '121',
          afterBlock: '122',
        },
        provenance: 'block-level delta',
        formula: 'protocol.transaction.state-impact.v1',
        evidenceReferences: ['snapshot:121', 'snapshot:122'],
        availability: [],
      },
      analysis: {
        economics: {
          protocolFeeUsdc: '500000',
          immediatePayoutUsdc: '1750000',
        },
        liquidations: [{
          account,
          bountyUsdc: '750000',
          provenance: 'exact_terminal_event',
        }],
        marginActions: [{
          account,
          marginDeltaUsdc: '-1000000',
          provenance: 'versioned_projection',
        }],
        trancheActions: [{
          tranche: 'senior',
          shares: '500000000000000000',
          provenance: 'exact_log',
        }],
        availability: [{
          field: 'analysis.oraclePublishTimes',
          reason: 'current_release_telemetry_missing',
        }],
        provenance: 'mixed_exact_and_derived',
        formula: 'protocol.transaction.state-impact.v1',
        evidenceReferences: ['action:liquidation:0'],
      },
    },
  }
}
