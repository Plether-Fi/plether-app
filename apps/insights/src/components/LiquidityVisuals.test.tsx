import { fireEvent, render, screen } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { describe, expect, it } from 'vitest'
import type { LpSettlementRecord, ReturnReportAudit } from '../api'
import {
  ResidualAccuracyMeter,
  SettlementContinuityStrip,
} from './LiquidityVisuals'
import {
  classifySettlementBlock,
  computeResidualAccuracy,
} from '../utils/liquidityVisuals'

const baseSettlement: LpSettlementRecord = {
  epoch: 496900,
  cutoffEpoch: '496899',
  status: 'confirmed_success',
  executionPath: 'cached_mark',
  txHash: `0x${'a'.repeat(64)}`,
  receiptBlock: '325000105',
  createdAt: '2026-11-26T12:00:00Z',
  confirmedAt: '2026-11-26T12:00:04Z',
  confirmationLatencySeconds: 4,
  replacementCount: 0,
  seniorDepositAssets: '5000000000',
  seniorRedeemAssets: '0',
  juniorDepositAssets: '1000000000',
  juniorRedeemAssets: '0',
  seniorBacklog: false,
  juniorBacklog: false,
  entriesDeferred: false,
}

const baseReport: ReturnReportAudit = {
  startBlockNumber: '324000000',
  endBlockNumber: '325000000',
  sourceCommit: '1209ca8',
  schemaVersion: 3,
  updatedAt: '2026-11-26T12:00:00Z',
  sourceBlockCount: 674,
  reportedReturnPp: 1.5,
  unexplainedPp: 0,
  netReturnUsdg: '1000000000',
  unexplainedUsdg: '0',
  liquidationFeesUsdg: '50000000',
  notesCount: 0,
}

describe('LiquidityVisuals', () => {
  it('classifies clear, warning, and blocked LP settlement epochs and renders interactive continuity strip', () => {
    const clear = baseSettlement
    const warn = { ...baseSettlement, epoch: 496901, seniorBacklog: true, txHash: `0x${'b'.repeat(64)}` }
    const failed = { ...baseSettlement, epoch: 496902, status: 'confirmed_revert', txHash: `0x${'c'.repeat(64)}` }

    expect(classifySettlementBlock(clear)).toBe('clear')
    expect(classifySettlementBlock(warn)).toBe('warning')
    expect(classifySettlementBlock(failed)).toBe('blocked')

    render(
      <MemoryRouter>
        <SettlementContinuityStrip settlements={[failed, warn, clear]} />
      </MemoryRouter>,
    )

    expect(screen.getByText('1 clear')).toBeInTheDocument()
    expect(screen.getByText('1 warn')).toBeInTheDocument()
    expect(screen.getByText('1 blocked')).toBeInTheDocument()

    // Hover the warning epoch block to inspect it and verify the block itself links to the epoch page
    const warnBlock = screen.getByRole('link', { name: /Epoch #496,901/i })
    expect(warnBlock).toHaveAttribute('href', '/liquidity/epochs/496901')
    fireEvent.mouseEnter(warnBlock)
    expect(screen.getByText(/Senior backlog/i)).toBeInTheDocument()
    expect(screen.getByRole('link', { name: /Epoch #496,901 →/i })).toHaveAttribute(
      'href',
      '/liquidity/epochs/496901',
    )
  })

  it('falls back to attribution report continuity blocks when no settlement txs exist', () => {
    render(
      <MemoryRouter>
        <SettlementContinuityStrip settlements={[]} reports={[baseReport]} />
      </MemoryRouter>,
    )
    expect(screen.getByText('Attribution report continuity')).toBeInTheDocument()
    expect(screen.getByText('1 clear')).toBeInTheDocument()
  })

  it('computes exact and partial residual accuracy meters without using $', () => {
    expect(computeResidualAccuracy(baseReport)).toEqual({
      available: true,
      exact: true,
      attributedPct: 100,
      residualPct: 0,
    })

    const partialReport: ReturnReportAudit = {
      ...baseReport,
      netReturnUsdg: '1000000000',
      unexplainedUsdg: '25000000',
      unexplainedPp: 0.0375,
    }
    expect(computeResidualAccuracy(partialReport)).toMatchObject({
      available: true,
      exact: false,
      attributedPct: 97.5,
      residualPct: 2.5,
    })

    const { rerender } = render(<ResidualAccuracyMeter report={baseReport} />)
    expect(screen.getByText('Exact (0.00)')).toBeInTheDocument()
    expect(screen.getByText('100.00%')).toBeInTheDocument()

    rerender(<ResidualAccuracyMeter report={partialReport} />)
    expect(screen.getByText('25.00 USDG')).toBeInTheDocument()
    expect(screen.getByText('97.50%')).toBeInTheDocument()
  })
})
