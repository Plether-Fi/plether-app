import { fireEvent, render, screen, within } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { PerpsCloseReconciliationDisclosure } from '../PerpsCloseReconciliationDetails'
import { derivePerpsCloseReconciliation } from '../../utils/perpsCloseReconciliation'
import { closeSettlementAdjustmentReceipt } from '../../utils/__fixtures__/closeSettlementAdjustment'

describe('PerpsCloseReconciliationDisclosure', () => {
  it('explains the adjustment without classifying it as debt or profit', () => {
    render(<PerpsCloseReconciliationDisclosure
      reconciliation={derivePerpsCloseReconciliation(closeSettlementAdjustmentReceipt)!}
    />)
    const trigger = screen.getByRole('button', { name: /Detailed close accounting/ })
    expect(trigger).toHaveAttribute('aria-expanded', 'false')
    expect(within(trigger).getByText('-7 271.14').closest('.text-brand-peach')).not.toBeNull()
    expect(screen.queryByTestId('close-reconciliation')).not.toBeInTheDocument()
    fireEvent.click(trigger)
    expect(trigger).toHaveAttribute('aria-expanded', 'true')
    expect(screen.getByText('Settlement adjustment').closest('div')?.querySelector('dd'))
      .toHaveClass('text-content-primary')
    expect(screen.getByText('The difference between the calculated close result and the recorded change in your account balance and trader claims. This amount is not classified as bad debt.'))
      .toBeInTheDocument()
    expect(screen.getByText('Actual account change includes changes to your Margin Account balance and trader claims.'))
      .toBeInTheDocument()
    fireEvent.click(trigger)
    expect(screen.queryByTestId('close-reconciliation')).not.toBeInTheDocument()
  })

  it('uses the actual zero outcome for summary color even when the calculated result is negative', () => {
    const reconciliation = derivePerpsCloseReconciliation({
      ...closeSettlementAdjustmentReceipt,
      executionBountyUsdc: '0',
      actionChargeCollectedUsdc: '0',
      grossAccountDebitUsdc: '0',
      preSettlementBalanceUsdc: '0',
    })!
    render(<PerpsCloseReconciliationDisclosure reconciliation={reconciliation} />)
    const trigger = screen.getByRole('button', { name: /Detailed close accounting/ })
    expect(within(trigger).getByText('0').closest('.text-content-secondary')).not.toBeNull()
    expect(trigger.querySelector('.text-brand-peach')).toBeNull()
  })
})
