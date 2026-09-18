import { fireEvent, render, screen, within } from '@testing-library/react'
import { describe, expect, it } from 'vitest'
import { PerpsCloseReconciliationDisclosure } from '../PerpsCloseReconciliationDetails'
import { derivePerpsCloseReconciliation } from '../../utils/perpsCloseReconciliation'
import { closeOrder14Receipt } from '../../utils/__fixtures__/closeOrder14'
import { closeSettlementAdjustmentReceipt } from '../../utils/__fixtures__/closeSettlementAdjustment'

describe('PerpsCloseReconciliationDisclosure', () => {
  it('explains the adjustment without classifying it as debt or profit', () => {
    render(<PerpsCloseReconciliationDisclosure
      reconciliation={derivePerpsCloseReconciliation(closeSettlementAdjustmentReceipt)!}
    />)
    expect(screen.queryByRole('button', { name: /(?:Show|Hide) detailed accounting/ })).not.toBeInTheDocument()
    expect(screen.getByTestId('close-reconciliation')).toBeVisible()
    expect(screen.getByRole('region', { name: 'This close' })).toBeVisible()
    expect(screen.getByRole('region', { name: 'Position lifetime' })).toBeVisible()
    expect(screen.getByRole('region', { name: 'Collateral & balances' })).toBeVisible()
    expect(screen.getByText('Settlement adjustment').closest('div')?.querySelector('dd'))
      .toHaveClass('text-content-primary')
    fireEvent.focus(screen.getByRole('button', { name: 'Settlement adjustment info' }))
    expect(screen.getByText('The difference between the calculated close result and the recorded change in your account balance and trader claims. This amount is not classified as bad debt.'))
      .toBeInTheDocument()
  })

  it.each([
    ['148190000', '-148.19', 'text-brand-peach'],
    ['-148190000', '+148.19', 'text-positive'],
    ['0', '0', 'text-content-secondary'],
  ])('shows lifetime VPI %s separately from the close rebate', (total, displayed, tone) => {
    const reconciliation = derivePerpsCloseReconciliation({
      ...closeSettlementAdjustmentReceipt,
      totalPositionVpiUsdc: total,
    })!
    render(<PerpsCloseReconciliationDisclosure reconciliation={reconciliation} />)
    const totalRow = screen.getByText('Total VPI balance').closest('div')!
    expect(within(totalRow).getByText(displayed)).toBeInTheDocument()
    expect(totalRow.querySelector('dd')).toHaveClass(tone)
    expect(within(screen.getByText('VPI rebate').closest('div')!).getByText('+251.81'))
      .toBeInTheDocument()
    expect(reconciliation.actualAccountChangeUsdc)
      .toBe(derivePerpsCloseReconciliation(closeSettlementAdjustmentReceipt)!.actualAccountChangeUsdc)
  })

  it.each([undefined, '', 'invalid'])('does not invent a total for unavailable history (%s)', (total) => {
    const reconciliation = derivePerpsCloseReconciliation({
      ...closeSettlementAdjustmentReceipt,
      totalPositionVpiUsdc: total,
    })!
    render(<PerpsCloseReconciliationDisclosure reconciliation={reconciliation} />)
    expect(within(screen.getByText('Total VPI balance').closest('div')!).getByText('Unavailable'))
      .toBeInTheDocument()
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
    const summary = screen.getByTestId('close-reconciliation-summary')
    expect(within(summary).getByText('0').closest('.text-content-secondary')).not.toBeNull()
    expect(summary.querySelector('.text-brand-peach')).toBeNull()
  })
  it('separates the lifetime result from the current close and reconciles VPI', () => {
    const reconciliation = derivePerpsCloseReconciliation({
      ...closeSettlementAdjustmentReceipt,
      totalPositionVpiUsdc: '148190000',
      positionLifetimeNetResultUsdc: '-8000000000',
      positionLifetimeTradesResultUsdc: '-7990000000',
      positionLifetimeAccountAdjustmentUsdc: '-10000000',
    }, { preExecutionPositionMarginUsdc: 500_000_000n })!
    render(<PerpsCloseReconciliationDisclosure reconciliation={reconciliation} />)
    expect(screen.getByText('Position fully closed')).toBeInTheDocument()
    expect(within(screen.getByTestId('close-reconciliation-summary')).getByText('Total realized result since opening').closest('div')?.querySelector('dd')).toHaveTextContent('-8 000')
    const close = within(screen.getByRole('region', { name: 'This close' }))
    const lifetime = within(screen.getByRole('region', { name: 'Position lifetime' }))
    const balances = within(screen.getByRole('region', { name: 'Collateral & balances' }))
    expect(close.getByText('Net result of this close').closest('div')?.querySelector('dd')).toHaveTextContent('-7 271.14')
    expect(close.getByText('Settlement adjustment')).toBeInTheDocument()
    expect(close.queryByText('Total VPI balance')).not.toBeInTheDocument()
    expect(lifetime.getByText('Total realized result since opening').closest('div')?.querySelector('dd')).toHaveTextContent('-8 000')
    expect(lifetime.getByText('Total VPI balance')).toBeInTheDocument()
    expect(balances.getByText('Total account change').closest('div')?.querySelector('dd')).toHaveTextContent('-7 271.14')
    expect(balances.getByText('Position margin released').closest('div')?.querySelector('dd')).toHaveTextContent('500')
    expect(screen.getByText('Previous trades net result').closest('div')?.querySelector('dd')).toHaveTextContent('-718.86')
    expect(screen.getByText('Other account changes').closest('div')?.querySelector('dd')).toHaveTextContent('-10')
    expect(screen.getByText('Previous net VPI').closest('div')?.querySelector('dd')).toHaveTextContent('-400')
    expect(screen.getByText('This close’s VPI').closest('div')?.querySelector('dd')).toHaveTextContent('+251.81')
    expect(within(screen.getByTestId('close-reconciliation-summary')).getByText('Position margin released').closest('div')?.querySelector('dd')).toHaveTextContent('500')
    expect(screen.queryByText('Remaining position quantity')).not.toBeInTheDocument()
  })

  it('shows the remaining quantity for partial closes and keeps incomplete lifetime accounting unavailable', () => {
    const reconciliation = derivePerpsCloseReconciliation({
      ...closeOrder14Receipt,
      positionLifetimeNetResultUsdc: '10',
      positionLifetimeTradesResultUsdc: '20',
      positionLifetimeAccountAdjustmentUsdc: '0',
    })!
    render(<PerpsCloseReconciliationDisclosure reconciliation={reconciliation} />)
    expect(screen.getByText('Position partially closed')).toBeInTheDocument()
    expect(within(screen.getByTestId('close-reconciliation-summary')).getByText('Remaining position quantity').closest('div')?.querySelector('dd')).toHaveTextContent('20 600')
    expect(within(screen.getByTestId('close-reconciliation-summary')).getByText('Total realized result since opening').closest('div')?.querySelector('dd')).toHaveTextContent('Unavailable')
    expect(within(screen.getByTestId('close-reconciliation-summary')).getByText('Position margin released').closest('div')?.querySelector('dd')).toHaveTextContent('Unavailable')
    expect(screen.queryByText('Position fully closed')).not.toBeInTheDocument()
  })

})
