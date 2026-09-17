import { render, screen } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import type { SponsoredOperation } from '../../perps-aa/operationStore'
import type { PerpsOrderHistoryRow } from '../../hooks/usePerpsHistory'
import { RecoveredOrderStatus } from '../RecoveredOrderStatus'

const history = vi.hoisted(() => ({ orderHistory: [] as PerpsOrderHistoryRow[], refetch: vi.fn() }))
vi.mock('../../hooks/usePerpsHistory', () => ({ usePerpsHistory: () => history }))
const operation = {
  accountAddress: '0x1111111111111111111111111111111111111111',
  transactionHash: `0x${'a'.repeat(64)}`, transactionHashVerified: true,
} as SponsoredOperation
const row = { account: operation.accountAddress, commitTxHash: operation.transactionHash,
  clientOrderId: `0x${'b'.repeat(64)}`, orderId: 12n, side: 'Long', size: '$100', status: 'Committed',
} as PerpsOrderHistoryRow

describe('recovered order execution', () => {
  beforeEach(() => { history.orderHistory = [] })
  it('shows pending execution instead of treating a confirmed commit as a filled trade', () => {
    history.orderHistory = [row]
    render(<RecoveredOrderStatus operation={operation} />)
    expect(screen.getByText('Awaiting execution')).toBeVisible()
  })
  it('shows the matching terminal outcome directly', () => {
    history.orderHistory = [{ ...row, status: 'Failed: Slippage' }]
    render(<RecoveredOrderStatus operation={operation} />)
    expect(screen.getByText('Failed: Slippage')).toBeVisible()
  })
  it('does not assign another account’s order or an ambiguous batch outcome to this attempt', () => {
    history.orderHistory = [{ ...row, account: '0x2222222222222222222222222222222222222222' }]
    const view = render(<RecoveredOrderStatus operation={operation} />)
    expect(screen.getByText(/Execution status is not available yet/)).toBeVisible()
    history.orderHistory = [row, { ...row, orderId: 13n }]
    view.rerender(<RecoveredOrderStatus operation={operation} />)
    expect(screen.getByText(/Execution status is not available yet/)).toBeVisible()
  })
})
