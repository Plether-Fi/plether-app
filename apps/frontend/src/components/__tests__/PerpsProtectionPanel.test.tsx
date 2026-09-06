import { fireEvent, render, screen, waitFor } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { PerpsProtectionPanel, ProtectionHistoryRow } from '../PerpsProtectionPanel'
import type { ProtectionHistoryRecord } from '../../hooks/useProtectionHistory'
import type { PositionProtection } from '../../contracts/positionProtection'
import type { PerpsPosition } from '../../hooks/usePerpsAccount'

const mocks = vi.hoisted(() => ({ manage: vi.fn(), refetch: vi.fn(), history: [] as ProtectionHistoryRecord[] }))
vi.mock('../../hooks/usePerpsTrading', () => ({ usePerpsTrading: () => ({ managePositionProtection: mocks.manage }) }))
vi.mock('../../perps-aa', () => ({ usePerpsIdentity: () => ({ accountAddress: '0x1111111111111111111111111111111111111111' }) }))
vi.mock('../../hooks/useProtectionHistory', () => ({
  useProtectionHistory: () => ({ isPending: false, isError: false, data: { pages: [{ protections: mocks.history }] }, refetch: mocks.refetch }),
  useProtectionEvents: () => ({ isPending: false, isError: false, data: { pages: [{ events: [] }] } }),
  useProtectionExecution: () => ({ data: undefined, isFetching: false, isError: false, refetch: mocks.refetch }),
}))
const protection: PositionProtection = {
  protectionId: 7n, parentOrderId: 12n, linkedOrderId: 0n, account: '0x1111111111111111111111111111111111111111', side: 0, size: 10n ** 18n,
  takeProfitTriggerPrice: 90_000_000n, stopLossTriggerPrice: 110_000_000n, triggerBountyUsdc: 200_000n, executionBountyUsdc: 200_000n,
  armedAt: 0n, armedBlock: 0n, triggerMarkPrice: 0n, triggerPublishTime: 0n, triggeredLeg: 0, status: 1,
}
const props = { protection, rawMark: 100_000_000n, cap: 200_000_000n, configuration: { enabled: true, triggerBountyUsdc: 200_000n, executionBountyUsdc: 200_000n }, pendingOrders: 1, onRefresh: vi.fn() }
const position: PerpsPosition = { exists: true, side: 0, direction: 'long', size: 10n ** 18n, entryPrice: 100_000_000n, marginUsdc: 100_000n, unrealizedPnlUsdc: 0n, maintenanceMarginUsdc: 1_000n, liquidatable: false }
describe('position protection management', () => {
  beforeEach(() => { vi.clearAllMocks(); mocks.history = []; mocks.manage.mockResolvedValue({ protectionId: 7n }) })
  it('cancels only protection and discloses that the parent opening order survives', async () => {
    render(<PerpsProtectionPanel {...props} configuration={{ enabled: false }} />)
    fireEvent.click(screen.getByRole('button', { name: 'Remove TP/SL' }))
    expect(screen.getByText(/opening order will remain committed/)).toBeInTheDocument()
    fireEvent.click(screen.getByRole('button', { name: 'Confirm removal' }))
    await waitFor(() => { expect(mocks.manage).toHaveBeenCalledWith({ action: 'cancel', protectionId: 7n }) })
    expect(screen.queryByRole('button', { name: 'Edit TP/SL' })).not.toBeInTheDocument()
  })
  it('converts displayed edits to raw prices and preserves the reviewed ID', async () => {
    render(<PerpsProtectionPanel {...props} />)
    fireEvent.click(screen.getByRole('button', { name: 'Edit TP/SL' }))
    fireEvent.change(screen.getByLabelText('Take profit (USDC)'), { target: { value: '1.2' } })
    fireEvent.click(screen.getByRole('button', { name: 'Review TP/SL' }))
    expect(mocks.manage).not.toHaveBeenCalled()
    expect(screen.getByRole('heading', { name: 'Review your TP/SL' })).toHaveFocus()
    fireEvent.click(screen.getByRole('button', { name: 'Confirm TP/SL' }))
    await waitFor(() => { expect(mocks.manage).toHaveBeenCalledWith({ action: 'replace', protectionId: 7n, params: { takeProfitTriggerPrice: 80_000_000n, stopLossTriggerPrice: 110_000_000n } }) })
  })
  it('blocks stale confirmation when the active protection changes', () => {
    const view = render(<PerpsProtectionPanel {...props} />)
    fireEvent.click(screen.getByRole('button', { name: 'Remove TP/SL' }))
    view.rerender(<PerpsProtectionPanel {...props} protection={{ ...protection, protectionId: 8n }} />)
    expect(screen.getByRole('button', { name: 'Confirm removal' })).toBeDisabled()
    expect(screen.getByRole('alert')).toHaveTextContent('changed')
    expect(mocks.manage).not.toHaveBeenCalled()
  })
  it('does not offer cancellation or replacement after the trigger is latched', () => {
    render(<PerpsProtectionPanel {...props} protection={{ ...protection, status: 8 }} />)
    expect(screen.getByText('Your position is still open')).toBeInTheDocument()
    expect(screen.getByText('Close delayed')).toBeInTheDocument()
    expect(screen.queryByRole('button', { name: 'Remove TP/SL' })).not.toBeInTheDocument()
    expect(screen.queryByRole('button', { name: 'Edit TP/SL' })).not.toBeInTheDocument()
  })
  it.each([
    [4, 'Closed', 'closed the protected position'], [5, 'Not completed', 'ended without a successful'],
    [6, 'Removed', 'did not close a position'], [7, 'Liquidated', 'position was liquidated'],
  ] as const)('shows terminal state %s with an explicit outcome and no management actions', (status, label, description) => {
    const row = { protectionId: '7', parentOrderId: '12', linkedOrderId: '19', side: 0, status, takeProfitTriggerPrice: '90000000', stopLossTriggerPrice: '110000000' } as ProtectionHistoryRecord
    render(<ProtectionHistoryRow row={row} initiallyExpanded events={[]} />)
    expect(screen.getByText(label)).toBeVisible()
    expect(screen.getByText(new RegExp(description))).toBeVisible()
    expect(screen.getByText(/Latest close #19/)).toBeVisible()
    expect(screen.queryByRole('button', { name: 'Edit TP/SL' })).not.toBeInTheDocument()
    expect(screen.queryByRole('button', { name: 'Remove TP/SL' })).not.toBeInTheDocument()
  })
  it('retains separate failed and queued close attempts in the activity timeline', () => {
    const row = { protectionId: '7', parentOrderId: '12', linkedOrderId: '20', side: 0, status: 8, takeProfitTriggerPrice: '90000000', stopLossTriggerPrice: '110000000' } as ProtectionHistoryRecord
    render(<ProtectionHistoryRow row={row} initiallyExpanded events={[
      { event: 'PositionProtectionCloseAttemptQueued', args: { linkedOrderId: '20' }, blockNumber: '120', blockHash: '0x120', logIndex: '1', transactionHash: '0xabc' },
      { event: 'PositionProtectionCloseAttemptFailed', args: { linkedOrderId: '19', reason: 2, relatched: true }, blockNumber: '119', blockHash: '0x119', logIndex: '1', transactionHash: '0xdef' },
    ]} />)
    expect(screen.getByRole('link', { name: /Close order queued/ })).toBeVisible()
    expect(screen.getByRole('link', { name: /Close did not complete/ })).toBeVisible()
    expect(screen.getByText(/Close #19/)).toBeVisible()
    expect(screen.getByText(/Close #20/)).toBeVisible()
    expect(screen.getByText('Expired · the original trigger remained binding after this attempt')).toBeVisible()
  })
  it('opens the latest terminal outcome when live protection finishes without a page reload', () => {
    const row = { protectionId: '7', parentOrderId: '12', linkedOrderId: '19', side: 0, status: 3, takeProfitTriggerPrice: '90000000', stopLossTriggerPrice: '110000000' } as ProtectionHistoryRecord
    mocks.history = [row]
    const view = render(<PerpsProtectionPanel {...props} protection={{ ...protection, status: 3 }} />)
    expect(screen.getByText(/A trigger was reached and a close order was queued/).closest('details')).not.toHaveAttribute('open')
    mocks.history = [{ ...row, status: 4 }]
    view.rerender(<PerpsProtectionPanel {...props} protection={undefined} position={{ ...position, exists: false }} />)
    expect(screen.getByText(/This TP\/SL closed the protected position/).closest('details')).toHaveAttribute('open')
    expect(screen.getByText('plDXY Perp · No open position')).toBeVisible()
    expect(screen.queryByText(/Full position/)).not.toBeInTheDocument()
    expect(screen.queryByRole('button', { name: 'Add TP/SL' })).not.toBeInTheDocument()
  })
  it('keeps the reviewed percentage trigger prices fixed when the market moves', async () => {
    const view = render(<PerpsProtectionPanel {...props} />)
    fireEvent.click(screen.getByRole('button', { name: 'Edit TP/SL' }))
    expect(screen.getByLabelText('Take profit (%)')).toHaveValue('10')
    fireEvent.change(screen.getByLabelText('Take profit (%)'), { target: { value: '20' } })
    fireEvent.click(screen.getByRole('button', { name: 'Review TP/SL' }))
    view.rerender(<PerpsProtectionPanel {...props} rawMark={99_000_000n} />)
    expect(screen.getByText('1.2000')).toBeInTheDocument()
    fireEvent.click(screen.getByRole('button', { name: 'Confirm TP/SL' }))
    await waitFor(() => { expect(mocks.manage).toHaveBeenCalledWith({ action: 'replace', protectionId: 7n, params: { takeProfitTriggerPrice: 80_000_000n, stopLossTriggerPrice: 110_000_000n } }) })
  })
  it('keeps invalid inputs editable and blocks review', () => {
    render(<PerpsProtectionPanel {...props} />)
    fireEvent.click(screen.getByRole('button', { name: 'Edit TP/SL' }))
    fireEvent.change(screen.getByLabelText('Take profit (USDC)'), { target: { value: '0.5' } })
    expect(screen.getByLabelText('Take profit (USDC)')).toHaveAttribute('aria-invalid', 'true')
    fireEvent.click(screen.getByRole('button', { name: 'Review TP/SL' }))
    expect(screen.getByRole('alert')).toHaveTextContent('Take profit must be above')
    expect(screen.queryByRole('button', { name: 'Confirm TP/SL' })).not.toBeInTheDocument()
    expect(mocks.manage).not.toHaveBeenCalled()
  })
  it('blocks creation if the execution reserve changes after review', () => {
    const createProps = { ...props, protection: undefined, position, pendingOrders: 0 }
    const view = render(<PerpsProtectionPanel {...createProps} />)
    fireEvent.click(screen.getByRole('button', { name: 'Add TP/SL' }))
    fireEvent.change(screen.getByLabelText('Stop loss (USDC)'), { target: { value: '0.9' } })
    fireEvent.click(screen.getByRole('button', { name: 'Review TP/SL' }))
    expect(screen.getByText(/from free margin/)).toHaveTextContent('0.4 USDC')
    view.rerender(<PerpsProtectionPanel {...createProps} configuration={{ ...props.configuration, triggerBountyUsdc: 300_000n }} />)
    expect(screen.getByRole('button', { name: 'Confirm TP/SL' })).toBeDisabled()
    expect(screen.getByRole('alert')).toHaveTextContent('changed')
    expect(mocks.manage).not.toHaveBeenCalled()
  })
  it('creates single-leg TP/SL only after explicit review and confirmation', async () => {
    render(<PerpsProtectionPanel {...props} protection={undefined} position={position} pendingOrders={0} />)
    fireEvent.click(screen.getByRole('button', { name: 'Add TP/SL' }))
    expect(screen.getByRole('button', { name: 'Review TP/SL' })).toBeDisabled()
    fireEvent.change(screen.getByLabelText('Stop loss (USDC)'), { target: { value: '0.9' } })
    fireEvent.click(screen.getByRole('button', { name: 'Review TP/SL' }))
    expect(screen.getByText('No take profit trigger')).toBeInTheDocument()
    expect(mocks.manage).not.toHaveBeenCalled()
    fireEvent.click(screen.getByRole('button', { name: 'Confirm TP/SL' }))
    await waitFor(() => { expect(mocks.manage).toHaveBeenCalledWith({ action: 'create', protectionId: undefined, params: { takeProfitTriggerPrice: 0n, stopLossTriggerPrice: 110_000_000n } }) })
    expect(screen.getByRole('heading', { name: 'Take profit & stop loss' })).toHaveFocus()
  })
  it('shows confirmation failure without losing the reviewed prices', async () => {
    mocks.manage.mockRejectedValue(new Error('Wallet request rejected'))
    render(<PerpsProtectionPanel {...props} />)
    fireEvent.click(screen.getByRole('button', { name: 'Edit TP/SL' }))
    fireEvent.click(screen.getByRole('button', { name: 'Review TP/SL' }))
    fireEvent.click(screen.getByRole('button', { name: 'Confirm TP/SL' }))
    await waitFor(() => { expect(screen.getByRole('alert')).toHaveTextContent('Wallet request rejected') })
    expect(screen.getByText('1.1000')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Back to edit' })).toBeEnabled()
  })
})
