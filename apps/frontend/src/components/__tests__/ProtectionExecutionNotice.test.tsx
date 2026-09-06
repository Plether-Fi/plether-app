import { act, fireEvent, render, screen } from '@testing-library/react'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { ProtectionExecutionNotice } from '../ProtectionExecutionNotice'
import type { PositionProtection } from '../../contracts/positionProtection'
import type { ProtectionExecutionReport, ProtectionExecutionReason } from '../../utils/protectionExecution'

const protection = { protectionId: 7n, linkedOrderId: 19n, account: '0x1111111111111111111111111111111111111111', status: 8 } as PositionProtection
function report(reason: ProtectionExecutionReason = 'queue-congested'): ProtectionExecutionReport {
  return { receivedAt: Date.now(), observation: {
    protectionId: '7', linkedOrderId: '19', account: protection.account, protectionStatus: 8,
    reason, checkedBlock: '120', checkedBlockHash: '0x' + 'ab'.repeat(32), checkedAt: new Date().toISOString(), ageSeconds: 0,
  } }
}
describe('protection execution notice', () => {
  afterEach(() => { vi.useRealTimers() })
  it.each([
    ['queue-congested', 'Waiting for the execution queue'], ['oracle-unavailable', 'Waiting for a usable oracle price'],
    ['pending-orders', 'Waiting for your pending orders'], ['operator-required', 'Operator review required'],
    ['execution-disabled', 'Automatic execution is paused'], ['check-failed', 'Execution check did not complete'],
    ['retry-ready', 'Preparing a retry'], ['queue-cleanup', 'Clearing an expired queue entry'],
  ] as const)('explains %s without offering an unsafe action', (reason, title) => {
    render(<ProtectionExecutionNotice protection={protection} report={report(reason)} />)
    expect(screen.getByRole('status')).toHaveTextContent(title)
    expect(screen.queryByRole('button', { name: /Retry close/ })).not.toBeInTheDocument()
  })
  it('expires a visible retry-ready report without a successful refetch', () => {
    vi.useFakeTimers()
    render(<ProtectionExecutionNotice protection={protection} report={report('retry-ready')} />)
    expect(screen.getByText('Preparing a retry')).toBeInTheDocument()
    act(() => { vi.advanceTimersByTime(60_000) })
    expect(screen.getByText('Execution status is out of date')).toBeInTheDocument()
    expect(screen.queryByText('Preparing a retry')).not.toBeInTheDocument()
  })
  it('does not keep a pending-transaction message after the close attempt changes', () => {
    const execution = report('retry-ready')
    if (!execution.observation) throw new Error('Missing fixture')
    execution.observation.transactionHash = '0x' + 'cd'.repeat(32)
    execution.observation.transactionAction = 'retry'
    const view = render(<ProtectionExecutionNotice protection={protection} report={execution} />)
    expect(screen.getByText('Retry awaiting confirmation')).toBeInTheDocument()
    expect(screen.getByRole('link', { name: /View pending transaction/ })).toHaveAttribute('rel', 'noopener noreferrer')
    view.rerender(<ProtectionExecutionNotice protection={{ ...protection, linkedOrderId: 20n }} report={execution} />)
    expect(screen.queryByRole('link')).not.toBeInTheDocument()
    expect(screen.getByText('Execution status is out of date')).toBeInTheDocument()
  })
  it('distinguishes loading, missing status and refresh errors', () => {
    const refresh = vi.fn()
    const view = render(<ProtectionExecutionNotice protection={protection} loading />)
    expect(screen.getByRole('status')).toHaveTextContent('Checking automatic execution')
    view.rerender(<ProtectionExecutionNotice protection={protection} onRefresh={refresh} />)
    expect(screen.getByRole('status')).toHaveTextContent('Automatic execution status unavailable')
    fireEvent.click(screen.getByRole('button', { name: 'Refresh execution status' }))
    expect(refresh).toHaveBeenCalledOnce()
    view.rerender(<ProtectionExecutionNotice protection={protection} report={report('retry-ready')} error />)
    expect(screen.getByRole('status')).toHaveTextContent('Unable to refresh execution status')
    expect(screen.queryByText('Preparing a retry')).not.toBeInTheDocument()
  })
})
