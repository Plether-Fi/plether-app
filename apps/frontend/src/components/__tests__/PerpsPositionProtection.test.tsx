import { beforeEach, describe, expect, it, vi } from 'vitest'
import { fireEvent, render, screen, waitFor } from '@testing-library/react'
import { PerpsPositionProtection, PerpsPositionProtectionPanel } from '../PerpsPositionProtection'

const mocks = vi.hoisted(() => ({
  simulate: vi.fn(), write: vi.fn(), receipt: vi.fn(), read: vi.fn(), verify: vi.fn(),
  account: { address: '0x0000000000000000000000000000000000000001', chainId: 421614 },
}))
vi.mock('wagmi', () => ({
  useAccount: () => mocks.account,
  usePublicClient: () => ({ getBlockNumber: async () => 123n,
    simulateContract: mocks.simulate, waitForTransactionReceipt: mocks.receipt, readContract: mocks.read }),
  useWriteContract: () => ({ writeContractAsync: mocks.write }),
}))
vi.mock('../../contracts/verifyPerpsV2Bindings', () => ({ verifyProtectionRetryBindings: mocks.verify }))

describe('position protection status and retry', () => {
  it('shows both trigger prices in display-price units without expanding details', () => {
    render(<PerpsPositionProtection id={7n} status={2} linkedOrderId={0n}
      takeProfitTriggerPrice={95_000_000n} stopLossTriggerPrice={100_000_000n} onRefresh={vi.fn()} />)
    expect(screen.getByLabelText('Stop-loss trigger price')).toHaveTextContent('1.0000')
    expect(screen.getByLabelText('Take-profit trigger price')).toHaveTextContent('1.0500')
    expect(screen.getByLabelText('Stop-loss trigger price')).toBeVisible()
    expect(screen.getByRole('button', { name: 'Details' })).toHaveAttribute('aria-expanded', 'false')
  })

  it('shows only the configured leg for one-sided protection', () => {
    const { rerender } = render(<PerpsPositionProtectionPanel id={7n} status={2} linkedOrderId={0n}
      stopLossTriggerPrice={100_000_000n} takeProfitTriggerPrice={0n} onRetry={vi.fn()} />)
    expect(screen.getByLabelText('Stop-loss trigger price')).toHaveTextContent('1.0000')
    expect(screen.queryByLabelText('Take-profit trigger price')).toBeNull()
    expect(screen.queryByText('TP')).toBeNull()
    rerender(<PerpsPositionProtectionPanel id={7n} status={2} linkedOrderId={0n}
      stopLossTriggerPrice={0n} takeProfitTriggerPrice={95_000_000n} onRetry={vi.fn()} />)
    expect(screen.queryByLabelText('Stop-loss trigger price')).toBeNull()
    expect(screen.queryByText('SL')).toBeNull()
    expect(screen.getByLabelText('Take-profit trigger price')).toHaveTextContent('1.0500')
  })

  it('keeps unavailable trigger prices distinct from unset legs', () => {
    render(<PerpsPositionProtectionPanel id={7n} status={2} linkedOrderId={0n} onRetry={vi.fn()} />)
    expect(screen.getByLabelText('Stop-loss trigger price')).toHaveTextContent('--')
    expect(screen.getByLabelText('Take-profit trigger price')).toHaveTextContent('--')
  })

  it.each([
    [1, 'Pending open', 'not yet armed'],
    [2, 'Armed', 'Cancel it'],
    [4, 'Executed', 'completed its close execution'],
    [5, 'Failed', 'ended in failure'],
    [6, 'Cancelled', 'was cancelled'],
    [7, 'Liquidated', 'was liquidated'],
  ] as const)('shows lifecycle status %s without offering a retry', (status, label, message) => {
    render(<PerpsPositionProtectionPanel id={7n} status={status} linkedOrderId={0n} canRetry walletOnNetwork onRetry={vi.fn()} />)
    expect(screen.getByText(label)).toBeVisible()
    fireEvent.click(screen.getByRole('button', { name: 'Details' }))
    expect(screen.getByText(new RegExp(message))).toBeVisible()
    expect(screen.queryByRole('button', { name: 'Retry protection close' })).toBeNull()
    if (status >= 4) expect(screen.queryByText(/remain locked|is active/)).toBeNull()
  })

  it('keeps the position uncluttered when no protection is attached', () => {
    const { container } = render(<PerpsPositionProtectionPanel id={0n} status={0} linkedOrderId={0n} onRetry={vi.fn()} />)
    expect(container).toBeEmptyDOMElement()
  })

  beforeEach(() => {
    vi.clearAllMocks()
    mocks.account.chainId = 421614
    mocks.verify.mockResolvedValue(undefined)
    mocks.simulate.mockResolvedValue({ request: {} })
    mocks.write.mockResolvedValue('0xhash')
    mocks.receipt.mockResolvedValue({ status: 'success' })
    mocks.read.mockResolvedValue({ linkedOrderId: 19n })
  })

  it('shows the irreversible latch and queues a fresh nonpayable attempt', async () => {
    const refresh = vi.fn().mockResolvedValue(undefined)
    render(<PerpsPositionProtection id={7n} status={8} linkedOrderId={11n} onRefresh={refresh} />)
    expect(screen.getByText('Waiting to retry')).toBeVisible()
    expect(screen.getByText(/cannot be cancelled/)).not.toBeVisible()
    expect(screen.queryByRole('button', { name: 'Retry protection close' })).toBeNull()
    fireEvent.click(screen.getByRole('button', { name: 'Details & retry' }))
    expect(screen.getByText(/cannot be cancelled/)).toBeVisible()
    fireEvent.click(screen.getByRole('button', { name: 'Retry protection close' }))
    await screen.findByText('Close attempt #19 queued.')
    expect(mocks.verify).toHaveBeenCalledWith(expect.anything(), 123n)
    expect(mocks.write).toHaveBeenCalledWith(expect.objectContaining({
      functionName: 'retryPositionProtectionClose', args: [7n], chainId: 421614,
    }))
    expect(mocks.write.mock.calls[0][0]).not.toHaveProperty('value')
    expect(refresh).toHaveBeenCalled()
  })

  it('does not expose retry while a close attempt is live', () => {
    render(<PerpsPositionProtection id={7n} status={3} linkedOrderId={11n} onRefresh={vi.fn()} />)
    expect(screen.getByText('Close queued')).toBeVisible()
    fireEvent.click(screen.getByRole('button', { name: 'Details' }))
    expect(screen.queryByRole('button', { name: 'Retry protection close' })).toBeNull()
    expect(screen.getByText(/market close is queued/)).toBeVisible()
  })

  it('requires the correct wallet network and handles permissionless races', async () => {
    mocks.account.chainId = 1
    const refresh = vi.fn().mockResolvedValue(undefined)
    const { rerender } = render(<PerpsPositionProtection id={7n} status={8} linkedOrderId={11n} onRefresh={refresh} />)
    fireEvent.click(screen.getByRole('button', { name: 'Details & retry' }))
    expect(screen.getByRole('button', { name: 'Retry protection close' })).toBeDisabled()
    mocks.account.chainId = 421614
    mocks.simulate.mockRejectedValueOnce(new Error('Protection is no longer latched'))
    rerender(<PerpsPositionProtection id={7n} status={8} linkedOrderId={11n} onRefresh={refresh} />)
    fireEvent.click(screen.getByRole('button', { name: 'Retry protection close' }))
    await screen.findByRole('alert')
    expect(mocks.write).not.toHaveBeenCalled()
    await waitFor(() => expect(refresh).toHaveBeenCalled())
  })

  it('blocks an incompatible deployment before requesting a signature', async () => {
    mocks.verify.mockRejectedValueOnce(new Error('V3 schema required'))
    render(<PerpsPositionProtection id={7n} status={8} linkedOrderId={11n} onRefresh={vi.fn()} />)
    fireEvent.click(screen.getByRole('button', { name: 'Details & retry' }))
    fireEvent.click(screen.getByRole('button', { name: 'Retry protection close' }))
    await screen.findByText('V3 schema required')
    expect(mocks.simulate).not.toHaveBeenCalled()
    expect(mocks.write).not.toHaveBeenCalled()
  })
})
