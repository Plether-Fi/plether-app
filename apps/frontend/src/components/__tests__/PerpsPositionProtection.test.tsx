import { beforeEach, describe, expect, it, vi } from 'vitest'
import { fireEvent, render, screen, waitFor } from '@testing-library/react'
import { PerpsPositionProtection } from '../PerpsPositionProtection'

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
