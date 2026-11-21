import { act, fireEvent, render, screen, waitFor } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { parseEther, toHex, zeroAddress, type Hex } from 'viem'
import { entryPoint09Address } from 'viem/account-abstraction'
import { KernelAttemptRecoveryControls } from '../KernelAttemptRecoveryControls'
import type { KernelOperationAttempt } from '../kernelOperationJournal'
import type { KernelPreparedCancellation, KernelPreparedOperation } from '../kernelRpc'

const { submit, serverNow, refreshClock } = vi.hoisted(() => ({ submit: vi.fn(), serverNow: vi.fn(), refreshClock: vi.fn() }))

vi.mock('../deadlineClock', () => ({ deadlineNow: serverNow, refreshDeadlineClock: refreshClock }))

vi.mock('../kernelOperationJournal', () => ({
  isKernelAttemptPending: () => true,
  kernelAttemptCanCancel: (attempt: KernelOperationAttempt) => attempt.phase === 'sending',
  kernelAttemptCanDiscard: (attempt: KernelOperationAttempt, now: number | undefined) =>
    attempt.phase === 'signed' && now !== undefined && now >= attempt.prepared.expiresAt * 1_000,
  kernelAttemptCanRetry: (attempt: KernelOperationAttempt, now: number | undefined) =>
    attempt.phase === 'signed' && now !== undefined && now < attempt.prepared.expiresAt * 1_000,
  kernelCancellationCanDiscard: () => false,
  kernelCancellationCanRetry: () => false,
  signAndSubmitKernelCancellation: submit,
}))

const sender = '0x0000000000000000000000000000000000000016' as const
const signature = `0x${'a1'.repeat(65)}` as Hex
const original: KernelPreparedOperation = {
  version: 1,
  preparationId: toHex(2n, { size: 32 }),
  mode: 'eth',
  entryPoint: entryPoint09Address,
  operation: {
    sender, nonce: 2n, callData: '0x1234', signature: '0x',
    callGasLimit: 40_000n, verificationGasLimit: 75_000n, preVerificationGas: 50_000n,
    maxFeePerGas: 1_000_000n, maxPriorityFeePerGas: 1_000n,
  },
  userOperationHash: toHex(3n, { size: 32 }),
  maxNetworkGasWei: 165_000_000_000n,
  expiresAt: 1_800_000_000,
  recoveryToken: 'test-credential',
}
const attempt: KernelOperationAttempt = {
  version: 1,
  scope: { deployment: 'kernel-test', chainId: 421614, entryPoint: entryPoint09Address, sender },
  ownerAddress: '0x0000000000000000000000000000000000000017',
  action: 'place-order', prepared: original, signature, nonceKey: 0n,
  expectedPaymaster: zeroAddress, phase: 'sending', createdAt: 0, updatedAt: 0,
}

describe('Kernel cancellation review', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    serverNow.mockReturnValue((original.expiresAt - 10) * 1_000)
    refreshClock.mockResolvedValue({ now: serverNow })
  })

  it('displays the ETH quote before approving the wallet signature', async () => {
    const cancellation: KernelPreparedCancellation = {
      ...original,
      mode: 'eth',
      cancelsUserOperationHash: original.userOperationHash,
      userOperationHash: toHex(4n, { size: 32 }),
      maxNetworkGasWei: parseEther('0.004'),
      operation: { ...original.operation, factory: sender, factoryData: '0x1234' },
    }
    const quoteCancellation = vi.fn(async () => cancellation)
    const signPrepared = vi.fn(async () => signature)
    submit.mockImplementationOnce(async () => signPrepared())
    render(<KernelAttemptRecoveryControls attempt={attempt} quoteCancellation={quoteCancellation} signPrepared={signPrepared} />)

    fireEvent.click(screen.getByRole('button', { name: 'Review cancellation' }))
    expect(await screen.findByText('0.004 ETH')).toBeInTheDocument()
    expect(screen.getByText('The quote includes deployment of this Trading Account.')).toBeInTheDocument()
    expect(quoteCancellation).toHaveBeenCalledExactlyOnceWith(original, signature)
    expect(signPrepared).not.toHaveBeenCalled()

    fireEvent.click(screen.getByRole('button', { name: 'Approve cancellation' }))
    await waitFor(() => expect(signPrepared).toHaveBeenCalledTimes(1))
    expect(submit).toHaveBeenCalledExactlyOnceWith(attempt, cancellation, signPrepared)
  })

  it('refreshes missing server time and switches from resend to discard at expiry', async () => {
    vi.useFakeTimers()
    try {
      const clockReady = Promise.withResolvers<{ now: typeof serverNow }>()
      serverNow.mockImplementation(() => { throw new Error('Server time is unavailable') })
      refreshClock.mockReturnValueOnce(clockReady.promise)
      render(<KernelAttemptRecoveryControls attempt={{ ...attempt, phase: 'signed' }} />)
      expect(screen.getByRole('button', { name: 'Refresh quote status' })).toBeInTheDocument()
      expect(screen.queryByRole('button', { name: 'Resend signed operation' })).not.toBeInTheDocument()

      await act(async () => {
        serverNow.mockReturnValue((original.expiresAt - 1) * 1_000)
        clockReady.resolve({ now: serverNow })
      })
      expect(screen.getByRole('button', { name: 'Resend signed operation' })).toBeInTheDocument()
      act(() => {
        serverNow.mockReturnValue(original.expiresAt * 1_000)
        vi.advanceTimersByTime(1_000)
      })
      expect(screen.queryByRole('button', { name: 'Resend signed operation' })).not.toBeInTheDocument()
      expect(screen.getByRole('button', { name: 'Discard unsent operation' })).toBeInTheDocument()
    } finally {
      vi.useRealTimers()
    }
  })
})
