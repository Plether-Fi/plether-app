import { act, renderHook } from '@testing-library/react'
import { Result } from 'better-result'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { PERPS_ARBITRUM_SEPOLIA } from '../../contracts/perpsAddresses'

const account = '0x2222222222222222222222222222222222222222'
const otherAccount = '0x3333333333333333333333333333333333333333'
const amount = 100_000_000_000n
const mocks = vi.hoisted(() => ({
  claim: vi.fn(), receipt: vi.fn(), balance: vi.fn(), deposit: vi.fn(),
  identity: {
    accountAddress: '0x2222222222222222222222222222222222222222',
    ownerAddress: '0x1111111111111111111111111111111111111111',
    chainId: 421614, status: 'ready', isAaManifestConfigured: true,
  },
}))
vi.mock('../../api', () => ({
  perpsApi: { claimTestnetFaucet: mocks.claim },
  testnetFaucetErrorMessage: () => 'Faucet unavailable. Try again.',
}))
vi.mock('wagmi', () => ({
  usePublicClient: () => ({ waitForTransactionReceipt: mocks.receipt, readContract: mocks.balance }),
}))
vi.mock('../../perps-aa', () => ({ usePerpsIdentity: () => mocks.identity }))
vi.mock('../usePerpsTrading', () => ({ usePerpsTrading: () => ({ depositMargin: mocks.deposit }) }))

import { useTestnetFunding } from '../useTestnetFunding'

function funding(status = 'submitted', overrides = {}) {
  return Result.ok({ data: {
    address: account, amount: amount.toString(), token: PERPS_ARBITRUM_SEPOLIA.usdc,
    txHash: `0x${'a'.repeat(64)}`, status, ...overrides,
  } })
}

beforeEach(() => {
  vi.useFakeTimers()
  vi.resetAllMocks()
  mocks.identity.accountAddress = account
  mocks.identity.status = 'ready'
  mocks.claim.mockResolvedValue(funding())
  mocks.receipt.mockResolvedValue({ status: 'success' })
  mocks.balance.mockResolvedValue(amount)
  mocks.deposit.mockResolvedValue('0xdeposit')
})
afterEach(() => { vi.useRealTimers() })

describe('automatic testnet funding', () => {
  it('waits for the receipt and arriving balance before depositing from the Trading Account exactly once', async () => {
    let confirm!: (value: { status: string }) => void
    mocks.receipt.mockReturnValue(new Promise((resolve) => { confirm = resolve }))
    mocks.balance.mockResolvedValueOnce(0n).mockResolvedValue(amount)
    const { result } = renderHook(() => useTestnetFunding(account, true))
    let pending!: Promise<void>
    await act(async () => { pending = result.current.requestFunds(account) })
    expect(result.current.phase).toBe('waiting')
    expect(mocks.balance).not.toHaveBeenCalled()
    expect(mocks.deposit).not.toHaveBeenCalled()
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.claim).toHaveBeenCalledTimes(1)
    await act(async () => { confirm({ status: 'success' }) })
    expect(mocks.deposit).not.toHaveBeenCalled()
    await act(async () => { await vi.advanceTimersByTimeAsync(2000); await pending })
    expect(mocks.balance).toHaveBeenCalledWith(expect.objectContaining({ functionName: 'balanceOf', args: [account] }))
    expect(mocks.deposit).toHaveBeenCalledExactlyOnceWith(amount, undefined, 'account')
    expect(result.current.phase).toBe('deposited')
    expect(result.current.claim?.status).toBe('minted')
  })

  it.each(['already_claimed', 'already_funded'])('deposits only the remaining funds for %s', async (status) => {
    mocks.claim.mockResolvedValue(funding(status, { txHash: null }))
    mocks.balance.mockResolvedValue(50n)
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.receipt).not.toHaveBeenCalled()
    expect(mocks.deposit).toHaveBeenCalledExactlyOnceWith(50n, undefined, 'account')
  })

  it('does not deposit unrelated funds above the faucet amount', async () => {
    mocks.balance.mockResolvedValue(amount * 2n)
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.deposit).toHaveBeenCalledExactlyOnceWith(amount, undefined, 'account')
  })

  it('stops waiting after a timeout and leaves an actionable error', async () => {
    mocks.balance.mockResolvedValue(0n)
    const { result } = renderHook(() => useTestnetFunding(account, true))
    let pending!: Promise<void>
    await act(async () => { pending = result.current.requestFunds(account) })
    await act(async () => { await vi.advanceTimersByTimeAsync(120_000); await pending })
    expect(result.current.error).toContain('Check your Margin Account')
    expect(result.current.phase).toBe('idle')
    expect(mocks.deposit).not.toHaveBeenCalled()
  })

  it('does not deposit after a reverted faucet transaction', async () => {
    mocks.receipt.mockResolvedValue({ status: 'reverted' })
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(result.current.error).toContain('reverted')
    expect(mocks.deposit).not.toHaveBeenCalled()
  })

  it.each(['disconnect', 'account change', 'unmount'])('cancels the automatic deposit on %s while waiting', async (change) => {
    let confirm!: (value: { status: string }) => void
    mocks.receipt.mockReturnValue(new Promise((resolve) => { confirm = resolve }))
    const { result, rerender, unmount } = renderHook(
      ({ recipient, enabled }) => useTestnetFunding(recipient, enabled),
      { initialProps: { recipient: account, enabled: true } },
    )
    let pending!: Promise<void>
    await act(async () => { pending = result.current.requestFunds(account) })
    if (change === 'unmount') unmount()
    else if (change === 'disconnect') rerender({ recipient: account, enabled: false })
    else {
      mocks.identity.accountAddress = otherAccount
      rerender({ recipient: otherAccount, enabled: true })
    }
    await act(async () => { confirm({ status: 'success' }); await pending })
    expect(mocks.deposit).not.toHaveBeenCalled()
  })

  it('keeps a rejected deposit recoverable without retrying it automatically', async () => {
    mocks.deposit.mockRejectedValue(new Error('User rejected the request.'))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(result.current.error).toBe('User rejected the request.')
    expect(result.current.phase).toBe('idle')
    await act(async () => { await vi.advanceTimersByTimeAsync(120_000) })
    expect(mocks.deposit).toHaveBeenCalledTimes(1)
    mocks.deposit.mockResolvedValue('0xdeposit')
    await act(async () => { await result.current.requestFunds(account) })
    expect(result.current.phase).toBe('deposited')
  })

  it('surfaces faucet failures without attempting a deposit', async () => {
    mocks.claim.mockResolvedValue(Result.err(new Error('unavailable')))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(result.current.error).toBe('Faucet unavailable. Try again.')
    expect(mocks.deposit).not.toHaveBeenCalled()
  })

  it('rejects a faucet response for another account', async () => {
    mocks.claim.mockResolvedValue(funding('minted', { address: otherAccount }))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(result.current.error).toContain('does not match')
    expect(mocks.deposit).not.toHaveBeenCalled()
  })
})
