import { act, renderHook } from '@testing-library/react'
import { Result } from 'better-result'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import type { TestnetFaucetClaim } from '../../api/types'

const mocks = vi.hoisted(() => ({
  start: vi.fn(), status: vi.fn(), refresh: vi.fn(), claim: vi.fn(), mint: vi.fn(), receipt: vi.fn(), balance: vi.fn(), deposit: vi.fn(),
  identity: {
    accountAddress: '0x2222222222222222222222222222222222222222',
    ownerAddress: '0x1111111111111111111111111111111111111111',
    chainId: 421614, status: 'ready', isAaManifestConfigured: true, sponsorshipEnabled: true,
    manifest: {
      settlementToken: '0x3333333333333333333333333333333333333333',
      marginClearinghouse: '0x4444444444444444444444444444444444444444',
      settlementTokenSupportsEip3009: false,
    },
  },
}))
const account = mocks.identity.accountAddress as `0x${string}`
const amount = 100_000_000_000n
const hash = `0x${'a'.repeat(64)}`
const faucetResponse = (overrides: Partial<TestnetFaucetClaim> = {}, chainId = 421614) => Result.ok({
  data: {
    address: account, token: mocks.identity.manifest.settlementToken,
    amount: amount.toString(), txHash: hash, status: 'submitted' as const, ...overrides,
  },
  meta: { chainId, blockNumber: 1, cached: false },
})

vi.mock('../../api', () => ({
  perpsApi: { claimTestnetFaucet: mocks.claim, claimMarginFunding: mocks.start, getMarginFunding: mocks.status },
  testnetFaucetErrorMessage: (error: { message: string }) => error.message,
}))
vi.mock('wagmi', () => ({
  useWriteContract: () => ({ writeContractAsync: mocks.mint }),
  usePublicClient: () => ({ waitForTransactionReceipt: mocks.receipt, readContract: mocks.balance }),
}))
vi.mock('../../contracts/perpsAddresses', () => ({
  PERPS_RELEASE_IS_DEPLOYED: true,
  PERPS_ARBITRUM_SEPOLIA_CHAIN_ID: 421614,
  PERPS_ARBITRUM_SEPOLIA: { settlementToken: '0x3333333333333333333333333333333333333333', marginClearinghouse: '0x4444444444444444444444444444444444444444' },
  PERPS_SETTLEMENT_TOKEN: { name: 'Mock USDG', symbol: 'USDG', decimals: 6, isMock: true },
  PERPS_SETTLEMENT_TOKEN_LABEL: 'USDG',
}))
vi.mock('../../perps-aa', () => ({ usePerpsIdentity: () => mocks.identity }))
vi.mock('../usePerpsTrading', () => ({ usePerpsTrading: () => ({ depositMargin: mocks.deposit, refreshAccount: mocks.refresh }) }))
import { useTestnetFunding } from '../useTestnetFunding'

beforeEach(() => {
  vi.resetAllMocks()
  mocks.identity.status = 'ready'
  mocks.identity.chainId = 421614
  mocks.start.mockResolvedValue(Result.ok({ data: { version: 1, destination: 'wallet', status: 'legacy_wallet', address: account, token: mocks.identity.manifest.settlementToken }, meta: { chainId: 421614 } }))
  mocks.claim.mockResolvedValue(faucetResponse())
  mocks.receipt.mockResolvedValue({ status: 'success' })
  mocks.balance.mockResolvedValue(amount)
  mocks.deposit.mockResolvedValue('0xdeposit')
})
afterEach(() => vi.useRealTimers())

const fundingId = 'a6415ba2-5668-4ba5-a058-f627bd1b938d'
function marginResponse(status = 'pending', phase = 'mint', overrides = {}) {
  return Result.ok({
    data: {
      version: 1, destination: 'margin', claimId: fundingId, address: account,
      token: mocks.identity.manifest.settlementToken,
      clearinghouse: mocks.identity.manifest.marginClearinghouse,
      amount: amount.toString(), phase, status, reason: null,
      depositTxHash: status === 'funded' ? hash : null, ...overrides,
    },
    meta: { chainId: 421614, blockNumber: 1, cached: false },
  })
}

describe('direct margin funding', () => {
  it('follows all three backend phases without a wallet mint or sponsored user deposit', async () => {
    vi.useFakeTimers()
    mocks.start.mockResolvedValue(marginResponse())
    mocks.status.mockResolvedValueOnce(marginResponse('pending', 'approval'))
      .mockResolvedValueOnce(marginResponse('pending', 'deposit'))
      .mockResolvedValueOnce(marginResponse('funded', 'deposit'))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    let pending!: Promise<void>
    await act(async () => { pending = result.current.requestFunds(account) })
    expect(mocks.start).toHaveBeenCalledExactlyOnceWith(mocks.identity.ownerAddress, account)
    expect(result.current.phase).toBe('claiming')
    await act(async () => { await vi.advanceTimersByTimeAsync(5_000) })
    expect(result.current.phase).toBe('approving')
    await act(async () => { await vi.advanceTimersByTimeAsync(5_000) })
    expect(result.current.phase).toBe('depositing')
    await act(async () => { await vi.advanceTimersByTimeAsync(5_000); await pending })
    expect(result.current.phase).toBe('deposited')
    expect(result.current.claim).toMatchObject({ destination: 'margin', status: 'margin_funded', txHash: hash })
    expect(mocks.refresh).toHaveBeenCalledOnce()
    expect(mocks.status).toHaveBeenCalledTimes(3)
    expect(mocks.claim).not.toHaveBeenCalled()
    expect(mocks.mint).not.toHaveBeenCalled()
    expect(mocks.deposit).not.toHaveBeenCalled()
    expect(mocks.receipt).not.toHaveBeenCalled()
    expect(mocks.balance).not.toHaveBeenCalled()
  })

  it('recovers completed funding after reload without another deposit', async () => {
    mocks.start.mockResolvedValue(marginResponse('funded', 'deposit'))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.start).toHaveBeenCalledOnce()
    expect(mocks.deposit).not.toHaveBeenCalled()
    expect(result.current.phase).toBe('deposited')
  })

  it('resumes a known failed phase only after an explicit retry', async () => {
    mocks.start.mockResolvedValueOnce(marginResponse('failed', 'deposit'))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(result.current.error).toContain('resume the same allocation')
    expect(mocks.status).not.toHaveBeenCalled()
    mocks.start.mockResolvedValue(marginResponse('funded', 'deposit'))
    await act(async () => { await result.current.requestFunds(account) })
    expect(result.current.phase).toBe('deposited')
    expect(mocks.claim).not.toHaveBeenCalled()
  })

  it('does not fall back to a wallet mint when the direct endpoint is unavailable', async () => {
    mocks.start.mockResolvedValue(Result.err(new Error('Direct funding unavailable.')))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.claim).not.toHaveBeenCalled()
    expect(mocks.deposit).not.toHaveBeenCalled()
    expect(result.current.phase).toBe('idle')
  })

  it.each([
    { address: '0x5555555555555555555555555555555555555555' },
    { token: '0x5555555555555555555555555555555555555555' },
    { clearinghouse: '0x5555555555555555555555555555555555555555' },
    { amount: '200000000000' }, { claimId: 'invalid' }, { depositTxHash: null },
  ])('rejects an invalid credit response %j', async override => {
    mocks.start.mockResolvedValue(marginResponse('funded', 'deposit', override))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(result.current.phase).toBe('idle')
    expect(result.current.error).toContain('could not be verified')
    expect(mocks.refresh).not.toHaveBeenCalled()
    expect(mocks.deposit).not.toHaveBeenCalled()
  })

  it('checks the beneficiary again when polling a previously verified claim', async () => {
    vi.useFakeTimers()
    mocks.start.mockResolvedValue(marginResponse())
    mocks.status.mockResolvedValue(marginResponse('funded', 'deposit', { address: '0x5555555555555555555555555555555555555555' }))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    let pending!: Promise<void>
    await act(async () => { pending = result.current.requestFunds(account) })
    await act(async () => { await vi.advanceTimersByTimeAsync(5_000); await pending })
    expect(result.current.phase).toBe('idle')
    expect(mocks.refresh).not.toHaveBeenCalled()
  })

  it('stops polling after disconnect while backend funding continues independently', async () => {
    vi.useFakeTimers()
    mocks.start.mockResolvedValue(marginResponse())
    const { result, rerender } = renderHook(({ enabled }) => useTestnetFunding(account, enabled), { initialProps: { enabled: true } })
    let pending!: Promise<void>
    await act(async () => { pending = result.current.requestFunds(account) })
    rerender({ enabled: false })
    await act(async () => { await pending; await vi.advanceTimersByTimeAsync(10_000) })
    expect(mocks.status).not.toHaveBeenCalled()
    expect(mocks.refresh).not.toHaveBeenCalled()
  })
})

describe('backend MockUSDG testnet funding', () => {
  it('requests a backend mint to the canonical account, confirms it and deposits without a wallet mint', async () => {
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.claim).toHaveBeenCalledExactlyOnceWith(account)
    expect(mocks.mint).not.toHaveBeenCalled()
    expect(mocks.receipt).toHaveBeenCalledWith({ hash, timeout: 120_000 })
    expect(mocks.deposit).toHaveBeenCalledExactlyOnceWith(amount, undefined, 'account')
    expect(result.current.phase).toBe('deposited')
  })

  it('does not request or deposit another allocation after a completed deposit', async () => {
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.claim).toHaveBeenCalledTimes(1)
    expect(mocks.deposit).toHaveBeenCalledTimes(1)
  })

  it('uses a backend-confirmed mint without requiring a historical receipt from the browser RPC', async () => {
    mocks.claim.mockResolvedValue(faucetResponse({ status: 'minted' }))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.receipt).not.toHaveBeenCalled()
    expect(result.current.phase).toBe('deposited')
  })

  it('recovers the backend claim after a rejected deposit', async () => {
    mocks.deposit.mockRejectedValueOnce(new Error('User rejected deposit'))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(result.current.error).toContain('You declined the request in your wallet.')
    mocks.claim.mockResolvedValue(faucetResponse({ status: 'already_claimed' }))
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.claim).toHaveBeenCalledTimes(2)
    expect(mocks.receipt).toHaveBeenCalledTimes(1)
    expect(mocks.deposit).toHaveBeenCalledTimes(2)
    expect(mocks.mint).not.toHaveBeenCalled()
    expect(result.current.phase).toBe('deposited')
  })

  it.each(['already_claimed', 'already_funded'] as const)('deposits only the remaining balance for an %s allocation', async status => {
    mocks.claim.mockResolvedValue(faucetResponse({ status, txHash: null }))
    mocks.balance.mockResolvedValue(50n)
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.deposit).toHaveBeenCalledExactlyOnceWith(50n, undefined, 'account')
    expect(result.current.phase).toBe('deposited')
  })

  it('explains consumed funding without depositing again after an uncertain deposit', async () => {
    mocks.deposit.mockRejectedValueOnce(new Error('Deposit confirmation timed out'))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    mocks.claim.mockResolvedValue(faucetResponse({ status: 'already_claimed' }))
    mocks.balance.mockResolvedValue(0n)
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.deposit).toHaveBeenCalledTimes(1)
    expect(result.current.error).toContain('Check your Margin Account and account activity.')
  })

  it('rejects a different recipient before calling the faucet', async () => {
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds('0x5555555555555555555555555555555555555555') })
    expect(result.current.error).toContain('canonical Plether Trading Account')
    expect(mocks.claim).not.toHaveBeenCalled()
    expect(mocks.deposit).not.toHaveBeenCalled()
  })

  it('does not call the faucet for another chain', async () => {
    mocks.identity.chainId = 1
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.claim).not.toHaveBeenCalled()
    expect(result.current.error).toContain('funding is unavailable')
  })

  it.each(['recipient', 'token', 'chain'] as const)('rejects a mismatched faucet response %s before depositing', async mismatch => {
    const other = '0x5555555555555555555555555555555555555555'
    mocks.claim.mockResolvedValue(faucetResponse({
      ...(mismatch === 'recipient' ? { address: other } : {}),
      ...(mismatch === 'token' ? { token: other } : {}),
    }, mismatch === 'chain' ? 1 : 421614))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(result.current.error).toContain('does not match this Trading Account')
    expect(result.current.claim).toBeNull()
    expect(mocks.receipt).not.toHaveBeenCalled()
    expect(mocks.deposit).not.toHaveBeenCalled()
  })

  it('shows a backend failure and leaves the request retryable without opening the wallet', async () => {
    mocks.claim.mockResolvedValueOnce(Result.err(new Error('The faucet is temporarily unavailable.')))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(result.current.error).toContain('The faucet is temporarily unavailable.')
    expect(mocks.deposit).not.toHaveBeenCalled()
    expect(mocks.mint).not.toHaveBeenCalled()
    await act(async () => { await result.current.requestFunds(account) })
    expect(result.current.phase).toBe('deposited')
  })

  it('does not deposit after a reverted mint and lets the backend recover on retry', async () => {
    mocks.receipt.mockResolvedValueOnce({ status: 'reverted' })
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(result.current.error).toContain('reverted')
    expect(mocks.deposit).not.toHaveBeenCalled()
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.claim).toHaveBeenCalledTimes(2)
    expect(result.current.phase).toBe('deposited')
  })
})

describe('backend MockUSDG pending confirmation', () => {
  it('rechecks the durable claim after receipt timeout before depositing', async () => {
    mocks.receipt.mockRejectedValueOnce(new Error('RPC timeout'))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(result.current.claim?.status).toBe('submitted')
    expect(result.current.error).toContain('Your claim is saved')
    expect(mocks.deposit).not.toHaveBeenCalled()
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.claim).toHaveBeenCalledTimes(2)
    expect(mocks.receipt).toHaveBeenNthCalledWith(2, { hash, timeout: 120_000 })
    expect(mocks.deposit).toHaveBeenCalledTimes(1)
  })

  it.each(['disabled', 'recipient', 'unmount'])('stops automatic deposit after %s while the faucet is pending', async change => {
    let claim!: (value: ReturnType<typeof faucetResponse>) => void
    mocks.claim.mockReturnValueOnce(new Promise(resolve => { claim = resolve }))
    const { result, rerender, unmount } = renderHook(
      ({ recipient, enabled }) => useTestnetFunding(recipient, enabled),
      { initialProps: { recipient: account, enabled: true } },
    )
    let pending!: Promise<void>
    await act(async () => { pending = result.current.requestFunds(account) })
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.claim).toHaveBeenCalledTimes(1)
    if (change === 'unmount') unmount()
    else rerender({ recipient: change === 'recipient' ? '0x5555555555555555555555555555555555555555' : account, enabled: change !== 'disabled' })
    await act(async () => { claim(faucetResponse()); await pending })
    expect(mocks.receipt).not.toHaveBeenCalled()
    expect(mocks.deposit).not.toHaveBeenCalled()
  })

  it('stops automatic deposit after disconnecting while confirmation is pending', async () => {
    let confirm!: (value: { status: string }) => void
    mocks.receipt.mockReturnValueOnce(new Promise(resolve => { confirm = resolve }))
    const { result, rerender } = renderHook(
      ({ enabled }) => useTestnetFunding(account, enabled),
      { initialProps: { enabled: true } },
    )
    let pending!: Promise<void>
    await act(async () => { pending = result.current.requestFunds(account) })
    rerender({ enabled: false })
    await act(async () => { confirm({ status: 'success' }); await pending })
    expect(mocks.deposit).not.toHaveBeenCalled()
  })
})
