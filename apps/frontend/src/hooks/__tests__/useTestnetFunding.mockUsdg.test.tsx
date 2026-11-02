import { act, renderHook } from '@testing-library/react'
import { Result } from 'better-result'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import type { TestnetFaucetClaim } from '../../api/types'

const mocks = vi.hoisted(() => ({
  claim: vi.fn(), mint: vi.fn(), receipt: vi.fn(), balance: vi.fn(), deposit: vi.fn(),
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
  perpsApi: { claimTestnetFaucet: mocks.claim },
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
vi.mock('../usePerpsTrading', () => ({ usePerpsTrading: () => ({ depositMargin: mocks.deposit }) }))
import { useTestnetFunding } from '../useTestnetFunding'

beforeEach(() => {
  vi.resetAllMocks()
  mocks.identity.status = 'ready'
  mocks.identity.chainId = 421614
  mocks.claim.mockResolvedValue(faucetResponse())
  mocks.receipt.mockResolvedValue({ status: 'success' })
  mocks.balance.mockResolvedValue(amount)
  mocks.deposit.mockResolvedValue('0xdeposit')
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
