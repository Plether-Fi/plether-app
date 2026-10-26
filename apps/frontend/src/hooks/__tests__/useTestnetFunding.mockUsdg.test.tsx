import { act, renderHook } from '@testing-library/react'
import { keccak256 } from 'viem'
import { beforeEach, describe, expect, it, vi } from 'vitest'

const mocks = vi.hoisted(() => ({
  claim: vi.fn(), mint: vi.fn(), code: vi.fn(), receipt: vi.fn(), balance: vi.fn(), deposit: vi.fn(),
  identity: {
    accountAddress: '0x2222222222222222222222222222222222222222',
    ownerAddress: '0x1111111111111111111111111111111111111111',
    chainId: 421614, status: 'ready', isAaManifestConfigured: true, sponsorshipEnabled: true,
    manifest: {
      usdc: '0x3333333333333333333333333333333333333333',
      marginClearinghouse: '0x4444444444444444444444444444444444444444',
      usdcSupportsEip3009: false,
    },
  },
}))
const account = mocks.identity.accountAddress as `0x${string}`
const amount = 100_000_000_000n
const hash = `0x${'a'.repeat(64)}`
vi.mock('../../api', () => ({ perpsApi: { claimTestnetFaucet: mocks.claim }, testnetFaucetErrorMessage: () => 'Faucet unavailable.' }))
vi.mock('wagmi', () => ({
  useWriteContract: () => ({ writeContractAsync: mocks.mint }),
  usePublicClient: () => ({ getCode: mocks.code, waitForTransactionReceipt: mocks.receipt, readContract: mocks.balance }),
}))
vi.mock('../../contracts/perpsAddresses', () => ({
  PERPS_ARBITRUM_SEPOLIA_CHAIN_ID: 421614,
  PERPS_ARBITRUM_SEPOLIA: { usdc: '0x3333333333333333333333333333333333333333', marginClearinghouse: '0x4444444444444444444444444444444444444444' },
  PERPS_SETTLEMENT_TOKEN: { name: 'Mock USDG', symbol: 'USDG', decimals: 6, isMock: true, supportsReceiveWithAuthorization: false, supportsBridgeFunding: false },
  PERPS_SETTLEMENT_TOKEN_LABEL: 'Mock USDG',
  PERPS_SETTLEMENT_TOKEN_CONTRACT: { runtimeCodeHash: keccak256('0x1234') },
}))
vi.mock('../../perps-aa', () => ({ usePerpsIdentity: () => mocks.identity }))
vi.mock('../usePerpsTrading', () => ({ usePerpsTrading: () => ({ depositMargin: mocks.deposit }) }))
import { useTestnetFunding } from '../useTestnetFunding'

beforeEach(() => {
  vi.clearAllMocks()
  mocks.identity.status = 'ready'
  mocks.code.mockResolvedValue('0x1234')
  mocks.mint.mockResolvedValue(hash)
  mocks.receipt.mockResolvedValue({ status: 'success' })
  mocks.balance.mockResolvedValue(amount)
  mocks.deposit.mockResolvedValue('0xdeposit')
})

describe('MockUSDG testnet funding', () => {
  it('mints to the canonical account, confirms and uses the ordinary balance deposit without a faucet service', async () => {
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.mint).toHaveBeenCalledExactlyOnceWith(expect.objectContaining({
      account: mocks.identity.ownerAddress, chainId: 421614,
      address: mocks.identity.manifest.usdc, functionName: 'mint', args: [account, amount],
    }))
    expect(mocks.receipt).toHaveBeenCalledWith({ hash, timeout: 120_000 })
    expect(mocks.deposit).toHaveBeenCalledExactlyOnceWith(amount, undefined, 'account')
    expect(mocks.claim).not.toHaveBeenCalled()
    expect(result.current.phase).toBe('deposited')
  })

  it('allows repeated minting after each completed deposit', async () => {
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.mint).toHaveBeenCalledTimes(2)
    expect(mocks.deposit).toHaveBeenCalledTimes(2)
  })

  it('reuses the confirmed mint after a rejected deposit instead of minting again', async () => {
    mocks.deposit.mockRejectedValueOnce(new Error('User rejected deposit'))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(result.current.error).toContain('You declined the request in your wallet.')
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.mint).toHaveBeenCalledTimes(1)
    expect(mocks.deposit).toHaveBeenCalledTimes(2)
    expect(result.current.phase).toBe('deposited')
  })

  it('keeps mint confirmation when balance lookup fails, so consumed funding can be minted again', async () => {
    mocks.balance.mockRejectedValueOnce(new Error('Balance lookup unavailable'))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(result.current.claim?.status).toBe('minted')
    mocks.balance.mockResolvedValueOnce(0n).mockResolvedValue(amount)
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.mint).toHaveBeenCalledTimes(2)
    expect(result.current.phase).toBe('deposited')
  })

  it('allows a new mint after prior confirmed funding was consumed following an uncertain deposit', async () => {
    mocks.deposit.mockRejectedValueOnce(new Error('Deposit confirmation timed out'))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    mocks.balance.mockResolvedValueOnce(0n).mockResolvedValue(amount)
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.mint).toHaveBeenCalledTimes(2)
    expect(result.current.phase).toBe('deposited')
  })

  it('deposits only the remaining confirmed funding on retry without a second mint', async () => {
    mocks.deposit.mockRejectedValueOnce(new Error('User rejected deposit'))
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    mocks.balance.mockResolvedValue(50n)
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.mint).toHaveBeenCalledTimes(1)
    expect(mocks.deposit).toHaveBeenLastCalledWith(50n, undefined, 'account')
    expect(result.current.phase).toBe('deposited')
  })

  it('rejects a different recipient before any transaction is sent', async () => {
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds('0x5555555555555555555555555555555555555555') })
    expect(result.current.error).toContain('canonical Plether Trading Account')
    expect(mocks.mint).not.toHaveBeenCalled()
    expect(mocks.deposit).not.toHaveBeenCalled()
  })

  it('checks the settlement-token code hash before minting', async () => {
    mocks.code.mockResolvedValue('0x5678')
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(result.current.error).toContain('bytecode does not match')
    expect(mocks.mint).not.toHaveBeenCalled()
  })

  it('does not deposit after a reverted mint', async () => {
    mocks.receipt.mockResolvedValue({ status: 'reverted' })
    const { result } = renderHook(() => useTestnetFunding(account, true))
    await act(async () => { await result.current.requestFunds(account) })
    expect(result.current.error).toContain('reverted')
    expect(mocks.deposit).not.toHaveBeenCalled()
    mocks.receipt.mockResolvedValue({ status: 'success' })
    await act(async () => { await result.current.requestFunds(account) })
    expect(mocks.mint).toHaveBeenCalledTimes(2)
    expect(result.current.phase).toBe('deposited')
  })
})
