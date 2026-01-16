import { describe, it, expect, beforeEach, beforeAll } from 'vitest'
import { renderHook, waitFor, act } from '@testing-library/react'
import { parseUnits } from 'viem'
import { useConnect, useAccount } from 'wagmi'
import { createTestWrapper } from '../../test/wrapper'
import {
  publicClient,
  impersonateAccount,
  stopImpersonating,
  walletClient,
  testClient,
  TEST_ACCOUNTS,
} from '../../test/anvil'
import { SEPOLIA_ADDRESSES } from '../../contracts/addresses'
import { ERC20_ABI } from '../../contracts/abis'
import { usePreviewMint, usePreviewBurn, useMint, useBurn } from '../usePlethCore'
import { foundry } from 'viem/chains'

const USER = TEST_ACCOUNTS[0]
const USDC_DECIMALS = 6
const PAIR_DECIMALS = 18

// USDC whale on Sepolia that has tokens (checksummed format)
const USDC_WHALE = '0x51B74cb47a2c8b80149E0b7A7Ea6260c96B99919' as const

async function verifyContractsExist(): Promise<boolean> {
  try {
    const code = await publicClient.getCode({ address: SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER })
    return code !== undefined && code !== '0x'
  } catch {
    return false
  }
}

async function dealUSDC(to: `0x${string}`, amount: bigint) {
  await impersonateAccount(USDC_WHALE)
  await testClient.setBalance({ address: USDC_WHALE, value: parseUnits('1', 18) })
  await walletClient.writeContract({
    address: SEPOLIA_ADDRESSES.USDC,
    abi: ERC20_ABI,
    functionName: 'transfer',
    args: [to, amount],
    account: USDC_WHALE,
    chain: foundry,
  })
  await stopImpersonating(USDC_WHALE)
}

async function getUSDCBalance(account: `0x${string}`): Promise<bigint> {
  return publicClient.readContract({
    address: SEPOLIA_ADDRESSES.USDC,
    abi: ERC20_ABI,
    functionName: 'balanceOf',
    args: [account],
  })
}

async function getBearBalance(account: `0x${string}`): Promise<bigint> {
  return publicClient.readContract({
    address: SEPOLIA_ADDRESSES.DXY_BEAR,
    abi: ERC20_ABI,
    functionName: 'balanceOf',
    args: [account],
  })
}

async function getBullBalance(account: `0x${string}`): Promise<bigint> {
  return publicClient.readContract({
    address: SEPOLIA_ADDRESSES.DXY_BULL,
    abi: ERC20_ABI,
    functionName: 'balanceOf',
    args: [account],
  })
}

async function approveUSDC(spender: `0x${string}`, amount: bigint) {
  await impersonateAccount(USER)
  const hash = await walletClient.writeContract({
    address: SEPOLIA_ADDRESSES.USDC,
    abi: ERC20_ABI,
    functionName: 'approve',
    args: [spender, amount],
    account: USER,
    chain: foundry,
  })
  await publicClient.waitForTransactionReceipt({ hash })
  await stopImpersonating(USER)
}

async function approveBear(spender: `0x${string}`, amount: bigint) {
  await impersonateAccount(USER)
  const hash = await walletClient.writeContract({
    address: SEPOLIA_ADDRESSES.DXY_BEAR,
    abi: ERC20_ABI,
    functionName: 'approve',
    args: [spender, amount],
    account: USER,
    chain: foundry,
  })
  await publicClient.waitForTransactionReceipt({ hash })
  await stopImpersonating(USER)
}

async function approveBull(spender: `0x${string}`, amount: bigint) {
  await impersonateAccount(USER)
  const hash = await walletClient.writeContract({
    address: SEPOLIA_ADDRESSES.DXY_BULL,
    abi: ERC20_ABI,
    functionName: 'approve',
    args: [spender, amount],
    account: USER,
    chain: foundry,
  })
  await publicClient.waitForTransactionReceipt({ hash })
  await stopImpersonating(USER)
}

function useConnectedAccount() {
  const { connect, connectors } = useConnect()
  const account = useAccount()

  return {
    connect: () => connect({ connector: connectors[0] }),
    ...account,
  }
}

describe('usePlethCore Integration Tests', () => {
  let contractsExist = false

  beforeAll(async () => {
    contractsExist = await verifyContractsExist()
    if (!contractsExist) {
      console.warn(
        '\n⚠️  Plether contracts not found on forked chain.\n' +
        '   Make sure Anvil is running with Sepolia fork:\n' +
        '   npm run anvil\n' +
        '   And that contracts are deployed on Sepolia at the configured addresses.\n'
      )
    }
  })

  beforeEach(async () => {
    if (!contractsExist) return
    // Give user some USDC for testing
    await dealUSDC(USER, parseUnits('10000', USDC_DECIMALS))
  })

  describe('usePreviewMint', () => {
    it('returns USDC required for minting pairs', async (ctx) => {
      if (!contractsExist) {
        ctx.skip()
        return
      }
      const wrapper = createTestWrapper()
      const pairAmount = parseUnits('100', PAIR_DECIMALS)

      // Must connect wallet first to get chainId for address resolution
      const { result: connectResult } = renderHook(() => useConnectedAccount(), { wrapper })

      await act(async () => {
        connectResult.current.connect()
      })

      await waitFor(() => {
        expect(connectResult.current.isConnected).toBe(true)
      })

      const { result } = renderHook(() => usePreviewMint(pairAmount), { wrapper })

      await waitFor(() => {
        expect(result.current.isLoading).toBe(false)
        expect(result.current.usdcRequired).toBeGreaterThan(0n)
      }, { timeout: 10000 })

      expect(result.current.error).toBeNull()
    })

    it('returns 0 for zero pair amount', async (ctx) => {
      if (!contractsExist) {
        ctx.skip()
        return
      }
      const wrapper = createTestWrapper()

      // Connect wallet first
      const { result: connectResult } = renderHook(() => useConnectedAccount(), { wrapper })

      await act(async () => {
        connectResult.current.connect()
      })

      await waitFor(() => {
        expect(connectResult.current.isConnected).toBe(true)
      })

      const { result } = renderHook(() => usePreviewMint(0n), { wrapper })

      // Query is disabled when pairAmount is 0, so it won't fetch
      expect(result.current.usdcRequired).toBe(0n)
    })
  })

  describe('usePreviewBurn', () => {
    it('returns USDC to receive when burning pairs', async (ctx) => {
      if (!contractsExist) {
        ctx.skip()
        return
      }
      const wrapper = createTestWrapper()
      const pairAmount = parseUnits('100', PAIR_DECIMALS)

      // Must connect wallet first to get chainId for address resolution
      const { result: connectResult } = renderHook(() => useConnectedAccount(), { wrapper })

      await act(async () => {
        connectResult.current.connect()
      })

      await waitFor(() => {
        expect(connectResult.current.isConnected).toBe(true)
      })

      const { result } = renderHook(() => usePreviewBurn(pairAmount), { wrapper })

      await waitFor(() => {
        expect(result.current.isLoading).toBe(false)
        expect(result.current.usdcToReturn).toBeGreaterThan(0n)
      }, { timeout: 10000 })

      expect(result.current.error).toBeNull()
    })
  })

  describe('useMint', () => {
    it('mints token pairs when called with valid amount', async (ctx) => {
      if (!contractsExist) {
        ctx.skip()
        return
      }
      const wrapper = createTestWrapper()
      const pairAmount = parseUnits('10', PAIR_DECIMALS)

      // First, get preview to know how much USDC we need
      const { result: previewResult } = renderHook(() => usePreviewMint(pairAmount), { wrapper })

      await waitFor(() => {
        expect(previewResult.current.isLoading).toBe(false)
      }, { timeout: 10000 })

      const usdcRequired = previewResult.current.usdcRequired

      // Approve USDC spending
      await approveUSDC(SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER, usdcRequired * 2n)

      // Connect wallet and mint
      const { result: connectResult } = renderHook(() => useConnectedAccount(), { wrapper })

      await act(async () => {
        connectResult.current.connect()
      })

      await waitFor(() => {
        expect(connectResult.current.isConnected).toBe(true)
      })

      const { result: mintResult } = renderHook(() => useMint(), { wrapper })

      const bearBefore = await getBearBalance(USER)
      const bullBefore = await getBullBalance(USER)

      await act(async () => {
        await mintResult.current.mint(pairAmount)
      })

      await waitFor(() => {
        expect(mintResult.current.isSuccess).toBe(true)
      }, { timeout: 30000 })

      const bearAfter = await getBearBalance(USER)
      const bullAfter = await getBullBalance(USER)

      expect(bearAfter - bearBefore).toBe(pairAmount)
      expect(bullAfter - bullBefore).toBe(pairAmount)
    })
  })

  describe('useBurn', () => {
    it('burns token pairs and returns USDC', async (ctx) => {
      if (!contractsExist) {
        ctx.skip()
        return
      }
      const wrapper = createTestWrapper()
      const pairAmount = parseUnits('10', PAIR_DECIMALS)

      // First mint some tokens to burn
      const { result: previewResult } = renderHook(() => usePreviewMint(pairAmount), { wrapper })

      await waitFor(() => {
        expect(previewResult.current.isLoading).toBe(false)
      }, { timeout: 10000 })

      await approveUSDC(SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER, previewResult.current.usdcRequired * 2n)

      // Connect and mint first
      const { result: connectResult } = renderHook(() => useConnectedAccount(), { wrapper })

      await act(async () => {
        connectResult.current.connect()
      })

      await waitFor(() => {
        expect(connectResult.current.isConnected).toBe(true)
      })

      const { result: mintResult } = renderHook(() => useMint(), { wrapper })

      await act(async () => {
        await mintResult.current.mint(pairAmount)
      })

      await waitFor(() => {
        expect(mintResult.current.isSuccess).toBe(true)
      }, { timeout: 30000 })

      // Now approve BEAR and BULL for burning
      await approveBear(SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER, pairAmount)
      await approveBull(SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER, pairAmount)

      // Burn the tokens
      const { result: burnResult } = renderHook(() => useBurn(), { wrapper })

      const usdcBefore = await getUSDCBalance(USER)

      await act(async () => {
        await burnResult.current.burn(pairAmount)
      })

      await waitFor(() => {
        expect(burnResult.current.isSuccess).toBe(true)
      }, { timeout: 30000 })

      const usdcAfter = await getUSDCBalance(USER)
      const bearAfter = await getBearBalance(USER)
      const bullAfter = await getBullBalance(USER)

      expect(usdcAfter).toBeGreaterThan(usdcBefore)
      expect(bearAfter).toBe(0n)
      expect(bullAfter).toBe(0n)
    })
  })
})
