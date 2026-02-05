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
  setERC20Balance,
  TEST_ACCOUNTS,
} from '../../test/anvil'
import { SEPOLIA_ADDRESSES, getAddresses } from '../../contracts/addresses'
import { ERC20_ABI, PLETH_CORE_ABI } from '../../contracts/abis'
import { usePreviewMint, usePreviewBurn } from '../usePlethCore'

const USER = TEST_ACCOUNTS[0]
const USDC_DECIMALS = 6
const PAIR_DECIMALS = 18

async function verifyContractsExist(): Promise<boolean> {
  try {
    const code = await publicClient.getCode({ address: SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER })
    return code !== undefined && code !== '0x'
  } catch {
    return false
  }
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
    chain: null,
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
    chain: null,
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
    chain: null,
  })
  await publicClient.waitForTransactionReceipt({ hash })
  await stopImpersonating(USER)
}

function useConnectedPreviewMint(pairAmount: bigint) {
  const { connect, connectors } = useConnect()
  const { isConnected, chainId } = useAccount()
  const preview = usePreviewMint(pairAmount)
  const addresses = chainId ? getAddresses(chainId) : null

  return {
    connect: () => connect({ connector: connectors[0] }),
    isConnected,
    chainId,
    refetchPreview: preview.refetch,
    contractAddress: addresses?.SYNTHETIC_SPLITTER,
    ...preview,
  }
}

function useConnectedPreviewBurn(pairAmount: bigint) {
  const { connect, connectors } = useConnect()
  const { isConnected, chainId } = useAccount()
  const preview = usePreviewBurn(pairAmount)

  return {
    connect: () => connect({ connector: connectors[0] }),
    isConnected,
    chainId,
    ...preview,
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
    // Give user some USDC and ETH for testing
    await setERC20Balance(SEPOLIA_ADDRESSES.USDC, USER, parseUnits('10000', USDC_DECIMALS))
    await testClient.setBalance({ address: USER, value: parseUnits('100', 18) })
  })

  describe('usePreviewMint', () => {
    it('returns USDC required for minting pairs', async (ctx) => {
      if (!contractsExist) {
        ctx.skip()
        return
      }
      const wrapper = createTestWrapper()
      const pairAmount = parseUnits('100', PAIR_DECIMALS)

      const { result } = renderHook(() => useConnectedPreviewMint(pairAmount), { wrapper })

      await act(async () => {
        result.current.connect()
      })

      await waitFor(() => {
        expect(result.current.isConnected).toBe(true)
        expect(result.current.chainId).toBeDefined()
      })

      // Trigger refetch after connection establishes chainId
      await act(async () => {
        await result.current.refetchPreview()
      })

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

      const { result } = renderHook(() => useConnectedPreviewMint(0n), { wrapper })

      await act(async () => {
        result.current.connect()
      })

      await waitFor(() => {
        expect(result.current.isConnected).toBe(true)
      })

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

      const { result } = renderHook(() => useConnectedPreviewBurn(pairAmount), { wrapper })

      await act(async () => {
        result.current.connect()
      })

      await waitFor(() => {
        expect(result.current.isConnected).toBe(true)
        expect(result.current.chainId).toBeDefined()
      })

      await waitFor(() => {
        expect(result.current.isLoading).toBe(false)
        expect(result.current.usdcToReturn).toBeGreaterThan(0n)
      }, { timeout: 10000 })

      expect(result.current.error).toBeNull()
    })
  })

  describe('mint contract (via viem)', () => {
    it('mints token pairs when called with valid amount', async (ctx) => {
      if (!contractsExist) {
        ctx.skip()
        return
      }
      const pairAmount = parseUnits('10', PAIR_DECIMALS)

      // Preview to get USDC required
      const previewResult = await publicClient.readContract({
        address: SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER,
        abi: PLETH_CORE_ABI,
        functionName: 'previewMint',
        args: [pairAmount],
      })
      const usdcRequired = previewResult[0]

      expect(usdcRequired).toBeGreaterThan(0n)

      await approveUSDC(SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER, usdcRequired * 2n)

      const bearBefore = await getBearBalance(USER)
      const bullBefore = await getBullBalance(USER)
      const usdcBefore = await getUSDCBalance(USER)

      await impersonateAccount(USER)
      const mintHash = await walletClient.writeContract({
        address: SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER,
        abi: PLETH_CORE_ABI,
        functionName: 'mint',
        args: [pairAmount],
        account: USER,
        chain: null,
      })
      await publicClient.waitForTransactionReceipt({ hash: mintHash })
      await stopImpersonating(USER)

      const bearAfter = await getBearBalance(USER)
      const bullAfter = await getBullBalance(USER)
      const usdcAfter = await getUSDCBalance(USER)

      // Verify token balances increased
      expect(bearAfter - bearBefore).toBe(pairAmount)
      expect(bullAfter - bullBefore).toBe(pairAmount)

      // Verify USDC was spent
      expect(usdcBefore - usdcAfter).toBe(usdcRequired)
    })
  })

  describe('burn contract (via viem)', () => {
    it('burns token pairs and returns USDC', async (ctx) => {
      if (!contractsExist) {
        ctx.skip()
        return
      }
      const pairAmount = parseUnits('10', PAIR_DECIMALS)

      // First mint some tokens to burn
      const previewMint = await publicClient.readContract({
        address: SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER,
        abi: PLETH_CORE_ABI,
        functionName: 'previewMint',
        args: [pairAmount],
      })

      await approveUSDC(SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER, previewMint[0] * 2n)

      await impersonateAccount(USER)
      const mintHash = await walletClient.writeContract({
        address: SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER,
        abi: PLETH_CORE_ABI,
        functionName: 'mint',
        args: [pairAmount],
        account: USER,
        chain: null,
      })
      await publicClient.waitForTransactionReceipt({ hash: mintHash })
      await stopImpersonating(USER)

      // Verify we have tokens
      const bearBalance = await getBearBalance(USER)
      const bullBalance = await getBullBalance(USER)
      expect(bearBalance).toBe(pairAmount)
      expect(bullBalance).toBe(pairAmount)

      // Preview burn
      const previewBurn = await publicClient.readContract({
        address: SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER,
        abi: PLETH_CORE_ABI,
        functionName: 'previewBurn',
        args: [pairAmount],
      })
      const usdcToReturn = previewBurn[0]
      expect(usdcToReturn).toBeGreaterThan(0n)

      // Approve BEAR and BULL for burning
      await approveBear(SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER, pairAmount)
      await approveBull(SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER, pairAmount)

      const usdcBefore = await getUSDCBalance(USER)

      // Burn via impersonation
      await impersonateAccount(USER)
      const burnHash = await walletClient.writeContract({
        address: SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER,
        abi: PLETH_CORE_ABI,
        functionName: 'burn',
        args: [pairAmount],
        account: USER,
        chain: null,
      })
      await publicClient.waitForTransactionReceipt({ hash: burnHash })
      await stopImpersonating(USER)

      const usdcAfter = await getUSDCBalance(USER)
      const bearAfter = await getBearBalance(USER)
      const bullAfter = await getBullBalance(USER)

      // Verify USDC was returned
      expect(usdcAfter - usdcBefore).toBe(usdcToReturn)

      // Verify tokens were burned
      expect(bearAfter).toBe(0n)
      expect(bullAfter).toBe(0n)
    })
  })

  describe('edge cases', () => {
    it('reverts mint when USDC allowance is insufficient', async (ctx) => {
      if (!contractsExist) {
        ctx.skip()
        return
      }
      const pairAmount = parseUnits('10', PAIR_DECIMALS)

      const previewResult = await publicClient.readContract({
        address: SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER,
        abi: PLETH_CORE_ABI,
        functionName: 'previewMint',
        args: [pairAmount],
      })
      const usdcRequired = previewResult[0]

      await approveUSDC(SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER, usdcRequired / 2n)

      await impersonateAccount(USER)
      await expect(
        walletClient.writeContract({
          address: SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER,
          abi: PLETH_CORE_ABI,
          functionName: 'mint',
          args: [pairAmount],
          account: USER,
          chain: null,
        })
      ).rejects.toThrow()
      await stopImpersonating(USER)
    })

    it('reverts burn when BEAR allowance is insufficient', async (ctx) => {
      if (!contractsExist) {
        ctx.skip()
        return
      }
      const pairAmount = parseUnits('10', PAIR_DECIMALS)

      const previewMint = await publicClient.readContract({
        address: SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER,
        abi: PLETH_CORE_ABI,
        functionName: 'previewMint',
        args: [pairAmount],
      })

      await approveUSDC(SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER, previewMint[0] * 2n)

      await impersonateAccount(USER)
      const mintHash = await walletClient.writeContract({
        address: SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER,
        abi: PLETH_CORE_ABI,
        functionName: 'mint',
        args: [pairAmount],
        account: USER,
        chain: null,
      })
      await publicClient.waitForTransactionReceipt({ hash: mintHash })
      await stopImpersonating(USER)

      await approveBull(SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER, pairAmount)

      await impersonateAccount(USER)
      await expect(
        walletClient.writeContract({
          address: SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER,
          abi: PLETH_CORE_ABI,
          functionName: 'burn',
          args: [pairAmount],
          account: USER,
          chain: null,
        })
      ).rejects.toThrow()
      await stopImpersonating(USER)
    })

    it('handles partial burn correctly', async (ctx) => {
      if (!contractsExist) {
        ctx.skip()
        return
      }
      const pairAmount = parseUnits('20', PAIR_DECIMALS)
      const burnAmount = parseUnits('10', PAIR_DECIMALS)

      const previewMint = await publicClient.readContract({
        address: SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER,
        abi: PLETH_CORE_ABI,
        functionName: 'previewMint',
        args: [pairAmount],
      })

      await approveUSDC(SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER, previewMint[0] * 2n)

      await impersonateAccount(USER)
      const mintHash = await walletClient.writeContract({
        address: SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER,
        abi: PLETH_CORE_ABI,
        functionName: 'mint',
        args: [pairAmount],
        account: USER,
        chain: null,
      })
      await publicClient.waitForTransactionReceipt({ hash: mintHash })
      await stopImpersonating(USER)

      await approveBear(SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER, burnAmount)
      await approveBull(SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER, burnAmount)

      const previewBurn = await publicClient.readContract({
        address: SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER,
        abi: PLETH_CORE_ABI,
        functionName: 'previewBurn',
        args: [burnAmount],
      })
      const expectedUsdc = previewBurn[0]

      const usdcBefore = await getUSDCBalance(USER)

      await impersonateAccount(USER)
      const burnHash = await walletClient.writeContract({
        address: SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER,
        abi: PLETH_CORE_ABI,
        functionName: 'burn',
        args: [burnAmount],
        account: USER,
        chain: null,
      })
      await publicClient.waitForTransactionReceipt({ hash: burnHash })
      await stopImpersonating(USER)

      const usdcAfter = await getUSDCBalance(USER)
      const bearAfter = await getBearBalance(USER)
      const bullAfter = await getBullBalance(USER)

      expect(usdcAfter - usdcBefore).toBe(expectedUsdc)
      expect(bearAfter).toBe(pairAmount - burnAmount)
      expect(bullAfter).toBe(pairAmount - burnAmount)
    })
  })
})
