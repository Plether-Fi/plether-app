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
import { SEPOLIA_ADDRESSES } from '../../contracts/addresses'
import { ERC20_ABI, CURVE_POOL_ABI, ZAP_ROUTER_ABI } from '../../contracts/abis'
import { useCurveQuote, useZapQuote } from '../useTrading'

const USER = TEST_ACCOUNTS[0]
const USDC_DECIMALS = 6
const TOKEN_DECIMALS = 18

async function verifyContractsExist(): Promise<boolean> {
  try {
    const [curveCode, zapCode] = await Promise.all([
      publicClient.getCode({ address: SEPOLIA_ADDRESSES.CURVE_POOL }),
      publicClient.getCode({ address: SEPOLIA_ADDRESSES.ZAP_ROUTER }),
    ])
    return (curveCode !== undefined && curveCode !== '0x') &&
           (zapCode !== undefined && zapCode !== '0x')
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

function useConnectedCurveQuote(tokenIn: 'USDC' | 'BEAR', amountIn: bigint) {
  const { connect, connectors } = useConnect()
  const { isConnected, chainId } = useAccount()
  const quote = useCurveQuote(tokenIn, amountIn)

  return {
    connect: () => connect({ connector: connectors[0] }),
    isConnected,
    chainId,
    ...quote,
  }
}

function useConnectedZapQuote(direction: 'buy' | 'sell', amount: bigint) {
  const { connect, connectors } = useConnect()
  const { isConnected, chainId } = useAccount()
  const quote = useZapQuote(direction, amount)

  return {
    connect: () => connect({ connector: connectors[0] }),
    isConnected,
    chainId,
    ...quote,
  }
}

describe('useTrading Integration Tests', () => {
  let contractsExist = false

  beforeAll(async () => {
    contractsExist = await verifyContractsExist()
    if (!contractsExist) {
      console.warn(
        '\n⚠️  Trading contracts not found on forked chain.\n' +
        '   Make sure Anvil is running with Sepolia fork:\n' +
        '   npm run anvil\n' +
        '   And that Curve Pool and Zap Router are deployed on Sepolia.\n'
      )
    }
  })

  beforeEach(async () => {
    if (!contractsExist) return
    await setERC20Balance(SEPOLIA_ADDRESSES.USDC, USER, parseUnits('10000', USDC_DECIMALS))
    await testClient.setBalance({ address: USER, value: parseUnits('100', 18) })
  })

  describe('useCurveQuote', () => {
    it('returns quote for USDC to BEAR swap', async (ctx) => {
      if (!contractsExist) {
        ctx.skip()
        return
      }
      const wrapper = createTestWrapper()
      const usdcAmount = parseUnits('100', USDC_DECIMALS)

      const { result } = renderHook(
        () => useConnectedCurveQuote('USDC', usdcAmount),
        { wrapper }
      )

      await act(async () => {
        result.current.connect()
      })

      await waitFor(() => {
        expect(result.current.isConnected).toBe(true)
        expect(result.current.chainId).toBeDefined()
      })

      await waitFor(() => {
        expect(result.current.isLoading).toBe(false)
        expect(result.current.amountOut).toBeGreaterThan(0n)
      }, { timeout: 10000 })

      expect(result.current.error).toBeNull()
    })

    it('calculates price impact for large swaps', async (ctx) => {
      if (!contractsExist) {
        ctx.skip()
        return
      }
      const wrapper = createTestWrapper()
      const largeAmount = parseUnits('10000', USDC_DECIMALS)

      const { result } = renderHook(
        () => useConnectedCurveQuote('USDC', largeAmount),
        { wrapper }
      )

      await act(async () => {
        result.current.connect()
      })

      await waitFor(() => {
        expect(result.current.isConnected).toBe(true)
      })

      await waitFor(() => {
        expect(result.current.isLoading).toBe(false)
      }, { timeout: 10000 })

      expect(result.current.priceImpact).toBeGreaterThanOrEqual(0)
    })
  })

  describe('useZapQuote', () => {
    it('returns quote for USDC to BULL zap buy', async (ctx) => {
      if (!contractsExist) {
        ctx.skip()
        return
      }
      const wrapper = createTestWrapper()
      const usdcAmount = parseUnits('100', USDC_DECIMALS)

      const { result } = renderHook(
        () => useConnectedZapQuote('buy', usdcAmount),
        { wrapper }
      )

      await act(async () => {
        result.current.connect()
      })

      await waitFor(() => {
        expect(result.current.isConnected).toBe(true)
        expect(result.current.chainId).toBeDefined()
      })

      await waitFor(() => {
        expect(result.current.isLoading).toBe(false)
        expect(result.current.amountOut).toBeGreaterThan(0n)
      }, { timeout: 10000 })

      expect(result.current.error).toBeNull()
    })

    it('returns quote for BULL to USDC zap sell', async (ctx) => {
      if (!contractsExist) {
        ctx.skip()
        return
      }
      const wrapper = createTestWrapper()
      const bullAmount = parseUnits('100', TOKEN_DECIMALS)

      const { result } = renderHook(
        () => useConnectedZapQuote('sell', bullAmount),
        { wrapper }
      )

      await act(async () => {
        result.current.connect()
      })

      await waitFor(() => {
        expect(result.current.isConnected).toBe(true)
      })

      await waitFor(() => {
        expect(result.current.isLoading).toBe(false)
        expect(result.current.amountOut).toBeGreaterThan(0n)
      }, { timeout: 10000 })

      expect(result.current.error).toBeNull()
    })
  })

  describe('Curve swap contract (via viem)', () => {
    it('executes USDC to BEAR swap', async (ctx) => {
      if (!contractsExist) {
        ctx.skip()
        return
      }
      const usdcAmount = parseUnits('100', USDC_DECIMALS)

      const quoteResult = await publicClient.readContract({
        address: SEPOLIA_ADDRESSES.CURVE_POOL,
        abi: CURVE_POOL_ABI,
        functionName: 'get_dy',
        args: [0n, 1n, usdcAmount],
      })

      expect(quoteResult).toBeGreaterThan(0n)

      await approveUSDC(SEPOLIA_ADDRESSES.CURVE_POOL, usdcAmount * 2n)

      const usdcBefore = await getUSDCBalance(USER)
      const bearBefore = await getBearBalance(USER)

      const minOut = quoteResult * 99n / 100n

      await impersonateAccount(USER)
      const swapHash = await walletClient.writeContract({
        address: SEPOLIA_ADDRESSES.CURVE_POOL,
        abi: CURVE_POOL_ABI,
        functionName: 'exchange',
        args: [0n, 1n, usdcAmount, minOut, USER],
        account: USER,
        chain: null,
      })
      await publicClient.waitForTransactionReceipt({ hash: swapHash })
      await stopImpersonating(USER)

      const usdcAfter = await getUSDCBalance(USER)
      const bearAfter = await getBearBalance(USER)

      expect(usdcBefore - usdcAfter).toBe(usdcAmount)
      expect(bearAfter - bearBefore).toBeGreaterThanOrEqual(minOut)
    })

    it('executes BEAR to USDC swap', async (ctx) => {
      if (!contractsExist) {
        ctx.skip()
        return
      }

      const buyAmount = parseUnits('100', USDC_DECIMALS)
      await approveUSDC(SEPOLIA_ADDRESSES.CURVE_POOL, buyAmount * 2n)

      await impersonateAccount(USER)
      const buyHash = await walletClient.writeContract({
        address: SEPOLIA_ADDRESSES.CURVE_POOL,
        abi: CURVE_POOL_ABI,
        functionName: 'exchange',
        args: [0n, 1n, buyAmount, 0n, USER],
        account: USER,
        chain: null,
      })
      await publicClient.waitForTransactionReceipt({ hash: buyHash })
      await stopImpersonating(USER)

      const bearBalance = await getBearBalance(USER)
      expect(bearBalance).toBeGreaterThan(0n)

      const sellAmount = bearBalance / 2n
      const quoteResult = await publicClient.readContract({
        address: SEPOLIA_ADDRESSES.CURVE_POOL,
        abi: CURVE_POOL_ABI,
        functionName: 'get_dy',
        args: [1n, 0n, sellAmount],
      })

      await approveBear(SEPOLIA_ADDRESSES.CURVE_POOL, sellAmount * 2n)

      const usdcBefore = await getUSDCBalance(USER)
      const bearBefore = await getBearBalance(USER)

      const minOut = quoteResult * 99n / 100n

      await impersonateAccount(USER)
      const sellHash = await walletClient.writeContract({
        address: SEPOLIA_ADDRESSES.CURVE_POOL,
        abi: CURVE_POOL_ABI,
        functionName: 'exchange',
        args: [1n, 0n, sellAmount, minOut, USER],
        account: USER,
        chain: null,
      })
      await publicClient.waitForTransactionReceipt({ hash: sellHash })
      await stopImpersonating(USER)

      const usdcAfter = await getUSDCBalance(USER)
      const bearAfter = await getBearBalance(USER)

      expect(bearBefore - bearAfter).toBe(sellAmount)
      expect(usdcAfter - usdcBefore).toBeGreaterThanOrEqual(minOut)
    })
  })

  describe('Zap Router contract (via viem)', () => {
    it('executes zapMint (USDC to BULL)', async (ctx) => {
      if (!contractsExist) {
        ctx.skip()
        return
      }
      const usdcAmount = parseUnits('100', USDC_DECIMALS)

      const previewResult = await publicClient.readContract({
        address: SEPOLIA_ADDRESSES.ZAP_ROUTER,
        abi: ZAP_ROUTER_ABI,
        functionName: 'previewZapMint',
        args: [usdcAmount],
      })
      const bullOut = previewResult[3]
      expect(bullOut).toBeGreaterThan(0n)

      await approveUSDC(SEPOLIA_ADDRESSES.ZAP_ROUTER, usdcAmount * 2n)

      const usdcBefore = await getUSDCBalance(USER)
      const bullBefore = await getBullBalance(USER)

      const minOut = bullOut * 99n / 100n
      const deadline = BigInt(Math.floor(Date.now() / 1000) + 3600)

      await impersonateAccount(USER)
      const zapHash = await walletClient.writeContract({
        address: SEPOLIA_ADDRESSES.ZAP_ROUTER,
        abi: ZAP_ROUTER_ABI,
        functionName: 'zapMint',
        args: [usdcAmount, minOut, 100n, deadline],
        account: USER,
        chain: null,
      })
      await publicClient.waitForTransactionReceipt({ hash: zapHash })
      await stopImpersonating(USER)

      const usdcAfter = await getUSDCBalance(USER)
      const bullAfter = await getBullBalance(USER)

      expect(usdcBefore - usdcAfter).toBe(usdcAmount)
      expect(bullAfter - bullBefore).toBeGreaterThanOrEqual(minOut)
    })
  })
})
