import { useAccount, useReadContract, useWriteContract, useWaitForTransactionReceipt, useSignTypedData } from 'wagmi'
import { useRef, useEffect, useCallback, useState } from 'react'
import { type Address } from 'viem'
import { CURVE_POOL_ABI, ZAP_ROUTER_ABI, ERC20_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'
import { useTransactionStore } from '../stores/transactionStore'
import { parseTransactionError } from '../utils/errors'

// Curve pool indices: USDC = 0, DXY-BEAR = 1
const USDC_INDEX = 0n
const BEAR_INDEX = 1n

export function useCurveQuote(tokenIn: 'USDC' | 'BEAR', amountIn: bigint) {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null

  const i = tokenIn === 'USDC' ? USDC_INDEX : BEAR_INDEX
  const j = tokenIn === 'USDC' ? BEAR_INDEX : USDC_INDEX

  const { data, isLoading, error, refetch } = useReadContract({
    address: addresses?.CURVE_POOL,
    abi: CURVE_POOL_ABI,
    functionName: 'get_dy',
    args: [i, j, amountIn],
    query: {
      enabled: !!addresses && amountIn > 0n,
    },
  })

  return {
    amountOut: data ?? 0n,
    isLoading,
    error,
    refetch,
  }
}

export function useCurveSwap() {
  const { address, chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const updateTransaction = useTransactionStore((s) => s.updateTransaction)
  const txIdRef = useRef<string | null>(null)

  const { writeContract, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess, isError } = useWaitForTransactionReceipt({
    hash,
  })

  useEffect(() => {
    if (isSuccess && txIdRef.current) {
      updateTransaction(txIdRef.current, { status: 'success' })
      txIdRef.current = null
    }
  }, [isSuccess, updateTransaction])

  useEffect(() => {
    if (isError && txIdRef.current) {
      updateTransaction(txIdRef.current, { status: 'failed' })
      txIdRef.current = null
    }
  }, [isError, updateTransaction])

  const swap = async (tokenIn: 'USDC' | 'BEAR', amountIn: bigint, minAmountOut: bigint) => {
    if (!address || !addresses) return

    const i = tokenIn === 'USDC' ? USDC_INDEX : BEAR_INDEX
    const j = tokenIn === 'USDC' ? BEAR_INDEX : USDC_INDEX

    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'swap',
      status: 'pending',
      hash: undefined,
      description: tokenIn === 'USDC' ? 'Swapping USDC for DXY-BEAR' : 'Swapping DXY-BEAR for USDC',
    })

    try {
      writeContract(
        {
          address: addresses.CURVE_POOL,
          abi: CURVE_POOL_ABI,
          functionName: 'exchange',
          args: [i, j, amountIn, minAmountOut, address],
        },
        {
          onSuccess: (hash) => {
            updateTransaction(txId, { hash, status: 'confirming' })
          },
          onError: () => {
            updateTransaction(txId, { status: 'failed' })
            txIdRef.current = null
          },
        }
      )
    } catch {
      updateTransaction(txId, { status: 'failed' })
      txIdRef.current = null
    }
  }

  return {
    swap,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}

export function useZapQuote(direction: 'buy' | 'sell', amount: bigint) {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null

  const { data: buyData, isLoading: buyLoading, error: buyError, refetch: buyRefetch } = useReadContract({
    address: addresses?.ZAP_ROUTER,
    abi: ZAP_ROUTER_ABI,
    functionName: 'previewZapMint',
    args: [amount],
    query: {
      enabled: !!addresses && amount > 0n && direction === 'buy',
    },
  })

  const { data: sellData, isLoading: sellLoading, error: sellError, refetch: sellRefetch } = useReadContract({
    address: addresses?.ZAP_ROUTER,
    abi: ZAP_ROUTER_ABI,
    functionName: 'previewZapBurn',
    args: [amount],
    query: {
      enabled: !!addresses && amount > 0n && direction === 'sell',
    },
  })

  const amountOut = direction === 'buy'
    ? (buyData?.[3] ?? 0n) // expectedTokensOut is index 3
    : (sellData?.[2] ?? 0n) // expectedUsdcOut is index 2

  return {
    amountOut,
    isLoading: direction === 'buy' ? buyLoading : sellLoading,
    error: direction === 'buy' ? buyError : sellError,
    refetch: direction === 'buy' ? buyRefetch : sellRefetch,
  }
}

export function useZapSwap() {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const updateTransaction = useTransactionStore((s) => s.updateTransaction)
  const txIdRef = useRef<string | null>(null)

  const { writeContract, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess, isError } = useWaitForTransactionReceipt({
    hash,
  })

  useEffect(() => {
    if (isSuccess && txIdRef.current) {
      updateTransaction(txIdRef.current, { status: 'success' })
      txIdRef.current = null
    }
  }, [isSuccess, updateTransaction])

  useEffect(() => {
    if (isError && txIdRef.current) {
      updateTransaction(txIdRef.current, { status: 'failed' })
      txIdRef.current = null
    }
  }, [isError, updateTransaction])

  const zapBuy = async (usdcAmount: bigint, minBullOut: bigint, maxSlippageBps: bigint, deadline: bigint) => {
    if (!addresses) return
    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'swap',
      status: 'pending',
      hash: undefined,
      description: 'Swapping USDC for DXY-BULL',
    })

    try {
      writeContract(
        {
          address: addresses.ZAP_ROUTER,
          abi: ZAP_ROUTER_ABI,
          functionName: 'zapMint',
          args: [usdcAmount, minBullOut, maxSlippageBps, deadline],
        },
        {
          onSuccess: (hash) => {
            updateTransaction(txId, { hash, status: 'confirming' })
          },
          onError: () => {
            updateTransaction(txId, { status: 'failed' })
            txIdRef.current = null
          },
        }
      )
    } catch {
      updateTransaction(txId, { status: 'failed' })
      txIdRef.current = null
    }
  }

  const zapSell = async (bullAmount: bigint, minUsdcOut: bigint, deadline: bigint) => {
    if (!addresses) return
    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'swap',
      status: 'pending',
      hash: undefined,
      description: 'Swapping DXY-BULL for USDC',
    })

    try {
      writeContract(
        {
          address: addresses.ZAP_ROUTER,
          abi: ZAP_ROUTER_ABI,
          functionName: 'zapBurn',
          args: [bullAmount, minUsdcOut, deadline],
        },
        {
          onSuccess: (hash) => {
            updateTransaction(txId, { hash, status: 'confirming' })
          },
          onError: () => {
            updateTransaction(txId, { status: 'failed' })
            txIdRef.current = null
          },
        }
      )
    } catch {
      updateTransaction(txId, { status: 'failed' })
      txIdRef.current = null
    }
  }

  return {
    zapBuy,
    zapSell,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}

export function useZapBuyWithPermit() {
  const { address, chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const updateTransaction = useTransactionStore((s) => s.updateTransaction)
  const txIdRef = useRef<string | null>(null)
  const [isSigningPermit, setIsSigningPermit] = useState(false)

  const { data: nonce } = useReadContract({
    address: addresses?.USDC,
    abi: ERC20_ABI,
    functionName: 'nonces',
    args: [address!],
    query: { enabled: !!address && !!addresses },
  })

  const { data: tokenName } = useReadContract({
    address: addresses?.USDC,
    abi: ERC20_ABI,
    functionName: 'name',
    query: { enabled: !!addresses },
  })

  const { signTypedDataAsync } = useSignTypedData()
  const { writeContract, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess, isError } = useWaitForTransactionReceipt({ hash })

  useEffect(() => {
    if (isSuccess && txIdRef.current) {
      updateTransaction(txIdRef.current, { status: 'success' })
      txIdRef.current = null
    }
  }, [isSuccess, updateTransaction])

  useEffect(() => {
    if (isError && txIdRef.current) {
      updateTransaction(txIdRef.current, { status: 'failed' })
      txIdRef.current = null
    }
  }, [isError, updateTransaction])

  const zapBuyWithPermit = useCallback(async (usdcAmount: bigint, minBullOut: bigint, maxSlippageBps: bigint) => {
    if (!address || !addresses || !chainId || nonce === undefined || !tokenName) return

    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'swap',
      status: 'pending',
      hash: undefined,
      description: 'Swapping USDC for DXY-BULL',
    })

    try {
      setIsSigningPermit(true)
      const deadline = BigInt(Math.floor(Date.now() / 1000) + 3600)

      const signature = await signTypedDataAsync({
        domain: {
          name: tokenName,
          version: '1',
          chainId: chainId,
          verifyingContract: addresses.USDC as Address,
        },
        types: {
          Permit: [
            { name: 'owner', type: 'address' },
            { name: 'spender', type: 'address' },
            { name: 'value', type: 'uint256' },
            { name: 'nonce', type: 'uint256' },
            { name: 'deadline', type: 'uint256' },
          ],
        },
        primaryType: 'Permit',
        message: {
          owner: address,
          spender: addresses.ZAP_ROUTER as Address,
          value: usdcAmount,
          nonce: nonce,
          deadline: deadline,
        },
      })
      setIsSigningPermit(false)

      const r = signature.slice(0, 66) as `0x${string}`
      const s = `0x${signature.slice(66, 130)}` as `0x${string}`
      const v = parseInt(signature.slice(130, 132), 16)

      writeContract(
        {
          address: addresses.ZAP_ROUTER,
          abi: ZAP_ROUTER_ABI,
          functionName: 'zapMintWithPermit',
          args: [usdcAmount, minBullOut, maxSlippageBps, deadline, v, r, s],
        },
        {
          onSuccess: (hash) => {
            updateTransaction(txId, { hash, status: 'confirming' })
          },
          onError: (err) => {
            updateTransaction(txId, { status: 'failed', errorMessage: parseTransactionError(err) })
            txIdRef.current = null
          },
        }
      )
    } catch (err) {
      setIsSigningPermit(false)
      updateTransaction(txId, { status: 'failed', errorMessage: parseTransactionError(err) })
      txIdRef.current = null
    }
  }, [address, addresses, chainId, nonce, tokenName, signTypedDataAsync, writeContract, addTransaction, updateTransaction])

  return {
    zapBuyWithPermit,
    isPending: isPending || isSigningPermit,
    isSigningPermit,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}

export function useZapSellWithPermit() {
  const { address, chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const updateTransaction = useTransactionStore((s) => s.updateTransaction)
  const txIdRef = useRef<string | null>(null)
  const [isSigningPermit, setIsSigningPermit] = useState(false)

  const { data: nonce } = useReadContract({
    address: addresses?.DXY_BULL,
    abi: ERC20_ABI,
    functionName: 'nonces',
    args: [address!],
    query: { enabled: !!address && !!addresses },
  })

  const { data: tokenName } = useReadContract({
    address: addresses?.DXY_BULL,
    abi: ERC20_ABI,
    functionName: 'name',
    query: { enabled: !!addresses },
  })

  const { signTypedDataAsync } = useSignTypedData()
  const { writeContract, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess, isError } = useWaitForTransactionReceipt({ hash })

  useEffect(() => {
    if (isSuccess && txIdRef.current) {
      updateTransaction(txIdRef.current, { status: 'success' })
      txIdRef.current = null
    }
  }, [isSuccess, updateTransaction])

  useEffect(() => {
    if (isError && txIdRef.current) {
      updateTransaction(txIdRef.current, { status: 'failed' })
      txIdRef.current = null
    }
  }, [isError, updateTransaction])

  const zapSellWithPermit = useCallback(async (bullAmount: bigint, minUsdcOut: bigint) => {
    if (!address || !addresses || !chainId || nonce === undefined || !tokenName) return

    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'swap',
      status: 'pending',
      hash: undefined,
      description: 'Swapping DXY-BULL for USDC',
    })

    try {
      setIsSigningPermit(true)
      const deadline = BigInt(Math.floor(Date.now() / 1000) + 3600)

      const signature = await signTypedDataAsync({
        domain: {
          name: tokenName,
          version: '1',
          chainId: chainId,
          verifyingContract: addresses.DXY_BULL as Address,
        },
        types: {
          Permit: [
            { name: 'owner', type: 'address' },
            { name: 'spender', type: 'address' },
            { name: 'value', type: 'uint256' },
            { name: 'nonce', type: 'uint256' },
            { name: 'deadline', type: 'uint256' },
          ],
        },
        primaryType: 'Permit',
        message: {
          owner: address,
          spender: addresses.ZAP_ROUTER as Address,
          value: bullAmount,
          nonce: nonce,
          deadline: deadline,
        },
      })
      setIsSigningPermit(false)

      const r = signature.slice(0, 66) as `0x${string}`
      const s = `0x${signature.slice(66, 130)}` as `0x${string}`
      const v = parseInt(signature.slice(130, 132), 16)

      writeContract(
        {
          address: addresses.ZAP_ROUTER,
          abi: ZAP_ROUTER_ABI,
          functionName: 'zapBurnWithPermit',
          args: [bullAmount, minUsdcOut, deadline, v, r, s],
        },
        {
          onSuccess: (hash) => {
            updateTransaction(txId, { hash, status: 'confirming' })
          },
          onError: (err) => {
            updateTransaction(txId, { status: 'failed', errorMessage: parseTransactionError(err) })
            txIdRef.current = null
          },
        }
      )
    } catch (err) {
      setIsSigningPermit(false)
      updateTransaction(txId, { status: 'failed', errorMessage: parseTransactionError(err) })
      txIdRef.current = null
    }
  }, [address, addresses, chainId, nonce, tokenName, signTypedDataAsync, writeContract, addTransaction, updateTransaction])

  return {
    zapSellWithPermit,
    isPending: isPending || isSigningPermit,
    isSigningPermit,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}
