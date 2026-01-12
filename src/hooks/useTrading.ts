import { useAccount, useReadContract, useWriteContract, useWaitForTransactionReceipt } from 'wagmi'
import { useRef, useEffect } from 'react'
import { CURVE_POOL_ABI, ZAP_ROUTER_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'
import { useTransactionStore } from '../stores/transactionStore'

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

  const functionName = direction === 'buy' ? 'previewZapMint' : 'previewZapBurn'

  const { data, isLoading, error, refetch } = useReadContract({
    address: addresses?.ZAP_ROUTER,
    abi: ZAP_ROUTER_ABI,
    functionName,
    args: [amount],
    query: {
      enabled: !!addresses && amount > 0n,
    },
  })

  return {
    amountOut: data ?? 0n,
    isLoading,
    error,
    refetch,
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
