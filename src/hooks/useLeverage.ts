import { useAccount, useReadContract, useWriteContract, useWaitForTransactionReceipt } from 'wagmi'
import { useRef, useEffect } from 'react'
import { LEVERAGE_ROUTER_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'
import { useTransactionStore } from '../stores/transactionStore'

export function useLeveragePosition(side: 'BEAR' | 'BULL') {
  const { address, chainId } = useAccount()
  const addresses = getAddresses(chainId ?? 1)
  const routerAddress = side === 'BEAR' ? addresses.LEVERAGE_ROUTER : addresses.BULL_LEVERAGE_ROUTER

  const { data, isLoading, error, refetch } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'getPosition',
    args: [address!],
    query: {
      enabled: !!address,
    },
  })

  const { data: healthFactor } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'getHealthFactor',
    args: [address!],
    query: {
      enabled: !!address && !!data && data[0] > 0n,
    },
  })

  const { data: liquidationPrice } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'getLiquidationPrice',
    args: [address!],
    query: {
      enabled: !!address && !!data && data[0] > 0n,
    },
  })

  return {
    collateral: data?.[0] ?? 0n,
    debt: data?.[1] ?? 0n,
    leverage: data?.[2] ?? 0n,
    healthFactor: healthFactor ?? 0n,
    liquidationPrice: liquidationPrice ?? 0n,
    hasPosition: (data?.[0] ?? 0n) > 0n,
    isLoading,
    error,
    refetch,
  }
}

export function usePreviewOpenLeverage(side: 'BEAR' | 'BULL', principal: bigint, leverage: bigint) {
  const { chainId } = useAccount()
  const addresses = getAddresses(chainId ?? 1)
  const routerAddress = side === 'BEAR' ? addresses.LEVERAGE_ROUTER : addresses.BULL_LEVERAGE_ROUTER

  const { data, isLoading, error, refetch } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'previewOpenLeverage',
    args: [principal, leverage],
    query: {
      enabled: principal > 0n && leverage > 0n,
    },
  })

  return {
    positionSize: data?.[0] ?? 0n,
    debt: data?.[1] ?? 0n,
    liquidationPrice: data?.[2] ?? 0n,
    isLoading,
    error,
    refetch,
  }
}

export function useOpenLeverage(side: 'BEAR' | 'BULL') {
  const { chainId } = useAccount()
  const addresses = getAddresses(chainId ?? 1)
  const routerAddress = side === 'BEAR' ? addresses.LEVERAGE_ROUTER : addresses.BULL_LEVERAGE_ROUTER
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

  const openPosition = async (principal: bigint, leverage: bigint, maxSlippageBps: bigint, deadline: bigint) => {
    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'leverage',
      status: 'pending',
      hash: undefined,
      description: `Opening ${side} leverage position`,
    })

    try {
      writeContract(
        {
          address: routerAddress,
          abi: LEVERAGE_ROUTER_ABI,
          functionName: 'openLeverage',
          args: [principal, leverage, maxSlippageBps, deadline],
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
    openPosition,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}

export function useCloseLeverage(side: 'BEAR' | 'BULL') {
  const { chainId } = useAccount()
  const addresses = getAddresses(chainId ?? 1)
  const routerAddress = side === 'BEAR' ? addresses.LEVERAGE_ROUTER : addresses.BULL_LEVERAGE_ROUTER
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

  const closePosition = async (debtToRepay: bigint, collateralToWithdraw: bigint, maxSlippageBps: bigint, deadline: bigint) => {
    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'leverage',
      status: 'pending',
      hash: undefined,
      description: `Closing ${side} leverage position`,
    })

    try {
      writeContract(
        {
          address: routerAddress,
          abi: LEVERAGE_ROUTER_ABI,
          functionName: 'closeLeverage',
          args: [debtToRepay, collateralToWithdraw, maxSlippageBps, deadline],
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
    closePosition,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}

export function useAdjustCollateral(side: 'BEAR' | 'BULL') {
  const { chainId } = useAccount()
  const addresses = getAddresses(chainId ?? 1)
  const routerAddress = side === 'BEAR' ? addresses.LEVERAGE_ROUTER : addresses.BULL_LEVERAGE_ROUTER
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

  const addCollateral = async (amount: bigint) => {
    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'leverage',
      status: 'pending',
      hash: undefined,
      description: 'Adding collateral',
    })

    try {
      writeContract(
        {
          address: routerAddress,
          abi: LEVERAGE_ROUTER_ABI,
          functionName: 'addCollateral',
          args: [amount],
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

  const removeCollateral = async (amount: bigint) => {
    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'leverage',
      status: 'pending',
      hash: undefined,
      description: 'Removing collateral',
    })

    try {
      writeContract(
        {
          address: routerAddress,
          abi: LEVERAGE_ROUTER_ABI,
          functionName: 'removeCollateral',
          args: [amount],
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
    addCollateral,
    removeCollateral,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}
