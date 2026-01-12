import { useAccount, useReadContract, useWriteContract, useWaitForTransactionReceipt } from 'wagmi'
import { useRef, useEffect } from 'react'
import { PLETH_CORE_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'
import { useTransactionStore } from '../stores/transactionStore'
import { parseTransactionError } from '../utils/errors'

export function usePlethCoreStatus() {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null

  const { data, isLoading, error, refetch } = useReadContract({
    address: addresses?.PLETH_CORE,
    abi: PLETH_CORE_ABI,
    functionName: 'currentStatus',
    query: {
      enabled: !!addresses,
    },
  })

  return {
    status: data ?? 0,
    isLoading,
    error,
    refetch,
  }
}

export function usePlethCoreSystemStatus() {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null

  const { data, isLoading, error, refetch } = useReadContract({
    address: addresses?.PLETH_CORE,
    abi: PLETH_CORE_ABI,
    functionName: 'getSystemStatus',
    query: {
      enabled: !!addresses,
    },
  })

  return {
    status: data?.[0] ?? 0,
    totalSupply: data?.[1] ?? 0n,
    oraclePrice: data?.[2] ?? 0n,
    isLoading,
    error,
    refetch,
  }
}

export function usePreviewMint(pairAmount: bigint) {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null

  const { data, isLoading, error, refetch } = useReadContract({
    address: addresses?.PLETH_CORE,
    abi: PLETH_CORE_ABI,
    functionName: 'previewMint',
    args: [pairAmount],
    query: {
      enabled: !!addresses && pairAmount > 0n,
    },
  })

  return {
    usdcRequired: data?.[0] ?? 0n,
    isLoading,
    error,
    refetch,
  }
}

export function usePreviewBurn(pairAmount: bigint) {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null

  const { data, isLoading, error, refetch } = useReadContract({
    address: addresses?.PLETH_CORE,
    abi: PLETH_CORE_ABI,
    functionName: 'previewBurn',
    args: [pairAmount],
    query: {
      enabled: !!addresses && pairAmount > 0n,
    },
  })

  return {
    usdcToReturn: data?.[0] ?? 0n,
    isLoading,
    error,
    refetch,
  }
}

export function useMint() {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const updateTransaction = useTransactionStore((s) => s.updateTransaction)
  const txIdRef = useRef<string | null>(null)

  const { writeContract, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess, isError, error: receiptError } = useWaitForTransactionReceipt({
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
      updateTransaction(txIdRef.current, {
        status: 'failed',
        errorMessage: parseTransactionError(receiptError),
      })
      txIdRef.current = null
    }
  }, [isError, receiptError, updateTransaction])

  const mint = async (pairAmount: bigint) => {
    if (!addresses) return

    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'mint',
      status: 'pending',
      hash: undefined,
      description: 'Minting DXY-BEAR + DXY-BULL',
    })

    try {
      writeContract(
        {
          address: addresses?.PLETH_CORE,
          abi: PLETH_CORE_ABI,
          functionName: 'mint',
          args: [pairAmount],
        },
        {
          onSuccess: (hash) => {
            updateTransaction(txId, { hash, status: 'confirming' })
          },
          onError: (err) => {
            updateTransaction(txId, {
              status: 'failed',
              errorMessage: parseTransactionError(err),
            })
            txIdRef.current = null
          },
        }
      )
    } catch (err) {
      updateTransaction(txId, {
        status: 'failed',
        errorMessage: parseTransactionError(err),
      })
      txIdRef.current = null
    }
  }

  return {
    mint,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}

export function useBurn() {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const updateTransaction = useTransactionStore((s) => s.updateTransaction)
  const txIdRef = useRef<string | null>(null)

  const { writeContract, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess, isError, error: receiptError } = useWaitForTransactionReceipt({
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
      updateTransaction(txIdRef.current, {
        status: 'failed',
        errorMessage: parseTransactionError(receiptError),
      })
      txIdRef.current = null
    }
  }, [isError, receiptError, updateTransaction])

  const burn = async (pairAmount: bigint) => {
    if (!addresses) return
    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'burn',
      status: 'pending',
      hash: undefined,
      description: 'Redeeming DXY-BEAR + DXY-BULL for USDC',
    })

    try {
      writeContract(
        {
          address: addresses?.PLETH_CORE,
          abi: PLETH_CORE_ABI,
          functionName: 'burn',
          args: [pairAmount],
        },
        {
          onSuccess: (hash) => {
            updateTransaction(txId, { hash, status: 'confirming' })
          },
          onError: (err) => {
            updateTransaction(txId, {
              status: 'failed',
              errorMessage: parseTransactionError(err),
            })
            txIdRef.current = null
          },
        }
      )
    } catch (err) {
      updateTransaction(txId, {
        status: 'failed',
        errorMessage: parseTransactionError(err),
      })
      txIdRef.current = null
    }
  }

  return {
    burn,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}
