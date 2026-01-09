import { useAccount, useReadContract, useWriteContract, useWaitForTransactionReceipt } from 'wagmi'
import { PLETH_CORE_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'
import { useTransactionStore } from '../stores/transactionStore'

export function usePlethCoreStatus() {
  const { chainId } = useAccount()
  const addresses = getAddresses(chainId ?? 1)

  const { data, isLoading, error, refetch } = useReadContract({
    address: addresses.PLETH_CORE,
    abi: PLETH_CORE_ABI,
    functionName: 'currentStatus',
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
  const addresses = getAddresses(chainId ?? 1)

  const { data, isLoading, error, refetch } = useReadContract({
    address: addresses.PLETH_CORE,
    abi: PLETH_CORE_ABI,
    functionName: 'getSystemStatus',
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

export function usePreviewMint(usdcAmount: bigint) {
  const { chainId } = useAccount()
  const addresses = getAddresses(chainId ?? 1)

  const { data, isLoading, error, refetch } = useReadContract({
    address: addresses.PLETH_CORE,
    abi: PLETH_CORE_ABI,
    functionName: 'previewMint',
    args: [usdcAmount],
    query: {
      enabled: usdcAmount > 0n,
    },
  })

  return {
    pairAmount: data ?? 0n,
    isLoading,
    error,
    refetch,
  }
}

export function usePreviewBurn(pairAmount: bigint) {
  const { chainId } = useAccount()
  const addresses = getAddresses(chainId ?? 1)

  const { data, isLoading, error, refetch } = useReadContract({
    address: addresses.PLETH_CORE,
    abi: PLETH_CORE_ABI,
    functionName: 'previewBurn',
    args: [pairAmount],
    query: {
      enabled: pairAmount > 0n,
    },
  })

  return {
    usdcAmount: data ?? 0n,
    isLoading,
    error,
    refetch,
  }
}

export function useMint() {
  const { chainId } = useAccount()
  const addresses = getAddresses(chainId ?? 1)
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const updateTransaction = useTransactionStore((s) => s.updateTransaction)

  const { writeContract, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess } = useWaitForTransactionReceipt({
    hash,
  })

  const mint = async (usdcAmount: bigint) => {
    const txId = crypto.randomUUID()
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
          address: addresses.PLETH_CORE,
          abi: PLETH_CORE_ABI,
          functionName: 'mint',
          args: [usdcAmount],
        },
        {
          onSuccess: (hash) => {
            updateTransaction(txId, { hash, status: 'confirming' })
          },
          onError: () => {
            updateTransaction(txId, { status: 'failed' })
          },
        }
      )
    } catch {
      updateTransaction(txId, { status: 'failed' })
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
  const addresses = getAddresses(chainId ?? 1)
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const updateTransaction = useTransactionStore((s) => s.updateTransaction)

  const { writeContract, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess } = useWaitForTransactionReceipt({
    hash,
  })

  const burn = async (pairAmount: bigint) => {
    const txId = crypto.randomUUID()
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
          address: addresses.PLETH_CORE,
          abi: PLETH_CORE_ABI,
          functionName: 'burn',
          args: [pairAmount],
        },
        {
          onSuccess: (hash) => {
            updateTransaction(txId, { hash, status: 'confirming' })
          },
          onError: () => {
            updateTransaction(txId, { status: 'failed' })
          },
        }
      )
    } catch {
      updateTransaction(txId, { status: 'failed' })
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
