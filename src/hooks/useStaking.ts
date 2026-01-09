import { useAccount, useReadContract, useWriteContract, useWaitForTransactionReceipt } from 'wagmi'
import { STAKED_TOKEN_ABI, ERC20_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'
import { useTransactionStore } from '../stores/transactionStore'

export function useStakedBalance(side: 'BEAR' | 'BULL') {
  const { address, chainId } = useAccount()
  const addresses = getAddresses(chainId ?? 1)
  const stakingAddress = side === 'BEAR' ? addresses.STAKING_BEAR : addresses.STAKING_BULL

  const { data: shares, isLoading: sharesLoading, refetch: refetchShares } = useReadContract({
    address: stakingAddress,
    abi: ERC20_ABI,
    functionName: 'balanceOf',
    args: [address!],
    query: {
      enabled: !!address,
    },
  })

  const { data: assets, isLoading: assetsLoading, refetch: refetchAssets } = useReadContract({
    address: stakingAddress,
    abi: STAKED_TOKEN_ABI,
    functionName: 'convertToAssets',
    args: [shares ?? 0n],
    query: {
      enabled: !!shares && shares > 0n,
    },
  })

  return {
    shares: shares ?? 0n,
    assets: assets ?? shares ?? 0n,
    isLoading: sharesLoading || assetsLoading,
    refetch: () => {
      refetchShares()
      refetchAssets()
    },
  }
}

export function useStakingInfo(side: 'BEAR' | 'BULL') {
  const { chainId } = useAccount()
  const addresses = getAddresses(chainId ?? 1)
  const stakingAddress = side === 'BEAR' ? addresses.STAKING_BEAR : addresses.STAKING_BULL

  const { data: totalAssets, isLoading, refetch } = useReadContract({
    address: stakingAddress,
    abi: STAKED_TOKEN_ABI,
    functionName: 'totalAssets',
  })

  return {
    totalAssets: totalAssets ?? 0n,
    isLoading,
    refetch,
  }
}

export function useStake(side: 'BEAR' | 'BULL') {
  const { address, chainId } = useAccount()
  const addresses = getAddresses(chainId ?? 1)
  const stakingAddress = side === 'BEAR' ? addresses.STAKING_BEAR : addresses.STAKING_BULL
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const updateTransaction = useTransactionStore((s) => s.updateTransaction)

  const { writeContract, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess } = useWaitForTransactionReceipt({
    hash,
  })

  const stake = async (amount: bigint) => {
    if (!address) return

    const txId = crypto.randomUUID()
    addTransaction({
      id: txId,
      type: 'stake',
      status: 'pending',
      hash: undefined,
      description: `Staking DXY-${side}`,
    })

    try {
      writeContract(
        {
          address: stakingAddress,
          abi: STAKED_TOKEN_ABI,
          functionName: 'deposit',
          args: [amount, address],
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
    stake,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}

export function useUnstake(side: 'BEAR' | 'BULL') {
  const { address, chainId } = useAccount()
  const addresses = getAddresses(chainId ?? 1)
  const stakingAddress = side === 'BEAR' ? addresses.STAKING_BEAR : addresses.STAKING_BULL
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const updateTransaction = useTransactionStore((s) => s.updateTransaction)

  const { writeContract, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess } = useWaitForTransactionReceipt({
    hash,
  })

  const unstake = async (shares: bigint) => {
    if (!address) return

    const txId = crypto.randomUUID()
    addTransaction({
      id: txId,
      type: 'unstake',
      status: 'pending',
      hash: undefined,
      description: `Unstaking sDXY-${side}`,
    })

    try {
      writeContract(
        {
          address: stakingAddress,
          abi: STAKED_TOKEN_ABI,
          functionName: 'redeem',
          args: [shares, address, address],
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
    unstake,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}
