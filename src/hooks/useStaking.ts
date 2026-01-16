import { useAccount, useReadContract, useWriteContract, useWaitForTransactionReceipt, useSignTypedData } from 'wagmi'
import { useRef, useEffect, useCallback, useState } from 'react'
import { zeroAddress } from 'viem'
import { STAKED_TOKEN_ABI, ERC20_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'
import { useTransactionStore } from '../stores/transactionStore'
import { parseTransactionError } from '../utils/errors'

export function useStakedBalance(side: 'BEAR' | 'BULL') {
  const { address, chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const stakingAddress = side === 'BEAR' ? addresses?.STAKING_BEAR : addresses?.STAKING_BULL
  const queryAddress = address ?? zeroAddress

  const { data: shares, isLoading: sharesLoading, refetch: refetchShares } = useReadContract({
    address: stakingAddress,
    abi: ERC20_ABI,
    functionName: 'balanceOf',
    args: [queryAddress],
    query: {
      enabled: !!address && !!stakingAddress,
    },
  })

  const { data: assets, isLoading: assetsLoading, refetch: refetchAssets } = useReadContract({
    address: stakingAddress,
    abi: STAKED_TOKEN_ABI,
    functionName: 'convertToAssets',
    args: [shares ?? 0n],
    query: {
      enabled: !!stakingAddress && !!shares && shares > 0n,
    },
  })

  return {
    shares: shares ?? 0n,
    assets: assets ?? shares ?? 0n,
    isLoading: sharesLoading || assetsLoading,
    refetch: () => {
      void refetchShares()
      void refetchAssets()
    },
  }
}

export function useStakingInfo(side: 'BEAR' | 'BULL') {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const stakingAddress = side === 'BEAR' ? addresses?.STAKING_BEAR : addresses?.STAKING_BULL

  const { data: totalAssets, isLoading, refetch } = useReadContract({
    address: stakingAddress,
    abi: STAKED_TOKEN_ABI,
    functionName: 'totalAssets',
    query: {
      enabled: !!stakingAddress,
    },
  })

  return {
    totalAssets: totalAssets ?? 0n,
    isLoading,
    refetch,
  }
}

export function usePreviewDeposit(side: 'BEAR' | 'BULL', assets: bigint) {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const stakingAddress = side === 'BEAR' ? addresses?.STAKING_BEAR : addresses?.STAKING_BULL

  const { data, isLoading, error, refetch } = useReadContract({
    address: stakingAddress,
    abi: STAKED_TOKEN_ABI,
    functionName: 'previewDeposit',
    args: [assets],
    query: {
      enabled: !!stakingAddress && assets > 0n,
    },
  })

  return {
    shares: data ?? 0n,
    isLoading,
    error,
    refetch,
  }
}

export function usePreviewRedeem(side: 'BEAR' | 'BULL', shares: bigint) {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const stakingAddress = side === 'BEAR' ? addresses?.STAKING_BEAR : addresses?.STAKING_BULL

  const { data, isLoading, error, refetch } = useReadContract({
    address: stakingAddress,
    abi: STAKED_TOKEN_ABI,
    functionName: 'previewRedeem',
    args: [shares],
    query: {
      enabled: !!stakingAddress && shares > 0n,
    },
  })

  return {
    assets: data ?? 0n,
    isLoading,
    error,
    refetch,
  }
}

export function useStake(side: 'BEAR' | 'BULL') {
  const { address, chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const stakingAddress = side === 'BEAR' ? addresses?.STAKING_BEAR : addresses?.STAKING_BULL
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

  const stake = async (amount: bigint) => {
    if (!address || !stakingAddress) return

    const txId = crypto.randomUUID()
    txIdRef.current = txId
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
  const addresses = chainId ? getAddresses(chainId) : null
  const stakingAddress = side === 'BEAR' ? addresses?.STAKING_BEAR : addresses?.STAKING_BULL
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

  const unstake = async (shares: bigint) => {
    if (!address || !stakingAddress) return

    const txId = crypto.randomUUID()
    txIdRef.current = txId
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
    unstake,
    isPending,
    isConfirming,
    isSuccess,
    error,
    reset,
    hash,
  }
}

export function useStakeWithPermit(side: 'BEAR' | 'BULL') {
  const { address, chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const tokenAddress = side === 'BEAR' ? addresses?.DXY_BEAR : addresses?.DXY_BULL
  const stakingAddress = side === 'BEAR' ? addresses?.STAKING_BEAR : addresses?.STAKING_BULL
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const updateTransaction = useTransactionStore((s) => s.updateTransaction)
  const txIdRef = useRef<string | null>(null)
  const [isSigningPermit, setIsSigningPermit] = useState(false)
  const [permitError, setPermitError] = useState<Error | null>(null)
  const [permitCompleted, setPermitCompleted] = useState(false)
  const queryAddress = address ?? zeroAddress

  const { data: nonce } = useReadContract({
    address: tokenAddress,
    abi: ERC20_ABI,
    functionName: 'nonces',
    args: [queryAddress],
    query: { enabled: !!address && !!tokenAddress },
  })

  const { data: tokenName } = useReadContract({
    address: tokenAddress,
    abi: ERC20_ABI,
    functionName: 'name',
    query: { enabled: !!tokenAddress },
  })

  const { signTypedDataAsync } = useSignTypedData()
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

  const stakeWithPermit = useCallback(async (amount: bigint) => {
    if (!address || !stakingAddress || !tokenAddress || !chainId || nonce === undefined || !tokenName) return

    setPermitError(null)
    setPermitCompleted(false)
    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'stake',
      status: 'pending',
      hash: undefined,
      description: `Staking DXY-${side}`,
    })

    try {
      setIsSigningPermit(true)
      const deadline = BigInt(Math.floor(Date.now() / 1000) + 3600)

      const signature = await signTypedDataAsync({
        domain: {
          name: tokenName,
          version: '1',
          chainId: chainId,
          verifyingContract: tokenAddress,
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
          spender: stakingAddress,
          value: amount,
          nonce: nonce,
          deadline: deadline,
        },
      })
      setIsSigningPermit(false)
      setPermitCompleted(true)

      const r = signature.slice(0, 66) as `0x${string}`
      const s = `0x${signature.slice(66, 130)}`
      const v = parseInt(signature.slice(130, 132), 16)

      writeContract(
        {
          address: stakingAddress,
          abi: STAKED_TOKEN_ABI,
          functionName: 'depositWithPermit',
          args: [amount, address, deadline, v, r, s],
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
      setIsSigningPermit(false)
      const error = err instanceof Error ? err : new Error(String(err))
      setPermitError(error)
      updateTransaction(txId, {
        status: 'failed',
        errorMessage: parseTransactionError(err),
      })
      txIdRef.current = null
    }
  }, [address, stakingAddress, tokenAddress, chainId, nonce, tokenName, signTypedDataAsync, writeContract, addTransaction, updateTransaction, side])

  const resetAll = () => {
    reset()
    setPermitError(null)
    setPermitCompleted(false)
  }

  return {
    stakeWithPermit,
    isPending: isPending || isSigningPermit,
    isSigningPermit,
    isConfirming,
    isSuccess,
    error: permitError ?? error,
    permitCompleted,
    reset: resetAll,
    hash,
  }
}
