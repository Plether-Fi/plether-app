import { useAccount, useReadContract, useWriteContract, useWaitForTransactionReceipt, useSignTypedData } from 'wagmi'
import { useRef, useEffect, useCallback, useState } from 'react'
import { zeroAddress } from 'viem'
import { Result } from 'better-result'
import { CURVE_POOL_ABI, ZAP_ROUTER_ABI, ERC20_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'
import { useTransactionStore } from '../stores/transactionStore'
import {
  parseTransactionError,
  getErrorMessage,
  type TransactionError,
} from '../utils/errors'
import { NotConnectedError } from './usePlethCore'

// Note: useTransactionStore is only used by useZapBuyWithPermit and useZapSellWithPermit
// useCurveSwap and useZapSwap do not create transactions - that's handled by TradeCard

export type SwapError = NotConnectedError | TransactionError

const USDC_INDEX = 0n
const BEAR_INDEX = 1n

export function useCurveQuote(tokenIn: 'USDC' | 'BEAR', amountIn: bigint) {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null

  const i = tokenIn === 'USDC' ? USDC_INDEX : BEAR_INDEX
  const j = tokenIn === 'USDC' ? BEAR_INDEX : USDC_INDEX

  const refAmount = tokenIn === 'USDC' ? 1_000_000n : 1_000_000_000_000_000_000n

  const { data, isLoading, error, refetch } = useReadContract({
    address: addresses?.CURVE_POOL,
    abi: CURVE_POOL_ABI,
    functionName: 'get_dy',
    args: [i, j, amountIn],
    query: {
      enabled: !!addresses && amountIn > 0n,
    },
  })

  const { data: refData } = useReadContract({
    address: addresses?.CURVE_POOL,
    abi: CURVE_POOL_ABI,
    functionName: 'get_dy',
    args: [i, j, refAmount],
    query: {
      enabled: !!addresses && amountIn > 0n,
    },
  })

  const amountOut = data ?? 0n
  let priceImpact = 0
  if (amountIn > 0n && amountOut > 0n && refData) {
    const spotRate = Number(refData) / Number(refAmount)
    const actualRate = Number(amountOut) / Number(amountIn)
    priceImpact = ((spotRate - actualRate) / spotRate) * 100
  }

  return {
    amountOut,
    priceImpact,
    isLoading,
    error,
    refetch,
  }
}

export function useCurveSwap() {
  const { address, chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null

  const { writeContractAsync, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess, isError } = useWaitForTransactionReceipt({
    hash,
  })

  const swap = async (
    tokenIn: 'USDC' | 'BEAR',
    amountIn: bigint,
    minAmountOut: bigint
  ): Promise<Result<`0x${string}`, SwapError>> => {
    if (!address || !addresses) {
      return Result.err(new NotConnectedError())
    }

    const i = tokenIn === 'USDC' ? USDC_INDEX : BEAR_INDEX
    const j = tokenIn === 'USDC' ? BEAR_INDEX : USDC_INDEX

    return Result.tryPromise({
      try: () => writeContractAsync({
        address: addresses.CURVE_POOL,
        abi: CURVE_POOL_ABI,
        functionName: 'exchange',
        args: [i, j, amountIn, minAmountOut, address],
      }),
      catch: (err) => {
        if (err instanceof Error && '_tag' in err) {
          return err as TransactionError
        }
        return parseTransactionError(err)
      },
    })
  }

  return {
    swap,
    isPending,
    isConfirming,
    isSuccess,
    isError,
    error,
    reset,
    hash,
  }
}

export function useZapQuote(direction: 'buy' | 'sell', amount: bigint) {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null

  const refAmount = direction === 'buy' ? 1_000_000n : 1_000_000_000_000_000_000n

  const { data: buyData, isLoading: buyLoading, error: buyError, refetch: buyRefetch } = useReadContract({
    address: addresses?.ZAP_ROUTER,
    abi: ZAP_ROUTER_ABI,
    functionName: 'previewZapMint',
    args: [amount],
    query: {
      enabled: !!addresses && amount > 0n && direction === 'buy',
    },
  })

  const { data: buyRefData } = useReadContract({
    address: addresses?.ZAP_ROUTER,
    abi: ZAP_ROUTER_ABI,
    functionName: 'previewZapMint',
    args: [refAmount],
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

  const { data: sellRefData } = useReadContract({
    address: addresses?.ZAP_ROUTER,
    abi: ZAP_ROUTER_ABI,
    functionName: 'previewZapBurn',
    args: [refAmount],
    query: {
      enabled: !!addresses && amount > 0n && direction === 'sell',
    },
  })

  const amountOut = direction === 'buy'
    ? (buyData?.[3] ?? 0n)
    : (sellData?.[2] ?? 0n)

  let priceImpact = 0
  if (direction === 'buy' && amount > 0n && buyData?.[3] && buyRefData?.[3]) {
    const spotRate = Number(buyRefData[3]) / Number(refAmount)
    const actualRate = Number(buyData[3]) / Number(amount)
    priceImpact = ((spotRate - actualRate) / spotRate) * 100
  } else if (direction === 'sell' && amount > 0n && sellData?.[2] && sellRefData?.[2]) {
    const spotRate = Number(sellRefData[2]) / Number(refAmount)
    const actualRate = Number(sellData[2]) / Number(amount)
    priceImpact = ((spotRate - actualRate) / spotRate) * 100
  }

  return {
    amountOut,
    priceImpact,
    isLoading: direction === 'buy' ? buyLoading : sellLoading,
    error: direction === 'buy' ? buyError : sellError,
    refetch: direction === 'buy' ? buyRefetch : sellRefetch,
  }
}

export function useZapSwap() {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null

  const { writeContractAsync, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess, isError } = useWaitForTransactionReceipt({
    hash,
  })

  const zapBuy = async (
    usdcAmount: bigint,
    minBullOut: bigint,
    maxSlippageBps: bigint,
    deadline: bigint
  ): Promise<Result<`0x${string}`, SwapError>> => {
    if (!addresses) {
      return Result.err(new NotConnectedError())
    }

    return Result.tryPromise({
      try: () => writeContractAsync({
        address: addresses.ZAP_ROUTER,
        abi: ZAP_ROUTER_ABI,
        functionName: 'zapMint',
        args: [usdcAmount, minBullOut, maxSlippageBps, deadline],
      }),
      catch: (err) => {
        if (err instanceof Error && '_tag' in err) {
          return err as TransactionError
        }
        return parseTransactionError(err)
      },
    })
  }

  const zapSell = async (
    bullAmount: bigint,
    minUsdcOut: bigint,
    deadline: bigint
  ): Promise<Result<`0x${string}`, SwapError>> => {
    if (!addresses) {
      return Result.err(new NotConnectedError())
    }

    return Result.tryPromise({
      try: () => writeContractAsync({
        address: addresses.ZAP_ROUTER,
        abi: ZAP_ROUTER_ABI,
        functionName: 'zapBurn',
        args: [bullAmount, minUsdcOut, deadline],
      }),
      catch: (err) => {
        if (err instanceof Error && '_tag' in err) {
          return err as TransactionError
        }
        return parseTransactionError(err)
      },
    })
  }

  return {
    zapBuy,
    zapSell,
    isPending,
    isConfirming,
    isSuccess,
    isError,
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
  const queryAddress = address ?? zeroAddress

  const { data: nonce } = useReadContract({
    address: addresses?.USDC,
    abi: ERC20_ABI,
    functionName: 'nonces',
    args: [queryAddress],
    query: { enabled: !!address && !!addresses },
  })

  const { data: tokenName } = useReadContract({
    address: addresses?.USDC,
    abi: ERC20_ABI,
    functionName: 'name',
    query: { enabled: !!addresses },
  })

  const { signTypedDataAsync } = useSignTypedData()
  const { writeContractAsync, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess, isError, error: receiptError } = useWaitForTransactionReceipt({ hash })

  useEffect(() => {
    if (isSuccess && txIdRef.current) {
      updateTransaction(txIdRef.current, { status: 'success' })
      txIdRef.current = null
    }
  }, [isSuccess, updateTransaction])

  useEffect(() => {
    if (isError && txIdRef.current) {
      const txError = parseTransactionError(receiptError)
      updateTransaction(txIdRef.current, {
        status: 'failed',
        errorMessage: getErrorMessage(txError),
      })
      txIdRef.current = null
    }
  }, [isError, receiptError, updateTransaction])

  const zapBuyWithPermit = useCallback(
    async (
      usdcAmount: bigint,
      minBullOut: bigint,
      maxSlippageBps: bigint
    ): Promise<Result<`0x${string}`, SwapError>> => {
      if (!address || !addresses || !chainId || nonce === undefined || !tokenName) {
        return Result.err(new NotConnectedError())
      }

      const txId = crypto.randomUUID()
      txIdRef.current = txId
      addTransaction({
        id: txId,
        type: 'swap',
        status: 'pending',
        hash: undefined,
        title: 'Swapping USDC for plDXY-BULL',
        steps: [{ label: 'Sign permit', status: 'pending' }, { label: 'Swap', status: 'pending' }],
      })

      return Result.tryPromise({
        try: async () => {
          setIsSigningPermit(true)
          const deadline = BigInt(Math.floor(Date.now() / 1000) + 3600)

          const signature = await signTypedDataAsync({
            domain: {
              name: tokenName,
              version: '1',
              chainId: chainId,
              verifyingContract: addresses.USDC,
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
              spender: addresses.ZAP_ROUTER,
              value: usdcAmount,
              nonce: nonce,
              deadline: deadline,
            },
          })
          setIsSigningPermit(false)

          const r: `0x${string}` = signature.slice(0, 66) as `0x${string}`
          const s: `0x${string}` = `0x${signature.slice(66, 130)}`
          const v = parseInt(signature.slice(130, 132), 16)

          const txHash = await writeContractAsync({
            address: addresses.ZAP_ROUTER,
            abi: ZAP_ROUTER_ABI,
            functionName: 'zapMintWithPermit',
            args: [usdcAmount, minBullOut, maxSlippageBps, deadline, v, r, s],
          })
          updateTransaction(txId, { hash: txHash, status: 'confirming' })
          return txHash
        },
        catch: (err) => {
          setIsSigningPermit(false)
          if (err instanceof Error && '_tag' in err) {
            return err as TransactionError
          }
          const txError = parseTransactionError(err)
          updateTransaction(txId, {
            status: 'failed',
            errorMessage: getErrorMessage(txError),
          })
          txIdRef.current = null
          return txError
        },
      })
    },
    [address, addresses, chainId, nonce, tokenName, signTypedDataAsync, writeContractAsync, addTransaction, updateTransaction]
  )

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
  const queryAddress = address ?? zeroAddress

  const { data: nonce } = useReadContract({
    address: addresses?.DXY_BULL,
    abi: ERC20_ABI,
    functionName: 'nonces',
    args: [queryAddress],
    query: { enabled: !!address && !!addresses },
  })

  const { data: tokenName } = useReadContract({
    address: addresses?.DXY_BULL,
    abi: ERC20_ABI,
    functionName: 'name',
    query: { enabled: !!addresses },
  })

  const { signTypedDataAsync } = useSignTypedData()
  const { writeContractAsync, data: hash, isPending, error, reset } = useWriteContract()

  const { isLoading: isConfirming, isSuccess, isError, error: receiptError } = useWaitForTransactionReceipt({ hash })

  useEffect(() => {
    if (isSuccess && txIdRef.current) {
      updateTransaction(txIdRef.current, { status: 'success' })
      txIdRef.current = null
    }
  }, [isSuccess, updateTransaction])

  useEffect(() => {
    if (isError && txIdRef.current) {
      const txError = parseTransactionError(receiptError)
      updateTransaction(txIdRef.current, {
        status: 'failed',
        errorMessage: getErrorMessage(txError),
      })
      txIdRef.current = null
    }
  }, [isError, receiptError, updateTransaction])

  const zapSellWithPermit = useCallback(
    async (bullAmount: bigint, minUsdcOut: bigint): Promise<Result<`0x${string}`, SwapError>> => {
      if (!address || !addresses || !chainId || nonce === undefined || !tokenName) {
        return Result.err(new NotConnectedError())
      }

      const txId = crypto.randomUUID()
      txIdRef.current = txId
      addTransaction({
        id: txId,
        type: 'swap',
        status: 'pending',
        hash: undefined,
        title: 'Swapping plDXY-BULL for USDC',
        steps: [{ label: 'Sign permit', status: 'pending' }, { label: 'Swap', status: 'pending' }],
      })

      return Result.tryPromise({
        try: async () => {
          setIsSigningPermit(true)
          const deadline = BigInt(Math.floor(Date.now() / 1000) + 3600)

          const signature = await signTypedDataAsync({
            domain: {
              name: tokenName,
              version: '1',
              chainId: chainId,
              verifyingContract: addresses.DXY_BULL,
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
              spender: addresses.ZAP_ROUTER,
              value: bullAmount,
              nonce: nonce,
              deadline: deadline,
            },
          })
          setIsSigningPermit(false)

          const r: `0x${string}` = signature.slice(0, 66) as `0x${string}`
          const s: `0x${string}` = `0x${signature.slice(66, 130)}`
          const v = parseInt(signature.slice(130, 132), 16)

          const txHash = await writeContractAsync({
            address: addresses.ZAP_ROUTER,
            abi: ZAP_ROUTER_ABI,
            functionName: 'zapBurnWithPermit',
            args: [bullAmount, minUsdcOut, deadline, v, r, s],
          })
          updateTransaction(txId, { hash: txHash, status: 'confirming' })
          return txHash
        },
        catch: (err) => {
          setIsSigningPermit(false)
          if (err instanceof Error && '_tag' in err) {
            return err as TransactionError
          }
          const txError = parseTransactionError(err)
          updateTransaction(txId, {
            status: 'failed',
            errorMessage: getErrorMessage(txError),
          })
          txIdRef.current = null
          return txError
        },
      })
    },
    [address, addresses, chainId, nonce, tokenName, signTypedDataAsync, writeContractAsync, addTransaction, updateTransaction]
  )

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
