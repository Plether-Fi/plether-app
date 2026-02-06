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
import { getDeadline } from '../utils/deadline'
import { EIP2612_PERMIT_TYPES, splitSignature } from '../utils/permit'

// Note: useTransactionStore is only used by useZapBuyWithPermit and useZapSellWithPermit
// useCurveSwap and useZapSwap do not create transactions - that's handled by TradeCard

export type SwapError = NotConnectedError | TransactionError

const USDC_INDEX = 0n
const BEAR_INDEX = 1n
const PRECISION = 10n ** 18n

function calculatePriceImpact(
  actualOutput: bigint,
  actualInput: bigint,
  refOutput: bigint,
  refInput: bigint
): number {
  if (actualInput === 0n || refInput === 0n || refOutput === 0n) return 0

  const spotRateScaled = refOutput * PRECISION / refInput
  const actualRateScaled = actualOutput * PRECISION / actualInput

  if (spotRateScaled === 0n) return 0
  if (actualRateScaled >= spotRateScaled) return 0

  const impactBps = (spotRateScaled - actualRateScaled) * 10000n / spotRateScaled
  return Number(impactBps) / 100
}

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
  const priceImpact = refData
    ? calculatePriceImpact(amountOut, amountIn, refData, refAmount)
    : 0

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
  if (direction === 'buy' && buyData?.[3] && buyRefData?.[3]) {
    priceImpact = calculatePriceImpact(buyData[3], amount, buyRefData[3], refAmount)
  } else if (direction === 'sell' && sellData?.[2] && sellRefData?.[2]) {
    priceImpact = calculatePriceImpact(sellData[2], amount, sellRefData[2], refAmount)
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
  const setStepInProgress = useTransactionStore((s) => s.setStepInProgress)
  const setStepSuccess = useTransactionStore((s) => s.setStepSuccess)
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
    if (isSuccess && txIdRef.current && hash) {
      setStepSuccess(txIdRef.current, hash)
      txIdRef.current = null
    }
  }, [isSuccess, hash, setStepSuccess])

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
        steps: [
          { label: 'Sign permit', status: 'pending' },
          { label: 'Swap', status: 'pending' },
          { label: 'Confirming onchain (~12s)', status: 'pending' },
        ],
      })
      setStepInProgress(txId, 0)

      return Result.tryPromise({
        try: async () => {
          setIsSigningPermit(true)
          const deadline = getDeadline(60)

          const signature = await signTypedDataAsync({
            domain: {
              name: tokenName,
              version: '1',
              chainId: chainId,
              verifyingContract: addresses.USDC,
            },
            types: EIP2612_PERMIT_TYPES,
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
          setStepInProgress(txId, 1)

          const { r, s, v } = splitSignature(signature)

          const txHash = await writeContractAsync({
            address: addresses.ZAP_ROUTER,
            abi: ZAP_ROUTER_ABI,
            functionName: 'zapMintWithPermit',
            args: [usdcAmount, minBullOut, maxSlippageBps, deadline, v, r, s],
          })
          setStepInProgress(txId, 2)
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
    [address, addresses, chainId, nonce, tokenName, signTypedDataAsync, writeContractAsync, addTransaction, updateTransaction, setStepInProgress]
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
  const setStepInProgress = useTransactionStore((s) => s.setStepInProgress)
  const setStepSuccess = useTransactionStore((s) => s.setStepSuccess)
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
    if (isSuccess && txIdRef.current && hash) {
      setStepSuccess(txIdRef.current, hash)
      txIdRef.current = null
    }
  }, [isSuccess, hash, setStepSuccess])

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
        steps: [
          { label: 'Sign permit', status: 'pending' },
          { label: 'Swap', status: 'pending' },
          { label: 'Confirming onchain (~12s)', status: 'pending' },
        ],
      })
      setStepInProgress(txId, 0)

      return Result.tryPromise({
        try: async () => {
          setIsSigningPermit(true)
          const deadline = getDeadline(60)

          const signature = await signTypedDataAsync({
            domain: {
              name: tokenName,
              version: '1',
              chainId: chainId,
              verifyingContract: addresses.DXY_BULL,
            },
            types: EIP2612_PERMIT_TYPES,
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
          setStepInProgress(txId, 1)

          const { r, s, v } = splitSignature(signature)

          const txHash = await writeContractAsync({
            address: addresses.ZAP_ROUTER,
            abi: ZAP_ROUTER_ABI,
            functionName: 'zapBurnWithPermit',
            args: [bullAmount, minUsdcOut, deadline, v, r, s],
          })
          setStepInProgress(txId, 2)
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
    [address, addresses, chainId, nonce, tokenName, signTypedDataAsync, writeContractAsync, addTransaction, updateTransaction, setStepInProgress]
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
