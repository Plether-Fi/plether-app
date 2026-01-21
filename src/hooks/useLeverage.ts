import { useAccount, useReadContract, useWriteContract, useWaitForTransactionReceipt } from 'wagmi'
import { useRef, useEffect, useMemo } from 'react'
import { zeroAddress, keccak256, encodeAbiParameters } from 'viem'
import { Result } from 'better-result'
import { LEVERAGE_ROUTER_ABI, MORPHO_ABI, PLETH_CORE_ABI, BASKET_ORACLE_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'
import { useTransactionStore } from '../stores/transactionStore'
import {
  parseTransactionError,
  getErrorMessage,
  type TransactionError,
} from '../utils/errors'
import { NotConnectedError } from './usePlethCore'

export type LeverageError = NotConnectedError | TransactionError

export function useLeveragePosition(side: 'BEAR' | 'BULL') {
  const { address, chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const routerAddress = side === 'BEAR' ? addresses?.LEVERAGE_ROUTER : addresses?.BULL_LEVERAGE_ROUTER
  const queryAddress = address ?? zeroAddress

  // Get Morpho address from router
  const { data: morphoAddress } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'MORPHO',
    query: {
      enabled: !!routerAddress,
    },
  })

  // Get market params from router
  const { data: marketParams } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'marketParams',
    query: {
      enabled: !!routerAddress,
    },
  })

  // Compute market ID from market params: keccak256(abi.encode(marketParams))
  const marketId = useMemo(() => {
    if (!marketParams) return undefined
    const [loanToken, collateralToken, oracle, irm, lltv] = marketParams
    return keccak256(
      encodeAbiParameters(
        [
          { type: 'address' },
          { type: 'address' },
          { type: 'address' },
          { type: 'address' },
          { type: 'uint256' },
        ],
        [loanToken, collateralToken, oracle, irm, lltv]
      )
    )
  }, [marketParams])

  // Query Morpho for position (collateral is stored there)
  const { data: morphoPosition, isLoading: positionLoading, error, refetch } = useReadContract({
    address: morphoAddress,
    abi: MORPHO_ABI,
    functionName: 'position',
    args: [marketId!, queryAddress],
    query: {
      enabled: !!address && !!morphoAddress && !!marketId,
    },
  })

  // Get debt from router (includes accrued interest)
  const { data: debt, isLoading: debtLoading } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'getActualDebt',
    args: [queryAddress],
    query: {
      enabled: !!address && !!routerAddress,
    },
  })

  // Get token price for proper USD calculation
  const { data: roundData } = useReadContract({
    address: addresses?.BASKET_ORACLE,
    abi: BASKET_ORACLE_ABI,
    functionName: 'latestRoundData',
    query: {
      enabled: !!addresses,
    },
  })

  const { data: cap } = useReadContract({
    address: addresses?.SYNTHETIC_SPLITTER,
    abi: PLETH_CORE_ABI,
    functionName: 'CAP',
    query: {
      enabled: !!addresses,
    },
  })

  // Calculate token price: BEAR = CAP - oracle, BULL = oracle (8 decimals)
  const oraclePrice = roundData?.[1] ?? 0n
  const capValue = cap ?? 0n
  const tokenPrice = side === 'BEAR'
    ? (capValue > oraclePrice ? capValue - oraclePrice : 0n)
    : oraclePrice

  // Collateral is the third field in Morpho position struct (staked token units, 21 decimals)
  const collateral = morphoPosition?.[2] ?? 0n
  const actualDebt = debt ?? 0n
  const hasPosition = collateral > 0n

  // Calculate collateral USD value: shares * price / 10^23 (gives 6-decimal USDC)
  // Shares have 21 effective decimals (18 + 1000x offset), price has 8 decimals
  const collateralUsdc = collateral * tokenPrice / 10n ** 23n
  const equity = collateralUsdc > actualDebt ? collateralUsdc - actualDebt : 1n
  const leverage = hasPosition && equity > 0n
    ? (collateralUsdc * 100n) / equity
    : 0n

  console.log(`[useLeveragePosition ${side}]`, {
    collateral: collateral.toString(),
    tokenPrice: tokenPrice.toString(),
    collateralUsdc: collateralUsdc.toString(),
    debt: actualDebt.toString(),
    equity: equity.toString(),
    leverage: leverage.toString(),
  })

  return {
    collateral,
    debt: actualDebt,
    leverage,
    healthFactor: 0n, // TODO: Calculate from Morpho oracle prices
    liquidationPrice: 0n, // TODO: Calculate from Morpho oracle prices
    hasPosition,
    isLoading: positionLoading || debtLoading,
    error,
    refetch,
  }
}

export function usePreviewOpenLeverage(side: 'BEAR' | 'BULL', principal: bigint, leverage: bigint) {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const routerAddress = side === 'BEAR' ? addresses?.LEVERAGE_ROUTER : addresses?.BULL_LEVERAGE_ROUTER

  const { data, isLoading, error, refetch } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'previewOpenLeverage',
    args: [principal, leverage],
    query: {
      enabled: !!routerAddress && principal > 0n && leverage > 0n,
    },
  })

  return {
    loanAmount: data?.[0] ?? 0n,
    totalUSDC: data?.[1] ?? 0n,
    expectedCollateralTokens: data?.[2] ?? 0n,
    expectedDebt: data?.[3] ?? 0n,
    isLoading,
    error,
    refetch,
  }
}

export function useOpenLeverage(side: 'BEAR' | 'BULL') {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const routerAddress = side === 'BEAR' ? addresses?.LEVERAGE_ROUTER : addresses?.BULL_LEVERAGE_ROUTER
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
      const txError = parseTransactionError(receiptError)
      updateTransaction(txIdRef.current, {
        status: 'failed',
        errorMessage: getErrorMessage(txError),
      })
      txIdRef.current = null
    }
  }, [isError, receiptError, updateTransaction])

  const openPosition = async (
    principal: bigint,
    leverage: bigint,
    maxSlippageBps: bigint,
    deadline: bigint
  ): Promise<Result<`0x${string}`, LeverageError>> => {
    if (!routerAddress) {
      return Result.err(new NotConnectedError())
    }

    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'leverage',
      status: 'pending',
      hash: undefined,
      description: `Opening ${side} leverage position`,
    })

    return Result.tryPromise({
      try: () =>
        new Promise<`0x${string}`>((resolve, reject) => {
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
                resolve(hash)
              },
              onError: (err) => {
                const txError = parseTransactionError(err)
                updateTransaction(txId, {
                  status: 'failed',
                  errorMessage: getErrorMessage(txError),
                })
                txIdRef.current = null
                reject(txError)
              },
            }
          )
        }),
      catch: (err) => {
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
  const addresses = chainId ? getAddresses(chainId) : null
  const routerAddress = side === 'BEAR' ? addresses?.LEVERAGE_ROUTER : addresses?.BULL_LEVERAGE_ROUTER
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
      const txError = parseTransactionError(receiptError)
      updateTransaction(txIdRef.current, {
        status: 'failed',
        errorMessage: getErrorMessage(txError),
      })
      txIdRef.current = null
    }
  }, [isError, receiptError, updateTransaction])

  const closePosition = async (
    collateralToWithdraw: bigint,
    maxSlippageBps: bigint,
    deadline: bigint
  ): Promise<Result<`0x${string}`, LeverageError>> => {
    if (!routerAddress) {
      return Result.err(new NotConnectedError())
    }

    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'leverage',
      status: 'pending',
      hash: undefined,
      description: `Closing ${side} leverage position`,
    })

    return Result.tryPromise({
      try: () =>
        new Promise<`0x${string}`>((resolve, reject) => {
          writeContract(
            {
              address: routerAddress,
              abi: LEVERAGE_ROUTER_ABI,
              functionName: 'closeLeverage',
              args: [collateralToWithdraw, maxSlippageBps, deadline],
            },
            {
              onSuccess: (hash) => {
                updateTransaction(txId, { hash, status: 'confirming' })
                resolve(hash)
              },
              onError: (err) => {
                const txError = parseTransactionError(err)
                updateTransaction(txId, {
                  status: 'failed',
                  errorMessage: getErrorMessage(txError),
                })
                txIdRef.current = null
                reject(txError)
              },
            }
          )
        }),
      catch: (err) => {
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
  const addresses = chainId ? getAddresses(chainId) : null
  const routerAddress = side === 'BEAR' ? addresses?.LEVERAGE_ROUTER : addresses?.BULL_LEVERAGE_ROUTER
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
      const txError = parseTransactionError(receiptError)
      updateTransaction(txIdRef.current, {
        status: 'failed',
        errorMessage: getErrorMessage(txError),
      })
      txIdRef.current = null
    }
  }, [isError, receiptError, updateTransaction])

  const addCollateral = async (amount: bigint): Promise<Result<`0x${string}`, LeverageError>> => {
    if (!routerAddress) {
      return Result.err(new NotConnectedError())
    }

    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'leverage',
      status: 'pending',
      hash: undefined,
      description: 'Adding collateral',
    })

    return Result.tryPromise({
      try: () =>
        new Promise<`0x${string}`>((resolve, reject) => {
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
                resolve(hash)
              },
              onError: (err) => {
                const txError = parseTransactionError(err)
                updateTransaction(txId, {
                  status: 'failed',
                  errorMessage: getErrorMessage(txError),
                })
                txIdRef.current = null
                reject(txError)
              },
            }
          )
        }),
      catch: (err) => {
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
  }

  const removeCollateral = async (amount: bigint): Promise<Result<`0x${string}`, LeverageError>> => {
    if (!routerAddress) {
      return Result.err(new NotConnectedError())
    }

    const txId = crypto.randomUUID()
    txIdRef.current = txId
    addTransaction({
      id: txId,
      type: 'leverage',
      status: 'pending',
      hash: undefined,
      description: 'Removing collateral',
    })

    return Result.tryPromise({
      try: () =>
        new Promise<`0x${string}`>((resolve, reject) => {
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
                resolve(hash)
              },
              onError: (err) => {
                const txError = parseTransactionError(err)
                updateTransaction(txId, {
                  status: 'failed',
                  errorMessage: getErrorMessage(txError),
                })
                txIdRef.current = null
                reject(txError)
              },
            }
          )
        }),
      catch: (err) => {
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
