import { useAccount, useReadContract, useWriteContract, usePublicClient } from 'wagmi'
import { useRef, useEffect, useMemo, useState, useCallback } from 'react'
import { zeroAddress, keccak256, encodeAbiParameters } from 'viem'
import { Result } from 'better-result'
import { LEVERAGE_ROUTER_ABI, MORPHO_ABI, PLETH_CORE_ABI, BASKET_ORACLE_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'
import { useTransactionStore } from '../stores/transactionStore'
import { useTransactionModal } from './useTransactionModal'
import {
  parseTransactionError,
  getErrorMessage,
  type TransactionError,
} from '../utils/errors'
import { NotConnectedError } from './usePlethCore'
import { useContractTransaction } from './useContractTransaction'

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
  const { data: morphoPosition, isLoading: positionLoading, error, refetch: refetchPosition } = useReadContract({
    address: morphoAddress,
    abi: MORPHO_ABI,
    functionName: 'position',
    args: [marketId ?? '0x', queryAddress],
    query: {
      enabled: !!address && !!morphoAddress && !!marketId,
    },
  })

  // Get debt from router (includes accrued interest)
  const { data: debt, isLoading: debtLoading, refetch: refetchDebt } = useReadContract({
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

  // Basket oracle returns BEAR price directly, BULL = CAP - BEAR (8 decimals)
  const oraclePrice = roundData?.[1] ?? 0n
  const capValue = cap ?? 0n
  const tokenPrice = side === 'BEAR'
    ? oraclePrice
    : (capValue > oraclePrice ? capValue - oraclePrice : 0n)

  // LLTV from market params (18 decimals, e.g., 915000000000000000 = 91.5%)
  const lltv = marketParams?.[4] ?? 0n

  // Collateral is the third field in Morpho position struct (staked token units, 21 decimals)
  const collateral = morphoPosition?.[2] ?? 0n
  const actualDebt = debt ?? 0n
  const hasPosition = collateral > 0n

  // Calculate collateral USD value: shares (21 dec) * price (8 dec) / 10^23 = USDC (6 dec)
  const collateralUsdc = collateral * tokenPrice / 10n ** 23n
  const equity = collateralUsdc > actualDebt ? collateralUsdc - actualDebt : 1n
  const leverage = hasPosition && equity > 0n
    ? (collateralUsdc * 100n) / equity
    : 0n

  // Health Factor = (collateralUsdc * LLTV) / (debt * 10^18)
  // Multiply by 100 first to preserve 2 decimal places
  const healthFactorScaled = actualDebt > 0n && lltv > 0n
    ? (collateralUsdc * lltv * 100n) / (actualDebt * 10n ** 18n)
    : 0n
  const healthFactor = Number(healthFactorScaled) / 100

  // Liquidation Price: price at which health = 1
  // collateral * liqPrice * LLTV / 10^23 = debt * 10^18
  // liqPrice = debt * 10^41 / (collateral * LLTV)
  // Result is in 8 decimals (oracle format), convert to 6 decimals (USDC) by dividing by 100
  const liquidationPriceRaw = hasPosition && collateral > 0n && lltv > 0n
    ? (actualDebt * 10n ** 41n) / (collateral * lltv)
    : 0n
  // For BULL, liquidation happens when BULL price drops, which means BEAR price rises
  // BULL liqPrice = CAP - BEAR liqPrice (but we want the BULL price threshold)
  const liquidationPrice = side === 'BEAR'
    ? liquidationPriceRaw / 100n  // Convert 8 dec to 6 dec for display
    : (capValue > liquidationPriceRaw ? (capValue - liquidationPriceRaw) / 100n : 0n)

  const refetch = async () => {
    await Promise.all([refetchPosition(), refetchDebt()])
  }

  return {
    collateral,
    collateralUsdc,
    debt: actualDebt,
    leverage,
    healthFactor,
    liquidationPrice,
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
  const { sendTransaction, ...txState } = useContractTransaction()

  const openPosition = async (
    principal: bigint,
    leverage: bigint,
    maxSlippageBps: bigint,
    deadline: bigint
  ): Promise<Result<`0x${string}`, LeverageError>> => {
    if (!routerAddress) {
      return Result.err(new NotConnectedError())
    }

    return sendTransaction(
      { type: 'leverage', title: `Opening ${side} leverage position`,
        steps: [{ label: 'Open position' }, { label: 'Confirming onchain (~12s)' }] },
      { address: routerAddress, abi: LEVERAGE_ROUTER_ABI, functionName: 'openLeverage',
        args: [principal, leverage, maxSlippageBps, deadline] },
    )
  }

  return { openPosition, ...txState }
}

export function useCloseLeverage(side: 'BEAR' | 'BULL') {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const routerAddress = side === 'BEAR' ? addresses?.LEVERAGE_ROUTER : addresses?.BULL_LEVERAGE_ROUTER
  const { sendTransaction, ...txState } = useContractTransaction()

  const closePosition = async (
    collateralToWithdraw: bigint,
    maxSlippageBps: bigint,
    deadline: bigint
  ): Promise<Result<`0x${string}`, LeverageError>> => {
    if (!routerAddress) {
      return Result.err(new NotConnectedError())
    }

    return sendTransaction(
      { type: 'leverage', title: `Closing ${side} leverage position`,
        steps: [{ label: 'Close position' }, { label: 'Confirming onchain (~12s)' }] },
      { address: routerAddress, abi: LEVERAGE_ROUTER_ABI, functionName: 'closeLeverage',
        args: [collateralToWithdraw, maxSlippageBps, deadline] },
    )
  }

  return { closePosition, ...txState }
}

export function useAdjustCollateral(side: 'BEAR' | 'BULL', onSuccessCallback?: () => void) {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const routerAddress = side === 'BEAR' ? addresses?.LEVERAGE_ROUTER : addresses?.BULL_LEVERAGE_ROUTER
  const addTransaction = useTransactionStore((s) => s.addTransaction)
  const setStepInProgress = useTransactionStore((s) => s.setStepInProgress)
  const setStepSuccess = useTransactionStore((s) => s.setStepSuccess)
  const setStepError = useTransactionStore((s) => s.setStepError)
  const txModal = useTransactionModal()
  const publicClient = usePublicClient()

  const { writeContractAsync, isPending, error, reset: resetWrite } = useWriteContract()

  const [isConfirming, setIsConfirming] = useState(false)
  const [isSuccess, setIsSuccess] = useState(false)
  const [hash, setHash] = useState<`0x${string}` | undefined>(undefined)

  // Store callback in ref so it can be called after component unmounts
  const onSuccessRef = useRef(onSuccessCallback)
  useEffect(() => {
    onSuccessRef.current = onSuccessCallback
  }, [onSuccessCallback])

  const addCollateral = useCallback(async (
    usdcAmount: bigint,
    maxSlippageBps: bigint,
    deadline: bigint,
    txContext?: { txId: string; stepIndex: number }
  ): Promise<Result<`0x${string}`, LeverageError>> => {
    if (!routerAddress) {
      return Result.err(new NotConnectedError())
    }

    const txId = txContext?.txId ?? crypto.randomUUID()
    const stepIndex = txContext?.stepIndex ?? 0

    if (!txContext) {
      addTransaction({
        id: txId,
        type: 'leverage',
        status: 'pending',
        hash: undefined,
        title: 'Adding collateral',
        steps: [
          { label: 'Add collateral', status: 'pending' },
          { label: 'Confirming onchain (~12s)', status: 'pending' },
        ],
      })
      txModal.open({ transactionId: txId })
      setStepInProgress(txId, stepIndex)
    }

    setIsSuccess(false)
    setIsConfirming(false)
    setHash(undefined)

    return Result.tryPromise({
      try: async () => {
        const txHash = await writeContractAsync({
          address: routerAddress,
          abi: LEVERAGE_ROUTER_ABI,
          functionName: 'addCollateral',
          args: [usdcAmount, maxSlippageBps, deadline],
        })

        setHash(txHash)
        setIsConfirming(true)
        // Move to "Awaiting confirmation" step
        const confirmStepIndex = txContext ? stepIndex + 1 : 1
        setStepInProgress(txId, confirmStepIndex)

        const receipt = await publicClient.waitForTransactionReceipt({ hash: txHash })

        if (receipt.status === 'reverted') {
          throw new Error('Transaction reverted')
        }

        setStepSuccess(txId, txHash)
        setIsConfirming(false)
        setIsSuccess(true)
        onSuccessRef.current?.()

        return txHash
      },
      catch: (err) => {
        setIsConfirming(false)
        const txError = err instanceof Error && '_tag' in err
          ? err as TransactionError
          : parseTransactionError(err)
        setStepError(txId, stepIndex, getErrorMessage(txError))
        return txError
      },
    })
  }, [routerAddress, addTransaction, setStepInProgress, setStepSuccess, setStepError, txModal, writeContractAsync, publicClient])

  const removeCollateral = useCallback(async (
    collateralToWithdraw: bigint,
    maxSlippageBps: bigint,
    deadline: bigint
  ): Promise<Result<`0x${string}`, LeverageError>> => {
    if (!routerAddress) {
      return Result.err(new NotConnectedError())
    }

    const txId = crypto.randomUUID()
    addTransaction({
      id: txId,
      type: 'leverage',
      status: 'pending',
      hash: undefined,
      title: 'Removing collateral',
      steps: [
        { label: 'Remove collateral', status: 'pending' },
        { label: 'Confirming onchain (~12s)', status: 'pending' },
      ],
    })
    txModal.open({ transactionId: txId })
    setStepInProgress(txId, 0)

    setIsSuccess(false)
    setIsConfirming(false)
    setHash(undefined)

    return Result.tryPromise({
      try: async () => {
        const txHash = await writeContractAsync({
          address: routerAddress,
          abi: LEVERAGE_ROUTER_ABI,
          functionName: 'removeCollateral',
          args: [collateralToWithdraw, maxSlippageBps, deadline],
        })

        setHash(txHash)
        setIsConfirming(true)
        // Move to "Awaiting confirmation" step
        setStepInProgress(txId, 1)

        const receipt = await publicClient.waitForTransactionReceipt({ hash: txHash })

        if (receipt.status === 'reverted') {
          throw new Error('Transaction reverted')
        }

        setStepSuccess(txId, txHash)
        setIsConfirming(false)
        setIsSuccess(true)
        onSuccessRef.current?.()

        return txHash
      },
      catch: (err) => {
        setIsConfirming(false)
        const txError = err instanceof Error && '_tag' in err
          ? err as TransactionError
          : parseTransactionError(err)
        setStepError(txId, 0, getErrorMessage(txError))
        return txError
      },
    })
  }, [routerAddress, addTransaction, setStepInProgress, setStepSuccess, setStepError, txModal, writeContractAsync, publicClient])

  const reset = useCallback(() => {
    resetWrite()
    setIsConfirming(false)
    setIsSuccess(false)
    setHash(undefined)
  }, [resetWrite])

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
