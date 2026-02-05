import { useCallback } from 'react'
import { useAccount, useWriteContract } from 'wagmi'
import { type Address } from 'viem'
import { useAllowance } from './useAllowance'
import { usePreviewMint, usePreviewBurn } from './usePlethCore'
import { useTransactionSequence, type TransactionStep } from './useTransactionSequence'
import { getAddresses, DEFAULT_CHAIN_ID } from '../contracts/addresses'
import { ERC20_ABI, PLETH_CORE_ABI } from '../contracts/abis'

interface UseMintFlowOptions {
  onSuccess?: () => void
}

export function useMintFlow(pairAmount: bigint, options: UseMintFlowOptions = {}) {
  const { onSuccess } = options
  const { chainId } = useAccount()
  const addresses = getAddresses(chainId ?? DEFAULT_CHAIN_ID)

  const { writeContractAsync } = useWriteContract()
  const sequence = useTransactionSequence()

  const { allowance: usdcAllowance, refetch: refetchUsdcAllowance } = useAllowance(
    addresses.USDC,
    addresses.SYNTHETIC_SPLITTER
  )
  const { allowance: bearAllowance, refetch: refetchBearAllowance } = useAllowance(
    addresses.DXY_BEAR,
    addresses.SYNTHETIC_SPLITTER
  )
  const { allowance: bullAllowance, refetch: refetchBullAllowance } = useAllowance(
    addresses.DXY_BULL,
    addresses.SYNTHETIC_SPLITTER
  )

  const { usdcRequired, isLoading: previewMintLoading } = usePreviewMint(pairAmount)
  const { usdcToReturn, isLoading: previewBurnLoading } = usePreviewBurn(pairAmount)

  const needsUsdcApproval = usdcRequired > 0n && usdcAllowance < usdcRequired
  const needsBearApproval = pairAmount > 0n && bearAllowance < pairAmount
  const needsBullApproval = pairAmount > 0n && bullAllowance < pairAmount

  const createApproveStep = useCallback((
    token: Address,
    spender: Address,
    amount: bigint,
    label: string,
    refetch: () => Promise<unknown>
  ): TransactionStep => ({
    label,
    action: async () => {
      const hash = await writeContractAsync({
        address: token,
        abi: ERC20_ABI,
        functionName: 'approve',
        args: [spender, amount],
      })
      await refetch()
      return hash
    },
  }), [writeContractAsync])

  const buildMintSteps = useCallback((): TransactionStep[] => {
    const steps: TransactionStep[] = []

    if (needsUsdcApproval) {
      steps.push(createApproveStep(
        addresses.USDC,
        addresses.SYNTHETIC_SPLITTER,
        usdcRequired,
        'Approve USDC',
        refetchUsdcAllowance
      ))
    }

    steps.push({
      label: 'Mint pairs',
      action: () => writeContractAsync({
        address: addresses.SYNTHETIC_SPLITTER,
        abi: PLETH_CORE_ABI,
        functionName: 'mint',
        args: [pairAmount],
      }),
    })

    return steps
  }, [
    pairAmount,
    needsUsdcApproval,
    usdcRequired,
    addresses,
    createApproveStep,
    refetchUsdcAllowance,
    writeContractAsync,
  ])

  const handleMint = useCallback(() => {
    if (pairAmount <= 0n) return

    void sequence.execute({
      title: 'Minting token pairs',
      type: 'mint',
      buildSteps: buildMintSteps,
      onSuccess,
    })
  }, [pairAmount, sequence, buildMintSteps, onSuccess])

  const buildRedeemSteps = useCallback((): TransactionStep[] => {
    const steps: TransactionStep[] = []

    if (needsBearApproval) {
      steps.push(createApproveStep(
        addresses.DXY_BEAR,
        addresses.SYNTHETIC_SPLITTER,
        pairAmount,
        'Approve plDXY-BEAR',
        refetchBearAllowance
      ))
    }

    if (needsBullApproval) {
      steps.push(createApproveStep(
        addresses.DXY_BULL,
        addresses.SYNTHETIC_SPLITTER,
        pairAmount,
        'Approve plDXY-BULL',
        refetchBullAllowance
      ))
    }

    steps.push({
      label: 'Redeem pairs',
      action: () => writeContractAsync({
        address: addresses.SYNTHETIC_SPLITTER,
        abi: PLETH_CORE_ABI,
        functionName: 'burn',
        args: [pairAmount],
      }),
    })

    return steps
  }, [
    pairAmount,
    needsBearApproval,
    needsBullApproval,
    addresses,
    createApproveStep,
    refetchBearAllowance,
    refetchBullAllowance,
    writeContractAsync,
  ])

  const handleRedeem = useCallback(() => {
    if (pairAmount <= 0n) return

    void sequence.execute({
      title: 'Redeeming token pairs',
      type: 'burn',
      buildSteps: buildRedeemSteps,
      onSuccess,
    })
  }, [pairAmount, sequence, buildRedeemSteps, onSuccess])

  return {
    // State
    isRunning: sequence.isRunning,
    isSuccess: sequence.isSuccess,
    currentStepIndex: sequence.currentStepIndex,
    error: sequence.error,

    // Computed values
    usdcRequired,
    usdcToReturn,
    previewMintLoading,
    previewBurnLoading,
    needsUsdcApproval,
    needsBearApproval,
    needsBullApproval,

    // Actions
    handleMint,
    handleRedeem,
    reset: sequence.reset,
  }
}
