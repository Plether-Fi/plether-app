import { useState, useEffect, useRef } from 'react'
import { useAccount } from 'wagmi'
import { parseUnits } from 'viem'
import { formatAmount } from '../utils/formatters'
import { getMinBalance } from '../utils/mint'
import { parseTransactionError } from '../utils/errors'
import { Alert, TokenIcon } from '../components/ui'
import { TokenInput } from '../components/TokenInput'
import { useTokenBalances, useMint, useBurn, usePreviewMint, usePreviewBurn, useAllowance, useApprove, useTransactionModal } from '../hooks'
import { getAddresses } from '../contracts/addresses'

type MintMode = 'mint' | 'redeem'
type PendingAction = 'mint' | 'burn' | null

function parsePairAmount(input: string): bigint {
  if (!input || isNaN(parseFloat(input))) return 0n
  try {
    return parseUnits(input, 18)
  } catch {
    return 0n
  }
}

export function Mint() {
  const { isConnected, chainId } = useAccount()
  const txModal = useTransactionModal()
  const [mode, setMode] = useState<MintMode>('mint')
  const [inputAmount, setInputAmount] = useState('')
  const [pendingAction, setPendingAction] = useState<PendingAction>(null)
  const pendingAmountRef = useRef<bigint>(0n)
  const usdcApproveHandledRef = useRef(false)
  const bearApproveHandledRef = useRef(false)
  const bullApproveHandledRef = useRef(false)
  const mintTriggeredRef = useRef(false)
  const burnTriggeredRef = useRef(false)

  const addresses = getAddresses(chainId ?? 1)
  const { usdcBalance, bearBalance, bullBalance, refetch: refetchBalances } = useTokenBalances()

  const pairAmountBigInt = parsePairAmount(inputAmount)

  const { allowance: usdcAllowance, refetch: refetchUsdcAllowance } = useAllowance(addresses.USDC, addresses.SYNTHETIC_SPLITTER)
  const { allowance: bearAllowance, refetch: refetchBearAllowance } = useAllowance(addresses.DXY_BEAR, addresses.SYNTHETIC_SPLITTER)
  const { allowance: bullAllowance, refetch: refetchBullAllowance } = useAllowance(addresses.DXY_BULL, addresses.SYNTHETIC_SPLITTER)

  const {
    approve: approveUsdc,
    isPending: usdcApprovePending,
    isConfirming: usdcApproveConfirming,
    isSuccess: usdcApproveSuccess,
    error: usdcApproveError,
  } = useApprove(addresses.USDC, addresses.SYNTHETIC_SPLITTER)
  const {
    approve: approveBear,
    isPending: bearApprovePending,
    isConfirming: bearApproveConfirming,
    isSuccess: bearApproveSuccess,
    error: bearApproveError,
  } = useApprove(addresses.DXY_BEAR, addresses.SYNTHETIC_SPLITTER)
  const {
    approve: approveBull,
    isPending: bullApprovePending,
    isConfirming: bullApproveConfirming,
    isSuccess: bullApproveSuccess,
    error: bullApproveError,
  } = useApprove(addresses.DXY_BULL, addresses.SYNTHETIC_SPLITTER)

  const {
    mint,
    isPending: mintPending,
    isConfirming: mintConfirming,
    isSuccess: mintSuccess,
    error: mintError,
    reset: resetMint,
    hash: mintHash,
  } = useMint()
  const {
    burn,
    isPending: burnPending,
    isConfirming: burnConfirming,
    isSuccess: burnSuccess,
    error: burnError,
    reset: resetBurn,
    hash: burnHash,
  } = useBurn()

  const { usdcRequired, isLoading: previewMintLoading } = usePreviewMint(pairAmountBigInt)
  const { usdcToReturn, isLoading: previewBurnLoading } = usePreviewBurn(pairAmountBigInt)

  const needsUsdcApproval = mode === 'mint' && usdcRequired > 0n && usdcAllowance < usdcRequired
  const needsBearApproval = mode === 'redeem' && pairAmountBigInt > 0n && bearAllowance < pairAmountBigInt
  const needsBullApproval = mode === 'redeem' && pairAmountBigInt > 0n && bullAllowance < pairAmountBigInt

  useEffect(() => {
    if (usdcApproveSuccess && !usdcApproveHandledRef.current) {
      usdcApproveHandledRef.current = true
      void refetchUsdcAllowance()
      if (pendingAction === 'mint' && pendingAmountRef.current > 0n) {
        mintTriggeredRef.current = true
        void mint(pendingAmountRef.current)
        pendingAmountRef.current = 0n
        setPendingAction(null)
      }
    }
  }, [usdcApproveSuccess, refetchUsdcAllowance, pendingAction, mint])

  useEffect(() => {
    if (bearApproveSuccess && !bearApproveHandledRef.current) {
      bearApproveHandledRef.current = true
      void refetchBearAllowance()
      if (pendingAction === 'burn' && pendingAmountRef.current > 0n) {
        if (bullAllowance < pendingAmountRef.current) {
          void approveBull(pendingAmountRef.current)
        } else {
          burnTriggeredRef.current = true
          void burn(pendingAmountRef.current)
          pendingAmountRef.current = 0n
          setPendingAction(null)
        }
      }
    }
  }, [bearApproveSuccess, refetchBearAllowance, pendingAction, bullAllowance, approveBull, burn])

  useEffect(() => {
    if (bullApproveSuccess && !bullApproveHandledRef.current) {
      bullApproveHandledRef.current = true
      void refetchBullAllowance()
      if (pendingAction === 'burn' && pendingAmountRef.current > 0n) {
        burnTriggeredRef.current = true
        void burn(pendingAmountRef.current)
        pendingAmountRef.current = 0n
        setPendingAction(null)
      }
    }
  }, [bullApproveSuccess, refetchBullAllowance, pendingAction, burn])

  useEffect(() => {
    if (mintSuccess) {
      if (mintHash) txModal.setSuccess(mintHash)
      void refetchBalances()
      setInputAmount('')
      resetMint()
      mintTriggeredRef.current = false
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [mintSuccess, mintHash, refetchBalances, resetMint])

  useEffect(() => {
    if (burnSuccess) {
      if (burnHash) txModal.setSuccess(burnHash)
      void refetchBalances()
      setInputAmount('')
      resetBurn()
      burnTriggeredRef.current = false
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [burnSuccess, burnHash, refetchBalances, resetBurn])

  useEffect(() => {
    const modal = useTransactionModal.getState()
    if (!modal.isOpen) return
    if (usdcApprovePending) {
      modal.setStepInProgress(0)
    } else if (usdcApproveConfirming) {
      modal.setStepInProgress(1)
    } else if (usdcApproveError) {
      modal.setError(0, parseTransactionError(usdcApproveError))
    }
  }, [usdcApprovePending, usdcApproveConfirming, usdcApproveError])

  useEffect(() => {
    const modal = useTransactionModal.getState()
    if (!modal.isOpen || !mintTriggeredRef.current) return
    const stepOffset = needsUsdcApproval || usdcApproveSuccess ? 2 : 0
    if (mintPending) {
      modal.setStepInProgress(stepOffset)
    } else if (mintConfirming) {
      modal.setStepInProgress(stepOffset + 1)
    } else if (mintError) {
      modal.setError(stepOffset, parseTransactionError(mintError))
    }
  }, [mintPending, mintConfirming, mintError, needsUsdcApproval, usdcApproveSuccess])

  useEffect(() => {
    const modal = useTransactionModal.getState()
    if (!modal.isOpen) return
    if (bearApprovePending) {
      modal.setStepInProgress(0)
    } else if (bearApproveConfirming) {
      modal.setStepInProgress(1)
    } else if (bearApproveError) {
      modal.setError(0, parseTransactionError(bearApproveError))
    }
  }, [bearApprovePending, bearApproveConfirming, bearApproveError])

  useEffect(() => {
    const modal = useTransactionModal.getState()
    if (!modal.isOpen) return
    if (bullApprovePending) {
      modal.setStepInProgress(2)
    } else if (bullApproveConfirming) {
      modal.setStepInProgress(3)
    } else if (bullApproveError) {
      modal.setError(2, parseTransactionError(bullApproveError))
    }
  }, [bullApprovePending, bullApproveConfirming, bullApproveError])

  useEffect(() => {
    const modal = useTransactionModal.getState()
    if (!modal.isOpen || !burnTriggeredRef.current) return
    const stepOffset = (needsBearApproval || bearApproveSuccess ? 2 : 0) + (needsBullApproval || bullApproveSuccess ? 2 : 0)
    if (burnPending) {
      modal.setStepInProgress(stepOffset)
    } else if (burnConfirming) {
      modal.setStepInProgress(stepOffset + 1)
    } else if (burnError) {
      modal.setError(stepOffset, parseTransactionError(burnError))
    }
  }, [burnPending, burnConfirming, burnError, needsBearApproval, bearApproveSuccess, needsBullApproval, bullApproveSuccess])

  const isPreviewLoading = mode === 'mint' ? previewMintLoading : previewBurnLoading
  const previewAmount = mode === 'mint' ? usdcRequired : usdcToReturn
  const outputDisplay = isPreviewLoading && parseFloat(inputAmount) > 0
    ? '...'
    : formatAmount(previewAmount, 6)
  const minBalance = getMinBalance(bearBalance, bullBalance)

  const handleMint = async () => {
    if (pairAmountBigInt <= 0n) return
    usdcApproveHandledRef.current = false
    mintTriggeredRef.current = false

    if (needsUsdcApproval) {
      txModal.open({
        title: 'Minting token pairs',
        steps: ['Approve USDC', 'Confirming approval', 'Mint pairs', 'Awaiting confirmation'],
        onRetry: () => void handleMint(),
      })
      pendingAmountRef.current = pairAmountBigInt
      setPendingAction('mint')
      await approveUsdc(usdcRequired)
      return
    }

    txModal.open({
      title: 'Minting token pairs',
      steps: ['Mint pairs', 'Awaiting confirmation'],
      onRetry: () => void handleMint(),
    })
    mintTriggeredRef.current = true
    await mint(pairAmountBigInt)
  }

  const handleRedeem = async () => {
    bearApproveHandledRef.current = false
    bullApproveHandledRef.current = false
    burnTriggeredRef.current = false

    const steps: string[] = []
    if (needsBearApproval) {
      steps.push('Approve DXY-BEAR', 'Confirming approval')
    }
    if (needsBullApproval) {
      steps.push('Approve DXY-BULL', 'Confirming approval')
    }
    steps.push('Redeem pairs', 'Awaiting confirmation')

    txModal.open({
      title: 'Redeeming token pairs',
      steps,
      onRetry: () => void handleRedeem(),
    })

    if (needsBearApproval) {
      pendingAmountRef.current = pairAmountBigInt
      setPendingAction('burn')
      await approveBear(pairAmountBigInt)
      return
    }
    if (needsBullApproval) {
      pendingAmountRef.current = pairAmountBigInt
      setPendingAction('burn')
      await approveBull(pairAmountBigInt)
      return
    }
    burnTriggeredRef.current = true
    await burn(pairAmountBigInt)
  }

  const getMintButtonText = () => {
    if (mintPending) return 'Minting...'
    if (usdcApprovePending) return 'Approving USDC...'
    if (usdcRequired > usdcBalance) return 'Insufficient USDC'
    if (needsUsdcApproval) return 'Approve USDC'
    return 'Mint Pairs'
  }

  const getRedeemButtonText = () => {
    if (burnPending) return 'Redeeming...'
    if (bearApprovePending) return 'Approving DXY-BEAR...'
    if (bullApprovePending) return 'Approving DXY-BULL...'
    if (pairAmountBigInt > minBalance) return 'Insufficient Balance'
    if (needsBearApproval) return 'Approve DXY-BEAR'
    if (needsBullApproval) return 'Approve DXY-BULL'
    return 'Redeem for USDC'
  }

  const insufficientBalance = mode === 'mint'
    ? usdcRequired > usdcBalance
    : pairAmountBigInt > minBalance

  const isActionDisabled = !inputAmount || parseFloat(inputAmount) <= 0 ||
    mintPending || burnPending || usdcApprovePending || bearApprovePending || bullApprovePending ||
    insufficientBalance

  return (
    <div className="space-y-10 max-w-xl mx-auto">
      <div className="mb-8">
        <h1 className="text-3xl font-semibold text-cyber-text-primary mb-1">Mint & Redeem</h1>
        <p className="text-cyber-text-secondary font-light">Create or redeem DXY-BEAR + DXY-BULL pairs</p>
      </div>

      <div className="bg-cyber-surface-dark  border border-cyber-border-glow/30 shadow-lg shadow-cyber-border-glow/10 overflow-hidden">
        <div className="flex border-b border-cyber-border-glow/30">
          <button
            onClick={() => { setMode('mint'); setInputAmount('') }}
            className={`flex-1 flex items-center justify-center gap-2 px-6 py-4 text-sm font-semibold transition-colors ${
              mode === 'mint'
                ? 'bg-cyber-surface-light text-cyber-neon-green border-b-2 border-cyber-neon-green'
                : 'text-cyber-text-secondary hover:text-cyber-bright-blue border-b-2 border-transparent'
            }`}
          >
            <span className="material-symbols-outlined text-xl">add_circle</span>
            Mint Pairs
          </button>
          <button
            onClick={() => { setMode('redeem'); setInputAmount('') }}
            className={`flex-1 flex items-center justify-center gap-2 px-6 py-4 text-sm font-semibold transition-colors ${
              mode === 'redeem'
                ? 'bg-cyber-surface-light text-cyber-electric-fuchsia border-b-2 border-cyber-electric-fuchsia'
                : 'text-cyber-text-secondary hover:text-cyber-bright-blue border-b-2 border-transparent'
            }`}
          >
            <span className="material-symbols-outlined text-xl">swap_horiz</span>
            Redeem
          </button>
        </div>

        <div className="p-6 md:p-8 space-y-6">
          {mode === 'mint' ? (
            <>
              <Alert variant="info">
                Mint equal amounts of DXY-BEAR and DXY-BULL from USDC.
                You'll receive both tokens in a 1:1 ratio.
              </Alert>

              <TokenInput
                label="Pairs to mint (of each token)"
                value={inputAmount}
                onChange={setInputAmount}
                token={{ symbol: 'PAIR', decimals: 18 }}
                balance={undefined}
              />

              <div className="flex justify-center -my-2 z-10 relative">
                <div className="bg-cyber-surface-light p-2 rounded-full border border-cyber-border-glow/30">
                  <span className="material-symbols-outlined text-cyber-bright-blue text-lg block">arrow_downward</span>
                </div>
              </div>

              <div className="bg-cyber-surface-light p-4 space-y-3 border border-cyber-border-glow/30">
                <p className="text-sm text-cyber-text-secondary">You will receive:</p>
                <div className="flex justify-between items-center">
                  <div className="flex items-center gap-2">
                    <TokenIcon side="BEAR" size="sm" />
                    <span className="text-cyber-electric-fuchsia font-medium">DXY-BEAR</span>
                  </div>
                  <span className="text-cyber-text-primary font-semibold">{inputAmount || '0'}</span>
                </div>
                <div className="flex justify-between items-center">
                  <div className="flex items-center gap-2">
                    <TokenIcon side="BULL" size="sm" />
                    <span className="text-cyber-neon-green font-medium">DXY-BULL</span>
                  </div>
                  <span className="text-cyber-text-primary font-semibold">{inputAmount || '0'}</span>
                </div>
                <div className="border-t border-cyber-border-glow/30 pt-3 mt-3">
                  <div className="flex justify-between items-center">
                    <span className="text-cyber-text-secondary">USDC required</span>
                    <span className="text-cyber-text-primary font-semibold text-lg">{outputDisplay} USDC</span>
                  </div>
                  <div className="flex justify-between items-center text-sm mt-1">
                    <span className="text-cyber-text-secondary">Your balance</span>
                    <span className="text-cyber-text-secondary">{formatAmount(usdcBalance, 6)} USDC</span>
                  </div>
                </div>
              </div>

              {isConnected ? (
                <button
                  onClick={() => void handleMint()}
                  disabled={isActionDisabled}
                  className="w-full bg-cyber-neon-green hover:bg-cyber-neon-green/90 text-cyber-bg font-semibold py-4 px-6  shadow-lg shadow-cyber-neon-green/40 transition-all transform hover:-translate-y-0.5 active:translate-y-0 text-lg disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none disabled:shadow-none"
                >
                  {getMintButtonText()}
                </button>
              ) : (
                <button
                  disabled
                  className="w-full bg-cyber-surface-light text-cyber-text-secondary font-semibold py-4 px-6  cursor-not-allowed"
                >
                  Connect Wallet to Mint
                </button>
              )}
            </>
          ) : (
            <>
              <Alert variant="warning" icon="info">
                Redeem equal amounts of DXY-BEAR and DXY-BULL to get back USDC.
                You need equal amounts of both tokens.
              </Alert>

              <div className="bg-cyber-surface-light  p-4 space-y-3 border border-cyber-border-glow/30">
                <p className="text-sm text-cyber-text-secondary">Your balances:</p>
                <div className="flex justify-between items-center">
                  <div className="flex items-center gap-2">
                    <TokenIcon side="BEAR" size="sm" />
                    <span className="text-cyber-electric-fuchsia font-medium">DXY-BEAR</span>
                  </div>
                  <span className="text-cyber-text-primary font-semibold">{formatAmount(bearBalance, 18)}</span>
                </div>
                <div className="flex justify-between items-center">
                  <div className="flex items-center gap-2">
                    <TokenIcon side="BULL" size="sm" />
                    <span className="text-cyber-neon-green font-medium">DXY-BULL</span>
                  </div>
                  <span className="text-cyber-text-primary font-semibold">{formatAmount(bullBalance, 18)}</span>
                </div>
              </div>

              <TokenInput
                label="Amount to redeem (of each token)"
                value={inputAmount}
                onChange={setInputAmount}
                token={{ symbol: 'PAIR', decimals: 18 }}
                balance={minBalance}
                balanceLabel="Max:"
              />

              <div className="flex justify-center -my-2 z-10 relative">
                <div className="bg-cyber-surface-light p-2 rounded-full border border-cyber-border-glow/30">
                  <span className="material-symbols-outlined text-cyber-bright-blue text-lg block">arrow_downward</span>
                </div>
              </div>

              <div className="bg-cyber-surface-light  p-4 border border-cyber-border-glow/30">
                <div className="flex justify-between items-center">
                  <span className="text-cyber-text-secondary">You will receive</span>
                  <span className="text-cyber-text-primary font-semibold text-lg">{outputDisplay} USDC</span>
                </div>
              </div>

              {isConnected ? (
                <button
                  onClick={() => void handleRedeem()}
                  disabled={isActionDisabled}
                  className="w-full bg-cyber-electric-fuchsia hover:bg-cyber-electric-fuchsia/90 text-cyber-text-primary font-semibold py-4 px-6  shadow-lg shadow-cyber-electric-fuchsia/40 transition-all transform hover:-translate-y-0.5 active:translate-y-0 text-lg disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none disabled:shadow-none"
                >
                  {getRedeemButtonText()}
                </button>
              ) : (
                <button
                  disabled
                  className="w-full bg-cyber-surface-light text-cyber-text-secondary font-semibold py-4 px-6  cursor-not-allowed"
                >
                  Connect Wallet to Redeem
                </button>
              )}
            </>
          )}
        </div>
      </div>
    </div>
  )
}
