import { useState, useCallback } from 'react'
import { useAccount } from 'wagmi'
import { parseUnits } from 'viem'
import { formatAmount, formatUsd } from '../utils/formatters'
import { getMinBalance } from '../utils/mint'
import { Alert, TokenIcon } from '../components/ui'
import { TokenInput } from '../components/TokenInput'
import { useUserDashboard, useProtocolStatus, useMintQuote, useBurnQuote, apiQueryKeys } from '../api'
import { useQueryClient } from '@tanstack/react-query'
import { useTransactionStore } from '../stores/transactionStore'
import { transactionManager } from '../services/transactionManager'

type MintMode = 'mint' | 'redeem'

function parsePairAmount(input: string): bigint {
  if (!input || isNaN(parseFloat(input))) return 0n
  try {
    return parseUnits(input, 18)
  } catch {
    return 0n
  }
}

export function Mint() {
  const { isConnected, address } = useAccount()
  const txStore = useTransactionStore()
  const queryClient = useQueryClient()

  const [mode, setMode] = useState<MintMode>('mint')
  const [inputAmount, setInputAmount] = useState('')

  const { data: protocolData } = useProtocolStatus()
  const isProtocolActive = protocolData?.data.status === 'ACTIVE'

  const { data: dashboardData } = useUserDashboard(address)
  const balances = dashboardData?.data.balances

  const usdcBalance = balances ? BigInt(balances.usdc) : 0n
  const bearBalance = balances ? BigInt(balances.bear) : 0n
  const bullBalance = balances ? BigInt(balances.bull) : 0n

  const refetchBalances = useCallback(() => {
    if (address) {
      void queryClient.invalidateQueries({ queryKey: apiQueryKeys.user.dashboard(address) })
    }
  }, [address, queryClient])

  const pairAmountBigInt = parsePairAmount(inputAmount)

  const mintOperationKey = 'mint'
  const redeemOperationKey = 'redeem'

  const mintTransactionId = txStore.activeOperations[mintOperationKey]
  const redeemTransactionId = txStore.activeOperations[redeemOperationKey]

  const mintTx = mintTransactionId
    ? txStore.transactions.find(t => t.id === mintTransactionId)
    : null
  const redeemTx = redeemTransactionId
    ? txStore.transactions.find(t => t.id === redeemTransactionId)
    : null

  const isMintPending = mintTx?.status === 'pending' || mintTx?.status === 'confirming'
  const isRedeemPending = redeemTx?.status === 'pending' || redeemTx?.status === 'confirming'
  const isRunning = isMintPending || isRedeemPending

  const amountStr = pairAmountBigInt > 0n ? pairAmountBigInt.toString() : undefined
  const { data: mintQuoteData, isLoading: previewMintLoading } = useMintQuote(amountStr)
  const { data: burnQuoteData, isLoading: previewBurnLoading } = useBurnQuote(amountStr)
  const usdcRequired = mintQuoteData ? BigInt(mintQuoteData.data.usdcIn) : 0n
  const usdcToReturn = burnQuoteData ? BigInt(burnQuoteData.data.usdcOut) : 0n

  const handleMint = useCallback(() => {
    if (pairAmountBigInt <= 0n) return

    void transactionManager.executeMint(pairAmountBigInt, usdcRequired, {
      onRetry: handleMint,
    }).then(() => {
      refetchBalances()
      setInputAmount('')
    })
  }, [pairAmountBigInt, usdcRequired, refetchBalances])

  const handleRedeem = useCallback(() => {
    if (pairAmountBigInt <= 0n) return

    void transactionManager.executeRedeem(pairAmountBigInt, {
      onRetry: handleRedeem,
    }).then(() => {
      refetchBalances()
      setInputAmount('')
    })
  }, [pairAmountBigInt, refetchBalances])

  const isPreviewLoading = mode === 'mint' ? previewMintLoading : previewBurnLoading
  const previewAmount = mode === 'mint' ? usdcRequired : usdcToReturn
  const outputDisplay = isPreviewLoading && parseFloat(inputAmount) > 0
    ? '...'
    : formatUsd(previewAmount)
  const minBalance = getMinBalance(bearBalance, bullBalance)

  const getMintButtonText = () => {
    if (isMintPending) return 'Processing...'
    if (usdcRequired > usdcBalance) return 'Insufficient USDC'
    return 'Mint Pairs'
  }

  const getRedeemButtonText = () => {
    if (isRedeemPending) return 'Processing...'
    if (pairAmountBigInt > minBalance) return 'Insufficient Balance'
    return 'Redeem for USDC'
  }

  const insufficientBalance = mode === 'mint'
    ? usdcRequired > usdcBalance
    : pairAmountBigInt > minBalance

  const isPaused = !isProtocolActive && mode === 'mint'
  const isActionDisabled = !inputAmount || parseFloat(inputAmount) <= 0 || isRunning || insufficientBalance || isPaused

  return (
    <div className="mx-auto min-w-0 max-w-xl space-y-6 sm:space-y-10">
      <div className="mb-6 sm:mb-8">
        <h1 className="mb-1 text-2xl font-semibold text-content-primary sm:text-3xl">Mint & Redeem</h1>
        <p className="text-sm font-light text-content-secondary sm:text-base">Create or redeem plDXY-BEAR + plDXY-BULL pairs</p>
      </div>

      {!isProtocolActive && protocolData && mode === 'mint' && (
        <Alert variant="warning" icon="warning">
          Protocol is currently {protocolData.data.status.toLowerCase()}. Minting is disabled.
        </Alert>
      )}

      <div className="bg-surface-panel border border-brand-border/30 overflow-hidden">
        <div className="flex border-b border-brand-border/30">
          <button
            onClick={() => { setMode('mint'); setInputAmount('') }}
            className={`flex min-h-12 min-w-0 flex-1 items-center justify-center gap-1.5 px-3 py-3 text-sm font-semibold transition-colors hover:underline hover:underline-offset-4 sm:gap-2 sm:px-6 sm:py-4 ${
              mode === 'mint'
                ? 'bg-surface-muted text-positive border-b-2 border-positive'
                : 'text-content-secondary hover:bg-[#3B212D] hover:text-[#FFAB96] border-b-2 border-transparent'
            }`}
          >
            <span className="material-symbols-outlined text-xl">add_circle</span>
            Mint Pairs
          </button>
          <button
            onClick={() => { setMode('redeem'); setInputAmount('') }}
            className={`flex min-h-12 min-w-0 flex-1 items-center justify-center gap-1.5 px-3 py-3 text-sm font-semibold transition-colors hover:underline hover:underline-offset-4 sm:gap-2 sm:px-6 sm:py-4 ${
              mode === 'redeem'
                ? 'bg-surface-muted text-brand-orange border-b-2 border-brand-orange'
                : 'text-content-secondary hover:bg-[#3B212D] hover:text-[#FFAB96] border-b-2 border-transparent'
            }`}
          >
            <span className="material-symbols-outlined text-xl">swap_horiz</span>
            Redeem
          </button>
        </div>

        <div className="min-w-0 space-y-5 p-4 sm:space-y-6 sm:p-6 md:p-8">
          {mode === 'mint' ? (
            <>
              <Alert variant="info">
                Mint equal amounts of plDXY-BEAR and plDXY-BULL from USDC.
                You'll receive both tokens in a 1:1 ratio.
              </Alert>

              <TokenInput
                label="Pairs to mint (of each token)"
                value={inputAmount}
                onChange={setInputAmount}
                token={{ symbol: 'PAIR', decimals: 18 }}
                balance={undefined}
              />

              <div className="flex justify-center z-10 relative">
                <div className="bg-surface-muted w-9 h-9 rounded-full border border-brand-border/30 flex items-center justify-center">
                  <span className="material-symbols-outlined text-brand-peach text-lg">arrow_downward</span>
                </div>
              </div>

              <div className="min-w-0 space-y-3 border border-brand-border/30 bg-surface-muted p-3 sm:p-4">
                <p className="text-sm text-content-secondary">You will receive:</p>
                <div className="flex min-w-0 items-center justify-between gap-3">
                  <div className="flex min-w-0 items-center gap-2">
                    <TokenIcon side="BULL" size="sm" />
                    <span className="break-words font-medium text-positive">plDXY-BULL</span>
                  </div>
                  <span className="min-w-0 break-all text-right font-semibold text-content-primary">{inputAmount || '0'}</span>
                </div>
                <div className="flex min-w-0 items-center justify-between gap-3">
                  <div className="flex min-w-0 items-center gap-2">
                    <TokenIcon side="BEAR" size="sm" />
                    <span className="break-words font-medium text-brand-orange">plDXY-BEAR</span>
                  </div>
                  <span className="min-w-0 break-all text-right font-semibold text-content-primary">{inputAmount || '0'}</span>
                </div>
                <div className="border-t border-brand-border/30 pt-3 mt-3">
                  <div className="flex min-w-0 items-start justify-between gap-3">
                    <span className="shrink-0 text-content-secondary">USDC required</span>
                    <span className="min-w-0 break-words text-right text-lg font-semibold text-content-primary">{outputDisplay} USDC</span>
                  </div>
                  <div className="mt-1 flex min-w-0 items-start justify-between gap-3 text-sm">
                    <span className="shrink-0 text-content-secondary">Your balance</span>
                    <span className="min-w-0 break-words text-right text-content-secondary">{formatUsd(usdcBalance)} USDC</span>
                  </div>
                </div>
              </div>

              {isConnected ? (
                <button
                  onClick={handleMint}
                  disabled={isActionDisabled}
                  className="w-full bg-positive text-app-bg enabled:hover:bg-[#00CC77] enabled:hover:underline enabled:hover:underline-offset-4 font-semibold py-4 px-6 transition-colors text-lg disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  {getMintButtonText()}
                </button>
              ) : (
                <button
                  disabled
                  className="w-full bg-surface-muted text-content-secondary font-semibold py-4 px-6 cursor-not-allowed"
                >
                  Connect Wallet to Mint
                </button>
              )}
            </>
          ) : (
            <>
              <Alert variant="warning" icon="info">
                Redeem equal amounts of plDXY-BEAR and plDXY-BULL to get back USDC.
                You need equal amounts of both tokens.
              </Alert>

              <div className="min-w-0 space-y-3 border border-brand-border/30 bg-surface-muted p-3 sm:p-4">
                <p className="text-sm text-content-secondary">Your balances:</p>
                <div className="flex min-w-0 items-center justify-between gap-3">
                  <div className="flex min-w-0 items-center gap-2">
                    <TokenIcon side="BULL" size="sm" />
                    <span className="break-words font-medium text-positive">plDXY-BULL</span>
                  </div>
                  <span className="min-w-0 break-all text-right font-semibold text-content-primary">{formatAmount(bullBalance, 18)}</span>
                </div>
                <div className="flex min-w-0 items-center justify-between gap-3">
                  <div className="flex min-w-0 items-center gap-2">
                    <TokenIcon side="BEAR" size="sm" />
                    <span className="break-words font-medium text-brand-orange">plDXY-BEAR</span>
                  </div>
                  <span className="min-w-0 break-all text-right font-semibold text-content-primary">{formatAmount(bearBalance, 18)}</span>
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

              <div className="flex justify-center z-10 relative">
                <div className="bg-surface-muted w-9 h-9 rounded-full border border-brand-border/30 flex items-center justify-center">
                  <span className="material-symbols-outlined text-brand-peach text-lg">arrow_downward</span>
                </div>
              </div>

              <div className="min-w-0 border border-brand-border/30 bg-surface-muted p-3 sm:p-4">
                <div className="flex min-w-0 items-start justify-between gap-3">
                  <span className="shrink-0 text-content-secondary">You will receive</span>
                  <span className="min-w-0 break-words text-right text-lg font-semibold text-content-primary">{outputDisplay} USDC</span>
                </div>
              </div>

              {isConnected ? (
                <button
                  onClick={handleRedeem}
                  disabled={isActionDisabled}
                  className="w-full bg-brand-orange text-content-primary enabled:hover:bg-[#FF572D] enabled:hover:underline enabled:hover:underline-offset-4 font-semibold py-4 px-6 transition-colors text-lg disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  {getRedeemButtonText()}
                </button>
              ) : (
                <button
                  disabled
                  className="w-full bg-surface-muted text-content-secondary font-semibold py-4 px-6 cursor-not-allowed"
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

export default Mint
