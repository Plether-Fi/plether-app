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
    <div className="space-y-10 max-w-xl mx-auto">
      <div className="mb-8">
        <h1 className="text-3xl font-semibold text-cyber-text-primary mb-1">Mint & Redeem</h1>
        <p className="text-cyber-text-secondary font-light">Create or redeem plDXY-BEAR + plDXY-BULL pairs</p>
      </div>

      {!isProtocolActive && protocolData && mode === 'mint' && (
        <Alert variant="warning" icon="warning">
          Protocol is currently {protocolData.data.status.toLowerCase()}. Minting is disabled.
        </Alert>
      )}

      <div className="bg-cyber-surface-dark border border-cyber-border-glow/30 shadow-lg shadow-cyber-border-glow/10 overflow-hidden">
        <div className="flex border-b border-cyber-border-glow/30">
          <button
            onClick={() => { setMode('mint'); setInputAmount('') }}
            className={`flex-1 flex items-center justify-center gap-2 px-6 py-4 text-sm font-semibold transition-colors cursor-pointer ${
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
            className={`flex-1 flex items-center justify-center gap-2 px-6 py-4 text-sm font-semibold transition-colors cursor-pointer ${
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
                <div className="bg-cyber-surface-light w-9 h-9 rounded-full border border-cyber-border-glow/30 flex items-center justify-center">
                  <span className="material-symbols-outlined text-cyber-bright-blue text-lg">arrow_downward</span>
                </div>
              </div>

              <div className="bg-cyber-surface-light p-4 space-y-3 border border-cyber-border-glow/30">
                <p className="text-sm text-cyber-text-secondary">You will receive:</p>
                <div className="flex justify-between items-center">
                  <div className="flex items-center gap-2">
                    <TokenIcon side="BULL" size="sm" />
                    <span className="text-cyber-neon-green font-medium">plDXY-BULL</span>
                  </div>
                  <span className="text-cyber-text-primary font-semibold">{inputAmount || '0'}</span>
                </div>
                <div className="flex justify-between items-center">
                  <div className="flex items-center gap-2">
                    <TokenIcon side="BEAR" size="sm" />
                    <span className="text-cyber-electric-fuchsia font-medium">plDXY-BEAR</span>
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
                    <span className="text-cyber-text-secondary">{formatUsd(usdcBalance)} USDC</span>
                  </div>
                </div>
              </div>

              {isConnected ? (
                <button
                  onClick={handleMint}
                  disabled={isActionDisabled}
                  className="w-full bg-cyber-neon-green hover:bg-cyber-neon-green/90 text-cyber-bg font-semibold py-4 px-6 shadow-lg shadow-cyber-neon-green/40 transition-all transform hover:-translate-y-0.5 active:translate-y-0 text-lg disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none disabled:shadow-none"
                >
                  {getMintButtonText()}
                </button>
              ) : (
                <button
                  disabled
                  className="w-full bg-cyber-surface-light text-cyber-text-secondary font-semibold py-4 px-6 cursor-not-allowed"
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

              <div className="bg-cyber-surface-light p-4 space-y-3 border border-cyber-border-glow/30">
                <p className="text-sm text-cyber-text-secondary">Your balances:</p>
                <div className="flex justify-between items-center">
                  <div className="flex items-center gap-2">
                    <TokenIcon side="BULL" size="sm" />
                    <span className="text-cyber-neon-green font-medium">plDXY-BULL</span>
                  </div>
                  <span className="text-cyber-text-primary font-semibold">{formatAmount(bullBalance, 18)}</span>
                </div>
                <div className="flex justify-between items-center">
                  <div className="flex items-center gap-2">
                    <TokenIcon side="BEAR" size="sm" />
                    <span className="text-cyber-electric-fuchsia font-medium">plDXY-BEAR</span>
                  </div>
                  <span className="text-cyber-text-primary font-semibold">{formatAmount(bearBalance, 18)}</span>
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
                <div className="bg-cyber-surface-light w-9 h-9 rounded-full border border-cyber-border-glow/30 flex items-center justify-center">
                  <span className="material-symbols-outlined text-cyber-bright-blue text-lg">arrow_downward</span>
                </div>
              </div>

              <div className="bg-cyber-surface-light p-4 border border-cyber-border-glow/30">
                <div className="flex justify-between items-center">
                  <span className="text-cyber-text-secondary">You will receive</span>
                  <span className="text-cyber-text-primary font-semibold text-lg">{outputDisplay} USDC</span>
                </div>
              </div>

              {isConnected ? (
                <button
                  onClick={handleRedeem}
                  disabled={isActionDisabled}
                  className="w-full bg-cyber-electric-fuchsia hover:bg-cyber-electric-fuchsia/90 text-cyber-text-primary font-semibold py-4 px-6 shadow-lg shadow-cyber-electric-fuchsia/40 transition-all transform hover:-translate-y-0.5 active:translate-y-0 text-lg disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none disabled:shadow-none"
                >
                  {getRedeemButtonText()}
                </button>
              ) : (
                <button
                  disabled
                  className="w-full bg-cyber-surface-light text-cyber-text-secondary font-semibold py-4 px-6 cursor-not-allowed"
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
