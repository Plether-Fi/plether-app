import { useState, useEffect, useRef, useCallback } from 'react'
import { useAccount } from 'wagmi'
import { parseUnits } from 'viem'
import { TokenInput } from './TokenInput'
import { InfoTooltip, OutputDisplay, Modal, Button } from './ui'
import { useCurveQuote, useCurveSwap, useZapQuote, useZapSwap, useAllowance, useApprove, useTransactionModal } from '../hooks'
import { getAddresses } from '../contracts/addresses'
import { useSettingsStore } from '../stores/settingsStore'
import { formatAmount } from '../utils/formatters'
import { parseTransactionError, getErrorMessage } from '../utils/errors'

type TradeMode = 'buy' | 'sell'
type TokenSide = 'BEAR' | 'BULL'

export interface TradeCardProps {
  usdcBalance: bigint
  bearBalance: bigint
  bullBalance: bigint
  refetchBalances?: () => void
}

export function TradeCard({ usdcBalance, bearBalance, bullBalance, refetchBalances }: TradeCardProps) {
  const { isConnected, chainId } = useAccount()
  const addresses = getAddresses(chainId ?? 11155111)
  const slippage = useSettingsStore((s) => s.slippage)
  const maxPriceImpact = useSettingsStore((s) => s.maxPriceImpact)
  const txModal = useTransactionModal()

  const [mode, setMode] = useState<TradeMode>('buy')
  const [selectedToken, setSelectedToken] = useState<TokenSide>('BEAR')
  const [inputAmount, setInputAmount] = useState('')
  const [showPriceImpactWarning, setShowPriceImpactWarning] = useState(false)
  const [showDetails, setShowDetails] = useState(false)
  const [pendingSwap, setPendingSwap] = useState(false)
  const pendingAmountRef = useRef<bigint>(0n)
  const approveHandledRef = useRef(false)
  const swapTriggeredRef = useRef(false)

  const inputToken = mode === 'buy'
    ? { symbol: 'USDC', decimals: 6 }
    : { symbol: `DXY-${selectedToken}`, decimals: 18 }

  const outputToken = mode === 'buy'
    ? { symbol: `DXY-${selectedToken}`, decimals: 18 }
    : { symbol: 'USDC', decimals: 6 }

  const inputBalance = mode === 'buy'
    ? usdcBalance
    : selectedToken === 'BEAR' ? bearBalance : bullBalance

  const inputDecimals = mode === 'buy' ? 6 : 18
  const inputAmountBigInt = inputAmount ? parseUnits(inputAmount, inputDecimals) : 0n

  const isBearTrade = selectedToken === 'BEAR'

  const { amountOut: curveAmountOut, priceImpact: curvePriceImpact, isLoading: curveQuoteLoading } = useCurveQuote(
    mode === 'buy' ? 'USDC' : 'BEAR',
    isBearTrade ? inputAmountBigInt : 0n
  )

  const { amountOut: zapAmountOut, priceImpact: zapPriceImpact, isLoading: zapQuoteLoading } = useZapQuote(
    mode,
    !isBearTrade ? inputAmountBigInt : 0n
  )

  const quoteAmountOut = isBearTrade ? curveAmountOut : zapAmountOut
  const priceImpact = isBearTrade ? curvePriceImpact : zapPriceImpact
  const isQuoteLoading = isBearTrade ? curveQuoteLoading : zapQuoteLoading

  useEffect(() => {
    if (priceImpact > 1) {
      setShowDetails(true)
    }
  }, [priceImpact])

  const {
    swap: curveSwap,
    isPending: curvePending,
    isConfirming: curveConfirming,
    isSuccess: curveSuccess,
    error: curveError,
    reset: resetCurve,
    hash: curveHash,
  } = useCurveSwap()
  const {
    zapBuy,
    zapSell,
    isPending: zapPending,
    isConfirming: zapConfirming,
    isSuccess: zapSuccess,
    error: zapError,
    reset: resetZap,
    hash: zapHash,
  } = useZapSwap()

  const spenderAddress = isBearTrade ? addresses.CURVE_POOL : addresses.ZAP_ROUTER
  const tokenToApprove = mode === 'buy'
    ? addresses.USDC
    : (selectedToken === 'BEAR' ? addresses.DXY_BEAR : addresses.DXY_BULL)

  const { allowance, refetch: refetchAllowance } = useAllowance(tokenToApprove, spenderAddress)
  const {
    approve,
    isPending: approvePending,
    isConfirming: approveConfirming,
    isSuccess: approveSuccess,
    error: approveError,
  } = useApprove(tokenToApprove, spenderAddress)

  const needsApproval = inputAmountBigInt > 0n && allowance < inputAmountBigInt
  const insufficientBalance = inputAmountBigInt > inputBalance

  const executeSwap = useCallback(async (amount: bigint) => {
    const slippageBps = BigInt(Math.floor(slippage * 100))
    const minAmountOut = quoteAmountOut - (quoteAmountOut * slippageBps / 10000n)
    const deadline = BigInt(Math.floor(Date.now() / 1000) + 1800)

    if (isBearTrade) {
      await curveSwap(mode === 'buy' ? 'USDC' : 'BEAR', amount, minAmountOut)
    } else {
      if (mode === 'buy') {
        await zapBuy(amount, minAmountOut, slippageBps, deadline)
      } else {
        await zapSell(amount, minAmountOut, deadline)
      }
    }
  }, [slippage, quoteAmountOut, isBearTrade, mode, curveSwap, zapBuy, zapSell])

  useEffect(() => {
    if (approveSuccess && !approveHandledRef.current) {
      approveHandledRef.current = true
      void refetchAllowance()
      if (pendingSwap && pendingAmountRef.current > 0n) {
        swapTriggeredRef.current = true
        void executeSwap(pendingAmountRef.current)
        pendingAmountRef.current = 0n
        setPendingSwap(false)
      }
    }
  }, [approveSuccess, refetchAllowance, pendingSwap, executeSwap])

  useEffect(() => {
    if (curveSuccess || zapSuccess) {
      const hash = curveHash ?? zapHash
      if (hash) txModal.setSuccess(hash)
      refetchBalances?.()
      setInputAmount('')
      resetCurve()
      resetZap()
      swapTriggeredRef.current = false
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [curveSuccess, zapSuccess, curveHash, zapHash, refetchBalances, resetCurve, resetZap])

  useEffect(() => {
    const modal = useTransactionModal.getState()
    if (!modal.isOpen) return

    if (approvePending) {
      modal.setStepInProgress(0)
    } else if (approveConfirming) {
      modal.setStepInProgress(1)
    } else if (approveError) {
      modal.setError(0, getErrorMessage(parseTransactionError(approveError)))
    }
  }, [approvePending, approveConfirming, approveError])

  useEffect(() => {
    const modal = useTransactionModal.getState()
    if (!modal.isOpen || !swapTriggeredRef.current) return
    const swapStepOffset = pendingSwap || approveSuccess ? 2 : 0

    if (curvePending || zapPending) {
      modal.setStepInProgress(swapStepOffset)
    } else if (curveConfirming || zapConfirming) {
      modal.setStepInProgress(swapStepOffset + 1)
    } else if (curveError || zapError) {
      modal.setError(swapStepOffset, getErrorMessage(parseTransactionError(curveError ?? zapError)))
    }
  }, [curvePending, zapPending, curveConfirming, zapConfirming, curveError, zapError, pendingSwap, approveSuccess])

  const proceedWithSwap = async () => {
    approveHandledRef.current = false
    swapTriggeredRef.current = false

    const tokenLabel = `DXY-${selectedToken}`
    const actionLabel = mode === 'buy' ? 'Buying' : 'Selling'

    if (needsApproval) {
      txModal.open({
        title: `${actionLabel} ${tokenLabel}`,
        steps: [
          `Approve ${inputToken.symbol}`,
          'Confirming approval',
          `${mode === 'buy' ? 'Buy' : 'Sell'} ${tokenLabel}`,
          'Awaiting confirmation',
        ],
        onRetry: () => void proceedWithSwap(),
      })
      pendingAmountRef.current = inputAmountBigInt
      setPendingSwap(true)
      await approve(inputAmountBigInt)
      return
    }

    txModal.open({
      title: `${actionLabel} ${tokenLabel}`,
      steps: [
        `${mode === 'buy' ? 'Buy' : 'Sell'} ${tokenLabel}`,
        'Awaiting confirmation',
      ],
      onRetry: () => void proceedWithSwap(),
    })
    swapTriggeredRef.current = true
    await executeSwap(inputAmountBigInt)
  }

  const handleSwap = async () => {
    if (!inputAmountBigInt || inputAmountBigInt <= 0n) return

    if (priceImpact > maxPriceImpact) {
      setShowPriceImpactWarning(true)
      return
    }

    await proceedWithSwap()
  }

  const handleConfirmHighImpact = async () => {
    setShowPriceImpactWarning(false)
    await proceedWithSwap()
  }

  const getButtonText = () => {
    if (curvePending || zapPending) return 'Swapping...'
    if (approvePending) return `Approving ${inputToken.symbol}...`
    if (insufficientBalance) return `Insufficient ${inputToken.symbol}`
    if (needsApproval) return `Approve ${inputToken.symbol}`
    return `${mode === 'buy' ? 'Buy' : 'Sell'} DXY-${selectedToken}`
  }

  const isPending = curvePending || zapPending || approvePending
  const isDisabled = !inputAmount || parseFloat(inputAmount) <= 0 || isPending || insufficientBalance

  const outputDisplay = isQuoteLoading && inputAmountBigInt > 0n
    ? '...'
    : formatAmount(quoteAmountOut, outputToken.decimals)

  return (
    <div className="max-w-xl mx-auto space-y-6">
      <div className="bg-cyber-surface-light p-1 flex text-sm font-medium mb-8 border border-cyber-border-glow/30">
        <button
          onClick={() => { setMode('buy'); setInputAmount('') }}
          className={`flex-1 py-2 px-4 transition-all ${
            mode === 'buy'
              ? 'bg-cyber-surface-dark text-cyber-bright-blue shadow-sm shadow-cyber-bright-blue/10 border border-cyber-bright-blue/50'
              : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
          }`}
        >
          Buy
        </button>
        <button
          onClick={() => { setMode('sell'); setInputAmount('') }}
          className={`flex-1 py-2 px-4 transition-all ${
            mode === 'sell'
              ? 'bg-cyber-surface-dark text-cyber-bright-blue shadow-sm shadow-cyber-bright-blue/10 border border-cyber-bright-blue/50'
              : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
          }`}
        >
          Sell
        </button>
      </div>

      <div className="space-y-2">
        <label className="text-sm font-medium text-cyber-text-secondary">Select Token</label>
        <div className="grid grid-cols-2 gap-4">
          <button
            onClick={() => { setSelectedToken('BEAR'); }}
            className={`relative p-4 text-center transition-all ${
              selectedToken === 'BEAR'
                ? 'border-2 border-cyber-electric-fuchsia bg-cyber-electric-fuchsia/10 shadow-md shadow-cyber-electric-fuchsia/20'
                : 'border border-cyber-border-glow/30 bg-cyber-surface-dark hover:border-cyber-electric-fuchsia/50 opacity-60 hover:opacity-100'
            }`}
          >
            <div className={`font-semibold ${selectedToken === 'BEAR' ? 'text-cyber-electric-fuchsia' : 'text-cyber-text-primary'}`}>DXY-BEAR</div>
            <div className={`text-xs mt-1 ${selectedToken === 'BEAR' ? 'text-cyber-electric-fuchsia/70' : 'text-cyber-text-secondary'}`}>Bearish on USD</div>
          </button>
          <button
            onClick={() => { setSelectedToken('BULL'); }}
            className={`relative p-4 text-center transition-all ${
              selectedToken === 'BULL'
                ? 'border-2 border-cyber-neon-green bg-cyber-neon-green/10 shadow-md shadow-cyber-neon-green/20'
                : 'border border-cyber-border-glow/30 bg-cyber-surface-dark hover:border-cyber-neon-green/50 opacity-60 hover:opacity-100'
            }`}
          >
            <div className={`font-semibold ${selectedToken === 'BULL' ? 'text-cyber-neon-green' : 'text-cyber-text-primary'}`}>DXY-BULL</div>
            <div className={`text-xs mt-1 ${selectedToken === 'BULL' ? 'text-cyber-neon-green/70' : 'text-cyber-text-secondary'}`}>Bullish on USD</div>
          </button>
        </div>
      </div>

      <div className="space-y-4">
        <TokenInput
          label={mode === 'buy' ? 'You pay' : 'You sell'}
          value={inputAmount}
          onChange={setInputAmount}
          token={inputToken}
          balance={inputBalance}
        />

        <div className="flex justify-center -my-2 z-10 relative">
          <div className="bg-cyber-surface-light p-2 rounded-full border border-cyber-border-glow/30 shadow-sm shadow-cyber-border-glow/10">
            <span className="material-symbols-outlined text-cyber-bright-blue text-lg block">arrow_downward</span>
          </div>
        </div>

        <OutputDisplay
          label="You receive"
          value={outputDisplay}
          token={outputToken.symbol}
          variant={mode === 'buy' ? selectedToken : 'neutral'}
        />
      </div>

      <div className="flex items-center justify-end gap-2 text-xs text-cyber-text-secondary">
        <span className="material-symbols-outlined text-[14px]">settings</span>
        <span>{slippage}% slippage</span>
      </div>

      <div className="border-t border-cyber-border-glow/30 pt-4">
        <button
          onClick={() => { setShowDetails(!showDetails); }}
          className="w-full flex justify-between items-center text-sm text-cyber-text-secondary hover:text-cyber-bright-blue"
        >
          <span>Swap details</span>
          <span className="material-symbols-outlined text-lg">{showDetails ? 'expand_less' : 'expand_more'}</span>
        </button>
      </div>

      {showDetails && (
        <div className="bg-cyber-surface-light p-3 space-y-2 text-sm border border-cyber-border-glow/30">
          <div className="flex justify-between">
            <span className="text-cyber-text-secondary">Route</span>
            <span className="text-cyber-text-primary">
              {selectedToken === 'BEAR'
                ? (mode === 'buy' ? 'USDC → Curve → DXY-BEAR' : 'DXY-BEAR → Curve → USDC')
                : (mode === 'buy' ? 'USDC → ZapRouter → DXY-BULL' : 'DXY-BULL → ZapRouter → USDC')}
            </span>
          </div>
          <div className="flex justify-between">
            <span className="text-cyber-text-secondary">Price Impact</span>
            <span className={
              priceImpact > 1 ? 'text-red-500' :
              priceImpact > slippage ? 'text-cyber-warning-text' :
              'text-cyber-text-primary'
            }>
              {priceImpact > 0 ? `${priceImpact.toFixed(2)}%` : '-'}
            </span>
          </div>
          {selectedToken === 'BULL' && mode === 'buy' && (
            <div className="flex justify-between items-center">
              <span className="text-cyber-text-secondary flex items-center gap-1">
                Safety Buffer
                <InfoTooltip content="Flash loan safety margin. You may receive up to 0.5% of output as BEAR instead of BULL, depending on slippage." />
              </span>
              <span className="text-cyber-text-primary">0.5%</span>
            </div>
          )}
        </div>
      )}

      {isConnected ? (
        <button
          onClick={() => void handleSwap()}
          disabled={isDisabled}
          className="w-full bg-cyber-bright-blue hover:bg-cyber-bright-blue/90 text-cyber-bg font-semibold py-4 px-6 shadow-lg shadow-cyber-bright-blue/40 transition-all transform hover:-translate-y-0.5 active:translate-y-0 text-lg disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none disabled:shadow-none"
        >
          {getButtonText()}
        </button>
      ) : (
        <button
          disabled
          className="w-full bg-cyber-surface-light text-cyber-text-secondary font-semibold py-4 px-6 cursor-not-allowed"
        >
          Connect Wallet to Trade
        </button>
      )}

      <Modal
        isOpen={showPriceImpactWarning}
        onClose={() => { setShowPriceImpactWarning(false); }}
        title="High Price Impact"
        size="sm"
      >
        <div className="space-y-4">
          <div className="bg-red-500/10 border border-red-500/30 p-4 text-center">
            <div className="text-3xl font-bold text-red-500">{priceImpact.toFixed(2)}%</div>
            <div className="text-sm text-cyber-text-secondary mt-1">Price Impact</div>
          </div>

          <p className="text-sm text-cyber-text-secondary">
            This trade has a price impact of <span className="text-red-500 font-medium">{priceImpact.toFixed(2)}%</span>,
            which exceeds your maximum threshold of <span className="text-cyber-text-primary font-medium">{maxPriceImpact}%</span>.
          </p>

          <p className="text-sm text-cyber-text-secondary">
            You will receive significantly less value than your input. Are you sure you want to proceed?
          </p>

          <div className="flex gap-3">
            <Button
              variant="secondary"
              onClick={() => { setShowPriceImpactWarning(false); }}
              className="flex-1"
            >
              Cancel
            </Button>
            <Button
              variant="primary"
              onClick={() => void handleConfirmHighImpact()}
              className="flex-1 !bg-red-500 hover:!bg-red-600"
            >
              Swap Anyway
            </Button>
          </div>
        </div>
      </Modal>
    </div>
  )
}
