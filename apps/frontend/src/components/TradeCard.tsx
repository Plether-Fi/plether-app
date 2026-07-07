import { useState, useEffect } from 'react'
import { useAccount } from 'wagmi'
import { parseUnits } from 'viem'
import { TokenInput } from './TokenInput'
import { InfoTooltip, OutputDisplay, Modal, Button } from './ui'
import { useTradeQuote, useZapQuote as useZapQuoteApi, useUserDashboard } from '../api'
import { useTransactionStore } from '../stores/transactionStore'
import { transactionManager } from '../services/transactionManager'
import { useSettingsStore } from '../stores/settingsStore'
import { formatAmount } from '../utils/formatters'

type TradeMode = 'buy' | 'sell'
type TokenSide = 'BEAR' | 'BULL'

export interface TradeCardProps {
  usdcBalance: bigint
  bearBalance: bigint
  bullBalance: bigint
  refetchBalances?: () => void
}

export function TradeCard({ usdcBalance, bearBalance, bullBalance, refetchBalances }: TradeCardProps) {
  const { isConnected, address } = useAccount()
  const slippage = useSettingsStore((s) => s.slippage)
  const maxPriceImpact = useSettingsStore((s) => s.maxPriceImpact)
  const txStore = useTransactionStore()

  const [mode, setMode] = useState<TradeMode>('buy')
  const [selectedToken, setSelectedToken] = useState<TokenSide>('BULL')
  const [inputAmount, setInputAmount] = useState('')
  const [showPriceImpactWarning, setShowPriceImpactWarning] = useState(false)
  const [showDetails, setShowDetails] = useState(false)

  const isBearTrade = selectedToken === 'BEAR'
  const operationKey = isBearTrade
    ? `swap-${mode}-bear`
    : (mode === 'buy' ? 'swap-buy-bull' : 'swap-sell-bull')

  const transactionId = txStore.activeOperations[operationKey]
  const currentTx = transactionId
    ? txStore.transactions.find(t => t.id === transactionId)
    : null
  const isPending = currentTx?.status === 'pending' || currentTx?.status === 'confirming'

  const inputToken = mode === 'buy'
    ? { symbol: 'USDC', decimals: 6 }
    : { symbol: `plDXY-${selectedToken}`, decimals: 18 }

  const outputToken = mode === 'buy'
    ? { symbol: `plDXY-${selectedToken}`, decimals: 18 }
    : { symbol: 'USDC', decimals: 6 }

  const inputBalance = mode === 'buy'
    ? usdcBalance
    : selectedToken === 'BEAR' ? bearBalance : bullBalance

  const inputDecimals = mode === 'buy' ? 6 : 18
  const inputAmountBigInt = inputAmount ? parseUnits(inputAmount, inputDecimals) : 0n

  const bearQuoteAmount = isBearTrade && inputAmountBigInt > 0n ? inputAmountBigInt.toString() : undefined
  const bearQuoteFrom = mode === 'buy' ? 'usdc' as const : 'bear' as const
  const { data: bearQuoteData, isLoading: bearQuoteLoading } = useTradeQuote(bearQuoteFrom, bearQuoteAmount)

  const bullQuoteAmount = !isBearTrade && inputAmountBigInt > 0n ? inputAmountBigInt.toString() : undefined
  const { data: bullQuoteData, isLoading: bullQuoteLoading } = useZapQuoteApi(mode, bullQuoteAmount)

  const quoteAmountOut = isBearTrade
    ? BigInt(bearQuoteData?.data.amountOut ?? '0')
    : BigInt(bullQuoteData?.data.output.amount ?? '0')
  const priceImpact = isBearTrade
    ? Number(bearQuoteData?.data.priceImpact ?? '0') / 100
    : Number(bullQuoteData?.data.priceImpact ?? '0') / 100
  const isQuoteLoading = isBearTrade ? bearQuoteLoading : bullQuoteLoading

  useEffect(() => {
    if (priceImpact > 1) {
      setShowDetails(true)
    }
  }, [priceImpact])

  const { data: dashboardData } = useUserDashboard(address)
  const dashAllowances = dashboardData?.data.allowances

  const needsApproval = (() => {
    if (!dashAllowances || inputAmountBigInt <= 0n) return false
    if (isBearTrade) {
      const allowance = mode === 'buy'
        ? BigInt(dashAllowances.usdc.curvePool)
        : BigInt(dashAllowances.bear.curvePool)
      return allowance < inputAmountBigInt
    }
    const allowance = mode === 'buy'
      ? BigInt(dashAllowances.usdc.zap)
      : BigInt(dashAllowances.bull.zapRouter)
    return allowance < inputAmountBigInt
  })()

  const insufficientBalance = inputAmountBigInt > inputBalance

  const handleSwapSuccess = () => {
    refetchBalances?.()
    setInputAmount('')
  }

  const proceedWithSwap = () => {
    const slippageBps = BigInt(Math.floor(slippage * 100))
    const minAmountOut = quoteAmountOut - (quoteAmountOut * slippageBps / 10000n)

    if (isBearTrade) {
      void transactionManager.executeCurveSwap(mode, inputAmountBigInt, minAmountOut, {
        onRetry: proceedWithSwap,
      }).then(handleSwapSuccess)
    } else {
      if (mode === 'buy') {
        void transactionManager.executeZapBuy(inputAmountBigInt, minAmountOut, slippageBps, {
          onRetry: proceedWithSwap,
        }).then(handleSwapSuccess)
      } else {
        void transactionManager.executeZapSell(inputAmountBigInt, minAmountOut, {
          onRetry: proceedWithSwap,
        }).then(handleSwapSuccess)
      }
    }
  }

  const handleSwap = () => {
    if (!inputAmountBigInt || inputAmountBigInt <= 0n) return

    if (priceImpact > maxPriceImpact) {
      setShowPriceImpactWarning(true)
      return
    }

    proceedWithSwap()
  }

  const handleConfirmHighImpact = () => {
    setShowPriceImpactWarning(false)
    proceedWithSwap()
  }

  const getButtonText = () => {
    if (isPending) return 'Swapping...'
    if (insufficientBalance) return `Insufficient ${inputToken.symbol}`
    if (needsApproval) return `Approve & ${mode === 'buy' ? 'Buy' : 'Sell'}`
    return `${mode === 'buy' ? 'Buy' : 'Sell'} plDXY-${selectedToken}`
  }

  const isDisabled = !inputAmount || parseFloat(inputAmount) <= 0 || isPending || insufficientBalance

  const outputDisplay = isQuoteLoading && inputAmountBigInt > 0n
    ? '...'
    : formatAmount(quoteAmountOut, outputToken.decimals)

  return (
    <div className="max-w-xl mx-auto space-y-6">
      <div className="bg-surface-muted p-1 flex text-sm font-medium mb-8 border border-brand-border/30">
        <button
          onClick={() => { setMode('buy'); setInputAmount('') }}
          className={`flex-1 py-2 px-4 transition-colors hover:underline hover:underline-offset-4 ${
            mode === 'buy'
              ? 'bg-surface-panel text-brand-peach border border-brand-peach/50'
              : 'text-content-secondary hover:bg-[#3B212D] hover:text-[#FFAB96]'
          }`}
        >
          Buy
        </button>
        <button
          onClick={() => { setMode('sell'); setInputAmount('') }}
          className={`flex-1 py-2 px-4 transition-colors hover:underline hover:underline-offset-4 ${
            mode === 'sell'
              ? 'bg-surface-panel text-brand-peach border border-brand-peach/50'
              : 'text-content-secondary hover:bg-[#3B212D] hover:text-[#FFAB96]'
          }`}
        >
          Sell
        </button>
      </div>

      <div className="space-y-2">
        <label className="text-sm font-medium text-content-secondary">Select Token</label>
        <div className="grid grid-cols-2 gap-4">
          <button
            onClick={() => { setSelectedToken('BULL'); }}
            className={`relative p-4 text-center transition-colors hover:underline hover:underline-offset-4 ${
              selectedToken === 'BULL'
                ? 'border-2 border-positive bg-positive/10'
                : 'border border-brand-border/30 bg-surface-panel hover:border-positive/50 hover:bg-[#3B212D]'
            }`}
          >
            <div className={`font-semibold ${selectedToken === 'BULL' ? 'text-positive' : 'text-content-primary'}`}>plDXY-BULL</div>
            <div className={`text-xs mt-1 ${selectedToken === 'BULL' ? 'text-positive/70' : 'text-content-secondary'}`}>Bullish on USD</div>
          </button>
          <button
            onClick={() => { setSelectedToken('BEAR'); }}
            className={`relative p-4 text-center transition-colors hover:underline hover:underline-offset-4 ${
              selectedToken === 'BEAR'
                ? 'border-2 border-brand-orange bg-brand-orange/10'
                : 'border border-brand-border/30 bg-surface-panel hover:border-brand-orange/50 hover:bg-[#3B212D]'
            }`}
          >
            <div className={`font-semibold ${selectedToken === 'BEAR' ? 'text-brand-orange' : 'text-content-primary'}`}>plDXY-BEAR</div>
            <div className={`text-xs mt-1 ${selectedToken === 'BEAR' ? 'text-brand-orange/70' : 'text-content-secondary'}`}>Bearish on USD</div>
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

        <div className="flex justify-center z-10 relative">
          <div className="bg-surface-muted w-9 h-9 rounded-full border border-brand-border/30 flex items-center justify-center">
            <span className="material-symbols-outlined text-brand-peach text-lg">arrow_downward</span>
          </div>
        </div>

        <OutputDisplay
          label="You receive"
          value={outputDisplay}
          token={outputToken.symbol}
          variant={mode === 'buy' ? selectedToken : 'neutral'}
        />
      </div>

      <div className="flex items-center justify-end gap-2 text-xs text-content-secondary">
        <span className="material-symbols-outlined text-[14px]">settings</span>
        <span>{slippage}% slippage</span>
      </div>

      <div className="border-t border-brand-border/30 pt-4">
        <button
          onClick={() => { setShowDetails(!showDetails); }}
          className="flex w-full items-center justify-between text-sm text-content-secondary transition-colors hover:text-[#FFAB96] hover:underline hover:underline-offset-4"
        >
          <span>Swap details</span>
          <span className="material-symbols-outlined text-lg">{showDetails ? 'expand_less' : 'expand_more'}</span>
        </button>
      </div>

      {showDetails && (
        <div className="bg-surface-muted p-3 space-y-2 text-sm border border-brand-border/30">
          <div className="flex justify-between">
            <span className="text-content-secondary">Route</span>
            <span className="text-content-primary">
              {selectedToken === 'BEAR'
                ? (mode === 'buy' ? 'USDC → Curve → plDXY-BEAR' : 'plDXY-BEAR → Curve → USDC')
                : (mode === 'buy' ? 'USDC → ZapRouter → plDXY-BULL' : 'plDXY-BULL → ZapRouter → USDC')}
            </span>
          </div>
          <div className="flex justify-between">
            <span className="text-content-secondary">Price Impact</span>
            <span className={
              priceImpact > 1 ? 'text-red-500' :
              priceImpact > slippage ? 'text-warning' :
              'text-content-primary'
            }>
              {priceImpact > 0 ? `${priceImpact.toFixed(2)}%` : '-'}
            </span>
          </div>
          {selectedToken === 'BULL' && mode === 'buy' && (
            <div className="flex justify-between items-center">
              <span className="text-content-secondary flex items-center gap-1">
                Safety Buffer
                <InfoTooltip content="Flash loan safety margin. You may receive up to 0.5% of output as BEAR instead of BULL, depending on slippage." />
              </span>
              <span className="text-content-primary">0.5%</span>
            </div>
          )}
        </div>
      )}

      {isConnected ? (
        <button
          onClick={() => { handleSwap() }}
          disabled={isDisabled}
          className="w-full border border-[#FFAB96] bg-[#FFAB96] px-6 py-4 text-lg font-semibold text-[#250917] transition-colors hover:bg-[#FF572D] hover:text-[#FFF5F9] hover:underline hover:underline-offset-4 disabled:cursor-not-allowed disabled:opacity-50 disabled:hover:no-underline"
        >
          {getButtonText()}
        </button>
      ) : (
        <button
          disabled
          className="w-full bg-surface-muted text-content-secondary font-semibold py-4 px-6 cursor-not-allowed"
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
            <div className="text-sm text-content-secondary mt-1">Price Impact</div>
          </div>

          <p className="text-sm text-content-secondary">
            This trade has a price impact of <span className="text-red-500 font-medium">{priceImpact.toFixed(2)}%</span>,
            which exceeds your maximum threshold of <span className="text-content-primary font-medium">{maxPriceImpact}%</span>.
          </p>

          <p className="text-sm text-content-secondary">
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
              onClick={() => { handleConfirmHighImpact() }}
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
