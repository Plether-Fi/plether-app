import { useState, useCallback, useEffect } from 'react'
import { useAccount } from 'wagmi'
import { parseUnits } from 'viem'
import { InfoTooltip } from './ui'
import { TokenInput } from './TokenInput'
import { formatUsd } from '../utils/formatters'
import { usePreviewOpenLeverage } from '../hooks'
import { useProtocolStatus, useUserDashboard } from '../api'
import { useTransactionStore } from '../stores/transactionStore'
import { transactionManager } from '../services/transactionManager'
import { useSettingsStore } from '../stores/settingsStore'

type TokenSide = 'BEAR' | 'BULL'

export interface LeverageCardProps {
  usdcBalance: bigint
  refetchBalances?: () => void
  onPositionOpened?: () => void
}

export function LeverageCard({ usdcBalance, refetchBalances, onPositionOpened }: LeverageCardProps) {
  const { isConnected, address } = useAccount()
  const slippage = useSettingsStore((s) => s.slippage)
  const { data: dashboardData } = useUserDashboard(address)
  const transactions = useTransactionStore((s) => s.transactions)
  const activeOperations = useTransactionStore((s) => s.activeOperations)

  const [selectedSide, setSelectedSide] = useState<TokenSide>('BULL')
  const [collateralAmount, setCollateralAmount] = useState('')
  const [targetLeverage, setTargetLeverage] = useState(2)
  const [trackedTxId, setTrackedTxId] = useState<string | null>(null)

  const operationKey = `leverage-open-${selectedSide}`

  const activeTransactionId = activeOperations[operationKey]
  const currentTxId = activeTransactionId || trackedTxId
  const currentTx = currentTxId
    ? transactions.find(t => t.id === currentTxId)
    : null
  const isRunning = currentTx?.status === 'pending' || currentTx?.status === 'confirming'

  useEffect(() => {
    if (activeTransactionId && activeTransactionId !== trackedTxId) {
      setTrackedTxId(activeTransactionId)
    }
  }, [activeTransactionId, trackedTxId])

  const { data: protocolData } = useProtocolStatus()
  const prices = protocolData?.data.prices
  const bearPrice = prices ? BigInt(prices.bear) : 0n
  const bullPrice = prices ? BigInt(prices.bull) : 0n
  const tokenPrice = selectedSide === 'BEAR' ? bearPrice : bullPrice

  const maxEffectiveLeverage = 11.76

  const contractLeverage = BigInt(Math.floor(targetLeverage * 1e18))

  const collateralBigInt = collateralAmount ? parseUnits(collateralAmount, 6) : 0n

  const { expectedCollateralTokens, expectedDebt, isLoading: previewLoading } = usePreviewOpenLeverage(
    selectedSide,
    collateralBigInt,
    contractLeverage
  )

  // Position value = collateral tokens (18 dec) * token price (8 dec) / 10^20 = USDC (6 dec)
  const expectedPositionValue = expectedCollateralTokens * tokenPrice / 10n ** 20n
  const slippageBps = BigInt(Math.floor(slippage * 100))
  const minCollateralOut = expectedCollateralTokens - (expectedCollateralTokens * slippageBps / 10000n)

  const authorization = dashboardData?.data.authorization
  const needsMorphoAuthorization = selectedSide === 'BEAR'
    ? !(authorization?.bearLeverageRouter)
    : !(authorization?.bullLeverageRouter)
  const insufficientBalance = collateralBigInt > usdcBalance

  const handleOpenSuccess = useCallback(() => {
    refetchBalances?.()
    onPositionOpened?.()
    setCollateralAmount('')
    setTargetLeverage(2)
    setTrackedTxId(null)
  }, [refetchBalances, onPositionOpened])

  const handleOpenPosition = useCallback(() => {
    if (collateralBigInt <= 0n || previewLoading || expectedCollateralTokens <= 0n) return

    void transactionManager.executeOpenLeverage(selectedSide, collateralBigInt, contractLeverage, slippageBps, minCollateralOut, {
      onRetry: handleOpenPosition,
    }).then(handleOpenSuccess)
  }, [collateralBigInt, previewLoading, expectedCollateralTokens, selectedSide, contractLeverage, slippageBps, minCollateralOut, handleOpenSuccess])

  const getButtonText = () => {
    if (isRunning) return 'Processing...'
    if (insufficientBalance) return 'Insufficient USDC'
    if (needsMorphoAuthorization) return 'Authorize & Open Position'
    return `Open ${selectedSide} Position`
  }

  const isDisabled = !collateralAmount || parseFloat(collateralAmount) <= 0 || isRunning || insufficientBalance || previewLoading || expectedCollateralTokens <= 0n

  const expectedEquity = expectedPositionValue > expectedDebt ? expectedPositionValue - expectedDebt : 0n
  const positionSizeDisplay = previewLoading && collateralBigInt > 0n
    ? '...'
    : formatUsd(expectedPositionValue)
  const equityDisplay = previewLoading && collateralBigInt > 0n
    ? '...'
    : formatUsd(expectedEquity)
  const debtDisplay = previewLoading && collateralBigInt > 0n
    ? '...'
    : formatUsd(expectedDebt)

  return (
    <div className="mx-auto min-w-0 max-w-xl space-y-5 sm:space-y-6">
      <div className="space-y-2">
        <label className="text-sm font-medium text-content-secondary">Position Side</label>
        <div className="grid grid-cols-1 gap-3 sm:grid-cols-2 sm:gap-4">
          <button
            onClick={() => { setSelectedSide('BULL'); }}
            className={`relative min-h-16 min-w-0 p-3 text-center transition-colors hover:underline hover:underline-offset-4 hover:decoration-current sm:p-4 ${
              selectedSide === 'BULL'
                ? 'border-2 border-positive bg-positive/10 text-positive'
                : 'border border-brand-border/30 bg-surface-panel text-content-primary hover:border-positive/50 hover:bg-[#3B212D]'
            }`}
          >
            <div className={`break-words font-semibold ${selectedSide === 'BULL' ? 'text-positive' : 'text-content-primary'}`}>plDXY-BULL</div>
            <div className={`text-xs mt-1 ${selectedSide === 'BULL' ? 'text-positive/70' : 'text-content-secondary'}`}>Bullish on USD</div>
          </button>
          <button
            onClick={() => { setSelectedSide('BEAR'); }}
            className={`relative min-h-16 min-w-0 p-3 text-center transition-colors hover:underline hover:underline-offset-4 hover:decoration-current sm:p-4 ${
              selectedSide === 'BEAR'
                ? 'border-2 border-brand-orange bg-brand-orange/10 text-brand-orange'
                : 'border border-brand-border/30 bg-surface-panel text-content-primary hover:border-brand-orange/50 hover:bg-[#3B212D]'
            }`}
          >
            <div className={`break-words font-semibold ${selectedSide === 'BEAR' ? 'text-brand-orange' : 'text-content-primary'}`}>plDXY-BEAR</div>
            <div className={`text-xs mt-1 ${selectedSide === 'BEAR' ? 'text-brand-orange/70' : 'text-content-secondary'}`}>Bearish on USD</div>
          </button>
        </div>
      </div>

      <TokenInput
        label="Collateral (USDC)"
        value={collateralAmount}
        onChange={setCollateralAmount}
        token={{ symbol: 'USDC', decimals: 6 }}
        balance={usdcBalance}
      />

      <div>
        <div className="flex items-center justify-between mb-2">
          <label className="text-sm text-content-secondary flex items-center gap-1">
            Leverage
            <InfoTooltip content="Target leverage for your position. Max is based on Morpho LLTV and token price." />
          </label>
          <span className="text-content-primary font-medium">{targetLeverage.toFixed(1)}x</span>
        </div>
        <input
          type="range"
          min="1.1"
          max={maxEffectiveLeverage.toFixed(1)}
          step="0.1"
          value={Math.min(targetLeverage, maxEffectiveLeverage)}
          onChange={(e) => { setTargetLeverage(parseFloat(e.target.value)); }}
          className="leverage-slider h-2 w-full cursor-pointer appearance-none accent-brand-orange"
        />
        <div className="flex justify-between text-xs text-content-secondary mt-1">
          <span>1.1x</span>
          <span>{maxEffectiveLeverage.toFixed(1)}x</span>
        </div>
      </div>

      <div className="min-w-0 space-y-2 border border-brand-border/20 bg-app-bg p-3 sm:p-4">
        <div className="mb-3 text-xs font-medium uppercase text-content-secondary">PREVIEW</div>
        <div className="grid min-h-6 min-w-0 grid-cols-[auto_minmax(0,1fr)] items-center gap-3 text-sm">
          <span className="text-sm text-content-secondary">Position Value</span>
          <span className="min-w-0 break-words text-right font-semibold text-content-primary [overflow-wrap:anywhere]">{positionSizeDisplay} USDC</span>
        </div>
        <div className="grid min-h-6 min-w-0 grid-cols-[auto_minmax(0,1fr)] items-center gap-3 text-sm">
          <span className="text-sm text-content-secondary">Your Equity</span>
          <span className="min-w-0 break-words text-right font-semibold text-content-primary [overflow-wrap:anywhere]">{equityDisplay} USDC</span>
        </div>
        <div className="grid min-h-6 min-w-0 grid-cols-[auto_minmax(0,1fr)] items-center gap-3 text-sm">
          <span className="flex items-center gap-1 text-sm text-content-secondary">
            Debt
            <InfoTooltip content="USDC borrowed from Morpho against your position" />
          </span>
          <span className="min-w-0 break-words text-right font-semibold text-warning [overflow-wrap:anywhere]">{debtDisplay} USDC</span>
        </div>
      </div>

      {isConnected ? (
        <button
          onClick={handleOpenPosition}
          disabled={isDisabled}
          className="min-h-12 w-full bg-brand-orange px-4 py-3 text-base font-semibold text-app-bg transition-colors enabled:hover:bg-[#FF572D] enabled:hover:underline enabled:hover:underline-offset-4 disabled:cursor-not-allowed disabled:opacity-50 sm:px-6 sm:py-4 sm:text-lg"
        >
          {getButtonText()}
        </button>
      ) : (
        <button
          disabled
          className="w-full bg-surface-muted text-content-secondary font-semibold py-4 px-6 cursor-not-allowed"
        >
          Connect Wallet
        </button>
      )}

      <p className="text-xs text-content-secondary text-center">
        Leverage trading carries significant risk. You may lose your entire collateral.
      </p>
    </div>
  )
}
