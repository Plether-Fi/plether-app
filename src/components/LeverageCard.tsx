import { useState, useEffect, useRef, useCallback } from 'react'
import { useAccount } from 'wagmi'
import { parseUnits, formatUnits } from 'viem'
import { InfoTooltip } from './ui'
import { TokenInput } from './TokenInput'
import { formatUsd } from '../utils/formatters'
import { usePreviewOpenLeverage, useOpenLeverage, useAllowance, useApprove } from '../hooks'
import { getAddresses, DEFAULT_CHAIN_ID } from '../contracts/addresses'
import { useSettingsStore } from '../stores/settingsStore'

type TokenSide = 'BEAR' | 'BULL'

export interface LeverageCardProps {
  usdcBalance: bigint
  refetchBalances?: () => void
}

export function LeverageCard({ usdcBalance, refetchBalances }: LeverageCardProps) {
  const { isConnected, chainId } = useAccount()
  const addresses = getAddresses(chainId ?? DEFAULT_CHAIN_ID)
  const slippage = useSettingsStore((s) => s.slippage)

  const [selectedSide, setSelectedSide] = useState<TokenSide>('BEAR')
  const [collateralAmount, setCollateralAmount] = useState('')
  const [leverage, setLeverage] = useState(2)
  const [pendingOpen, setPendingOpen] = useState(false)
  const pendingAmountRef = useRef<bigint>(0n)
  const approveHandledRef = useRef(false)

  const collateralBigInt = collateralAmount ? parseUnits(collateralAmount, 6) : 0n
  const leverageBps = BigInt(Math.floor(leverage * 100))

  const { positionSize, liquidationPrice: previewLiqPrice, isLoading: previewLoading } = usePreviewOpenLeverage(
    selectedSide,
    collateralBigInt,
    leverageBps
  )

  const { openPosition, isPending, isSuccess, reset } = useOpenLeverage(selectedSide)

  const routerAddress = selectedSide === 'BEAR' ? addresses.LEVERAGE_ROUTER : addresses.BULL_LEVERAGE_ROUTER
  const { allowance, refetch: refetchAllowance } = useAllowance(addresses.USDC, routerAddress)
  const { approve, isPending: approvePending, isSuccess: approveSuccess } = useApprove(addresses.USDC, routerAddress)

  const needsApproval = collateralBigInt > 0n && allowance < collateralBigInt
  const insufficientBalance = collateralBigInt > usdcBalance

  const executeOpenPosition = useCallback(async (amount: bigint) => {
    const slippageBps = BigInt(Math.floor(slippage * 100))
    const deadline = BigInt(Math.floor(Date.now() / 1000) + 1800)
    await openPosition(amount, leverageBps, slippageBps, deadline)
  }, [slippage, leverageBps, openPosition])

  useEffect(() => {
    if (approveSuccess && !approveHandledRef.current) {
      approveHandledRef.current = true
      void refetchAllowance()
      if (pendingOpen && pendingAmountRef.current > 0n) {
        void executeOpenPosition(pendingAmountRef.current)
        pendingAmountRef.current = 0n
        setPendingOpen(false)
      }
    }
  }, [approveSuccess, refetchAllowance, pendingOpen, executeOpenPosition])

  useEffect(() => {
    if (isSuccess) {
      refetchBalances?.()
      setCollateralAmount('')
      setLeverage(2)
      reset()
    }
  }, [isSuccess, refetchBalances, reset])

  const handleOpenPosition = async () => {
    if (!collateralBigInt || collateralBigInt <= 0n) return

    approveHandledRef.current = false

    if (needsApproval) {
      pendingAmountRef.current = collateralBigInt
      setPendingOpen(true)
      await approve(collateralBigInt)
      return
    }

    await executeOpenPosition(collateralBigInt)
  }

  const getButtonText = () => {
    if (isPending) return 'Opening Position...'
    if (approvePending) return 'Approving USDC...'
    if (insufficientBalance) return 'Insufficient USDC'
    if (needsApproval) return 'Approve USDC'
    return `Open ${selectedSide} Position`
  }

  const isActionPending = isPending || approvePending
  const isDisabled = !collateralAmount || parseFloat(collateralAmount) <= 0 || isActionPending || insufficientBalance

  const collateralNum = parseFloat(collateralAmount) || 0
  const positionSizeDisplay = previewLoading && collateralBigInt > 0n
    ? '...'
    : formatUsd(positionSize)
  const liquidationPriceDisplay = previewLoading && collateralBigInt > 0n
    ? '...'
    : previewLiqPrice > 0n
      ? `$${parseFloat(formatUnits(previewLiqPrice, 6)).toFixed(2)}`
      : '$0.00'

  return (
    <div className="max-w-xl mx-auto space-y-6">
      <div className="space-y-2">
        <label className="text-sm font-medium text-cyber-text-secondary">Position Side</label>
        <div className="grid grid-cols-2 gap-4">
          <button
            onClick={() => { setSelectedSide('BEAR'); }}
            className={`relative p-4 text-center transition-all ${
              selectedSide === 'BEAR'
                ? 'border-2 border-cyber-electric-fuchsia bg-cyber-electric-fuchsia/10 shadow-md shadow-cyber-electric-fuchsia/20'
                : 'border border-cyber-border-glow/30 bg-cyber-surface-dark hover:border-cyber-electric-fuchsia/50 opacity-60 hover:opacity-100'
            }`}
          >
            <div className={`font-semibold ${selectedSide === 'BEAR' ? 'text-cyber-electric-fuchsia' : 'text-cyber-text-primary'}`}>DXY-BEAR</div>
            <div className={`text-xs mt-1 ${selectedSide === 'BEAR' ? 'text-cyber-electric-fuchsia/70' : 'text-cyber-text-secondary'}`}>Bearish on USD</div>
          </button>
          <button
            onClick={() => { setSelectedSide('BULL'); }}
            className={`relative p-4 text-center transition-all ${
              selectedSide === 'BULL'
                ? 'border-2 border-cyber-neon-green bg-cyber-neon-green/10 shadow-md shadow-cyber-neon-green/20'
                : 'border border-cyber-border-glow/30 bg-cyber-surface-dark hover:border-cyber-neon-green/50 opacity-60 hover:opacity-100'
            }`}
          >
            <div className={`font-semibold ${selectedSide === 'BULL' ? 'text-cyber-neon-green' : 'text-cyber-text-primary'}`}>DXY-BULL</div>
            <div className={`text-xs mt-1 ${selectedSide === 'BULL' ? 'text-cyber-neon-green/70' : 'text-cyber-text-secondary'}`}>Bullish on USD</div>
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
          <label className="text-sm text-cyber-text-secondary flex items-center gap-1">
            Leverage
            <InfoTooltip content="Higher leverage increases both potential profits and liquidation risk" />
          </label>
          <span className="text-cyber-text-primary font-medium">{leverage}x</span>
        </div>
        <input
          type="range"
          min="1.1"
          max="5"
          step="0.1"
          value={leverage}
          onChange={(e) => { setLeverage(parseFloat(e.target.value)); }}
          className="w-full h-2 bg-cyber-surface-light appearance-none cursor-pointer accent-cyber-electric-fuchsia"
        />
        <div className="flex justify-between text-xs text-cyber-text-secondary mt-1">
          <span>1.1x</span>
          <span>5x</span>
        </div>
      </div>

      <div className="bg-cyber-surface-light p-4 space-y-3 border border-cyber-border-glow/30">
        <h4 className="text-sm font-medium text-cyber-text-secondary">Position Preview</h4>
        <div className="flex justify-between">
          <span className="text-cyber-text-secondary text-sm">Position Size</span>
          <span className="text-cyber-text-primary">{positionSizeDisplay}</span>
        </div>
        <div className="flex justify-between">
          <span className="text-cyber-text-secondary text-sm">Collateral</span>
          <span className="text-cyber-text-primary">{formatUsd(BigInt(Math.floor(collateralNum * 1e6)))}</span>
        </div>
        <div className="flex justify-between">
          <span className="text-cyber-text-secondary text-sm flex items-center gap-1">
            Liquidation Price
            <InfoTooltip content="If DXY reaches this price, your position will be liquidated" />
          </span>
          <span className="text-cyber-warning-text">{liquidationPriceDisplay}</span>
        </div>
      </div>

      {isConnected ? (
        <button
          onClick={() => void handleOpenPosition()}
          disabled={isDisabled}
          className="w-full bg-cyber-electric-fuchsia hover:bg-cyber-electric-fuchsia/90 text-cyber-bg font-semibold py-4 px-6 shadow-lg shadow-cyber-electric-fuchsia/40 transition-all transform hover:-translate-y-0.5 active:translate-y-0 text-lg disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none disabled:shadow-none"
        >
          {getButtonText()}
        </button>
      ) : (
        <button
          disabled
          className="w-full bg-cyber-surface-light text-cyber-text-secondary font-semibold py-4 px-6 cursor-not-allowed"
        >
          Connect Wallet
        </button>
      )}

      <p className="text-xs text-cyber-text-secondary text-center">
        Leverage trading carries significant risk. You may lose your entire collateral.
      </p>
    </div>
  )
}
