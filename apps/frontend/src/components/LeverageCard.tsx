import { useState, useCallback, useEffect } from 'react'
import { useAccount, useReadContract } from 'wagmi'
import { parseUnits, zeroAddress } from 'viem'
import { InfoTooltip } from './ui'
import { TokenInput } from './TokenInput'
import { formatUsd } from '../utils/formatters'
import { usePreviewOpenLeverage, useAllowance } from '../hooks'
import { useProtocolStatus } from '../api'
import { useTransactionStore } from '../stores/transactionStore'
import { transactionManager } from '../services/transactionManager'
import { getAddresses, DEFAULT_CHAIN_ID } from '../contracts/addresses'
import { useSettingsStore } from '../stores/settingsStore'
import { LEVERAGE_ROUTER_ABI, MORPHO_ABI } from '../contracts/abis'

type TokenSide = 'BEAR' | 'BULL'

export interface LeverageCardProps {
  usdcBalance: bigint
  refetchBalances?: () => void
  onPositionOpened?: () => void
}

export function LeverageCard({ usdcBalance, refetchBalances, onPositionOpened }: LeverageCardProps) {
  const { isConnected, address, chainId } = useAccount()
  const addresses = getAddresses(chainId ?? DEFAULT_CHAIN_ID)
  const slippage = useSettingsStore((s) => s.slippage)
  const transactions = useTransactionStore((s) => s.transactions)
  const activeOperations = useTransactionStore((s) => s.activeOperations)

  const [selectedSide, setSelectedSide] = useState<TokenSide>('BULL')
  const [collateralAmount, setCollateralAmount] = useState('')
  const [targetLeverage, setTargetLeverage] = useState(2)
  const [trackedTxId, setTrackedTxId] = useState<string | null>(null)

  const routerAddress = selectedSide === 'BEAR' ? addresses.LEVERAGE_ROUTER : addresses.BULL_LEVERAGE_ROUTER
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

  const { data: morphoAddress } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'MORPHO',
  })

  const { data: isAuthorized, refetch: refetchAuthorization } = useReadContract({
    address: morphoAddress,
    abi: MORPHO_ABI,
    functionName: 'isAuthorized',
    args: [address ?? zeroAddress, routerAddress],
    query: {
      enabled: !!morphoAddress && !!address,
    },
  })

  const needsMorphoAuthorization = !isAuthorized

  const { allowance: usdcAllowance, refetch: refetchUsdcAllowance } = useAllowance(
    addresses.USDC,
    routerAddress
  )

  const needsUsdcApproval = collateralBigInt > 0n && usdcAllowance < collateralBigInt
  const insufficientBalance = collateralBigInt > usdcBalance

  useEffect(() => {
    if (currentTx?.status === 'success') {
      refetchBalances?.()
      onPositionOpened?.()
      void refetchAuthorization()
      void refetchUsdcAllowance()
      setCollateralAmount('')
      setTargetLeverage(2)
      setTrackedTxId(null)
    }
  }, [currentTx?.status, refetchBalances, onPositionOpened, refetchAuthorization, refetchUsdcAllowance])

  const handleOpenPosition = useCallback(() => {
    if (collateralBigInt <= 0n) return

    const slippageBps = BigInt(Math.floor(slippage * 100))

    void transactionManager.executeOpenLeverage(selectedSide, collateralBigInt, contractLeverage, slippageBps, {
      onRetry: handleOpenPosition,
    })
  }, [collateralBigInt, selectedSide, contractLeverage, slippage])

  const getButtonText = () => {
    if (isRunning) return 'Processing...'
    if (insufficientBalance) return 'Insufficient USDC'
    if (needsMorphoAuthorization && needsUsdcApproval) return 'Authorize, Approve & Open'
    if (needsMorphoAuthorization) return 'Authorize & Open Position'
    if (needsUsdcApproval) return 'Approve & Open Position'
    return `Open ${selectedSide} Position`
  }

  const isDisabled = !collateralAmount || parseFloat(collateralAmount) <= 0 || isRunning || insufficientBalance

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
    <div className="max-w-xl mx-auto space-y-6">
      <div className="space-y-2">
        <label className="text-sm font-medium text-cyber-text-secondary">Position Side</label>
        <div className="grid grid-cols-2 gap-4">
          <button
            onClick={() => { setSelectedSide('BULL'); }}
            className={`relative p-4 text-center transition-all ${
              selectedSide === 'BULL'
                ? 'border-2 border-cyber-neon-green bg-cyber-neon-green/10 shadow-md shadow-cyber-neon-green/20'
                : 'border border-cyber-border-glow/30 bg-cyber-surface-dark hover:border-cyber-neon-green/50 opacity-60 hover:opacity-100'
            }`}
          >
            <div className={`font-semibold ${selectedSide === 'BULL' ? 'text-cyber-neon-green' : 'text-cyber-text-primary'}`}>plDXY-BULL</div>
            <div className={`text-xs mt-1 ${selectedSide === 'BULL' ? 'text-cyber-neon-green/70' : 'text-cyber-text-secondary'}`}>Bullish on USD</div>
          </button>
          <button
            onClick={() => { setSelectedSide('BEAR'); }}
            className={`relative p-4 text-center transition-all ${
              selectedSide === 'BEAR'
                ? 'border-2 border-cyber-electric-fuchsia bg-cyber-electric-fuchsia/10 shadow-md shadow-cyber-electric-fuchsia/20'
                : 'border border-cyber-border-glow/30 bg-cyber-surface-dark hover:border-cyber-electric-fuchsia/50 opacity-60 hover:opacity-100'
            }`}
          >
            <div className={`font-semibold ${selectedSide === 'BEAR' ? 'text-cyber-electric-fuchsia' : 'text-cyber-text-primary'}`}>plDXY-BEAR</div>
            <div className={`text-xs mt-1 ${selectedSide === 'BEAR' ? 'text-cyber-electric-fuchsia/70' : 'text-cyber-text-secondary'}`}>Bearish on USD</div>
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
            <InfoTooltip content="Target leverage for your position. Max is based on Morpho LLTV and token price." />
          </label>
          <span className="text-cyber-text-primary font-medium">{targetLeverage.toFixed(1)}x</span>
        </div>
        <input
          type="range"
          min="1.1"
          max={maxEffectiveLeverage.toFixed(1)}
          step="0.1"
          value={Math.min(targetLeverage, maxEffectiveLeverage)}
          onChange={(e) => { setTargetLeverage(parseFloat(e.target.value)); }}
          className="w-full h-2 bg-cyber-surface-light appearance-none cursor-pointer accent-cyber-electric-fuchsia"
        />
        <div className="flex justify-between text-xs text-cyber-text-secondary mt-1">
          <span>1.1x</span>
          <span>{maxEffectiveLeverage.toFixed(1)}x</span>
        </div>
      </div>

      <div className="bg-cyber-surface-light p-4 space-y-3 border border-cyber-border-glow/30">
        <h4 className="text-sm font-medium text-cyber-text-secondary">Position Preview</h4>
        <div className="flex justify-between">
          <span className="text-cyber-text-secondary text-sm">Position Value</span>
          <span className="text-cyber-text-primary">{positionSizeDisplay} USDC</span>
        </div>
        <div className="flex justify-between">
          <span className="text-cyber-text-secondary text-sm">Your Equity</span>
          <span className="text-cyber-text-primary">{equityDisplay} USDC</span>
        </div>
        <div className="flex justify-between">
          <span className="text-cyber-text-secondary text-sm flex items-center gap-1">
            Debt
            <InfoTooltip content="USDC borrowed from Morpho against your position" />
          </span>
          <span className="text-cyber-warning-text">{debtDisplay} USDC</span>
        </div>
      </div>

      {isConnected ? (
        <button
          onClick={handleOpenPosition}
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
