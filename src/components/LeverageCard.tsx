import { useState, useCallback } from 'react'
import { useAccount, useWriteContract } from 'wagmi'
import { parseUnits } from 'viem'
import { InfoTooltip } from './ui'
import { TokenInput } from './TokenInput'
import { formatUsd } from '../utils/formatters'
import { usePreviewOpenLeverage, useAllowance, useTransactionSequence, type TransactionStep } from '../hooks'
import { getAddresses, DEFAULT_CHAIN_ID } from '../contracts/addresses'
import { useSettingsStore } from '../stores/settingsStore'
import { ERC20_ABI, LEVERAGE_ROUTER_ABI } from '../contracts/abis'

type TokenSide = 'BEAR' | 'BULL'

export interface LeverageCardProps {
  usdcBalance: bigint
  refetchBalances?: () => void
}

export function LeverageCard({ usdcBalance, refetchBalances }: LeverageCardProps) {
  const { isConnected, chainId } = useAccount()
  const addresses = getAddresses(chainId ?? DEFAULT_CHAIN_ID)
  const slippage = useSettingsStore((s) => s.slippage)

  const [selectedSide, setSelectedSide] = useState<TokenSide>('BULL')
  const [collateralAmount, setCollateralAmount] = useState('')
  const [leverage, setLeverage] = useState(2)

  const collateralBigInt = collateralAmount ? parseUnits(collateralAmount, 6) : 0n
  const leverageWei = parseUnits(leverage.toString(), 18)

  const { totalUSDC, expectedDebt, isLoading: previewLoading } = usePreviewOpenLeverage(
    selectedSide,
    collateralBigInt,
    leverageWei
  )

  const routerAddress = selectedSide === 'BEAR' ? addresses.LEVERAGE_ROUTER : addresses.BULL_LEVERAGE_ROUTER

  const { writeContractAsync } = useWriteContract()
  const sequence = useTransactionSequence()

  const { allowance: usdcAllowance, refetch: refetchUsdcAllowance } = useAllowance(
    addresses.USDC,
    routerAddress
  )

  const needsUsdcApproval = collateralBigInt > 0n && usdcAllowance < collateralBigInt
  const insufficientBalance = collateralBigInt > usdcBalance

  const handleSuccess = useCallback(() => {
    refetchBalances?.()
    setCollateralAmount('')
    setLeverage(2)
  }, [refetchBalances])

  const buildOpenLeverageSteps = useCallback((): TransactionStep[] => {
    const steps: TransactionStep[] = []

    if (needsUsdcApproval) {
      steps.push({
        label: 'Approve USDC',
        action: async () => {
          const hash = await writeContractAsync({
            address: addresses.USDC,
            abi: ERC20_ABI,
            functionName: 'approve',
            args: [routerAddress, collateralBigInt],
          })
          await refetchUsdcAllowance()
          return hash
        },
      })
    }

    steps.push({
      label: `Open ${selectedSide} position`,
      action: () => {
        const slippageBps = BigInt(Math.floor(slippage * 100))
        const deadline = BigInt(Math.floor(Date.now() / 1000) + 1800)
        return writeContractAsync({
          address: routerAddress,
          abi: LEVERAGE_ROUTER_ABI,
          functionName: 'openLeverage',
          args: [collateralBigInt, leverageWei, slippageBps, deadline],
        })
      },
    })

    return steps
  }, [
    needsUsdcApproval,
    selectedSide,
    collateralBigInt,
    leverageWei,
    slippage,
    addresses.USDC,
    routerAddress,
    writeContractAsync,
    refetchUsdcAllowance,
  ])

  const handleOpenPosition = useCallback(() => {
    if (collateralBigInt <= 0n) return

    void sequence.execute({
      title: `Opening ${selectedSide} leverage position`,
      buildSteps: buildOpenLeverageSteps,
      onSuccess: handleSuccess,
    })
  }, [collateralBigInt, selectedSide, sequence, buildOpenLeverageSteps, handleSuccess])

  const getButtonText = () => {
    if (sequence.isRunning) return 'Processing...'
    if (insufficientBalance) return 'Insufficient USDC'
    if (needsUsdcApproval) return 'Approve & Open Position'
    return `Open ${selectedSide} Position`
  }

  const isDisabled = !collateralAmount || parseFloat(collateralAmount) <= 0 || sequence.isRunning || insufficientBalance

  const collateralNum = parseFloat(collateralAmount) || 0
  const positionSizeDisplay = previewLoading && collateralBigInt > 0n
    ? '...'
    : formatUsd(totalUSDC)
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
            <div className={`font-semibold ${selectedSide === 'BULL' ? 'text-cyber-neon-green' : 'text-cyber-text-primary'}`}>DXY-BULL</div>
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
            <div className={`font-semibold ${selectedSide === 'BEAR' ? 'text-cyber-electric-fuchsia' : 'text-cyber-text-primary'}`}>DXY-BEAR</div>
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
          <span className="text-cyber-text-secondary text-sm">Total Position</span>
          <span className="text-cyber-text-primary">{positionSizeDisplay}</span>
        </div>
        <div className="flex justify-between">
          <span className="text-cyber-text-secondary text-sm">Collateral</span>
          <span className="text-cyber-text-primary">{formatUsd(BigInt(Math.floor(collateralNum * 1e6)))}</span>
        </div>
        <div className="flex justify-between">
          <span className="text-cyber-text-secondary text-sm flex items-center gap-1">
            Debt
            <InfoTooltip content="Amount borrowed to create the leveraged position" />
          </span>
          <span className="text-cyber-warning-text">{debtDisplay}</span>
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
