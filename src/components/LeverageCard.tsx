import { useState, useCallback } from 'react'
import { useAccount, useWriteContract, useReadContract } from 'wagmi'
import { parseUnits, zeroAddress } from 'viem'
import { InfoTooltip } from './ui'
import { TokenInput } from './TokenInput'
import { formatUsd } from '../utils/formatters'
import { usePreviewOpenLeverage, useAllowance, useTransactionSequence, useTokenPrices, type TransactionStep } from '../hooks'
import { getAddresses, DEFAULT_CHAIN_ID } from '../contracts/addresses'
import { useSettingsStore } from '../stores/settingsStore'
import { ERC20_ABI, LEVERAGE_ROUTER_ABI, MORPHO_ABI } from '../contracts/abis'

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

  const [selectedSide, setSelectedSide] = useState<TokenSide>('BULL')
  const [collateralAmount, setCollateralAmount] = useState('')
  const [targetLeverage, setTargetLeverage] = useState(2)

  const routerAddress = selectedSide === 'BEAR' ? addresses.LEVERAGE_ROUTER : addresses.BULL_LEVERAGE_ROUTER

  const { bearPrice, bullPrice, cap } = useTokenPrices()
  const tokenPrice = selectedSide === 'BEAR' ? bearPrice : bullPrice

  // Max effective leverage from LLTV: maxLev = 1 / (1 - lltv)
  // LLTV = 91.5% → 1 / 0.085 = 11.76x
  const maxEffectiveLeverage = 11.76

  // Calculate contract leverage parameter from desired effective leverage
  // BULL: effectiveLeverage ≈ contractLeverage × tokenPrice / CAP (you only keep BULL portion)
  // BEAR: effectiveLeverage = contractLeverage (contract handles it directly)
  const contractLeverage = selectedSide === 'BULL' && tokenPrice > 0n && cap > 0n
    ? BigInt(Math.floor(targetLeverage * 1e18)) * cap / tokenPrice
    : BigInt(Math.floor(targetLeverage * 1e18))

  const collateralBigInt = collateralAmount ? parseUnits(collateralAmount, 6) : 0n

  const { expectedCollateralTokens, expectedDebt, isLoading: previewLoading } = usePreviewOpenLeverage(
    selectedSide,
    collateralBigInt,
    contractLeverage
  )

  // Expected tokens (18 dec) × price (8 dec) / 10^20 = 6 dec USDC
  const expectedPositionValue = expectedCollateralTokens * tokenPrice / 10n ** 20n

  // Get Morpho address from router
  const { data: morphoAddress } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'MORPHO',
  })

  // Check if user has authorized the router in Morpho
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
    onPositionOpened?.()
    setCollateralAmount('')
    setTargetLeverage(2)
  }, [refetchBalances, onPositionOpened])

  const buildOpenLeverageSteps = useCallback((): TransactionStep[] => {
    const steps: TransactionStep[] = []

    if (needsMorphoAuthorization && morphoAddress) {
      steps.push({
        label: 'Authorize Morpho',
        action: async () => {
          const hash = await writeContractAsync({
            address: morphoAddress,
            abi: MORPHO_ABI,
            functionName: 'setAuthorization',
            args: [routerAddress, true],
          })
          await refetchAuthorization()
          return hash
        },
      })
    }

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
        console.log('[openLeverage] args:', {
          principal: collateralBigInt.toString(),
          leverage: contractLeverage.toString(),
          slippageBps: slippageBps.toString(),
          deadline: deadline.toString(),
        })
        return writeContractAsync({
          address: routerAddress,
          abi: LEVERAGE_ROUTER_ABI,
          functionName: 'openLeverage',
          args: [collateralBigInt, contractLeverage, slippageBps, deadline],
        })
      },
    })

    return steps
  }, [
    needsMorphoAuthorization,
    morphoAddress,
    needsUsdcApproval,
    selectedSide,
    collateralBigInt,
    contractLeverage,
    slippage,
    addresses.USDC,
    routerAddress,
    writeContractAsync,
    refetchAuthorization,
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
    if (needsMorphoAuthorization && needsUsdcApproval) return 'Authorize, Approve & Open'
    if (needsMorphoAuthorization) return 'Authorize & Open Position'
    if (needsUsdcApproval) return 'Approve & Open Position'
    return `Open ${selectedSide} Position`
  }

  const isDisabled = !collateralAmount || parseFloat(collateralAmount) <= 0 || sequence.isRunning || insufficientBalance

  const collateralNum = parseFloat(collateralAmount) || 0
  const positionSizeDisplay = previewLoading && collateralBigInt > 0n
    ? '...'
    : formatUsd(expectedPositionValue)
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
          <span className="text-cyber-text-primary">{positionSizeDisplay}</span>
        </div>
        <div className="flex justify-between">
          <span className="text-cyber-text-secondary text-sm">Your Equity</span>
          <span className="text-cyber-text-primary">{formatUsd(BigInt(Math.floor(collateralNum * 1e6)))}</span>
        </div>
        <div className="flex justify-between">
          <span className="text-cyber-text-secondary text-sm flex items-center gap-1">
            Debt
            <InfoTooltip content="USDC borrowed from Morpho against your position" />
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
