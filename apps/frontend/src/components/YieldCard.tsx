import { useState, useCallback } from 'react'
import { useAccount, useWriteContract, useReadContract } from 'wagmi'
import { parseUnits, zeroAddress, type Address } from 'viem'
import { TokenInput } from './TokenInput'
import { formatUsd } from '../utils/formatters'
import { useTransactionSequence, useAllowance, useMorphoApy, type TransactionStep } from '../hooks'
import { getAddresses, DEFAULT_CHAIN_ID } from '../contracts/addresses'
import { ERC20_ABI, LEVERAGE_ROUTER_ABI, MORPHO_ABI } from '../contracts/abis'

type SupplyMode = 'supply' | 'withdraw'
type BorrowMode = 'borrow' | 'repay'
type MarketSide = 'BEAR' | 'BULL'

export interface MarketData {
  suppliedAmount: bigint
  borrowedAmount: bigint
  availableToBorrow: bigint
  collateral: bigint
}

export interface YieldCardProps {
  bearMarket: MarketData
  bullMarket: MarketData
  usdcBalance: bigint
  onSuccess?: () => void
}

interface MarketParams {
  loanToken: Address
  collateralToken: Address
  oracle: Address
  irm: Address
  lltv: bigint
}

function useMarketConfig(side: MarketSide) {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const routerAddress = side === 'BEAR' ? addresses?.LEVERAGE_ROUTER : addresses?.BULL_LEVERAGE_ROUTER

  const { data: morphoAddress } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'MORPHO',
    query: { enabled: !!routerAddress },
  })

  const { data: marketParamsRaw } = useReadContract({
    address: routerAddress,
    abi: LEVERAGE_ROUTER_ABI,
    functionName: 'marketParams',
    query: { enabled: !!routerAddress },
  })

  const marketParams: MarketParams | undefined = marketParamsRaw
    ? {
        loanToken: marketParamsRaw[0],
        collateralToken: marketParamsRaw[1],
        oracle: marketParamsRaw[2],
        irm: marketParamsRaw[3],
        lltv: marketParamsRaw[4],
      }
    : undefined

  return { morphoAddress, marketParams }
}

interface MarketColumnProps {
  side: MarketSide
  market: MarketData
  usdcBalance: bigint
  onSuccess?: () => void
}

function MarketColumn({ side, market, usdcBalance, onSuccess }: MarketColumnProps) {
  const { isConnected, address, chainId } = useAccount()
  const addresses = getAddresses(chainId ?? DEFAULT_CHAIN_ID)
  const { morphoAddress, marketParams } = useMarketConfig(side)
  const { supplyApy, borrowApy, utilization } = useMorphoApy(side)

  const [supplyMode, setSupplyMode] = useState<SupplyMode>('supply')
  const [borrowMode, setBorrowMode] = useState<BorrowMode>('borrow')
  const [supplyAmount, setSupplyAmount] = useState('')
  const [borrowAmount, setBorrowAmount] = useState('')

  const supplyBigInt = supplyAmount ? parseUnits(supplyAmount, 6) : 0n
  const borrowBigInt = borrowAmount ? parseUnits(borrowAmount, 6) : 0n

  const { allowance: usdcAllowance, refetch: refetchAllowance } = useAllowance(
    addresses.USDC,
    morphoAddress ?? zeroAddress
  )

  const { writeContractAsync } = useWriteContract()
  const supplySequence = useTransactionSequence()
  const borrowSequence = useTransactionSequence()

  const hasCollateral = market.collateral > 0n
  const needsApprovalForSupply = supplyMode === 'supply' && supplyBigInt > 0n && usdcAllowance < supplyBigInt
  const needsApprovalForRepay = borrowMode === 'repay' && borrowBigInt > 0n && usdcAllowance < borrowBigInt
  const insufficientBalanceSupply = supplyMode === 'supply' && supplyBigInt > usdcBalance
  const insufficientBalanceRepay = borrowMode === 'repay' && borrowBigInt > usdcBalance
  const insufficientSupplied = supplyMode === 'withdraw' && supplyBigInt > market.suppliedAmount
  const insufficientBorrowable = borrowMode === 'borrow' && borrowBigInt > market.availableToBorrow

  const buildSupplySteps = useCallback((): TransactionStep[] => {
    if (!morphoAddress || !marketParams || !address) return []
    const steps: TransactionStep[] = []

    if (supplyMode === 'supply') {
      if (needsApprovalForSupply) {
        steps.push({
          label: 'Approve USDC',
          action: async () => {
            const hash = await writeContractAsync({
              address: addresses.USDC,
              abi: ERC20_ABI,
              functionName: 'approve',
              args: [morphoAddress, supplyBigInt],
            })
            await refetchAllowance()
            return hash
          },
        })
      }

      steps.push({
        label: 'Supply USDC',
        action: () => writeContractAsync({
          address: morphoAddress,
          abi: MORPHO_ABI,
          functionName: 'supply',
          args: [marketParams, supplyBigInt, 0n, address, '0x'],
        }),
      })
    } else {
      steps.push({
        label: 'Withdraw USDC',
        action: () => writeContractAsync({
          address: morphoAddress,
          abi: MORPHO_ABI,
          functionName: 'withdraw',
          args: [marketParams, supplyBigInt, 0n, address, address],
        }),
      })
    }

    return steps
  }, [supplyMode, supplyBigInt, needsApprovalForSupply, morphoAddress, marketParams, address, addresses, writeContractAsync, refetchAllowance])

  const buildBorrowSteps = useCallback((): TransactionStep[] => {
    if (!morphoAddress || !marketParams || !address) return []
    const steps: TransactionStep[] = []

    if (borrowMode === 'borrow') {
      steps.push({
        label: 'Borrow USDC',
        action: () => writeContractAsync({
          address: morphoAddress,
          abi: MORPHO_ABI,
          functionName: 'borrow',
          args: [marketParams, borrowBigInt, 0n, address, address],
        }),
      })
    } else {
      if (needsApprovalForRepay) {
        steps.push({
          label: 'Approve USDC',
          action: async () => {
            const hash = await writeContractAsync({
              address: addresses.USDC,
              abi: ERC20_ABI,
              functionName: 'approve',
              args: [morphoAddress, borrowBigInt],
            })
            await refetchAllowance()
            return hash
          },
        })
      }

      steps.push({
        label: 'Repay USDC',
        action: () => writeContractAsync({
          address: morphoAddress,
          abi: MORPHO_ABI,
          functionName: 'repay',
          args: [marketParams, borrowBigInt, 0n, address, '0x'],
        }),
      })
    }

    return steps
  }, [borrowMode, borrowBigInt, needsApprovalForRepay, morphoAddress, marketParams, address, addresses, writeContractAsync, refetchAllowance])

  const handleSupply = useCallback(() => {
    if (supplyBigInt <= 0n || !morphoAddress || !marketParams) return

    void supplySequence.execute({
      title: supplyMode === 'supply' ? `Supplying USDC to ${side}` : `Withdrawing USDC from ${side}`,
      buildSteps: buildSupplySteps,
      onSuccess: () => {
        setSupplyAmount('')
        onSuccess?.()
      },
    })
  }, [supplyBigInt, morphoAddress, marketParams, supplyMode, side, supplySequence, buildSupplySteps, onSuccess])

  const handleBorrow = useCallback(() => {
    if (borrowBigInt <= 0n || !morphoAddress || !marketParams) return

    void borrowSequence.execute({
      title: borrowMode === 'borrow' ? `Borrowing USDC from ${side}` : `Repaying USDC to ${side}`,
      buildSteps: buildBorrowSteps,
      onSuccess: () => {
        setBorrowAmount('')
        onSuccess?.()
      },
    })
  }, [borrowBigInt, morphoAddress, marketParams, borrowMode, side, borrowSequence, buildBorrowSteps, onSuccess])

  const getSupplyButtonText = () => {
    if (supplySequence.isRunning) return 'Processing...'
    if (supplyMode === 'supply') {
      if (insufficientBalanceSupply) return 'Insufficient USDC'
      if (needsApprovalForSupply) return 'Approve & Supply'
      return 'Supply'
    }
    if (insufficientSupplied) return 'Insufficient'
    return 'Withdraw'
  }

  const getBorrowButtonText = () => {
    if (borrowSequence.isRunning) return 'Processing...'
    if (borrowMode === 'borrow') {
      if (!hasCollateral) return 'No Collateral'
      if (insufficientBorrowable) return 'Exceeds Available'
      return 'Borrow'
    }
    if (insufficientBalanceRepay) return 'Insufficient USDC'
    if (needsApprovalForRepay) return 'Approve & Repay'
    return 'Repay'
  }

  const isSupplyDisabled = !supplyAmount ||
    parseFloat(supplyAmount) <= 0 ||
    supplySequence.isRunning ||
    (supplyMode === 'supply' && insufficientBalanceSupply) ||
    (supplyMode === 'withdraw' && insufficientSupplied) ||
    !isConnected

  const isBorrowDisabled = !borrowAmount ||
    parseFloat(borrowAmount) <= 0 ||
    borrowSequence.isRunning ||
    (borrowMode === 'borrow' && (insufficientBorrowable || !hasCollateral)) ||
    (borrowMode === 'repay' && insufficientBalanceRepay) ||
    !isConnected

  const accentColor = side === 'BEAR' ? 'cyber-electric-fuchsia' : 'cyber-neon-green'
  const accentColorClass = side === 'BEAR' ? 'text-cyber-electric-fuchsia' : 'text-cyber-neon-green'
  const borderClass = side === 'BEAR' ? 'border-cyber-electric-fuchsia/30' : 'border-cyber-neon-green/30'
  const bgClass = side === 'BEAR' ? 'bg-cyber-electric-fuchsia/10' : 'bg-cyber-neon-green/10'

  return (
    <div className={`flex-1 border ${borderClass} bg-cyber-surface-dark`}>
      {/* Header */}
      <div className={`${bgClass} px-4 py-3 border-b ${borderClass}`}>
        <h3 className={`font-semibold ${accentColorClass}`}>{side} Market</h3>
        <p className="text-xs text-cyber-text-secondary mt-0.5">splDXY-{side} collateral</p>
      </div>

      {/* Stats */}
      <div className="p-4 space-y-2 border-b border-cyber-border-glow/20">
        <div className="flex justify-between text-sm">
          <span className="text-cyber-text-secondary">Supplied</span>
          <span className="text-cyber-text-primary font-medium">{formatUsd(market.suppliedAmount)} USDC</span>
        </div>
        <div className="flex justify-between text-sm">
          <span className="text-cyber-text-secondary">Supply APY</span>
          <span className="text-cyber-neon-green font-medium">{(supplyApy * 100).toFixed(2)}%</span>
        </div>
        <div className="flex justify-between text-sm">
          <span className="text-cyber-text-secondary">Borrowed</span>
          <span className="text-cyber-text-primary font-medium">{formatUsd(market.borrowedAmount)} USDC</span>
        </div>
        <div className="flex justify-between text-sm">
          <span className="text-cyber-text-secondary">Borrow APY</span>
          <span className="text-cyber-warning-text font-medium">{(borrowApy * 100).toFixed(2)}%</span>
        </div>
        <div className="flex justify-between text-sm">
          <span className="text-cyber-text-secondary">Utilization</span>
          <span className="text-cyber-text-primary font-medium">{(utilization * 100).toFixed(1)}%</span>
        </div>
        <div className="flex justify-between text-sm">
          <span className="text-cyber-text-secondary">Collateral</span>
          <span className={`font-medium ${hasCollateral ? accentColorClass : 'text-cyber-text-secondary'}`}>
            {hasCollateral ? `${formatUsd(market.collateral)} USDC` : 'None'}
          </span>
        </div>
        <div className="flex justify-between text-sm">
          <span className="text-cyber-text-secondary">Available</span>
          <span className="text-cyber-text-primary font-medium">{formatUsd(market.availableToBorrow)} USDC</span>
        </div>
      </div>

      {/* Supply Section */}
      <div className="p-4 border-b border-cyber-border-glow/20">
        <div className="flex text-xs font-medium mb-3 border border-cyber-border-glow/30">
          <button
            onClick={() => { setSupplyMode('supply'); setSupplyAmount('') }}
            className={`flex-1 py-1.5 px-3 transition-all ${
              supplyMode === 'supply'
                ? `${bgClass} ${accentColorClass}`
                : 'text-cyber-text-secondary hover:text-cyber-text-primary'
            }`}
          >
            Supply
          </button>
          <button
            onClick={() => { setSupplyMode('withdraw'); setSupplyAmount('') }}
            className={`flex-1 py-1.5 px-3 transition-all ${
              supplyMode === 'withdraw'
                ? `${bgClass} ${accentColorClass}`
                : 'text-cyber-text-secondary hover:text-cyber-text-primary'
            }`}
          >
            Withdraw
          </button>
        </div>
        <div className="space-y-3">
          <TokenInput
            value={supplyAmount}
            onChange={setSupplyAmount}
            token={{ symbol: 'USDC', decimals: 6 }}
            balance={supplyMode === 'supply' ? usdcBalance : market.suppliedAmount}
            compact
          />
          <button
            onClick={handleSupply}
            disabled={isSupplyDisabled}
            className={`w-full bg-${accentColor} hover:bg-${accentColor}/90 text-cyber-bg font-semibold py-2 px-4 text-sm shadow-lg shadow-${accentColor}/30 transition-all disabled:opacity-50 disabled:cursor-not-allowed`}
            style={{
              backgroundColor: isSupplyDisabled ? undefined : (side === 'BEAR' ? '#FF00CC' : '#00FF99'),
              boxShadow: isSupplyDisabled ? undefined : `0 10px 15px -3px ${side === 'BEAR' ? 'rgba(255,0,204,0.3)' : 'rgba(0,255,153,0.3)'}`,
            }}
          >
            {getSupplyButtonText()}
          </button>
        </div>
      </div>

      {/* Borrow Section */}
      <div className="p-4">
        <div className="flex text-xs font-medium mb-3 border border-cyber-border-glow/30">
          <button
            onClick={() => { setBorrowMode('borrow'); setBorrowAmount('') }}
            className={`flex-1 py-1.5 px-3 transition-all ${
              borrowMode === 'borrow'
                ? `${bgClass} ${accentColorClass}`
                : 'text-cyber-text-secondary hover:text-cyber-text-primary'
            }`}
          >
            Borrow
          </button>
          <button
            onClick={() => { setBorrowMode('repay'); setBorrowAmount('') }}
            className={`flex-1 py-1.5 px-3 transition-all ${
              borrowMode === 'repay'
                ? `${bgClass} ${accentColorClass}`
                : 'text-cyber-text-secondary hover:text-cyber-text-primary'
            }`}
          >
            Repay
          </button>
        </div>
        <div className="space-y-3">
          <TokenInput
            value={borrowAmount}
            onChange={setBorrowAmount}
            token={{ symbol: 'USDC', decimals: 6 }}
            balance={borrowMode === 'borrow' ? market.availableToBorrow : market.borrowedAmount}
            balanceLabel={borrowMode === 'borrow' ? 'Available:' : 'Owed:'}
            compact
          />
          {borrowMode === 'borrow' && !hasCollateral && (
            <p className="text-xs text-cyber-warning-text">
              Open a {side} leverage position to borrow
            </p>
          )}
          <button
            onClick={handleBorrow}
            disabled={isBorrowDisabled}
            className={`w-full bg-${accentColor} hover:bg-${accentColor}/90 text-cyber-bg font-semibold py-2 px-4 text-sm shadow-lg shadow-${accentColor}/30 transition-all disabled:opacity-50 disabled:cursor-not-allowed`}
            style={{
              backgroundColor: isBorrowDisabled ? undefined : (side === 'BEAR' ? '#FF00CC' : '#00FF99'),
              boxShadow: isBorrowDisabled ? undefined : `0 10px 15px -3px ${side === 'BEAR' ? 'rgba(255,0,204,0.3)' : 'rgba(0,255,153,0.3)'}`,
            }}
          >
            {getBorrowButtonText()}
          </button>
        </div>
      </div>
    </div>
  )
}

export function YieldCard({
  bearMarket,
  bullMarket,
  usdcBalance,
  onSuccess,
}: YieldCardProps) {
  return (
    <div className="max-w-4xl mx-auto">
      {/* Summary row */}
      <div className="grid grid-cols-4 gap-4 mb-6">
        <div className="bg-cyber-surface-light p-3 border border-cyber-border-glow/30">
          <p className="text-xs text-cyber-text-secondary">Total Supplied</p>
          <p className="text-lg font-bold text-cyber-text-primary">
            {formatUsd(bearMarket.suppliedAmount + bullMarket.suppliedAmount)} USDC
          </p>
        </div>
        <div className="bg-cyber-surface-light p-3 border border-cyber-border-glow/30">
          <p className="text-xs text-cyber-text-secondary">Total Borrowed</p>
          <p className="text-lg font-bold text-cyber-text-primary">
            {formatUsd(bearMarket.borrowedAmount + bullMarket.borrowedAmount)} USDC
          </p>
        </div>
        <div className="bg-cyber-surface-light p-3 border border-cyber-border-glow/30">
          <p className="text-xs text-cyber-text-secondary">Total Collateral</p>
          <p className="text-lg font-bold text-cyber-text-primary">
            {formatUsd(bearMarket.collateral + bullMarket.collateral)} USDC
          </p>
        </div>
        <div className="bg-cyber-surface-light p-3 border border-cyber-border-glow/30">
          <p className="text-xs text-cyber-text-secondary">Total Available</p>
          <p className="text-lg font-bold text-cyber-text-primary">
            {formatUsd(bearMarket.availableToBorrow + bullMarket.availableToBorrow)} USDC
          </p>
        </div>
      </div>

      {/* Two-column market layout */}
      <div className="flex gap-6">
        <MarketColumn
          side="BULL"
          market={bullMarket}
          usdcBalance={usdcBalance}
          onSuccess={onSuccess}
        />
        <MarketColumn
          side="BEAR"
          market={bearMarket}
          usdcBalance={usdcBalance}
          onSuccess={onSuccess}
        />
      </div>
    </div>
  )
}
