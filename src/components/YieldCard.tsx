import { useState, useCallback } from 'react'
import { useAccount, useWriteContract, useReadContract } from 'wagmi'
import { parseUnits, zeroAddress, type Address } from 'viem'
import { TokenInput } from './TokenInput'
import { formatUsd } from '../utils/formatters'
import { useTransactionSequence, useAllowance, type TransactionStep } from '../hooks'
import { getAddresses, DEFAULT_CHAIN_ID } from '../contracts/addresses'
import { ERC20_ABI, LEVERAGE_ROUTER_ABI, MORPHO_ABI } from '../contracts/abis'

type SupplyMode = 'supply' | 'withdraw'
type BorrowMode = 'borrow' | 'repay'

export interface YieldCardProps {
  suppliedAmount: bigint
  borrowedAmount: bigint
  availableToBorrow: bigint
  usdcBalance: bigint
  suppliedBalance: bigint
  hasCollateral?: boolean
  onSuccess?: () => void
}

interface MarketParams {
  loanToken: Address
  collateralToken: Address
  oracle: Address
  irm: Address
  lltv: bigint
}

function useMarketConfig() {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null
  const routerAddress = addresses?.LEVERAGE_ROUTER

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

export function YieldCard({
  suppliedAmount,
  borrowedAmount,
  availableToBorrow,
  usdcBalance,
  suppliedBalance,
  hasCollateral = false,
  onSuccess,
}: YieldCardProps) {
  const { isConnected, address, chainId } = useAccount()
  const addresses = getAddresses(chainId ?? DEFAULT_CHAIN_ID)
  const { morphoAddress, marketParams } = useMarketConfig()

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

  const needsApprovalForSupply = supplyMode === 'supply' && supplyBigInt > 0n && usdcAllowance < supplyBigInt
  const needsApprovalForRepay = borrowMode === 'repay' && borrowBigInt > 0n && usdcAllowance < borrowBigInt
  const insufficientBalanceSupply = supplyMode === 'supply' && supplyBigInt > usdcBalance
  const insufficientBalanceRepay = borrowMode === 'repay' && borrowBigInt > usdcBalance
  const insufficientSupplied = supplyMode === 'withdraw' && supplyBigInt > suppliedBalance
  const insufficientBorrowable = borrowMode === 'borrow' && borrowBigInt > availableToBorrow

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
      title: supplyMode === 'supply' ? 'Supplying USDC' : 'Withdrawing USDC',
      buildSteps: buildSupplySteps,
      onSuccess: () => {
        setSupplyAmount('')
        onSuccess?.()
      },
    })
  }, [supplyBigInt, morphoAddress, marketParams, supplyMode, supplySequence, buildSupplySteps, onSuccess])

  const handleBorrow = useCallback(() => {
    if (borrowBigInt <= 0n || !morphoAddress || !marketParams) return

    void borrowSequence.execute({
      title: borrowMode === 'borrow' ? 'Borrowing USDC' : 'Repaying USDC',
      buildSteps: buildBorrowSteps,
      onSuccess: () => {
        setBorrowAmount('')
        onSuccess?.()
      },
    })
  }, [borrowBigInt, morphoAddress, marketParams, borrowMode, borrowSequence, buildBorrowSteps, onSuccess])

  const formatMaxAmount = (amount: bigint, decimals: number) => {
    const value = Number(amount) / 10 ** decimals
    return value > 0 ? value.toString() : ''
  }

  const handleSuppliedClick = () => {
    setSupplyMode('withdraw')
    setSupplyAmount(formatMaxAmount(suppliedBalance, 6))
  }

  const handleBorrowedClick = () => {
    setBorrowMode('repay')
    setBorrowAmount(formatMaxAmount(borrowedAmount, 6))
  }

  const handleAvailableClick = () => {
    setBorrowMode('borrow')
    setBorrowAmount(formatMaxAmount(availableToBorrow, 6))
  }

  const getSupplyButtonText = () => {
    if (supplySequence.isRunning) return 'Processing...'
    if (supplyMode === 'supply') {
      if (insufficientBalanceSupply) return 'Insufficient USDC'
      if (needsApprovalForSupply) return 'Approve & Supply'
      return 'Supply USDC'
    }
    if (insufficientSupplied) return 'Insufficient Supplied'
    return 'Withdraw USDC'
  }

  const getBorrowButtonText = () => {
    if (borrowSequence.isRunning) return 'Processing...'
    if (borrowMode === 'borrow') {
      if (!hasCollateral) return 'No Collateral'
      if (insufficientBorrowable) return 'Exceeds Available'
      return 'Borrow USDC'
    }
    if (insufficientBalanceRepay) return 'Insufficient USDC'
    if (needsApprovalForRepay) return 'Approve & Repay'
    return 'Repay USDC'
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

  return (
    <div className="max-w-xl mx-auto space-y-6">
      <div className="grid grid-cols-3 gap-4">
        <button
          onClick={handleSuppliedClick}
          className="bg-cyber-surface-light p-3 border border-cyber-border-glow/30 text-left transition-all hover:border-cyber-neon-green/50 hover:shadow-md hover:shadow-cyber-neon-green/10"
        >
          <p className="text-xs text-cyber-text-secondary">Supplied</p>
          <p className="text-lg font-bold text-cyber-text-primary">{formatUsd(suppliedAmount)}</p>
        </button>
        <button
          onClick={handleBorrowedClick}
          className="bg-cyber-surface-light p-3 border border-cyber-border-glow/30 text-left transition-all hover:border-cyber-warning-text/50 hover:shadow-md hover:shadow-cyber-warning-text/10"
        >
          <p className="text-xs text-cyber-text-secondary">Borrowed</p>
          <p className="text-lg font-bold text-cyber-text-primary">{formatUsd(borrowedAmount)}</p>
        </button>
        <button
          onClick={handleAvailableClick}
          className="bg-cyber-surface-light p-3 border border-cyber-border-glow/30 text-left transition-all hover:border-cyber-bright-blue/50 hover:shadow-md hover:shadow-cyber-bright-blue/10"
        >
          <p className="text-xs text-cyber-text-secondary">Available</p>
          <p className="text-lg font-bold text-cyber-text-primary">{formatUsd(availableToBorrow)}</p>
          <p className="text-xs text-cyber-text-secondary">to borrow</p>
        </button>
      </div>

      <div className="bg-cyber-surface-light p-4 border border-cyber-border-glow/30">
        <h4 className="font-medium text-cyber-text-primary mb-4">Supply USDC</h4>
        <div className="bg-cyber-surface-dark p-1 flex text-sm font-medium mb-4 border border-cyber-border-glow/30">
          <button
            onClick={() => { setSupplyMode('supply'); setSupplyAmount('') }}
            className={`flex-1 py-2 px-4 transition-all ${
              supplyMode === 'supply'
                ? 'bg-cyber-surface-light text-cyber-neon-green shadow-sm shadow-cyber-neon-green/10 border border-cyber-neon-green/50'
                : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
            }`}
          >
            Supply
          </button>
          <button
            onClick={() => { setSupplyMode('withdraw'); setSupplyAmount('') }}
            className={`flex-1 py-2 px-4 transition-all ${
              supplyMode === 'withdraw'
                ? 'bg-cyber-surface-light text-cyber-neon-green shadow-sm shadow-cyber-neon-green/10 border border-cyber-neon-green/50'
                : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
            }`}
          >
            Withdraw
          </button>
        </div>
        <div className="space-y-4">
          <TokenInput
            value={supplyAmount}
            onChange={setSupplyAmount}
            token={{ symbol: 'USDC', decimals: 6 }}
            balance={supplyMode === 'supply' ? usdcBalance : suppliedBalance}
          />
          <button
            onClick={handleSupply}
            disabled={isSupplyDisabled}
            className="w-full bg-cyber-neon-green hover:bg-cyber-neon-green/90 text-cyber-bg font-semibold py-3 px-6 shadow-lg shadow-cyber-neon-green/40 transition-all disabled:opacity-50 disabled:cursor-not-allowed"
          >
            {getSupplyButtonText()}
          </button>
        </div>
      </div>

      <div className="bg-cyber-surface-light p-4 border border-cyber-border-glow/30">
        <h4 className="font-medium text-cyber-text-primary mb-4">Borrow USDC</h4>
        <div className="bg-cyber-surface-dark p-1 flex text-sm font-medium mb-4 border border-cyber-border-glow/30">
          <button
            onClick={() => { setBorrowMode('borrow'); setBorrowAmount('') }}
            className={`flex-1 py-2 px-4 transition-all ${
              borrowMode === 'borrow'
                ? 'bg-cyber-surface-light text-cyber-neon-green shadow-sm shadow-cyber-neon-green/10 border border-cyber-neon-green/50'
                : 'text-cyber-text-secondary hover:text-cyber-neon-green'
            }`}
          >
            Borrow
          </button>
          <button
            onClick={() => { setBorrowMode('repay'); setBorrowAmount('') }}
            className={`flex-1 py-2 px-4 transition-all ${
              borrowMode === 'repay'
                ? 'bg-cyber-surface-light text-cyber-neon-green shadow-sm shadow-cyber-neon-green/10 border border-cyber-neon-green/50'
                : 'text-cyber-text-secondary hover:text-cyber-neon-green'
            }`}
          >
            Repay
          </button>
        </div>
        <div className="space-y-4">
          <TokenInput
            value={borrowAmount}
            onChange={setBorrowAmount}
            token={{ symbol: 'USDC', decimals: 6 }}
            balance={borrowMode === 'borrow' ? availableToBorrow : borrowedAmount}
            balanceLabel={borrowMode === 'borrow' ? 'Available:' : 'Owed:'}
          />
          {borrowMode === 'borrow' && !hasCollateral && (
            <p className="text-xs text-cyber-warning-text">
              Borrowing requires staked collateral from a leverage position (splDXY-BEAR or splDXY-BULL)
            </p>
          )}
          {borrowMode === 'borrow' && hasCollateral && (
            <p className="text-xs text-cyber-text-secondary">
              Borrow against your staked collateral from leverage positions
            </p>
          )}
          <button
            onClick={handleBorrow}
            disabled={isBorrowDisabled}
            className="w-full bg-cyber-neon-green hover:bg-cyber-neon-green/90 text-cyber-bg font-semibold py-3 px-6 shadow-lg shadow-cyber-neon-green/40 transition-all disabled:opacity-50 disabled:cursor-not-allowed"
          >
            {getBorrowButtonText()}
          </button>
        </div>
      </div>
    </div>
  )
}
