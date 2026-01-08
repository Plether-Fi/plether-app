import { useState } from 'react'
import { TokenInput } from './TokenInput'
import { formatUsd, formatPercent } from '../utils/formatters'

type SupplyMode = 'supply' | 'withdraw'
type BorrowMode = 'borrow' | 'repay'

export interface YieldCardProps {
  suppliedAmount: bigint
  borrowedAmount: bigint
  availableToBorrow: bigint
  supplyApy: number
  borrowApy: number
  usdcBalance: bigint
  suppliedBalance: bigint
}

export function YieldCard({
  suppliedAmount,
  borrowedAmount,
  availableToBorrow,
  supplyApy,
  borrowApy,
  usdcBalance,
  suppliedBalance,
}: YieldCardProps) {
  const [supplyMode, setSupplyMode] = useState<SupplyMode>('supply')
  const [borrowMode, setBorrowMode] = useState<BorrowMode>('borrow')
  const [supplyAmount, setSupplyAmount] = useState('')
  const [borrowAmount, setBorrowAmount] = useState('')

  const handleSupply = async () => {
    console.log(`${supplyMode}:`, { amount: supplyAmount })
  }

  const handleBorrow = async () => {
    console.log(`${borrowMode}:`, { amount: borrowAmount })
  }

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

  return (
    <div className="max-w-xl mx-auto space-y-6">
      <div className="grid grid-cols-3 gap-4">
        <button
          onClick={handleSuppliedClick}
          className="bg-cyber-surface-light p-3 border border-cyber-border-glow/30 text-left transition-all hover:border-cyber-neon-green/50 hover:shadow-md hover:shadow-cyber-neon-green/10"
        >
          <p className="text-xs text-cyber-text-secondary">Supplied</p>
          <p className="text-lg font-bold text-cyber-text-primary">{formatUsd(suppliedAmount)}</p>
          <p className="text-xs text-cyber-neon-green">+{formatPercent(supplyApy)} APY</p>
        </button>
        <button
          onClick={handleBorrowedClick}
          className="bg-cyber-surface-light p-3 border border-cyber-border-glow/30 text-left transition-all hover:border-cyber-warning-text/50 hover:shadow-md hover:shadow-cyber-warning-text/10"
        >
          <p className="text-xs text-cyber-text-secondary">Borrowed</p>
          <p className="text-lg font-bold text-cyber-text-primary">{formatUsd(borrowedAmount)}</p>
          <p className="text-xs text-cyber-warning-text">-{formatPercent(borrowApy)} APY</p>
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
        <div className="flex items-center justify-between mb-4">
          <h4 className="font-medium text-cyber-text-primary">Supply USDC</h4>
          <span className="text-sm text-cyber-neon-green">{formatPercent(supplyApy)} APY</span>
        </div>
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
            disabled={!supplyAmount || parseFloat(supplyAmount) <= 0}
            className="w-full bg-cyber-neon-green hover:bg-cyber-neon-green/90 text-cyber-bg font-semibold py-3 px-6 shadow-lg shadow-cyber-neon-green/40 transition-all disabled:opacity-50 disabled:cursor-not-allowed"
          >
            {supplyMode === 'supply' ? 'Supply' : 'Withdraw'} USDC
          </button>
        </div>
      </div>

      <div className="bg-cyber-surface-light p-4 border border-cyber-border-glow/30">
        <div className="flex items-center justify-between mb-4">
          <h4 className="font-medium text-cyber-text-primary">Borrow USDC</h4>
          <span className="text-sm text-cyber-warning-text">{formatPercent(borrowApy)} APY</span>
        </div>
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
          {borrowMode === 'borrow' && (
            <p className="text-xs text-cyber-text-secondary">
              Borrowing requires staked collateral (sDXY-BEAR or sDXY-BULL)
            </p>
          )}
          <button
            onClick={handleBorrow}
            disabled={!borrowAmount || parseFloat(borrowAmount) <= 0}
            className="w-full bg-cyber-neon-green hover:bg-cyber-neon-green/90 text-cyber-bg font-semibold py-3 px-6 shadow-lg shadow-cyber-neon-green/40 transition-all disabled:opacity-50 disabled:cursor-not-allowed"
          >
            {borrowMode === 'borrow' ? 'Borrow' : 'Repay'} USDC
          </button>
        </div>
      </div>
    </div>
  )
}
