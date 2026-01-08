import { useState } from 'react'
import { useAccount } from 'wagmi'
import { formatAmount, formatDate } from '../utils/formatters'
import type { HistoricalTransaction, TransactionType } from '../types'

const mockTransactions: HistoricalTransaction[] = [
  {
    id: '1',
    hash: '0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef',
    type: 'swap_buy_bear',
    timestamp: Date.now() / 1000 - 3600,
    amount: 1000n * 10n ** 6n,
    tokenSymbol: 'USDC',
    status: 'success',
  },
  {
    id: '2',
    hash: '0xabcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890',
    type: 'stake_bear',
    timestamp: Date.now() / 1000 - 7200,
    amount: 500n * 10n ** 18n,
    tokenSymbol: 'DXY-BEAR',
    status: 'success',
  },
  {
    id: '3',
    hash: '0x567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234',
    type: 'leverage_open',
    timestamp: Date.now() / 1000 - 86400,
    amount: 2000n * 10n ** 6n,
    tokenSymbol: 'USDC',
    status: 'success',
  },
  {
    id: '4',
    hash: '0x890abcdef1234567890abcdef1234567890abcdef1234567890abcdef123456',
    type: 'mint',
    timestamp: Date.now() / 1000 - 172800,
    amount: 5000n * 10n ** 6n,
    tokenSymbol: 'USDC',
    status: 'success',
  },
]

const filterOptions = [
  { id: 'all', label: 'All', icon: 'list' },
  { id: 'mint', label: 'Mint/Burn', icon: 'add_circle' },
  { id: 'swap', label: 'Swaps', icon: 'swap_horiz' },
  { id: 'stake', label: 'Staking', icon: 'paid' },
  { id: 'leverage', label: 'Leverage', icon: 'trending_up' },
  { id: 'morpho', label: 'Lending', icon: 'account_balance' },
]

export function History() {
  const { isConnected } = useAccount()
  const [filter, setFilter] = useState('all')

  const transactions = mockTransactions

  const filteredTransactions = transactions.filter((tx) => {
    if (filter === 'all') return true
    if (filter === 'mint') return tx.type === 'mint' || tx.type === 'burn'
    if (filter === 'swap') return tx.type.startsWith('swap_')
    if (filter === 'stake') return tx.type.startsWith('stake_') || tx.type.startsWith('unstake_')
    if (filter === 'leverage') return tx.type.startsWith('leverage_')
    if (filter === 'morpho') return tx.type.startsWith('morpho_')
    return true
  })

  return (
    <div className="space-y-10">
      <div className="mb-8">
        <h1 className="text-3xl font-semibold text-cyber-text-primary mb-1">Transaction History</h1>
        <p className="text-cyber-text-secondary font-light">View your past transactions</p>
      </div>

      {/* Filters */}
      <div className="flex flex-wrap gap-2">
        {filterOptions.map((option) => (
          <button
            key={option.id}
            onClick={() => setFilter(option.id)}
            className={`
              flex items-center gap-2 px-4 py-2  text-sm font-medium transition-all
              ${filter === option.id
                ? 'bg-cyber-neon-green/20 text-cyber-neon-green border border-cyber-neon-green/50 shadow-sm shadow-cyber-neon-green/20'
                : 'bg-cyber-surface-dark text-cyber-text-secondary border border-cyber-border-glow/30 hover:text-cyber-bright-blue hover:border-cyber-bright-blue/50'
              }
            `}
          >
            <span className="material-symbols-outlined text-lg">{option.icon}</span>
            {option.label}
          </button>
        ))}
      </div>

      {/* Transaction list */}
      {isConnected ? (
        filteredTransactions.length > 0 ? (
          <div className="bg-cyber-surface-dark  border border-cyber-border-glow/30 shadow-lg shadow-cyber-border-glow/10 overflow-hidden">
            <div className="divide-y divide-cyber-border-glow/20">
              {filteredTransactions.map((tx) => (
                <TransactionRow key={tx.id} transaction={tx} />
              ))}
            </div>
          </div>
        ) : (
          <div className="bg-cyber-surface-dark  p-12 text-center border border-cyber-border-glow/30">
            <span className="material-symbols-outlined text-4xl text-cyber-text-secondary mb-4 block">search_off</span>
            <p className="text-cyber-text-secondary">No transactions found</p>
          </div>
        )
      ) : (
        <div className="bg-cyber-surface-dark  p-12 text-center border border-cyber-border-glow/30 shadow-lg">
          <div className="w-16 h-16 mx-auto mb-4 rounded-full bg-cyber-surface-light flex items-center justify-center">
            <span className="material-symbols-outlined text-3xl text-cyber-text-secondary">lock</span>
          </div>
          <h2 className="text-xl font-semibold text-cyber-text-primary mb-2">Connect Your Wallet</h2>
          <p className="text-cyber-text-secondary">Connect your wallet to view transaction history</p>
        </div>
      )}
    </div>
  )
}

interface TransactionRowProps {
  transaction: HistoricalTransaction
}

function TransactionRow({ transaction }: TransactionRowProps) {
  const typeLabels: Record<TransactionType, string> = {
    mint: 'Mint Pairs',
    burn: 'Burn Pairs',
    swap_buy_bear: 'Buy DXY-BEAR',
    swap_sell_bear: 'Sell DXY-BEAR',
    swap_buy_bull: 'Buy DXY-BULL',
    swap_sell_bull: 'Sell DXY-BULL',
    stake_bear: 'Stake DXY-BEAR',
    stake_bull: 'Stake DXY-BULL',
    unstake_bear: 'Unstake DXY-BEAR',
    unstake_bull: 'Unstake DXY-BULL',
    leverage_open: 'Open Leverage',
    leverage_close: 'Close Leverage',
    leverage_adjust: 'Adjust Leverage',
    morpho_supply: 'Supply to Morpho',
    morpho_withdraw: 'Withdraw from Morpho',
    morpho_borrow: 'Borrow from Morpho',
    morpho_repay: 'Repay Morpho',
  }

  const typeIcons: Record<TransactionType, string> = {
    mint: 'add_circle',
    burn: 'remove_circle',
    swap_buy_bear: 'shopping_cart',
    swap_sell_bear: 'sell',
    swap_buy_bull: 'shopping_cart',
    swap_sell_bull: 'sell',
    stake_bear: 'lock',
    stake_bull: 'lock',
    unstake_bear: 'lock_open',
    unstake_bull: 'lock_open',
    leverage_open: 'trending_up',
    leverage_close: 'trending_down',
    leverage_adjust: 'tune',
    morpho_supply: 'savings',
    morpho_withdraw: 'output',
    morpho_borrow: 'request_quote',
    morpho_repay: 'paid',
  }

  const getTypeColor = (type: TransactionType): string => {
    if (type.includes('bear')) return 'text-cyber-electric-fuchsia'
    if (type.includes('bull')) return 'text-cyber-neon-green'
    if (type === 'mint') return 'text-cyber-neon-green'
    if (type === 'burn') return 'text-cyber-warning-text'
    if (type.includes('leverage')) return 'text-cyber-bright-blue'
    if (type.includes('morpho')) return 'text-cyber-electric-fuchsia'
    return 'text-cyber-text-primary'
  }

  const getIconBg = (type: TransactionType): string => {
    if (type.includes('bear')) return 'bg-cyber-electric-fuchsia/20'
    if (type.includes('bull')) return 'bg-cyber-neon-green/20'
    if (type === 'mint') return 'bg-cyber-neon-green/20'
    if (type === 'burn') return 'bg-cyber-warning-bg'
    if (type.includes('leverage')) return 'bg-cyber-bright-blue/20'
    if (type.includes('morpho')) return 'bg-cyber-electric-fuchsia/20'
    return 'bg-cyber-surface-light'
  }

  const truncatedHash = `${transaction.hash.slice(0, 10)}...${transaction.hash.slice(-8)}`
  const decimals = transaction.tokenSymbol === 'USDC' ? 6 : 18

  return (
    <div className="flex items-center justify-between px-6 py-4 hover:bg-cyber-surface-light/50 transition-colors">
      <div className="flex items-center gap-4">
        <div className={`w-10 h-10  ${getIconBg(transaction.type)} flex items-center justify-center`}>
          <span className={`material-symbols-outlined ${getTypeColor(transaction.type)}`}>
            {typeIcons[transaction.type]}
          </span>
        </div>
        <div>
          <p className={`font-semibold ${getTypeColor(transaction.type)}`}>
            {typeLabels[transaction.type]}
          </p>
          <p className="text-sm text-cyber-text-secondary">
            {formatDate(transaction.timestamp)}
          </p>
        </div>
      </div>

      <div className="text-right">
        <p className="text-cyber-text-primary font-medium">
          {formatAmount(transaction.amount, decimals)} {transaction.tokenSymbol}
        </p>
        <a
          href={`https://etherscan.io/tx/${transaction.hash}`}
          target="_blank"
          rel="noopener noreferrer"
          className="text-sm text-cyber-bright-blue hover:text-cyber-bright-blue/80 inline-flex items-center gap-1"
        >
          {truncatedHash}
          <span className="material-symbols-outlined text-sm">open_in_new</span>
        </a>
      </div>

      <div className={`
        px-3 py-1 rounded-full text-xs font-semibold
        ${transaction.status === 'success'
          ? 'bg-cyber-neon-green/20 text-cyber-neon-green border border-cyber-neon-green/30'
          : 'bg-cyber-electric-fuchsia/20 text-cyber-electric-fuchsia border border-cyber-electric-fuchsia/30'
        }
      `}>
        {transaction.status === 'success' ? 'Success' : 'Failed'}
      </div>
    </div>
  )
}
