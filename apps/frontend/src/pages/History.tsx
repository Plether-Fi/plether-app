import { useState, useMemo } from 'react'
import { useAccount } from 'wagmi'
import { TransactionRow } from '../components/TransactionRow'
import { ConnectWalletPrompt } from '../components/ConnectWalletPrompt'
import { useTransactionHistory } from '../api'
import { transformTransaction } from '../utils/history'

const filterOptions = [
  { id: 'all', label: 'All', icon: 'list' },
  { id: 'mint', label: 'Mint/Burn', icon: 'add_circle' },
  { id: 'swap', label: 'Swaps', icon: 'swap_horiz' },
  { id: 'stake', label: 'Staking', icon: 'paid' },
  { id: 'leverage', label: 'Leverage', icon: 'trending_up' },
  { id: 'morpho', label: 'Lending', icon: 'account_balance' },
]

function TransactionSkeleton() {
  return (
    <div className="flex items-center justify-between px-6 py-4 animate-pulse">
      <div className="flex items-center gap-4">
        <div className="w-10 h-10 bg-cyber-surface-light rounded" />
        <div>
          <div className="h-4 w-24 bg-cyber-surface-light rounded mb-2" />
          <div className="h-3 w-16 bg-cyber-surface-light rounded" />
        </div>
      </div>
      <div className="text-right">
        <div className="h-4 w-20 bg-cyber-surface-light rounded mb-2" />
        <div className="h-3 w-28 bg-cyber-surface-light rounded" />
      </div>
      <div className="h-6 w-16 bg-cyber-surface-light rounded-full" />
    </div>
  )
}

export function History() {
  const { isConnected, address } = useAccount()
  const [filter, setFilter] = useState('all')

  const { data, isLoading, hasNextPage, fetchNextPage, isFetchingNextPage } = useTransactionHistory(
    address,
  )

  const transactions = useMemo(() => {
    if (!data?.pages) return []
    return data.pages.flatMap(page => page.data.transactions.map(transformTransaction))
  }, [data])

  const filteredTransactions = useMemo(() => {
    return transactions.filter((tx) => {
      if (filter === 'all') return true
      if (filter === 'mint') return tx.type === 'mint' || tx.type === 'burn'
      if (filter === 'swap') return tx.type.startsWith('swap_')
      if (filter === 'stake') return tx.type.startsWith('stake_') || tx.type.startsWith('unstake_')
      if (filter === 'leverage') return tx.type.startsWith('leverage_')
      if (filter === 'morpho') return tx.type.startsWith('morpho_')
      return true
    })
  }, [transactions, filter])

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
            onClick={() => { setFilter(option.id); }}
            className={`
              flex items-center gap-2 px-4 py-2  text-sm font-medium transition-all cursor-pointer
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
        isLoading ? (
          <div className="bg-cyber-surface-dark border border-cyber-border-glow/30 shadow-lg shadow-cyber-border-glow/10 overflow-hidden">
            <div className="divide-y divide-cyber-border-glow/20">
              {[1, 2, 3, 4, 5].map((i) => (
                <TransactionSkeleton key={i} />
              ))}
            </div>
          </div>
        ) : filteredTransactions.length > 0 ? (
          <>
            <div className="bg-cyber-surface-dark border border-cyber-border-glow/30 shadow-lg shadow-cyber-border-glow/10 overflow-hidden">
              <div className="divide-y divide-cyber-border-glow/20">
                {filteredTransactions.map((tx) => (
                  <TransactionRow key={tx.id} transaction={tx} />
                ))}
              </div>
            </div>
            {hasNextPage && (
              <div className="flex justify-center">
                <button
                  onClick={() => { void fetchNextPage(); }}
                  disabled={isFetchingNextPage}
                  className="px-6 py-2 bg-cyber-surface-dark text-cyber-text-secondary border border-cyber-border-glow/30 hover:text-cyber-bright-blue hover:border-cyber-bright-blue/50 transition-all cursor-pointer disabled:opacity-50"
                >
                  {isFetchingNextPage ? 'Loading...' : 'Load more'}
                </button>
              </div>
            )}
          </>
        ) : (
          <div className="bg-cyber-surface-dark p-12 text-center border border-cyber-border-glow/30">
            <span className="material-symbols-outlined text-4xl text-cyber-text-secondary mb-4 block">search_off</span>
            <p className="text-cyber-text-secondary">No transactions found</p>
            <p className="text-cyber-text-secondary/60 text-sm mt-2">
              Your transaction history will appear here once indexed
            </p>
          </div>
        )
      ) : (
        <ConnectWalletPrompt description="Connect your wallet to view transaction history." />
      )}
    </div>
  )
}

export default History
