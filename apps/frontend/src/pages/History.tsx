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
    <div className="grid grid-cols-[minmax(0,1fr)_auto] items-center gap-4 px-4 py-4 sm:px-6 lg:grid-cols-[minmax(0,1fr)_auto_auto]">
      <div className="flex min-w-0 items-center gap-3 sm:gap-4">
        <div className="h-10 w-10 shrink-0 rounded bg-surface-muted" />
        <div className="min-w-0">
          <div className="h-4 w-24 bg-surface-muted rounded mb-2" />
          <div className="h-3 w-16 bg-surface-muted rounded" />
        </div>
      </div>
      <div className="min-w-0 text-right">
        <div className="mb-2 ml-auto h-4 w-20 max-w-full rounded bg-surface-muted" />
        <div className="ml-auto h-3 w-28 max-w-full rounded bg-surface-muted" />
      </div>
      <div className="col-span-2 h-6 w-16 justify-self-end rounded-full bg-surface-muted lg:col-span-1" />
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
    <div className="min-w-0 space-y-6 sm:space-y-10">
      <div className="mb-6 sm:mb-8">
        <h1 className="mb-1 text-2xl font-semibold text-content-primary sm:text-3xl">Transaction History</h1>
        <p className="text-sm font-light text-content-secondary sm:text-base">View your past transactions</p>
      </div>

      {/* Filters */}
      <div className="grid grid-cols-2 gap-2 sm:flex sm:flex-wrap">
        {filterOptions.map((option) => (
          <button
            key={option.id}
            onClick={() => { setFilter(option.id); }}
            className={`
              flex min-h-11 items-center justify-center gap-2 px-3 py-2 text-sm font-medium transition-colors hover:underline hover:underline-offset-4 sm:px-4
              ${filter === option.id
                ? 'bg-positive/20 text-positive border border-positive/50'
                : 'bg-surface-panel text-content-secondary border border-brand-border/30 hover:bg-[#3B212D] hover:text-[#FFAB96] hover:border-[#FFAB96]/50'
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
          <div className="bg-surface-panel border border-brand-border/30 overflow-hidden">
            <div className="divide-y divide-brand-border/20">
              {[1, 2, 3, 4, 5].map((i) => (
                <TransactionSkeleton key={i} />
              ))}
            </div>
          </div>
        ) : filteredTransactions.length > 0 ? (
          <>
            <div className="bg-surface-panel border border-brand-border/30 overflow-hidden">
              <div className="divide-y divide-brand-border/20">
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
                  className="min-h-11 border border-brand-border/30 bg-surface-panel px-6 py-2 text-content-secondary transition-colors hover:border-[#FFAB96]/50 hover:bg-[#3B212D] hover:text-[#FFAB96] hover:underline hover:underline-offset-4 disabled:cursor-not-allowed disabled:opacity-50 disabled:hover:no-underline"
                >
                  {isFetchingNextPage ? 'Loading...' : 'Load more'}
                </button>
              </div>
            )}
          </>
        ) : (
          <div className="border border-brand-border/30 bg-surface-panel p-6 text-center sm:p-12">
            <span className="material-symbols-outlined text-4xl text-content-secondary mb-4 block">search_off</span>
            <p className="text-content-secondary">No transactions found</p>
            <p className="text-content-secondary/60 text-sm mt-2">
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
