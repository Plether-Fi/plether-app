import { useState, useMemo } from 'react'
import { useAccount } from 'wagmi'
import { TransactionRow } from '../components/TransactionRow'
import { ConnectWalletPrompt } from '../components/ConnectWalletPrompt'
import { useTransactionHistory } from '../api'
import type { HistoricalTransaction, TransactionType as LocalTxType, TokenSymbol } from '../types'
import type { Transaction, TransactionType as ApiTxType } from '../api/types'

const filterOptions = [
  { id: 'all', label: 'All', icon: 'list' },
  { id: 'mint', label: 'Mint/Burn', icon: 'add_circle' },
  { id: 'swap', label: 'Swaps', icon: 'swap_horiz' },
  { id: 'stake', label: 'Staking', icon: 'paid' },
  { id: 'leverage', label: 'Leverage', icon: 'trending_up' },
  { id: 'morpho', label: 'Lending', icon: 'account_balance' },
]

function mapApiTypeToLocal(tx: Transaction): LocalTxType {
  switch (tx.type) {
    case 'mint': return 'mint'
    case 'burn': return 'burn'
    case 'swap':
    case 'zap_buy':
      return tx.side === 'bear' ? 'swap_buy_bear' : 'swap_buy_bull'
    case 'zap_sell':
      return tx.side === 'bear' ? 'swap_sell_bear' : 'swap_sell_bull'
    case 'stake':
      return tx.side === 'bear' ? 'stake_bear' : 'stake_bull'
    case 'unstake':
      return tx.side === 'bear' ? 'unstake_bear' : 'unstake_bull'
    case 'leverage_open': return 'leverage_open'
    case 'leverage_close': return 'leverage_close'
    case 'collateral_add':
    case 'collateral_remove':
      return 'leverage_adjust'
    case 'supply': return 'morpho_supply'
    case 'withdraw': return 'morpho_withdraw'
    case 'borrow': return 'morpho_borrow'
    case 'repay': return 'morpho_repay'
    default:
      return 'mint'
  }
}

function getTokenSymbol(tx: Transaction): TokenSymbol {
  const localType = mapApiTypeToLocal(tx)
  if (localType === 'mint' || localType === 'burn') return 'USDC'
  if (localType.includes('bear')) return 'plDXY-BEAR'
  if (localType.includes('bull')) return 'plDXY-BULL'
  if (localType.includes('stake') || localType.includes('unstake')) {
    return tx.side === 'bear' ? 'plDXY-BEAR' : 'plDXY-BULL'
  }
  return 'USDC'
}

function getAmount(tx: Transaction): bigint {
  const data = tx.data as unknown as Record<string, unknown>
  if ('usdcAmount' in data) return BigInt(data.usdcAmount as string)
  if ('pairAmount' in data) return BigInt(data.pairAmount as string)
  if ('amountIn' in data) return BigInt(data.amountIn as string)
  if ('amountOut' in data) return BigInt(data.amountOut as string)
  if ('assets' in data) return BigInt(data.assets as string)
  if ('principal' in data) return BigInt(data.principal as string)
  if ('amount' in data) return BigInt(data.amount as string)
  if ('tokensSold' in data) return BigInt(data.tokensSold as string)
  return 0n
}

function transformTransaction(tx: Transaction): HistoricalTransaction {
  return {
    id: tx.id,
    hash: tx.hash,
    type: mapApiTypeToLocal(tx),
    timestamp: tx.timestamp,
    amount: getAmount(tx),
    tokenSymbol: getTokenSymbol(tx),
    status: tx.status,
  }
}

function filterApiType(filter: string): ApiTxType | undefined {
  switch (filter) {
    case 'mint': return 'mint'
    case 'swap': return 'swap'
    case 'stake': return 'stake'
    case 'leverage': return 'leverage_open'
    case 'morpho': return 'supply'
    default: return undefined
  }
}

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

  const apiType = filter !== 'all' ? filterApiType(filter) : undefined
  const { data, isLoading, hasNextPage, fetchNextPage, isFetchingNextPage } = useTransactionHistory(
    address,
    { type: apiType }
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
