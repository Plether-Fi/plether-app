import { useChainId } from 'wagmi'
import { formatAmount, formatDate } from '../utils/formatters'
import { getExplorerTxUrl } from '../utils/explorer'
import type { HistoricalTransaction, TransactionType } from '../types'

const typeLabels: Record<TransactionType, string> = {
  mint: 'Mint Pairs',
  burn: 'Burn Pairs',
  swap_buy_bear: 'Buy plDXY-BEAR',
  swap_sell_bear: 'Sell plDXY-BEAR',
  swap_buy_bull: 'Buy plDXY-BULL',
  swap_sell_bull: 'Sell plDXY-BULL',
  stake_bear: 'Stake plDXY-BEAR',
  stake_bull: 'Stake plDXY-BULL',
  unstake_bear: 'Unstake plDXY-BEAR',
  unstake_bull: 'Unstake plDXY-BULL',
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

function getTypeColor(type: TransactionType): string {
  if (type.includes('bear')) return 'text-cyber-electric-fuchsia'
  if (type.includes('bull')) return 'text-cyber-neon-green'
  if (type === 'mint') return 'text-cyber-neon-green'
  if (type === 'burn') return 'text-cyber-warning-text'
  if (type.includes('leverage')) return 'text-cyber-bright-blue'
  if (type.includes('morpho')) return 'text-cyber-electric-fuchsia'
  return 'text-cyber-text-primary'
}

function getIconBg(type: TransactionType): string {
  if (type.includes('bear')) return 'bg-cyber-electric-fuchsia/20'
  if (type.includes('bull')) return 'bg-cyber-neon-green/20'
  if (type === 'mint') return 'bg-cyber-neon-green/20'
  if (type === 'burn') return 'bg-cyber-warning-bg'
  if (type.includes('leverage')) return 'bg-cyber-bright-blue/20'
  if (type.includes('morpho')) return 'bg-cyber-electric-fuchsia/20'
  return 'bg-cyber-surface-light'
}

export interface TransactionRowProps {
  transaction: HistoricalTransaction
}

export function TransactionRow({ transaction }: TransactionRowProps) {
  const chainId = useChainId()
  const truncatedHash = `${transaction.hash.slice(0, 10)}...${transaction.hash.slice(-8)}`
  const decimals = transaction.tokenSymbol === 'USDC' ? 6 : 18

  return (
    <div className="flex items-center justify-between px-6 py-4 hover:bg-cyber-surface-light/50 transition-colors">
      <div className="flex items-center gap-4">
        <div className={`w-10 h-10 ${getIconBg(transaction.type)} flex items-center justify-center`}>
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
          {formatAmount(transaction.amount, decimals)} {transaction.tokenSymbol === 'Pairs' && transaction.amount === 1000000000000000000n ? 'Pair' : transaction.tokenSymbol}
        </p>
        <a
          href={getExplorerTxUrl(chainId, transaction.hash)}
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
