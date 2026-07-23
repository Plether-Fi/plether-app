import { useChainId } from 'wagmi'
import { formatAmount, formatDate } from '../utils/formatters'
import { getExplorerTxUrl } from '../utils/explorer'
import { TokenLabel } from './ui/TokenLabel'
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
  leverage_open_bear: 'Open BEAR Leverage',
  leverage_open_bull: 'Open BULL Leverage',
  leverage_close_bear: 'Close BEAR Leverage',
  leverage_close_bull: 'Close BULL Leverage',
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
  leverage_open_bear: 'trending_up',
  leverage_open_bull: 'trending_up',
  leverage_close_bear: 'trending_down',
  leverage_close_bull: 'trending_down',
  leverage_adjust: 'tune',
  morpho_supply: 'savings',
  morpho_withdraw: 'output',
  morpho_borrow: 'request_quote',
  morpho_repay: 'paid',
}

function getIconColor(type: TransactionType): string {
  if (type.includes('bear')) return 'text-brand-orange'
  if (type.includes('bull')) return 'text-positive'
  if (type === 'mint') return 'text-positive'
  if (type === 'burn') return 'text-warning'
  if (type.includes('morpho')) return 'text-brand-orange'
  return 'text-content-primary'
}

function getIconBg(type: TransactionType): string {
  if (type.includes('bear')) return 'bg-brand-orange/20'
  if (type.includes('bull')) return 'bg-positive/20'
  if (type === 'mint') return 'bg-positive/20'
  if (type === 'burn') return 'bg-warning-bg'
  if (type.includes('leverage')) return 'bg-brand-peach/20'
  if (type.includes('morpho')) return 'bg-brand-orange/20'
  return 'bg-surface-muted'
}

function formatTokenLabel(tokenSymbol: string, amount: bigint): string {
  if (tokenSymbol === 'Pairs' && amount === 1000000000000000000n) return 'Pair'
  return tokenSymbol
}

export interface TransactionRowProps {
  transaction: HistoricalTransaction
}

export function TransactionRow({ transaction }: TransactionRowProps) {
  const chainId = useChainId()
  const truncatedHash = `${transaction.hash.slice(0, 10)}...${transaction.hash.slice(-8)}`
  const decimals = transaction.tokenSymbol === 'USDC' ? 6 : 18

  return (
    <div className="grid grid-cols-[minmax(0,1fr)_auto] items-center gap-x-3 gap-y-4 px-4 py-4 transition-colors hover:bg-[#3B212D] sm:gap-x-4 sm:px-6 xl:grid-cols-[minmax(0,1fr)_7rem_7rem_14rem_5rem] xl:gap-y-0">
      <div className="col-span-2 flex min-w-0 items-center gap-3 sm:gap-4 xl:col-span-1">
        <div className={`h-10 w-10 shrink-0 ${getIconBg(transaction.type)} flex items-center justify-center`}>
          <span className={`material-symbols-outlined ${getIconColor(transaction.type)}`}>
            {typeIcons[transaction.type]}
          </span>
        </div>
        <div className="min-w-0">
          <p className="break-words font-semibold text-content-primary">
            {typeLabels[transaction.type]}
          </p>
          <p className="text-sm text-content-secondary">
            {formatDate(transaction.timestamp)}
          </p>
        </div>
      </div>

      <div className="min-w-0 tabular-nums xl:text-right">
        <p className="break-all font-medium text-content-primary">
          {formatAmount(transaction.amount, decimals)}
        </p>
        {transaction.secondaryAmount != null && transaction.secondarySymbol && (
          <p className="break-all text-xs text-content-secondary">
            {formatAmount(transaction.secondaryAmount, transaction.secondarySymbol === 'USDC' ? 6 : 18)}
          </p>
        )}
      </div>

      <div className="min-w-0 justify-self-end space-y-1 text-right xl:justify-self-stretch xl:text-left">
        <TokenLabel token={formatTokenLabel(transaction.tokenSymbol, transaction.amount)} />
        {transaction.secondaryAmount != null && transaction.secondarySymbol && (
          <div>
            <TokenLabel token={transaction.secondarySymbol} />
          </div>
        )}
      </div>

      <a
        href={getExplorerTxUrl(chainId, transaction.hash)}
        target="_blank"
        rel="noopener noreferrer"
        className="inline-flex min-h-11 min-w-0 max-w-full items-center gap-1 break-all text-sm text-brand-peach hover:text-[#FFAB96]/80 xl:min-h-0"
      >
        {truncatedHash}
        <span className="material-symbols-outlined shrink-0 text-sm">open_in_new</span>
      </a>

      <div className={`
        justify-self-end whitespace-nowrap rounded-full px-3 py-1 text-center text-xs font-semibold xl:justify-self-stretch
        ${transaction.status === 'success'
          ? 'bg-positive/20 text-positive border border-positive/30'
          : 'bg-brand-orange/20 text-brand-orange border border-brand-orange/30'
        }
      `}>
        {transaction.status === 'success' ? 'Success' : 'Failed'}
      </div>
    </div>
  )
}
