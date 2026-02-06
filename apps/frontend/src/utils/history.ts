import type { Transaction } from '../api/types'
import type { HistoricalTransaction, TransactionType as LocalTxType, TokenSymbol } from '../types'

const CURVE_USDC_INDEX = 0

export function mapApiTypeToLocal(tx: Transaction): LocalTxType {
  switch (tx.type) {
    case 'mint': return 'mint'
    case 'burn': return 'burn'
    case 'swap': {
      const data = tx.data as unknown as Record<string, unknown>
      const soldId = Number(data.soldId ?? -1)
      return soldId === CURVE_USDC_INDEX ? 'swap_buy_bear' : 'swap_sell_bear'
    }
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
    case 'supply':
    case 'lending_supply':
      return 'morpho_supply'
    case 'withdraw':
    case 'lending_withdraw':
      return 'morpho_withdraw'
    case 'borrow':
    case 'lending_borrow':
      return 'morpho_borrow'
    case 'repay':
    case 'lending_repay':
      return 'morpho_repay'
    default:
      return 'mint'
  }
}

export function toBigInt(val: unknown): bigint {
  if (typeof val === 'bigint') return val
  if (typeof val === 'number') return BigInt(Math.trunc(val))
  if (typeof val === 'string' && val !== '') return BigInt(val)
  return 0n
}

export function getAmountDisplay(tx: Transaction): { amount: bigint; tokenSymbol: TokenSymbol } {
  const data = tx.data as unknown as Record<string, unknown>
  const sideToken = (tx.side === 'bear' ? 'plDXY-BEAR' : 'plDXY-BULL') satisfies TokenSymbol

  switch (tx.type) {
    case 'mint':
    case 'burn':
      return { amount: toBigInt(data.amount), tokenSymbol: 'Pairs' }
    case 'zap_buy':
      return { amount: toBigInt(data.tokensOut), tokenSymbol: 'plDXY-BULL' }
    case 'zap_sell':
      return { amount: toBigInt(data.amountOut), tokenSymbol: 'USDC' }
    case 'swap': {
      const soldId = Number(data.soldId ?? -1)
      return soldId === CURVE_USDC_INDEX
        ? { amount: toBigInt(data.tokensBought), tokenSymbol: 'plDXY-BEAR' }
        : { amount: toBigInt(data.tokensBought), tokenSymbol: 'USDC' }
    }
    case 'stake':
    case 'unstake':
      return { amount: toBigInt(data.assets), tokenSymbol: sideToken }
    case 'leverage_open':
      return { amount: toBigInt(data.principal), tokenSymbol: 'USDC' }
    case 'leverage_close':
      return { amount: toBigInt(data.usdcReturned), tokenSymbol: 'USDC' }
    case 'collateral_add':
    case 'collateral_remove':
      return { amount: toBigInt(data.amount), tokenSymbol: sideToken }
    case 'supply':
    case 'withdraw':
    case 'borrow':
    case 'repay':
    case 'lending_supply':
    case 'lending_withdraw':
    case 'lending_borrow':
    case 'lending_repay':
      return { amount: toBigInt(data.assets), tokenSymbol: 'USDC' }
    default:
      return { amount: 0n, tokenSymbol: 'USDC' }
  }
}

export function transformTransaction(tx: Transaction): HistoricalTransaction {
  const { amount, tokenSymbol } = getAmountDisplay(tx)
  return {
    id: tx.id,
    hash: tx.hash,
    type: mapApiTypeToLocal(tx),
    timestamp: tx.timestamp,
    amount,
    tokenSymbol,
    status: tx.status,
  }
}
