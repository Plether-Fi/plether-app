import { describe, it, expect } from 'vitest'
import { toBigInt, getAmountDisplay, transformTransaction, mapApiTypeToLocal } from '../history'
import type { Transaction } from '../../api/types'

function makeTx(overrides: Partial<Transaction> & Pick<Transaction, 'type' | 'data'>): Transaction {
  return {
    id: '1',
    hash: '0xabc',
    timestamp: 1700000000,
    blockNumber: 100,
    status: 'success',
    ...overrides,
  }
}

describe('toBigInt', () => {
  it('passes through bigint values', () => {
    expect(toBigInt(42n)).toBe(42n)
  })

  it('converts string integers', () => {
    expect(toBigInt('1000000')).toBe(1000000n)
  })

  it('converts numbers by truncating decimals', () => {
    expect(toBigInt(3.9)).toBe(3n)
    expect(toBigInt(0)).toBe(0n)
  })

  it('returns 0n for empty string', () => {
    expect(toBigInt('')).toBe(0n)
  })

  it('returns 0n for null/undefined', () => {
    expect(toBigInt(null)).toBe(0n)
    expect(toBigInt(undefined)).toBe(0n)
  })
})

describe('getAmountDisplay', () => {
  describe('mint/burn — pair count in 18 decimals', () => {
    it('mint uses amount as Pairs', () => {
      const result = getAmountDisplay(makeTx({
        type: 'mint',
        data: { amount: '5000000000000000000' } as never,
      }))
      expect(result).toEqual({ amount: 5000000000000000000n, tokenSymbol: 'Pairs' })
    })

    it('burn uses amount as Pairs', () => {
      const result = getAmountDisplay(makeTx({
        type: 'burn',
        data: { amount: '3000000000000000000' } as never,
      }))
      expect(result).toEqual({ amount: 3000000000000000000n, tokenSymbol: 'Pairs' })
    })
  })

  describe('zap_buy — tokensOut in 18 decimals', () => {
    it('uses tokensOut field', () => {
      const result = getAmountDisplay(makeTx({
        type: 'zap_buy',
        side: 'bull',
        data: { usdcIn: '10000000', tokensOut: '2000000000000000000', maxSlippageBps: '100', actualSwapOut: '1900000000000000000' } as never,
      }))
      expect(result).toEqual({ amount: 2000000000000000000n, tokenSymbol: 'plDXY-BULL' })
    })
  })

  describe('zap_sell — amountOut in USDC 6 decimals', () => {
    it('uses amountOut field', () => {
      const result = getAmountDisplay(makeTx({
        type: 'zap_sell',
        side: 'bull',
        data: { amountIn: '1000000000000000000', amountOut: '5000000' } as never,
      }))
      expect(result).toEqual({ amount: 5000000n, tokenSymbol: 'USDC' })
    })
  })

  describe('swap — Curve pool USDC(0)/BEAR(1)', () => {
    it('buy BEAR (soldId=0): shows BEAR amount received', () => {
      const result = getAmountDisplay(makeTx({
        type: 'swap',
        data: { soldId: '0', tokensSold: '5000000', boughtId: '1', tokensBought: '4000000000000000000', fee: '10', packedPriceScale: '1' } as never,
      }))
      expect(result).toEqual({ amount: 4000000000000000000n, tokenSymbol: 'plDXY-BEAR' })
    })

    it('sell BEAR (soldId=1): shows USDC amount received', () => {
      const result = getAmountDisplay(makeTx({
        type: 'swap',
        data: { soldId: '1', tokensSold: '3000000000000000000', boughtId: '0', tokensBought: '4000000', fee: '10', packedPriceScale: '1' } as never,
      }))
      expect(result).toEqual({ amount: 4000000n, tokenSymbol: 'USDC' })
    })
  })

  describe('stake/unstake — assets in 18 decimals', () => {
    it('stake bear', () => {
      const result = getAmountDisplay(makeTx({
        type: 'stake',
        side: 'bear',
        data: { assets: '1000000000000000000', shares: '900000000000000000' } as never,
      }))
      expect(result).toEqual({ amount: 1000000000000000000n, tokenSymbol: 'plDXY-BEAR' })
    })

    it('unstake bull', () => {
      const result = getAmountDisplay(makeTx({
        type: 'unstake',
        side: 'bull',
        data: { assets: '2000000000000000000', shares: '1800000000000000000' } as never,
      }))
      expect(result).toEqual({ amount: 2000000000000000000n, tokenSymbol: 'plDXY-BULL' })
    })
  })

  describe('leverage_open — principal in USDC 6 decimals', () => {
    it('uses principal field', () => {
      const result = getAmountDisplay(makeTx({
        type: 'leverage_open',
        side: 'bear',
        data: { principal: '10000000', leverage: '3', loanAmount: '20000000', tokensReceived: '30', debtIncurred: '20', maxSlippageBps: '100' } as never,
      }))
      expect(result).toEqual({ amount: 10000000n, tokenSymbol: 'USDC' })
    })
  })

  describe('leverage_close — usdcReturned in USDC 6 decimals', () => {
    it('uses usdcReturned field', () => {
      const result = getAmountDisplay(makeTx({
        type: 'leverage_close',
        side: 'bear',
        data: { debtRepaid: '50', collateralWithdrawn: '100', usdcReturned: '3000000', maxSlippageBps: '100' } as never,
      }))
      expect(result).toEqual({ amount: 3000000n, tokenSymbol: 'USDC' })
    })
  })

  describe('lending — assets in USDC 6 decimals', () => {
    it.each([
      'lending_supply',
      'lending_withdraw',
      'lending_borrow',
      'lending_repay',
    ] as const)('%s uses assets field', (type) => {
      const result = getAmountDisplay(makeTx({
        type,
        side: 'bear',
        data: { assets: '8000000', shares: '7000000' } as never,
      }))
      expect(result).toEqual({ amount: 8000000n, tokenSymbol: 'USDC' })
    })
  })

  it('returns 0n USDC for unknown type', () => {
    const result = getAmountDisplay(makeTx({
      type: 'unknown_type' as never,
      data: { foo: '123' } as never,
    }))
    expect(result).toEqual({ amount: 0n, tokenSymbol: 'USDC' })
  })
})

describe('mapApiTypeToLocal', () => {
  it('maps mint/burn directly', () => {
    expect(mapApiTypeToLocal(makeTx({ type: 'mint', data: {} as never }))).toBe('mint')
    expect(mapApiTypeToLocal(makeTx({ type: 'burn', data: {} as never }))).toBe('burn')
  })

  it('maps swap buy BEAR (soldId=0 means sold USDC)', () => {
    expect(mapApiTypeToLocal(makeTx({ type: 'swap', data: { soldId: '0', boughtId: '1' } as never }))).toBe('swap_buy_bear')
  })

  it('maps swap sell BEAR (soldId=1 means sold BEAR)', () => {
    expect(mapApiTypeToLocal(makeTx({ type: 'swap', data: { soldId: '1', boughtId: '0' } as never }))).toBe('swap_sell_bear')
  })

  it('maps zap_buy with side', () => {
    expect(mapApiTypeToLocal(makeTx({ type: 'zap_buy', side: 'bear', data: {} as never }))).toBe('swap_buy_bear')
    expect(mapApiTypeToLocal(makeTx({ type: 'zap_buy', side: 'bull', data: {} as never }))).toBe('swap_buy_bull')
  })

  it('maps zap_sell with side', () => {
    expect(mapApiTypeToLocal(makeTx({ type: 'zap_sell', side: 'bear', data: {} as never }))).toBe('swap_sell_bear')
    expect(mapApiTypeToLocal(makeTx({ type: 'zap_sell', side: 'bull', data: {} as never }))).toBe('swap_sell_bull')
  })

  it('maps stake/unstake with side', () => {
    expect(mapApiTypeToLocal(makeTx({ type: 'stake', side: 'bear', data: {} as never }))).toBe('stake_bear')
    expect(mapApiTypeToLocal(makeTx({ type: 'unstake', side: 'bull', data: {} as never }))).toBe('unstake_bull')
  })

  it('maps leverage types', () => {
    expect(mapApiTypeToLocal(makeTx({ type: 'leverage_open', data: {} as never }))).toBe('leverage_open')
    expect(mapApiTypeToLocal(makeTx({ type: 'leverage_close', data: {} as never }))).toBe('leverage_close')
    expect(mapApiTypeToLocal(makeTx({ type: 'collateral_add', data: {} as never }))).toBe('leverage_adjust')
    expect(mapApiTypeToLocal(makeTx({ type: 'collateral_remove', data: {} as never }))).toBe('leverage_adjust')
  })

  it('maps lending_* types from backend', () => {
    expect(mapApiTypeToLocal(makeTx({ type: 'lending_supply', data: {} as never }))).toBe('morpho_supply')
    expect(mapApiTypeToLocal(makeTx({ type: 'lending_withdraw', data: {} as never }))).toBe('morpho_withdraw')
    expect(mapApiTypeToLocal(makeTx({ type: 'lending_borrow', data: {} as never }))).toBe('morpho_borrow')
    expect(mapApiTypeToLocal(makeTx({ type: 'lending_repay', data: {} as never }))).toBe('morpho_repay')
  })

  it('also maps short lending types for compatibility', () => {
    expect(mapApiTypeToLocal(makeTx({ type: 'supply', data: {} as never }))).toBe('morpho_supply')
    expect(mapApiTypeToLocal(makeTx({ type: 'withdraw', data: {} as never }))).toBe('morpho_withdraw')
    expect(mapApiTypeToLocal(makeTx({ type: 'borrow', data: {} as never }))).toBe('morpho_borrow')
    expect(mapApiTypeToLocal(makeTx({ type: 'repay', data: {} as never }))).toBe('morpho_repay')
  })
})

describe('transformTransaction', () => {
  it('mint: 5 pairs shows as 5000000000000000000n Pairs', () => {
    const result = transformTransaction(makeTx({
      id: 'tx-42',
      hash: '0xdeadbeef',
      type: 'mint',
      timestamp: 1700000000,
      status: 'success',
      data: { amount: '5000000000000000000' } as never,
    }))
    expect(result).toEqual({
      id: 'tx-42',
      hash: '0xdeadbeef',
      type: 'mint',
      timestamp: 1700000000,
      amount: 5000000000000000000n,
      tokenSymbol: 'Pairs',
      status: 'success',
    })
  })

  it('zap_buy maps to swap_buy_bull with correct amount', () => {
    const result = transformTransaction(makeTx({
      type: 'zap_buy',
      side: 'bull',
      data: { usdcIn: '10000000', tokensOut: '2000000000000000000', maxSlippageBps: '100', actualSwapOut: '1900000000000000000' } as never,
    }))
    expect(result.type).toBe('swap_buy_bull')
    expect(result.tokenSymbol).toBe('plDXY-BULL')
    expect(result.amount).toBe(2000000000000000000n)
  })

  it('lending_supply maps to morpho_supply', () => {
    const result = transformTransaction(makeTx({
      type: 'lending_supply',
      side: 'bear',
      data: { assets: '5000000', shares: '4500000' } as never,
    }))
    expect(result.type).toBe('morpho_supply')
    expect(result.tokenSymbol).toBe('USDC')
    expect(result.amount).toBe(5000000n)
  })

  it('preserves failed status', () => {
    const result = transformTransaction(makeTx({
      type: 'burn',
      status: 'failed',
      data: { amount: '1000000000000000000' } as never,
    }))
    expect(result.status).toBe('failed')
  })
})
