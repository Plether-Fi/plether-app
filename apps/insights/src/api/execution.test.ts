import { describe, expect, it } from 'vitest'
import { formatEthWei, formatRateBps, isLowEthBalance } from './execution'

describe('execution formatters', () => {
  it('formats wei balances cleanly to 4 decimal ETH places without floating-point drift', () => {
    expect(formatEthWei('1250000000000000000')).toBe('1.2500 ETH')
    expect(formatEthWei('5000000000000000')).toBe('0.0050 ETH')
    expect(formatEthWei('0')).toBe('0.0000 ETH')
    expect(formatEthWei(null)).toBe('Unavailable')
    expect(formatEthWei('invalid')).toBe('Unavailable')
  })

  it('flags balances below 0.01 ETH as low without flagging null', () => {
    expect(isLowEthBalance('9999999999999999')).toBe(true)
    expect(isLowEthBalance('10000000000000000')).toBe(false)
    expect(isLowEthBalance(null)).toBe(false)
  })

  it('formats basis-point rates cleanly', () => {
    expect(formatRateBps(9850)).toBe('98.50%')
    expect(formatRateBps(0)).toBe('0.00%')
    expect(formatRateBps(null)).toBe('—')
  })
})
