import { describe, expect, it } from 'vitest'
import { formatNativeWei } from './protocolWallets'

describe('operational wallet native balance formatting', () => {
  it('does not render a positive sub-micro-ETH balance as zero', () => {
    expect(formatNativeWei('1')).toBe('<0.000001 ETH')
    expect(formatNativeWei('999999999999')).toBe('<0.000001 ETH')
    expect(formatNativeWei('1000000000000')).toBe('0.000001 ETH')
  })

  it('keeps exact zero and unavailable values distinct', () => {
    expect(formatNativeWei('0')).toBe('0.000000 ETH')
    expect(formatNativeWei(null)).toBe('Unavailable')
  })
})
