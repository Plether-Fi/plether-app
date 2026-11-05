import { describe, expect, it } from 'vitest'
import { selectedWallet } from './selection'

const owner = { address: '0x1111111111111111111111111111111111111111' }
const other = { address: '0x2222222222222222222222222222222222222222' }

describe('registration owner selection', () => {
  it('selects the sole initial wallet', () => {
    expect(selectedWallet([owner], null)).toBe(owner)
  })

  it('requires a choice when multiple wallets are initially connected', () => {
    expect(selectedWallet([owner, other], null)).toBeUndefined()
  })

  it('preserves the owner when another wallet connects or the list reorders', () => {
    expect(selectedWallet([other, owner], owner.address)).toBe(owner)
  })

  it('does not silently replace an owner that disconnects', () => {
    expect(selectedWallet([other], owner.address)).toBeUndefined()
  })
})
