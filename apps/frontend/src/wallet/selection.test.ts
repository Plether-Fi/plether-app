import { describe, expect, it } from 'vitest'
import { readWalletSelection, selectedWallet, writeWalletSelection } from './selection'

const first = { address: '0x1111111111111111111111111111111111111111' }
const second = { address: '0x2222222222222222222222222222222222222222' }

describe('owner wallet selection', () => {
  it('keeps the same owner when connected wallets reorder', () => {
    expect(selectedWallet([first, second], first.address)).toBe(first)
    expect(selectedWallet([second, first], first.address)).toBe(first)
  })

  it('requires a choice for a restored session with multiple wallets', () => {
    expect(selectedWallet([first, second], undefined)).toBeUndefined()
    expect(selectedWallet([first], undefined)).toBe(first)
  })

  it('does not replace an unavailable owner or an explicitly disconnected wallet', () => {
    expect(selectedWallet([second], first.address)).toBeUndefined()
    expect(selectedWallet([first], null)).toBeUndefined()
  })

  it('persists explicit disconnect without inheriting another wallet on reload', () => {
    writeWalletSelection(localStorage, 'owner', first.address)
    expect(readWalletSelection(localStorage, 'owner')).toBe(first.address)
    writeWalletSelection(localStorage, 'owner', null)
    expect(readWalletSelection(localStorage, 'owner')).toBeNull()
  })

  it('rejects invalid stored values and tolerates blocked browser storage', () => {
    localStorage.setItem('owner', 'invalid')
    expect(readWalletSelection(localStorage, 'owner')).toBeUndefined()
    const blocked = {
      getItem: () => { throw new Error('Blocked') },
      setItem: () => { throw new Error('Blocked') },
    }
    expect(readWalletSelection(blocked, 'owner')).toBeUndefined()
    expect(() => writeWalletSelection(blocked, 'owner', first.address)).not.toThrow()
  })
})
