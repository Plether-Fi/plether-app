import { beforeEach, describe, expect, it, vi } from 'vitest'
import { loadWalletSelection, saveWalletSelection, readWalletSelection, selectedWallet, writeWalletSelection } from './selection'

const first = { address: '0x1111111111111111111111111111111111111111' }
const second = { address: '0x2222222222222222222222222222222222222222' }

beforeEach(() => { localStorage.clear(); sessionStorage.clear() })

describe('owner wallet selection', () => {
  it('restores the chosen owner after the browser discards tab storage', () => {
    sessionStorage.setItem('owner', second.address)
    expect(loadWalletSelection('owner')).toBe(second.address)
    sessionStorage.clear()
    expect(loadWalletSelection('owner')).toBe(second.address)
    expect(selectedWallet([first, second], loadWalletSelection('owner'))).toBe(second)
  })

  it('keeps explicit disconnect after tab storage is discarded or contains an older owner', () => {
    saveWalletSelection('owner', first.address)
    saveWalletSelection('owner', null)
    sessionStorage.clear()
    expect(loadWalletSelection('owner')).toBeNull()
    sessionStorage.setItem('owner', first.address)
    expect(loadWalletSelection('owner')).toBeNull()
  })

  it('keeps tab restoration working when local storage access is blocked', () => {
    const blocked = vi.spyOn(window, 'localStorage', 'get').mockImplementation(() => { throw new Error('Blocked') })
    try {
      saveWalletSelection('owner', first.address)
      expect(loadWalletSelection('owner')).toBe(first.address)
    } finally { blocked.mockRestore() }
  })

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
