import { describe, expect, it } from 'vitest'
import { selectedKernelTestOwner } from '../kernelTestOptIn'

const external = '0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
const privy = '0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'

const optIn = new URL('https://app.sepolia.plether.com/?kernel-test=1')

describe('the Sepolia homepage Kernel test', () => {
  it('admits only the two configured owners, independent of address case', () => {
    const owners = `${external},${privy}`
    expect(selectedKernelTestOwner(external, owners, optIn)).toBe(true)
    expect(selectedKernelTestOwner(privy.toUpperCase(), owners, optIn)).toBe(true)
    expect(selectedKernelTestOwner('0xcccccccccccccccccccccccccccccccccccccccc', owners, optIn)).toBe(false)
    expect(selectedKernelTestOwner(undefined, owners, optIn)).toBe(false)
  })

  it('keeps the normal account flow without an explicit opt-in on the Sepolia homepage', () => {
    const owners = `${external},${privy}`
    for (const href of [
      'https://app.sepolia.plether.com/',
      'https://app.sepolia.plether.com/?kernel-test=0',
      'https://app.sepolia.plether.com/?kernel-test=0&kernel-test=1',
      'https://app.sepolia.plether.com/vaults?kernel-test=1',
      'https://app.plether.com/?kernel-test=1',
    ]) expect(selectedKernelTestOwner(external, owners, new URL(href))).toBe(false)
  })

  it('fails closed on a missing, incomplete or duplicate configuration', () => {
    for (const invalid of [undefined, external, `${external},invalid`, `${external},${external.toUpperCase()}`]) {
      expect(selectedKernelTestOwner(external, invalid, optIn)).toBe(false)
    }
  })
})
