import { describe, expect, it } from 'vitest'
import { kernelTestOwnerAllowed } from '../kernelTestMount'

const external = '0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
const privy = '0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'

describe('the shared-host Kernel test', () => {
  it('admits only the two configured owners, independent of address case', () => {
    const owners = `${external},${privy}`
    expect(kernelTestOwnerAllowed(external, owners)).toBe(true)
    expect(kernelTestOwnerAllowed(privy.toUpperCase(), owners)).toBe(true)
    expect(kernelTestOwnerAllowed('0xcccccccccccccccccccccccccccccccccccccccc', owners)).toBe(false)
  })

  it('fails closed on a missing, incomplete or duplicate configuration', () => {
    for (const invalid of [undefined, external, `${external},invalid`, `${external},${external.toUpperCase()}`]) {
      expect(kernelTestOwnerAllowed(external, invalid)).toBe(false)
    }
  })
})
