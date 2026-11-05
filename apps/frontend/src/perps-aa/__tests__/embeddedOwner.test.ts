import { describe, expect, it } from 'vitest'
import { assertEmbeddedPerpsOwner, UnsupportedEmbeddedOwnerError } from '../embeddedOwner'

const OWNER = '0x1111111111111111111111111111111111111111'
const OTHER = '0x2222222222222222222222222222222222222222'

describe('embedded Trading Account owners', () => {
  it('accepts the connected embedded EOA', () => {
    expect(() => assertEmbeddedPerpsOwner({
      address: OWNER, embeddedWalletInfo: { accountType: 'eoa' },
    }, OWNER)).not.toThrow()
  })

  it('rejects a remembered Reown smart account before deriving a Trading Account', () => {
    expect(() => assertEmbeddedPerpsOwner({
      address: OWNER, embeddedWalletInfo: { accountType: 'smartAccount' },
    }, OWNER)).toThrow(UnsupportedEmbeddedOwnerError)
  })

  it('does not accept an EOA snapshot belonging to a different selected owner', () => {
    expect(() => assertEmbeddedPerpsOwner({
      address: OTHER, embeddedWalletInfo: { accountType: 'eoa' },
    }, OWNER)).toThrow('still reconnecting')
  })

  it.each([undefined, { address: OWNER }, { address: OWNER, embeddedWalletInfo: {} }])(
    'waits for account metadata instead of assuming a standard signature', account => {
      expect(() => assertEmbeddedPerpsOwner(account, OWNER)).toThrow()
    },
  )
})
