import { describe, expect, it } from 'vitest'
import { arbitrumSepolia, mainnet, sepolia } from 'wagmi/chains'
import { getAddresses, SEPOLIA_ADDRESSES } from '../addresses'

describe('getAddresses', () => {
  it('returns Arbitrum Sepolia spot deployment addresses for chain 421614', () => {
    const addresses = getAddresses(arbitrumSepolia.id)

    expect(addresses.USDC).toBe('0xf1e1B188b87525C51ECe4bae8627ae621D769651')
    expect(addresses.SYNTHETIC_SPLITTER).toBe('0xebefb54a70391ACac074fA68d7929C4a7Ea5f77c')
    expect(addresses.BASKET_ORACLE).toBe('0x2c448B9c7be8244D7F44Ca8D3B81bd6Fb1F7FCa5')
  })

  it('keeps Ethereum Sepolia separate from Arbitrum Sepolia', () => {
    expect(getAddresses(sepolia.id)).toBe(SEPOLIA_ADDRESSES)
    expect(getAddresses(arbitrumSepolia.id).SYNTHETIC_SPLITTER).not.toBe(
      SEPOLIA_ADDRESSES.SYNTHETIC_SPLITTER
    )
  })

  it('still resolves mainnet addresses', () => {
    expect(getAddresses(mainnet.id).USDC).toBe('0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48')
  })
})
