import { describe, expect, it } from 'vitest'
import { arbitrumSepolia, mainnet, sepolia } from 'wagmi/chains'
import { getAddresses, SEPOLIA_ADDRESSES } from '../addresses'

describe('getAddresses', () => {
  it('returns Arbitrum Sepolia spot deployment addresses for chain 421614', () => {
    const addresses = getAddresses(arbitrumSepolia.id)

    expect(addresses.USDC).toBe('0xf1e1B188b87525C51ECe4bae8627ae621D769651')
    expect(addresses.SYNTHETIC_SPLITTER).toBe('0x1E34a6D7289C30B81d3520B72A8303c1F0153644')
    expect(addresses.DXY_BEAR).toBe('0x2cAE323E3278619f45349676f60C42afD2AEE20D')
    expect(addresses.DXY_BULL).toBe('0x82101515f79a796c33b26FE488d2C6356eBCb1e6')
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
