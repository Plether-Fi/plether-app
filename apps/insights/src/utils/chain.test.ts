import { describe, expect, it } from 'vitest'
import {
  ARBITRUM_ONE_CHAIN_ID,
  ARBITRUM_SEPOLIA_CHAIN_ID,
  explorerAddressUrl,
  explorerTxUrl,
  resolvePerpsChain,
  vaultsAppUrl,
} from './chain'

describe('Perps chain resolution', () => {
  it('defaults to Arbitrum Sepolia when no chain ID is provided', () => {
    const chain = resolvePerpsChain(undefined)
    expect(chain.chainId).toBe(ARBITRUM_SEPOLIA_CHAIN_ID)
    expect(chain.name).toBe('Arbitrum Sepolia')
    expect(chain.isTestnet).toBe(true)
    expect(chain.explorerBaseUrl).toBe('https://sepolia.arbiscan.io')
    expect(chain.tradeAppUrl).toBe('https://app.sepolia.plether.com')
  })

  it('resolves Arbitrum One mainnet metadata and explorer URLs cleanly', () => {
    const chain = resolvePerpsChain(ARBITRUM_ONE_CHAIN_ID)
    expect(chain.chainId).toBe(ARBITRUM_ONE_CHAIN_ID)
    expect(chain.name).toBe('Arbitrum One')
    expect(chain.isTestnet).toBe(false)
    expect(chain.tradeAppUrl).toBe('https://app.plether.com')
    expect(explorerAddressUrl('0xabc', ARBITRUM_ONE_CHAIN_ID)).toBe('https://arbiscan.io/address/0xabc')
    expect(explorerTxUrl('0xdef', ARBITRUM_ONE_CHAIN_ID)).toBe('https://arbiscan.io/tx/0xdef')
    expect(vaultsAppUrl(ARBITRUM_ONE_CHAIN_ID)).toBe('https://app.plether.com/vaults')
    expect(vaultsAppUrl(ARBITRUM_SEPOLIA_CHAIN_ID)).toBe('https://app.sepolia.plether.com/vaults')
  })
})
