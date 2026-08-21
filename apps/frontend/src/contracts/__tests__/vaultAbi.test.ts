import { describe, expect, it } from 'vitest'

import { TRANCHE_VAULT_READ_ABI } from '../abis'

describe('tranche vault ABI', () => {
  it('exposes fee-free ERC-4626 share conversion for canonical live pricing', () => {
    const convertToAssets = TRANCHE_VAULT_READ_ABI.find((item) => (
      item.type === 'function' && item.name === 'convertToAssets'
    ))

    expect(convertToAssets).toMatchObject({
      stateMutability: 'view',
      inputs: [{ name: 'shares', type: 'uint256' }],
      outputs: [{ name: 'assets', type: 'uint256' }],
    })
  })
})
