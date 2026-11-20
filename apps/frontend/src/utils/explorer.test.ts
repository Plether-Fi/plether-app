import { describe, expect, it } from 'vitest'
import { arbitrumSepolia, mainnet } from 'wagmi/chains'
import { getExplorerTxUrl, getExplorerUserOperationUrl } from './explorer'

describe('explorer links', () => {
  it('uses the reviewed chain explorer for both transaction and UserOperation templates', () => {
    const txHash = '{transactionHash}'
    const operationHash = '{userOperationHash}'
    const transaction = getExplorerTxUrl(arbitrumSepolia.id, txHash)
    const operation = getExplorerUserOperationUrl(arbitrumSepolia.id, operationHash)

    expect(transaction.endsWith(`/tx/${txHash}`)).toBe(true)
    expect(operation.endsWith(`/op/${operationHash}`)).toBe(true)
    expect(new URL(operation).origin).toBe(new URL(transaction).origin)
    expect(new URL(operation).hostname).toMatch(/^arbitrum-sepolia\./)
  })

  it('retains mainnet transaction links', () => {
    expect(getExplorerTxUrl(mainnet.id, '0xabc')).toBe('https://etherscan.io/tx/0xabc')
  })
})
