import { describe, expect, it } from 'vitest'
import type { Address, Hex } from 'viem'
import { executeSponsoredPerpsAction } from '../execution'
import type { PerpsAaDeploymentManifest } from '../manifest'

const ADDRESS = '0x1111111111111111111111111111111111111111' as Address
const OTHER_ADDRESS = '0x2222222222222222222222222222222222222222' as Address
const BYTES32 = `0x${'11'.repeat(32)}` as Hex

function manifest(): PerpsAaDeploymentManifest {
  return {
    version: 'perps-aa-arbitrum-sepolia-v1',
    chainId: 421614,
    entryPoint: ADDRESS,
    paymaster: OTHER_ADDRESS,
    policyId: BYTES32,
    sponsorServiceRpcUrl: 'https://example.com/sponsor',
    bundlerRpcUrl: 'https://example.com/bundler',
    smartAccountMode: 'eip-7702',
    smartAccountFactory: null,
    smartAccountImplementation: OTHER_ADDRESS,
    accountRuntimeCodeHash: BYTES32,
    usdc: ADDRESS,
    usdcSupportsEip3009: false,
    usdcEip712Name: null,
    usdcEip712Version: null,
    marginClearinghouse: OTHER_ADDRESS,
    cfdEngine: ADDRESS,
    orderRouter: OTHER_ADDRESS,
    userOperationExplorerUrlTemplate:
      'https://example.com/user-operation/{userOperationHash}',
    transactionExplorerUrlTemplate:
      'https://example.com/transaction/{transactionHash}',
    testnetFaucet: null,
    sponsorshipEnabled: false,
  }
}

describe('executeSponsoredPerpsAction', () => {
  it('fails closed when the remote manifest kill switch is off', async () => {
    await expect(executeSponsoredPerpsAction({
      manifest: manifest(),
      ownerAddress: ADDRESS,
      action: {
        kind: 'place-order',
        account: ADDRESS,
        calls: [],
      },
      runtime: {
        chainId: 421614,
        ownerAddress: ADDRESS,
        factoryAddress: null,
        implementationAddress: OTHER_ADDRESS,
        implementationVersion: 'delegate-v1',
        accountRuntimeCodeHash: BYTES32,
        smartAccount: {
          accountAddress: ADDRESS,
          entryPoint: ADDRESS,
          buildUserOperation: async () => {
            throw new Error('must not build')
          },
          applyPaymaster: (operation) => operation,
          applyGasEstimate: (operation) => operation,
          signUserOperation: async (operation) => operation,
        },
      },
    })).rejects.toMatchObject({
      reason: 'SPONSOR_UNAVAILABLE',
      retryable: true,
    })
  })
})
