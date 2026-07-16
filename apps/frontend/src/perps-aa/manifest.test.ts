import { describe, expect, it } from 'vitest'
import {
  PerpsAaManifestValidationError,
  parsePerpsAaManifest,
} from './manifest'

function validManifest(): Record<string, unknown> {
  return {
    version: 'perps-aa-arbitrum-sepolia-v1',
    chainId: 421614,
    entryPoint: '0x1111111111111111111111111111111111111111',
    paymaster: '0x2222222222222222222222222222222222222222',
    policyId: `0x${'3'.repeat(64)}`,
    sponsorServiceRpcUrl: 'https://sponsor.example.com/rpc',
    bundlerRpcUrl: 'https://bundler.example.com/rpc',
    smartAccountMode: 'separate-immutable',
    smartAccountFactory: '0x4444444444444444444444444444444444444444',
    smartAccountImplementation:
      '0x5555555555555555555555555555555555555555',
    accountRuntimeCodeHash: `0x${'6'.repeat(64)}`,
    usdc: '0x7777777777777777777777777777777777777777',
    usdcSupportsEip3009: false,
    usdcEip712Name: null,
    usdcEip712Version: null,
    marginClearinghouse: '0x8888888888888888888888888888888888888888',
    cfdEngine: '0x9999999999999999999999999999999999999999',
    orderRouter: '0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
    userOperationExplorerUrlTemplate:
      'https://explorer.example.com/user-op/{userOperationHash}',
    transactionExplorerUrlTemplate:
      'https://explorer.example.com/tx/{transactionHash}',
    testnetFaucet: null,
    sponsorshipEnabled: false,
  }
}

describe('parsePerpsAaManifest', () => {
  it('parses the reviewed v1 shape and normalizes addresses', () => {
    const manifest = parsePerpsAaManifest(validManifest())

    expect(manifest.version).toBe('perps-aa-arbitrum-sepolia-v1')
    expect(manifest.chainId).toBe(421614)
    expect(manifest.smartAccountMode).toBe('separate-immutable')
    expect(manifest.smartAccountFactory).toMatch(/^0x[0-9A-Fa-f]{40}$/)
    expect(manifest.sponsorshipEnabled).toBe(false)
  })

  it('rejects missing and unknown fields instead of applying defaults', () => {
    const manifest = validManifest()
    delete manifest.paymaster
    manifest.unreviewedEndpoint = 'https://example.com'

    expect(() => parsePerpsAaManifest(manifest)).toThrowError(
      PerpsAaManifestValidationError
    )
    expect(() => parsePerpsAaManifest(manifest)).toThrow(
      /missing required field "paymaster".*unknown field "unreviewedEndpoint"/
    )
  })

  it('rejects unsupported manifest versions', () => {
    const manifest = validManifest()
    manifest.version = 'perps-aa-arbitrum-sepolia-v2'

    expect(() => parsePerpsAaManifest(manifest)).toThrow(/supported v1/)
  })

  it('rejects inconsistent account-mode configuration', () => {
    const manifest = validManifest()
    manifest.smartAccountMode = 'eip-7702'

    expect(() => parsePerpsAaManifest(manifest)).toThrow(
      /smartAccountFactory.*must be null/
    )
  })

  it('rejects EIP-3009 capability without exact EIP-712 metadata', () => {
    const manifest = validManifest()
    manifest.usdcSupportsEip3009 = true

    expect(() => parsePerpsAaManifest(manifest)).toThrow(
      /requires both USDC EIP-712 name and version/
    )
  })

  it('rejects endpoint credentials in client-visible URLs', () => {
    const manifest = validManifest()
    manifest.bundlerRpcUrl = 'https://secret:token@bundler.example.com/rpc'

    expect(() => parsePerpsAaManifest(manifest)).toThrow(
      /must not embed credentials/
    )
  })
})
