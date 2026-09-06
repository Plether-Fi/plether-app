import { describe, expect, it, vi } from 'vitest'
import publicManifest from '../../public/perps-aa-manifest.json'
import {
  PERMISSIONLESS_SIMPLE_ACCOUNT_V08_FACTORY,
  PERPS_ENTRY_POINT_V08,
  PerpsAaManifestValidationError,
  fetchPerpsAaManifest,
  parsePerpsAaManifest,
} from './manifest'

function validManifest(): Record<string, unknown> {
  return {
    version: 'perps-aa-arbitrum-sepolia-v2',
    chainId: 421614,
    entryPoint: PERPS_ENTRY_POINT_V08,
    entryPointVersion: '0.8',
    pimlicoRpcUrl: '/api/perps/v1/aa/pimlico',
    smartAccountMode: 'simple',
    smartAccountVersion: 'permissionless-simple-v0.8',
    smartAccountIndex: '0',
    smartAccountFactory: PERMISSIONLESS_SIMPLE_ACCOUNT_V08_FACTORY,
    usdc: '0x7777777777777777777777777777777777777777',
    usdcSupportsEip3009: false,
    usdcEip712Name: null,
    usdcEip712Version: null,
    marginClearinghouse: '0x8888888888888888888888888888888888888888',
    cfdEngine: '0x9999999999999999999999999999999999999999',
    orderRouter: '0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
    orderLifecycleBook: '0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
    positionProtectionBook: '0x63973Eb0B5a862dfc95348D4d575FC55C9546F04',
    policyEvaluator: '0xcccccccccccccccccccccccccccccccccccccccc',
    userOperationExplorerUrlTemplate:
      'https://explorer.example.com/user-op/{userOperationHash}',
    transactionExplorerUrlTemplate:
      'https://explorer.example.com/tx/{transactionHash}',
    testnetFaucet: null,
    sponsorshipEnabled: false,
  }
}

describe('parsePerpsAaManifest', () => {
  it('validates the manifest served by the app', () => {
    const manifest = parsePerpsAaManifest(publicManifest)

    expect(manifest.version).toBe('perps-aa-arbitrum-sepolia-20260905-v2')
    expect(manifest.chainId).toBe(421614)
  })

  it('parses the reviewed V2 shape and normalizes addresses', () => {
    const manifest = parsePerpsAaManifest(validManifest())

    expect(manifest.version).toBe('perps-aa-arbitrum-sepolia-v2')
    expect(manifest.chainId).toBe(421614)
    expect(manifest.smartAccountMode).toBe('simple')
    expect(manifest.entryPointVersion).toBe('0.8')
    expect(manifest.pimlicoRpcUrl).toBe('/api/perps/v1/aa/pimlico')
    expect(manifest.smartAccountFactory).toMatch(/^0x[0-9A-Fa-f]{40}$/)
    expect(manifest.sponsorshipEnabled).toBe(false)
  })

  it('rejects missing and unknown fields instead of applying defaults', () => {
    const manifest = validManifest()
    delete manifest.pimlicoRpcUrl
    manifest.unreviewedEndpoint = 'https://example.com'

    expect(() => parsePerpsAaManifest(manifest)).toThrowError(
      PerpsAaManifestValidationError
    )
    expect(() => parsePerpsAaManifest(manifest)).toThrow(
      /missing required field "pimlicoRpcUrl".*unknown field "unreviewedEndpoint"/
    )
  })

  it('parses the V2 deployment bindings', () => {
    expect(parsePerpsAaManifest(validManifest())).toMatchObject({
      version: 'perps-aa-arbitrum-sepolia-v2',
      orderLifecycleBook: '0xbBbBBBBbbBBBbbbBbbBbbbbBBbBbbbbBbBbbBBbB',
      policyEvaluator: '0xCcCCccccCCCCcCCCCCCcCcCccCcCCCcCcccccccC',
    })
  })

  it('rejects a V2 manifest without lifecycle dependencies', () => {
    const manifest = validManifest()
    delete manifest.policyEvaluator

    expect(() => parsePerpsAaManifest(manifest)).toThrow(
      /missing required field "policyEvaluator"/
    )
  })

  it('rejects unsupported manifest versions', () => {
    const manifest = validManifest()
    manifest.version = 'perps-aa-arbitrum-sepolia-v3'

    expect(() => parsePerpsAaManifest(manifest)).toThrow(/bounded V2 manifest/)
  })

  it('rejects V1 manifests even when their legacy shape is otherwise valid', () => {
    const manifest = validManifest()
    manifest.version = 'perps-aa-arbitrum-sepolia-v1'

    expect(() => parsePerpsAaManifest(manifest)).toThrow(/bounded V2 manifest/)
  })

  it('rejects unsupported account modes', () => {
    const manifest = validManifest()
    manifest.smartAccountMode = 'eip-7702'

    expect(() => parsePerpsAaManifest(manifest)).toThrow(
      /smartAccountMode.*must be "simple"/
    )
  })

  it('rejects EIP-3009 capability without exact EIP-712 metadata', () => {
    const manifest = validManifest()
    manifest.usdcSupportsEip3009 = true

    expect(() => parsePerpsAaManifest(manifest)).toThrow(
      /requires both USDC EIP-712 name and version/
    )
  })

  it('requires a same-origin proxy instead of a client-visible Pimlico URL', () => {
    const manifest = validManifest()
    manifest.pimlicoRpcUrl =
      'https://api.pimlico.io/v2/421614/rpc?apikey=secret'

    expect(() => parsePerpsAaManifest(manifest)).toThrow(
      /same-origin/
    )
  })

  it('pins the deterministic SimpleAccount index to zero', () => {
    const manifest = validManifest()
    manifest.smartAccountIndex = '1'

    expect(() => parsePerpsAaManifest(manifest)).toThrow(
      /smartAccountIndex.*must be "0"/
    )
  })

  it('pins the reviewed EntryPoint and SimpleAccount factory', () => {
    const entryPointManifest = validManifest()
    entryPointManifest.entryPoint =
      '0x1111111111111111111111111111111111111111'
    expect(() => parsePerpsAaManifest(entryPointManifest)).toThrow(
      /entryPoint.*reviewed deployment/
    )

    const factoryManifest = validManifest()
    factoryManifest.smartAccountFactory =
      '0x4444444444444444444444444444444444444444'
    expect(() => parsePerpsAaManifest(factoryManifest)).toThrow(
      /smartAccountFactory.*reviewed deployment/
    )
  })
})

describe('fetchPerpsAaManifest', () => {
  it('conditionally revalidates the manifest and forwards cancellation', async () => {
    const signal = new AbortController().signal
    const fetchManifest = vi.fn(async () => new Response(
      JSON.stringify(validManifest()),
      {
        status: 200,
        headers: { 'Content-Type': 'application/json' },
      }
    ))

    await expect(fetchPerpsAaManifest('/perps-aa-manifest.json', {
      fetch: fetchManifest,
      signal,
    })).resolves.toMatchObject({
      version: 'perps-aa-arbitrum-sepolia-v2',
      chainId: 421614,
    })

    expect(fetchManifest).toHaveBeenCalledWith(
      '/perps-aa-manifest.json',
      {
        cache: 'no-cache',
        credentials: 'omit',
        signal,
      }
    )
  })
})
