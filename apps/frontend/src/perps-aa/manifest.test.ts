import { describe, expect, it, vi } from 'vitest'
import publicManifest from '../../public/perps-aa-manifest.json'
import {
  PERMISSIONLESS_SIMPLE_ACCOUNT_V08_FACTORY,
  PERPS_AA_LEGACY_RPC_PATH,
  PERPS_AA_NATIVE_RPC_PATH,
  PERPS_ENTRY_POINT_V08,
  PerpsAaManifestValidationError,
  bundlerRpcUrlForManifest,
  fetchPerpsAaManifest,
  isPerpsAaManifestV2,
  parsePerpsAaManifest,
  paymasterRpcUrlForManifest,
} from './manifest'

function validManifest(): Record<string, unknown> {
  return {
    version: 'perps-aa-arbitrum-sepolia-v2',
    chainId: 421614,
    entryPoint: PERPS_ENTRY_POINT_V08,
    entryPointVersion: '0.8',
    pimlicoRpcUrl: PERPS_AA_LEGACY_RPC_PATH,
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
    policyEvaluator: '0xcccccccccccccccccccccccccccccccccccccccc',
    userOperationExplorerUrlTemplate:
      'https://explorer.example.com/user-op/{userOperationHash}',
    transactionExplorerUrlTemplate:
      'https://explorer.example.com/tx/{transactionHash}',
    testnetFaucet: null,
    sponsorshipEnabled: false,
  }
}

function validManifestV2(): Record<string, unknown> {
  const common = validManifest()
  delete common.pimlicoRpcUrl
  return {
    ...common,
    version: 'perps-aa-arbitrum-sepolia-v2',
    bundlerRpcUrl: PERPS_AA_NATIVE_RPC_PATH,
    paymasterRpcUrl: PERPS_AA_NATIVE_RPC_PATH,
    paymasterAddress: '0x1234567890123456789012345678901234567890',
    paymasterVersion: 'plether-verifying-v1',
  }
}

function validManifestV1(): Record<string, unknown> {
  return {
    ...validManifest(),
    version: 'perps-aa-arbitrum-sepolia-v1',
  }
}

describe('parsePerpsAaManifest', () => {
  it('validates the manifest served by the app', () => {
    const manifest = parsePerpsAaManifest(publicManifest)

    expect(manifest.version).toBe('perps-aa-arbitrum-sepolia-20260905-v2')
    expect(manifest.chainId).toBe(421614)
    expect(isPerpsAaManifestV2(manifest)).toBe(false)
    expect(bundlerRpcUrlForManifest(manifest)).toBe(
      PERPS_AA_LEGACY_RPC_PATH
    )
  })

  it('parses the current v2 Pimlico shape and normalizes addresses', () => {
    const manifest = parsePerpsAaManifest(validManifest())

    expect(manifest.version).toBe('perps-aa-arbitrum-sepolia-v2')
    expect(manifest.chainId).toBe(421614)
    expect(manifest.smartAccountMode).toBe('simple')
    expect(manifest.entryPointVersion).toBe('0.8')
    expect(manifest.pimlicoRpcUrl).toBe('/api/perps/v1/aa/pimlico')
    expect(manifest.smartAccountFactory).toMatch(/^0x[0-9A-Fa-f]{40}$/)
    expect(manifest.sponsorshipEnabled).toBe(false)
  })

  it('parses the legacy v1 Pimlico shape', () => {
    const manifest = parsePerpsAaManifest(validManifestV1())

    expect(manifest.version).toBe('perps-aa-arbitrum-sepolia-v1')
    expect(isPerpsAaManifestV2(manifest)).toBe(false)
    expect(bundlerRpcUrlForManifest(manifest)).toBe(
      PERPS_AA_LEGACY_RPC_PATH
    )
    expect(paymasterRpcUrlForManifest(manifest)).toBe(
      PERPS_AA_LEGACY_RPC_PATH
    )
  })

  it('parses the exact v2 Alto and Plether paymaster shape', () => {
    const manifest = parsePerpsAaManifest(validManifestV2())

    expect(manifest.version).toBe('perps-aa-arbitrum-sepolia-v2')
    expect(manifest).toMatchObject({
      bundlerRpcUrl: '/api/perps/v1/aa/rpc',
      paymasterRpcUrl: '/api/perps/v1/aa/rpc',
      paymasterAddress: '0x1234567890123456789012345678901234567890',
      paymasterVersion: 'plether-verifying-v1',
    })
    expect(isPerpsAaManifestV2(manifest)).toBe(true)
    expect(bundlerRpcUrlForManifest(manifest)).toBe(
      PERPS_AA_NATIVE_RPC_PATH
    )
    expect(paymasterRpcUrlForManifest(manifest)).toBe(
      PERPS_AA_NATIVE_RPC_PATH
    )
    expect('pimlicoRpcUrl' in manifest).toBe(false)
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

    expect(() => parsePerpsAaManifest(manifest)).toThrow(/supported v1 or v2/)
  })

  it('rejects a hybrid v2 manifest containing both RPC schemas', () => {
    const hybrid = validManifestV2()
    hybrid.pimlicoRpcUrl = PERPS_AA_LEGACY_RPC_PATH

    expect(() => parsePerpsAaManifest(hybrid)).toThrow(
      /unknown field "pimlicoRpcUrl"/
    )
  })

  it('rejects native RPC fields on a v1 manifest', () => {
    const nativeV1 = validManifestV2()
    nativeV1.version = 'perps-aa-arbitrum-sepolia-v1'

    expect(() => parsePerpsAaManifest(nativeV1)).toThrow(
      /missing required field "pimlicoRpcUrl".*unknown field "bundlerRpcUrl"/
    )
  })

  it('rejects an ambiguous v2 manifest without either RPC schema', () => {
    const ambiguous = validManifest()
    delete ambiguous.pimlicoRpcUrl

    expect(() => parsePerpsAaManifest(ambiguous)).toThrow(
      /missing required field "pimlicoRpcUrl"/
    )
  })

  it.each([
    'bundlerRpcUrl',
    'paymasterRpcUrl',
    'paymasterAddress',
    'paymasterVersion',
  ])('requires v2 field %s', (field) => {
    const manifest = validManifestV2()
    delete manifest[field]

    expect(() => parsePerpsAaManifest(manifest)).toThrow(
      new RegExp(`missing required field "${field}"`)
    )
  })

  it('requires same-origin v2 RPCs and reviewed paymaster metadata', () => {
    for (const field of ['bundlerRpcUrl', 'paymasterRpcUrl']) {
      const manifest = validManifestV2()
      manifest[field] = 'https://bundler.example/rpc?key=secret'
      expect(() => parsePerpsAaManifest(manifest)).toThrow(/same-origin/)
    }

    const zeroPaymaster = validManifestV2()
    zeroPaymaster.paymasterAddress =
      '0x0000000000000000000000000000000000000000'
    expect(() => parsePerpsAaManifest(zeroPaymaster)).toThrow(
      /paymasterAddress.*must not be the zero address/
    )

    const unknownVersion = validManifestV2()
    unknownVersion.paymasterVersion = 'plether-verifying-v2'
    expect(() => parsePerpsAaManifest(unknownVersion)).toThrow(
      /paymasterVersion.*plether-verifying-v1/
    )
  })

  it('pins v1 to the reviewed Pimlico endpoint exactly', () => {
    for (const endpoint of [
      PERPS_AA_NATIVE_RPC_PATH,
      `${PERPS_AA_LEGACY_RPC_PATH}/extra`,
      `${PERPS_AA_LEGACY_RPC_PATH}?mode=legacy`,
    ]) {
      const manifest = validManifestV1()
      manifest.pimlicoRpcUrl = endpoint

      expect(() => parsePerpsAaManifest(manifest)).toThrow(
        new RegExp(PERPS_AA_LEGACY_RPC_PATH)
      )
    }
  })

  it.each(['bundlerRpcUrl', 'paymasterRpcUrl'])(
    'pins v2 %s to the shared native endpoint exactly',
    (field) => {
      for (const endpoint of [
        PERPS_AA_LEGACY_RPC_PATH,
        `${PERPS_AA_NATIVE_RPC_PATH}/extra`,
        `${PERPS_AA_NATIVE_RPC_PATH}?method=send`,
      ]) {
        const manifest = validManifestV2()
        manifest[field] = endpoint

        expect(() => parsePerpsAaManifest(manifest)).toThrow(
          new RegExp(PERPS_AA_NATIVE_RPC_PATH)
        )
      }
    }
  )

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
