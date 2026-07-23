import { describe, expect, it } from 'vitest'
import {
  DEFAULT_TESTNET_AA_MANIFEST_URL,
  resolvePerpsAaManifestUrl,
} from './manifestUrl'

describe('resolvePerpsAaManifestUrl', () => {
  it('uses an explicitly configured manifest URL', () => {
    expect(resolvePerpsAaManifestUrl(' /custom-manifest.json ', false)).toBe(
      '/custom-manifest.json'
    )
  })

  it('uses the bundled manifest for a testnet deployment', () => {
    expect(resolvePerpsAaManifestUrl(undefined, true)).toBe(
      DEFAULT_TESTNET_AA_MANIFEST_URL
    )
  })

  it('keeps non-testnet deployments fail-closed without configuration', () => {
    expect(resolvePerpsAaManifestUrl(undefined, false)).toBeNull()
  })
})
