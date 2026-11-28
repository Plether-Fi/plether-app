import { describe, expect, it } from 'vitest'
import {
  PROTOCOL_CONTRACT_SOURCE_PATHS,
  deriveProtocolHealthBanner,
  deriveProtocolStateSummary,
  formatProtocolParamValue,
  parameterImpactHint,
  protocolCommitGitHubUrl,
  protocolContractGitHubUrl,
  protocolTreeGitHubUrl,
  resolveParameterRange,
  shortHash,
  type ProtocolManifestResponse,
  type ProtocolParameterRecord,
} from './protocol'

function baseManifest(overrides: Partial<ProtocolManifestResponse> = {}): ProtocolManifestResponse {
  return {
    network: { chainId: 421614, releaseVersion: 'v2.0.0', settlementAssetSymbol: 'USDG' },
    observedAt: '2026-11-26T18:00:00Z',
    rpcAvailable: true,
    verifiedAtBlock: '332900000',
    release: {
      version: 'v2.0.0',
      date: '2026-10-28',
      phase: 'active',
      deploymentBlock: '323658447',
      sourceCommit: '1209ca8a5b94a4fb6323c7c4585846ed93db3932',
      forgeVersion: '1.5.1-stable',
      manifestSha256: '584cdacada35f8225d2a88811edfbd9622764fe0e6da4b275ba8f33d6f8c0c74',
      bundleSha256: '3ea6e372776abb4fe6b7f3c6b2914b381f4f3da3e71b28872a293d6d32cc8ba7',
      pythAddress: '0x0b73614636c855bf23f342f307fb981a3e47f42b',
      schemaVersion: 4,
      orderInterfaceVersion: 3,
      aaManifestVersion: 'v2',
    },
    settlementToken: {
      name: 'Mock USDG',
      symbol: 'USDG',
      decimals: 6,
      isMock: true,
      address: '0x3d051dbbf5ff427a4ae8ad09746902666af8fb9d',
    },
    safety: {
      available: true,
      phase: 'active',
      lastMarkPrice: '94743619',
      lastMarkTime: '2026-11-26T17:59:30Z',
      tradingActive: true,
      withdrawalLive: true,
      lpEpochSettlementPaused: false,
      oracleFrozen: false,
      fadWindow: false,
      housePoolPaused: false,
      degradedMode: false,
    },
    verification: {
      totalContracts: 30,
      verifiedCount: 30,
      mismatchCount: 0,
      uncheckedCount: 0,
      matchedParameters: 20,
      driftedParameters: 0,
      unavailableParameters: 0,
    },
    parameters: [],
    contracts: [],
    releaseHistory: [],
    ...overrides,
  }
}

describe('protocol API formatters and health helpers', () => {
  it('formats bps, wad, assets (in USDG, never $), seconds, and count accurately', () => {
    expect(formatProtocolParamValue('20', 'bps')).toBe('0.20% (20 bps)')
    expect(formatProtocolParamValue('5000', 'bps')).toBe('50.00% (5,000 bps)')
    expect(formatProtocolParamValue('10000000000000000', 'wad')).toBe('1.00%')
    expect(formatProtocolParamValue('400000000000000000', 'wad')).toBe('40.00%')
    expect(formatProtocolParamValue('200000', 'assets')).toBe('0.20 USDG')
    expect(formatProtocolParamValue('1000000000', 'assets')).toBe('1,000.00 USDG')
    expect(formatProtocolParamValue('40000000000000', 'assets')).toBe('40.00M USDG')
    expect(formatProtocolParamValue('40000000000000', 'assets')).not.toContain('$')
    expect(formatProtocolParamValue('15', 'seconds')).toBe('15s')
    expect(formatProtocolParamValue('300', 'seconds')).toBe('5m')
    expect(formatProtocolParamValue('3600', 'seconds')).toBe('1h')
    expect(formatProtocolParamValue('172800', 'seconds')).toBe('2d (48h)')
    expect(formatProtocolParamValue('5', 'count')).toBe('5')
  })

  it('fails closed to Unavailable for null, empty, or malformed values', () => {
    expect(formatProtocolParamValue(null, 'bps')).toBe('Unavailable')
    expect(formatProtocolParamValue(undefined, 'assets')).toBe('Unavailable')
    expect(formatProtocolParamValue('', 'seconds')).toBe('Unavailable')
    expect(formatProtocolParamValue('not-a-number', 'wad')).toBe('Unavailable')
  })

  it('derives leverage impact hints for margin parameters', () => {
    const im: ProtocolParameterRecord = {
      key: 'initialMarginBps',
      category: 'trader',
      label: 'Initial margin',
      unit: 'bps',
      contract: 'CfdEngine',
      description: 'Min margin',
      manifestDefault: '20',
      liveValue: '20',
      status: 'match',
    }
    expect(parameterImpactHint(im)).toBe('Up to 500× nominal leverage')
    expect(parameterImpactHint({ ...im, key: 'fadMarginBps', liveValue: '300' })).toBe(
      'Caps weekend leverage at ~33×',
    )
    expect(parameterImpactHint({ ...im, key: 'executionFeeBps', liveValue: '4' })).toBeNull()
  })

  it('shortens 32-byte hashes and handles empty/missing hashes cleanly', () => {
    expect(shortHash('0xa0f8e5be6e622a5a83e46e88f6b012757ef924cf613a50cba52a2b5c66b76a6b')).toBe(
      '0xa0f8e5…b76a6b',
    )
    expect(shortHash('0x')).toBe('Empty (0x)')
    expect(shortHash(null)).toBe('Unavailable')
  })

  it('classifies protocol health banner across verified, drift, pause, mismatch, and rpc-down states', () => {
    expect(deriveProtocolHealthBanner(baseManifest()).tone).toBe('live')
    expect(deriveProtocolHealthBanner(baseManifest()).label).toBe('Verified · 30/30 bytecode match')

    const drifted = baseManifest({
      verification: { ...baseManifest().verification, driftedParameters: 2 },
    })
    expect(deriveProtocolHealthBanner(drifted)).toMatchObject({
      tone: 'degraded',
      label: 'Parameter drift detected',
    })

    const paused = baseManifest({
      safety: { ...baseManifest().safety, housePoolPaused: true },
    })
    expect(deriveProtocolHealthBanner(paused)).toMatchObject({
      tone: 'blocked',
      label: 'Safety pause active',
    })

    const mismatch = baseManifest({
      verification: { ...baseManifest().verification, verifiedCount: 29, mismatchCount: 1 },
    })
    expect(deriveProtocolHealthBanner(mismatch)).toMatchObject({
      tone: 'blocked',
      label: 'Bytecode mismatch detected',
    })

    const rpcDown = baseManifest({ rpcAvailable: false })
    expect(deriveProtocolHealthBanner(rpcDown)).toMatchObject({
      tone: 'stale',
      label: 'RPC verification unavailable',
    })
  })

  it('summarizes protocol state cleanly for active, FAD, frozen, paused, and unavailable states', () => {
    const base = baseManifest().safety
    expect(deriveProtocolStateSummary(base)).toMatchObject({ label: 'Active', tone: 'positive' })
    expect(deriveProtocolStateSummary({ ...base, fadWindow: true })).toMatchObject({
      label: 'FAD window',
      tone: 'warning',
    })
    expect(deriveProtocolStateSummary({ ...base, oracleFrozen: true })).toMatchObject({
      label: 'Oracle frozen',
      tone: 'negative',
    })
    expect(deriveProtocolStateSummary({ ...base, available: false })).toMatchObject({
      label: 'Unavailable',
      tone: 'neutral',
    })
  })

  it('computes min-max range bar positions and expands scale if a live value exceeds default ceiling', () => {
    const range = resolveParameterRange({
      key: 'initialMarginBps',
      category: 'trader',
      label: 'Initial margin',
      unit: 'bps',
      contract: 'CfdEngine',
      description: 'Min margin',
      manifestDefault: '20',
      liveValue: '20',
      status: 'match',
    })
    expect(range.minLabel).toBe('0%')
    expect(range.maxLabel).toBe('1%')
    expect(range.livePercent).toBe(20)
    expect(range.defaultPercent).toBe(20)

    const unavailableRange = resolveParameterRange({
      key: 'executionFeeBps',
      category: 'trader',
      label: 'Protocol execution fee',
      unit: 'bps',
      contract: 'CfdEngine',
      description: 'Fee',
      manifestDefault: '4',
      liveValue: null,
      status: 'unavailable',
    })
    expect(unavailableRange.livePercent).toBeNull()
    expect(unavailableRange.defaultPercent).toBe(16)

    const expanded = resolveParameterRange({
      key: 'executionFeeBps',
      category: 'trader',
      label: 'Protocol execution fee',
      unit: 'bps',
      contract: 'CfdEngine',
      description: 'Fee',
      manifestDefault: '4',
      liveValue: '50',
      status: 'drifted',
    })
    expect(expanded.maxLabel).toBe('0.50%')
    expect(expanded.livePercent).toBe(100)
    expect(expanded.defaultPercent).toBe(8)
  })

  it('builds pinned GitHub source links for protocol contracts and commits', () => {
    const commit = '1209ca8a5b94a4fb6323c7c4585846ed93db3932'

    expect(Object.keys(PROTOCOL_CONTRACT_SOURCE_PATHS)).toHaveLength(30)
    expect(protocolContractGitHubUrl({ key: 'cfdOrderAdmissionValidator' }, commit)).toEqual({
      url: `https://github.com/Plether-Fi/plether-core/blob/${commit}/packages/perps/src/CfdOrderAdmissionValidator.sol`,
      path: 'packages/perps/src/CfdOrderAdmissionValidator.sol',
      ref: commit,
    })
    expect(protocolContractGitHubUrl({ key: 'cfdEngine' }, commit)).toEqual({
      url: `https://github.com/Plether-Fi/plether-core/blob/${commit}/packages/perps/src/CfdEngine.sol`,
      path: 'packages/perps/src/CfdEngine.sol',
      ref: commit,
    })
    expect(protocolContractGitHubUrl({ key: 'seniorVault' }, commit)?.url).toBe(
      `https://github.com/Plether-Fi/plether-core/blob/${commit}/packages/perps/src/TrancheVault.sol`,
    )
    expect(
      protocolContractGitHubUrl({ key: 'settlementToken' }, commit, { settlementTokenIsMock: true })?.url,
    ).toBe(
      `https://github.com/Plether-Fi/plether-core/blob/${commit}/script/DeployPerpsArbitrumSepolia.s.sol`,
    )
    expect(
      protocolContractGitHubUrl({ key: 'settlementToken' }, commit, { settlementTokenIsMock: false }),
    ).toBeNull()
    expect(
      protocolContractGitHubUrl({ key: 'customContract', sourcePath: '/packages/perps/src/Custom.sol' }, null),
    ).toEqual({
      url: 'https://github.com/Plether-Fi/plether-core/blob/master/packages/perps/src/Custom.sol',
      path: 'packages/perps/src/Custom.sol',
      ref: 'master',
    })
    expect(protocolContractGitHubUrl({ key: 'unknownContract' }, commit)).toBeNull()

    expect(protocolCommitGitHubUrl(commit)).toBe(
      `https://github.com/Plether-Fi/plether-core/commit/${commit}`,
    )
    expect(protocolCommitGitHubUrl('not-a-sha')).toBeNull()
    expect(protocolTreeGitHubUrl(commit)).toBe(
      `https://github.com/Plether-Fi/plether-core/tree/${commit}/packages/perps/src`,
    )
    expect(protocolTreeGitHubUrl(null)).toBe(
      'https://github.com/Plether-Fi/plether-core/tree/master/packages/perps/src',
    )
  })
})
