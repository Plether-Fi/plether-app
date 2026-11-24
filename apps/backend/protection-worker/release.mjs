// No historical adapter: workers require the new, deployed schema-four release.
const SUPPORTED_PERPS_CHAIN_IDS = new Set([421614, 42161])

export function requireDeployedRelease(release) {
  const chainId = release?.network?.chainId
  if (release?.schemaVersion !== 4 || release?.orderInterfaceVersion !== 3 ||
      !SUPPORTED_PERPS_CHAIN_IDS.has(chainId) || !['deployed', 'seeded', 'active'].includes(release?.release?.phase)) {
    throw new Error('Perps release is prepared or unsupported; deployment is required')
  }
  if ('mockUsdc' in release.contracts || !release.contracts.settlementToken) throw new Error('Invalid USDG token binding')
  if (!Number.isSafeInteger(release.release.deploymentBlock) || release.release.deploymentBlock <= 0) throw new Error('Missing deployment boundary')
  for (const pin of Object.values(release.contracts)) {
    if (!/^0x[0-9a-fA-F]{40}$/.test(pin.address) || /^0x0{40}$/.test(pin.address) ||
        !/^0x[0-9a-fA-F]{64}$/.test(pin.runtimeCodeHash) || /^0x0{64}$/.test(pin.runtimeCodeHash)) throw new Error('Undeployed contract pin')
  }
  const token = release.settlementToken
  if (chainId === 421614) {
    if (token?.name !== 'Mock USDG' || token.symbol !== 'USDG' || token.decimals !== 6 || token.isMock !== true ||
        token.supportsReceiveWithAuthorization !== false || token.supportsBridgeFunding !== false) throw new Error('Invalid MockUSDG capabilities')
  } else {
    if (typeof token?.name !== 'string' || token.name.length === 0 || token.symbol !== 'USDG' || token.decimals !== 6 ||
        token.isMock !== false) throw new Error('Invalid mainnet USDG settlement token capabilities')
  }
}
