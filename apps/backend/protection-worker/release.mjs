// No historical adapter: workers require the new, deployed schema-four release.
export function requireDeployedRelease(release) {
  if (release?.schemaVersion !== 4 || release?.orderInterfaceVersion !== 3 ||
      release?.network?.chainId !== 421614 || !['deployed', 'seeded', 'active'].includes(release?.release?.phase)) {
    throw new Error('Perps release is prepared or unsupported; deployment is required')
  }
  if ('mockUsdc' in release.contracts || !release.contracts.settlementToken) throw new Error('Invalid USDG token binding')
  if (!Number.isSafeInteger(release.release.deploymentBlock) || release.release.deploymentBlock <= 0) throw new Error('Missing deployment boundary')
  for (const pin of Object.values(release.contracts)) {
    if (!/^0x[0-9a-fA-F]{40}$/.test(pin.address) || /^0x0{40}$/.test(pin.address) ||
        !/^0x[0-9a-fA-F]{64}$/.test(pin.runtimeCodeHash) || /^0x0{64}$/.test(pin.runtimeCodeHash)) throw new Error('Undeployed contract pin')
  }
  const token = release.settlementToken
  if (token?.name !== 'Mock USDG' || token.symbol !== 'USDG' || token.decimals !== 6 || token.isMock !== true ||
      token.supportsReceiveWithAuthorization !== false || token.supportsBridgeFunding !== false) throw new Error('Invalid MockUSDG capabilities')
}
