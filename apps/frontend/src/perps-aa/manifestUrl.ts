export const DEFAULT_TESTNET_AA_MANIFEST_URL = '/perps-aa-manifest.json'

export function resolvePerpsAaManifestUrl(
  configuredValue: unknown,
  isTestnetDeployment: boolean
): string | null {
  if (typeof configuredValue === 'string' && configuredValue.trim() !== '') {
    return configuredValue.trim()
  }

  return isTestnetDeployment ? DEFAULT_TESTNET_AA_MANIFEST_URL : null
}
