// Server-side only. Imported by the separate local Vite config, never app code.
import { parseFrankfurtTarget, type FrankfurtOutputs } from './aa-frankfurt-target.ts'
export const localManifestPath = '/perps-aa-manifest.frankfurt.json'
type Environment = Record<string, string | undefined>

export function frankfurtLocalProfile(env: Environment, outputs: FrankfurtOutputs, source: Record<string, unknown>) {
  const tunnel = parseFrankfurtTarget(outputs)
  const url = new URL(tunnel.local_url)
  const apiOnly = env.AA_FRANKFURT_MODE === 'api-readonly'
  if (!['', 'api-readonly', 'aa'].includes(env.AA_FRANKFURT_MODE ?? '')) {
    throw new Error('AA_FRANKFURT_MODE must be api-readonly or aa')
  }
  if (Object.keys(env).some(key => key.startsWith('VITE_') && /(?:ORIGIN_TOKEN|PRIVATE_KEY)/.test(key))) {
    throw new Error('Private keys and origin credentials must never use VITE_ variables')
  }
  if (env.VITE_API_URL || env.VITE_API_PROXY_PRESERVE_PATH === '1'
    || (env.VITE_API_PROXY_TARGET && new URL(env.VITE_API_PROXY_TARGET).origin !== url.origin)) {
    throw new Error('Remove conflicting API overrides; Frankfurt must use its same-origin local proxy')
  }
  if (!apiOnly && (!/^[0-9a-f]{64}$/.test(env.AA_PROXY_ORIGIN_TOKEN ?? '')
    || /^(.)\1+$/.test(env.AA_PROXY_ORIGIN_TOKEN ?? ''))) {
    throw new Error('Provide the dedicated Frankfurt AA_PROXY_ORIGIN_TOKEN server-side')
  }
  const paymasterAddress = env.AA_FRANKFURT_PAYMASTER_ADDRESS
  if (!apiOnly && (!/^0x[0-9a-fA-F]{40}$/.test(paymasterAddress ?? '') || /^0x0{40}$/.test(paymasterAddress ?? ''))) {
    throw new Error('Provide the separately deployed and verified Frankfurt paymaster address')
  }
  if (source.version !== 'perps-aa-arbitrum-sepolia-20260910-v2' || source.chainId !== 421614
    || source.orderRouter !== '0x6215d36fcbd610ca1525252eebcbfd8b223a6072') {
    throw new Error('Local sponsorship must use the checked-in Core v1.2.3 manifest')
  }
  if (!['', 'false', 'true'].includes(env.AA_FRANKFURT_SPONSORSHIP_ENABLED ?? '')) {
    throw new Error('AA_FRANKFURT_SPONSORSHIP_ENABLED must be true or false')
  }
  if (apiOnly && env.AA_FRANKFURT_SPONSORSHIP_ENABLED === 'true') {
    throw new Error('API-only mode cannot enable sponsorship')
  }
  const manifest = { ...source }
  delete manifest.pimlicoRpcUrl
  Object.assign(manifest, {
    bundlerRpcUrl: '/api/perps/v1/aa/rpc', paymasterRpcUrl: '/api/perps/v1/aa/rpc',
    paymasterAddress: apiOnly ? null : paymasterAddress,
    paymasterVersion: apiOnly ? null : 'plether-verifying-v1', testnetFaucet: null,
    sponsorshipEnabled: env.AA_FRANKFURT_SPONSORSHIP_ENABLED === 'true',
  })
  if (apiOnly) {
    // Preserve the supported legacy manifest shape with issuance disabled.
    // This is a local, blocked route: no Pimlico credentials or fallback exist.
    for (const field of ['bundlerRpcUrl', 'paymasterRpcUrl', 'paymasterAddress', 'paymasterVersion']) delete manifest[field]
    manifest.pimlicoRpcUrl = '/api/perps/v1/aa/pimlico'
  }
  return { target: url.origin, manifest, apiOnly }
}
