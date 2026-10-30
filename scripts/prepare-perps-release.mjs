// Offline candidate generation only. This command never changes the active
// release, enables sponsorship, calls RPC, or broadcasts transactions.
import fs from 'node:fs'
import path from 'node:path'
import { pathToFileURL } from 'node:url'
import { createHash } from 'node:crypto'

const ADDRESS = /^0x[0-9a-fA-F]{40}$/
const HASH = /^0x[0-9a-fA-F]{64}$/
const nonzero = value => !/^0x0+$/.test(value)
const requireValue = (condition, message) => { if (!condition) throw Error(message) }
const requireAddress = (value, label) => requireValue(typeof value === 'string' && ADDRESS.test(value) && nonzero(value), `${label} must be a deployed nonzero address`)
const requireHash = (value, label) => requireValue(typeof value === 'string' && HASH.test(value) && nonzero(value), `${label} must be a nonzero runtime hash`)

export const terraformBindings = {
  perps_settlement_token: 'settlementToken',
  perps_order_router: 'orderRouter',
  perps_order_lifecycle_book: 'orderLifecycleBook',
  perps_cfd_engine: 'cfdEngine',
  perps_margin_clearinghouse: 'marginClearinghouse',
  perps_house_pool: 'housePool',
  perps_plether_oracle: 'pletherOracle',
  perps_senior_vault: 'seniorVault',
  perps_junior_vault: 'juniorVault',
  perps_settlement_monitor_lens: 'settlementMonitorLens',
  perps_cfd_engine_settlement_sidecar: 'cfdEngineSettlementSidecar',
  perps_cfd_engine_lens: 'cfdEngineLens',
  perps_account_lens: 'cfdEngineAccountLens',
  vault_history_house_pool_address: 'housePool',
  vault_history_senior_vault_address: 'seniorVault',
  vault_history_junior_vault_address: 'juniorVault',
}
const aaBindings = {
  settlementToken: 'settlementToken', marginClearinghouse: 'marginClearinghouse',
  cfdEngine: 'cfdEngine', orderRouter: 'orderRouter', orderLifecycleBook: 'orderLifecycleBook',
  policyEvaluator: 'cfdOrderPolicyEvaluator', positionProtectionBook: 'positionProtectionBook',
}
const otherContracts = ['perpsPublicLens', 'settlementMonitorLensSidecar', 'orderRouterAdmin', 'cfdEngineAdmin', 'cfdEnginePlanner',
  'terminalNavBookV2', 'housePoolRedemptionMathSidecar', 'cfdEngineProtocolLens', 'cfdEngineOpenQuoter', 'cfdClosePreview',
  'orderRouterExecutionSidecar', 'orderRecoverySidecar', 'orderRouterLiquidationBatchSidecar', 'emergencyPauseCoordinator']

export function settlementContract(manifest) {
  requireValue(manifest?.schemaVersion === 4, 'Canonical Core manifest schema 4 is required')
  return manifest.contracts?.settlementToken
}

export function validateCoreRelease(manifest) {
  requireValue(manifest?.schemaVersion === 4, 'Canonical Core manifest schema 4 is required')
  requireValue(manifest.network?.chainId === 421614, 'Expected Arbitrum Sepolia chain 421614')
  requireValue(['deployed', 'seeded', 'active'].includes(manifest.release?.phase), 'Prepared manifests cannot become app release candidates; use verified deployed evidence')
  requireValue(Number.isSafeInteger(manifest.release.deploymentBlock) && manifest.release.deploymentBlock > 0, 'Missing deployment block')
  requireValue(typeof manifest.release.version === 'string' && /^v[0-9]+\.[0-9]+\.[0-9]+(?:-[a-zA-Z0-9.-]+)?$/.test(manifest.release.version), 'Missing release version')
  requireValue(/^[0-9a-f]{40}$/i.test(manifest.release.sourceCommit ?? ''), 'Missing immutable Core source commit')
  requireValue(manifest.verification?.deployedPhase?.passed === true, 'Core deployed-phase verification must pass')
  requireAddress(manifest.release.pyth, 'release.pyth')
  for (const [name, contract] of Object.entries(manifest.contracts ?? {})) {
    requireAddress(contract?.address, `${name}.address`)
    requireHash(contract?.runtimeCodeHash, `${name}.runtimeCodeHash`)
  }
  const contracts = manifest.contracts
  const expected = new Set([...Object.values(terraformBindings), ...Object.values(aaBindings), ...otherContracts])
  requireValue(Object.keys(contracts).length === expected.size, 'Unexpected canonical contract graph')
  for (const name of new Set([...Object.values(terraformBindings), ...Object.values(aaBindings), ...otherContracts])) {
    requireAddress(contracts[name]?.address, `${name}.address`)
    requireHash(contracts[name]?.runtimeCodeHash, `${name}.runtimeCodeHash`)
  }
  if (manifest.schemaVersion === 4) {
    const token = manifest.settlementToken
    requireValue(token?.name === 'Mock USDG' && token.symbol === 'USDG' && token.decimals === 6 && token.isMock === true,
      'Schema 4 candidate requires six-decimal MockUSDG metadata')
    requireValue(token.supportsReceiveWithAuthorization === false && token.supportsBridgeFunding === false,
      'MockUSDG must not advertise signed deposits or bridge funding')
    requireValue(contracts.settlementToken.artifact === 'script/DeployPerpsArbitrumSepolia.s.sol:MockUSDG', 'Wrong MockUSDG artifact')
    const rewards = manifest.rewards
    requireValue(rewards?.status === 'not_applicable' && rewards.payoutAddress === null && rewards.payoutController === null,
      'MockUSDG rewards must remain not applicable with no payout configuration')
    requireValue(JSON.stringify(rewards.holdingContracts) === JSON.stringify(['marginClearinghouse', 'housePool']), 'Unexpected rewards holding contracts')
  }
  return contracts
}

// The profile deliberately supplies AA infrastructure independently of Core.
// Never copy paymaster/factory addresses or rollout flags from the old release.
export function prepareRelease(manifest, profile, sourceManifestSha256) {
  const contracts = validateCoreRelease(manifest)
  requireValue(manifest.orderInterfaceVersion === 3, 'Core manifest must declare orderInterfaceVersion 3')
  requireValue(/^[a-f0-9]{64}$/i.test(sourceManifestSha256 ?? ''), 'Missing source manifest SHA-256')
  requireValue(typeof profile?.integration?.aaManifestVersion === 'string' && /^perps-aa-[a-z0-9]+(?:-[a-z0-9]+)*-v2$/.test(profile.integration.aaManifestVersion),
    'Set a bounded V2 AA manifest version (perps-aa-...-v2) in the integration profile')
  requireValue(profile.integration.orderInterfaceVersion === 3, 'The current app requires orderInterfaceVersion 3')
  requireValue(Number.isSafeInteger(profile.integration.volumeHistoryStartTimestamp) && profile.integration.volumeHistoryStartTimestamp > 0 && profile.integration.volumeHistoryStartTimestamp % 60 === 0,
    'Set the first whole minute after deployment as volumeHistoryStartTimestamp')
  requireValue(/^[a-f0-9]{64}$/i.test(profile.bundleSha256 ?? ''), 'Set the reviewed release bundle SHA-256')
  const aaProfile = profile.aa
  requireValue(aaProfile && typeof aaProfile === 'object', 'Missing AA integration profile')
  for (const key of ['entryPoint', 'paymasterAddress', 'smartAccountFactory']) requireAddress(aaProfile[key], `aa.${key}`)
  requireValue(aaProfile.entryPoint.toLowerCase() === '0x4337084d9e255ff0702461cf8895ce9e3b5ff108' &&
    aaProfile.smartAccountFactory.toLowerCase() === '0x13e9ed32155810fdbd067d4522c492d6f68e5944', 'Unsupported EntryPoint or Trading Account factory')
  requireValue(aaProfile.entryPointVersion === '0.8' && aaProfile.smartAccountMode === 'simple' && aaProfile.smartAccountVersion === 'permissionless-simple-v0.8' && aaProfile.smartAccountIndex === '0', 'Unsupported Trading Account profile')
  requireValue(aaProfile.paymasterVersion === 'plether-verifying-v1' && aaProfile.preparationRpcVersion === 1, 'Unsupported paymaster profile')
  requireValue(aaProfile.bundlerRpcUrl === '/api/perps/v1/aa/rpc' && aaProfile.paymasterRpcUrl === aaProfile.bundlerRpcUrl, 'AA RPC must use the backend gateway')
  const appRelease = structuredClone(manifest)
  appRelease.integration = { ...profile.integration }
  appRelease.release.manifestSha256 = sourceManifestSha256
  appRelease.release.bundleSha256 = profile.bundleSha256
  const aaManifest = {
    version: profile.integration.aaManifestVersion,
    orderInterfaceVersion: profile.integration.orderInterfaceVersion,
    chainId: manifest.network.chainId,
    ...Object.fromEntries(['entryPoint', 'entryPointVersion', 'bundlerRpcUrl', 'paymasterRpcUrl', 'paymasterAddress', 'paymasterVersion',
      'preparationRpcVersion', 'smartAccountMode', 'smartAccountVersion', 'smartAccountIndex', 'smartAccountFactory']
      .map(key => [key, aaProfile[key]])),
    ...Object.fromEntries(Object.entries(aaBindings).map(([key, name]) => [key, contracts[name].address])),
    settlementTokenSupportsEip3009: false,
    settlementTokenEip712Name: null,
    settlementTokenEip712Version: null,
    userOperationExplorerUrlTemplate: 'https://arbitrum-sepolia.blockscout.com/op/{userOperationHash}',
    transactionExplorerUrlTemplate: 'https://arbitrum-sepolia.blockscout.com/tx/{transactionHash}',
    testnetFaucet: null,
    sponsorshipEnabled: false,
  }
  const terraform = {
    perps_chain_id: String(manifest.network.chainId),
    ...Object.fromEntries(Object.entries(terraformBindings).map(([key, name]) => [key, contracts[name].address])),
    vault_history_deployment_block: String(manifest.release.deploymentBlock),
    perps_indexer_start_block: String(manifest.release.deploymentBlock),
  }
  const backendEnv = Object.fromEntries(Object.entries(terraform).map(([key, value]) => [key.toUpperCase(), value]))
  return {
    'mock-usdg-prepared.json': appRelease,
    'perps-aa-manifest.json': aaManifest,
    'perps-core.tfvars.json': terraform,
    'perps-backend.env.json': backendEnv,
    'activation-review.json': {
      status: 'candidate_only',
      sourceManifestSha256,
      settlementToken: { address: contracts.settlementToken.address, runtimeCodeHash: contracts.settlementToken.runtimeCodeHash },
      requiredFollowups: [
        'Review and publish the complete new Core deployment and ABI bundle; regenerate protection-worker ABI from that bundle.',
        'Review backend/frontend deployment workflow pins and Terraform deployment_target.tf together with the candidate; activate only after the new release pins and evidence have been reviewed.',
        'Review the integrated cfdClosePreview contract and close-assistance bindings against the new release; do not reuse old deployment bindings.',
        'Configure native AA safety inputs, sponsor/paymaster policy, RPC and worker signers for the new deployment. Sponsorship stays disabled in this candidate.',
        'Use a fresh indexer deployment boundary and verify history/competition release isolation before activation.',
        'Keep MockUSDG rewards not applicable. Mint to the Trading Account and use approve plus deposit; do not enable EIP-3009 or bridge funding.',
      ],
    },
  }
}

export function writeCandidate(directory, artifacts) {
  // Refuse an existing directory, including every active config/public directory.
  // Validation and rendering happen before this single filesystem mutation.
  const encoded = Object.entries(artifacts).map(([name, value]) => [name, JSON.stringify(value, null, 2) + '\n'])
  fs.mkdirSync(directory, { recursive: false })
  for (const [name, value] of encoded) fs.writeFileSync(path.join(directory, name), value, { flag: 'wx' })
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  const [manifestPath, profilePath, outputPath, ...extra] = process.argv.slice(2)
  requireValue(manifestPath && profilePath && outputPath && extra.length === 0,
    'Usage: node scripts/prepare-perps-release.mjs <deployed-core-manifest.json> <integration-profile.json> <new-output-directory>')
  const source = fs.readFileSync(manifestPath)
  const artifacts = prepareRelease(JSON.parse(source), JSON.parse(fs.readFileSync(profilePath)), createHash('sha256').update(source).digest('hex'))
  writeCandidate(outputPath, artifacts)
  console.log(`Prepared ${Object.keys(artifacts).length} candidate files in ${outputPath}. Active configuration unchanged.`)
}
