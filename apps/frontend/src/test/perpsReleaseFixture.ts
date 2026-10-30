import release from '../contracts/__tests__/fixtures/perps-canonical-release.json'
import { parsePerpsReleaseConfiguration, settlementTokenContract } from '../contracts/perpsRelease'
const parsed = parsePerpsReleaseConfiguration(release)
const contracts = Object.fromEntries(Object.entries(release.contracts).map(([key, value]) => [key, value.address]))
export const fixturePerpsAddresses = {
  PERPS_RELEASE: parsed,
  PERPS_RELEASE_IS_DEPLOYED: true,
  PERPS_SETTLEMENT_TOKEN: parsed.settlementToken,
  PERPS_SETTLEMENT_TOKEN_LABEL: parsed.settlementToken.symbol,
  PERPS_SETTLEMENT_TOKEN_CONTRACT: settlementTokenContract(parsed),
  PERPS_ARBITRUM_SEPOLIA_CHAIN_ID: 421614,
  PERPS_ARBITRUM_SEPOLIA_DEPLOYMENT_BLOCK: release.release.deploymentBlock,
  PERPS_CLOSE_PREVIEW_ADDRESS: release.contracts.cfdClosePreview.address,
  PERPS_ARBITRUM_SEPOLIA: { ...contracts, pyth: release.release.pyth, policyEvaluator: contracts.cfdOrderPolicyEvaluator },
}
