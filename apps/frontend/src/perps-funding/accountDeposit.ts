import { PERPS_ACTIVE_DEPLOYMENT, isPerpsManifestForActiveDeployment } from '../contracts/perpsAddresses'
import type { PerpsIdentityContextValue } from '../perps-aa/PerpsIdentityContext'
import { sameAddress } from './validation'
import type { FundingAccountDepositDestination } from './types'

/** A fallback deposit uses only the active, authenticated Trading Account. */
export function assertFundingAccountDepositIdentity(destination: FundingAccountDepositDestination, identity: PerpsIdentityContextValue): void {
  if (identity.status !== 'ready' || !identity.ownerAddress || !identity.accountAddress || !identity.manifest || !isPerpsManifestForActiveDeployment(identity.manifest) || !sameAddress(identity.ownerAddress, destination.owner) || !sameAddress(identity.accountAddress, destination.beneficiary) || identity.chainId !== destination.destinationChainId || PERPS_ACTIVE_DEPLOYMENT.chainId !== destination.destinationChainId || PERPS_ACTIVE_DEPLOYMENT.releaseId !== destination.releaseId || !sameAddress(PERPS_ACTIVE_DEPLOYMENT.contracts.usdc, destination.token) || !sameAddress(PERPS_ACTIVE_DEPLOYMENT.contracts.marginClearinghouse, destination.clearinghouse)) throw new Error('Return to the original Trading Account and reviewed destination release before depositing these funds.')
}
