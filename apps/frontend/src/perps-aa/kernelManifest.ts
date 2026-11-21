import { PERPS_ARBITRUM_SEPOLIA, PERPS_RELEASE, PERPS_SETTLEMENT_TOKEN } from '../contracts/perpsAddresses'
import { getExplorerTxUrl, getExplorerUserOperationUrl } from '../utils/explorer'
import { kernelSepoliaCanaryProfile } from './kernelDeploymentPins'
import { fetchKernelManifest } from './kernelRpc'
import { KERNEL_MANIFEST_VERSION_PREFIX, type KernelPerpsAaDeploymentManifest } from './manifest'

export async function loadKernelTradingManifest(deployment: string, signal: AbortSignal): Promise<KernelPerpsAaDeploymentManifest> {
  if (deployment !== kernelSepoliaCanaryProfile.deployment || !PERPS_RELEASE.isDeployed) {
    throw new Error('No reviewed Kernel deployment is configured')
  }
  const kernel = await fetchKernelManifest(signal)
  const { profile } = kernel
  if (profile.deployment !== deployment) {
    throw new Error('Kernel gateway does not match the configured deployment')
  }
  return {
    version: `${KERNEL_MANIFEST_VERSION_PREFIX}${deployment}`,
    kernel,
    orderInterfaceVersion: 3,
    chainId: profile.chainId,
    entryPoint: profile.entryPoint,
    entryPointVersion: '0.9',
    smartAccountMode: 'kernel',
    smartAccountVersion: 'kernel-v4-uups',
    smartAccountIndex: '0',
    smartAccountFactory: profile.factory,
    settlementToken: kernel.settlementToken,
    settlementTokenSupportsEip3009: PERPS_SETTLEMENT_TOKEN.supportsReceiveWithAuthorization,
    settlementTokenEip712Name: null,
    settlementTokenEip712Version: null,
    marginClearinghouse: kernel.marginClearinghouse,
    cfdEngine: kernel.cfdEngine,
    orderRouter: kernel.orderRouter,
    orderLifecycleBook: PERPS_ARBITRUM_SEPOLIA.orderLifecycleBook,
    positionProtectionBook: kernel.positionProtectionBook,
    policyEvaluator: PERPS_ARBITRUM_SEPOLIA.policyEvaluator,
    userOperationExplorerUrlTemplate: getExplorerUserOperationUrl(profile.chainId, '{userOperationHash}'),
    transactionExplorerUrlTemplate: getExplorerTxUrl(profile.chainId, '{transactionHash}'),
    testnetFaucet: null,
    sponsorshipEnabled: kernel.readiness.sponsored,
  }
}
