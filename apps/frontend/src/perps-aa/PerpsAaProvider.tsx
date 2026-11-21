import { withCloseAssistanceRecovery } from './closeAssistanceRecovery'
import { lazyWithRetry as lazy } from '../utils/lazyWithRetry'
import { useSponsoredOperationStore } from './operationStore'
import { closeAssistanceManifest } from './sponsoredClose'
import {
  Suspense,
  useCallback,
  useMemo,
  useRef,
  useState,
  type ReactNode,
} from 'react'
import { isAddressEqual } from 'viem'
import { usePublicClient, useWalletClient } from 'wagmi'
import { useWalletActions } from '../wallet/useWalletActions'
import {
  PERPS_ARBITRUM_SEPOLIA,
  PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
  PERPS_RELEASE_IS_DEPLOYED,
} from '../contracts/perpsAddresses'
import { isSepoliaDeployment } from '../utils/deployment'
import {
  WagmiPerpsIdentityProvider,
  type PerpsAccountAddressResolver,
} from './PerpsIdentityProvider'
import { resolvePerpsAaManifestUrl } from './manifestUrl'
import {
  bundlerRpcUrlForManifest,
  isPerpsAaManifestV2 as isNativePaymasterManifest,
  paymasterRpcUrlForManifest,
} from './manifest'
import {
  PerpsAaRuntimeProvider,
} from './runtime'
import type { PerpsAaSmartAccountRuntime } from './runtimeContext'
import { usePerpsIdentity } from './usePerpsIdentity'

const SponsoredOperationRecovery = lazy(async () => {
  const module = await import('./SponsoredOperationRecovery')
  return { default: module.SponsoredOperationRecovery }
})
const KernelOperationRecovery = lazy(async () => {
  const module = await import('./useKernelAttempts')
  return { default: module.KernelOperationRecovery }
})

function configuredManifestUrl(): string | null {
  if (!PERPS_RELEASE_IS_DEPLOYED) return null
  const value: unknown = import.meta.env.VITE_PERPS_AA_MANIFEST_URL
  return resolvePerpsAaManifestUrl(value, isSepoliaDeployment())
}

function configuredKernelDeployment(): string | undefined {
  const value: unknown = import.meta.env.VITE_PERPS_KERNEL_AA_DEPLOYMENT
  return typeof value === 'string' && value !== '' ? value : undefined
}

function ConnectedPerpsAaRuntime({
  runtime, kernelDeployment, children,
}: {
  runtime: PerpsAaSmartAccountRuntime | undefined
  kernelDeployment: string | undefined
  children: ReactNode
}) {
  const identity = usePerpsIdentity()
  const effectiveRuntime = runtime?.kernel && (
    identity.status !== 'ready' || identity.chainId !== runtime.chainId ||
    identity.manifest?.smartAccountMode !== 'kernel' ||
    identity.manifest.version !== runtime.manifestVersion ||
    identity.manifest.kernel.profile.deployment !== runtime.kernel.scope.deployment ||
    !identity.ownerAddress || !isAddressEqual(identity.ownerAddress, runtime.ownerAddress) ||
    !identity.accountAddress || !isAddressEqual(identity.accountAddress, runtime.smartAccount.accountAddress)
  ) ? undefined : runtime

  return (
    <PerpsAaRuntimeProvider runtime={effectiveRuntime}>
      <Suspense fallback={null}>
        {PERPS_RELEASE_IS_DEPLOYED && (effectiveRuntime?.kernel
          ? <KernelOperationRecovery scope={effectiveRuntime.kernel.scope} />
          : kernelDeployment === undefined && <SponsoredOperationRecovery />)}
      </Suspense>
      {children}
    </PerpsAaRuntimeProvider>
  )
}

export function PerpsAaProvider({
  children,
  manifestUrl = configuredManifestUrl(),
  kernelDeployment = configuredKernelDeployment(),
  runtime,
}: {
  children: ReactNode
  manifestUrl?: string | null
  kernelDeployment?: string
  runtime?: PerpsAaSmartAccountRuntime
}) {
  const publicClient = usePublicClient()
  const { data: connectedWalletClient } = useWalletClient()
  const wallet = useWalletActions()
  const walletClient = wallet.configured && !wallet.signerReady ? undefined : connectedWalletClient
  const [resolvedRuntime, setResolvedRuntime] = useState<{
    connectionKey: string
    runtime: PerpsAaSmartAccountRuntime
  }>()
  const runtimeCache = useRef<{
    key: string
    promise: Promise<PerpsAaSmartAccountRuntime>
  } | undefined>(undefined)
  const connectionKey = walletClient
    ? [
        publicClient.uid,
        walletClient.uid,
        publicClient.chain.id,
        walletClient.chain.id,
        walletClient.account.address.toLowerCase(),
        kernelDeployment ?? 'simple',
      ].join(':')
    : 'wallet-unavailable'

  const getRuntime = useCallback(
    async ({
      ownerAddress,
      chainId,
      manifest,
      signal,
    }: Parameters<PerpsAccountAddressResolver>[0]) => {
      if (runtime) return runtime
      if (!walletClient) {
        throw new Error('The connected wallet client is unavailable')
      }

      const key = manifest.smartAccountMode === 'kernel' ? [
        chainId, ownerAddress.toLowerCase(), publicClient.uid, walletClient.uid,
        JSON.stringify(manifest.kernel),
      ].join(':') : [
        chainId,
        ownerAddress.toLowerCase(),
        publicClient.uid,
        walletClient.uid,
        manifest.version,
        manifest.entryPoint.toLowerCase(),
        manifest.entryPointVersion,
        manifest.smartAccountFactory.toLowerCase(),
        manifest.smartAccountVersion,
        manifest.smartAccountIndex,
        bundlerRpcUrlForManifest(manifest),
        paymasterRpcUrlForManifest(manifest),
        isNativePaymasterManifest(manifest)
          ? manifest.paymasterAddress.toLowerCase()
          : 'pimlico-singleton-v8',
        isNativePaymasterManifest(manifest)
          ? manifest.paymasterVersion
          : 'pimlico-rpc-shape',
      ].join(':')
      let cacheEntry = runtimeCache.current
      if (cacheEntry?.key !== key) {
        const promise = manifest.smartAccountMode === 'kernel'
          ? import('./kernelRuntime').then(({ createKernelRuntime }) => createKernelRuntime({ manifest, ownerAddress, walletClient, publicClient }))
          : import('./managedPimlicoRuntime').then(async ({ createManagedAaRuntime }) => withCloseAssistanceRecovery(
              await createManagedAaRuntime({ manifest, ownerAddress, walletClient, publicClient }),
              () => useSponsoredOperationStore.getState().operations,
              config => createManagedAaRuntime({ manifest: closeAssistanceManifest(manifest, config), ownerAddress, walletClient, publicClient }),
              preparedManifest => createManagedAaRuntime({ manifest: preparedManifest, ownerAddress, walletClient, publicClient }),
            ))
        cacheEntry = {
          key,
          promise,
        }
        runtimeCache.current = cacheEntry
      }
      const runtimePromise = cacheEntry.promise
      let nextRuntime: PerpsAaSmartAccountRuntime
      try {
        nextRuntime = await runtimePromise
      } catch (error) {
        if (runtimeCache.current === cacheEntry) {
          runtimeCache.current = undefined
        }
        throw error
      }
      signal.throwIfAborted()
      return nextRuntime
    },
    [publicClient, runtime, walletClient]
  )

  const effectiveRuntime = runtime ??
    (
      resolvedRuntime?.connectionKey === connectionKey
        ? resolvedRuntime.runtime
        : undefined
    )
  const accountAddressResolver = useMemo<
    PerpsAccountAddressResolver
  >(() => {
    return async ({ ownerAddress, chainId, manifest, signal }) => {
      signal.throwIfAborted()
      const nextRuntime = await getRuntime({
        ownerAddress,
        chainId,
        manifest,
        signal,
      })
      if (
        Boolean(nextRuntime.kernel) !== (manifest.smartAccountMode === 'kernel') ||
        nextRuntime.chainId !== chainId ||
        nextRuntime.chainId !== manifest.chainId ||
        manifest.chainId !== PERPS_ARBITRUM_SEPOLIA_CHAIN_ID ||
        !isAddressEqual(nextRuntime.ownerAddress, ownerAddress) ||
        !isAddressEqual(
          nextRuntime.factoryAddress,
          manifest.smartAccountFactory
        ) ||
        nextRuntime.accountVersion !== manifest.smartAccountVersion ||
        nextRuntime.accountIndex !== manifest.smartAccountIndex ||
        !isAddressEqual(manifest.settlementToken, PERPS_ARBITRUM_SEPOLIA.settlementToken) ||
        !isAddressEqual(
          manifest.marginClearinghouse,
          PERPS_ARBITRUM_SEPOLIA.marginClearinghouse
        ) ||
        !isAddressEqual(
          manifest.cfdEngine,
          PERPS_ARBITRUM_SEPOLIA.cfdEngine
        ) ||
        !isAddressEqual(
          manifest.orderRouter,
          PERPS_ARBITRUM_SEPOLIA.orderRouter
        ) ||
        !isAddressEqual(
          nextRuntime.smartAccount.entryPoint,
          manifest.entryPoint
        )
      ) {
        throw new Error(
          'Smart-account runtime owner, chain, or deployment metadata does not match the reviewed manifest'
        )
      }
      setResolvedRuntime({ connectionKey, runtime: nextRuntime })
      return {
        accountAddress: nextRuntime.smartAccount.accountAddress,
        accountVersion: nextRuntime.accountVersion,
        accountIndex: nextRuntime.accountIndex,
        entryPoint: nextRuntime.smartAccount.entryPoint,
        entryPointVersion: manifest.entryPointVersion,
        factoryAddress: nextRuntime.factoryAddress,
      }
    }
  }, [connectionKey, getRuntime])

  const loadManifest = useMemo(() => kernelDeployment === undefined
    ? undefined
    : async (signal: AbortSignal) => (await import('./kernelManifest')).loadKernelTradingManifest(kernelDeployment, signal), [kernelDeployment])

  return (
    <WagmiPerpsIdentityProvider
      manifestUrl={manifestUrl}
      loadManifest={loadManifest}
      kernelDeployment={kernelDeployment}
      accountAddressResolver={accountAddressResolver}
    >
      <ConnectedPerpsAaRuntime runtime={effectiveRuntime} kernelDeployment={kernelDeployment}>
        {children}
      </ConnectedPerpsAaRuntime>
    </WagmiPerpsIdentityProvider>
  )
}
