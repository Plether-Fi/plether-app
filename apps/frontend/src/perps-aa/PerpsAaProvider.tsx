import {
  useCallback,
  useMemo,
  useRef,
  useState,
  type ReactNode,
} from 'react'
import { isAddressEqual } from 'viem'
import { usePublicClient, useWalletClient } from 'wagmi'
import {
  PERPS_ARBITRUM_SEPOLIA,
  PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
} from '../contracts/perpsAddresses'
import { isSepoliaDeployment } from '../utils/deployment'
import {
  WagmiPerpsIdentityProvider,
  type PerpsAccountAddressResolver,
} from './PerpsIdentityProvider'
import { resolvePerpsAaManifestUrl } from './manifestUrl'
import {
  PerpsAaRuntimeProvider,
} from './runtime'
import type { PerpsAaSmartAccountRuntime } from './runtimeContext'
import { SponsoredOperationRecovery } from './SponsoredOperationRecovery'
import { createManagedPimlicoRuntime } from './managedPimlicoRuntime'

function configuredManifestUrl(): string | null {
  const value: unknown = import.meta.env.VITE_PERPS_AA_MANIFEST_URL
  return resolvePerpsAaManifestUrl(value, isSepoliaDeployment())
}

export function PerpsAaProvider({
  children,
  manifestUrl = configuredManifestUrl(),
  runtime,
}: {
  children: ReactNode
  manifestUrl?: string | null
  runtime?: PerpsAaSmartAccountRuntime
}) {
  const publicClient = usePublicClient()
  const { data: walletClient } = useWalletClient()
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

      const key = [
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
        manifest.pimlicoRpcUrl,
      ].join(':')
      let cacheEntry = runtimeCache.current
      if (cacheEntry?.key !== key) {
        const promise = createManagedPimlicoRuntime({
          manifest,
          ownerAddress,
          walletClient,
          publicClient,
        })
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
      setResolvedRuntime({ connectionKey, runtime: nextRuntime })
      return nextRuntime
    },
    [connectionKey, publicClient, runtime, walletClient]
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
        !isAddressEqual(manifest.usdc, PERPS_ARBITRUM_SEPOLIA.usdc) ||
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
      return {
        accountAddress: nextRuntime.smartAccount.accountAddress,
        accountVersion: nextRuntime.accountVersion,
        accountIndex: nextRuntime.accountIndex,
        entryPoint: nextRuntime.smartAccount.entryPoint,
        entryPointVersion: '0.8',
        factoryAddress: nextRuntime.factoryAddress,
      }
    }
  }, [getRuntime])

  return (
    <PerpsAaRuntimeProvider runtime={effectiveRuntime}>
      <WagmiPerpsIdentityProvider
        manifestUrl={manifestUrl}
        accountAddressResolver={accountAddressResolver}
      >
        <SponsoredOperationRecovery />
        {children}
      </WagmiPerpsIdentityProvider>
    </PerpsAaRuntimeProvider>
  )
}
