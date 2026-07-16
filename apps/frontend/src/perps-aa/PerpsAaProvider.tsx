import { useMemo, type ReactNode } from 'react'
import { isAddressEqual } from 'viem'
import {
  PERPS_ARBITRUM_SEPOLIA,
  PERPS_ARBITRUM_SEPOLIA_CHAIN_ID,
} from '../contracts/perpsAddresses'
import {
  WagmiPerpsIdentityProvider,
  type PerpsAccountAddressResolver,
} from './PerpsIdentityProvider'
import {
  PerpsAaRuntimeProvider,
} from './runtime'
import type { PerpsAaSmartAccountRuntime } from './runtimeContext'
import { SponsoredOperationRecovery } from './SponsoredOperationRecovery'

function configuredManifestUrl(): string | null {
  const value: unknown = import.meta.env.VITE_PERPS_AA_MANIFEST_URL
  return typeof value === 'string' && value.trim() !== ''
    ? value.trim()
    : null
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
  const accountAddressResolver = useMemo<
    PerpsAccountAddressResolver | undefined
  >(() => {
    if (!runtime) return undefined

    return ({ ownerAddress, chainId, manifest, signal }) => {
      signal.throwIfAborted()
      const factoryMatches =
        (
          runtime.factoryAddress === null &&
          manifest.smartAccountFactory === null
        ) ||
        (
          runtime.factoryAddress !== null &&
          manifest.smartAccountFactory !== null &&
          isAddressEqual(
            runtime.factoryAddress,
            manifest.smartAccountFactory
          )
        )
      if (
        runtime.chainId !== chainId ||
        runtime.chainId !== manifest.chainId ||
        manifest.chainId !== PERPS_ARBITRUM_SEPOLIA_CHAIN_ID ||
        !isAddressEqual(runtime.ownerAddress, ownerAddress) ||
        !factoryMatches ||
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
          runtime.smartAccount.entryPoint,
          manifest.entryPoint
        ) ||
        !isAddressEqual(
          runtime.implementationAddress,
          manifest.smartAccountImplementation
        ) ||
        runtime.accountRuntimeCodeHash.toLowerCase() !==
          manifest.accountRuntimeCodeHash.toLowerCase()
      ) {
        throw new Error(
          'Smart-account runtime owner, chain, or deployment metadata does not match the reviewed manifest'
        )
      }
      return {
        accountAddress: runtime.smartAccount.accountAddress,
        implementationVersion: runtime.implementationVersion,
      }
    }
  }, [runtime])

  return (
    <PerpsAaRuntimeProvider runtime={runtime}>
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
