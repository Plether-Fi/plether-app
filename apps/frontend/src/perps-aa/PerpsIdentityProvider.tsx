import {
  useCallback,
  useEffect,
  useMemo,
  useState,
  type ReactNode,
} from 'react'
import { getAddress, isAddress, isAddressEqual, type Address } from 'viem'
import { useAccount, usePublicClient } from 'wagmi'
import { useWalletActions } from '../wallet/useWalletActions'
import { UnsupportedEmbeddedOwnerError } from './embeddedOwner'
import { UnsupportedKernelOwnerError } from './kernelAccount'
import { kernelTestMounted, kernelTestOwnerAllowed } from './kernelTestMount'
import { verifyPerpsV3DeploymentBindings } from '../contracts/verifyPerpsV3Bindings'
import { PERPS_ARBITRUM_SEPOLIA_CHAIN_ID } from '../contracts/perpsAddresses'
import {
  comparePerpsIdentities,
  createPersistedPerpsIdentity,
  readPersistedPerpsIdentity,
  writePersistedPerpsIdentity,
  type PerpsIdentityField,
  type PerpsIdentityStorage,
  type PersistedPerpsIdentity,
} from './identityPersistence'
import {
  fetchPerpsAaManifest,
  KERNEL_DEPLOYMENT_PATTERN,
  type PerpsAaDeploymentManifest,
  type PerpsAaManifestFetch,
  type PerpsEntryPointVersion,
} from './manifest'
import {
  PerpsIdentityContext,
  type PerpsIdentityContextValue,
  type PerpsIdentityError,
  type PerpsIdentityErrorCode,
} from './PerpsIdentityContext'

export interface ResolvedPerpsAccount {
  accountAddress: Address
  accountVersion: string
  accountIndex: string
  entryPoint: Address
  entryPointVersion: PerpsEntryPointVersion
  factoryAddress: Address
}

export type PerpsAccountAddressResolver = (input: {
  ownerAddress: Address
  chainId: number
  manifest: PerpsAaDeploymentManifest
  signal: AbortSignal
}) => ResolvedPerpsAccount | Promise<ResolvedPerpsAccount>

export type PerpsDeploymentVerifier = (input: {
  manifest: PerpsAaDeploymentManifest
  signal: AbortSignal
}) => void | Promise<void>

type PerpsManifestLoader = (signal: AbortSignal) => Promise<PerpsAaDeploymentManifest>

interface AsyncIdentityResolution {
  status:
    | 'selection-required'
    | 'ready'
    | 'continuity-required'
    | 'blocked'
  manifest: PerpsAaDeploymentManifest | null
  identity: PersistedPerpsIdentity | null
  proposedIdentity: PersistedPerpsIdentity | null
  changedIdentityFields: readonly PerpsIdentityField[]
  error: PerpsIdentityError | null
}

interface PerpsIdentityProviderProps {
  children: ReactNode
  ownerAddress?: Address
  chainId?: number
  manifestUrl?: string | null
  accountAddressResolver?: PerpsAccountAddressResolver
  deploymentVerifier?: PerpsDeploymentVerifier
  refreshIntervalMs?: number | false
  storage?: PerpsIdentityStorage | null
  fetch?: PerpsAaManifestFetch
  loadManifest?: PerpsManifestLoader
  kernelDeployment?: string
}

type WagmiPerpsIdentityProviderProps = Omit<
  PerpsIdentityProviderProps,
  'ownerAddress' | 'chainId' | 'deploymentVerifier'
>

const asyncInputIdentityIds = new WeakMap<object, number>()
let nextAsyncInputIdentityId = 1

function asyncInputIdentity(value: object | null | undefined): string {
  if (value === undefined) return 'undefined'
  if (value === null) return 'null'

  const existing = asyncInputIdentityIds.get(value)
  if (existing !== undefined) return existing.toString()

  const identity = nextAsyncInputIdentityId
  nextAsyncInputIdentityId += 1
  asyncInputIdentityIds.set(value, identity)
  return identity.toString()
}

function blockedResolution(
  code: PerpsIdentityErrorCode,
  message: string,
  manifest: PerpsAaDeploymentManifest | null = null
): AsyncIdentityResolution {
  return {
    status: 'blocked',
    manifest,
    identity: null,
    proposedIdentity: null,
    changedIdentityFields: [],
    error: { code, message },
  }
}

function getBrowserStorage(
  configuredStorage: PerpsIdentityStorage | null | undefined
): PerpsIdentityStorage | null {
  if (configuredStorage !== undefined) return configuredStorage
  try {
    return globalThis.localStorage
  } catch {
    return null
  }
}

async function resolveConfiguredIdentity(input: {
  manifestUrl: string
  loadManifest?: PerpsManifestLoader
  ownerAddress: Address
  chainId: number
  accountAddressResolver: PerpsAccountAddressResolver
  deploymentVerifier?: PerpsDeploymentVerifier
  storage: PerpsIdentityStorage | null
  fetch?: PerpsAaManifestFetch
  signal: AbortSignal
}): Promise<AsyncIdentityResolution> {
  if (kernelTestMounted && !kernelTestOwnerAllowed(input.ownerAddress, import.meta.env.VITE_PERPS_KERNEL_TEST_OWNERS)) {
    return blockedResolution('UNSUPPORTED_OWNER_WALLET', 'Connect one of the two wallets enrolled in the Sepolia Kernel test.')
  }
  let manifest: PerpsAaDeploymentManifest
  try {
    manifest = input.loadManifest
      ? await input.loadManifest(input.signal)
      : await fetchPerpsAaManifest(input.manifestUrl, { fetch: input.fetch, signal: input.signal })
  } catch {
    return blockedResolution(
      'MANIFEST_LOAD_FAILED',
      'The Trading Account deployment manifest could not be validated.'
    )
  }

  if (manifest.chainId !== input.chainId) {
    return blockedResolution(
      'CHAIN_MISMATCH',
      `The connected chain (${String(input.chainId)}) does not match the sponsorship manifest (${String(manifest.chainId)}).`,
      manifest
    )
  }

  if (input.deploymentVerifier !== undefined) {
    try {
      await input.deploymentVerifier({ manifest, signal: input.signal })
    } catch {
      return blockedResolution(
        'DEPLOYMENT_BINDING_MISMATCH',
        'Trading is blocked because the deployed Router, Engine, Clearinghouse, Pool, lifecycle book, or policy evaluator binding does not match the reviewed manifest.',
        manifest
      )
    }
  }

  let resolvedAccount: ResolvedPerpsAccount
  try {
    resolvedAccount = await input.accountAddressResolver({
      ownerAddress: input.ownerAddress,
      chainId: input.chainId,
      manifest,
      signal: input.signal,
    })
  } catch (error) {
    if (error instanceof UnsupportedEmbeddedOwnerError || error instanceof UnsupportedKernelOwnerError) {
      return blockedResolution('UNSUPPORTED_OWNER_WALLET', error.message, manifest)
    }
    return blockedResolution(
      'ACCOUNT_RESOLUTION_FAILED',
      'The reviewed trading account address could not be resolved.',
      manifest
    )
  }

  if (
    !isAddress(resolvedAccount.accountAddress) ||
    resolvedAccount.accountVersion.trim() === '' ||
    !/^(0|[1-9][0-9]*)$/.test(resolvedAccount.accountIndex) ||
    !isAddress(resolvedAccount.entryPoint) ||
    !isAddress(resolvedAccount.factoryAddress)
  ) {
    return blockedResolution(
      'ACCOUNT_RESOLUTION_FAILED',
      'The trading account resolver returned invalid identity metadata.',
      manifest
    )
  }

  const accountAddress = getAddress(resolvedAccount.accountAddress)
  const sameAddress = isAddressEqual(input.ownerAddress, accountAddress)
  if (sameAddress) {
    return blockedResolution(
      'ACCOUNT_RESOLUTION_FAILED',
      'The trading account address is inconsistent with the reviewed account mode.',
      manifest
    )
  }

  let proposedIdentity: PersistedPerpsIdentity
  try {
    proposedIdentity = createPersistedPerpsIdentity({
      chainId: input.chainId,
      ownerAddress: input.ownerAddress,
      accountAddress,
      accountMode: manifest.smartAccountMode,
      entryPoint: resolvedAccount.entryPoint,
      entryPointVersion: resolvedAccount.entryPointVersion,
      factoryAddress: resolvedAccount.factoryAddress,
      accountVersion: resolvedAccount.accountVersion,
      accountIndex: resolvedAccount.accountIndex,
      manifestVersion: manifest.version,
    })
  } catch {
    return blockedResolution(
      'ACCOUNT_RESOLUTION_FAILED',
      'The resolved trading account identity is invalid.',
      manifest
    )
  }

  if (input.storage === null) {
    return blockedResolution(
      'IDENTITY_STORAGE_UNAVAILABLE',
      'Trading account continuity cannot be verified because identity storage is unavailable.',
      manifest
    )
  }

  const persisted = readPersistedPerpsIdentity(
    input.storage,
    input.chainId,
    input.ownerAddress,
    manifest.smartAccountMode === 'kernel' ? manifest.kernel.profile.deployment : undefined,
  )
  if (persisted.status === 'unavailable') {
    return blockedResolution(
      'IDENTITY_STORAGE_UNAVAILABLE',
      'Trading account continuity cannot be verified because identity storage is unavailable.',
      manifest
    )
  }
  if (persisted.status === 'invalid') {
    return blockedResolution(
      'IDENTITY_STORAGE_INVALID',
      'The saved trading account identity is invalid and must be reviewed before continuing.',
      manifest
    )
  }
  if (persisted.status === 'missing') {
    const writeResult = writePersistedPerpsIdentity(
      input.storage,
      proposedIdentity
    )
    if (!writeResult.ok) {
      return blockedResolution(
        'IDENTITY_PERSIST_FAILED',
        'The derived trading account identity could not be saved.',
        manifest
      )
    }
    return {
      status: 'ready',
      manifest,
      identity: proposedIdentity,
      proposedIdentity: null,
      changedIdentityFields: [],
      error: null,
    }
  }

  const continuity = comparePerpsIdentities(
    persisted.identity,
    proposedIdentity
  )
  if (!continuity.matches) {
    if (manifest.smartAccountMode === 'kernel') {
      return blockedResolution(
        'ACCOUNT_RESOLUTION_FAILED',
        'The Trading Account binding changed for this Kernel deployment. Trading is paused until the deployment is verified.',
        manifest,
      )
    }
    const writeResult = writePersistedPerpsIdentity(
      input.storage,
      proposedIdentity
    )
    if (!writeResult.ok) {
      return blockedResolution(
        'IDENTITY_PERSIST_FAILED',
        'The updated trading account identity could not be saved.',
        manifest
      )
    }
    return {
      status: 'ready',
      manifest,
      identity: proposedIdentity,
      proposedIdentity: null,
      changedIdentityFields: [],
      error: null,
    }
  }

  return {
    status: 'ready',
    manifest,
    identity: persisted.identity,
    proposedIdentity: null,
    changedIdentityFields: [],
    error: null,
  }
}

export function PerpsIdentityProvider({
  children,
  ownerAddress,
  chainId,
  manifestUrl,
  accountAddressResolver,
  deploymentVerifier,
  refreshIntervalMs = 30_000,
  storage: configuredStorage,
  fetch,
  loadManifest,
  kernelDeployment,
}: PerpsIdentityProviderProps) {
  const normalizedManifestUrl = manifestUrl?.trim() ?? ''
  const isAaManifestConfigured = loadManifest !== undefined || normalizedManifestUrl !== ''
  const storage = getBrowserStorage(configuredStorage)
  const [reloadCount, setReloadCount] = useState(0)
  const identityKey = [
    normalizedManifestUrl,
    chainId ?? 'no-chain',
    ownerAddress?.toLowerCase() ?? 'no-owner',
    asyncInputIdentity(accountAddressResolver),
    asyncInputIdentity(deploymentVerifier),
    asyncInputIdentity(storage),
    asyncInputIdentity(fetch),
    asyncInputIdentity(loadManifest),
  ].join(':')
  const requestKey = `${identityKey}:${reloadCount.toString()}`
  const [asyncResolution, setAsyncResolution] = useState<{
    identityKey: string
    requestKey: string
    value: AsyncIdentityResolution
  } | null>(null)

  useEffect(() => {
    if (
      !isAaManifestConfigured ||
      accountAddressResolver === undefined ||
      ownerAddress === undefined ||
      chainId === undefined
    ) {
      return
    }

    const abortController = new AbortController()
    void resolveConfiguredIdentity({
      manifestUrl: normalizedManifestUrl,
      ownerAddress,
      chainId,
      accountAddressResolver,
      deploymentVerifier,
      storage,
      fetch,
      loadManifest,
      signal: abortController.signal,
    }).then((value) => {
      if (!abortController.signal.aborted) {
        setAsyncResolution({ identityKey, requestKey, value })
      }
    })

    return () => {
      abortController.abort()
    }
  }, [
    accountAddressResolver,
    deploymentVerifier,
    chainId,
    fetch,
    identityKey,
    isAaManifestConfigured,
    loadManifest,
    normalizedManifestUrl,
    ownerAddress,
    requestKey,
    storage,
  ])

  const reloadIdentity = useCallback(() => {
    setReloadCount((current) => current + 1)
  }, [setReloadCount])

  useEffect(() => {
    if (
      !isAaManifestConfigured ||
      refreshIntervalMs === false ||
      refreshIntervalMs <= 0
    ) {
      return
    }

    let interval: ReturnType<typeof globalThis.setInterval> | undefined

    const stopInterval = () => {
      if (interval === undefined) return
      globalThis.clearInterval(interval)
      interval = undefined
    }

    const startInterval = () => {
      stopInterval()
      interval = globalThis.setInterval(reloadIdentity, refreshIntervalMs)
    }

    if (typeof document === 'undefined') {
      startInterval()
      return stopInterval
    }

    const handleVisibilityChange = () => {
      if (document.visibilityState === 'hidden') {
        stopInterval()
        return
      }

      // A deployment or sponsorship kill-switch may have changed while this
      // tab was suspended. Revalidate immediately, then keep the usual cadence
      // for as long as the document remains visible.
      reloadIdentity()
      startInterval()
    }

    document.addEventListener('visibilitychange', handleVisibilityChange)
    if (document.visibilityState !== 'hidden') {
      startInterval()
    }

    return () => {
      document.removeEventListener('visibilitychange', handleVisibilityChange)
      stopInterval()
    }
  }, [isAaManifestConfigured, refreshIntervalMs, reloadIdentity])

  const currentResolution = asyncResolution?.identityKey === identityKey
    ? asyncResolution.value
    : null

  const confirmIdentityAfterContinuityCheck = useCallback((): boolean => {
    if (
      storage === null ||
      currentResolution?.proposedIdentity === null ||
      currentResolution?.proposedIdentity === undefined
    ) {
      return false
    }

    const result = writePersistedPerpsIdentity(
      storage,
      currentResolution.proposedIdentity
    )
    if (!result.ok) {
      setAsyncResolution({
        identityKey,
        requestKey,
        value: blockedResolution(
          'IDENTITY_PERSIST_FAILED',
          'The selected trading account identity could not be saved.',
          currentResolution.manifest
        ),
      })
      return false
    }

    setAsyncResolution({
      identityKey,
      requestKey,
      value: {
        status: 'ready',
        manifest: currentResolution.manifest,
        identity: currentResolution.proposedIdentity,
        proposedIdentity: null,
        changedIdentityFields: [],
        error: null,
      },
    })
    return true
  }, [currentResolution, identityKey, requestKey, storage])

  const contextValue = useMemo<PerpsIdentityContextValue>(() => {
    const settings: Pick<PerpsIdentityContextValue, 'confirmIdentityAfterContinuityCheck' | 'reloadIdentity' | 'kernelGasPreferenceScope'> = {
      confirmIdentityAfterContinuityCheck,
      reloadIdentity,
    }
    if (kernelDeployment !== undefined && KERNEL_DEPLOYMENT_PATTERN.test(kernelDeployment) && ownerAddress !== undefined) {
      const kernelChainId = PERPS_ARBITRUM_SEPOLIA_CHAIN_ID
      const saved = storage && readPersistedPerpsIdentity(storage, kernelChainId, ownerAddress, kernelDeployment)
      settings.kernelGasPreferenceScope = {
        chainId: kernelChainId,
        deployment: kernelDeployment,
        ownerAddress,
        ...(saved?.status === 'found' ? { sender: saved.identity.accountAddress } : {}),
      }
    }
    if (!isAaManifestConfigured) {
      if (ownerAddress === undefined || chainId === undefined) {
        return {
          status: 'disconnected',
          ownerAddress,
          accountAddress: undefined,
          chainId,
          isAaManifestConfigured: true,
          sponsorshipEnabled: false,
          manifest: null,
          identity: null,
          proposedIdentity: null,
          changedIdentityFields: [],
          error: null,
          ...settings,
        }
      }

      return {
        status: 'blocked',
        ownerAddress,
        accountAddress: undefined,
        chainId,
        isAaManifestConfigured: true,
        sponsorshipEnabled: false,
        manifest: null,
        identity: null,
        proposedIdentity: null,
        changedIdentityFields: [],
        error: {
          code: 'SPONSORSHIP_MANIFEST_REQUIRED',
          message: 'Perps is sponsorship-only on testnet. A reviewed gas-sponsorship manifest is required before any Trading Account action can be used.',
        },
        ...settings,
      }
    }

    if (accountAddressResolver === undefined) {
      return {
        status: 'blocked',
        ownerAddress,
        accountAddress: undefined,
        chainId,
        isAaManifestConfigured: true,
        sponsorshipEnabled: false,
        manifest: null,
        identity: null,
        proposedIdentity: null,
        changedIdentityFields: [],
        error: {
          code: 'ACCOUNT_RESOLVER_MISSING',
          message: 'Gas sponsorship is configured without a reviewed trading account resolver.',
        },
        ...settings,
      }
    }

    if (ownerAddress === undefined || chainId === undefined) {
      return {
        status: 'disconnected',
        ownerAddress,
        accountAddress: undefined,
        chainId,
        isAaManifestConfigured: true,
        sponsorshipEnabled: false,
        manifest: null,
        identity: null,
        proposedIdentity: null,
        changedIdentityFields: [],
        error: null,
        ...settings,
      }
    }

    if (currentResolution === null) {
      return {
        status: 'loading',
        ownerAddress,
        accountAddress: undefined,
        chainId,
        isAaManifestConfigured: true,
        sponsorshipEnabled: false,
        manifest: null,
        identity: null,
        proposedIdentity: null,
        changedIdentityFields: [],
        error: null,
        ...settings,
      }
    }

    return {
      status: currentResolution.status,
      ownerAddress,
      accountAddress: currentResolution.status === 'ready'
        ? currentResolution.identity?.accountAddress
        : undefined,
      chainId,
      isAaManifestConfigured: true,
      sponsorshipEnabled:
        currentResolution.manifest?.sponsorshipEnabled === true,
      manifest: currentResolution.manifest,
      identity: currentResolution.identity,
      proposedIdentity: currentResolution.proposedIdentity,
      changedIdentityFields: currentResolution.changedIdentityFields,
      error: currentResolution.error,
      ...settings,
    }
  }, [
    accountAddressResolver,
    chainId,
    confirmIdentityAfterContinuityCheck,
    currentResolution,
    isAaManifestConfigured,
    kernelDeployment,
    ownerAddress,
    reloadIdentity,
    storage,
  ])

  return (
    <PerpsIdentityContext value={contextValue}>
      {children}
    </PerpsIdentityContext>
  )
}

export function WagmiPerpsIdentityProvider(
  props: WagmiPerpsIdentityProviderProps
) {
  const { address: ownerAddress, chainId } = useAccount()
  const wallet = useWalletActions()
  const signerReady = !wallet.configured || wallet.signerReady
  const publicClient = usePublicClient()
  const deploymentVerifier = useCallback<PerpsDeploymentVerifier>(async ({
    manifest,
    signal,
  }) => {
    if (signal.aborted) return
    await verifyPerpsV3DeploymentBindings(publicClient, manifest)
  }, [publicClient])
  return (
    <PerpsIdentityProvider
      {...props}
      ownerAddress={signerReady ? ownerAddress : undefined}
      chainId={chainId}
      deploymentVerifier={deploymentVerifier}
    />
  )
}
