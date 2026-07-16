import { createContext } from 'react'
import type { Address } from 'viem'
import type {
  PerpsIdentityField,
  PersistedPerpsIdentity,
} from './identityPersistence'
import type { PerpsAaDeploymentManifest } from './manifest'

export type PerpsIdentityStatus =
  | 'disconnected'
  | 'loading'
  | 'selection-required'
  | 'ready'
  | 'continuity-required'
  | 'blocked'

export type PerpsIdentityErrorCode =
  | 'ACCOUNT_RESOLVER_MISSING'
  | 'MANIFEST_LOAD_FAILED'
  | 'CHAIN_MISMATCH'
  | 'ACCOUNT_RESOLUTION_FAILED'
  | 'IDENTITY_STORAGE_INVALID'
  | 'IDENTITY_STORAGE_UNAVAILABLE'
  | 'IDENTITY_PERSIST_FAILED'
  | 'SPONSORSHIP_MANIFEST_REQUIRED'

export interface PerpsIdentityError {
  code: PerpsIdentityErrorCode
  message: string
}

export interface PerpsIdentityContextValue {
  status: PerpsIdentityStatus
  ownerAddress: Address | undefined
  accountAddress: Address | undefined
  chainId: number | undefined
  isAaManifestConfigured: boolean
  sponsorshipEnabled: boolean
  manifest: PerpsAaDeploymentManifest | null
  identity: PersistedPerpsIdentity | null
  proposedIdentity: PersistedPerpsIdentity | null
  changedIdentityFields: readonly PerpsIdentityField[]
  error: PerpsIdentityError | null
  confirmIdentityAfterContinuityCheck: () => boolean
  reloadIdentity: () => void
}

export const PerpsIdentityContext = createContext<
  PerpsIdentityContextValue | undefined
>(undefined)
