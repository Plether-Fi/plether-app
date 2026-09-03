import { isAddressEqual, type Hex } from 'viem'
import type { SponsoredOperation } from './operationStore'
import {
  authorityBoundSponsorshipValidUntil,
  pimlicoSponsorshipValidUntil,
} from './paymasterValidity'
import { readPersistedManagedUserOperation } from './persistedUserOperation'
import type {
  ManagedUserOperation,
  PerpsAaSmartAccountRuntime,
} from './runtimeContext'

export type ProtocolOperationResolution =
  | {
      status: 'confirmed' | 'execution-reverted'
      transactionHash: Hex
    }
  | {
      status: 'expired'
    }
  | {
      status: 'outcome-unknown'
      protocolNonceAdvanced: true
    }

function verifiedPersistedUserOperation(
  operation: SponsoredOperation,
  runtime: PerpsAaSmartAccountRuntime,
  userOperationHash: Hex
): ManagedUserOperation | undefined {
  if (operation.submissionMetadataVersion !== 1) {
    return undefined
  }
  const signedOperation = readPersistedManagedUserOperation(
    operation.signedUserOperation
  )
  if (
    !signedOperation ||
    !isAddressEqual(
      signedOperation.sender,
      operation.accountAddress
    )
  ) {
    return undefined
  }
  const recomputedHash =
    runtime.smartAccount.getUserOperationHash(signedOperation)
  if (recomputedHash.toLowerCase() !== userOperationHash.toLowerCase()) {
    return undefined
  }
  return signedOperation
}

/**
 * Resolves a hash-bound operation only from a single safe-chain snapshot.
 * Exact inclusion evidence wins first. Otherwise an advanced nonce is
 * ambiguous, while an elapsed sponsorship deadline proves that an operation
 * at or ahead of the current nonce can no longer be included.
 */
export async function resolveProtocolOperation(input: {
  operation: SponsoredOperation
  runtime: PerpsAaSmartAccountRuntime
  userOperationHash: Hex
}): Promise<ProtocolOperationResolution | undefined> {
  if (input.runtime.getRecoverySnapshot === undefined) {
    return undefined
  }

  try {
    const signedOperation = verifiedPersistedUserOperation(
      input.operation,
      input.runtime,
      input.userOperationHash
    )
    const operationNonce = signedOperation?.nonce
    const validUntil = signedOperation
      ? input.operation.sponsorshipAuthority !== undefined
        ? authorityBoundSponsorshipValidUntil(
            input.operation.sponsorshipAuthority,
            signedOperation
          )
        : input.runtime.sponsorshipValidUntil
          ? input.runtime.sponsorshipValidUntil(signedOperation)
          : pimlicoSponsorshipValidUntil(
              signedOperation.paymaster,
              signedOperation.paymasterData
            )
      : undefined

    const snapshot = await input.runtime.getRecoverySnapshot(
      input.userOperationHash,
      operationNonce === undefined ? 0n : operationNonce >> 64n
    )
    if (snapshot.userOperationEvidence.kind === 'included') {
      return {
        status: snapshot.userOperationEvidence.success
          ? 'confirmed'
          : 'execution-reverted',
        transactionHash:
          snapshot.userOperationEvidence.transactionHash,
      }
    }
    if (snapshot.userOperationEvidence.kind === 'not-safe-yet') {
      return undefined
    }
    if (snapshot.userOperationEvidence.kind === 'inconclusive') {
      return undefined
    }

    if (operationNonce !== undefined) {
      // Once this nonce has advanced, a missing exact event cannot distinguish
      // inclusion from another operation consuming the nonce. Expiry must not
      // turn that ambiguity into a retry-safe result.
      if (snapshot.accountNonce > operationNonce) {
        return {
          status: 'outcome-unknown',
          protocolNonceAdvanced: true,
        }
      }
      // The safe block timestamp is authoritative. A signed operation whose
      // sponsorship has elapsed cannot land even if an earlier nonce gap has
      // kept the account nonce below this operation's nonce.
      if (
        validUntil !== undefined &&
        snapshot.blockTimestamp > validUntil &&
        snapshot.accountNonce <= operationNonce
      ) {
        return { status: 'expired' }
      }
    }

    return undefined
  } catch {
    // Corrupt persisted metadata or an unavailable chain/index read cannot
    // prove that rebuilding is safe.
    return undefined
  }
}
