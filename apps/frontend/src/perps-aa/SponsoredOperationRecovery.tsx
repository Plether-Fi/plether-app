import { useEffect } from 'react'
import { clearDepositAuthorization } from './authorizationStore'
import {
  hasSponsoredOperationSignal,
  isSponsoredOperationTerminal,
  SPONSORED_OPERATION_STORAGE_NAME,
  useSponsoredOperationStore,
} from './operationStore'
import { reconcilePimlicoUserOperation } from './operationReconciler'
import { usePerpsAaRuntime } from './runtimeContext'

export function SponsoredOperationRecovery() {
  const runtime = usePerpsAaRuntime()
  const accountAddress = runtime?.smartAccount.accountAddress

  useEffect(() => {
    const onStorage = (event: StorageEvent) => {
      if (event.key === SPONSORED_OPERATION_STORAGE_NAME) {
        void useSponsoredOperationStore.persist.rehydrate()
      }
    }
    globalThis.addEventListener('storage', onStorage)
    return () => {
      globalThis.removeEventListener('storage', onStorage)
    }
  }, [])

  useEffect(() => {
    if (!runtime || !accountAddress) return

    const recovering = new Set<string>()

    const scan = () => {
      const store = useSponsoredOperationStore.getState()
      store.cleanupOperations()

      const currentOperations = useSponsoredOperationStore.getState().operations
        .filter((operation) =>
          operation.accountAddress.toLowerCase() ===
            accountAddress.toLowerCase()
        )

      for (const operation of currentOperations) {
        if (
          !isSponsoredOperationTerminal(operation.status) &&
          operation.userOperationHash === undefined &&
          !hasSponsoredOperationSignal(operation.id)
        ) {
          useSponsoredOperationStore.getState().failOperation({
            id: operation.id,
            reason: 'UNKNOWN',
            retryable: true,
          })
        }
      }

      const recoverable = useSponsoredOperationStore.getState().operations
        .filter((operation) =>
          operation.accountAddress.toLowerCase() ===
            accountAddress.toLowerCase() &&
          operation.userOperationHash !== undefined &&
          (
            operation.status === 'submitting' ||
            operation.status === 'confirming' ||
            operation.status === 'receipt-timeout'
          ) &&
          !hasSponsoredOperationSignal(operation.id) &&
          !recovering.has(operation.id)
        )

      for (const operation of recoverable) {
        const userOperationHash = operation.userOperationHash
        if (!userOperationHash) continue

        recovering.add(operation.id)
        store.transition(operation.id, 'confirming')
        void reconcilePimlicoUserOperation({
          runtime,
          userOperationHash,
        }).then((outcome) => {
          if (outcome.transactionHash) {
            useSponsoredOperationStore.getState().recordTransactionHash(
              operation.id,
              outcome.transactionHash
            )
          }

          if (outcome.kind === 'confirmed') {
            if (operation.authorizationToken) {
              clearDepositAuthorization({
                chainId: operation.chainId,
                ownerAddress: operation.ownerAddress,
                accountAddress: operation.accountAddress,
                token: operation.authorizationToken,
              })
            }
            useSponsoredOperationStore.getState().transition(
              operation.id,
              'confirmed'
            )
            return
          }

          if (outcome.kind === 'terminal') {
            useSponsoredOperationStore.getState().failOperation({
              id: operation.id,
              status: outcome.terminalStatus,
              reason: outcome.terminalStatus,
              retryable: false,
            })
          }
          // not_found and not_submitted are intentionally non-terminal.
          // Pimlico retains status for a limited period, so neither proves
          // that rebuilding the same Plether action is safe.
        }).catch(() => {
          useSponsoredOperationStore.getState().failOperation({
            id: operation.id,
            status: 'receipt-timeout',
            reason: 'BUNDLER_UNAVAILABLE',
            retryable: false,
          })
        }).finally(() => {
          recovering.delete(operation.id)
        })
      }
    }

    scan()
    const interval = globalThis.setInterval(scan, 5_000)

    return () => {
      globalThis.clearInterval(interval)
      recovering.clear()
    }
  }, [accountAddress, runtime])

  return null
}
