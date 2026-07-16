import { useEffect } from 'react'
import { createPletherBundlerAdapter } from './adapters/bundler'
import { clearDepositAuthorization } from './authorizationStore'
import {
  findBundlerRequestError,
  findSponsorRequestError,
} from './errors'
import {
  hasSponsoredOperationSignal,
  isSponsoredOperationTerminal,
  SPONSORED_OPERATION_STORAGE_NAME,
  useSponsoredOperationStore,
} from './operationStore'
import { usePerpsIdentity } from './usePerpsIdentity'

export function SponsoredOperationRecovery() {
  const identity = usePerpsIdentity()
  const accountAddress = identity.accountAddress
  const bundlerRpcUrl = identity.manifest?.bundlerRpcUrl
  const paymaster = identity.manifest?.paymaster

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
    if (!bundlerRpcUrl || !accountAddress || !paymaster) return

    const recoveryControllers = new Map<string, AbortController>()

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
          !recoveryControllers.has(operation.id)
        )

      for (const operation of recoverable) {
        const userOperationHash = operation.userOperationHash
        if (!userOperationHash) continue

        const abortController = new AbortController()
        recoveryControllers.set(operation.id, abortController)
        const bundler = createPletherBundlerAdapter({
          rpcUrl: bundlerRpcUrl,
          getSignal: () => abortController.signal,
          expectedSender: accountAddress,
          expectedPaymaster: paymaster,
        })

        store.transition(operation.id, 'confirming')
        void bundler.waitForUserOperationReceipt?.({
          userOperationHash,
        }).then((receipt) => {
          const transactionHash = receipt.receipt?.transactionHash
          if (transactionHash) {
            useSponsoredOperationStore.getState().recordTransactionHash(
              operation.id,
              transactionHash
            )
          }
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
        }).catch((error: unknown) => {
          if (abortController.signal.aborted) return
          const bundlerError = findBundlerRequestError(error)
          const sponsorError = findSponsorRequestError(error)
          const terminalStatus = bundlerError?.terminalStatus
          const operationStatus =
            terminalStatus && terminalStatus !== 'receipt-timeout'
              ? terminalStatus
              : 'receipt-timeout'
          useSponsoredOperationStore.getState().failOperation({
            id: operation.id,
            status: operationStatus,
            reason: sponsorError?.reason ??
              terminalStatus ??
              'BUNDLER_UNAVAILABLE',
            retryable: sponsorError?.retryable ??
              bundlerError?.retryable ??
              true,
            replacementUserOperationHash:
              bundlerError?.replacementUserOperationHash as
                | `0x${string}`
                | undefined,
          })
        }).finally(() => {
          recoveryControllers.delete(operation.id)
        })
      }
    }

    scan()
    const interval = globalThis.setInterval(scan, 5_000)

    return () => {
      globalThis.clearInterval(interval)
      for (const controller of recoveryControllers.values()) {
        controller.abort()
      }
      recoveryControllers.clear()
    }
  }, [accountAddress, bundlerRpcUrl, paymaster])

  return null
}
