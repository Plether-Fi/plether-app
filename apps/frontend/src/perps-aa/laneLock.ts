import type { Address } from 'viem'
import { SponsoredOperationLockedError } from './operationLockError'

export class SponsoredOperationCoordinationError extends Error {
  constructor(message: string, options?: ErrorOptions) {
    super(message, options)
    this.name = 'SponsoredOperationCoordinationError'
  }
}

function browserLaneName(input: {
  chainId: number
  accountAddress: Address
  lane: string
}): string {
  return [
    'plether-perps-user-operation',
    input.chainId.toString(),
    input.accountAddress.toLowerCase(),
    input.lane,
  ].join(':')
}

function browserRecoveryName(input: {
  chainId: number
  accountAddress: Address
  operationId: string
}): string {
  return [
    'plether-perps-user-operation-recovery',
    input.chainId.toString(),
    input.accountAddress.toLowerCase(),
    input.operationId,
  ].join(':')
}

async function acquireBrowserLock(
  name: string,
  unavailable: () => Error
): Promise<ReleaseSponsoredOperationBrowserLock> {
  const lockManager = (
    globalThis.navigator as unknown as { locks?: LockManager }
  ).locks
  if (!lockManager) {
    throw new SponsoredOperationCoordinationError(
      'This browser cannot safely coordinate sponsored Trading Account actions across tabs.'
    )
  }

  let releaseHold: (() => void) | undefined
  const hold = new Promise<void>((resolve) => {
    releaseHold = resolve
  })

  return await new Promise<ReleaseSponsoredOperationBrowserLock>(
    (resolve, reject) => {
      const drained = lockManager.request(
        name,
        { mode: 'exclusive', ifAvailable: true },
        async (lock) => {
          if (!lock) {
            reject(unavailable())
            return
          }

          let released = false
          resolve(async () => {
            if (!released) {
              released = true
              releaseHold?.()
            }
            // Signalling the hold only schedules the Web Lock callback to exit.
            // Wait for request() itself so consumers cannot start the next
            // action while the browser still reports this lock as occupied.
            await drained
          })
          await hold
        }
      ).catch((error: unknown) => {
        reject(new SponsoredOperationCoordinationError(
          'Unable to coordinate the sponsored Trading Account operation.',
          { cause: error }
        ))
      })
    }
  )
}

export type ReleaseSponsoredOperationBrowserLock = () => Promise<void>

export async function acquireSponsoredOperationBrowserLane(input: {
  chainId: number
  accountAddress: Address
  lane: string
}): Promise<ReleaseSponsoredOperationBrowserLock> {
  return acquireBrowserLock(
    browserLaneName(input),
    () => new SponsoredOperationLockedError('another-browser-tab')
  )
}

/**
 * Coordinates background reconciliation for one persisted operation without
 * occupying the account's submission lane. This lets a later nonce submit
 * after exact canonical inclusion while ensuring that two tabs cannot race a
 * safe confirmation against a reorg retraction for the same hash.
 */
export async function acquireSponsoredOperationBrowserRecoveryLock(input: {
  chainId: number
  accountAddress: Address
  operationId: string
}): Promise<ReleaseSponsoredOperationBrowserLock> {
  return acquireBrowserLock(
    browserRecoveryName(input),
    () => new SponsoredOperationCoordinationError(
      'Another browser tab is already reconciling this sponsored operation.'
    )
  )
}
