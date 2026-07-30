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

export async function acquireSponsoredOperationBrowserLane(input: {
  chainId: number
  accountAddress: Address
  lane: string
}): Promise<() => void> {
  const lockManager = (
    globalThis.navigator as unknown as { locks?: LockManager }
  ).locks
  if (!lockManager) {
    throw new SponsoredOperationCoordinationError(
      'This browser cannot safely coordinate sponsored Trading Account actions across tabs.'
    )
  }

  const name = browserLaneName(input)
  let releaseHold: (() => void) | undefined
  const hold = new Promise<void>((resolve) => {
    releaseHold = resolve
  })

  return await new Promise<() => void>((resolve, reject) => {
    void lockManager.request(
      name,
      { mode: 'exclusive', ifAvailable: true },
      async (lock) => {
        if (!lock) {
          reject(new SponsoredOperationLockedError('another-browser-tab'))
          return
        }

        let released = false
        resolve(() => {
          if (released) return
          released = true
          releaseHold?.()
        })
        await hold
      }
    ).catch((error: unknown) => {
      reject(new SponsoredOperationCoordinationError(
        'Unable to coordinate the sponsored Trading Account lane.',
        { cause: error }
      ))
    })
  })
}
