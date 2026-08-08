import { afterEach, describe, expect, it, vi } from 'vitest'
import type { Address } from 'viem'
import {
  acquireSponsoredOperationBrowserLane,
  SponsoredOperationCoordinationError,
} from './laneLock'
import { SponsoredOperationLockedError } from './operationStore'

const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address

function acquire() {
  return acquireSponsoredOperationBrowserLane({
    chainId: 421614,
    accountAddress: ACCOUNT,
    lane: 'default',
  })
}

describe('sponsored browser lane lock', () => {
  afterEach(() => {
    vi.unstubAllGlobals()
  })

  it('fails closed when the browser does not expose Web Locks', async () => {
    vi.stubGlobal('navigator', {})

    await expect(acquire()).rejects.toBeInstanceOf(
      SponsoredOperationCoordinationError
    )
  })

  it('allows only one tab-equivalent holder at a time', async () => {
    let held = false
    const request = vi.fn(async (
      name: string,
      _options: LockOptions,
      callback: (lock: Lock | null) => Promise<unknown> | unknown
    ) => {
      if (held) return await callback(null)
      held = true
      try {
        return await callback({
          name,
          mode: 'exclusive',
        } as Lock)
      } finally {
        held = false
      }
    })
    vi.stubGlobal('navigator', {
      locks: { request } as unknown as LockManager,
    })

    const releaseFirst = await acquire()
    await expect(acquire()).rejects.toBeInstanceOf(
      SponsoredOperationLockedError
    )

    releaseFirst()
    await vi.waitFor(() => {
      expect(held).toBe(false)
    })

    const releaseSecond = await acquire()
    releaseSecond()
  })
})
