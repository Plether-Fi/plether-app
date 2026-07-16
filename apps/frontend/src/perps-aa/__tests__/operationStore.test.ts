import { beforeEach, describe, expect, it } from 'vitest'
import type { Address } from 'viem'
import {
  cancelSponsoredOperationRequest,
  SponsoredOperationLockedError,
  useSponsoredOperationStore,
} from '../operationStore'

const OWNER = '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address

function begin(id: string) {
  useSponsoredOperationStore.getState().beginOperation({
    id,
    ownerAddress: OWNER,
    accountAddress: ACCOUNT,
    chainId: 421614,
    accountMode: 'separate-immutable',
    manifestVersion: 'v1',
    action: 'place-order',
  })
}

describe('sponsored operation store', () => {
  beforeEach(() => {
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
  })

  it('accepts sponsorship only after the stub request has completed', () => {
    begin('operation-1')

    useSponsoredOperationStore.getState().transition(
      'operation-1',
      'requesting-stub'
    )
    expect(
      useSponsoredOperationStore.getState().operations[0]?.sponsorshipAccepted
    ).toBe(false)

    useSponsoredOperationStore.getState().transition('operation-1', 'estimating')
    expect(
      useSponsoredOperationStore.getState().operations[0]?.sponsorshipAccepted
    ).toBe(true)
  })

  it('allows only one mutable flow per Trading Account lane', () => {
    begin('operation-1')

    expect(() => begin('operation-2')).toThrow(SponsoredOperationLockedError)

    useSponsoredOperationStore.getState().transition('operation-1', 'confirmed')
    expect(() => begin('operation-2')).not.toThrow()
  })

  it('persists UserOperation and transaction hashes separately', () => {
    begin('operation-1')
    useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      '0x1234'
    )
    useSponsoredOperationStore.getState().recordTransactionHash(
      'operation-1',
      '0xabcd'
    )

    expect(useSponsoredOperationStore.getState().operations[0]).toMatchObject({
      userOperationHash: '0x1234',
      transactionHash: '0xabcd',
    })
  })

  it('does not locally cancel an operation once submission has started', () => {
    begin('operation-1')
    useSponsoredOperationStore.getState().transition(
      'operation-1',
      'submitting'
    )

    cancelSponsoredOperationRequest('operation-1')

    expect(useSponsoredOperationStore.getState().operations[0]?.status)
      .toBe('submitting')
    expect(() => begin('operation-2')).toThrow(SponsoredOperationLockedError)
  })

  it('removes stale and terminal operations from persisted active lanes', () => {
    begin('operation-1')
    useSponsoredOperationStore.setState((state) => ({
      operations: state.operations.map((operation) => ({
        ...operation,
        status: 'confirmed',
        updatedAt: Date.now(),
      })),
      activeLanes: state.activeLanes,
    }))

    useSponsoredOperationStore.getState().cleanupOperations()

    expect(() => begin('operation-2')).not.toThrow()
  })

  it('keeps the account lane locked while a submitted operation receipt is uncertain', () => {
    begin('operation-1')
    useSponsoredOperationStore.getState().recordUserOperationHash(
      'operation-1',
      `0x${'12'.repeat(32)}`
    )
    useSponsoredOperationStore.getState().failOperation({
      id: 'operation-1',
      status: 'receipt-timeout',
      reason: 'BUNDLER_UNAVAILABLE',
      retryable: true,
    })

    expect(() => begin('operation-2')).toThrow(SponsoredOperationLockedError)

    useSponsoredOperationStore.getState().transition(
      'operation-1',
      'dropped'
    )
    expect(() => begin('operation-2')).not.toThrow()
  })
})
