import { act, render, waitFor } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import type { Address, Hex } from 'viem'
import { getOrCreateDepositAuthorization } from './authorizationStore'
import {
  PerpsAaRuntimeContext,
  type ManagedUserOperationReceipt,
  type PerpsAaSmartAccountRuntime,
} from './runtimeContext'
import { SponsoredOperationRecovery } from './SponsoredOperationRecovery'
import { useSponsoredOperationStore } from './operationStore'

const OWNER = '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address
const TOKEN = '0x9999999999999999999999999999999999999999' as Address
const USER_OPERATION_HASH = `0x${'aa'.repeat(32)}` as Hex
const TRANSACTION_HASH = `0x${'bb'.repeat(32)}` as Hex

function runtimeValue(input: {
  status?: PerpsAaSmartAccountRuntime['smartAccount']['getUserOperationStatus']
  receipt?: PerpsAaSmartAccountRuntime['smartAccount']['getUserOperationReceipt']
} = {}): PerpsAaSmartAccountRuntime {
  return {
    chainId: 421614,
    ownerAddress: OWNER,
    factoryAddress:
      '0x6666666666666666666666666666666666666666',
    accountVersion: 'permissionless-simple-v0.8',
    accountIndex: '0',
    smartAccount: {
      accountAddress: ACCOUNT,
      entryPoint:
        '0x3333333333333333333333333333333333333333',
      prepareUserOperation: vi.fn(),
      signUserOperation: vi.fn(),
      getUserOperationHash: vi.fn(),
      sendUserOperation: vi.fn(),
      getUserOperationStatus: input.status ?? vi.fn(async () => ({
        status: 'not_found',
        transactionHash: null,
      })),
      getUserOperationReceipt: input.receipt ?? vi.fn(),
    },
  }
}

describe('SponsoredOperationRecovery', () => {
  beforeEach(() => {
    globalThis.localStorage.clear()
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
  })

  it('releases an interrupted pre-hash lane after reload', async () => {
    useSponsoredOperationStore.getState().beginOperation({
      id: 'interrupted-operation',
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      chainId: 421614,
      accountMode: 'simple',
      manifestVersion: 'perps-aa-arbitrum-sepolia-v1',
      action: 'deposit',
    })
    useSponsoredOperationStore.getState().transition(
      'interrupted-operation',
      'requesting-stub'
    )

    render(
      <PerpsAaRuntimeContext value={runtimeValue()}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(
        useSponsoredOperationStore.getState().operations[0]
      ).toMatchObject({
        status: 'failed',
        reason: 'UNKNOWN',
        retryable: true,
      })
    })

    act(() => {
      useSponsoredOperationStore.getState().beginOperation({
        id: 'next-operation',
        ownerAddress: OWNER,
        accountAddress: ACCOUNT,
        chainId: 421614,
        accountMode: 'simple',
        manifestVersion: 'perps-aa-arbitrum-sepolia-v1',
        action: 'deposit',
      })
    })

    expect(
      useSponsoredOperationStore.getState().operations.at(-1)?.id
    ).toBe('next-operation')
  })

  it('clears a recovered EIP-3009 authorization after inclusion', async () => {
    const initialAuthorization = getOrCreateDepositAuthorization({
      chainId: 421614,
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      token: TOKEN,
      amount: 25_000_000n,
      nowSeconds: 1_000n,
    })
    useSponsoredOperationStore.getState().beginOperation({
      id: 'recoverable-deposit',
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      chainId: 421614,
      accountMode: 'simple',
      manifestVersion: 'perps-aa-arbitrum-sepolia-v1',
      action: 'deposit',
      authorizationToken: TOKEN,
    })
    useSponsoredOperationStore.getState().recordUserOperationHash(
      'recoverable-deposit',
      USER_OPERATION_HASH
    )
    useSponsoredOperationStore.getState().transition(
      'recoverable-deposit',
      'confirming'
    )
    const receipt = {
      actualGasCost: 1n,
      actualGasUsed: 1n,
      entryPoint:
        '0x3333333333333333333333333333333333333333',
      logs: [],
      nonce: 0n,
      sender: ACCOUNT,
      success: true,
      userOpHash: USER_OPERATION_HASH,
      receipt: {
        transactionHash: TRANSACTION_HASH,
        status: 'success',
      },
    } as ManagedUserOperationReceipt

    render(
      <PerpsAaRuntimeContext value={runtimeValue({
        status: vi.fn(async () => ({
          status: 'included',
          transactionHash: TRANSACTION_HASH,
        })),
        receipt: vi.fn(async () => receipt),
      })}>
        <SponsoredOperationRecovery />
      </PerpsAaRuntimeContext>
    )

    await waitFor(() => {
      expect(
        useSponsoredOperationStore.getState().operations[0]?.status
      ).toBe('confirmed')
    })

    const nextAuthorization = getOrCreateDepositAuthorization({
      chainId: 421614,
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      token: TOKEN,
      amount: 25_000_000n,
      nowSeconds: 1_000n,
    })
    expect(nextAuthorization.nonce).not.toBe(initialAuthorization.nonce)
  })
})
