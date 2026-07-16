import { act, render, waitFor } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import type { Address, Hex } from 'viem'
import { getOrCreateDepositAuthorization } from './authorizationStore'
import {
  PerpsIdentityContext,
  type PerpsIdentityContextValue,
} from './PerpsIdentityContext'
import type { PerpsAaDeploymentManifest } from './manifest'
import { SponsoredOperationRecovery } from './SponsoredOperationRecovery'
import { useSponsoredOperationStore } from './operationStore'

const OWNER = '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address
const PAYMASTER = '0x4444444444444444444444444444444444444444' as Address
const TOKEN = '0x9999999999999999999999999999999999999999' as Address
const USER_OPERATION_HASH = `0x${'aa'.repeat(32)}` as Hex
const TRANSACTION_HASH = `0x${'bb'.repeat(32)}` as Hex

const MANIFEST: PerpsAaDeploymentManifest = {
  version: 'perps-aa-arbitrum-sepolia-v1',
  chainId: 421614,
  entryPoint: '0x3333333333333333333333333333333333333333',
  paymaster: PAYMASTER,
  policyId: `0x${'55'.repeat(32)}`,
  sponsorServiceRpcUrl: 'https://sponsor.example/rpc',
  bundlerRpcUrl: 'https://bundler.example/rpc',
  smartAccountMode: 'separate-immutable',
  smartAccountFactory: '0x6666666666666666666666666666666666666666',
  smartAccountImplementation: '0x7777777777777777777777777777777777777777',
  accountRuntimeCodeHash: `0x${'88'.repeat(32)}`,
  usdc: TOKEN,
  usdcSupportsEip3009: false,
  usdcEip712Name: null,
  usdcEip712Version: null,
  marginClearinghouse: '0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
  cfdEngine: '0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
  orderRouter: '0xcccccccccccccccccccccccccccccccccccccccc',
  userOperationExplorerUrlTemplate:
    'https://explorer.example/user-op/{userOperationHash}',
  transactionExplorerUrlTemplate:
    'https://explorer.example/tx/{transactionHash}',
  testnetFaucet: null,
  sponsorshipEnabled: true,
}

function identityValue(): PerpsIdentityContextValue {
  return {
    status: 'ready',
    ownerAddress: OWNER,
    accountAddress: ACCOUNT,
    chainId: 421614,
    isAaManifestConfigured: true,
    sponsorshipEnabled: true,
    manifest: MANIFEST,
    identity: null,
    proposedIdentity: null,
    changedIdentityFields: [],
    error: null,
    confirmIdentityAfterContinuityCheck: () => false,
    reloadIdentity: () => undefined,
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

  afterEach(() => {
    vi.unstubAllGlobals()
  })

  it('releases an interrupted pre-hash lane after reload', async () => {
    useSponsoredOperationStore.getState().beginOperation({
      id: 'interrupted-operation',
      ownerAddress: OWNER,
      accountAddress: ACCOUNT,
      chainId: 421614,
      accountMode: 'separate-immutable',
      manifestVersion: 'perps-aa-arbitrum-sepolia-v1',
      action: 'deposit',
    })
    useSponsoredOperationStore.getState().transition(
      'interrupted-operation',
      'requesting-stub'
    )

    render(
      <PerpsIdentityContext value={identityValue()}>
        <SponsoredOperationRecovery />
      </PerpsIdentityContext>
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
        accountMode: 'separate-immutable',
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
      accountMode: 'separate-immutable',
      manifestVersion: MANIFEST.version,
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
    vi.stubGlobal('fetch', vi.fn<typeof fetch>().mockResolvedValue(
      new Response(JSON.stringify({
        jsonrpc: '2.0',
        id: 1,
        result: {
          userOpHash: USER_OPERATION_HASH,
          sender: ACCOUNT,
          paymaster: PAYMASTER,
          success: true,
          receipt: {
            transactionHash: TRANSACTION_HASH,
            status: '0x1',
            logs: [],
          },
        },
      }), { status: 200 })
    ))

    render(
      <PerpsIdentityContext value={identityValue()}>
        <SponsoredOperationRecovery />
      </PerpsIdentityContext>
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
