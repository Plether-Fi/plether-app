import { describe, expect, it, vi } from 'vitest'
import type { Address, Hex } from 'viem'
import { createPletherBundlerAdapter } from '../adapters/bundler'
import { createPletherSponsorAdapter } from '../adapters/sponsor'
import {
  BundlerRequestError,
  SponsorRequestError,
} from '../errors'
import {
  serializeUserOperationV08,
  type UserOperationV08,
} from '../types'

const ACCOUNT = '0x1111111111111111111111111111111111111111' as Address
const ENTRY_POINT = '0x2222222222222222222222222222222222222222' as Address
const PAYMASTER = '0x3333333333333333333333333333333333333333' as Address
const POLICY_ID = `0x${'44'.repeat(32)}` as Hex
const USER_OPERATION_HASH = `0x${'55'.repeat(32)}` as Hex
const TRANSACTION_HASH = `0x${'66'.repeat(32)}` as Hex

function operation(): UserOperationV08 {
  return {
    sender: ACCOUNT,
    nonce: 7n,
    callData: '0x1234',
    callGasLimit: 1n,
    verificationGasLimit: 2n,
    preVerificationGas: 3n,
    maxFeePerGas: 4n,
    maxPriorityFeePerGas: 5n,
    signature: '0xdeadbeef',
  }
}

function jsonResponse(result: unknown): Response {
  return new Response(JSON.stringify({
    jsonrpc: '2.0',
    id: 1,
    result,
  }), {
    status: 200,
    headers: { 'content-type': 'application/json' },
  })
}

describe('UserOperation v0.8 serialization', () => {
  it('omits the account signature for sponsor requests', () => {
    expect(
      serializeUserOperationV08(operation(), { includeSignature: false })
    ).toEqual({
      sender: ACCOUNT,
      nonce: '0x7',
      callData: '0x1234',
      callGasLimit: '0x1',
      verificationGasLimit: '0x2',
      preVerificationGas: '0x3',
      maxFeePerGas: '0x4',
      maxPriorityFeePerGas: '0x5',
    })
  })
})

describe('Plether sponsor adapter', () => {
  it('uses exact ERC-7677 methods, canonical chain quantity, and policy context', async () => {
    const fetcher = vi.fn<typeof fetch>().mockResolvedValue(
      jsonResponse({
        paymaster: PAYMASTER,
        paymasterData: '0x1234',
        paymasterVerificationGasLimit: '0x10',
        paymasterPostOpGasLimit: '0x20',
      })
    )
    const adapter = createPletherSponsorAdapter({
      rpcUrl: '/sponsor',
      manifestVersion: 'perps-aa-arbitrum-sepolia-v1',
      policyId: POLICY_ID,
      expectedPaymaster: PAYMASTER,
      fetcher,
    })

    const response = await adapter.getPaymasterStubData({
      chainId: 421614,
      entryPoint: ENTRY_POINT,
      account: ACCOUNT,
      action: 'place-order',
      operation: operation(),
    })

    expect(response.paymasterData).toBe('0x1234')
    const request = JSON.parse(
      String(fetcher.mock.calls[0]?.[1]?.body)
    ) as {
      method: string
      params: [
        Record<string, unknown>,
        Address,
        Hex,
        Record<string, unknown>,
      ]
    }
    expect(request.method).toBe('pm_getPaymasterStubData')
    expect(request.params[0]).not.toHaveProperty('signature')
    expect(request.params[1]).toBe(ENTRY_POINT)
    expect(request.params[2]).toBe('0x66eee')
    expect(request.params[3]).toEqual({
      manifestVersion: 'perps-aa-arbitrum-sepolia-v1',
      policyId: POLICY_ID,
      action: 'place-order',
    })
  })

  it('preserves stable sponsor reason, retryable, and call index', async () => {
    const fetcher = vi.fn<typeof fetch>().mockResolvedValue(
      new Response(JSON.stringify({
        jsonrpc: '2.0',
        id: 1,
        error: {
          code: -32501,
          message: 'estimate again',
          data: {
            reason: 'RESTART_ESTIMATION',
            retryable: true,
            callIndex: 2,
          },
        },
      }), { status: 200 })
    )
    const adapter = createPletherSponsorAdapter({
      rpcUrl: '/sponsor',
      manifestVersion: 'v1',
      policyId: POLICY_ID,
      fetcher,
    })

    const rejection = adapter.getPaymasterData({
      chainId: 421614,
      entryPoint: ENTRY_POINT,
      account: ACCOUNT,
      action: 'deposit',
      operation: operation(),
    })

    await expect(rejection).rejects.toMatchObject({
      name: 'SponsorRequestError',
      reason: 'RESTART_ESTIMATION',
      retryable: true,
      callIndex: 2,
      rpcCode: -32501,
    } satisfies Partial<SponsorRequestError>)
  })

  it('fails closed when the sponsor selects a different paymaster', async () => {
    const adapter = createPletherSponsorAdapter({
      rpcUrl: '/sponsor',
      manifestVersion: 'v1',
      policyId: POLICY_ID,
      expectedPaymaster: PAYMASTER,
      fetcher: vi.fn<typeof fetch>().mockResolvedValue(
        jsonResponse({
          paymaster: ACCOUNT,
          paymasterData: '0x1234',
          paymasterVerificationGasLimit: '0x10',
          paymasterPostOpGasLimit: '0x20',
        })
      ),
    })

    await expect(adapter.getPaymasterStubData({
      chainId: 421614,
      entryPoint: ENTRY_POINT,
      account: ACCOUNT,
      action: 'deposit',
      operation: operation(),
    })).rejects.toMatchObject({
      reason: 'ACCOUNT_NOT_TRUSTED',
      retryable: false,
    })
  })

  it('rejects conflicting split and packed paymaster identities', async () => {
    const packedDifferentPaymaster =
      `0x${ACCOUNT.slice(2)}${'00'.repeat(189)}` as Hex
    const adapter = createPletherSponsorAdapter({
      rpcUrl: '/sponsor',
      manifestVersion: 'v1',
      policyId: POLICY_ID,
      expectedPaymaster: PAYMASTER,
      fetcher: vi.fn<typeof fetch>().mockResolvedValue(
        jsonResponse({
          paymaster: PAYMASTER,
          paymasterAndData: packedDifferentPaymaster,
        })
      ),
    })

    await expect(adapter.getPaymasterStubData({
      chainId: 421614,
      entryPoint: ENTRY_POINT,
      account: ACCOUNT,
      action: 'deposit',
      operation: operation(),
    })).rejects.toMatchObject({
      reason: 'ACCOUNT_NOT_TRUSTED',
      retryable: false,
    })
  })
})

describe('Plether bundler adapter', () => {
  it('captures the UserOperation hash before the included transaction hash', async () => {
    const events: string[] = []
    const fetcher = vi.fn<typeof fetch>()
      .mockResolvedValueOnce(jsonResponse(USER_OPERATION_HASH))
      .mockResolvedValueOnce(jsonResponse({
        userOpHash: USER_OPERATION_HASH,
        sender: ACCOUNT,
        paymaster: PAYMASTER,
        success: true,
        receipt: {
          transactionHash: TRANSACTION_HASH,
          status: '0x1',
          logs: [],
        },
      }))
    const adapter = createPletherBundlerAdapter({
      rpcUrl: '/bundler',
      pollIntervalMs: 1,
      fetcher,
      expectedSender: ACCOUNT,
      expectedPaymaster: PAYMASTER,
      onUserOperationHash: (hash) => {
        events.push(`userop:${hash}`)
      },
      onTransactionHash: (hash) => {
        events.push(`tx:${hash}`)
      },
    })

    const hash = await adapter.sendUserOperation({
      operation: operation(),
      entryPoint: ENTRY_POINT,
    })
    await adapter.waitForUserOperationReceipt?.({ userOperationHash: hash })

    expect(events).toEqual([
      `userop:${USER_OPERATION_HASH}`,
      `tx:${TRANSACTION_HASH}`,
    ])
  })

  it('rejects a resolved receipt whose UserOperation execution reverted', async () => {
    const adapter = createPletherBundlerAdapter({
      rpcUrl: '/bundler',
      fetcher: vi.fn<typeof fetch>().mockResolvedValue(
        jsonResponse({
          userOpHash: USER_OPERATION_HASH,
          sender: ACCOUNT,
          paymaster: PAYMASTER,
          success: false,
          reason: 'execution reverted',
          receipt: {
            transactionHash: TRANSACTION_HASH,
            status: '0x1',
            logs: [],
          },
        })
      ),
      expectedSender: ACCOUNT,
      expectedPaymaster: PAYMASTER,
    })

    const receipt = adapter.waitForUserOperationReceipt?.({
      userOperationHash: USER_OPERATION_HASH,
    })

    await expect(receipt).rejects.toMatchObject({
      name: 'BundlerRequestError',
      terminalStatus: 'execution-reverted',
      retryable: false,
    } satisfies Partial<BundlerRequestError>)
  })

  it('fails closed when a resolved receipt omits execution success', async () => {
    const adapter = createPletherBundlerAdapter({
      rpcUrl: '/bundler',
      fetcher: vi.fn<typeof fetch>().mockResolvedValue(
        jsonResponse({
          userOpHash: USER_OPERATION_HASH,
          sender: ACCOUNT,
          paymaster: PAYMASTER,
          receipt: {
            transactionHash: TRANSACTION_HASH,
            status: '0x1',
            logs: [],
          },
        })
      ),
      expectedSender: ACCOUNT,
      expectedPaymaster: PAYMASTER,
    })

    await expect(adapter.waitForUserOperationReceipt?.({
      userOperationHash: USER_OPERATION_HASH,
    })).rejects.toMatchObject({
      name: 'BundlerRequestError',
      retryable: true,
    })
  })

  it('rejects a receipt for a different UserOperation', async () => {
    const adapter = createPletherBundlerAdapter({
      rpcUrl: '/bundler',
      expectedSender: ACCOUNT,
      expectedPaymaster: PAYMASTER,
      fetcher: vi.fn<typeof fetch>().mockResolvedValue(
        jsonResponse({
          userOpHash: `0x${'77'.repeat(32)}`,
          sender: ACCOUNT,
          paymaster: PAYMASTER,
          success: true,
          receipt: {
            transactionHash: TRANSACTION_HASH,
            status: '0x1',
            logs: [],
          },
        })
      ),
    })

    await expect(adapter.waitForUserOperationReceipt?.({
      userOperationHash: USER_OPERATION_HASH,
    })).rejects.toMatchObject({
      name: 'BundlerRequestError',
      retryable: true,
    } satisfies Partial<BundlerRequestError>)
  })
})
