import { describe, expect, it, vi } from 'vitest'
import type { Address, Hex } from 'viem'
import { reconcilePimlicoUserOperation } from '../operationReconciler'
import type {
  ManagedUserOperationReceipt,
  PerpsAaSmartAccountRuntime,
  PimlicoUserOperationStatus,
} from '../runtimeContext'

const OWNER = '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address
const ENTRY_POINT = '0x3333333333333333333333333333333333333333' as Address
const HASH = `0x${'44'.repeat(32)}` as Hex
const TRANSACTION_HASH = `0x${'55'.repeat(32)}` as Hex

function receiptNotFoundError(): Error {
  const error = new Error('receipt not found')
  error.name = 'UserOperationReceiptNotFoundError'
  return error
}

function runtime(
  status: PimlicoUserOperationStatus,
  receiptSuccess = true,
  receiptAvailable = status === 'included'
): PerpsAaSmartAccountRuntime {
  const receipt = {
    actualGasCost: 1n,
    actualGasUsed: 1n,
    entryPoint: ENTRY_POINT,
    logs: [],
    nonce: 0n,
    sender: ACCOUNT,
    success: receiptSuccess,
    userOpHash: HASH,
    reason: receiptSuccess ? undefined : 'execution reverted',
    receipt: {
      transactionHash: TRANSACTION_HASH,
      status: receiptSuccess ? 'success' : 'reverted',
    },
  } as ManagedUserOperationReceipt

  return {
    chainId: 421614,
    ownerAddress: OWNER,
    factoryAddress:
      '0x6666666666666666666666666666666666666666',
    accountVersion: 'permissionless-simple-v0.8',
    accountIndex: '0',
    smartAccount: {
      accountAddress: ACCOUNT,
      entryPoint: ENTRY_POINT,
      prepareUserOperation: vi.fn(),
      signUserOperation: vi.fn(),
      getUserOperationHash: vi.fn(),
      sendUserOperation: vi.fn(),
      getUserOperationStatus: vi.fn(async () => ({
        status,
        transactionHash:
          status === 'submitted' || status === 'included'
            ? TRANSACTION_HASH
            : null,
      })),
      getUserOperationReceipt: vi.fn(async () => {
        if (!receiptAvailable) {
          throw receiptNotFoundError()
        }
        return receipt
      }),
    },
  }
}

describe('reconcilePimlicoUserOperation', () => {
  it.each([
    'not_found',
    'not_submitted',
    'submitted',
    'queued',
    'rejected',
    'failed',
    'reverted',
  ] satisfies PimlicoUserOperationStatus[])(
    'keeps %s fail-closed and pending',
    async (status) => {
      await expect(reconcilePimlicoUserOperation({
        runtime: runtime(status),
        userOperationHash: HASH,
      })).resolves.toMatchObject({
        kind: 'pending',
        status,
      })
    }
  )

  it('requires a successful UserOperation receipt after inclusion', async () => {
    await expect(reconcilePimlicoUserOperation({
      runtime: runtime('included', false),
      userOperationHash: HASH,
    })).resolves.toMatchObject({
      kind: 'terminal',
      terminalStatus: 'execution-reverted',
      message: 'execution reverted',
    })
  })

  it('returns the included transaction hash only after a successful receipt', async () => {
    await expect(reconcilePimlicoUserOperation({
      runtime: runtime('included'),
      userOperationHash: HASH,
    })).resolves.toMatchObject({
      kind: 'confirmed',
      transactionHash: TRANSACTION_HASH,
    })
  })

  it('keeps vendor inclusion pending until a canonical-safe receipt is available', async () => {
    await expect(reconcilePimlicoUserOperation({
      runtime: runtime('included', true, false),
      userOperationHash: HASH,
    })).resolves.toMatchObject({
      kind: 'pending',
      status: 'included',
      transactionHash: TRANSACTION_HASH,
    })
  })

  it('lets an exact receipt override a stale terminal status', async () => {
    const managedRuntime = runtime('failed', true, true)
    await expect(reconcilePimlicoUserOperation({
      runtime: managedRuntime,
      userOperationHash: HASH,
    })).resolves.toMatchObject({
      kind: 'confirmed',
      transactionHash: TRANSACTION_HASH,
    })
    expect(
      managedRuntime.smartAccount.getUserOperationStatus
    ).not.toHaveBeenCalled()
  })

  it('keeps transport failures fail-closed instead of trusting status', async () => {
    const managedRuntime = runtime('not_submitted')
    managedRuntime.smartAccount.getUserOperationReceipt = vi.fn(async () => {
      throw new Error('RPC transport unavailable')
    })

    await expect(reconcilePimlicoUserOperation({
      runtime: managedRuntime,
      userOperationHash: HASH,
    })).rejects.toThrow('RPC transport unavailable')
    expect(
      managedRuntime.smartAccount.getUserOperationStatus
    ).not.toHaveBeenCalled()
  })
})
