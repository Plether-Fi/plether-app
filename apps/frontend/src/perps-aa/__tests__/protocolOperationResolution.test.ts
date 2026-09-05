import { concatHex, numberToHex, type Address, type Hex } from 'viem'
import { describe, expect, it, vi } from 'vitest'
import type { SponsoredOperation } from '../operationStore'
import {
  createSponsorshipAuthority,
  PLETHER_PAYMASTER_POLICY_ID,
  PLETHER_PAYMASTER_POST_OP_GAS_LIMIT,
  PLETHER_PAYMASTER_VERIFICATION_GAS_LIMIT,
  PLETHER_SIMPLE_ACCOUNT_PROXY_CODE_HASH,
  PIMLICO_SINGLETON_PAYMASTER_V8,
  pletherSponsorshipValidUntil,
  type PersistedSponsorshipAuthorityV1,
} from '../paymasterValidity'
import { persistManagedUserOperation } from '../persistedUserOperation'
import {
  resolveProtocolOperation,
} from '../protocolOperationResolution'
import type {
  ManagedUserOperation,
  PerpsAaSmartAccountRuntime,
  SponsoredOperationRecoverySnapshot,
} from '../runtimeContext'

const OWNER = '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address
const ENTRY_POINT = '0x3333333333333333333333333333333333333333' as Address
const HASH = `0x${'44'.repeat(32)}` as Hex
const OTHER_HASH = `0x${'55'.repeat(32)}` as Hex
const TRANSACTION_HASH = `0x${'66'.repeat(32)}` as Hex
const PLETHER_PAYMASTER_A =
  '0x1234567890123456789012345678901234567890' as Address
const PLETHER_PAYMASTER_B =
  '0x9876543210987654321098765432109876543210' as Address

function paymasterData(validUntil: bigint): Hex {
  return concatHex([
    '0x01',
    numberToHex(validUntil, { size: 6 }),
    numberToHex(0n, { size: 6 }),
    `0x${'11'.repeat(65)}`,
  ])
}

function pletherPaymasterData(validUntil: bigint): Hex {
  return concatHex([
    numberToHex(validUntil, { size: 6 }),
    numberToHex(validUntil - 300n, { size: 6 }),
    numberToHex(1_000_000n, { size: 16 }),
    PLETHER_PAYMASTER_POLICY_ID,
    PLETHER_SIMPLE_ACCOUNT_PROXY_CODE_HASH,
    `0x${'44'.repeat(65)}`,
  ])
}

function signedOperation(input: {
  nonce?: bigint
  validUntil?: bigint
} = {}): ManagedUserOperation {
  return {
    sender: ACCOUNT,
    nonce: input.nonce ?? 7n,
    callData: '0x1234',
    callGasLimit: 1n,
    verificationGasLimit: 2n,
    preVerificationGas: 3n,
    maxFeePerGas: 4n,
    maxPriorityFeePerGas: 5n,
    paymaster: PIMLICO_SINGLETON_PAYMASTER_V8,
    paymasterData: paymasterData(input.validUntil ?? 1_000n),
    paymasterVerificationGasLimit: 6n,
    paymasterPostOpGasLimit: 7n,
    signature: '0xdeadbeef',
  }
}

function pletherSignedOperation(input: {
  paymaster?: Address
  nonce?: bigint
  validUntil?: bigint
} = {}): ManagedUserOperation {
  return {
    ...signedOperation({ nonce: input.nonce }),
    paymaster: input.paymaster ?? PLETHER_PAYMASTER_A,
    paymasterData: pletherPaymasterData(input.validUntil ?? 1_000n),
    paymasterVerificationGasLimit:
      PLETHER_PAYMASTER_VERIFICATION_GAS_LIMIT,
    paymasterPostOpGasLimit: PLETHER_PAYMASTER_POST_OP_GAS_LIMIT,
  }
}

function operation(
  signedUserOperation = signedOperation(),
  sponsorshipAuthority?: PersistedSponsorshipAuthorityV1
): SponsoredOperation {
  return {
    id: 'operation-1',
    ownerAddress: OWNER,
    accountAddress: ACCOUNT,
    chainId: 421614,
    accountMode: 'simple',
    manifestVersion: 'test-manifest',
    action: 'place-order',
    lane: 'default',
    status: 'receipt-timeout',
    sponsorshipAccepted: true,
    userOperationHash: HASH,
    signedUserOperation: persistManagedUserOperation(signedUserOperation),
    submissionMetadataVersion: 1,
    ...(sponsorshipAuthority ? { sponsorshipAuthority } : {}),
    retryCount: 0,
    createdAt: 1,
    updatedAt: 1,
    statusTimestamps: { 'receipt-timeout': 1 },
  }
}

function runtime(
  snapshot: SponsoredOperationRecoverySnapshot,
  computedHash: Hex = HASH
): PerpsAaSmartAccountRuntime {
  return {
    chainId: 421614,
    ownerAddress: OWNER,
    factoryAddress:
      '0x7777777777777777777777777777777777777777',
    accountVersion: 'permissionless-simple-v0.8',
    accountIndex: '0',
    getRecoverySnapshot: vi.fn(async () => snapshot),
    smartAccount: {
      accountAddress: ACCOUNT,
      entryPoint: ENTRY_POINT,
      prepareUserOperation: vi.fn(),
      signUserOperation: vi.fn(),
      getUserOperationHash: vi.fn(() => computedHash),
      sendUserOperation: vi.fn(),
      getUserOperationStatus: vi.fn(),
      getUserOperationReceipt: vi.fn(),
    },
  }
}

function notLocatedSnapshot(input: {
  accountNonce: bigint
  blockTimestamp: bigint
}): SponsoredOperationRecoverySnapshot {
  return {
    blockNumber: 123n,
    blockTimestamp: input.blockTimestamp,
    accountNonce: input.accountNonce,
    userOperationEvidence: { kind: 'not-located' },
  }
}

describe('resolveProtocolOperation', () => {
  it('expires after the safe deadline when the account nonce equals the operation nonce', async () => {
    const managedRuntime = runtime(notLocatedSnapshot({
      accountNonce: 7n,
      blockTimestamp: 1_001n,
    }))

    await expect(resolveProtocolOperation({
      operation: operation(),
      runtime: managedRuntime,
      userOperationHash: HASH,
    })).resolves.toEqual({ status: 'expired' })
  })

  it('expires after the safe deadline when an earlier nonce gap strands the operation', async () => {
    const managedRuntime = runtime(notLocatedSnapshot({
      accountNonce: 7n,
      blockTimestamp: 1_001n,
    }))

    await expect(resolveProtocolOperation({
      operation: operation(signedOperation({ nonce: 8n })),
      runtime: managedRuntime,
      userOperationHash: HASH,
    })).resolves.toEqual({ status: 'expired' })
  })

  it(
    'recovers a native-paymaster journal after rollback to a runtime without native sponsorship decoding',
    async () => {
      const signedUserOperation = pletherSignedOperation()
      const managedRuntime = runtime(notLocatedSnapshot({
        accountNonce: 7n,
        blockTimestamp: 1_001n,
      }))

      await expect(resolveProtocolOperation({
        operation: operation(
          signedUserOperation,
          createSponsorshipAuthority({
            paymasterAddress: PLETHER_PAYMASTER_A,
            validUntil: 1_000n,
          })
        ),
        runtime: managedRuntime,
        userOperationHash: HASH,
      })).resolves.toEqual({ status: 'expired' })
    }
  )

  it('uses the journal authority after a paymaster rotation', async () => {
    const signedUserOperation = pletherSignedOperation({
      paymaster: PLETHER_PAYMASTER_A,
    })
    const managedRuntime = runtime(notLocatedSnapshot({
      accountNonce: 7n,
      blockTimestamp: 1_001n,
    }))
    managedRuntime.sponsorshipValidUntil = vi.fn((candidate) =>
      pletherSponsorshipValidUntil(PLETHER_PAYMASTER_B, candidate)
    )

    await expect(resolveProtocolOperation({
      operation: operation(
        signedUserOperation,
        createSponsorshipAuthority({
          paymasterAddress: PLETHER_PAYMASTER_A,
          validUntil: 1_000n,
        })
      ),
      runtime: managedRuntime,
      userOperationHash: HASH,
    })).resolves.toEqual({ status: 'expired' })
    expect(managedRuntime.sponsorshipValidUntil).not.toHaveBeenCalled()
  })

  it.each([
    {
      name: 'changed deadline',
      authority: {
        version: 1,
        paymasterAddress: PLETHER_PAYMASTER_A,
        validUntil: '999',
      },
    },
    {
      name: 'changed paymaster',
      authority: {
        version: 1,
        paymasterAddress: PLETHER_PAYMASTER_B,
        validUntil: '1000',
      },
    },
    {
      name: 'non-canonical deadline',
      authority: {
        version: 1,
        paymasterAddress: PLETHER_PAYMASTER_A,
        validUntil: '01000',
      },
    },
    {
      name: 'unknown authority version',
      authority: {
        version: 2,
        paymasterAddress: PLETHER_PAYMASTER_A,
        validUntil: '1000',
      },
    },
  ])('fails closed for tampered native sponsorship authority: $name', async ({
    authority,
  }) => {
    const managedRuntime = runtime(notLocatedSnapshot({
      accountNonce: 7n,
      blockTimestamp: 1_001n,
    }))
    const persistedOperation = {
      ...operation(pletherSignedOperation()),
      sponsorshipAuthority: authority,
    } as SponsoredOperation

    await expect(resolveProtocolOperation({
      operation: persistedOperation,
      runtime: managedRuntime,
      userOperationHash: HASH,
    })).resolves.toBeUndefined()
  })

  it('keeps the exact deadline and future sponsorship fail-closed', async () => {
    for (const blockTimestamp of [999n, 1_000n]) {
      const managedRuntime = runtime(notLocatedSnapshot({
        accountNonce: 7n,
        blockTimestamp,
      }))

      await expect(resolveProtocolOperation({
        operation: operation(signedOperation({ nonce: 8n })),
        runtime: managedRuntime,
        userOperationHash: HASH,
      })).resolves.toBeUndefined()
    }
  })

  it('keeps nonce advancement outcome-unknown even after sponsorship expiry', async () => {
    const managedRuntime = runtime(notLocatedSnapshot({
      accountNonce: 8n,
      blockTimestamp: 1_001n,
    }))

    await expect(resolveProtocolOperation({
      operation: operation(),
      runtime: managedRuntime,
      userOperationHash: HASH,
    })).resolves.toEqual({
      status: 'outcome-unknown',
      protocolNonceAdvanced: true,
    })
  })

  it.each([
    { success: true, status: 'confirmed' as const },
    { success: false, status: 'execution-reverted' as const },
  ])('lets exact safe inclusion resolve as $status first', async ({
    success,
    status,
  }) => {
    const managedRuntime = runtime({
      blockNumber: 123n,
      blockTimestamp: 1_001n,
      accountNonce: 8n,
      userOperationEvidence: {
        kind: 'included',
        success,
        transactionHash: TRANSACTION_HASH,
        blockNumber: 122n,
      },
    })

    await expect(resolveProtocolOperation({
      operation: operation(),
      runtime: managedRuntime,
      userOperationHash: HASH,
    })).resolves.toEqual({
      status,
      transactionHash: TRANSACTION_HASH,
    })
  })

  it('waits when the exact event range has not reached the safe head', async () => {
    const managedRuntime = runtime({
      blockNumber: 123n,
      blockTimestamp: 1_001n,
      accountNonce: 7n,
      userOperationEvidence: { kind: 'not-safe-yet' },
    })

    await expect(resolveProtocolOperation({
      operation: operation(),
      runtime: managedRuntime,
      userOperationHash: HASH,
    })).resolves.toBeUndefined()
  })

  it('uses the verified operation nonce key for the atomic snapshot', async () => {
    const nonceKey = 9n
    const nonce = (nonceKey << 64n) | 8n
    const managedRuntime = runtime(notLocatedSnapshot({
      accountNonce: nonce,
      blockTimestamp: 1_000n,
    }))

    await resolveProtocolOperation({
      operation: operation(signedOperation({ nonce })),
      runtime: managedRuntime,
      userOperationHash: HASH,
    })

    expect(managedRuntime.getRecoverySnapshot).toHaveBeenCalledWith(
      HASH,
      nonceKey
    )
  })

  it('fails closed when the persisted preimage does not match the recorded hash', async () => {
    const managedRuntime = runtime(notLocatedSnapshot({
      accountNonce: 7n,
      blockTimestamp: 1_001n,
    }), OTHER_HASH)

    await expect(resolveProtocolOperation({
      operation: operation(),
      runtime: managedRuntime,
      userOperationHash: HASH,
    })).resolves.toBeUndefined()
  })

  it('fails closed when the safe snapshot is unavailable', async () => {
    const managedRuntime = runtime(notLocatedSnapshot({
      accountNonce: 7n,
      blockTimestamp: 1_001n,
    }))
    managedRuntime.getRecoverySnapshot = vi.fn(async () => {
      throw new Error('RPC unavailable')
    })

    await expect(resolveProtocolOperation({
      operation: operation(),
      runtime: managedRuntime,
      userOperationHash: HASH,
    })).resolves.toBeUndefined()
  })
})
