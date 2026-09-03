import { concatHex, numberToHex, type Hex } from 'viem'
import { describe, expect, it } from 'vitest'
import {
  PLETHER_PAYMASTER_POLICY_ID,
  PLETHER_PAYMASTER_POST_OP_GAS_LIMIT,
  PLETHER_PAYMASTER_VERIFICATION_GAS_LIMIT,
  PLETHER_PAYMASTER_DATA_BYTES,
  PLETHER_SIMPLE_ACCOUNT_PROXY_CODE_HASH,
  PIMLICO_SINGLETON_PAYMASTER_V8,
  knownSponsorshipValidUntil,
  pletherSponsorshipValidUntil,
  pimlicoSponsorshipValidUntil,
} from '../paymasterValidity'
import type { ManagedUserOperation } from '../runtimeContext'

const PLETHER_PAYMASTER =
  '0x1234567890123456789012345678901234567890'

function verifyingPaymasterData(input: {
  validUntil: bigint
  validAfter?: bigint
  combinedModeByte?: number
  signatureBytes?: number
}): Hex {
  return concatHex([
    numberToHex(input.combinedModeByte ?? 1, { size: 1 }),
    numberToHex(input.validUntil, { size: 6 }),
    numberToHex(input.validAfter ?? 0n, { size: 6 }),
    `0x${'11'.repeat(input.signatureBytes ?? 65)}`,
  ])
}

function pletherPaymasterData(
  validUntil: bigint,
  input: {
    validAfter?: bigint
    policyId?: Hex
    accountCodeHash?: Hex
  } = {}
): Hex {
  return concatHex([
    numberToHex(validUntil, { size: 6 }),
    numberToHex(
      input.validAfter ?? (validUntil > 300n ? validUntil - 300n : 0n),
      { size: 6 }
    ),
    numberToHex(1_000_000n, { size: 16 }),
    input.policyId ?? PLETHER_PAYMASTER_POLICY_ID,
    input.accountCodeHash ?? PLETHER_SIMPLE_ACCOUNT_PROXY_CODE_HASH,
    `0x${'44'.repeat(65)}`,
  ])
}

function pletherOperation(
  validUntil = 1_784_869_349n
): ManagedUserOperation {
  return {
    sender: '0x1111111111111111111111111111111111111111',
    nonce: 0n,
    callData: '0x',
    callGasLimit: 1n,
    verificationGasLimit: 2n,
    preVerificationGas: 3n,
    maxFeePerGas: 4n,
    maxPriorityFeePerGas: 5n,
    paymaster: PLETHER_PAYMASTER,
    paymasterData: pletherPaymasterData(validUntil),
    paymasterVerificationGasLimit:
      PLETHER_PAYMASTER_VERIFICATION_GAS_LIMIT,
    paymasterPostOpGasLimit: PLETHER_PAYMASTER_POST_OP_GAS_LIMIT,
    signature: '0xdeadbeef',
  }
}

describe('pimlicoSponsorshipValidUntil', () => {
  it('parses the SingletonPaymaster verifying-mode deadline', () => {
    expect(pimlicoSponsorshipValidUntil(
      PIMLICO_SINGLETON_PAYMASTER_V8,
      verifyingPaymasterData({
        validUntil: 1_784_869_349n,
      })
    )).toBe(1_784_869_349n)
  })

  it('fails closed for an unbounded or unknown paymaster format', () => {
    expect(pimlicoSponsorshipValidUntil(
      PIMLICO_SINGLETON_PAYMASTER_V8,
      verifyingPaymasterData({ validUntil: 0n })
    )).toBeUndefined()
    expect(pimlicoSponsorshipValidUntil(
      PIMLICO_SINGLETON_PAYMASTER_V8,
      verifyingPaymasterData({
        validUntil: 1_784_869_349n,
        combinedModeByte: 3,
      })
    )).toBeUndefined()
    expect(pimlicoSponsorshipValidUntil(
      PIMLICO_SINGLETON_PAYMASTER_V8,
      '0x90'
    )).toBeUndefined()
    expect(pimlicoSponsorshipValidUntil(
      '0x1111111111111111111111111111111111111111',
      verifyingPaymasterData({ validUntil: 1_784_869_349n })
    )).toBeUndefined()
  })
})

describe('pletherSponsorshipValidUntil', () => {
  it('parses the exact fixed Plether verifying-paymaster envelope', () => {
    const operation = pletherOperation()

    expect(operation.paymasterData &&
      (operation.paymasterData.length - 2) / 2
    ).toBe(PLETHER_PAYMASTER_DATA_BYTES)
    expect(pletherSponsorshipValidUntil(
      PLETHER_PAYMASTER,
      operation
    )).toBe(1_784_869_349n)
  })

  it('fails closed for the wrong address, length, or zero deadline', () => {
    expect(pletherSponsorshipValidUntil(
      '0x9999999999999999999999999999999999999999',
      pletherOperation()
    )).toBeUndefined()
    expect(pletherSponsorshipValidUntil(
      PLETHER_PAYMASTER,
      {
        ...pletherOperation(),
        paymasterData: '0x1234',
      }
    )).toBeUndefined()
    expect(pletherSponsorshipValidUntil(
      PLETHER_PAYMASTER,
      pletherOperation(0n)
    )).toBeUndefined()
  })

  it('fails closed when fixed policy, account runtime, gas, or validity changes', () => {
    const operation = pletherOperation()
    expect(pletherSponsorshipValidUntil(PLETHER_PAYMASTER, {
      ...operation,
      paymasterData: pletherPaymasterData(1_784_869_349n, {
        policyId: `0x${'11'.repeat(32)}`,
      }),
    })).toBeUndefined()
    expect(pletherSponsorshipValidUntil(PLETHER_PAYMASTER, {
      ...operation,
      paymasterData: pletherPaymasterData(1_784_869_349n, {
        accountCodeHash: `0x${'22'.repeat(32)}`,
      }),
    })).toBeUndefined()
    expect(pletherSponsorshipValidUntil(PLETHER_PAYMASTER, {
      ...operation,
      paymasterVerificationGasLimit:
        PLETHER_PAYMASTER_VERIFICATION_GAS_LIMIT + 1n,
    })).toBeUndefined()
    expect(pletherSponsorshipValidUntil(PLETHER_PAYMASTER, {
      ...operation,
      paymasterData: pletherPaymasterData(1_784_869_349n, {
        validAfter: 1_784_868_748n,
      }),
    })).toBeUndefined()
  })

  it(
    'keeps the Pimlico decoder available alongside native-paymaster recovery',
    () => {
      const legacyOperation = {
        ...pletherOperation(),
        paymaster: PIMLICO_SINGLETON_PAYMASTER_V8,
        paymasterData: verifyingPaymasterData({ validUntil: 2_000n }),
      }

      expect(knownSponsorshipValidUntil(
        legacyOperation,
        PLETHER_PAYMASTER
      )).toBe(2_000n)
      expect(knownSponsorshipValidUntil(
        pletherOperation(3_000n),
        PLETHER_PAYMASTER
      )).toBe(3_000n)
    }
  )
})
