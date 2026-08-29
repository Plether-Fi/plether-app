import type { Address } from 'viem'
import { describe, expect, it } from 'vitest'
import {
  persistManagedUserOperation,
  readPersistedManagedUserOperation,
} from '../persistedUserOperation'
import type { ManagedUserOperation } from '../runtimeContext'

const ACCOUNT = '0x2222222222222222222222222222222222222222' as Address
const FACTORY = '0x3333333333333333333333333333333333333333' as Address
const PAYMASTER = '0x888888888888Ec68A58AB8094Cc1AD20Ba3D2402' as Address

function operation(): ManagedUserOperation {
  return {
    sender: ACCOUNT,
    nonce: 7n,
    factory: FACTORY,
    factoryData: '0x1234',
    callData: '0x5678',
    callGasLimit: 1n,
    verificationGasLimit: 2n,
    preVerificationGas: 3n,
    maxFeePerGas: 4n,
    maxPriorityFeePerGas: 5n,
    paymaster: PAYMASTER,
    paymasterVerificationGasLimit: 6n,
    paymasterPostOpGasLimit: 7n,
    paymasterData: '0x90',
    signature: '0xdeadbeef',
  }
}

describe('persisted UserOperation recovery preimage', () => {
  it('round-trips every v0.8 hash-preimage field without bigint JSON values', () => {
    const persisted = persistManagedUserOperation(operation())

    expect(persisted).toMatchObject({
      nonce: '7',
      callGasLimit: '1',
      verificationGasLimit: '2',
      preVerificationGas: '3',
      maxFeePerGas: '4',
      maxPriorityFeePerGas: '5',
      paymasterVerificationGasLimit: '6',
      paymasterPostOpGasLimit: '7',
    })
    expect(JSON.stringify(persisted)).not.toContain('"7n"')
    expect(readPersistedManagedUserOperation(persisted)).toEqual(operation())
  })

  it('rejects noncanonical integers and partial paymaster metadata', () => {
    const persisted = persistManagedUserOperation(operation())

    expect(readPersistedManagedUserOperation({
      ...persisted,
      nonce: '07',
    })).toBeUndefined()
    expect(readPersistedManagedUserOperation({
      ...persisted,
      paymasterData: undefined,
    })).toBeUndefined()
    expect(readPersistedManagedUserOperation({
      ...persisted,
      callGasLimit: (1n << 256n).toString(),
    })).toBeUndefined()
  })

  it('refuses unsupported authorization-bearing operations before submission', () => {
    expect(() => persistManagedUserOperation({
      ...operation(),
      authorization: {} as never,
    })).toThrow('Authorization-bearing UserOperations are not supported')
  })
})
