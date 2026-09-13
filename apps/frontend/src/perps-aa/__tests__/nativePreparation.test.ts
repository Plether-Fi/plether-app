import { concatHex, numberToHex, type Hex } from 'viem'
import { getUserOperationHash } from 'viem/account-abstraction'
import { describe, expect, it } from 'vitest'
import { preparationIdentifier, validateNativePreparation, SEPOLIA_NATIVE_EXECUTION_GAS_CAP } from '../nativePreparation'
import { PERPS_ENTRY_POINT_V08, type PerpsAaDeploymentManifestV2 } from '../manifest'
import { PLETHER_PAYMASTER_POLICY_ID, PLETHER_SIMPLE_ACCOUNT_PROXY_CODE_HASH } from '../paymasterValidity'
import type { ManagedUserOperation } from '../runtimeContext'

const manifest = { chainId: 421614, entryPoint: PERPS_ENTRY_POINT_V08, version: 'perps-aa-arbitrum-sepolia-v2',
  paymasterAddress: '0x1234567890123456789012345678901234567890', paymasterVersion: 'plether-verifying-v1',
  bundlerRpcUrl: '/api/perps/v1/aa/rpc', paymasterRpcUrl: '/api/perps/v1/aa/rpc',
} as PerpsAaDeploymentManifestV2
const expected = { sender: '0x2222222222222222222222222222222222222222' as const, callData: '0x1234' as const }

function fixture(callGasLimit = 100000n, signedCeiling?: bigint) {
  const now = BigInt(Math.floor(Date.now() / 1000))
  const op: ManagedUserOperation = { ...expected, nonce: 3n, signature: '0x', callGasLimit,
    verificationGasLimit: 100000n, preVerificationGas: 50000n, maxFeePerGas: 1000000n, maxPriorityFeePerGas: 1n,
    paymaster: manifest.paymasterAddress, paymasterVerificationGasLimit: 100000n, paymasterPostOpGasLimit: 0n,
    paymasterData: concatHex([numberToHex(now + 300n, { size: 6 }), numberToHex(now - 30n, { size: 6 }),
      numberToHex(signedCeiling ?? (callGasLimit + 250000n) * 1000000n, { size: 16 }), PLETHER_PAYMASTER_POLICY_ID, PLETHER_SIMPLE_ACCOUNT_PROXY_CODE_HASH,
      `0x${'44'.repeat(65)}` as Hex]),
  }
  const wire = Object.fromEntries(Object.entries(op).filter(([key]) => key !== 'signature')
    .map(([key, value]) => [key, typeof value === 'bigint' ? numberToHex(value) : value]))
  return { version: 1, entryPoint: manifest.entryPoint.toLowerCase(), operation: wire,
    userOperationHash: getUserOperationHash({ userOperation: op, chainId: 421614, entryPointAddress: manifest.entryPoint, entryPointVersion: '0.8' }) }
}

describe('native preparation response binding', () => {
  it.each([2_000_000n, 2_023_995n, SEPOLIA_NATIVE_EXECUTION_GAS_CAP])('accepts the approved Sepolia envelope at %s gas with its exact hash', callGas => {
    expect(validateNativePreparation(fixture(callGas), expected, manifest).callGasLimit).toBe(callGas)
  })
  it('rejects one gas over the cap even with a matching hash and enough authorization', () => {
    expect(() => validateNativePreparation(fixture(SEPOLIA_NATIVE_EXECUTION_GAS_CAP + 1n), expected, manifest)).toThrow('exceeds bounds')
  })
  it('does not widen the paymaster liability allowance', () => {
    expect(() => validateNativePreparation(fixture(2_023_995n, 350000000000n), expected, manifest)).toThrow('economic ceiling')
  })
  it('does not enable native preparation for mainnet', () => {
    expect(() => validateNativePreparation(fixture(2_023_995n), expected, { ...manifest, chainId: 42161 } as unknown as PerpsAaDeploymentManifestV2)).toThrow('restricted to Arbitrum Sepolia')
  })
  it('accepts a correctly bound native operation and reproducible hash', () => {
    expect(validateNativePreparation(fixture(), expected, manifest)).toMatchObject({ ...expected, nonce: 3n, signature: '0x' })
  })
  it.each([
    ['sender', '0x3333333333333333333333333333333333333333'], ['callData', '0xffff'],
    ['factory', '0x3333333333333333333333333333333333333333'], ['signature', '0x'], ['authorization', {}],
    ['nonce', '0x10000000000000000'], ['callGasLimit', '0x0'], ['maxFeePerGas', '0xffffffffffff'],
    ['paymasterData', '0x1234'], ['paymasterPostOpGasLimit', '0x1'], ['maxPriorityFeePerGas', '0xffffffff'],
  ])('rejects mutated %s before wallet signing', (key, value) => {
    const response = fixture()
    response.operation[key] = value
    expect(() => validateNativePreparation(response, expected, manifest)).toThrow()
  })
  it('rejects a changed returned hash or EntryPoint', () => {
    expect(() => validateNativePreparation({ ...fixture(), userOperationHash: '0x00' }, expected, manifest)).toThrow()
    expect(() => validateNativePreparation({ ...fixture(), entryPoint: expected.sender }, expected, manifest)).toThrow()
  })
  it.each(['paymasterVerificationGasLimit', 'paymasterPostOpGasLimit', 'paymasterData'])(
    'rejects missing %s before calculating sponsorship liability', key => {
      const response = fixture()
      delete response.operation[key]
      expect(() => validateNativePreparation(response, expected, manifest)).toThrow()
    },
  )
  it('uses a stable opaque ID for the same journaled attempt', () => {
    expect(preparationIdentifier('attempt-a')).toBe(preparationIdentifier('attempt-a'))
    expect(preparationIdentifier('attempt-a')).not.toBe(preparationIdentifier('attempt-b'))
    expect(preparationIdentifier('attempt-a')).toMatch(/^0x[0-9a-f]{64}$/)
  })
})
