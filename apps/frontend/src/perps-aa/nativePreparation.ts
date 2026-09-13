import { getAddress, isHex, keccak256, toBytes, hexToBigInt, slice, type Address, type Hex } from 'viem'
import { getUserOperationHash } from 'viem/account-abstraction'
import type { PerpsAaDeploymentManifestV2 } from './manifest'
import { manifestSponsorshipValidUntil } from './paymasterValidity'
import type { ManagedUserOperation } from './runtimeContext'

export const SEPOLIA_NATIVE_EXECUTION_GAS_CAP = 2_100_000n

export interface PreparationBinding {
  sender: Address
  callData: Hex
  factory?: Address
  factoryData?: Hex
}

export function preparationIdentifier(identifier: string): Hex {
  return keccak256(toBytes(`PletherPreparation/v1:${identifier}`))
}

function record(value: unknown): Record<string, unknown> {
  if (!value || typeof value !== 'object' || Array.isArray(value)) throw new Error('Invalid preparation response')
  return value as Record<string, unknown>
}

/** Untrusted server output must not change anything the user reviewed. */
export function validateNativePreparation(
  value: unknown, expected: PreparationBinding, manifest: PerpsAaDeploymentManifestV2,
): ManagedUserOperation {
  if (manifest.chainId !== 421614) throw new Error('Native preparation is restricted to Arbitrum Sepolia')
  const response = record(value)
  if (Object.keys(response).some(key => !['version', 'entryPoint', 'operation', 'userOperationHash'].includes(key))
    || response.version !== 1 || response.entryPoint !== manifest.entryPoint.toLowerCase()) {
    throw new Error('Unsupported preparation response')
  }
  const raw = record(response.operation)
  const quantities = ['nonce', 'callGasLimit', 'verificationGasLimit', 'preVerificationGas', 'maxFeePerGas',
    'maxPriorityFeePerGas', 'paymasterVerificationGasLimit', 'paymasterPostOpGasLimit'] as const
  const bytes = ['sender', 'callData', 'factory', 'factoryData', 'paymaster', 'paymasterData'] as const
  if (Object.keys(raw).some(key => ![...quantities, ...bytes].includes(key as typeof quantities[number]))) {
    throw new Error('Unexpected prepared operation fields')
  }
  const parsed: Record<string, unknown> = { signature: '0x' }
  for (const key of quantities) {
    const v = raw[key]
    if (typeof v !== 'string' || !/^0x(?:0|[1-9a-f][0-9a-f]*)$/.test(v)) throw new Error('Invalid prepared quantity')
    parsed[key] = BigInt(v)
  }
  for (const key of bytes) {
    if (raw[key] === undefined && (key === 'factory' || key === 'factoryData')) continue
    if (typeof raw[key] !== 'string' || !isHex(raw[key], { strict: true }) || raw[key].length % 2) throw new Error('Invalid prepared bytes')
    parsed[key] = raw[key]
  }
  const op = parsed as unknown as ManagedUserOperation
  const { paymasterVerificationGasLimit, paymasterPostOpGasLimit, paymasterData } = op
  if (paymasterVerificationGasLimit === undefined || paymasterPostOpGasLimit === undefined || paymasterData === undefined) {
    throw new Error('Prepared sponsorship fields are required')
  }
  if (getAddress(op.sender) !== getAddress(expected.sender) || op.callData.toLowerCase() !== expected.callData.toLowerCase()
    || op.factory?.toLowerCase() !== expected.factory?.toLowerCase()
    || op.factoryData?.toLowerCase() !== expected.factoryData?.toLowerCase()
    || op.paymaster?.toLowerCase() !== manifest.paymasterAddress.toLowerCase()
    || op.nonce < 0n || op.nonce >= 1n << 64n
    || op.callGasLimit < 1n || op.callGasLimit > SEPOLIA_NATIVE_EXECUTION_GAS_CAP
    || op.verificationGasLimit < 1n || op.verificationGasLimit > 1_000_000n
    || op.preVerificationGas < 1n || op.preVerificationGas > 1_000_000n
    || op.maxFeePerGas < 1n || op.maxFeePerGas > 10_000_000_000n
    || op.maxPriorityFeePerGas > 2_000_000_000n || op.maxPriorityFeePerGas > op.maxFeePerGas) {
    throw new Error('Prepared operation differs from the reviewed intent or exceeds bounds')
  }
  const validUntil = manifestSponsorshipValidUntil(manifest, op)
  if (validUntil === undefined || validUntil <= BigInt(Math.floor(Date.now() / 1000) + 30)) {
    throw new Error('Prepared sponsorship is invalid or expiring')
  }
  const liability = (op.callGasLimit + op.verificationGasLimit + op.preVerificationGas
    + paymasterVerificationGasLimit + paymasterPostOpGasLimit) * op.maxFeePerGas
  const signedCeiling = hexToBigInt(slice(paymasterData, 12, 28))
  if (liability > signedCeiling || signedCeiling > 10_000_000_000_000_000n) {
    throw new Error('Prepared sponsorship exceeds the reviewed economic ceiling')
  }
  const hash = getUserOperationHash({ userOperation: op, chainId: manifest.chainId,
    entryPointAddress: manifest.entryPoint, entryPointVersion: '0.8' })
  if (response.userOperationHash !== hash) throw new Error('Prepared operation hash mismatch')
  return op
}
