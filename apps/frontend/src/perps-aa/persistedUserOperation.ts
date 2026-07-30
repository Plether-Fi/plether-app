import {
  getAddress,
  isAddress,
  isHex,
  type Address,
  type Hex,
} from 'viem'
import type { ManagedUserOperation } from './runtimeContext'

const MAX_UINT256 = (1n << 256n) - 1n

export interface PersistedManagedUserOperationV1 {
  sender: Address
  nonce: string
  factory?: Address
  factoryData?: Hex
  callData: Hex
  callGasLimit: string
  verificationGasLimit: string
  preVerificationGas: string
  maxFeePerGas: string
  maxPriorityFeePerGas: string
  paymaster?: Address
  paymasterVerificationGasLimit?: string
  paymasterPostOpGasLimit?: string
  paymasterData?: Hex
  signature: Hex
}

function canonicalUint(value: unknown): bigint | undefined {
  if (
    typeof value !== 'string' ||
    !/^(0|[1-9]\d*)$/.test(value)
  ) {
    return undefined
  }
  const parsed = BigInt(value)
  return parsed <= MAX_UINT256 ? parsed : undefined
}

function strictAddress(value: unknown): Address | undefined {
  return typeof value === 'string' &&
    isAddress(value, { strict: true })
    ? getAddress(value)
    : undefined
}

function strictHex(value: unknown): Hex | undefined {
  return typeof value === 'string' &&
    isHex(value, { strict: true })
    ? value
    : undefined
}

export function persistManagedUserOperation(
  operation: ManagedUserOperation
): PersistedManagedUserOperationV1 {
  if (operation.authorization !== undefined) {
    throw new Error(
      'Authorization-bearing UserOperations are not supported by recovery'
    )
  }
  return {
    sender: getAddress(operation.sender),
    nonce: operation.nonce.toString(),
    factory: operation.factory
      ? getAddress(operation.factory)
      : undefined,
    factoryData: operation.factoryData,
    callData: operation.callData,
    callGasLimit: operation.callGasLimit.toString(),
    verificationGasLimit: operation.verificationGasLimit.toString(),
    preVerificationGas: operation.preVerificationGas.toString(),
    maxFeePerGas: operation.maxFeePerGas.toString(),
    maxPriorityFeePerGas: operation.maxPriorityFeePerGas.toString(),
    paymaster: operation.paymaster
      ? getAddress(operation.paymaster)
      : undefined,
    paymasterVerificationGasLimit:
      operation.paymasterVerificationGasLimit?.toString(),
    paymasterPostOpGasLimit:
      operation.paymasterPostOpGasLimit?.toString(),
    paymasterData: operation.paymasterData,
    signature: operation.signature,
  }
}

export function readPersistedManagedUserOperation(
  value: unknown
): ManagedUserOperation | undefined {
  if (!value || typeof value !== 'object') return undefined
  const record = value as Record<string, unknown>

  const sender = strictAddress(record.sender)
  const nonce = canonicalUint(record.nonce)
  const callData = strictHex(record.callData)
  const callGasLimit = canonicalUint(record.callGasLimit)
  const verificationGasLimit = canonicalUint(record.verificationGasLimit)
  const preVerificationGas = canonicalUint(record.preVerificationGas)
  const maxFeePerGas = canonicalUint(record.maxFeePerGas)
  const maxPriorityFeePerGas = canonicalUint(record.maxPriorityFeePerGas)
  const signature = strictHex(record.signature)
  if (
    !sender ||
    nonce === undefined ||
    !callData ||
    callGasLimit === undefined ||
    verificationGasLimit === undefined ||
    preVerificationGas === undefined ||
    maxFeePerGas === undefined ||
    maxPriorityFeePerGas === undefined ||
    !signature
  ) {
    return undefined
  }

  const hasFactoryField =
    record.factory !== undefined || record.factoryData !== undefined
  const factory = strictAddress(record.factory)
  const factoryData = strictHex(record.factoryData)
  if (hasFactoryField && (!factory || !factoryData)) return undefined

  const hasPaymasterField =
    record.paymaster !== undefined ||
    record.paymasterVerificationGasLimit !== undefined ||
    record.paymasterPostOpGasLimit !== undefined ||
    record.paymasterData !== undefined
  const paymaster = strictAddress(record.paymaster)
  const paymasterVerificationGasLimit = canonicalUint(
    record.paymasterVerificationGasLimit
  )
  const paymasterPostOpGasLimit = canonicalUint(
    record.paymasterPostOpGasLimit
  )
  const paymasterData = strictHex(record.paymasterData)
  if (
    hasPaymasterField &&
    (
      !paymaster ||
      paymasterVerificationGasLimit === undefined ||
      paymasterPostOpGasLimit === undefined ||
      !paymasterData
    )
  ) {
    return undefined
  }

  return {
    sender,
    nonce,
    ...(factory && factoryData
      ? { factory, factoryData }
      : {}),
    callData,
    callGasLimit,
    verificationGasLimit,
    preVerificationGas,
    maxFeePerGas,
    maxPriorityFeePerGas,
    ...(paymaster &&
      paymasterVerificationGasLimit !== undefined &&
      paymasterPostOpGasLimit !== undefined &&
      paymasterData
      ? {
          paymaster,
          paymasterVerificationGasLimit,
          paymasterPostOpGasLimit,
          paymasterData,
        }
      : {}),
    signature,
  }
}
