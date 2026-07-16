import {
  getAddress,
  numberToHex,
  type Address,
  type Hex,
  type Log,
} from 'viem'

export interface UserOperationV08 {
  sender: Address
  nonce: bigint
  factory?: Address
  factoryData?: Hex
  callData: Hex
  callGasLimit: bigint
  verificationGasLimit: bigint
  preVerificationGas: bigint
  maxFeePerGas: bigint
  maxPriorityFeePerGas: bigint
  paymaster?: Address
  paymasterVerificationGasLimit?: bigint
  paymasterPostOpGasLimit?: bigint
  paymasterData?: Hex
  signature: Hex
}

export interface RpcUserOperationV08 {
  sender: Address
  nonce: Hex
  factory?: Address
  factoryData?: Hex
  callData: Hex
  callGasLimit: Hex
  verificationGasLimit: Hex
  preVerificationGas: Hex
  maxFeePerGas: Hex
  maxPriorityFeePerGas: Hex
  paymaster?: Address
  paymasterVerificationGasLimit?: Hex
  paymasterPostOpGasLimit?: Hex
  paymasterData?: Hex
  signature?: Hex
}

export interface UserOperationGasEstimateV08 {
  callGasLimit: bigint
  verificationGasLimit: bigint
  preVerificationGas: bigint
  paymasterVerificationGasLimit?: bigint
  paymasterPostOpGasLimit?: bigint
}

export interface UserOperationReceiptV08 {
  userOpHash?: Hex
  sender?: Address
  nonce?: Hex
  paymaster?: Address
  actualGasCost?: Hex
  actualGasUsed?: Hex
  success?: boolean
  reason?: string
  logs?: readonly unknown[]
  receipt?: {
    transactionHash?: Hex
    status?: Hex | 'success' | 'reverted' | boolean
    logs?: readonly Log[]
    [key: string]: unknown
  }
  [key: string]: unknown
}

function assertNonNegative(value: bigint, label: string): void {
  if (value < 0n) {
    throw new Error(`${label} cannot be negative`)
  }
}

function quantity(value: bigint, label: string): Hex {
  assertNonNegative(value, label)
  return numberToHex(value)
}

function validateRelatedFields(operation: UserOperationV08): void {
  if (operation.factoryData && !operation.factory) {
    throw new Error('UserOperation factoryData requires a factory')
  }

  const paymasterFields = [
    operation.paymasterVerificationGasLimit,
    operation.paymasterPostOpGasLimit,
    operation.paymasterData,
  ]
  const hasAnyPaymasterField = operation.paymaster !== undefined ||
    paymasterFields.some((field) => field !== undefined)

  if (hasAnyPaymasterField && !operation.paymaster) {
    throw new Error('UserOperation paymaster fields require a paymaster address')
  }

  if (
    operation.paymaster &&
    (
      operation.paymasterVerificationGasLimit === undefined ||
      operation.paymasterPostOpGasLimit === undefined ||
      operation.paymasterData === undefined
    )
  ) {
    throw new Error('UserOperation paymaster requires both gas limits and paymasterData')
  }
}

export function serializeUserOperationV08(
  operation: UserOperationV08,
  options: { includeSignature: boolean }
): RpcUserOperationV08 {
  validateRelatedFields(operation)

  return {
    sender: getAddress(operation.sender),
    nonce: quantity(operation.nonce, 'UserOperation nonce'),
    ...(operation.factory ? { factory: getAddress(operation.factory) } : {}),
    ...(operation.factoryData ? { factoryData: operation.factoryData } : {}),
    callData: operation.callData,
    callGasLimit: quantity(operation.callGasLimit, 'UserOperation callGasLimit'),
    verificationGasLimit: quantity(
      operation.verificationGasLimit,
      'UserOperation verificationGasLimit'
    ),
    preVerificationGas: quantity(
      operation.preVerificationGas,
      'UserOperation preVerificationGas'
    ),
    maxFeePerGas: quantity(operation.maxFeePerGas, 'UserOperation maxFeePerGas'),
    maxPriorityFeePerGas: quantity(
      operation.maxPriorityFeePerGas,
      'UserOperation maxPriorityFeePerGas'
    ),
    ...(operation.paymaster ? { paymaster: getAddress(operation.paymaster) } : {}),
    ...(operation.paymasterVerificationGasLimit !== undefined
      ? {
          paymasterVerificationGasLimit: quantity(
            operation.paymasterVerificationGasLimit,
            'UserOperation paymasterVerificationGasLimit'
          ),
        }
      : {}),
    ...(operation.paymasterPostOpGasLimit !== undefined
      ? {
          paymasterPostOpGasLimit: quantity(
            operation.paymasterPostOpGasLimit,
            'UserOperation paymasterPostOpGasLimit'
          ),
        }
      : {}),
    ...(operation.paymasterData ? { paymasterData: operation.paymasterData } : {}),
    ...(options.includeSignature ? { signature: operation.signature } : {}),
  }
}

export function extractUserOperationTransactionHash(
  receipt: UserOperationReceiptV08
): Hex | undefined {
  return receipt.receipt?.transactionHash
}
