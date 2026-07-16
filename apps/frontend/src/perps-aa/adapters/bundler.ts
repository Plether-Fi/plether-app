import {
  getAddress,
  hexToBigInt,
  isAddress,
  isAddressEqual,
  isHex,
  size,
  type Address,
  type Hex,
} from 'viem'
import type { BundlerAdapter } from '@plether/perps-aa-client'
import { BundlerRequestError } from '../errors'
import {
  JsonRpcRequestError,
  JsonRpcTimeoutError,
  JsonRpcTransportError,
  jsonRpcRequest,
} from '../rpc'
import {
  extractUserOperationTransactionHash,
  serializeUserOperationV08,
  type UserOperationGasEstimateV08,
  type UserOperationReceiptV08,
  type UserOperationV08,
} from '../types'

interface RpcGasEstimate {
  callGasLimit: Hex
  verificationGasLimit: Hex
  preVerificationGas: Hex
  paymasterVerificationGasLimit?: Hex
  paymasterPostOpGasLimit?: Hex
}

export interface PletherBundlerAdapterOptions {
  rpcUrl: string
  pollIntervalMs?: number
  receiptTimeoutMs?: number
  requestTimeoutMs?: number
  fetcher?: typeof fetch
  getSignal?: () => AbortSignal | undefined
  expectedSender?: Address
  expectedPaymaster?: Address
  onUserOperationHash?: (hash: Hex) => void
  onTransactionHash?: (hash: Hex) => void
}

function wait(milliseconds: number, signal?: AbortSignal): Promise<void> {
  return new Promise((resolve, reject) => {
    if (signal?.aborted) {
      reject(
        signal.reason instanceof Error
          ? signal.reason
          : new DOMException('Bundler request cancelled', 'AbortError')
      )
      return
    }

    const onComplete = () => {
      signal?.removeEventListener('abort', onAbort)
      resolve()
    }
    const timeout = globalThis.setTimeout(onComplete, milliseconds)
    const onAbort = () => {
      globalThis.clearTimeout(timeout)
      signal?.removeEventListener('abort', onAbort)
      reject(
        signal?.reason instanceof Error
          ? signal.reason
          : new DOMException('Bundler request cancelled', 'AbortError')
      )
    }
    signal?.addEventListener('abort', onAbort, { once: true })
  })
}

function parseEstimate(value: RpcGasEstimate): UserOperationGasEstimateV08 {
  return {
    callGasLimit: hexToBigInt(value.callGasLimit),
    verificationGasLimit: hexToBigInt(value.verificationGasLimit),
    preVerificationGas: hexToBigInt(value.preVerificationGas),
    ...(value.paymasterVerificationGasLimit !== undefined
      ? {
          paymasterVerificationGasLimit: hexToBigInt(
            value.paymasterVerificationGasLimit
          ),
        }
      : {}),
    ...(value.paymasterPostOpGasLimit !== undefined
      ? {
          paymasterPostOpGasLimit: hexToBigInt(
            value.paymasterPostOpGasLimit
          ),
        }
      : {}),
  }
}

function terminalStatusFromReason(reason: string | undefined) {
  const normalized = reason?.toLowerCase() ?? ''
  if (normalized.includes('replaced')) return 'replaced' as const
  if (normalized.includes('expired')) return 'expired' as const
  if (normalized.includes('dropped')) return 'dropped' as const
  return undefined
}

function asBundlerError(error: unknown): BundlerRequestError {
  if (error instanceof BundlerRequestError) return error

  if (error instanceof JsonRpcRequestError) {
    const reason = error.data?.reason ?? error.data?.status
    return new BundlerRequestError({
      message: error.message,
      retryable: error.data?.retryable ?? true,
      terminalStatus: terminalStatusFromReason(reason),
      replacementUserOperationHash: error.data?.replacementUserOperationHash,
      cause: error,
    })
  }

  if (error instanceof JsonRpcTimeoutError || error instanceof JsonRpcTransportError) {
    return new BundlerRequestError({
      message: error.message,
      retryable: true,
      cause: error,
    })
  }

  return new BundlerRequestError({
    message: error instanceof Error ? error.message : String(error),
    retryable: true,
    cause: error,
  })
}

function receiptSucceeded(receipt: UserOperationReceiptV08): boolean {
  if (receipt.success !== true) return false
  const status = receipt.receipt?.status
  return status === true || status === 'success' || status === '0x1'
}

function parseReceipt(
  value: unknown,
  input: {
    userOperationHash: Hex
    expectedSender?: Address
    expectedPaymaster?: Address
  }
): UserOperationReceiptV08 | null {
  if (value === null) return null
  if (!value || typeof value !== 'object') {
    throw new BundlerRequestError({
      message: 'Bundler returned an invalid UserOperation receipt',
      retryable: true,
    })
  }
  const receipt = value as UserOperationReceiptV08

  if (
    receipt.userOpHash === undefined ||
    !isHex(receipt.userOpHash, { strict: true }) ||
    size(receipt.userOpHash) !== 32 ||
    receipt.userOpHash.toLowerCase() !== input.userOperationHash.toLowerCase()
  ) {
    throw new BundlerRequestError({
      message: 'Bundler receipt UserOperation hash is missing or mismatched',
      retryable: true,
    })
  }
  if (typeof receipt.success !== 'boolean') {
    throw new BundlerRequestError({
      message: 'Bundler receipt is missing the UserOperation success flag',
      retryable: true,
    })
  }
  if (receipt.sender !== undefined && !isAddress(receipt.sender)) {
    throw new BundlerRequestError({
      message: 'Bundler receipt sender is invalid',
      retryable: true,
    })
  }
  if (
    input.expectedSender &&
    (
      receipt.sender === undefined ||
      !isAddressEqual(receipt.sender, input.expectedSender)
    )
  ) {
    throw new BundlerRequestError({
      message: 'Bundler receipt sender does not match the Trading Account',
      retryable: false,
    })
  }
  if (receipt.paymaster !== undefined && !isAddress(receipt.paymaster)) {
    throw new BundlerRequestError({
      message: 'Bundler receipt paymaster is invalid',
      retryable: true,
    })
  }
  if (
    input.expectedPaymaster &&
    (
      receipt.paymaster === undefined ||
      !isAddressEqual(receipt.paymaster, input.expectedPaymaster)
    )
  ) {
    throw new BundlerRequestError({
      message: 'Bundler receipt paymaster does not match the reviewed manifest',
      retryable: false,
    })
  }
  const transactionHash = receipt.receipt?.transactionHash
  if (
    transactionHash === undefined ||
    !isHex(transactionHash, { strict: true }) ||
    size(transactionHash) !== 32
  ) {
    throw new BundlerRequestError({
      message: 'Bundler receipt transaction hash is missing or invalid',
      retryable: true,
    })
  }
  const transactionStatus = receipt.receipt?.status
  if (
    transactionStatus !== true &&
    transactionStatus !== false &&
    transactionStatus !== 'success' &&
    transactionStatus !== 'reverted' &&
    transactionStatus !== '0x1' &&
    transactionStatus !== '0x0'
  ) {
    throw new BundlerRequestError({
      message: 'Bundler transaction receipt status is missing or invalid',
      retryable: true,
    })
  }
  if (!Array.isArray(receipt.receipt?.logs)) {
    throw new BundlerRequestError({
      message: 'Bundler transaction receipt logs are missing or invalid',
      retryable: true,
    })
  }
  return receipt
}

export function createPletherBundlerAdapter(
  options: PletherBundlerAdapterOptions
): BundlerAdapter<
  UserOperationV08,
  UserOperationGasEstimateV08,
  UserOperationReceiptV08
> {
  const request = async <T>(method: string, params: readonly unknown[]): Promise<T> => {
    try {
      return await jsonRpcRequest<T>({
        url: options.rpcUrl,
        method,
        params,
        timeoutMs: options.requestTimeoutMs,
        signal: options.getSignal?.(),
        fetcher: options.fetcher,
      })
    } catch (error) {
      throw asBundlerError(error)
    }
  }

  return {
    estimateUserOperationGas: async ({ operation, entryPoint }) => {
      const result = await request<RpcGasEstimate>(
        'eth_estimateUserOperationGas',
        [
          serializeUserOperationV08(operation, { includeSignature: true }),
          getAddress(entryPoint),
        ]
      )
      return parseEstimate(result)
    },

    sendUserOperation: async ({ operation, entryPoint }) => {
      const hash = await request<Hex>('eth_sendUserOperation', [
        serializeUserOperationV08(operation, { includeSignature: true }),
        getAddress(entryPoint),
      ])
      if (!isHex(hash, { strict: true }) || size(hash) !== 32) {
        throw new BundlerRequestError({
          message: 'Bundler returned an invalid UserOperation hash',
          retryable: true,
        })
      }
      options.onUserOperationHash?.(hash)
      return hash
    },

    waitForUserOperationReceipt: async ({ userOperationHash }) => {
      const startedAt = Date.now()
      const timeoutMs = options.receiptTimeoutMs ?? 120_000
      const pollIntervalMs = options.pollIntervalMs ?? 1_500

      while (Date.now() - startedAt < timeoutMs) {
        const receipt = parseReceipt(
          await request<unknown>('eth_getUserOperationReceipt', [userOperationHash]),
          {
            userOperationHash,
            expectedSender: options.expectedSender,
            expectedPaymaster: options.expectedPaymaster,
          }
        )
        if (receipt) {
          if (!receiptSucceeded(receipt)) {
            throw new BundlerRequestError({
              message: receipt.reason ?? 'The UserOperation reverted during execution',
              retryable: false,
              terminalStatus: 'execution-reverted',
            })
          }

          const transactionHash = extractUserOperationTransactionHash(receipt)
          if (transactionHash) options.onTransactionHash?.(transactionHash)
          return receipt
        }

        await wait(pollIntervalMs, options.getSignal?.())
      }

      throw new BundlerRequestError({
        message: 'Timed out waiting for UserOperation inclusion',
        retryable: true,
        terminalStatus: 'receipt-timeout',
      })
    },
  }
}
