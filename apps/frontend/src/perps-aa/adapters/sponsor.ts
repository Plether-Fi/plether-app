import {
  getAddress,
  isAddress,
  isHex,
  numberToHex,
  slice,
  type Address,
  type Hex,
} from 'viem'
import type {
  Eip7677PaymasterResponse,
  PerpsActionKind,
  SponsorAdapter,
} from '@plether/perps-aa-client'
import { asSponsorRequestError, SponsorRequestError } from '../errors'
import { jsonRpcRequest } from '../rpc'
import {
  serializeUserOperationV08,
  type UserOperationV08,
} from '../types'

type SponsorMethod = 'pm_getPaymasterStubData' | 'pm_getPaymasterData'

export interface SponsorPolicyContext {
  manifestVersion: string
  policyId: Hex
  action: PerpsActionKind
}

export interface PletherSponsorAdapterOptions {
  rpcUrl: string
  manifestVersion: string
  policyId: Hex
  expectedPaymaster?: Address
  timeoutMs?: number
  fetcher?: typeof fetch
  getSignal?: () => AbortSignal | undefined
}

function optionalHex(value: unknown, label: string): Hex | undefined {
  if (value === undefined) return undefined
  if (!isHex(value)) throw new Error(`${label} must be hex`)
  return value
}

function optionalAddress(value: unknown, label: string): Address | undefined {
  if (value === undefined) return undefined
  if (typeof value !== 'string' || !isAddress(value)) {
    throw new Error(`${label} must be an address`)
  }
  return getAddress(value)
}

function parsePaymasterResponse(value: unknown): Eip7677PaymasterResponse {
  if (!value || typeof value !== 'object') {
    throw new Error('Paymaster response must be an object')
  }
  const response = value as Record<string, unknown>

  return {
    ...(response.paymasterAndData !== undefined
      ? { paymasterAndData: optionalHex(response.paymasterAndData, 'paymasterAndData') }
      : {}),
    ...(response.paymaster !== undefined
      ? { paymaster: optionalAddress(response.paymaster, 'paymaster') }
      : {}),
    ...(response.paymasterData !== undefined
      ? { paymasterData: optionalHex(response.paymasterData, 'paymasterData') }
      : {}),
    ...(response.paymasterVerificationGasLimit !== undefined
      ? {
          paymasterVerificationGasLimit: optionalHex(
            response.paymasterVerificationGasLimit,
            'paymasterVerificationGasLimit'
          ),
        }
      : {}),
    ...(response.paymasterPostOpGasLimit !== undefined
      ? {
          paymasterPostOpGasLimit: optionalHex(
            response.paymasterPostOpGasLimit,
            'paymasterPostOpGasLimit'
          ),
        }
      : {}),
  }
}

export function createPletherSponsorAdapter(
  options: PletherSponsorAdapterOptions
): SponsorAdapter<UserOperationV08> {
  const request = async (
    method: SponsorMethod,
    input: {
      chainId: number
      entryPoint: Address
      account: Address
      action: PerpsActionKind
      operation: UserOperationV08
    }
  ): Promise<Eip7677PaymasterResponse> => {
    try {
      const context: SponsorPolicyContext = {
        manifestVersion: options.manifestVersion,
        policyId: options.policyId,
        action: input.action,
      }
      const result = await jsonRpcRequest<unknown>({
        url: options.rpcUrl,
        method,
        params: [
          serializeUserOperationV08(input.operation, { includeSignature: false }),
          getAddress(input.entryPoint),
          numberToHex(input.chainId),
          context,
        ],
        timeoutMs: options.timeoutMs,
        signal: options.getSignal?.(),
        fetcher: options.fetcher,
      })
      const response = parsePaymasterResponse(result)
      const splitPaymaster = response.paymaster
      const packedPaymaster = response.paymasterAndData
        ? getAddress(slice(response.paymasterAndData, 0, 20))
        : undefined
      const expectedPaymaster = options.expectedPaymaster
        ? getAddress(options.expectedPaymaster)
        : undefined
      if (
        (splitPaymaster && packedPaymaster &&
          splitPaymaster !== packedPaymaster) ||
        (expectedPaymaster &&
          splitPaymaster !== expectedPaymaster &&
          packedPaymaster !== expectedPaymaster) ||
        (expectedPaymaster &&
          splitPaymaster === undefined &&
          packedPaymaster === undefined)
      ) {
        throw new SponsorRequestError({
          reason: 'ACCOUNT_NOT_TRUSTED',
          message: 'Sponsor response paymaster does not match the reviewed manifest',
          retryable: false,
        })
      }
      return response
    } catch (error) {
      throw asSponsorRequestError(error)
    }
  }

  return {
    getPaymasterStubData: (input) =>
      request('pm_getPaymasterStubData', input),
    getPaymasterData: (input) =>
      request('pm_getPaymasterData', input),
  }
}
