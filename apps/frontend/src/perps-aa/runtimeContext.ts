import { createContext, use } from 'react'
import type {
  PerpsActionKind,
  SmartAccountCall,
} from '@plether/perps-aa-client'
import type { Address, Hex } from 'viem'
import type {
  UserOperation,
  UserOperationReceipt,
} from 'viem/account-abstraction'

export type ManagedUserOperation = UserOperation<'0.8'>
export type ManagedUserOperationReceipt = UserOperationReceipt<'0.8'>

export class UserOperationReceiptNotSafeError extends Error {
  readonly receipt: ManagedUserOperationReceipt

  constructor(receipt: ManagedUserOperationReceipt) {
    super('The UserOperation receipt has not reached the canonical safe block')
    this.name = 'UserOperationReceiptNotSafeError'
    this.receipt = receipt
  }
}

export type PimlicoUserOperationStatus =
  | 'not_found'
  | 'not_submitted'
  | 'submitted'
  | 'queued'
  | 'rejected'
  | 'reverted'
  | 'included'
  | 'failed'

export interface PimlicoUserOperationStatusResult {
  status: PimlicoUserOperationStatus
  transactionHash: Hex | null
}

export interface ManagedPimlicoSmartAccount {
  accountAddress: Address
  entryPoint: Address
  prepareUserOperation(input: {
    calls: readonly SmartAccountCall[]
    action: PerpsActionKind
  }): Promise<ManagedUserOperation>
  signUserOperation(
    operation: ManagedUserOperation
  ): Promise<ManagedUserOperation>
  getUserOperationHash(operation: ManagedUserOperation): Hex
  sendUserOperation(operation: ManagedUserOperation): Promise<Hex>
  getUserOperationStatus(
    userOperationHash: Hex
  ): Promise<PimlicoUserOperationStatusResult>
  getUserOperationReceipt(
    userOperationHash: Hex
  ): Promise<ManagedUserOperationReceipt>
}

export interface SponsoredOperationRecoverySnapshot {
  blockNumber: bigint
  blockTimestamp: bigint
  accountNonce: bigint
  userOperationEvidence:
    | {
        kind: 'included'
        success: boolean
        transactionHash: Hex
        blockNumber: bigint
      }
    | {
        kind: 'not-located'
      }
    | {
        kind: 'not-safe-yet'
      }
}

export interface PerpsAaSmartAccountRuntime {
  chainId: number
  ownerAddress: Address
  smartAccount: ManagedPimlicoSmartAccount
  factoryAddress: Address
  accountVersion: string
  accountIndex: string
  manifestVersion?: string
  walletFamily?: string
  walletVersion?: string
  getRecoverySnapshot?(
    userOperationHash: Hex,
    nonceKey?: bigint
  ): Promise<SponsoredOperationRecoverySnapshot>
}

export const PerpsAaRuntimeContext = createContext<
  PerpsAaSmartAccountRuntime | undefined
>(undefined)

export function usePerpsAaRuntime(): PerpsAaSmartAccountRuntime | undefined {
  return use(PerpsAaRuntimeContext)
}
