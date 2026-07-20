import { createContext, useContext } from 'react'
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

export interface PerpsAaSmartAccountRuntime {
  chainId: number
  ownerAddress: Address
  smartAccount: ManagedPimlicoSmartAccount
  factoryAddress: Address
  accountVersion: string
  accountIndex: string
  walletFamily?: string
  walletVersion?: string
}

export const PerpsAaRuntimeContext = createContext<
  PerpsAaSmartAccountRuntime | undefined
>(undefined)

export function usePerpsAaRuntime(): PerpsAaSmartAccountRuntime | undefined {
  return useContext(PerpsAaRuntimeContext)
}
