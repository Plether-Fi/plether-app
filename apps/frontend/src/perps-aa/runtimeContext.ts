import { createContext, useContext } from 'react'
import type { SmartAccountAdapter } from '@plether/perps-aa-client'
import type { Address, Hex } from 'viem'
import type {
  UserOperationGasEstimateV08,
  UserOperationV08,
} from './types'

export interface PerpsAaSmartAccountRuntime {
  chainId: number
  ownerAddress: Address
  smartAccount: SmartAccountAdapter<
    UserOperationV08,
    UserOperationGasEstimateV08
  >
  factoryAddress: Address | null
  implementationAddress: Address
  implementationVersion: string
  accountRuntimeCodeHash: Hex
  walletFamily?: string
  walletVersion?: string
}

export const PerpsAaRuntimeContext = createContext<
  PerpsAaSmartAccountRuntime | undefined
>(undefined)

export function usePerpsAaRuntime(): PerpsAaSmartAccountRuntime | undefined {
  return useContext(PerpsAaRuntimeContext)
}
