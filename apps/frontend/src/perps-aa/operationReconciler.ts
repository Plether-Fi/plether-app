import type { Hex } from 'viem'
import type {
  ManagedUserOperationReceipt,
  PerpsAaSmartAccountRuntime,
  PimlicoUserOperationStatus,
} from './runtimeContext'

export type PimlicoReconciliationOutcome =
  | {
      kind: 'pending'
      status: PimlicoUserOperationStatus
      transactionHash: Hex | null
    }
  | {
      kind: 'confirmed'
      receipt: ManagedUserOperationReceipt
      transactionHash: Hex
    }
  | {
      kind: 'terminal'
      terminalStatus: 'execution-reverted' | 'dropped'
      message: string
      transactionHash: Hex | null
    }

export async function reconcilePimlicoUserOperation(input: {
  runtime: PerpsAaSmartAccountRuntime
  userOperationHash: Hex
}): Promise<PimlicoReconciliationOutcome> {
  const status = await input.runtime.smartAccount.getUserOperationStatus(
    input.userOperationHash
  )

  if (status.status === 'included') {
    const receipt =
      await input.runtime.smartAccount.getUserOperationReceipt(
        input.userOperationHash
      )
    if (
      !receipt.success ||
      receipt.receipt.status !== 'success'
    ) {
      return {
        kind: 'terminal',
        terminalStatus: 'execution-reverted',
        message:
          receipt.reason ??
          'The UserOperation reverted during execution',
        transactionHash:
          status.transactionHash ?? receipt.receipt.transactionHash,
      }
    }
    return {
      kind: 'confirmed',
      receipt,
      transactionHash: receipt.receipt.transactionHash,
    }
  }

  if (status.status === 'reverted') {
    return {
      kind: 'terminal',
      terminalStatus: 'execution-reverted',
      message: 'Pimlico reports that the UserOperation reverted',
      transactionHash: status.transactionHash,
    }
  }
  if (status.status === 'rejected' || status.status === 'failed') {
    return {
      kind: 'terminal',
      terminalStatus: 'dropped',
      message: `Pimlico reports that the UserOperation was ${status.status}`,
      transactionHash: status.transactionHash,
    }
  }

  return {
    kind: 'pending',
    status: status.status,
    transactionHash: status.transactionHash,
  }
}
