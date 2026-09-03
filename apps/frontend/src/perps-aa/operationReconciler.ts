import type { Hex } from 'viem'
import type {
  ManagedUserOperationReceipt,
  PerpsAaSmartAccountRuntime,
  BundlerUserOperationStatus,
} from './runtimeContext'
import { UserOperationReceiptNotSafeError } from './runtimeContext'

export type UserOperationReconciliationOutcome =
  | {
      kind: 'pending'
      status: BundlerUserOperationStatus
      transactionHash: Hex | null
    }
  | {
      kind: 'confirmed'
      receipt: ManagedUserOperationReceipt
      transactionHash: Hex
    }
  | {
      kind: 'included'
      receipt: ManagedUserOperationReceipt
      transactionHash: Hex
    }
  | {
      kind: 'terminal'
      terminalStatus: 'execution-reverted' | 'dropped'
      message: string
      transactionHash: Hex | null
    }

function outcomeFromReceipt(
  receipt: ManagedUserOperationReceipt
): Extract<
  UserOperationReconciliationOutcome,
  { kind: 'confirmed' | 'terminal' }
> {
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
      transactionHash: receipt.receipt.transactionHash,
    }
  }
  return {
    kind: 'confirmed',
    receipt,
    transactionHash: receipt.receipt.transactionHash,
  }
}

function isReceiptNotFoundError(error: unknown): boolean {
  let current = error
  const seen = new Set<object>()

  for (let depth = 0; depth < 8 && current !== undefined; depth += 1) {
    if (!current || typeof current !== 'object' || seen.has(current)) {
      return false
    }
    seen.add(current)
    if (
      (current as { name?: unknown }).name ===
      'UserOperationReceiptNotFoundError'
    ) {
      return true
    }
    current = (current as { cause?: unknown }).cause
  }
  return false
}

type ExactReceiptResult =
  | {
      kind: 'safe'
      receipt: ManagedUserOperationReceipt
    }
  | {
      kind: 'included'
      receipt: ManagedUserOperationReceipt
    }
  | {
      kind: 'not-found'
    }

async function getExactReceipt(input: {
  runtime: PerpsAaSmartAccountRuntime
  userOperationHash: Hex
}): Promise<ExactReceiptResult> {
  try {
    return {
      kind: 'safe',
      receipt: await input.runtime.smartAccount.getUserOperationReceipt(
        input.userOperationHash
      ),
    }
  } catch (error) {
    if (error instanceof UserOperationReceiptNotSafeError) {
      return {
        kind: 'included',
        receipt: error.receipt,
      }
    }
    if (isReceiptNotFoundError(error)) return { kind: 'not-found' }
    throw error
  }
}

export async function reconcileUserOperation(input: {
  runtime: PerpsAaSmartAccountRuntime
  userOperationHash: Hex
}): Promise<UserOperationReconciliationOutcome> {
  // The exact receipt is authoritative and must win over stale bundler status
  // data. Only a typed not-found result permits status/expiry reconciliation;
  // transport, decoding, and account-mismatch failures stay fail-closed.
  const exactReceipt = await getExactReceipt(input)
  if (exactReceipt.kind === 'safe') {
    return outcomeFromReceipt(exactReceipt.receipt)
  }
  if (exactReceipt.kind === 'included') {
    return {
      kind: 'included',
      receipt: exactReceipt.receipt,
      transactionHash: exactReceipt.receipt.receipt.transactionHash,
    }
  }

  const status = await input.runtime.smartAccount.getUserOperationStatus(
    input.userOperationHash
  )

  // Vendor status is diagnostic only. Rejected, failed, and reverted do not
  // prove canonical inclusion or that the signed operation can never land.
  // A vendor-only "included" status remains pending. The separate `included`
  // outcome above requires an exact receipt whose transaction and EntryPoint
  // event match the canonical latest chain, but only a safe receipt or the
  // hash-bound nonce/expiry evidence may resolve the durable lane.
  return {
    kind: 'pending',
    status: status.status,
    transactionHash: status.transactionHash,
  }
}

export type PimlicoReconciliationOutcome = UserOperationReconciliationOutcome
/** @deprecated Use reconcileUserOperation. */
export const reconcilePimlicoUserOperation = reconcileUserOperation
