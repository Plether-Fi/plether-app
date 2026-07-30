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

function outcomeFromReceipt(
  receipt: ManagedUserOperationReceipt
): Extract<
  PimlicoReconciliationOutcome,
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

async function getExactReceiptOrUndefined(input: {
  runtime: PerpsAaSmartAccountRuntime
  userOperationHash: Hex
}): Promise<ManagedUserOperationReceipt | undefined> {
  try {
    return await input.runtime.smartAccount.getUserOperationReceipt(
      input.userOperationHash
    )
  } catch (error) {
    if (isReceiptNotFoundError(error)) return undefined
    throw error
  }
}

export async function reconcilePimlicoUserOperation(input: {
  runtime: PerpsAaSmartAccountRuntime
  userOperationHash: Hex
}): Promise<PimlicoReconciliationOutcome> {
  // The exact receipt is authoritative and must win over stale Pimlico status
  // data. Only a typed not-found result permits status/expiry reconciliation;
  // transport, decoding, and account-mismatch failures stay fail-closed.
  const exactReceipt = await getExactReceiptOrUndefined(input)
  if (exactReceipt) return outcomeFromReceipt(exactReceipt)

  const status = await input.runtime.smartAccount.getUserOperationStatus(
    input.userOperationHash
  )

  // Vendor status is diagnostic only. Rejected, failed, and reverted do not
  // prove canonical inclusion or that the signed operation can never land.
  // Even "included" remains pending until the runtime returns a receipt whose
  // EntryPoint event and transaction are canonical at the safe block.
  // Keep the lane ambiguous until an exact receipt/event or hash-bound
  // nonce/expiry evidence resolves it.
  return {
    kind: 'pending',
    status: status.status,
    transactionHash: status.transactionHash,
  }
}
