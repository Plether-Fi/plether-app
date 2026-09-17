import type { SponsoredOperation } from '../perps-aa/operationStore'

export function accountOperationGuidance(operation: SponsoredOperation, now: number) {
  const elapsed = now - (operation.statusTimestamps[operation.status] ?? operation.createdAt)
  if (operation.status === 'receipt-timeout' || operation.status === 'outcome-unknown'
    || (operation.userOperationHash && elapsed >= 60_000)) {
    return {
      attention: true,
      title: 'Check your previous transaction',
      description: 'We haven’t confirmed this transaction yet. Check its status before trading again. You may need to verify your wallet with a gas-free message.',
      action: 'Check transaction status',
    }
  }
  if (operation.reason === 'ACCOUNT_DEPLOYMENT_PENDING') {
    return {
      attention: false,
      title: 'Confirming your Trading Account',
      description: 'Your account setup needs confirmation before this transaction can continue. View its progress and the next step.',
      action: 'View account setup',
    }
  }
  if (['signature-declined', 'preparation-pending', 'sponsorship-refused'].includes(operation.status)) {
    return {
      attention: true,
      title: 'A saved transaction needs attention',
      description: 'Review this earlier transaction to see whether you can continue it or safely discard it. Resolve it before starting another trade.',
      action: 'Review saved transaction',
    }
  }
  if (operation.status === 'awaiting-signature') {
    return {
      attention: false,
      title: 'Finish the request in your wallet',
      description: 'This transaction is waiting for your signature. Open your wallet to approve or reject the request. View the transaction if no request is visible.',
      action: 'View pending transaction',
    }
  }
  if (operation.status === 'confirming' || operation.status === 'submitting') {
    return {
      attention: false,
      title: operation.status === 'confirming' ? 'Waiting for transaction confirmation' : 'Sending your transaction',
      description: 'We’re checking this transaction before allowing another action from your Trading Account. You can view its progress below.',
      action: 'View transaction progress',
    }
  }
  return {
    attention: elapsed >= 60_000,
    title: elapsed >= 60_000 ? 'Check your pending transaction' : 'Preparing your transaction',
    description: elapsed >= 60_000
      ? 'This transaction is still pending. Open its details to check what needs to happen next.'
      : 'This earlier action is being prepared. View its progress before starting another trade.',
    action: 'View pending transaction',
  }
}
