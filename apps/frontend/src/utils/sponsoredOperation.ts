import type {
  SponsoredOperation,
  SponsoredOperationStatus,
} from '../perps-aa'
import { isDefinitiveSponsorshipRefusal } from '../perps-aa/errors'

/** Older journals classified lost responses and unusable retries as refusals. */
export function sponsoredOperationDisplayStatus(operation: SponsoredOperation): SponsoredOperationStatus {
  return operation.nativePreparation && operation.status === 'sponsorship-refused'
    && !isDefinitiveSponsorshipRefusal(operation.reason)
    ? 'preparation-pending' : operation.status
}

export function sponsoredOperationActionLabel(
  action: SponsoredOperation['action']
): string {
  return {
    deposit: 'Deposit margin',
    'place-order': 'Commit order',
    'place-protected-order': 'Open with TP/SL',
    'create-protection': 'Add TP/SL',
    'replace-protection': 'Edit TP/SL',
    'cancel-protection': 'Cancel TP/SL',
    'cancel-order': 'Cancel order',
    'add-margin': 'Add position margin',
    withdraw: 'Withdraw margin',
    'withdraw-to-owner': 'Withdraw to Owner Wallet',
    'settle-claim': 'Settle trader claim',
  }[action]
}

export function sponsoredOperationStatusLabel(
  status: SponsoredOperationStatus
): string {
  switch (status) {
    case 'signature-declined': return 'Signature declined'
    case 'preparation-pending': return 'Preparation recovery'
    case 'sponsorship-refused': return 'Sponsorship unavailable'
    case 'building':
    case 'requesting-stub':
    case 'estimating':
    case 'requesting-sponsorship':
      return 'Preparing sponsored transaction'
    case 'awaiting-signature':
      return 'Awaiting signature'
    case 'journaling':
      return 'Saving recovery record'
    case 'submitting':
      return 'Submitting'
    case 'confirming':
      return 'Pending onchain'
    case 'confirmed':
      return 'Confirmed'
    case 'execution-reverted':
      return 'Failed onchain'
    case 'dropped':
      return 'Dropped by bundler'
    case 'replaced':
      return 'Replaced'
    case 'expired':
      return 'Expired'
    case 'outcome-unknown':
      return 'Outcome unknown'
    case 'receipt-timeout':
      return 'Status unknown — checking'
    case 'cancelled':
      return 'Cancelled locally'
    case 'failed':
      return 'Failed'
  }
}
