import type {
  SponsoredOperation,
  SponsoredOperationStatus,
} from '../perps-aa'

export function sponsoredOperationActionLabel(
  action: SponsoredOperation['action']
): string {
  return {
    deposit: 'Deposit margin',
    'place-order': 'Commit order',
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
    case 'building':
    case 'requesting-stub':
    case 'estimating':
    case 'requesting-sponsorship':
      return 'Preparing sponsored transaction'
    case 'awaiting-signature':
      return 'Confirm in wallet'
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
      return 'Inclusion timeout'
    case 'cancelled':
      return 'Cancelled locally'
    case 'failed':
      return 'Failed'
  }
}
