import type { SponsoredOperationStatus } from '../perps-aa'

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
    case 'receipt-timeout':
      return 'Inclusion timeout'
    case 'cancelled':
      return 'Cancelled locally'
    case 'failed':
      return 'Failed'
  }
}
