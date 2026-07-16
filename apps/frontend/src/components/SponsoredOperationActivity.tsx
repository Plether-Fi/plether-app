import {
  canCancelSponsoredOperationLocally,
  sponsorReasonMessage,
  SponsorRequestError,
  cancelSponsoredOperationRequest,
  usePerpsIdentity,
  useSponsoredOperationStore,
} from '../perps-aa'
import { sponsoredOperationStatusLabel } from '../utils/sponsoredOperation'

function actionLabel(action: string): string {
  return {
    deposit: 'Deposit margin',
    'place-order': 'Commit order',
    'add-margin': 'Add position margin',
    withdraw: 'Withdraw margin',
    'withdraw-to-owner': 'Withdraw to Owner Wallet',
    'settle-claim': 'Settle trader claim',
  }[action] ?? action
}

function abbreviatedHash(hash: string): string {
  return `${hash.slice(0, 10)}…${hash.slice(-8)}`
}

export function SponsoredOperationActivity() {
  const identity = usePerpsIdentity()
  const operations = useSponsoredOperationStore((state) => state.operations)
  if (!identity.isAaManifestConfigured || !identity.accountAddress) return null
  const accountAddress = identity.accountAddress

  const operation = operations
    .filter((item) =>
      item.accountAddress.toLowerCase() === accountAddress.toLowerCase()
    )
    .sort((a, b) => b.updatedAt - a.updatedAt)
    .at(0)
  if (!operation) return null

  const manifest = identity.manifest
  const userOperationUrl = operation.userOperationHash && manifest
    ? manifest.userOperationExplorerUrlTemplate.replace(
        '{userOperationHash}',
        operation.userOperationHash
      )
    : undefined
  const transactionUrl = operation.transactionHash && manifest
    ? manifest.transactionExplorerUrlTemplate.replace(
        '{transactionHash}',
        operation.transactionHash
      )
    : undefined
  const reasonMessage = operation.reason
    ? sponsorReasonMessage(new SponsorRequestError({
        reason: operation.reason,
        message: operation.reason,
        retryable: operation.retryable ?? false,
      }))
    : undefined
  const canCancelLocally = canCancelSponsoredOperationLocally(operation)

  return (
    <section className="border border-brand-border/30 bg-surface-panel px-4 py-3 text-sm">
      <div className="flex flex-wrap items-center justify-between gap-3">
        <div>
          <div className="font-semibold text-content-primary">
            {actionLabel(operation.action)}
          </div>
          <div className="mt-1 text-content-secondary">
            {sponsoredOperationStatusLabel(operation.status)}
          </div>
        </div>
        {operation.sponsorshipAccepted ? (
          <div className="text-right">
            <div className="font-semibold text-positive">Sponsored by Plether</div>
            <div className="text-xs text-content-secondary">0 ETH network gas · USDC costs still apply</div>
          </div>
        ) : null}
        {canCancelLocally ? (
          <button
            type="button"
            className="border border-brand-border/40 px-3 py-1.5 text-xs font-semibold text-content-secondary hover:border-[#FFAB96] hover:text-[#FFAB96]"
            onClick={() => {
              cancelSponsoredOperationRequest(operation.id)
            }}
          >
            Cancel local request
          </button>
        ) : null}
      </div>

      {operation.userOperationHash || operation.transactionHash ? (
        <div className="mt-3 flex flex-wrap gap-x-5 gap-y-2 text-xs">
          {operation.userOperationHash ? (
            userOperationUrl ? (
              <a
                href={userOperationUrl}
                target="_blank"
                rel="noreferrer"
                className="text-[#FFAB96] hover:underline"
              >
                UserOperation {abbreviatedHash(operation.userOperationHash)}
              </a>
            ) : (
              <span className="text-content-secondary">
                UserOperation {abbreviatedHash(operation.userOperationHash)}
              </span>
            )
          ) : null}
          {operation.transactionHash ? (
            transactionUrl ? (
              <a
                href={transactionUrl}
                target="_blank"
                rel="noreferrer"
                className="text-[#FFAB96] hover:underline"
              >
                Transaction {abbreviatedHash(operation.transactionHash)}
              </a>
            ) : (
              <span className="text-content-secondary">
                Transaction {abbreviatedHash(operation.transactionHash)}
              </span>
            )
          ) : null}
        </div>
      ) : null}

      {reasonMessage ? (
        <p className="mt-3 border border-brand-orange/30 bg-brand-orange/10 p-3 text-content-secondary">
          {reasonMessage}
          {operation.retryable ? ' Retry the same Trading Account action or contact support.' : ''}
        </p>
      ) : null}
    </section>
  )
}
