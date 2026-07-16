import { useState } from 'react'
import {
  canCancelSponsoredOperationLocally,
  isSponsoredOperationTerminal,
  sponsorReasonMessage,
  SponsorRequestError,
  cancelSponsoredOperationRequest,
  type SponsoredOperation,
  type SponsoredOperationStatus,
  usePerpsIdentity,
  useSponsoredOperationStore,
} from '../perps-aa'
import {
  sponsoredOperationActionLabel,
  sponsoredOperationStatusLabel,
} from '../utils/sponsoredOperation'
import { Badge, Modal } from './ui'

function abbreviatedHash(hash: string): string {
  return `${hash.slice(0, 10)}…${hash.slice(-8)}`
}

function formatOperationTime(timestamp: number): string {
  return new Date(timestamp).toLocaleString(undefined, {
    dateStyle: 'medium',
    timeStyle: 'short',
  })
}

function isFailedStatus(status: SponsoredOperationStatus): boolean {
  return status !== 'confirmed' && isSponsoredOperationTerminal(status)
}

function statusBadgeVariant(
  status: SponsoredOperationStatus
): 'default' | 'success' | 'warning' | 'danger' | 'info' {
  if (status === 'confirmed') return 'success'
  if (isFailedStatus(status)) return 'danger'
  if (status === 'receipt-timeout') return 'warning'
  if (status === 'awaiting-signature') return 'info'
  return 'warning'
}

function HashActions({
  hash,
  label,
  explorerUrl,
}: {
  hash: string
  label: string
  explorerUrl?: string
}) {
  return (
    <div className="grid min-w-0 grid-cols-[auto_minmax(0,1fr)] items-center gap-x-3 gap-y-1 text-xs">
      <span className="text-content-secondary">{label}</span>
      <span className="flex min-w-0 items-center justify-end gap-1">
        <span className="min-w-0 truncate text-content-primary" title={hash}>
          {abbreviatedHash(hash)}
        </span>
        <button
          type="button"
          aria-label={`Copy ${label} hash`}
          title={`Copy ${label} hash`}
          className="inline-flex h-5 w-5 shrink-0 items-center justify-center text-content-secondary transition-colors hover:text-[#FFAB96]"
          onClick={() => {
            void navigator.clipboard.writeText(hash)
          }}
        >
          <span className="material-symbols-outlined !text-[14px] !leading-none">
            content_copy
          </span>
        </button>
        {explorerUrl ? (
          <a
            aria-label={`Open ${label} in block explorer`}
            title={`Open ${label} in block explorer`}
            href={explorerUrl}
            target="_blank"
            rel="noopener noreferrer"
            className="inline-flex h-5 w-5 shrink-0 items-center justify-center text-content-secondary transition-colors hover:text-[#FFAB96]"
          >
            <span className="material-symbols-outlined !text-[14px] !leading-none">
              open_in_new
            </span>
          </a>
        ) : null}
      </span>
    </div>
  )
}

function AddressRow({
  address,
  label,
}: {
  address: string
  label: string
}) {
  return (
    <div className="grid min-w-0 grid-cols-[auto_minmax(0,1fr)] items-center gap-3 text-sm">
      <span className="text-content-secondary">{label}</span>
      <span className="flex min-w-0 items-center justify-end gap-1">
        <span className="min-w-0 truncate text-content-primary" title={address}>
          {abbreviatedHash(address)}
        </span>
        <button
          type="button"
          aria-label={`Copy ${label} address`}
          title={`Copy ${label} address`}
          className="inline-flex h-5 w-5 shrink-0 items-center justify-center text-content-secondary transition-colors hover:text-[#FFAB96]"
          onClick={() => {
            void navigator.clipboard.writeText(address)
          }}
        >
          <span className="material-symbols-outlined !text-[14px] !leading-none">
            content_copy
          </span>
        </button>
      </span>
    </div>
  )
}

function OperationHistoryItem({
  operation,
  manifest,
}: {
  operation: SponsoredOperation
  manifest: ReturnType<typeof usePerpsIdentity>['manifest']
}) {
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
  const replacementUserOperationUrl =
    operation.replacementUserOperationHash && manifest
      ? manifest.userOperationExplorerUrlTemplate.replace(
          '{userOperationHash}',
          operation.replacementUserOperationHash
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
    <article
      className="space-y-3 border border-brand-border/30 bg-app-bg/30 p-4"
      data-operation-id={operation.id}
    >
      <div className="min-w-0">
        <h3 className="font-semibold text-content-primary">
          {sponsoredOperationActionLabel(operation.action)}
        </h3>
        <div className="mt-1 flex flex-wrap items-center gap-x-3 gap-y-1">
          <time
            dateTime={new Date(operation.updatedAt).toISOString()}
            className="text-xs text-content-secondary"
          >
            {formatOperationTime(operation.updatedAt)}
          </time>
          <Badge variant={statusBadgeVariant(operation.status)}>
            {sponsoredOperationStatusLabel(operation.status)}
          </Badge>
          {operation.sponsorshipAccepted ? (
            <span className="text-xs text-positive">
              Sponsored by Plether · 0 ETH network gas
            </span>
          ) : null}
        </div>
      </div>

      {operation.userOperationHash ? (
        <HashActions
          hash={operation.userOperationHash}
          label="UserOperation"
          explorerUrl={userOperationUrl}
        />
      ) : null}
      {operation.transactionHash ? (
        <HashActions
          hash={operation.transactionHash}
          label="Transaction"
          explorerUrl={transactionUrl}
        />
      ) : null}
      {operation.replacementUserOperationHash ? (
        <HashActions
          hash={operation.replacementUserOperationHash}
          label="Replacement UserOperation"
          explorerUrl={replacementUserOperationUrl}
        />
      ) : null}

      {operation.action === 'place-order' && operation.status === 'confirmed' ? (
        <p className="text-xs leading-5 text-content-secondary">
          The sponsored order commit is confirmed. Keeper execution is tracked separately in order history.
        </p>
      ) : null}

      {reasonMessage ? (
        <p className="border border-brand-orange/30 bg-brand-orange/10 p-3 text-xs leading-5 text-content-secondary">
          {reasonMessage}
          {operation.retryable
            ? ' Retry the same Trading Account action or contact support.'
            : ''}
        </p>
      ) : null}

      {canCancelLocally ? (
        <button
          type="button"
          className="border border-brand-border/40 px-3 py-1.5 text-xs font-semibold text-content-secondary transition-colors hover:border-[#FFAB96] hover:text-[#FFAB96]"
          onClick={() => {
            cancelSponsoredOperationRequest(operation.id)
          }}
        >
          Cancel local request
        </button>
      ) : null}
    </article>
  )
}

export function SponsoredOperationHistoryButton() {
  const identity = usePerpsIdentity()
  const operations = useSponsoredOperationStore((state) => state.operations)
  const [openedForIdentity, setOpenedForIdentity] = useState<string | null>(null)
  const identityKey = identity.accountAddress && identity.chainId !== undefined
    ? `${identity.chainId.toString()}:${identity.accountAddress.toLowerCase()}`
    : null
  const accountAddress = identity.accountAddress?.toLowerCase()
  const ownerAddress = identity.ownerAddress?.toLowerCase()
  const accountOperations = accountAddress && identity.chainId !== undefined
    ? operations
        .filter((operation) =>
          operation.accountAddress.toLowerCase() === accountAddress &&
          operation.chainId === identity.chainId &&
          (ownerAddress === undefined ||
            operation.ownerAddress.toLowerCase() === ownerAddress)
        )
        .sort((a, b) => b.updatedAt - a.updatedAt)
    : []
  const completedCount = accountOperations.filter(
    (operation) => operation.status === 'confirmed'
  ).length
  const failedCount = accountOperations.filter(
    (operation) => isFailedStatus(operation.status)
  ).length
  const pendingCount =
    accountOperations.length - completedCount - failedCount
  const displayCount = completedCount > 99
    ? '99+'
    : completedCount.toString()
  const buttonTone = failedCount > 0
    ? 'border-brand-orange text-brand-orange hover:bg-brand-orange/15'
    : pendingCount > 0
      ? 'border-[#FFAB96] text-[#FFAB96] hover:bg-[#FFAB96]/15'
      : completedCount > 0
        ? 'border-positive text-positive hover:bg-positive/15'
        : 'border-brand-border/50 text-content-secondary hover:border-[#FFAB96] hover:text-[#FFAB96]'
  const transactionWord = completedCount === 1 ? 'transaction' : 'transactions'
  const badgeLabel =
    `${completedCount.toString()} completed sponsored ${transactionWord}; ` +
    `${pendingCount.toString()} pending; ${failedCount.toString()} failed. ` +
    'Open recent sponsored transactions.'

  if (
    !identity.isAaManifestConfigured ||
    identity.status !== 'ready' ||
    identityKey === null
  ) {
    return null
  }

  return (
    <>
      <button
        type="button"
        aria-label={badgeLabel}
        title="Recent sponsored transactions"
        className={`relative inline-flex h-9 w-9 shrink-0 items-center justify-center rounded-full border text-xs font-semibold transition-colors ${buttonTone}`}
        onClick={() => {
          setOpenedForIdentity(identityKey)
        }}
      >
        <span className={displayCount === '99+' ? 'text-[10px]' : ''}>
          {displayCount}
        </span>
        {pendingCount > 0 ? (
          <span
            aria-hidden="true"
            className="absolute -right-0.5 -top-0.5 h-2.5 w-2.5 animate-pulse rounded-full border border-surface-panel bg-[#FFAB96]"
          />
        ) : failedCount > 0 ? (
          <span
            aria-hidden="true"
            className="absolute -right-0.5 -top-0.5 h-2.5 w-2.5 rounded-full border border-surface-panel bg-brand-orange"
          />
        ) : null}
      </button>

      <Modal
        isOpen={openedForIdentity === identityKey}
        onClose={() => {
          setOpenedForIdentity(null)
        }}
        title="Recent sponsored transactions"
        size="xl"
        analyticsId="sponsored_operation_history"
      >
        <div className="space-y-5">
          <p className="text-sm leading-6 text-content-secondary">
            Recent sponsored operations saved in this browser for the current Trading Account.
          </p>

          <div className="space-y-3 border border-brand-border/30 bg-app-bg/30 p-4">
            {identity.ownerAddress ? (
              <AddressRow
                address={identity.ownerAddress}
                label="Owner Wallet"
              />
            ) : null}
            {identity.accountAddress ? (
              <AddressRow
                address={identity.accountAddress}
                label="Trading Account"
              />
            ) : null}
          </div>

          <div className="grid grid-cols-3 gap-3 text-center">
            <div className="border border-positive/30 bg-positive/10 p-3">
              <div className="text-lg font-semibold text-positive">
                {completedCount}
              </div>
              <div className="text-xs text-content-secondary">Completed</div>
            </div>
            <div className="border border-[#FFAB96]/30 bg-[#FFAB96]/10 p-3">
              <div className="text-lg font-semibold text-[#FFAB96]">
                {pendingCount}
              </div>
              <div className="text-xs text-content-secondary">Pending</div>
            </div>
            <div className="border border-brand-orange/30 bg-brand-orange/10 p-3">
              <div className="text-lg font-semibold text-brand-orange">
                {failedCount}
              </div>
              <div className="text-xs text-content-secondary">Failed</div>
            </div>
          </div>

          {accountOperations.length > 0 ? (
            <div className="space-y-3">
              {accountOperations.map((operation) => (
                <OperationHistoryItem
                  key={operation.id}
                  operation={operation}
                  manifest={identity.manifest}
                />
              ))}
            </div>
          ) : (
            <div className="border border-dashed border-brand-border/40 p-8 text-center text-sm text-content-secondary">
              No sponsored transactions yet.
            </div>
          )}
        </div>
      </Modal>
    </>
  )
}
