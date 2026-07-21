import { useEffect, useRef, useState } from 'react'
import {
  canCancelSponsoredOperationLocally,
  getSponsoredOperationAttentionRevision,
  isSponsoredOperationAttentionStatus,
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

const SUCCESS_FEEDBACK_DURATION_MS = 5_000
const SUCCESS_EXIT_ANIMATION_MS = 240

function abbreviatedHash(hash: string): string {
  return `${hash.slice(0, 10)}…${hash.slice(-8)}`
}

function blockscoutAddressUrl(
  transactionExplorerUrlTemplate: string,
  address: string
): string | undefined {
  try {
    const url = new URL(
      transactionExplorerUrlTemplate.replace(
        '{transactionHash}',
        `0x${'0'.repeat(64)}`
      )
    )
    const transactionPathIndex = url.pathname.lastIndexOf('/tx/')
    if (transactionPathIndex === -1) return undefined

    url.pathname =
      `${url.pathname.slice(0, transactionPathIndex)}/address/${address}`
    url.search = ''
    return url.toString()
  } catch {
    return undefined
  }
}

function formatOperationTime(timestamp: number): string {
  return new Date(timestamp).toLocaleString(undefined, {
    dateStyle: 'medium',
    timeStyle: 'short',
  })
}

function isFailedStatus(status: SponsoredOperationStatus): boolean {
  return [
    'failed',
    'execution-reverted',
    'dropped',
    'expired',
  ].includes(status)
}

function isUnreviewedAttentionOperation(
  operation: SponsoredOperation
): boolean {
  if (!isSponsoredOperationAttentionStatus(operation.status)) return false

  return (operation.acknowledgedAttentionRevision ?? 0) <
    getSponsoredOperationAttentionRevision(operation)
}

function isInProgressStatus(status: SponsoredOperationStatus): boolean {
  return !isSponsoredOperationTerminal(status) && status !== 'receipt-timeout'
}

function actionCountLabel(count: number): string {
  return `${count.toString()} ${count === 1 ? 'action' : 'actions'}`
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
  explorerUrl,
  label,
}: {
  address: string
  explorerUrl?: string
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
        {explorerUrl ? (
          <a
            aria-label={`View ${label} on Blockscout`}
            title={`View ${label} on Blockscout`}
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
  const primaryExplorerUrl = transactionUrl ?? replacementUserOperationUrl ?? userOperationUrl
  const primaryExplorerLabel = transactionUrl
    ? 'View transaction on Blockscout'
    : replacementUserOperationUrl
      ? 'View replacement operation'
      : 'Track operation on Blockscout'
  const reasonMessage = operation.reason
    ? sponsorReasonMessage(new SponsorRequestError({
        reason: operation.reason,
        message: operation.reason,
        retryable: operation.retryable ?? false,
      }))
    : undefined
  const canCancelLocally = canCancelSponsoredOperationLocally(operation)
  const hasTechnicalDetails = Boolean(
    operation.userOperationHash ??
    operation.transactionHash ??
    operation.replacementUserOperationHash
  )
  const wasSubmitted = hasTechnicalDetails
  const sponsorshipSummary = wasSubmitted && operation.sponsorshipAccepted
    ? 'Sponsored by Plether · 0 ETH network gas'
    : isSponsoredOperationTerminal(operation.status) && !wasSubmitted
      ? 'Not submitted · No network gas used'
      : operation.sponsorshipAccepted
        ? 'Gas sponsorship approved'
        : undefined
  const itemTone = isSponsoredOperationAttentionStatus(operation.status)
    ? 'border-brand-orange/50'
    : isInProgressStatus(operation.status)
      ? 'border-[#FFAB96]/50'
      : 'border-brand-border/30'

  return (
    <article
      className={`space-y-3 border bg-app-bg/30 p-4 ${itemTone}`}
      data-operation-id={operation.id}
    >
      <div className="min-w-0">
        <div className="flex flex-wrap items-baseline gap-x-3 gap-y-1">
          <h3 className="font-semibold text-content-primary">
            {sponsoredOperationActionLabel(operation.action)}
          </h3>
          <time
            dateTime={new Date(operation.updatedAt).toISOString()}
            className="whitespace-nowrap text-xs text-content-secondary"
          >
            {formatOperationTime(operation.updatedAt)}
          </time>
        </div>
        <div className="mt-2 flex flex-wrap items-center gap-x-3 gap-y-1">
          <Badge variant={statusBadgeVariant(operation.status)}>
            {sponsoredOperationStatusLabel(operation.status)}
          </Badge>
          {sponsorshipSummary ? (
            <span className={`text-xs ${wasSubmitted ? 'text-positive' : 'text-content-secondary'}`}>
              {sponsorshipSummary}
            </span>
          ) : null}
        </div>
      </div>

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

      {primaryExplorerUrl || canCancelLocally ? (
        <div className="flex flex-wrap items-center gap-3">
          {primaryExplorerUrl ? (
            <a
              href={primaryExplorerUrl}
              target="_blank"
              rel="noopener noreferrer"
              className="inline-flex items-center gap-1 text-xs font-semibold text-[#FFAB96] hover:underline hover:underline-offset-4"
            >
              {primaryExplorerLabel}
              <span aria-hidden="true" className="material-symbols-outlined !text-[14px] !leading-none">
                open_in_new
              </span>
            </a>
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
        </div>
      ) : null}

      {hasTechnicalDetails ? (
        <details className="border-t border-brand-border/20 pt-3">
          <summary className="cursor-pointer text-xs font-semibold text-content-secondary transition-colors hover:text-content-primary">
            Technical details
          </summary>
          <div className="mt-3 space-y-2">
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
          </div>
        </details>
      ) : null}
    </article>
  )
}

export function SponsoredOperationHistoryButton() {
  const identity = usePerpsIdentity()
  const operations = useSponsoredOperationStore((state) => state.operations)
  const [openedActivity, setOpenedActivity] = useState<{
    identityKey: string
    attentionOperationIds: string[]
  } | null>(null)
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
  const attentionOperations = accountOperations.filter(
    (operation) => isSponsoredOperationAttentionStatus(operation.status)
  )
  const unreviewedAttentionOperations = attentionOperations.filter(
    isUnreviewedAttentionOperation
  )
  const inProgressOperations = accountOperations.filter(
    (operation) => isInProgressStatus(operation.status)
  )
  const openedAttentionOperationIds = new Set(
    openedActivity?.identityKey === identityKey
      ? openedActivity.attentionOperationIds
      : []
  )
  const needsAttentionOperations = attentionOperations.filter(
    (operation) =>
      openedAttentionOperationIds.has(operation.id) ||
      isUnreviewedAttentionOperation(operation)
  )
  const needsAttentionOperationIds = new Set(
    needsAttentionOperations.map((operation) => operation.id)
  )
  const recentOperations = accountOperations.filter(
    (operation) =>
      !needsAttentionOperationIds.has(operation.id) &&
      !isInProgressStatus(operation.status)
  )
  const [confirmationFeedback, setConfirmationFeedback] = useState<{
    identityKey: string
    operationId: string
    phase: 'visible' | 'exiting'
    sequence: number
  } | null>(null)
  const confirmationTimeoutRef = useRef<number | null>(null)
  const confirmationExitTimeoutRef = useRef<number | null>(null)
  const confirmationSequenceRef = useRef(0)

  useEffect(() => {
    if (
      identityKey === null ||
      accountAddress === undefined ||
      identity.chainId === undefined
    ) {
      return
    }

    const unsubscribe = useSponsoredOperationStore.subscribe(
      (state, previousState) => {
        const previousOperations = new Map(
          previousState.operations.map((operation) => [operation.id, operation])
        )
        const newlyConfirmedOperation = state.operations
          .filter((operation) =>
            operation.accountAddress.toLowerCase() === accountAddress &&
            operation.chainId === identity.chainId &&
            (ownerAddress === undefined ||
              operation.ownerAddress.toLowerCase() === ownerAddress) &&
            operation.status === 'confirmed' &&
            previousOperations.has(operation.id) &&
            previousOperations.get(operation.id)?.status !== 'confirmed'
          )
          .sort((a, b) =>
            (b.statusTimestamps.confirmed ?? b.updatedAt) -
            (a.statusTimestamps.confirmed ?? a.updatedAt)
          )
          .at(0)

        if (!newlyConfirmedOperation) return

        if (confirmationTimeoutRef.current !== null) {
          window.clearTimeout(confirmationTimeoutRef.current)
        }
        if (confirmationExitTimeoutRef.current !== null) {
          window.clearTimeout(confirmationExitTimeoutRef.current)
        }

        confirmationSequenceRef.current += 1
        const sequence = confirmationSequenceRef.current
        const feedback = {
          identityKey,
          operationId: newlyConfirmedOperation.id,
          phase: 'visible' as const,
          sequence,
        }
        setConfirmationFeedback(feedback)
        confirmationTimeoutRef.current = window.setTimeout(() => {
          if (confirmationSequenceRef.current !== sequence) return

          setConfirmationFeedback((currentFeedback) =>
            currentFeedback?.identityKey === feedback.identityKey &&
            currentFeedback.operationId === feedback.operationId &&
            currentFeedback.sequence === sequence
              ? { ...currentFeedback, phase: 'exiting' }
              : currentFeedback
          )
          confirmationTimeoutRef.current = null
          confirmationExitTimeoutRef.current = window.setTimeout(() => {
            if (confirmationSequenceRef.current !== sequence) return

            setConfirmationFeedback((currentFeedback) =>
              currentFeedback?.identityKey === feedback.identityKey &&
              currentFeedback.operationId === feedback.operationId &&
              currentFeedback.sequence === sequence
                ? null
                : currentFeedback
            )
            confirmationExitTimeoutRef.current = null
          }, SUCCESS_EXIT_ANIMATION_MS)
        }, SUCCESS_FEEDBACK_DURATION_MS)
      }
    )

    return () => {
      unsubscribe()
    }
  }, [
    accountAddress,
    identity.chainId,
    identityKey,
    ownerAddress,
  ])

  useEffect(() => {
    return () => {
      if (confirmationTimeoutRef.current !== null) {
        window.clearTimeout(confirmationTimeoutRef.current)
        confirmationTimeoutRef.current = null
      }
      if (confirmationExitTimeoutRef.current !== null) {
        window.clearTimeout(confirmationExitTimeoutRef.current)
        confirmationExitTimeoutRef.current = null
      }
    }
  }, [])

  const activeConfirmationFeedback =
    confirmationFeedback?.identityKey === identityKey
      ? confirmationFeedback
      : null
  const showConfirmationFeedback = activeConfirmationFeedback !== null
  const isConfirmationFeedbackVisible =
    activeConfirmationFeedback?.phase === 'visible'
  const isConfirmationFeedbackExiting =
    activeConfirmationFeedback?.phase === 'exiting'
  const unreviewedAttentionCount = unreviewedAttentionOperations.length
  const inProgressCount = inProgressOperations.length
  const attentionSummaryCount = openedActivity?.identityKey === identityKey
    ? needsAttentionOperations.length
    : 0
  const buttonTone = isConfirmationFeedbackVisible
    ? 'border-positive text-positive hover:bg-positive/15'
    : unreviewedAttentionCount > 0
      ? 'border-brand-orange text-brand-orange hover:bg-brand-orange/15'
      : inProgressCount > 0
        ? 'border-[#FFAB96] text-[#FFAB96] hover:bg-[#FFAB96]/15'
        : 'border-brand-border/50 text-content-secondary hover:border-[#FFAB96] hover:text-[#FFAB96]'
  const buttonIcon = unreviewedAttentionCount > 0
    ? 'warning'
    : inProgressCount > 0
      ? 'progress_activity'
      : 'history'
  const buttonTitle = showConfirmationFeedback
    ? 'Transaction confirmed'
    : unreviewedAttentionCount > 0
      ? `${actionCountLabel(unreviewedAttentionCount)} ${unreviewedAttentionCount === 1 ? 'needs' : 'need'} attention`
      : inProgressCount > 0
        ? `${actionCountLabel(inProgressCount)} in progress`
        : 'Trading Account activity'
  const buttonStatusLabel = [
    unreviewedAttentionCount > 0
      ? `${actionCountLabel(unreviewedAttentionCount)} ${unreviewedAttentionCount === 1 ? 'needs' : 'need'} attention`
      : null,
    inProgressCount > 0
      ? `${actionCountLabel(inProgressCount)} in progress`
      : null,
  ].filter(Boolean).join('; ')
  const openActivityLabel = buttonStatusLabel
    ? `Open Trading Account activity. ${buttonStatusLabel}.`
    : 'Open Trading Account activity.'
  const buttonLabel = showConfirmationFeedback
    ? `Transaction confirmed. ${openActivityLabel}`
    : openActivityLabel
  const statusSummary = attentionSummaryCount > 0
    ? {
        title: `${actionCountLabel(attentionSummaryCount)} ${attentionSummaryCount === 1 ? 'needs' : 'need'} attention`,
        description: `Review the highlighted ${attentionSummaryCount === 1 ? 'action' : 'actions'} before retrying.${inProgressCount > 0 ? ` ${actionCountLabel(inProgressCount)} still in progress.` : ''}`,
        tone: 'border-brand-orange/40 bg-brand-orange/10',
      }
    : inProgressCount > 0
      ? {
          title: `${actionCountLabel(inProgressCount)} in progress`,
          description: 'Plether is waiting for wallet approval, sponsorship, submission, or onchain confirmation.',
          tone: 'border-[#FFAB96]/40 bg-[#FFAB96]/10',
        }
      : null

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
        aria-label={buttonLabel}
        title={buttonTitle}
        className={`relative inline-flex h-9 w-9 shrink-0 items-center justify-center overflow-visible rounded-full border transition-colors duration-200 motion-reduce:transition-none ${buttonTone}`}
        onClick={() => {
          useSponsoredOperationStore.getState().acknowledgeOperations(
            unreviewedAttentionOperations.map((operation) => ({
              id: operation.id,
              attentionRevision:
                getSponsoredOperationAttentionRevision(operation),
            }))
          )
          setOpenedActivity({
            identityKey,
            attentionOperationIds: unreviewedAttentionOperations.map(
              (operation) => operation.id
            ),
          })
        }}
      >
        <span
          aria-hidden="true"
          className={`sponsored-activity-base-icon material-symbols-outlined relative z-10 !text-[20px] !leading-none ${buttonIcon === 'progress_activity' && !showConfirmationFeedback ? 'animate-spin motion-reduce:animate-none' : ''} ${isConfirmationFeedbackVisible ? 'sponsored-activity-base-icon-suppressed' : ''} ${isConfirmationFeedbackExiting ? 'sponsored-activity-base-icon-return' : ''}`}
        >
          {buttonIcon}
        </span>
        {showConfirmationFeedback ? (
          <span
            key={activeConfirmationFeedback.sequence}
            aria-hidden="true"
            className="pointer-events-none absolute inset-0 z-20 flex items-center justify-center"
          >
            <svg
              data-testid="sponsored-operation-success-icon"
              viewBox="0 0 20 20"
              fill="none"
              className={`h-5 w-5 overflow-visible ${isConfirmationFeedbackExiting ? 'sponsored-activity-success-icon-exit' : 'sponsored-activity-success-icon-enter'}`}
            >
              <path
                className="sponsored-activity-success-check-path"
                d="M4.5 10.25 8 13.75 15.5 6.25"
                stroke="currentColor"
                strokeWidth="2.25"
                strokeLinecap="round"
                strokeLinejoin="round"
              />
            </svg>
          </span>
        ) : null}
        {isConfirmationFeedbackVisible ? (
          <span
            key={`success-ring-${activeConfirmationFeedback.sequence.toString()}`}
            aria-hidden="true"
            className="sponsored-activity-success-ring"
          />
        ) : null}
      </button>

      <Modal
        isOpen={openedActivity?.identityKey === identityKey}
        onClose={() => {
          setOpenedActivity(null)
        }}
        title="Trading Account activity"
        size="xl"
        analyticsId="sponsored_operation_history"
      >
        <div className="space-y-5">
          {statusSummary ? (
            <div className={`border p-4 ${statusSummary.tone}`} aria-live="polite">
              <div className="font-semibold text-content-primary">
                {statusSummary.title}
              </div>
              <p className="mt-1 text-sm leading-5 text-content-secondary">
                {statusSummary.description}
              </p>
            </div>
          ) : null}

          {needsAttentionOperations.length > 0 ? (
            <section className="space-y-3" aria-labelledby="sponsored-activity-attention">
              <h3 id="sponsored-activity-attention" className="text-sm font-semibold uppercase tracking-wide text-brand-orange">
                Needs attention
              </h3>
              {needsAttentionOperations.map((operation) => (
                <OperationHistoryItem
                  key={operation.id}
                  operation={operation}
                  manifest={identity.manifest}
                />
              ))}
            </section>
          ) : null}

          {inProgressOperations.length > 0 ? (
            <section className="space-y-3" aria-labelledby="sponsored-activity-progress">
              <h3 id="sponsored-activity-progress" className="text-sm font-semibold uppercase tracking-wide text-[#FFAB96]">
                In progress
              </h3>
              {inProgressOperations.map((operation) => (
                <OperationHistoryItem
                  key={operation.id}
                  operation={operation}
                  manifest={identity.manifest}
                />
              ))}
            </section>
          ) : null}

          {recentOperations.length > 0 ? (
            <section className="space-y-3" aria-labelledby="sponsored-activity-recent">
              <h3 id="sponsored-activity-recent" className="text-sm font-semibold uppercase tracking-wide text-content-secondary">
                Recent activity
              </h3>
              {recentOperations.map((operation) => (
                <OperationHistoryItem
                  key={operation.id}
                  operation={operation}
                  manifest={identity.manifest}
                />
              ))}
            </section>
          ) : null}

          {accountOperations.length === 0 ? (
            <div className="border border-dashed border-brand-border/40 p-8 text-center text-sm text-content-secondary">
              No Trading Account activity saved on this device.
            </div>
          ) : null}

          <details className="border border-brand-border/30 bg-app-bg/30 p-4">
            <summary className="cursor-pointer text-sm font-semibold text-content-primary">
              Account details
            </summary>
            <div className="mt-4 space-y-3 border-t border-brand-border/20 pt-4">
              {identity.ownerAddress ? (
                <AddressRow
                  address={identity.ownerAddress}
                  explorerUrl={identity.manifest
                    ? blockscoutAddressUrl(
                        identity.manifest.transactionExplorerUrlTemplate,
                        identity.ownerAddress
                      )
                    : undefined}
                  label="Owner Wallet"
                />
              ) : null}
              {identity.accountAddress ? (
                <AddressRow
                  address={identity.accountAddress}
                  explorerUrl={identity.manifest
                    ? blockscoutAddressUrl(
                        identity.manifest.transactionExplorerUrlTemplate,
                        identity.accountAddress
                      )
                    : undefined}
                  label="Trading Account"
                />
              ) : null}
            </div>
          </details>

          <p className="text-xs leading-5 text-content-secondary">
            This activity is saved on this device. Completed and unsuccessful actions are retained for up to 24 hours and may not appear on another device.
          </p>
        </div>
      </Modal>
    </>
  )
}
