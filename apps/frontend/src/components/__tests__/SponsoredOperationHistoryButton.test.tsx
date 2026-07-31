import { act, fireEvent, render, screen, within } from '@testing-library/react'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import type { Address, Hex } from 'viem'
import type { SponsoredOperation, SponsoredOperationStatus } from '../../perps-aa'

const identityMocks = vi.hoisted(() => ({
  ownerAddress: '0x1111111111111111111111111111111111111111',
  accountAddress: '0x2222222222222222222222222222222222222222',
  chainId: 421614,
}))

vi.mock('../../perps-aa', async (importOriginal) => {
  const actual = await importOriginal<typeof import('../../perps-aa')>()
  return {
    ...actual,
    usePerpsIdentity: () => ({
      status: 'ready',
      ownerAddress: identityMocks.ownerAddress,
      accountAddress: identityMocks.accountAddress,
      chainId: identityMocks.chainId,
      isAaManifestConfigured: true,
      sponsorshipEnabled: true,
      manifest: {
        userOperationExplorerUrlTemplate:
          'https://arbitrum-sepolia.blockscout.com/op/{userOperationHash}',
        transactionExplorerUrlTemplate:
          'https://arbitrum-sepolia.blockscout.com/tx/{transactionHash}',
      },
      identity: null,
      proposedIdentity: null,
      changedIdentityFields: [],
      error: null,
      confirmIdentityAfterContinuityCheck: () => false,
      reloadIdentity: () => undefined,
    }),
  }
})

import { SponsoredOperationHistoryButton } from '../SponsoredOperationActivity'
import {
  createSponsoredOperationSignal,
  useSponsoredOperationStore,
} from '../../perps-aa'

const OTHER_ACCOUNT =
  '0x3333333333333333333333333333333333333333' as Address
const USER_OPERATION_HASH = `0x${'12'.repeat(32)}` as Hex
const TRANSACTION_HASH = `0x${'34'.repeat(32)}` as Hex

function operation(input: {
  id: string
  action: SponsoredOperation['action']
  status: SponsoredOperationStatus
  updatedAt: number
  accountAddress?: Address
  chainId?: number
  userOperationHash?: Hex
  includedTransactionHash?: Hex
  transactionHash?: Hex
  reason?: SponsoredOperation['reason']
  sponsorshipAccepted?: boolean
  retryable?: boolean
  manifestVersion?: string
  submissionMetadataVersion?: 1
}): SponsoredOperation {
  return {
    id: input.id,
    ownerAddress: identityMocks.ownerAddress as Address,
    accountAddress:
      input.accountAddress ?? identityMocks.accountAddress as Address,
    chainId: input.chainId ?? identityMocks.chainId,
    accountMode: 'simple',
    manifestVersion:
      input.manifestVersion ?? 'perps-aa-arbitrum-sepolia-v1',
    action: input.action,
    lane: 'default',
    status: input.status,
    sponsorshipAccepted:
      input.sponsorshipAccepted ?? input.status !== 'failed',
    userOperationHash: input.userOperationHash,
    includedTransactionHash: input.includedTransactionHash,
    inclusionObservedAt:
      input.includedTransactionHash === undefined
        ? undefined
        : input.updatedAt,
    inclusionEvidenceRevision:
      input.includedTransactionHash === undefined
        ? undefined
        : 1,
    transactionHash: input.transactionHash,
    transactionHashVerified: input.transactionHash !== undefined,
    reason: input.reason,
    retryable: input.retryable ?? input.reason !== undefined,
    submissionMetadataVersion: input.submissionMetadataVersion,
    retryCount: 0,
    createdAt: input.updatedAt - 1_000,
    updatedAt: input.updatedAt,
    statusTimestamps: {
      [input.status]: input.updatedAt,
    },
  }
}

describe('SponsoredOperationHistoryButton', () => {
  beforeEach(() => {
    globalThis.localStorage.clear()
    vi.stubGlobal('navigator', {
      locks: {
        request: vi.fn(async (
          name: string,
          _options: LockOptions,
          callback: (lock: Lock | null) => Promise<unknown> | unknown
        ) => await callback({ name, mode: 'exclusive' } as Lock)),
      } as unknown as LockManager,
    })
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
  })

  afterEach(() => {
    vi.useRealTimers()
    vi.restoreAllMocks()
    vi.unstubAllGlobals()
  })

  it('does not claim sponsorship for a failure before submission', () => {
    useSponsoredOperationStore.setState({
      operations: [
        operation({
          id: 'unavailable',
          action: 'deposit',
          status: 'failed',
          updatedAt: Date.now(),
          reason: 'SPONSOR_UNAVAILABLE',
          // Cover records persisted before sponsorship acceptance was tracked
          // at the correct transition.
          sponsorshipAccepted: true,
        }),
      ],
      activeLanes: {},
    })

    render(<SponsoredOperationHistoryButton />)
    fireEvent.click(screen.getByRole('button', {
      name: 'Open Trading Account activity. 1 action needs attention.',
    }))

    expect(screen.getByText('Not included · No network gas used'))
      .toBeInTheDocument()
    expect(screen.queryByText('Sponsored by Plether · 0 ETH network gas'))
      .not.toBeInTheDocument()
    expect(screen.getByText(
      'Plether gas sponsorship is temporarily unavailable. Your action was not sent. Retry the same Trading Account action or contact support.'
    )).toBeInTheDocument()
    expect(screen.queryByText(/could not verify whether this transaction/i))
      .not.toBeInTheDocument()
  })

  it('shows recovered submission uncertainty without calling it a sponsorship failure', () => {
    useSponsoredOperationStore.setState({
      operations: [
        operation({
          id: 'submission-uncertain',
          action: 'place-order',
          status: 'confirming',
          updatedAt: Date.now(),
          userOperationHash: USER_OPERATION_HASH,
          reason: 'BUNDLER_UNAVAILABLE',
          sponsorshipAccepted: true,
          retryable: false,
        }),
      ],
      activeLanes: {},
    })

    render(<SponsoredOperationHistoryButton />)
    fireEvent.click(screen.getByRole('button', {
      name: 'Open Trading Account activity. 1 action in progress.',
    }))

    expect(screen.getByText('Checking submission status')).toBeInTheDocument()
    expect(screen.getByText(
      'Gas sponsorship approved · Submission unconfirmed'
    )).toBeInTheDocument()
    expect(screen.getByText(
      'Plether could not verify whether this transaction was submitted or included. We’re checking its status. Do not retry this action yet.'
    )).toBeInTheDocument()
    expect(screen.queryByText(/could not sponsor/i)).not.toBeInTheDocument()
    expect(screen.queryByText(/your action was not sent/i))
      .not.toBeInTheDocument()
    expect(screen.getByRole('link', {
      name: 'Check operation on Blockscout',
    })).toHaveAttribute(
      'href',
      `https://arbitrum-sepolia.blockscout.com/op/${USER_OPERATION_HASH}`
    )
  })

  it('labels a receipt timeout as unknown submission status', () => {
    useSponsoredOperationStore.setState({
      operations: [
        operation({
          id: 'receipt-timeout',
          action: 'place-order',
          status: 'receipt-timeout',
          updatedAt: Date.now(),
          userOperationHash: USER_OPERATION_HASH,
          reason: 'BUNDLER_UNAVAILABLE',
          sponsorshipAccepted: true,
          retryable: false,
        }),
      ],
      activeLanes: {},
    })

    render(<SponsoredOperationHistoryButton />)
    fireEvent.click(screen.getByRole('button', {
      name: 'Open Trading Account activity. 1 action needs attention.',
    }))

    expect(screen.getByText('Submission status unknown')).toBeInTheDocument()
    expect(screen.getByText(
      'Gas sponsorship approved · Submission unconfirmed'
    )).toBeInTheDocument()
    expect(screen.getByText(
      'Plether could not verify whether this transaction was submitted or included. We’re checking its status. Do not retry this action yet.'
    )).toBeInTheDocument()
    expect(screen.queryByText(/could not sponsor/i)).not.toBeInTheDocument()
    expect(screen.queryByText(/your action was not sent/i))
      .not.toBeInTheDocument()
  })

  it('shows latest-chain inclusion as static background verification', () => {
    useSponsoredOperationStore.setState({
      operations: [
        operation({
          id: 'included-awaiting-safe',
          action: 'place-order',
          status: 'confirming',
          updatedAt: Date.now(),
          userOperationHash: USER_OPERATION_HASH,
          includedTransactionHash: TRANSACTION_HASH,
          sponsorshipAccepted: true,
        }),
      ],
      activeLanes: {
        [`${identityMocks.accountAddress.toLowerCase()}:default`]:
          'included-awaiting-safe',
      },
    })

    render(<SponsoredOperationHistoryButton />)
    const activityButton = screen.getByRole('button', {
      name: 'Open Trading Account activity. 1 action included onchain.',
    })
    const activityIcon = activityButton.querySelector(
      '.sponsored-activity-base-icon'
    )
    expect(activityButton).toHaveAttribute('title', '1 action included onchain')
    expect(activityIcon).toHaveTextContent('check_circle')
    expect(activityIcon).not.toHaveClass('animate-spin')

    fireEvent.click(activityButton)

    const dialog = screen.getByRole('dialog')
    const includedSection = within(dialog).getByRole('region', {
      name: 'Included onchain',
    })
    const includedItem = includedSection.querySelector(
      '[data-operation-id="included-awaiting-safe"]'
    )
    expect(includedItem).not.toBeNull()

    expect(within(dialog).getByText('1 action included onchain'))
      .toBeInTheDocument()
    expect(within(dialog).getByText(
      'Safety verification continues in the background. No action is required.'
    )).toBeInTheDocument()
    expect(within(includedItem as HTMLElement).getByText('Included onchain'))
      .toBeInTheDocument()
    expect(within(includedItem as HTMLElement).getByText(
      'Sponsored by Plether · 0 ETH network gas'
    )).toBeInTheDocument()
    expect(within(includedItem as HTMLElement).getByText(
      'The transaction is onchain. Safety verification continues in the background; no action is required.'
    )).toBeInTheDocument()
    expect(within(dialog).queryByRole('region', { name: 'In progress' }))
      .not.toBeInTheDocument()
    expect(screen.queryByText('Submission status unknown'))
      .not.toBeInTheDocument()
    expect(screen.queryByText(/could not verify whether this transaction/i))
      .not.toBeInTheDocument()
    expect(screen.queryByText('Needs attention')).not.toBeInTheDocument()
    expect(screen.getByRole('link', {
      name: 'View included transaction on Blockscout',
    })).toHaveAttribute(
      'href',
      `https://arbitrum-sepolia.blockscout.com/tx/${TRANSACTION_HASH}`
    )

    fireEvent.click(screen.getByText('Technical details'))
    expect(screen.getByRole('link', {
      name: 'Open Included transaction in block explorer',
    })).toHaveAttribute(
      'href',
      `https://arbitrum-sepolia.blockscout.com/tx/${TRANSACTION_HASH}`
    )
  })

  it('keeps the spinner only for foreground work when another action is included', () => {
    useSponsoredOperationStore.setState({
      operations: [
        operation({
          id: 'pending',
          action: 'deposit',
          status: 'confirming',
          updatedAt: 200,
          userOperationHash: USER_OPERATION_HASH,
        }),
        operation({
          id: 'included',
          action: 'place-order',
          status: 'confirming',
          updatedAt: 100,
          userOperationHash: USER_OPERATION_HASH,
          includedTransactionHash: TRANSACTION_HASH,
          sponsorshipAccepted: true,
        }),
      ],
      activeLanes: {},
    })

    render(<SponsoredOperationHistoryButton />)
    const activityButton = screen.getByRole('button', {
      name:
        'Open Trading Account activity. 1 action in progress; 1 action included onchain.',
    })
    const activityIcon = activityButton.querySelector(
      '.sponsored-activity-base-icon'
    )
    expect(activityIcon).toHaveTextContent('progress_activity')
    expect(activityIcon).toHaveClass('animate-spin')

    fireEvent.click(activityButton)

    const dialog = screen.getByRole('dialog')
    expect(within(dialog).getByText('1 action in progress'))
      .toBeInTheDocument()
    expect(within(dialog).getByText(
      'Plether is waiting for wallet approval, sponsorship, submission, or onchain confirmation. 1 action already included onchain; safety verification continues in the background.'
    )).toBeInTheDocument()

    const inProgressSection = within(dialog).getByRole('region', {
      name: 'In progress',
    })
    const includedSection = within(dialog).getByRole('region', {
      name: 'Included onchain',
    })
    expect(inProgressSection.querySelector('[data-operation-id="pending"]'))
      .toBeInTheDocument()
    expect(inProgressSection.querySelector('[data-operation-id="included"]'))
      .not.toBeInTheDocument()
    expect(includedSection.querySelector('[data-operation-id="included"]'))
      .toBeInTheDocument()
    expect(includedSection.querySelector('[data-operation-id="pending"]'))
      .not.toBeInTheDocument()
  })

  it('turns a retracted inclusion back into submission attention', () => {
    useSponsoredOperationStore.getState().beginOperation({
      id: 'reorged-inclusion',
      ownerAddress: identityMocks.ownerAddress as Address,
      accountAddress: identityMocks.accountAddress as Address,
      chainId: identityMocks.chainId,
      accountMode: 'simple',
      manifestVersion: 'v1',
      action: 'place-order',
    })
    expect(useSponsoredOperationStore.getState().recordUserOperationHash(
      'reorged-inclusion',
      USER_OPERATION_HASH
    )).toBe(true)
    expect(useSponsoredOperationStore.getState().recordObservedInclusion(
      'reorged-inclusion',
      { transactionHash: TRANSACTION_HASH }
    )).toBe(true)

    render(<SponsoredOperationHistoryButton />)
    const includedButton = screen.getByRole('button', {
      name: 'Open Trading Account activity. 1 action included onchain.',
    })
    expect(includedButton).toHaveTextContent('check_circle')
    expect(includedButton.querySelector('.sponsored-activity-base-icon'))
      .not.toHaveClass('animate-spin')
    fireEvent.click(includedButton)
    expect(screen.getByRole('region', { name: 'Included onchain' }))
      .toBeInTheDocument()

    act(() => {
      expect(
        useSponsoredOperationStore
          .getState()
          .clearObservedInclusion('reorged-inclusion')
      ).toBe(true)
      useSponsoredOperationStore.getState().failOperation({
        id: 'reorged-inclusion',
        status: 'receipt-timeout',
        reason: 'BUNDLER_UNAVAILABLE',
        retryable: false,
      })
    })

    const attentionButton = screen.getByRole('button', {
      name: 'Open Trading Account activity. 1 action needs attention.',
    })
    expect(attentionButton).toHaveTextContent('warning')
    expect(attentionButton.querySelector('.sponsored-activity-base-icon'))
      .not.toHaveClass('animate-spin')
    expect(screen.queryByRole('region', { name: 'Included onchain' }))
      .not.toBeInTheDocument()
    expect(screen.getByText('Submission status unknown')).toBeInTheDocument()
    const attentionSection = screen.getByRole('region', {
      name: 'Needs attention',
    })
    expect(attentionSection.querySelector(
      '[data-operation-id="reorged-inclusion"]'
    )).toBeInTheDocument()
    expect(screen.queryByRole('link', {
      name: 'View included transaction on Blockscout',
    })).not.toBeInTheDocument()
  })

  it('tells the user when protocol evidence makes retrying safe', () => {
    useSponsoredOperationStore.setState({
      operations: [
        operation({
          id: 'expired',
          action: 'place-order',
          status: 'expired',
          updatedAt: Date.now(),
          userOperationHash: USER_OPERATION_HASH,
          reason: 'expired',
          sponsorshipAccepted: true,
          retryable: true,
        }),
      ],
      activeLanes: {},
    })

    render(<SponsoredOperationHistoryButton />)
    fireEvent.click(screen.getByRole('button', {
      name: 'Open Trading Account activity. 1 action needs attention.',
    }))

    expect(screen.getByText('Expired')).toBeInTheDocument()
    expect(screen.getByText(
      'This operation expired before it was included onchain. It is safe to retry the action.'
    )).toBeInTheDocument()
    expect(screen.getByText('Not included · No network gas used'))
      .toBeInTheDocument()
  })

  it('still surfaces a safely verified execution revert as new attention', () => {
    useSponsoredOperationStore.setState({
      operations: [
        operation({
          id: 'execution-reverted',
          action: 'place-order',
          status: 'execution-reverted',
          updatedAt: Date.now(),
          userOperationHash: USER_OPERATION_HASH,
          includedTransactionHash: TRANSACTION_HASH,
          transactionHash: TRANSACTION_HASH,
          sponsorshipAccepted: true,
          retryable: false,
        }),
      ],
      activeLanes: {},
    })

    render(<SponsoredOperationHistoryButton />)
    fireEvent.click(screen.getByRole('button', {
      name: 'Open Trading Account activity. 1 action needs attention.',
    }))

    expect(screen.getByText('Failed onchain')).toBeInTheDocument()
    expect(screen.getByText(
      'The transaction was included but failed during onchain execution.'
    )).toBeInTheDocument()
  })

  it('requires explicit confirmation to release a legacy lock as outcome unknown', async () => {
    const confirm = vi.fn(() => true)
    vi.stubGlobal('confirm', confirm)
    useSponsoredOperationStore.setState({
      operations: [
        operation({
          id: 'legacy-lock',
          action: 'place-order',
          status: 'receipt-timeout',
          updatedAt: Date.now(),
          userOperationHash: USER_OPERATION_HASH,
          reason: 'BUNDLER_UNAVAILABLE',
          sponsorshipAccepted: true,
          retryable: false,
          manifestVersion:
            'perps-aa-arbitrum-sepolia-20260717-v1',
        }),
      ],
      activeLanes: {
        [`${identityMocks.accountAddress.toLowerCase()}:default`]:
          'legacy-lock',
      },
    })

    render(<SponsoredOperationHistoryButton />)
    fireEvent.click(screen.getByRole('button', {
      name: 'Open Trading Account activity. 1 action needs attention.',
    }))
    fireEvent.click(screen.getByRole('button', {
      name: 'Force-release stale local lock',
    }))

    expect(confirm).toHaveBeenCalledWith(expect.stringContaining(
      'may already have executed or may still execute later'
    ))
    expect(await screen.findByText('Outcome unknown')).toBeInTheDocument()
    expect(screen.getByText('Past onchain outcome unverified'))
      .toBeInTheDocument()
    expect(screen.getByText(
      'You force-released this stale local lock. Plether cannot prove whether the old action executed or may still execute later. Close or reload every other Plether tab, then review your Trading Account and operation hash. Do not repeat the action unless you accept that risk.'
    )).toBeInTheDocument()
    expect(screen.queryByText('Not included · No network gas used'))
      .not.toBeInTheDocument()
    expect(useSponsoredOperationStore.getState().activeLanes).toEqual({})
  })

  it('does not call a consumed nonce replaced or retry-safe', () => {
    useSponsoredOperationStore.setState({
      operations: [
        operation({
          id: 'nonce-consumed',
          action: 'place-order',
          status: 'outcome-unknown',
          updatedAt: Date.now(),
          userOperationHash: USER_OPERATION_HASH,
          sponsorshipAccepted: true,
          retryable: false,
        }),
      ],
      activeLanes: {},
    })

    render(<SponsoredOperationHistoryButton />)
    fireEvent.click(screen.getByRole('button', {
      name: 'Open Trading Account activity. 1 action needs attention.',
    }))

    expect(screen.getByText('Outcome unknown')).toBeInTheDocument()
    expect(screen.getByText(
      'The old nonce can no longer land, but Plether could not prove which operation consumed it. Refresh and review your Trading Account before taking another action. Do not blindly retry it.'
    )).toBeInTheDocument()
    expect(screen.queryByText(/safe to retry/i)).not.toBeInTheDocument()
  })

  it('prioritizes attention, foreground progress, included, then recent activity', () => {
    useSponsoredOperationStore.setState({
      operations: [
        operation({
          id: 'confirmed',
          action: 'deposit',
          status: 'confirmed',
          updatedAt: 300,
          userOperationHash: USER_OPERATION_HASH,
          transactionHash: TRANSACTION_HASH,
        }),
        operation({
          id: 'pending',
          action: 'place-order',
          status: 'confirming',
          updatedAt: 400,
        }),
        operation({
          id: 'included',
          action: 'place-order',
          status: 'confirming',
          updatedAt: 350,
          userOperationHash: USER_OPERATION_HASH,
          includedTransactionHash: TRANSACTION_HASH,
        }),
        operation({
          id: 'failed',
          action: 'withdraw',
          status: 'failed',
          updatedAt: 200,
          reason: 'POLICY_DENIED',
        }),
        operation({
          id: 'other-account',
          action: 'settle-claim',
          status: 'failed',
          updatedAt: 600,
          accountAddress: OTHER_ACCOUNT,
        }),
        operation({
          id: 'other-chain',
          action: 'add-margin',
          status: 'confirmed',
          updatedAt: 500,
          chainId: 1,
        }),
      ],
      activeLanes: {},
    })

    render(<SponsoredOperationHistoryButton />)

    const activityButton = screen.getByRole('button', {
      name:
        'Open Trading Account activity. 1 action needs attention; 1 action in progress; 1 action included onchain.',
    })
    expect(activityButton).not.toHaveTextContent('1')
    expect(activityButton).toHaveTextContent('warning')

    fireEvent.click(activityButton)

    expect(
      useSponsoredOperationStore.getState().operations
        .find((item) => item.id === 'failed')?.acknowledgedAttentionRevision
    ).toBe(1)
    expect(
      useSponsoredOperationStore.getState().operations
        .find((item) => item.id === 'other-account')
        ?.acknowledgedAttentionRevision
    ).toBeUndefined()
    expect(screen.getByRole('button', {
      name:
        'Open Trading Account activity. 1 action in progress; 1 action included onchain.',
    })).toHaveTextContent('progress_activity')

    const dialog = screen.getByRole('dialog')
    const historyItems = dialog.querySelectorAll('[data-operation-id]')
    expect(historyItems).toHaveLength(4)
    expect(historyItems[0]).toHaveAttribute('data-operation-id', 'failed')
    expect(historyItems[1]).toHaveAttribute('data-operation-id', 'pending')
    expect(historyItems[2]).toHaveAttribute('data-operation-id', 'included')
    expect(historyItems[3]).toHaveAttribute('data-operation-id', 'confirmed')
    expect(within(dialog).getByText('Needs attention')).toBeInTheDocument()
    expect(within(dialog).getByText('In progress')).toBeInTheDocument()
    expect(within(dialog).getByRole('region', { name: 'Included onchain' }))
      .toBeInTheDocument()
    expect(within(dialog).getByText('Recent activity')).toBeInTheDocument()

    fireEvent.click(within(dialog).getByText('Account details'))
    expect(
      within(dialog).getByRole('button', {
        name: 'Copy Owner Wallet address',
      })
    ).toBeInTheDocument()
    expect(
      within(dialog).getByRole('button', {
        name: 'Copy Trading Account address',
      })
    ).toBeInTheDocument()
    expect(
      within(dialog).getByRole('link', {
        name: 'View Owner Wallet on Blockscout',
      })
    ).toHaveAttribute(
      'href',
      `https://arbitrum-sepolia.blockscout.com/address/${identityMocks.ownerAddress}`
    )
    expect(
      within(dialog).getByRole('link', {
        name: 'View Trading Account on Blockscout',
      })
    ).toHaveAttribute(
      'href',
      `https://arbitrum-sepolia.blockscout.com/address/${identityMocks.accountAddress}`
    )
    expect(within(dialog).getAllByText('Commit order')).toHaveLength(2)
    expect(within(dialog).getByText('Deposit margin')).toBeInTheDocument()
    expect(within(dialog).getByText('Withdraw margin')).toBeInTheDocument()
    expect(within(dialog).queryByText('Settle trader claim')).not.toBeInTheDocument()
    expect(within(dialog).queryByText('Add position margin')).not.toBeInTheDocument()
    expect(within(dialog).getByText(/not eligible for sponsored network gas/i))
      .toBeInTheDocument()

    const confirmedItem = dialog.querySelector('[data-operation-id="confirmed"]')
    expect(confirmedItem).not.toBeNull()
    expect(
      within(confirmedItem as HTMLElement).getByRole('link', {
        name: 'View transaction on Blockscout',
      })
    ).toHaveAttribute(
      'href',
      `https://arbitrum-sepolia.blockscout.com/tx/${TRANSACTION_HASH}`
    )
    fireEvent.click(within(confirmedItem as HTMLElement).getByText('Technical details'))
    expect(
      within(confirmedItem as HTMLElement).getByRole('link', {
        name: 'Open UserOperation in block explorer',
      })
    ).toHaveAttribute(
      'href',
      `https://arbitrum-sepolia.blockscout.com/op/${USER_OPERATION_HASH}`
    )
    expect(
      within(confirmedItem as HTMLElement).getByRole('link', {
        name: 'Open Transaction in block explorer',
      })
    ).toHaveAttribute(
      'href',
      `https://arbitrum-sepolia.blockscout.com/tx/${TRANSACTION_HASH}`
    )
  })

  it('keeps failed or cancellable history reachable when zero operations completed', () => {
    createSponsoredOperationSignal('building')
    useSponsoredOperationStore.setState({
      operations: [
        operation({
          id: 'building',
          action: 'deposit',
          status: 'building',
          updatedAt: 500,
        }),
      ],
      activeLanes: {},
    })

    render(<SponsoredOperationHistoryButton />)

    const activityButton = screen.getByRole('button', {
      name: /open trading account activity\. 1 action in progress/i,
    })
    expect(activityButton).not.toHaveTextContent('0')
    fireEvent.click(activityButton)

    fireEvent.click(
      within(screen.getByRole('dialog')).getByRole('button', {
        name: 'Cancel local request',
      })
    )

    expect(useSponsoredOperationStore.getState().operations[0]?.status)
      .toBe('cancelled')
    expect(screen.getByText('Cancelled locally')).toBeInTheDocument()
  })

  it('does not surface a completed count or redundant up-to-date message', () => {
    useSponsoredOperationStore.setState({
      operations: [
        operation({
          id: 'confirmed',
          action: 'deposit',
          status: 'confirmed',
          updatedAt: 300,
          transactionHash: TRANSACTION_HASH,
        }),
      ],
      activeLanes: {},
    })

    render(<SponsoredOperationHistoryButton />)

    const activityButton = screen.getByRole('button', {
      name: 'Open Trading Account activity.',
    })
    expect(activityButton).not.toHaveTextContent('1')
    expect(activityButton).toHaveAttribute('title', 'Trading Account activity')
    expect(activityButton).toHaveClass('rounded-full')

    fireEvent.click(activityButton)
    const dialog = screen.getByRole('dialog')
    expect(within(dialog).queryByText('You’re up to date')).not.toBeInTheDocument()
    expect(within(dialog).getByText('Recent activity')).toBeInTheDocument()
  })

  it('acknowledges visible failures until the operation fails again', () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2026-07-17T08:00:00.000Z'))
    useSponsoredOperationStore.setState({
      operations: [
        operation({
          id: 'failed',
          action: 'withdraw',
          status: 'failed',
          updatedAt: Date.now() - 1_000,
          reason: 'POLICY_DENIED',
        }),
      ],
      activeLanes: {},
    })

    render(<SponsoredOperationHistoryButton />)

    fireEvent.click(screen.getByRole('button', {
      name: 'Open Trading Account activity. 1 action needs attention.',
    }))

    expect(
      useSponsoredOperationStore.getState().operations[0]
        ?.acknowledgedAttentionRevision
    ).toBe(1)
    expect(screen.getByText('1 action needs attention')).toBeInTheDocument()
    expect(screen.getByText('Needs attention')).toBeInTheDocument()
    expect(screen.getByRole('button', {
      name: 'Open Trading Account activity.',
    })).toHaveTextContent('history')

    fireEvent.keyDown(document, { key: 'Escape' })
    fireEvent.click(screen.getByRole('button', {
      name: 'Open Trading Account activity.',
    }))
    expect(screen.queryByText('1 action needs attention'))
      .not.toBeInTheDocument()
    expect(screen.queryByText('Needs attention')).not.toBeInTheDocument()
    expect(screen.getByText('Recent activity')).toBeInTheDocument()
    expect(screen.getByText('Withdraw margin')).toBeInTheDocument()

    act(() => {
      useSponsoredOperationStore.getState().recordTransactionHash(
        'failed',
        TRANSACTION_HASH
      )
    })
    expect(screen.getByRole('button', {
      name: 'Open Trading Account activity.',
    })).toHaveTextContent('history')

    act(() => {
      useSponsoredOperationStore.getState().beginOperation({
        id: 'failed-again',
        ownerAddress: identityMocks.ownerAddress as Address,
        accountAddress: identityMocks.accountAddress as Address,
        chainId: 421614,
        accountMode: 'simple',
        manifestVersion: 'v1',
        action: 'withdraw',
      })
      useSponsoredOperationStore.getState().failOperation({
        id: 'failed-again',
        reason: 'POLICY_DENIED',
        retryable: false,
      })
    })

    expect(screen.getByRole('button', {
      name: 'Open Trading Account activity. 1 action needs attention.',
    })).toHaveTextContent('warning')
    expect(screen.getByRole('dialog')).toBeInTheDocument()

    fireEvent.keyDown(document, { key: 'Escape' })
    fireEvent.click(screen.getByRole('button', {
      name: 'Open Trading Account activity. 1 action needs attention.',
    }))
    expect(
      useSponsoredOperationStore.getState().operations
        .find((operation) => operation.id === 'failed-again')
        ?.acknowledgedAttentionRevision
    ).toBe(1)
    expect(screen.getByText('1 action needs attention')).toBeInTheDocument()
    expect(screen.getByRole('button', {
      name: 'Open Trading Account activity.',
    })).toHaveTextContent('history')
  })

  it('shows a green confirmation check for five seconds, then restores history', () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2026-07-17T08:00:00.000Z'))
    useSponsoredOperationStore.setState({
      operations: [
        operation({
          id: 'deposit',
          action: 'deposit',
          status: 'confirming',
          updatedAt: Date.now() - 1_000,
          userOperationHash: USER_OPERATION_HASH,
        }),
      ],
      activeLanes: {},
    })

    render(<SponsoredOperationHistoryButton />)

    act(() => {
      useSponsoredOperationStore.getState().transition('deposit', 'confirmed')
    })

    const confirmationButton = screen.getByRole('button', {
      name: 'Transaction confirmed. Open Trading Account activity.',
    })
    expect(confirmationButton).toHaveAttribute('title', 'Transaction confirmed')
    expect(confirmationButton).toHaveClass(
      'rounded-full',
      'border-positive',
      'text-positive'
    )
    expect(screen.getByTestId('sponsored-operation-success-icon'))
      .toHaveClass('sponsored-activity-success-icon-enter')
    expect(
      confirmationButton.querySelector('.sponsored-activity-success-ring')
    ).toBeInTheDocument()

    fireEvent.click(confirmationButton)
    expect(screen.getByRole('link', {
      name: 'Track operation on Blockscout',
    })).toHaveAttribute(
      'href',
      `https://arbitrum-sepolia.blockscout.com/op/${USER_OPERATION_HASH}`
    )
    fireEvent.keyDown(document, { key: 'Escape' })

    act(() => {
      vi.advanceTimersByTime(4_000)
      useSponsoredOperationStore
        .getState()
        .recordTransactionHash('deposit', TRANSACTION_HASH)
      vi.advanceTimersByTime(999)
    })
    expect(screen.getByRole('button', {
      name: 'Transaction confirmed. Open Trading Account activity.',
    })).toContainElement(screen.getByTestId('sponsored-operation-success-icon'))

    act(() => {
      vi.advanceTimersByTime(1)
    })

    expect(screen.getByTestId('sponsored-operation-success-icon'))
      .toHaveClass('sponsored-activity-success-icon-exit')
    expect(within(confirmationButton).getByText('history'))
      .toHaveClass('sponsored-activity-base-icon-return')

    act(() => {
      vi.advanceTimersByTime(240)
    })

    const historyButton = screen.getByRole('button', {
      name: 'Open Trading Account activity.',
    })
    expect(historyButton).toHaveAttribute('title', 'Trading Account activity')
    expect(historyButton).toHaveTextContent('history')
  })

  it('restarts the five-second confirmation window for the next success', () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2026-07-17T08:00:00.000Z'))
    useSponsoredOperationStore.setState({
      operations: [
        operation({
          id: 'deposit',
          action: 'deposit',
          status: 'confirming',
          updatedAt: Date.now() - 2_000,
        }),
        operation({
          id: 'order',
          action: 'place-order',
          status: 'confirming',
          updatedAt: Date.now() - 1_000,
        }),
      ],
      activeLanes: {},
    })

    render(<SponsoredOperationHistoryButton />)

    act(() => {
      useSponsoredOperationStore.getState().transition('deposit', 'confirmed')
    })
    act(() => {
      vi.advanceTimersByTime(4_000)
    })
    act(() => {
      useSponsoredOperationStore.getState().transition('order', 'confirmed')
    })
    act(() => {
      vi.advanceTimersByTime(1_000)
    })
    expect(screen.getByRole('button', {
      name: 'Transaction confirmed. Open Trading Account activity.',
    })).toContainElement(screen.getByTestId('sponsored-operation-success-icon'))

    act(() => {
      vi.advanceTimersByTime(4_000)
    })
    expect(screen.getByTestId('sponsored-operation-success-icon'))
      .toHaveClass('sponsored-activity-success-icon-exit')

    act(() => {
      vi.advanceTimersByTime(240)
    })
    expect(screen.getByRole('button', {
      name: 'Open Trading Account activity.',
    })).toHaveTextContent('history')
  })
})
