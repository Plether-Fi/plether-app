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
import { useSponsoredOperationStore } from '../../perps-aa'

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
  transactionHash?: Hex
  reason?: SponsoredOperation['reason']
}): SponsoredOperation {
  return {
    id: input.id,
    ownerAddress: identityMocks.ownerAddress as Address,
    accountAddress:
      input.accountAddress ?? identityMocks.accountAddress as Address,
    chainId: input.chainId ?? identityMocks.chainId,
    accountMode: 'simple',
    manifestVersion: 'perps-aa-arbitrum-sepolia-v1',
    action: input.action,
    lane: 'default',
    status: input.status,
    sponsorshipAccepted: input.status !== 'failed',
    userOperationHash: input.userOperationHash,
    transactionHash: input.transactionHash,
    reason: input.reason,
    retryable: input.reason !== undefined,
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
    useSponsoredOperationStore.setState({
      operations: [],
      activeLanes: {},
    })
  })

  afterEach(() => {
    vi.useRealTimers()
  })

  it('prioritizes actions that need attention over in-progress and recent activity', () => {
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
      name: /open trading account activity\. 1 action needs attention; 1 action in progress/i,
    })
    expect(activityButton).not.toHaveTextContent('1')

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
      name: 'Open Trading Account activity. 1 action in progress.',
    })).toHaveTextContent('progress_activity')

    const dialog = screen.getByRole('dialog')
    const historyItems = dialog.querySelectorAll('[data-operation-id]')
    expect(historyItems).toHaveLength(3)
    expect(historyItems[0]).toHaveAttribute('data-operation-id', 'failed')
    expect(historyItems[1]).toHaveAttribute('data-operation-id', 'pending')
    expect(historyItems[2]).toHaveAttribute('data-operation-id', 'confirmed')
    expect(within(dialog).getByText('Needs attention')).toBeInTheDocument()
    expect(within(dialog).getByText('In progress')).toBeInTheDocument()
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
    expect(within(dialog).getByText('Commit order')).toBeInTheDocument()
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
    expect(screen.getByText('Needs attention')).toBeInTheDocument()
    expect(screen.getByRole('button', {
      name: 'Open Trading Account activity.',
    })).toHaveTextContent('history')

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
      useSponsoredOperationStore.getState().transition('failed', 'building')
      useSponsoredOperationStore.getState().failOperation({
        id: 'failed',
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
      useSponsoredOperationStore.getState().operations[0]
        ?.acknowledgedAttentionRevision
    ).toBe(2)
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
