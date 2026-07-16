import { fireEvent, render, screen, within } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
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

  it('counts confirmed operations and shows current-account history newest first', () => {
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
          status: 'confirmed',
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

    const badge = screen.getByRole('button', {
      name: /1 completed sponsored transaction; 1 pending; 1 failed/i,
    })
    expect(badge).toHaveTextContent('1')

    fireEvent.click(badge)

    const dialog = screen.getByRole('dialog')
    const historyItems = dialog.querySelectorAll('[data-operation-id]')
    expect(historyItems).toHaveLength(3)
    expect(historyItems[0]).toHaveAttribute('data-operation-id', 'pending')
    expect(historyItems[1]).toHaveAttribute('data-operation-id', 'confirmed')
    expect(historyItems[2]).toHaveAttribute('data-operation-id', 'failed')
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
    expect(within(dialog).getByText('Commit order')).toBeInTheDocument()
    expect(within(dialog).getByText('Deposit margin')).toBeInTheDocument()
    expect(within(dialog).getByText('Withdraw margin')).toBeInTheDocument()
    expect(within(dialog).queryByText('Settle trader claim')).not.toBeInTheDocument()
    expect(within(dialog).queryByText('Add position margin')).not.toBeInTheDocument()
    expect(within(dialog).getByText(/not eligible for sponsored network gas/i))
      .toBeInTheDocument()
    expect(
      within(dialog).getByRole('link', {
        name: 'Open UserOperation in block explorer',
      })
    ).toHaveAttribute(
      'href',
      `https://arbitrum-sepolia.blockscout.com/op/${USER_OPERATION_HASH}`
    )
    expect(
      within(dialog).getByRole('link', {
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

    const badge = screen.getByRole('button', {
      name: /0 completed sponsored transactions; 1 pending; 0 failed/i,
    })
    expect(badge).toHaveTextContent('0')
    fireEvent.click(badge)

    fireEvent.click(
      within(screen.getByRole('dialog')).getByRole('button', {
        name: 'Cancel local request',
      })
    )

    expect(useSponsoredOperationStore.getState().operations[0]?.status)
      .toBe('cancelled')
    expect(screen.getByText('Cancelled locally')).toBeInTheDocument()
  })
})
