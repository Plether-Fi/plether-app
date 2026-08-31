import { useLayoutEffect } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { expect, userEvent, waitFor, within } from 'storybook/test'
import type { Address, Hex } from 'viem'
import { SponsoredOperationHistoryButton } from '../components/SponsoredOperationActivity'
import {
  PerpsIdentityContext,
  isSponsoredOperationTerminal,
  type PerpsAaDeploymentManifest,
  type PerpsIdentityContextValue,
  type SponsoredOperation,
  type SponsoredOperationStatus,
  useSponsoredOperationStore,
} from '../perps-aa'

const OWNER_ADDRESS =
  '0x1111111111111111111111111111111111111111' as Address
const ACCOUNT_ADDRESS =
  '0x2222222222222222222222222222222222222222' as Address
const NOW = Date.UTC(2026, 6, 16, 15, 30)

const MANIFEST: PerpsAaDeploymentManifest = {
  version: 'perps-aa-arbitrum-sepolia-v2',
  chainId: 421614,
  entryPoint: '0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108',
  entryPointVersion: '0.8',
  pimlicoRpcUrl: '/api/perps/v1/aa/pimlico',
  smartAccountMode: 'simple',
  smartAccountVersion: 'permissionless-simple-v0.8',
  smartAccountIndex: '0',
  smartAccountFactory: '0x13E9ed32155810FDbd067D4522C492D6f68E5944',
  usdc: '0x1647e41f49ED6D688936092B5a291c4B28106343',
  usdcSupportsEip3009: false,
  usdcEip712Name: null,
  usdcEip712Version: null,
  marginClearinghouse: '0x2f98787F6dCC3b1f2E4a2AFa5acf410159b9F211',
  cfdEngine: '0x3dc9C0A1f9C745A4B08BD5C2E6c7aE613561c20D',
  orderRouter: '0x97A901dE2B267c307E264FD5F71403F8072F73e7',
  orderLifecycleBook: '0xa210928a7E0AE27626B8d0E67Bbd82305438aB9E',
  policyEvaluator: '0xaa4703B190684b5A57b8a9aA432fA043B169D171',
  userOperationExplorerUrlTemplate:
    'https://arbitrum-sepolia.blockscout.com/op/{userOperationHash}',
  transactionExplorerUrlTemplate:
    'https://arbitrum-sepolia.blockscout.com/tx/{transactionHash}',
  testnetFaucet: null,
  sponsorshipEnabled: true,
}

const IDENTITY: PerpsIdentityContextValue = {
  status: 'ready',
  ownerAddress: OWNER_ADDRESS,
  accountAddress: ACCOUNT_ADDRESS,
  chainId: MANIFEST.chainId,
  isAaManifestConfigured: true,
  sponsorshipEnabled: true,
  manifest: MANIFEST,
  identity: null,
  proposedIdentity: null,
  changedIdentityFields: [],
  error: null,
  confirmIdentityAfterContinuityCheck: () => false,
  reloadIdentity: () => undefined,
}

function hash(byte: string): Hex {
  return `0x${byte.repeat(64)}` as Hex
}

function operation(input: {
  id: string
  action: SponsoredOperation['action']
  status: SponsoredOperationStatus
  minutesAgo: number
  userOperationHash?: Hex
  includedTransactionHash?: Hex
  transactionHash?: Hex
  reason?: SponsoredOperation['reason']
  retryable?: boolean
}): SponsoredOperation {
  const timestamp = NOW - input.minutesAgo * 60_000
  return {
    id: input.id,
    ownerAddress: OWNER_ADDRESS,
    accountAddress: ACCOUNT_ADDRESS,
    chainId: MANIFEST.chainId,
    accountMode: MANIFEST.smartAccountMode,
    manifestVersion: MANIFEST.version,
    action: input.action,
    lane: 'default',
    status: input.status,
    sponsorshipAccepted:
      input.status !== 'building' && input.status !== 'failed',
    userOperationHash: input.userOperationHash,
    includedTransactionHash: input.includedTransactionHash,
    inclusionObservedAt:
      input.includedTransactionHash === undefined ? undefined : timestamp,
    inclusionEvidenceRevision:
      input.includedTransactionHash === undefined ? undefined : 1,
    transactionHash: input.transactionHash,
    transactionHashVerified:
      input.transactionHash === undefined ? undefined : true,
    reason: input.reason,
    retryable: input.retryable,
    retryCount: 0,
    createdAt: timestamp - 5_000,
    updatedAt: timestamp,
    statusTimestamps: {
      [input.status]: timestamp,
    },
  }
}

const confirmedOperations = [
  operation({
    id: 'deposit-confirmed',
    action: 'deposit',
    status: 'confirmed',
    minutesAgo: 12,
    userOperationHash: hash('1'),
    transactionHash: hash('2'),
  }),
  operation({
    id: 'order-confirmed',
    action: 'place-order',
    status: 'confirmed',
    minutesAgo: 4,
    userOperationHash: hash('3'),
    transactionHash: hash('4'),
  }),
]

const pendingOperations = [
  operation({
    id: 'order-pending',
    action: 'place-order',
    status: 'confirming',
    minutesAgo: 1,
    userOperationHash: hash('5'),
  }),
]

const includedOperations = [
  operation({
    id: 'order-included',
    action: 'place-order',
    status: 'confirming',
    minutesAgo: 1,
    userOperationHash: hash('7'),
    includedTransactionHash: hash('8'),
  }),
]

const successFeedbackOperations = [
  operation({
    id: 'deposit-success-feedback',
    action: 'deposit',
    status: 'confirming',
    minutesAgo: 0,
    userOperationHash: hash('6'),
  }),
]

const failedOperations = [
  operation({
    id: 'withdraw-failed',
    action: 'withdraw',
    status: 'failed',
    minutesAgo: 3,
    reason: 'POLICY_DENIED',
    retryable: false,
  }),
]

const mixedOperations = [
  ...confirmedOperations,
  ...pendingOperations,
  ...includedOperations,
  ...failedOperations,
]

function WalletHeaderPreview({
  operations,
  confirmOperationId,
}: {
  operations: SponsoredOperation[]
  confirmOperationId?: string
}) {
  useLayoutEffect(() => {
    const previousOperations =
      useSponsoredOperationStore.getState().operations
    const previousActiveLanes =
      useSponsoredOperationStore.getState().activeLanes
    const activeOperation = operations
      .filter((operation) => !isSponsoredOperationTerminal(operation.status))
      .sort((left, right) => right.updatedAt - left.updatedAt)
      .at(0)

    useSponsoredOperationStore.setState({
      operations,
      activeLanes: activeOperation
        ? {
            [`${activeOperation.accountAddress.toLowerCase()}:${activeOperation.lane}`]:
              activeOperation.id,
          }
        : {},
    })

    const confirmationTimeoutId = confirmOperationId
      ? window.setTimeout(() => {
          useSponsoredOperationStore
            .getState()
            .transition(confirmOperationId, 'confirmed')
        }, 900)
      : null

    return () => {
      if (confirmationTimeoutId !== null) {
        window.clearTimeout(confirmationTimeoutId)
      }
      useSponsoredOperationStore.setState({
        operations: previousOperations,
        activeLanes: previousActiveLanes,
      })
    }
  }, [confirmOperationId, operations])

  return (
    <PerpsIdentityContext.Provider value={IDENTITY}>
      <div className="min-h-40 bg-app-bg p-8">
        <div className="ml-auto flex w-fit items-center gap-4 border border-brand-border/30 bg-surface-panel p-4">
          <span className="border border-warning/30 bg-warning-bg px-2 py-0.5 text-xs font-medium text-warning">
            Arbitrum Sepolia
          </span>
          <SponsoredOperationHistoryButton />
          <button
            type="button"
            className="flex items-center gap-2 border border-[#FF572D] bg-[#FF572D] px-4 py-2 text-[#FFF5F9]"
          >
            <span className="h-2 w-2 rounded-full bg-positive" />
            <span className="text-sm font-medium">0x1111...1111</span>
          </button>
        </div>
      </div>
    </PerpsIdentityContext.Provider>
  )
}

const meta: Meta<typeof SponsoredOperationHistoryButton> = {
  title: 'Perps/Trading Account Activity',
  component: SponsoredOperationHistoryButton,
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta
type Story = StoryObj<typeof meta>

export const Empty: Story = {
  render: () => <WalletHeaderPreview operations={[]} />,
}

export const Confirmed: Story = {
  render: () => (
    <WalletHeaderPreview operations={confirmedOperations} />
  ),
}

export const SuccessFeedback: Story = {
  name: 'Success feedback (5 seconds)',
  render: () => (
    <WalletHeaderPreview
      operations={successFeedbackOperations}
      confirmOperationId="deposit-success-feedback"
    />
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const confirmationButton = await canvas.findByRole('button', {
      name: 'Transaction confirmed. Open Trading Account activity.',
    }, { timeout: 2_500 })

    expect(confirmationButton).toHaveClass('rounded-full')
    await waitFor(() => {
      expect(
        within(confirmationButton).getByTestId(
          'sponsored-operation-success-icon'
        )
      ).toBeVisible()
    })
  },
}

export const Pending: Story = {
  render: () => (
    <WalletHeaderPreview operations={pendingOperations} />
  ),
}

export const IncludedAwaitingSafeConfirmation: Story = {
  name: 'Included onchain (background verification)',
  render: () => (
    <WalletHeaderPreview operations={includedOperations} />
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const activityButton = await canvas.findByRole('button', {
      name: 'Open Trading Account activity. 1 action included onchain.',
    })
    const activityIcon = activityButton.querySelector(
      '.sponsored-activity-base-icon'
    )
    expect(activityIcon).toHaveTextContent('check_circle')
    expect(activityIcon).not.toHaveClass('animate-spin')

    await userEvent.click(activityButton)

    const dialog = await within(document.body).findByRole('dialog')
    expect(within(dialog).getByText('1 action included onchain')).toBeVisible()
    expect(within(dialog).getByText(
      'Safety verification continues in the background. No action is required.'
    )).toBeVisible()
    expect(within(dialog).getByRole('region', {
      name: 'Included onchain',
    })).toBeVisible()
    expect(within(dialog).queryByRole('region', { name: 'In progress' }))
      .not.toBeInTheDocument()
    expect(within(dialog).getByRole('link', {
      name: 'View included transaction on Blockscout',
    })).toBeVisible()
  },
}

export const PendingAndIncluded: Story = {
  name: 'Pending + included onchain',
  render: () => (
    <WalletHeaderPreview
      operations={[...pendingOperations, ...includedOperations]}
    />
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const activityButton = await canvas.findByRole('button', {
      name:
        'Open Trading Account activity. 1 action in progress; 1 action included onchain.',
    })
    const activityIcon = activityButton.querySelector(
      '.sponsored-activity-base-icon'
    )
    expect(activityIcon).toHaveTextContent('progress_activity')
    expect(activityIcon).toHaveClass('animate-spin')

    await userEvent.click(activityButton)

    const dialog = await within(document.body).findByRole('dialog')
    expect(within(dialog).getByRole('region', { name: 'In progress' }))
      .toBeVisible()
    expect(within(dialog).getByRole('region', { name: 'Included onchain' }))
      .toBeVisible()
  },
}

export const Failed: Story = {
  render: () => (
    <WalletHeaderPreview operations={failedOperations} />
  ),
}

export const MixedHistory: Story = {
  render: () => (
    <WalletHeaderPreview operations={mixedOperations} />
  ),
}

export const ModalOpen: Story = {
  render: () => (
    <WalletHeaderPreview operations={mixedOperations} />
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    await userEvent.click(
      await canvas.findByRole('button', {
        name:
          'Open Trading Account activity. 1 action needs attention; 1 action in progress; 1 action included onchain.',
      })
    )

    expect(
      await within(document.body).findByRole('dialog')
    ).toBeVisible()
  },
}
