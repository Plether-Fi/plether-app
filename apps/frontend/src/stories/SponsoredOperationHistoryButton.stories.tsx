import { useLayoutEffect } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { expect, userEvent, within } from 'storybook/test'
import type { Address, Hex } from 'viem'
import { SponsoredOperationHistoryButton } from '../components/SponsoredOperationActivity'
import {
  PerpsIdentityContext,
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
  version: 'perps-aa-arbitrum-sepolia-v1',
  chainId: 421614,
  entryPoint: '0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108',
  entryPointVersion: '0.8',
  pimlicoRpcUrl: '/api/perps/v1/aa/pimlico',
  smartAccountMode: 'simple',
  smartAccountVersion: 'permissionless-simple-v0.8',
  smartAccountIndex: '0',
  smartAccountFactory: '0x13E9ed32155810FDbd067D4522C492D6f68E5944',
  usdc: '0xB15503d70B0eAa644dc6650d2A248762F7c5bCE3',
  usdcSupportsEip3009: false,
  usdcEip712Name: null,
  usdcEip712Version: null,
  marginClearinghouse: '0x19c2f60f6312EAF9acDE4C2b04551a05cA9bE76e',
  cfdEngine: '0x6A25eA1015b5f032d8a2D95d57AEfcB99219bF0a',
  orderRouter: '0x04E3103752f623fBcDcD01f588590Af4c53E4c1E',
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
    transactionHash: input.transactionHash,
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

    useSponsoredOperationStore.setState({
      operations,
      activeLanes: {},
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
    expect(
      within(confirmationButton).getByTestId(
        'sponsored-operation-success-icon'
      )
    ).toBeVisible()
  },
}

export const Pending: Story = {
  render: () => (
    <WalletHeaderPreview operations={pendingOperations} />
  ),
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
        name: /open trading account activity\. 1 action needs attention; 1 action in progress/i,
      })
    )

    expect(
      await within(document.body).findByRole('dialog')
    ).toBeVisible()
  },
}
