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
  usdc: '0xf1e1B188b87525C51ECe4bae8627ae621D769651',
  usdcSupportsEip3009: false,
  usdcEip712Name: null,
  usdcEip712Version: null,
  marginClearinghouse: '0x731bb0939CE531728459394A277B28Cbff8df049',
  cfdEngine: '0xA1Ebfb8aD9C90367eA30A29592419d447E3f8224',
  orderRouter: '0x4A0a6c028164A1254e10C3e39cc89Af45090069e',
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
}: {
  operations: SponsoredOperation[]
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

    return () => {
      useSponsoredOperationStore.setState({
        operations: previousOperations,
        activeLanes: previousActiveLanes,
      })
    }
  }, [operations])

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
  title: 'Perps/Sponsored Transaction History',
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
        name: /2 completed sponsored transactions; 1 pending; 1 failed/i,
      })
    )

    expect(
      await within(document.body).findByRole('dialog')
    ).toBeVisible()
  },
}
