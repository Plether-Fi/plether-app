import { useLayoutEffect, type ReactNode } from 'react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { createConfig, WagmiProvider } from 'wagmi'
import { arbitrumSepolia, mainnet, sepolia } from 'wagmi/chains'
import { custom } from 'viem'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { expect, userEvent, waitFor, within } from 'storybook/test'
import type { Address, Hex } from 'viem'
import { SponsoredOperationHistoryButton } from '../components/SponsoredOperationActivity'
import { PerpsTradeTicket } from '../components/PerpsTradeTicket'
import { usePerpsUiStore } from '../stores/perpsUiStore'
import { anvil } from '../config/wagmi'
import { PerpsAaRuntimeContext, type PerpsAaSmartAccountRuntime } from '../perps-aa/runtimeContext'
import type { PerpsAaDeploymentManifestV2 } from '../perps-aa/manifest'
import type { PreparationStatusV1 } from '../perps-aa/preparedOperation'
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
  usdc: '0xf7cbfcc74f2d9eb6fa7dc11941b3bef9fd7f8eb8',
  usdcSupportsEip3009: false,
  usdcEip712Name: null,
  usdcEip712Version: null,
  marginClearinghouse: '0xfa6e677ec1062757c1194d411a5e61e1e9644499',
  cfdEngine: '0xafece93321be41aa73474457e2f47cf7b2fb738f',
  orderRouter: '0x6215d36fcbd610ca1525252eebcbfd8b223a6072',
  orderLifecycleBook: '0x753eb48305ffb88bb70869ade2c4efa941879221',
  policyEvaluator: '0x43c93d3028fcd4c1f578a50639750b8fbfdee799',
  positionProtectionBook: '0x3204c51cd567d6490c011399ccbaaf67b5d3d768',
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
  runtime,
  children,
}: {
  operations: SponsoredOperation[]
  confirmOperationId?: string
  runtime?: PerpsAaSmartAccountRuntime
  children?: ReactNode
}) {
  useLayoutEffect(() => {
    const previousOperations =
      useSponsoredOperationStore.getState().operations
    const previousActiveLanes =
      useSponsoredOperationStore.getState().activeLanes
    const previousActivityRequest = usePerpsUiStore.getState().activityRequest
    usePerpsUiStore.setState({ activityRequest: null })
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
      usePerpsUiStore.setState({ activityRequest: previousActivityRequest })
    }
  }, [confirmOperationId, operations])

  return (
    <PerpsAaRuntimeContext value={runtime}>
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
        {children}
      </div>
    </PerpsIdentityContext.Provider>
    </PerpsAaRuntimeContext>
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

const NATIVE_MANIFEST: PerpsAaDeploymentManifestV2 = {
  ...MANIFEST,
  version: 'perps-aa-arbitrum-sepolia-v2',
  bundlerRpcUrl: '/storybook/aa/rpc',
  paymasterRpcUrl: '/storybook/aa/rpc',
  paymasterAddress: '0x3333333333333333333333333333333333333333',
  paymasterVersion: 'plether-verifying-v1',
  preparationRpcVersion: 1,
}

function recoveryFixture(action: SponsoredOperation['action'], interrupted = false): SponsoredOperation {
  const id = hash(action === 'deposit' ? 'a' : 'b')
  return {
    ...operation({ id, action, status: interrupted ? 'preparation-pending' : 'signature-declined', minutesAgo: 1 }),
    walletPreparationOutcome: interrupted ? 'unknown' : 'declined',
    walletPreparationRevision: 1,
    nativePreparation: {
      version: 1, preparationId: id, manifest: NATIVE_MANIFEST,
      action: { kind: action, account: ACCOUNT_ADDRESS, calls: [] },
    },
    preparedOperation: {
      version: 1, expectedHash: hash('c'), validUntil: '2000000000',
      operation: {
        sender: ACCOUNT_ADDRESS, nonce: '0', callData: '0x', callGasLimit: '100000',
        verificationGasLimit: '100000', preVerificationGas: '50000',
        maxFeePerGas: '10000000', maxPriorityFeePerGas: '1000000',
      },
    },
  }
}

const RESUMABLE_STATUS: PreparationStatusV1 = {
  version: 1, authorizationState: 'signed', phase: 'prepared', reason: 'RESUMABLE',
  recoverable: true, freshReviewAllowed: true, validUntil: '2000000000',
  serverTime: String(NOW / 1000), safeBlockTimestamp: String(NOW / 1000 - 60),
  userOperationHash: hash('c'), transactionHash: null,
}

function previewRuntime(status: PreparationStatusV1): PerpsAaSmartAccountRuntime {
  // Status is local fixture data. Signing/submission are deliberately unavailable
  // in these visual examples; never connect a Storybook fixture to a real wallet.
  const unavailable = async (): Promise<never> => {
    throw new Error('This Storybook example previews recovery controls. Wallet signing is available in the app.')
  }
  return {
    chainId: MANIFEST.chainId, ownerAddress: OWNER_ADDRESS,
    factoryAddress: MANIFEST.smartAccountFactory,
    accountVersion: MANIFEST.smartAccountVersion, accountIndex: MANIFEST.smartAccountIndex,
    smartAccount: {
      accountAddress: ACCOUNT_ADDRESS, entryPoint: MANIFEST.entryPoint,
      getPreparationStatus: async () => status,
      prepareUserOperation: unavailable, signUserOperation: unavailable,
      sendUserOperation: unavailable, getUserOperationReceipt: unavailable,
      getUserOperationStatus: unavailable, getUserOperationHash: () => hash('c'),
    },
  }
}

function recoveryStory(action: SponsoredOperation['action'], status = RESUMABLE_STATUS, interrupted = false): Story {
  const operations = [recoveryFixture(action, interrupted)]
  const runtime = previewRuntime(status)
  return {
    parameters: { docs: { description: { story: 'Real activity UI with a mocked preparation-status response. Wallet signing and submission are unavailable in this visual preview.' } } },
    render: () => <WalletHeaderPreview operations={operations} runtime={runtime} />,
    play: async ({ canvasElement }) => {
      await userEvent.click(await within(canvasElement).findByRole('button', { name: /Open Trading Account activity/ }))
      const dialog = within(await within(document.body).findByRole('dialog'))
      await waitFor(() => {
        if (status.recoverable) expect(dialog.getByRole('button', { name: /^Resume / })).toBeEnabled()
        else expect(dialog.queryByRole('button', { name: /^Resume / })).not.toBeInTheDocument()
        if (status.freshReviewAllowed && !interrupted) expect(dialog.getByRole('button', { name: 'Discard saved transaction' })).toBeEnabled()
        else expect(dialog.getByRole('button', { name: 'Discard saved transaction' })).toBeDisabled()
      })
    },
  }
}

export const SignatureDeclinedDeposit: Story = recoveryStory('deposit')
export const SignatureDeclinedOrder: Story = recoveryStory('place-order')
export const InterruptedWalletRecovery: Story = recoveryStory('deposit', RESUMABLE_STATUS, true)
export const SponsorshipReservationWait: Story = recoveryStory('place-order', {
  ...RESUMABLE_STATUS, phase: 'expiry-awaiting-reconciliation', reason: 'SAFE_EXPIRY_WAIT',
  recoverable: false, freshReviewAllowed: false,
})

const recoveryPreviewTransport = custom({
  request: async () => {
    throw new Error('Live RPC calls are unavailable in this recovery preview.')
  },
}, { retryCount: 0 })
const recoveryTradeConfig = createConfig({
  chains: [mainnet, sepolia, arbitrumSepolia, anvil],
  storage: null,
  transports: {
    [mainnet.id]: recoveryPreviewTransport,
    [sepolia.id]: recoveryPreviewTransport,
    [arbitrumSepolia.id]: recoveryPreviewTransport,
    [anvil.id]: recoveryPreviewTransport,
  },
})
recoveryTradeConfig.setState((state) => ({ ...state, chainId: arbitrumSepolia.id }))
const recoveryTradeQueryClient = new QueryClient({
  defaultOptions: { queries: { retry: false, refetchOnWindowFocus: false } },
})
const recoveryTradeOperations = [recoveryFixture('deposit')]
const recoveryTradeRuntime = previewRuntime(RESUMABLE_STATUS)

export const RecoveryFromTradeForm: Story = {
  parameters: {
    docs: {
      description: {
        story: 'The real trade form with a saved, declined deposit. Review saved transaction leads to Resume and Discard. Status is mocked; wallet signing and live RPC calls are unavailable.',
      },
    },
  },
  render: () => (
    <WagmiProvider config={recoveryTradeConfig}>
      <QueryClientProvider client={recoveryTradeQueryClient}>
        <WalletHeaderPreview operations={recoveryTradeOperations} runtime={recoveryTradeRuntime}>
          <div className="mx-auto mt-6 max-w-md">
            <PerpsTradeTicket
              enableLiveTrading
              availableToTradeRaw={1_000_000_000n}
              oraclePriceRaw={98_300_000n}
              oraclePriceDisplay="98.30"
            />
          </div>
        </WalletHeaderPreview>
      </QueryClientProvider>
    </WagmiProvider>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const recoveryButton = await canvas.findByRole('button', { name: 'Review saved transaction' })
    expect(await canvas.findByText(/A saved transaction needs attention/)).toBeVisible()
    expect(canvas.queryByText('A Trading Account action is in progress. Wait for it to finish.')).not.toBeInTheDocument()
    await userEvent.click(recoveryButton)
    const dialog = within(await within(document.body).findByRole('dialog'))
    await waitFor(() => {
      expect(dialog.getByRole('button', { name: /^Resume / })).toBeEnabled()
      expect(dialog.getByRole('button', { name: 'Discard saved transaction' })).toBeEnabled()
    })
    await userEvent.click(dialog.getByRole('button', { name: 'Close dialog' }))
    await waitFor(() => expect(within(document.body).queryByRole('dialog')).not.toBeInTheDocument())
    expect(recoveryButton).toBeVisible()
  },
}

const expiredSignedStatus: PreparationStatusV1 & { recoveryVerified: true; canRetire: boolean } = {
  ...RESUMABLE_STATUS, phase: 'resolved', authorizationState: 'expired',
  recoverable: false, reason: 'PREPARATION_UNUSABLE', recoveryVerified: true as const, canRetire: true,
}
const signedRecoveryOperations: SponsoredOperation[] = [{
  ...recoveryFixture('place-order'), status: 'receipt-timeout', userOperationHash: hash('c'),
}]
const signedRecoveryRuntime: PerpsAaSmartAccountRuntime = {
  ...previewRuntime(expiredSignedStatus),
  preparationRecovery: {
    // Local UI simulation only: no wallet, signing, or submission is connected.
    verify: async () => {},
    status: async () => expiredSignedStatus,
    retire: async () => { throw new Error('Signed attempts cannot be discarded in this preview') },
    headers: () => ({}), bindOperation: () => {}, operationHeaders: () => ({}),
  },
}
export const SignedRecoveryVerifyFirst: Story = {
  parameters: { docs: { description: { story: 'An expired sponsorship with an unconfirmed signed transaction. Verification is simulated locally; no wallet request or transaction is sent.' } } },
  render: () => <WalletHeaderPreview operations={signedRecoveryOperations} runtime={signedRecoveryRuntime} />,
  play: async ({ canvasElement }) => {
    await userEvent.click(await within(canvasElement).findByRole('button', { name: /Open Trading Account activity/ }))
    const dialog = within(await within(document.body).findByRole('dialog'))
    expect(await dialog.findByText('1. Verify your wallet')).toBeVisible()
    expect(dialog.queryByRole('button', { name: 'Check recovery again' })).not.toBeInTheDocument()
    expect(dialog.queryByText(/sponsorship has.*expired/)).not.toBeInTheDocument()
  },
}
export const SignedRecoveryCheckingOutcome: Story = {
  ...SignedRecoveryVerifyFirst,
  play: async context => {
    await SignedRecoveryVerifyFirst.play?.(context)
    const dialog = within(await within(document.body).findByRole('dialog'))
    await userEvent.click(dialog.getByRole('button', { name: 'Verify wallet to recover' }))
    expect(await dialog.findByText('2. Check transaction outcome')).toBeVisible()
    expect(await dialog.findByText(/This alone does not confirm the transaction outcome or unlock trading/)).toBeVisible()
    expect(dialog.queryByRole('button', { name: 'Verify wallet to recover' })).not.toBeInTheDocument()
    expect(dialog.getByRole('button', { name: 'Check recovery again' })).toBeEnabled()
  },
}
