import { accountPositionFixture } from './perpsAccountFixtures'
import { useState } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { userEvent, within } from 'storybook/test'
import { PerpsAccountPanel } from '../components/PerpsAccountPanel'
import { PerpsTradeTicket } from '../components/PerpsTradeTicket'
import {
  PerpsIdentityContext,
  type PerpsIdentityContextValue,
} from '../perps-aa'
import type {
  PerpsOrderHistoryRow,
  PerpsPendingOrder,
  PerpsTradeHistoryRow,
} from '../hooks'

const USDC = 1_000_000n
const NOW_SECONDS = Math.floor(Date.now() / 1_000)
const STORY_ADDRESS = '0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B'

const STORY_IDENTITY: PerpsIdentityContextValue = {
  status: 'ready',
  ownerAddress: STORY_ADDRESS,
  accountAddress: STORY_ADDRESS,
  chainId: 421614,
  isAaManifestConfigured: false,
  sponsorshipEnabled: false,
  manifest: null,
  identity: null,
  proposedIdentity: null,
  changedIdentityFields: [],
  error: null,
  confirmIdentityAfterContinuityCheck: () => false,
  reloadIdentity: () => undefined,
}

const connectedPosition = accountPositionFixture()

const largeConnectedPosition = accountPositionFixture({ size: 4_000_000n * 10n ** 18n, entryCostUsdcAtoms: 3_875_200_000_000n, positionMarginUsdc: 800_000_000_000n }, 95_651_300n)

const pendingOrders = [
  {
    orderId: 72n,
    side: 0,
    direction: 'long',
    sizeDelta: 500n * 10n ** 18n,
    marginDeltaUsdc: 100n * USDC,
    acceptablePrice: 102_400_000n,
    isReduceOnly: false,
    status: 1,
    estimatedNotionalUsdc: 520n * USDC,
    commitTime: BigInt(NOW_SECONDS - 42),
    expiryTime: BigInt(NOW_SECONDS + 2_958),
  },
  {
    orderId: 71n,
    side: 0,
    direction: 'long',
    sizeDelta: 250n * 10n ** 18n,
    marginDeltaUsdc: 0n,
    acceptablePrice: 99_100_000n,
    isReduceOnly: true,
    status: 1,
    estimatedNotionalUsdc: 255n * USDC,
    commitTime: BigInt(NOW_SECONDS - 3_700),
    expiryTime: BigInt(NOW_SECONDS - 100),
  },
] satisfies PerpsPendingOrder[]

const failedOrderHistory = [
  {
    orderId: 69n,
    time: '14:31',
    market: 'plDXY Perp',
    side: 'Long',
    type: 'Open',
    price: 'Not executed',
    size: '1 250',
    status: 'Failed',
    account: STORY_ADDRESS,
    clientOrderId: `0x${'69'.repeat(32)}`,
    terminalReason: 'Slippage',
    commitTxHash: '0x6900000000000000000000000000000000000000000000000000000000000001',
    revealTxHash: '0x6900000000000000000000000000000000000000000000000000000000000002',
  },
  {
    orderId: 68n,
    time: '13:04',
    market: 'plDXY Perp',
    side: 'Short',
    type: 'Close',
    price: 'Not executed',
    size: '600',
    status: 'Expired',
    account: STORY_ADDRESS,
    clientOrderId: `0x${'68'.repeat(32)}`,
    terminalReason: 'Expired',
    commitTxHash: '0x6800000000000000000000000000000000000000000000000000000000000001',
  },
] satisfies PerpsOrderHistoryRow[]

const closeTradeHistory = [
  {
    orderId: 75n,
    activityType: 'Close',
    time: '15:22',
    market: 'plDXY Perp',
    side: 'Close Long',
    price: '1.0412',
    size: '650',
    pnl: '+75.40',
    txHash: '0x7500000000000000000000000000000000000000000000000000000000000001',
  },
] satisfies PerpsTradeHistoryRow[]

const closeOrderHistory = [{
  orderId: 75n,
  time: '15:22',
  market: 'plDXY Perp',
  side: 'Long',
  type: 'Close',
  price: '1.0412',
  size: '650',
  status: 'Executed',
  account: STORY_ADDRESS,
  clientOrderId: `0x${'75'.repeat(32)}`,
  commitTxHash: '0x7400000000000000000000000000000000000000000000000000000000000001',
  revealTxHash: closeTradeHistory[0].txHash,
  receiptHash: `0x${'76'.repeat(32)}`,
  activitySizeDeltaRaw: 650n * 10n ** 18n,
  receiptEconomics: {
    executionNotionalUsdc: (650n * USDC).toString(),
    executionBountyUsdc: '0',
    realizedPnlUsdc: '75400000',
    vpiUsdc: '2000000',
    carryUsdc: '4000000',
    executionFeeUsdc: '1000000',
    frozenSpreadUsdc: '0',
    actionChargeAssessedUsdc: '6000000',
    actionChargeCollectedUsdc: '6000000',
    grossAccountDebitUsdc: '7000000',
    preSettlementBalanceUsdc: (1_000n * USDC).toString(),
    postSettlementBalanceUsdc: '1068400000',
    preTraderClaimBalanceUsdc: '0',
    postTraderClaimBalanceUsdc: '0',
    postPositionSize: '0',
    postPositionMarginUsdc: '0',
    postPositionEquityUsdc: '0',
    postLeverageBps: '0',
  },
}] satisfies PerpsOrderHistoryRow[]

const meta: Meta<typeof PerpsAccountPanel> = {
  title: 'Perps/Account Panel',
  component: PerpsAccountPanel,
  tags: ['autodocs'],
  parameters: {
    layout: 'fullscreen',
  },
  decorators: [
    (Story) => (
      <PerpsIdentityContext.Provider value={STORY_IDENTITY}>
        <Story />
      </PerpsIdentityContext.Provider>
    ),
  ],
}

export default meta
type Story = StoryObj<typeof meta>

export const Default: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-5xl">
        <PerpsAccountPanel />
      </div>
    </div>
  ),
}

export const ConnectedPosition: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-5xl">
        <PerpsAccountPanel
          isConnected
          equityUsdc={448_250_000n}
          freeBuyingPowerUsdc={848_250_000n}
          position={connectedPosition}
          onClosePosition={() => undefined}
        />
      </div>
    </div>
  ),
}

export const LargeConnectedPosition: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-5xl">
        <PerpsAccountPanel
          isConnected
          equityUsdc={largeConnectedPosition.positionEquityUsdc}
          freeBuyingPowerUsdc={248_250n * USDC}
          position={largeConnectedPosition}
          onClosePosition={() => undefined}
        />
      </div>
    </div>
  ),
}

function ClosePositionFlowStory() {
  const [closePositionRequestId, setClosePositionRequestId] = useState(0)

  return (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto grid max-w-6xl gap-6 lg:grid-cols-[minmax(0,1fr)_320px]">
        <PerpsAccountPanel
          isConnected
          equityUsdc={448_250_000n}
          freeBuyingPowerUsdc={848_250_000n}
          position={connectedPosition}
          onClosePosition={() => {
            setClosePositionRequestId((requestId) => requestId + 1)
          }}
        />
        <PerpsTradeTicket
          closePositionRequestId={closePositionRequestId}
          currentPosition={connectedPosition}
          oraclePriceRaw={96_531_000n}
        />
      </div>
    </div>
  )
}

export const ClosePositionFlow: Story = {
  render: () => <ClosePositionFlowStory />,
}

export const EditPositionMargin: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-5xl">
        <PerpsAccountPanel
          initialPositionMarginModalOpen
          isConnected
          equityUsdc={448_250_000n}
          freeBuyingPowerUsdc={848_250_000n}
          position={connectedPosition}
        />
      </div>
    </div>
  ),
}

export const OpenOrdersPending: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-6xl">
        <PerpsAccountPanel initialTab="openOrders" isConnected pendingOrders={[pendingOrders[0]]} />
      </div>
    </div>
  ),
}

export const OpenOrdersPendingAndExpired: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-6xl">
        <PerpsAccountPanel initialTab="openOrders" isConnected pendingOrders={pendingOrders} />
      </div>
    </div>
  ),
}

export const OrderHistoryFailures: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-6xl">
        <PerpsAccountPanel initialTab="orderHistory" isConnected orderHistory={failedOrderHistory} />
      </div>
    </div>
  ),
}

export const TransactionHistoryCloseResult: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-6xl">
        <PerpsAccountPanel
          initialTab="tradeHistory"
          isConnected
          orderHistory={closeOrderHistory}
          tradeHistory={closeTradeHistory}
        />
      </div>
    </div>
  ),
}

export const UnrealizedPnlTooltip: Story = {
  render: ConnectedPosition.render,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    await userEvent.hover(canvas.getByLabelText('Unrealized PnL details'))
  },
}


export const LiquidationRegression: Story = {
  render: () => <div className="min-h-screen bg-app-bg p-8"><PerpsAccountPanel isConnected
    equityUsdc={250_000_000n} freeBuyingPowerUsdc={750_000_000n}
    position={accountPositionFixture({ size: 10_000n * 10n ** 18n, entryCostUsdcAtoms: 10_000_000_000n, positionMarginUsdc: 250_000_000n }, 100_000_000n)} /></div>,
}
export const UnavailableRisk: Story = {
  render: () => <div className="min-h-screen bg-app-bg p-8"><PerpsAccountPanel isConnected
    position={{ ...connectedPosition, riskStatus: 'unavailable', liquidationPrice: undefined, liquidationThreshold: { status: 'unavailable' }, pendingCarryUsdc: undefined }} /></div>,
}
export const LiquidatableCarry: Story = {
  render: () => <div className="min-h-screen bg-app-bg p-8"><PerpsAccountPanel isConnected
    position={{ ...accountPositionFixture({ positionMarginUsdc: 10_000_000_000n }), liquidatable: true, uncoveredCarryUsdc: 1_250_000n }} /></div>,
}
