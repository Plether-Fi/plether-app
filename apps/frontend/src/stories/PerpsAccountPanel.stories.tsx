import type { Meta, StoryObj } from '@storybook/react-vite'
import { userEvent, within } from 'storybook/test'
import { PerpsAccountPanel } from '../components/PerpsAccountPanel'
import type {
  PerpsOrderHistoryRow,
  PerpsPendingOrder,
  PerpsPosition,
  PerpsTradeHistoryRow,
} from '../hooks'

const USDC = 1_000_000n
const POSITION_SIZE = 2_000n * 10n ** 18n
const NOW_SECONDS = Math.floor(Date.now() / 1_000)

const connectedPosition = {
  exists: true,
  side: 0,
  direction: 'long',
  size: POSITION_SIZE,
  entryPrice: 98_300_000n,
  marginUsdc: 400n * USDC,
  unrealizedPnlUsdc: 48_250_000n,
  maintenanceMarginUsdc: 20n * USDC,
  liquidatable: false,
  estimatedNotionalUsdc: 1_999_920_000n,
  entryNotionalUsdc: 2_000_000_000n,
  dxyExposureUsdc: 2_069_380_000n,
  displayDxyPrice: 101_700_000n,
  liquidationPrice: 110_000_000n,
  pendingCarryUsdc: 1_250_000n,
} satisfies PerpsPosition

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
    failureReason: 'SlippageExceeded',
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
    failureReason: 'Expired',
    commitTxHash: '0x6800000000000000000000000000000000000000000000000000000000000001',
  },
] satisfies PerpsOrderHistoryRow[]

const closeTradeHistory = [
  {
    time: '15:22',
    market: 'plDXY Perp',
    side: 'Close Long',
    price: '1.0412',
    size: '650',
    pnl: '+75.40',
    txHash: '0x7500000000000000000000000000000000000000000000000000000000000001',
  },
] satisfies PerpsTradeHistoryRow[]

const meta: Meta<typeof PerpsAccountPanel> = {
  title: 'Perps/Account Panel',
  component: PerpsAccountPanel,
  tags: ['autodocs'],
  parameters: {
    layout: 'fullscreen',
  },
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
          equityUsdc={1_248_250_000n}
          freeBuyingPowerUsdc={848_250_000n}
          position={connectedPosition}
        />
      </div>
    </div>
  ),
}

export const EditPositionMargin: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-5xl">
        <PerpsAccountPanel
          initialPositionMarginModalOpen
          isConnected
          equityUsdc={1_248_250_000n}
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
        <PerpsAccountPanel initialTab="tradeHistory" isConnected tradeHistory={closeTradeHistory} />
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
