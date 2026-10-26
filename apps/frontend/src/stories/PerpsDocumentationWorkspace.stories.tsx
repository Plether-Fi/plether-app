import { PERPS_SETTLEMENT_TOKEN_LABEL } from '../contracts/perpsAddresses'
import { accountPositionFixture, accountStoryIdentity } from './perpsAccountFixtures'
import { PerpsIdentityContext } from '../perps-aa'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { Button, TokenAmount } from '../components/ui'
import { PerpsAccountPanel } from '../components/PerpsAccountPanel'
import { PerpsInstrumentPanel } from '../components/PerpsInstrumentPanel'
import { PerpsMarketStatePanel } from '../components/PerpsMarketStatePanel'
import { PerpsTradeTicket } from '../components/PerpsTradeTicket'
import {
  OperationStateCard,
  PerpsTradingAccountPanel,
} from '../components/PerpsTradingAccountPanel'


const ASSETS = 1_000_000n
const position = accountPositionFixture()

const accountPanel = (
  <PerpsTradingAccountPanel
    ownerWalletAddress="0x9B2F4e0E78E36D97f91c80D5B1aED422d3C2e741"
    tradingAccountAddress="0x62A9c44fAbC68B6dE62059E827cE972bD09E6c18"
    accountModel="smart-account"
    marginAccountAssets="1 248.25"
  />
)

function MarketHeader({ freshness = 'fresh' }: { freshness?: 'fresh' | 'market-closed' | 'stale' }) {
  return (
    <PerpsInstrumentPanel
      stats={[
        {
          label: 'plDXY Perp price',
          value: '1.0170',
          freshness,
          freshnessTooltip: freshness === 'fresh'
            ? 'updated 18s ago'
            : freshness === 'market-closed'
              ? 'using validated frozen basket from 8h 12m ago'
              : 'last validated update 4m 38s ago',
        },
        { label: '24h change', value: '+0.24%', tone: 'positive' },
        { label: 'FAD status', value: freshness === 'fresh' ? 'Live' : 'Close-only' },
        { label: 'Oracle state', value: freshness === 'market-closed' ? 'Frozen' : freshness === 'stale' ? 'Stale' : 'Fresh' },
        {
          label: 'Directional limit used',
          directionalLimit: {
            usagePercent: 87,
            side: 'long',
            totalExposure: <TokenAmount token={PERPS_SETTLEMENT_TOKEN_LABEL} amount="8.36M" />,
            netExposure: <TokenAmount token={PERPS_SETTLEMENT_TOKEN_LABEL} amount="3.07M" />,
            limit: <TokenAmount token={PERPS_SETTLEMENT_TOKEN_LABEL} amount="3.53M" />,
          },
        },
        { label: 'Free pool liquidity', value: <TokenAmount token={PERPS_SETTLEMENT_TOKEN_LABEL} amount="6.3M" /> },
      ]}
    />
  )
}

function DocumentationWorkspace() {
  return <div />
}

const meta: Meta<typeof DocumentationWorkspace> = {
  title: 'Documentation/Trader Workspace',
  component: DocumentationWorkspace,
  decorators: [(Story) => <PerpsIdentityContext.Provider value={accountStoryIdentity}><Story /></PerpsIdentityContext.Provider>],
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta
type Story = StoryObj<typeof meta>

export const MarketAndAccountReadiness: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-7xl space-y-5">
        <MarketHeader />
        <PerpsMarketStatePanel
          currentPhase="open"
          currentDuration="1d 12h"
          nextPhase="close-only"
          nextDuration="3h"
        />
        <div className="grid gap-5 lg:grid-cols-[minmax(0,1fr)_420px]">
          <div className="space-y-5">
            {accountPanel}
            <PerpsAccountPanel
              isConnected
              position={position}
              equityAssets={448_250_000n}
              freeBuyingPowerAssets={848_250_000n}
            />
          </div>
          <PerpsTradeTicket
            initialDirection="long"
            initialOrderQuantity="2 000"
            oraclePriceRaw={98_300_000n}
            oraclePriceDisplay="1.0170"
            oracleFreshness="fresh"
            oracleFreshnessTooltip="updated 18s ago"
            availableToTradeRaw={848_250_000n}
            availableToTradeAmount="848.25"
            settlementBalanceAssetsRaw={1_248_250_000n}
            positionEquityAssetsRaw={position.positionEquityAssets}
            withdrawableAssetsRaw={847_000_000n}
            walletAssetsRaw={5_000_000_000n}
            currentPosition={position}
            maintenanceMarginBps={100n}
            initialMarginBps={200n}
            executionFeeBps={4n}
          />
        </div>
      </div>
    </div>
  ),
}

export const CloseOnlyReduceOnly: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-6xl space-y-5">
        <MarketHeader freshness="market-closed" />
        <PerpsMarketStatePanel
          currentPhase="close-only"
          currentDuration="1d 8h"
          nextPhase="open"
          nextDuration="1d"
        />
        <div className="ml-auto max-w-md">
          <PerpsTradeTicket
            initialDirection="short"
            initialOrderQuantity="500"
            initialReduceOnly
            oracleFrozen
            oraclePriceRaw={100_000_000n}
            oraclePriceDisplay="1.0000"
            oracleFreshness="market-closed"
            oracleFreshnessTooltip="using the validated stored basket"
            availableToTradeRaw={1_500n * ASSETS}
            availableToTradeAmount="1 500"
            settlementBalanceAssetsRaw={1_255n * ASSETS}
            withdrawableAssetsRaw={1_000n * ASSETS}
            walletAssetsRaw={2_000n * ASSETS}
            currentPosition={position}
            maintenanceMarginBps={100n}
            initialMarginBps={200n}
            executionFeeBps={4n}
            marketPhase="close-only"
            marketCurrentDuration="1d 8h"
          />
        </div>
      </div>
    </div>
  ),
}

export const DisabledReviewMessages: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto grid max-w-7xl gap-5 md:grid-cols-3">
        {[
          ['Insufficient margin', `Deposit 152.40 ${PERPS_SETTLEMENT_TOKEN_LABEL} more before committing this order.`],
          ['Minimum order size', `Minimum order size is 103.18 ${PERPS_SETTLEMENT_TOKEN_LABEL}.`],
          ['Skew capacity reached', `Max Long exposure is 953.33 ${PERPS_SETTLEMENT_TOKEN_LABEL} before hitting the market skew cap.`],
        ].map(([title, message]) => (
          <section key={title} className="border border-brand-border/30 bg-surface-panel p-5">
            <h2 className="text-lg font-semibold text-content-primary">{title}</h2>
            <div className="mt-5 border border-brand-orange/30 bg-brand-orange/10 p-4 text-sm leading-5 text-brand-peach">
              {message}
            </div>
            <Button className="mt-5 w-full" disabled>
              Review Long
            </Button>
          </section>
        ))}
      </div>
    </div>
  ),
}

export const ExecutedPartialReduction: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto grid max-w-7xl gap-5 lg:grid-cols-[minmax(0,1fr)_380px]">
        <PerpsAccountPanel
          isConnected
          position={{
            ...position,
            size: 1_500n * 10n ** 18n,
            marginAssets: 300n * ASSETS,
            estimatedNotionalAssets: 1_530n * ASSETS,
            dxyExposureAssets: 1_550n * ASSETS,
            unrealizedPnlAssets: 36_200_000n,
          }}
          equityAssets={1_336_200_000n}
          freeBuyingPowerAssets={1_036_200_000n}
        />
        <OperationStateCard
          title="Partial reduction executed"
          stage="Settlement result"
          message={`500 ${PERPS_SETTLEMENT_TOKEN_LABEL} of exposure closed. 100 ${PERPS_SETTLEMENT_TOKEN_LABEL} of position margin was released and 75.40 ${PERPS_SETTLEMENT_TOKEN_LABEL} of realized result was credited to the Margin Account.`}
          tone="success"
          identifierLabel="Finalization transaction"
          identifier="0x75c4000000000000000000000000000000000000000000000000000000001032"
        />
      </div>
    </div>
  ),
}

export const DegradedMarketAndAccount: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-7xl space-y-5">
        <MarketHeader freshness="stale" />
        <PerpsMarketStatePanel
          currentPhase="degraded"
          currentDuration="until recapitalized"
          nextPhase="open"
          nextDuration="after recovery"
        />
        <div className="grid gap-5 lg:grid-cols-2">
          {accountPanel}
          <PerpsAccountPanel
            isConnected
            position={position}
            equityAssets={448_250_000n}
            freeBuyingPowerAssets={848_250_000n}
          />
        </div>
      </div>
    </div>
  ),
}

export const PendingFinalizationAndCleanup: Story = {
  render: () => {
    const nowSeconds = Math.floor(Date.now() / 1_000)

    return (
      <div className="min-h-screen bg-app-bg p-4 md:p-8">
        <div className="mx-auto grid max-w-7xl gap-5 lg:grid-cols-[380px_minmax(0,1fr)]">
          <OperationStateCard
            title="Finalize Trade"
            stage="Pending reveal"
            message="Automatic keeper processing did not arrive during the grace period. The same order can now be finalized manually under the original FIFO, oracle and acceptable-price rules."
            tone="pending"
            identifierLabel="Order ID"
            identifier={`72 · Acceptable price 1.0240 · Reward 0.20 ${PERPS_SETTLEMENT_TOKEN_LABEL} · Ready now`}
            action={<Button className="w-full">Finalize Trade</Button>}
          />
          <PerpsAccountPanel
            initialTab="openOrders"
            isConnected
            pendingOrders={[
              {
                orderId: 72n,
                side: 0,
                direction: 'long',
                sizeDelta: 500n * 10n ** 18n,
                marginDeltaAssets: 100n * ASSETS,
                acceptablePrice: 102_400_000n,
                isReduceOnly: false,
                status: 1,
                estimatedNotionalAssets: 520n * ASSETS,
                expiryTime: BigInt(nowSeconds + 2_958),
              },
              {
                orderId: 71n,
                side: 0,
                direction: 'long',
                sizeDelta: 250n * 10n ** 18n,
                marginDeltaAssets: 0n,
                acceptablePrice: 99_100_000n,
                isReduceOnly: true,
                status: 1,
                estimatedNotionalAssets: 255n * ASSETS,
                expiryTime: BigInt(nowSeconds - 100),
              },
            ]}
          />
        </div>
      </div>
    )
  },
}
