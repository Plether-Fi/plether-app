import type { Meta, StoryObj } from '@storybook/react-vite'
import { type ReactNode } from 'react'
import { OperationStateCard } from '../components/PerpsTradingAccountPanel'
import { PerpsClaimPanel } from '../components/PerpsClaimPanel'
import { CloseSettlementReconciliationPanel } from '../components/documentation/CloseSettlementReconciliationPanel'

const TRADING_ACCOUNT = '0x62A9c44fAbC68B6dE62059E827cE972bD09E6c18'

const meta: Meta<typeof PerpsClaimPanel> = {
  title: 'Documentation/Trader Claims',
  component: PerpsClaimPanel,
  parameters: {
    layout: 'fullscreen',
  },
  args: {
    claimUsdc: '800.00',
    status: 'waiting',
    tradingAccountAddress: TRADING_ACCOUNT,
    marginAccountUsdc: '2 450.00',
    aggregateClaimsUsdc: '1 200.00',
    housePoolAssetsUsdc: '1 100.00',
    coverageRatio: '91.7%',
  },
}

export default meta
type Story = StoryObj<typeof meta>

function Frame({ children }: { children: ReactNode }) {
  return (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-2xl">{children}</div>
    </div>
  )
}

export const WaitingForLiquidity: Story = {
  render: (args) => <Frame><PerpsClaimPanel {...args} /></Frame>,
}

export const AvailableToSettle: Story = {
  args: {
    status: 'available',
    housePoolAssetsUsdc: '1 250.00',
    coverageRatio: '104.2%',
  },
  render: (args) => <Frame><PerpsClaimPanel {...args} /></Frame>,
}

export const SettlementConfirmation: Story = {
  args: {
    status: 'available',
    housePoolAssetsUsdc: '1 250.00',
    coverageRatio: '104.2%',
    initialConfirmationOpen: true,
  },
  render: (args) => <Frame><PerpsClaimPanel {...args} /></Frame>,
}

export const SuccessfullySettled: Story = {
  args: {
    claimUsdc: '0.00',
    status: 'settled',
    marginAccountUsdc: '3 250.00',
    settledCreditUsdc: '800.00',
    aggregateClaimsUsdc: '400.00',
    housePoolAssetsUsdc: '400.00',
    coverageRatio: '100%',
  },
  render: (args) => <Frame><PerpsClaimPanel {...args} /></Frame>,
}

export const CloseCreatedClaim: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto grid max-w-5xl gap-5 lg:grid-cols-2">
        <OperationStateCard
          title="Close executed"
          stage="Settlement result"
          message="The position closed successfully. Released margin returned immediately, while the fresh positive payout became a trader claim."
          tone="success"
          identifierLabel="Finalization transaction"
          identifier="0x75c4...1032"
        />
        <PerpsClaimPanel
          claimUsdc="800.00"
          status="waiting"
          tradingAccountAddress={TRADING_ACCOUNT}
          marginAccountUsdc="2 450.00"
        />
      </div>
    </div>
  ),
}

export const CompletedFullClose: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-6xl">
        <CloseSettlementReconciliationPanel
          settlementItems={[
            { label: 'Realized price PnL', amount: '+390.00', tone: 'positive' },
            { label: 'Signed VPI', amount: '-12.00', tone: 'negative' },
            { label: 'Execution fee', amount: '-4.00', tone: 'negative' },
            { label: 'Carry', amount: '-6.00', tone: 'negative' },
            { label: 'Frozen spread assessed', amount: '24.00', tone: 'warning' },
            { label: 'Frozen spread paid', amount: '-18.00', tone: 'negative' },
            { label: 'Frozen spread waived', amount: '6.00', tone: 'warning' },
            {
              label: 'Net settlement',
              amount: '+350.00',
              tone: 'positive',
              detail: 'Fresh HousePool-funded payout after costs',
            },
          ]}
          fundingItems={[
            {
              label: 'Released margin',
              amount: '+1 000.00',
              tone: 'positive',
              detail: 'Existing position collateral',
            },
            {
              label: 'Immediate Margin Account credit',
              amount: '+1 000.00',
              tone: 'positive',
              detail: 'Released margin only',
            },
            {
              label: 'Trader claim created',
              amount: '350.00',
              tone: 'warning',
              detail: 'Complete fresh payout recorded in full',
            },
          ]}
          message={(
            <>
              The released margin was credited immediately. Because the HousePool could not fund the complete fresh
              <strong className="font-semibold text-content-primary"> 350.00 USDC </strong>
              payout, none of it was paid immediately; the full amount became a trader claim.
            </>
          )}
          transactionHash="0x75c4000000000000000000000000000000000000000000000000000000001032"
        />
      </div>
    </div>
  ),
}

export const FrozenCloseResult: Story = {
  render: () => (
    <Frame>
      <OperationStateCard
        title="Frozen-market close executed"
        stage="Final settlement"
        message="The position closed successfully. The fixed frozen-market spread is reconciled separately from VPI, fees and carry."
        tone="success"
        identifierLabel="Finalization transaction"
        identifier="0x8af100000000000000000000000000000000000000000000000000000000d0f2"
        action={(
          <dl className="border border-brand-border/20 bg-app-bg px-4 text-sm">
            {[
              ['Realized price PnL', '+390.00 USDC'],
              ['VPI / Price impact', '-12.00 USDC'],
              ['Frozen close spread assessed', '24.00 USDC'],
              ['Frozen close spread paid to LPs', '-18.00 USDC'],
              ['Frozen close spread waived', '6.00 USDC'],
              ['Execution fee', '-4.00 USDC'],
              ['Accrued carry', '-6.00 USDC'],
              ['Net close settlement', '+350.00 USDC'],
            ].map(([label, value]) => (
              <div key={label} className="flex justify-between gap-4 border-b border-brand-border/15 py-2.5 last:border-b-0">
                <dt className="text-content-secondary">{label}</dt>
                <dd className="text-right font-semibold text-content-primary">{value}</dd>
              </div>
            ))}
          </dl>
        )}
      />
    </Frame>
  ),
}
