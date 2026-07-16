import type { Meta, StoryObj } from '@storybook/react-vite'
import { type ReactNode } from 'react'
import { OperationStateCard } from '../components/PerpsTradingAccountPanel'
import { PerpsClaimPanel } from '../components/PerpsClaimPanel'

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
  },
  render: (args) => <Frame><PerpsClaimPanel {...args} /></Frame>,
}

export const SettlementConfirmation: Story = {
  args: {
    status: 'available',
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
      <div className="mx-auto grid max-w-5xl gap-5 lg:grid-cols-2">
        <OperationStateCard
          title="Full close executed"
          stage="Final settlement"
          message="The complete position closed and its remaining margin was released."
          tone="success"
          identifierLabel="Finalization transaction"
          identifier="0x75c4000000000000000000000000000000000000000000000000000000001032"
          action={(
            <dl className="border border-brand-border/20 bg-app-bg px-4 text-sm">
              {[
                ['Realized price PnL', '+390.00 USDC'],
                ['VPI / Price impact', '-12.00 USDC'],
                ['Execution fee', '-4.00 USDC'],
                ['Accrued carry', '-6.00 USDC'],
                ['Released margin', '+1 000.00 USDC'],
                ['Immediate Margin Account credit', '+1 000.00 USDC'],
                ['Trader claim created', '368.00 USDC'],
              ].map(([label, value]) => (
                <div key={label} className="flex justify-between gap-4 border-b border-brand-border/15 py-2.5 last:border-b-0">
                  <dt className="text-content-secondary">{label}</dt>
                  <dd className="text-right font-semibold text-content-primary">{value}</dd>
                </div>
              ))}
            </dl>
          )}
        />
        <PerpsClaimPanel
          claimUsdc="368.00"
          status="waiting"
          tradingAccountAddress={TRADING_ACCOUNT}
          marginAccountUsdc="3 450.00"
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
