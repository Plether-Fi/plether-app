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
