import type { Meta, StoryObj } from '@storybook/react-vite'
import { type ReactNode } from 'react'
import { Button } from '../components/ui'
import {
  OperationStateCard,
  PerpsTradingAccountPanel,
} from '../components/PerpsTradingAccountPanel'

const OWNER = '0x9B2F4e0E78E36D97f91c80D5B1aED422d3C2e741'
const ACCOUNT = '0x62A9c44fAbC68B6dE62059E827cE972bD09E6c18'
const USER_OPERATION = '0xd194d074cd07e66a0cd88de803b87cb675b5060cf0435f1250f2461c873eb924'

const meta: Meta<typeof PerpsTradingAccountPanel> = {
  title: 'Documentation/Trading Account and Sponsorship',
  component: PerpsTradingAccountPanel,
  parameters: {
    layout: 'fullscreen',
  },
  args: {
    ownerWalletAddress: OWNER,
    tradingAccountAddress: ACCOUNT,
    accountModel: 'smart-account',
    marginAccountUsdc: '12 480.25',
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

export const AccountIdentity: Story = {
  render: (args) => <Frame><PerpsTradingAccountPanel {...args} /></Frame>,
}

export const SameAddressAccount: Story = {
  args: {
    tradingAccountAddress: OWNER,
    accountModel: 'same-address',
  },
  render: (args) => <Frame><PerpsTradingAccountPanel {...args} /></Frame>,
}

export const SponsorUnavailable: Story = {
  args: {
    sponsorshipStatus: 'unavailable',
    retryAt: 'in 45s',
    supportedAlternative: 'Wait for sponsorship recovery. No automatic owner-wallet gas fallback is enabled.',
  },
  render: (args) => <Frame><PerpsTradingAccountPanel {...args} /></Frame>,
}

export const SponsorRateLimited: Story = {
  args: {
    sponsorshipStatus: 'rate-limited',
    retryAt: 'at 14:32 UTC',
  },
  render: (args) => <Frame><PerpsTradingAccountPanel {...args} /></Frame>,
}

export const BundlerRejected: Story = {
  args: {
    sponsorshipStatus: 'bundler-rejected',
  },
  render: (args) => <Frame><PerpsTradingAccountPanel {...args} /></Frame>,
}

export const UserOperationDropped: Story = {
  args: {
    sponsorshipStatus: 'user-operation-dropped',
  },
  render: (args) => <Frame><PerpsTradingAccountPanel {...args} /></Frame>,
}

export const FailureStateComparison: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto grid max-w-7xl gap-4 md:grid-cols-2">
        <OperationStateCard
          title="Sponsored operation failed"
          stage="Sponsored submission"
          message="The included commitment call reverted atomically. No order or reservation exists."
          tone="error"
          identifierLabel="Transaction hash"
          identifier="0x14f1...2c90"
        />
        <OperationStateCard
          title="UserOperation dropped"
          stage="Sponsored submission"
          message="The bundler stopped tracking the operation before confirmed inclusion. Check for a transaction hash before retrying."
          tone="error"
          identifierLabel="UserOperation hash"
          identifier={USER_OPERATION}
        />
        <OperationStateCard
          title="Finalization transaction failed"
          stage="Delayed order"
          message="The execution attempt reverted. The order remains Pending with its reservations unchanged."
          tone="error"
          identifierLabel="Order ID"
          identifier="72"
          action={<Button variant="secondary" size="sm">Retry Finalizing</Button>}
        />
        <OperationStateCard
          title="Order failed"
          stage="Delayed order"
          message="The order reached a terminal Slippage exceeded state. A fresh trade requires a new commitment."
          tone="error"
          identifierLabel="Order ID"
          identifier="69"
        />
      </div>
    </div>
  ),
}

export const FirstDepositAuthorization: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-5xl space-y-5">
        <PerpsTradingAccountPanel
          ownerWalletAddress={OWNER}
          tradingAccountAddress={ACCOUNT}
          accountModel="smart-account"
          marginAccountUsdc="0.00"
          sponsorshipStatus="available"
        />
        <div className="grid gap-4 md:grid-cols-2">
          <OperationStateCard
            title="USDC transfer authorization"
            stage="Wallet authorization"
            message="Authorize exactly 10 000 USDC for transfer from the Owner Wallet to the Trading Account."
            tone="success"
            identifierLabel="Destination"
            identifier={ACCOUNT}
          />
          <OperationStateCard
            title="Sponsored deposit operation"
            stage="Trading Account operation"
            message="Receive the authorized USDC, approve the clearinghouse and deposit the complete amount into the Margin Account as one atomic batch."
            tone="pending"
            identifierLabel="Amount"
            identifier="10 000 USDC"
          />
        </div>
      </div>
    </div>
  ),
}

export const WithdrawalConfirmation: Story = {
  render: () => (
    <div className="min-h-screen bg-app-bg p-4 md:p-8">
      <div className="mx-auto max-w-5xl space-y-5">
        <PerpsTradingAccountPanel
          ownerWalletAddress={OWNER}
          tradingAccountAddress={ACCOUNT}
          accountModel="smart-account"
          marginAccountUsdc="12 480.25"
          sponsorshipStatus="available"
        />
        <OperationStateCard
          title="Withdraw 1 500.00 USDC"
          stage="Wallet confirmation"
          message={`The sponsored operation withdraws eligible Margin Account USDC through the Trading Account and atomically transfers it to the verified owner wallet ${OWNER}.`}
          tone="pending"
          identifierLabel="Trading Account"
          identifier={ACCOUNT}
          action={<Button className="w-full">Authorize Sponsored Withdrawal</Button>}
        />
      </div>
    </div>
  ),
}
