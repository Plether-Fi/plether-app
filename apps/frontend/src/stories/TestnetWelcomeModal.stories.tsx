import type { Meta, StoryObj } from '@storybook/react-vite'
import { useState } from 'react'
import { TestnetWelcomeModalView } from '../components/TestnetWelcomeModal'
import type { TestnetFaucetClaim } from '../api/types'

const mintedClaim: TestnetFaucetClaim = {
  address: '0x6b72fE6CC52201a1eb7892A813C6C10cCe62745c',
  amount: '100000000000',
  token: '0xf1e1B188b87525C51ECe4bae8627ae621D769651',
  txHash: '0x59b206624fef9a56643ac88ca92b80fb263c4a64d1d619682dbfeb8e511fa8cf',
  status: 'minted',
}

const alreadyClaimedClaim: TestnetFaucetClaim = {
  ...mintedClaim,
  status: 'already_claimed',
}

const meta: Meta<typeof TestnetWelcomeModalView> = {
  title: 'Testnet/Welcome Modal',
  component: TestnetWelcomeModalView,
  parameters: {
    layout: 'fullscreen',
  },
  args: {
    isOpen: true,
    walletAddress: mintedClaim.address,
    onClose: () => {},
    onRequestFunds: () => {},
    onDeposit: () => {},
  },
}

export default meta
type Story = StoryObj<typeof meta>

function ModalFrame(args: React.ComponentProps<typeof TestnetWelcomeModalView>) {
  const [walletAddress, setWalletAddress] = useState(args.walletAddress)

  return (
    <div className="min-h-screen bg-app-bg p-6">
      <TestnetWelcomeModalView
        {...args}
        walletAddress={walletAddress}
        onWalletAddressChange={setWalletAddress}
      />
    </div>
  )
}

export const RequestFunds: Story = {
  render: (args) => <ModalFrame {...args} />,
}

export const MintedToWallet: Story = {
  args: {
    claim: mintedClaim,
  },
  render: (args) => <ModalFrame {...args} />,
}

export const AlreadyClaimed: Story = {
  args: {
    claim: alreadyClaimedClaim,
  },
  render: (args) => <ModalFrame {...args} />,
}

export const Error: Story = {
  args: {
    submitError: 'Internal error: faucet signer is not configured.',
  },
  render: (args) => <ModalFrame {...args} />,
}
