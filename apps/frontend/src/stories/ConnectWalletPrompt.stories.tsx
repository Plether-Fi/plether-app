import type { Meta, StoryObj } from '@storybook/react-vite'
import { ConnectWalletPrompt } from '../components/ConnectWalletPrompt'

const meta: Meta<typeof ConnectWalletPrompt> = {
  title: 'Components/ConnectWalletPrompt',
  component: ConnectWalletPrompt,
  tags: ['autodocs'],
  argTypes: {
    description: {
      control: 'text',
      description: 'Custom description text (uses default if not provided)',
    },
  },
}

export default meta
type Story = StoryObj<typeof ConnectWalletPrompt>

export const Default: Story = {}

export const CustomDescription: Story = {
  args: {
    description: 'Connect your wallet to stake plDXY-BEAR and plDXY-BULL tokens.',
  },
}

export const HistoryPage: Story = {
  args: {
    description: 'Connect your wallet to view transaction history.',
  },
}
