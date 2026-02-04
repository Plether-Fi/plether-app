import type { Meta, StoryObj } from '@storybook/react-vite'
import { DashboardTile } from '../components/DashboardTile'

const meta: Meta<typeof DashboardTile> = {
  title: 'Components/DashboardTile',
  component: DashboardTile,
  tags: ['autodocs'],
  parameters: {
    controls: { disable: true },
  },
  decorators: [
    (Story) => (
      <div className="max-w-sm">
        <Story />
      </div>
    ),
  ],
}

export default meta
type Story = StoryObj<typeof meta>

export const Bull: Story = {
  render: () => (
    <DashboardTile
      variant="bull"
      title="plDXY-BULL Position"
      balance={1500000000000000000000n}
      balanceDecimals={18}
      balanceToken="plDXY-BULL"
      secondaryValue={750000000000000000000n}
      secondaryLabel="Staked Balance"
      secondaryDecimals={18}
      secondaryToken="plDXY-BULL"
    />
  ),
}

export const USDC: Story = {
  render: () => (
    <DashboardTile
      variant="usdc"
      title="USDC Lending"
      balance={5000000000n}
      balanceDecimals={6}
      balanceToken="USDC"
      secondaryValue={2500000000n}
      secondaryLabel="Total Lending"
      secondaryDecimals={6}
      secondaryToken="USDC"
    />
  ),
}

export const Bear: Story = {
  render: () => (
    <DashboardTile
      variant="bear"
      title="plDXY-BEAR Position"
      balance={2000000000000000000000n}
      balanceDecimals={18}
      balanceToken="plDXY-BEAR"
      secondaryValue={1000000000000000000000n}
      secondaryLabel="Staked Balance"
      secondaryDecimals={18}
      secondaryToken="plDXY-BEAR"
    />
  ),
}

export const WithAPY: Story = {
  render: () => (
    <DashboardTile
      variant="usdc"
      title="USDC Lending"
      balance={5000000000n}
      balanceDecimals={6}
      balanceToken="USDC"
      secondaryValue={2500000000n}
      secondaryLabel="Total Lending"
      secondaryDecimals={6}
      secondaryToken="USDC"
      apy={5.25}
    />
  ),
}

export const ZeroBalances: Story = {
  render: () => (
    <DashboardTile
      variant="bull"
      title="plDXY-BULL Position"
      balance={0n}
      balanceDecimals={18}
      balanceToken="plDXY-BULL"
      secondaryValue={0n}
      secondaryLabel="Staked Balance"
      secondaryDecimals={18}
      secondaryToken="plDXY-BULL"
    />
  ),
}

export const Loading: Story = {
  render: () => (
    <DashboardTile
      variant="bear"
      title="plDXY-BEAR Position"
      balance={0n}
      balanceDecimals={18}
      balanceToken="plDXY-BEAR"
      secondaryValue={0n}
      secondaryLabel="Staked Balance"
      secondaryDecimals={18}
      secondaryToken="plDXY-BEAR"
      isLoading
    />
  ),
}

export const AllVariants: Story = {
  render: () => (
    <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
      <DashboardTile
        variant="bull"
        title="plDXY-BULL Position"
        balance={1500000000000000000000n}
        balanceDecimals={18}
        balanceToken="plDXY-BULL"
        secondaryValue={750000000000000000000n}
        secondaryLabel="Staked Balance"
        secondaryDecimals={18}
        secondaryToken="plDXY-BULL"
      />
      <DashboardTile
        variant="usdc"
        title="USDC Lending"
        balance={5000000000n}
        balanceDecimals={6}
        balanceToken="USDC"
        secondaryValue={2500000000n}
        secondaryLabel="Total Lending"
        secondaryDecimals={6}
        secondaryToken="USDC"
      />
      <DashboardTile
        variant="bear"
        title="plDXY-BEAR Position"
        balance={2000000000000000000000n}
        balanceDecimals={18}
        balanceToken="plDXY-BEAR"
        secondaryValue={1000000000000000000000n}
        secondaryLabel="Staked Balance"
        secondaryDecimals={18}
        secondaryToken="plDXY-BEAR"
      />
    </div>
  ),
  decorators: [
    (Story) => (
      <div className="max-w-4xl">
        <Story />
      </div>
    ),
  ],
}
