import type { Meta, StoryObj } from '@storybook/react-vite'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { useState, type ReactNode } from 'react'
import { expect, userEvent, within } from 'storybook/test'
import { StakingCard } from '../components/StakingCard'
import { apiQueryKeys, type ApiResponse, type ProtocolStatus } from '../api'

// 1,250 underlying tokens back 1,000 shares (18 asset / 21 share decimals).
// A 100-token stake therefore returns 80 shares, not 100 or 125.
const protocolStatus: ApiResponse<ProtocolStatus> = {
  data: {
    prices: { bear: '100000000', bull: '100000000', cap: '200000000', priceChange24h: null },
    status: 'ACTIVE', oracle: { price: '100000000', updatedAt: 1_800_000_000, decimals: 8 },
    staking: {
      bear: { totalAssets: '1250000000000000000000', totalShares: '1000000000000000000000000', exchangeRate: '1250000000000000000' },
      bull: { totalAssets: '1250000000000000000000', totalShares: '1000000000000000000000000', exchangeRate: '1250000000000000000' },
      apy7d: { bear: null, bull: null },
    },
    apy: { bear: { supply: 0, borrow: 0, utilization: 0 }, bull: { supply: 0, borrow: 0, utilization: 0 } },
    timestamp: 1_800_000_000,
  },
  meta: { cached: false, blockNumber: 1, chainId: 1 },
}

function StakingProtocolFixture({ children }: { children: ReactNode }) {
  const [client] = useState(() => {
    const queryClient = new QueryClient({ defaultOptions: { queries: { enabled: false } } })
    queryClient.setQueryData(apiQueryKeys.protocol.status(), protocolStatus)
    return queryClient
  })
  return <QueryClientProvider client={client}>{children}</QueryClientProvider>
}

interface StakingCardArgs {
  side: 'BEAR' | 'BULL'
  tokenBalance: number
  stakedBalance: number
}

const meta: Meta<StakingCardArgs> = {
  title: 'Components/StakingCard',
  tags: ['autodocs'],
  decorators: [(Story) => <StakingProtocolFixture><Story /></StakingProtocolFixture>],
  argTypes: {
    side: {
      control: 'radio',
      options: ['BEAR', 'BULL'],
      description: 'Token side to stake',
    },
    tokenBalance: {
      control: { type: 'number', min: 0 },
      description: 'Available token balance',
    },
    stakedBalance: {
      control: { type: 'number', min: 0 },
      description: 'Currently staked shares balance',
    },
  },
}

export default meta
type Story = StoryObj<StakingCardArgs>

function toTokenBigint(value: number): bigint {
  return BigInt(Math.floor(value * 1e18))
}

function toShareBigint(value: number): bigint {
  return BigInt(Math.floor(value * 1e21))
}

export const BearStaking: Story = {
  args: {
    side: 'BEAR',
    tokenBalance: 5000,
    stakedBalance: 1000,
  },
  render: (args) => (
    <div className="max-w-md">
      <StakingCard
        side={args.side}
        tokenBalance={toTokenBigint(args.tokenBalance)}
        stakedBalance={toShareBigint(args.stakedBalance)}
      />
    </div>
  ),
}

export const BullStaking: Story = {
  args: {
    side: 'BULL',
    tokenBalance: 7500,
    stakedBalance: 2000,
  },
  render: (args) => (
    <div className="max-w-md">
      <StakingCard
        side={args.side}
        tokenBalance={toTokenBigint(args.tokenBalance)}
        stakedBalance={toShareBigint(args.stakedBalance)}
      />
    </div>
  ),
}

export const BothCards: Story = {
  render: () => (
    <div className="grid grid-cols-2 gap-6 max-w-4xl">
      <StakingCard side="BULL" tokenBalance={toTokenBigint(7500)} stakedBalance={toShareBigint(2000)} />
      <StakingCard side="BEAR" tokenBalance={toTokenBigint(5000)} stakedBalance={toShareBigint(1000)} />
    </div>
  ),
}

export const StakeBearFlow: Story = {
  args: {
    side: 'BEAR',
    tokenBalance: 5000,
    stakedBalance: 1000,
  },
  render: (args) => (
    <div className="max-w-md">
      <StakingCard
        side={args.side}
        tokenBalance={toTokenBigint(args.tokenBalance)}
        stakedBalance={toShareBigint(args.stakedBalance)}
      />
    </div>
  ),
  play: async ({ canvasElement, step }) => {
    const canvas = within(canvasElement)

    await step('Verify Stake mode is selected by default', async () => {
      const stakeButton = canvas.getByRole('button', { name: /^stake$/i })
      expect(stakeButton).toHaveClass('bg-brand-orange')
    })

    await step('Enter stake amount', async () => {
      const input = canvas.getByPlaceholderText('0.00')
      await userEvent.clear(input)
      await userEvent.type(input, '100')
      expect(input).toHaveValue('100')
    })

    await step('Verify output display shows expected shares', async () => {
      const output = canvas.getByText('You will receive').parentElement!
      await expect(within(output).getByText('80 splDXY-BEAR')).toBeVisible()
      await userEvent.clear(canvas.getByLabelText('plDXY-BEAR to stake'))
      await userEvent.type(canvas.getByLabelText('plDXY-BEAR to stake'), '250')
      await expect(within(output).getByText('200 splDXY-BEAR')).toBeVisible()
    })
  },
}

export const UnstakeBullFlow: Story = {
  args: {
    side: 'BULL',
    tokenBalance: 7500,
    stakedBalance: 2000,
  },
  render: (args) => (
    <div className="max-w-md">
      <StakingCard
        side={args.side}
        tokenBalance={toTokenBigint(args.tokenBalance)}
        stakedBalance={toShareBigint(args.stakedBalance)}
      />
    </div>
  ),
  play: async ({ canvasElement, step }) => {
    const canvas = within(canvasElement)

    await step('Switch to Unstake mode', async () => {
      const unstakeButton = canvas.getByRole('button', { name: /^unstake$/i })
      await userEvent.click(unstakeButton)
      expect(unstakeButton).toHaveClass('bg-positive')
    })

    await step('Enter unstake amount', async () => {
      const input = canvas.getByPlaceholderText('0.00')
      await userEvent.clear(input)
      await userEvent.type(input, '50')
      expect(input).toHaveValue('50')
      const output = canvas.getByText('You will receive').parentElement!
      await expect(within(output).getByText('62.5 plDXY-BULL')).toBeVisible()
    })
  },
}

export const ModeToggle: Story = {
  args: {
    side: 'BEAR',
    tokenBalance: 5000,
    stakedBalance: 1000,
  },
  render: (args) => (
    <div className="max-w-md">
      <StakingCard
        side={args.side}
        tokenBalance={toTokenBigint(args.tokenBalance)}
        stakedBalance={toShareBigint(args.stakedBalance)}
      />
    </div>
  ),
  play: async ({ canvasElement, step }) => {
    const canvas = within(canvasElement)

    await step('Start in Stake mode', async () => {
      const stakeButton = canvas.getByRole('button', { name: /^stake$/i })
      expect(stakeButton).toHaveClass('bg-brand-orange')
    })

    await step('Switch to Unstake mode', async () => {
      const unstakeButton = canvas.getByRole('button', { name: /^unstake$/i })
      await userEvent.click(unstakeButton)
      expect(unstakeButton).toHaveClass('bg-brand-orange')
    })

    await step('Switch back to Stake mode', async () => {
      const stakeButton = canvas.getByRole('button', { name: /^stake$/i })
      await userEvent.click(stakeButton)
      expect(stakeButton).toHaveClass('bg-brand-orange')
    })
  },
}
