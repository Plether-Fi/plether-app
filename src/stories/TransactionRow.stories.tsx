import type { Meta, StoryObj } from '@storybook/react-vite'

type TransactionType =
  | 'mint' | 'burn'
  | 'swap_buy_bear' | 'swap_sell_bear'
  | 'swap_buy_bull' | 'swap_sell_bull'
  | 'stake_bear' | 'stake_bull'
  | 'unstake_bear' | 'unstake_bull'
  | 'leverage_open' | 'leverage_close' | 'leverage_adjust'
  | 'morpho_supply' | 'morpho_withdraw' | 'morpho_borrow' | 'morpho_repay'

const transactionTypes: TransactionType[] = [
  'mint', 'burn',
  'swap_buy_bear', 'swap_sell_bear',
  'swap_buy_bull', 'swap_sell_bull',
  'stake_bear', 'stake_bull',
  'unstake_bear', 'unstake_bull',
  'leverage_open', 'leverage_close', 'leverage_adjust',
  'morpho_supply', 'morpho_withdraw', 'morpho_borrow', 'morpho_repay',
]

interface TransactionRowProps {
  type: TransactionType
  amount: number
  tokenSymbol: string
  hash?: string
  timestamp?: Date
}

const meta: Meta<TransactionRowProps> = {
  title: 'Components/TransactionRow',
  tags: ['autodocs'],
  argTypes: {
    type: {
      control: 'select',
      options: transactionTypes,
      description: 'Transaction type',
    },
    amount: {
      control: { type: 'number', min: 0 },
      description: 'Transaction amount',
    },
    tokenSymbol: {
      control: 'select',
      options: ['USDC', 'DXY-BEAR', 'DXY-BULL'],
      description: 'Token symbol',
    },
  },
}

export default meta
type Story = StoryObj<TransactionRowProps>

const typeLabels: Record<TransactionType, string> = {
  mint: 'Mint Pairs',
  burn: 'Burn Pairs',
  swap_buy_bear: 'Buy DXY-BEAR',
  swap_sell_bear: 'Sell DXY-BEAR',
  swap_buy_bull: 'Buy DXY-BULL',
  swap_sell_bull: 'Sell DXY-BULL',
  stake_bear: 'Stake DXY-BEAR',
  stake_bull: 'Stake DXY-BULL',
  unstake_bear: 'Unstake DXY-BEAR',
  unstake_bull: 'Unstake DXY-BULL',
  leverage_open: 'Open Leverage',
  leverage_close: 'Close Leverage',
  leverage_adjust: 'Adjust Leverage',
  morpho_supply: 'Supply to Morpho',
  morpho_withdraw: 'Withdraw from Morpho',
  morpho_borrow: 'Borrow from Morpho',
  morpho_repay: 'Repay Morpho',
}

const typeIcons: Record<TransactionType, string> = {
  mint: 'add_circle',
  burn: 'remove_circle',
  swap_buy_bear: 'shopping_cart',
  swap_sell_bear: 'sell',
  swap_buy_bull: 'shopping_cart',
  swap_sell_bull: 'sell',
  stake_bear: 'lock',
  stake_bull: 'lock',
  unstake_bear: 'lock_open',
  unstake_bull: 'lock_open',
  leverage_open: 'trending_up',
  leverage_close: 'trending_down',
  leverage_adjust: 'tune',
  morpho_supply: 'savings',
  morpho_withdraw: 'output',
  morpho_borrow: 'request_quote',
  morpho_repay: 'paid',
}

function getTypeColor(type: TransactionType): string {
  if (type.includes('bear')) return 'text-cyber-electric-fuchsia'
  if (type.includes('bull')) return 'text-cyber-neon-green'
  if (type === 'mint') return 'text-cyber-neon-green'
  if (type === 'burn') return 'text-cyber-warning-text'
  if (type.includes('leverage')) return 'text-cyber-bright-blue'
  if (type.includes('morpho')) return 'text-cyber-electric-fuchsia'
  return 'text-cyber-text-primary'
}

function getIconBg(type: TransactionType): string {
  if (type.includes('bear')) return 'bg-cyber-electric-fuchsia/20'
  if (type.includes('bull')) return 'bg-cyber-neon-green/20'
  if (type === 'mint') return 'bg-cyber-neon-green/20'
  if (type === 'burn') return 'bg-cyber-warning-bg'
  if (type.includes('leverage')) return 'bg-cyber-bright-blue/20'
  if (type.includes('morpho')) return 'bg-cyber-electric-fuchsia/20'
  return 'bg-cyber-surface-light'
}

function TransactionRow({ type, amount, tokenSymbol, hash = '0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef', timestamp = new Date() }: TransactionRowProps) {
  const truncatedHash = `${hash.slice(0, 10)}...${hash.slice(-8)}`
  const decimals = tokenSymbol === 'USDC' ? 2 : 4

  return (
    <div className="flex items-center justify-between px-6 py-4 hover:bg-cyber-surface-light/50 transition-colors">
      <div className="flex items-center gap-4">
        <div className={`w-10 h-10 ${getIconBg(type)} flex items-center justify-center`}>
          <span className={`material-symbols-outlined ${getTypeColor(type)}`}>
            {typeIcons[type]}
          </span>
        </div>
        <div>
          <p className={`font-semibold ${getTypeColor(type)}`}>
            {typeLabels[type]}
          </p>
          <p className="text-xs text-cyber-text-secondary">
            {timestamp.toLocaleDateString()} {timestamp.toLocaleTimeString()}
          </p>
        </div>
      </div>
      <div className="text-right">
        <p className="font-medium text-cyber-text-primary">
          {amount.toFixed(decimals)} {tokenSymbol}
        </p>
        <a
          href={`https://etherscan.io/tx/${hash}`}
          target="_blank"
          rel="noopener noreferrer"
          className="text-xs text-cyber-bright-blue hover:underline"
        >
          {truncatedHash}
        </a>
      </div>
    </div>
  )
}

export const BuyBear: Story = {
  args: {
    type: 'swap_buy_bear',
    amount: 1000,
    tokenSymbol: 'DXY-BEAR',
  },
  render: (args) => (
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30">
      <TransactionRow {...args} />
    </div>
  ),
}

export const BuyBull: Story = {
  args: {
    type: 'swap_buy_bull',
    amount: 500,
    tokenSymbol: 'DXY-BULL',
  },
  render: (args) => (
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30">
      <TransactionRow {...args} />
    </div>
  ),
}

export const StakeBear: Story = {
  args: {
    type: 'stake_bear',
    amount: 250,
    tokenSymbol: 'DXY-BEAR',
  },
  render: (args) => (
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30">
      <TransactionRow {...args} />
    </div>
  ),
}

export const LeverageOpen: Story = {
  args: {
    type: 'leverage_open',
    amount: 5000,
    tokenSymbol: 'USDC',
  },
  render: (args) => (
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30">
      <TransactionRow {...args} />
    </div>
  ),
}

export const Mint: Story = {
  args: {
    type: 'mint',
    amount: 10000,
    tokenSymbol: 'USDC',
  },
  render: (args) => (
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30">
      <TransactionRow {...args} />
    </div>
  ),
}

export const TransactionList: Story = {
  render: () => (
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30 divide-y divide-cyber-border-glow/30">
      <TransactionRow type="swap_buy_bear" amount={1000} tokenSymbol="DXY-BEAR" />
      <TransactionRow type="swap_buy_bull" amount={500} tokenSymbol="DXY-BULL" />
      <TransactionRow type="stake_bear" amount={250} tokenSymbol="DXY-BEAR" />
      <TransactionRow type="leverage_open" amount={5000} tokenSymbol="USDC" />
      <TransactionRow type="mint" amount={10000} tokenSymbol="USDC" />
    </div>
  ),
}
