import type { Meta, StoryObj } from '@storybook/react-vite'
import { TransactionRow } from '../components/TransactionRow'
import type { HistoricalTransaction, TransactionType, TokenSymbol } from '../types'

const transactionTypes: TransactionType[] = [
  'mint', 'burn',
  'swap_buy_bear', 'swap_sell_bear',
  'swap_buy_bull', 'swap_sell_bull',
  'stake_bear', 'stake_bull',
  'unstake_bear', 'unstake_bull',
  'leverage_open', 'leverage_close', 'leverage_adjust',
  'morpho_supply', 'morpho_withdraw', 'morpho_borrow', 'morpho_repay',
]

interface TransactionRowArgs {
  type: TransactionType
  amount: number
  tokenSymbol: TokenSymbol
}

const meta: Meta<TransactionRowArgs> = {
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
      options: ['USDC', 'plDXY-BEAR', 'plDXY-BULL', 'splDXY-BEAR', 'splDXY-BULL'],
      description: 'Token symbol',
    },
  },
}

export default meta
type Story = StoryObj<TransactionRowArgs>

function argsToTransaction(args: TransactionRowArgs, id: string = '1'): HistoricalTransaction {
  const decimals = args.tokenSymbol === 'USDC' ? 6 : 18
  return {
    id,
    hash: '0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef',
    type: args.type,
    timestamp: Date.now() / 1000,
    amount: BigInt(Math.floor(args.amount * 10 ** decimals)),
    tokenSymbol: args.tokenSymbol,
    status: 'success',
  }
}

export const BuyBear: Story = {
  args: {
    type: 'swap_buy_bear',
    amount: 1000,
    tokenSymbol: 'plDXY-BEAR',
  },
  render: (args) => (
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30">
      <TransactionRow transaction={argsToTransaction(args)} />
    </div>
  ),
}

export const BuyBull: Story = {
  args: {
    type: 'swap_buy_bull',
    amount: 500,
    tokenSymbol: 'plDXY-BULL',
  },
  render: (args) => (
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30">
      <TransactionRow transaction={argsToTransaction(args)} />
    </div>
  ),
}

export const StakeBear: Story = {
  args: {
    type: 'stake_bear',
    amount: 250,
    tokenSymbol: 'plDXY-BEAR',
  },
  render: (args) => (
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30">
      <TransactionRow transaction={argsToTransaction(args)} />
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
      <TransactionRow transaction={argsToTransaction(args)} />
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
      <TransactionRow transaction={argsToTransaction(args)} />
    </div>
  ),
}

export const TransactionList: Story = {
  render: () => (
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30 divide-y divide-cyber-border-glow/30">
      <TransactionRow transaction={argsToTransaction({ type: 'swap_buy_bear', amount: 1000, tokenSymbol: 'plDXY-BEAR' }, '1')} />
      <TransactionRow transaction={argsToTransaction({ type: 'swap_buy_bull', amount: 500, tokenSymbol: 'plDXY-BULL' }, '2')} />
      <TransactionRow transaction={argsToTransaction({ type: 'stake_bear', amount: 250, tokenSymbol: 'plDXY-BEAR' }, '3')} />
      <TransactionRow transaction={argsToTransaction({ type: 'leverage_open', amount: 5000, tokenSymbol: 'USDC' }, '4')} />
      <TransactionRow transaction={argsToTransaction({ type: 'mint', amount: 10000, tokenSymbol: 'USDC' }, '5')} />
    </div>
  ),
}
