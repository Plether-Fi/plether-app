import type { Meta, StoryObj } from '@storybook/react-vite'
import { VaultAssetChart } from '../components/VaultAssetChart'
import { createVaultAssetHistory } from './fixtures/vaultAssetHistory'

const history = createVaultAssetHistory()

const meta = {
  title: 'Vaults/Asset Chart',
  component: VaultAssetChart,
  tags: ['autodocs'],
  parameters: {
    layout: 'fullscreen',
    docs: {
      description: {
        component: 'Seven days of total assets (TVL) and value unavailable for withdrawal, using illustrative hourly data. Hover, tap, or use the arrow keys to inspect both values.',
      },
    },
  },
  decorators: [
    (Story) => (
      <div className="min-h-screen bg-app-bg p-4 sm:p-8">
        <div className="mx-auto max-w-6xl">
          <p className="text-xs font-semibold uppercase tracking-[0.18em] text-content-secondary">Vault activity</p>
          <h2 className="mb-4 mt-1 text-2xl font-semibold text-content-primary">Holders and recent activity</h2>
          <Story />
        </div>
      </div>
    ),
  ],
  args: { tranche: 'junior', history },
  argTypes: {
    tranche: { control: 'inline-radio', options: ['senior', 'junior'] },
    history: { control: false },
  },
} satisfies Meta<typeof VaultAssetChart>

export default meta
type Story = StoryObj<typeof meta>

export const Junior: Story = {}
export const Senior: Story = { args: { tranche: 'senior' } }

export const PartialHistory: Story = {
  args: {
    history: {
      ...history,
      coverage: { ...history.coverage, complete: false },
      junior: {
        ...history.junior,
        points: history.junior.points.slice(56).map((point, index) => ({
          ...point,
          markFresh: index < 42 || index > 56,
          lockedAssets: index >= 42 && index <= 56 ? null : point.lockedAssets,
        })),
      },
    },
  },
}

export const Loading: Story = { args: { history: undefined, isLoading: true } }
export const Unavailable: Story = { args: { history: undefined, isError: true } }
