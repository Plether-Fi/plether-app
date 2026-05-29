import type { Meta, StoryObj } from '@storybook/react-vite'
import { PerpsAccountPanel } from '../components/PerpsAccountPanel'

const meta: Meta<typeof PerpsAccountPanel> = {
  title: 'Perps/Account Panel',
  component: PerpsAccountPanel,
  tags: ['autodocs'],
  parameters: {
    layout: 'fullscreen',
  },
}

export default meta
type Story = StoryObj<typeof meta>

export const Default: Story = {
  render: () => (
    <div className="min-h-screen bg-cyber-bg p-4 md:p-8">
      <div className="mx-auto max-w-5xl">
        <PerpsAccountPanel />
      </div>
    </div>
  ),
}
