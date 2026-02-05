import type { Meta, StoryObj } from '@storybook/react-vite'
import { TokenIcon } from '../components/ui/TokenIcon'

const meta: Meta<typeof TokenIcon> = {
  title: 'UI/TokenIcon',
  component: TokenIcon,
  tags: ['autodocs'],
  argTypes: {
    side: {
      control: 'select',
      options: ['BEAR', 'BULL'],
    },
    size: {
      control: 'select',
      options: ['sm', 'md', 'lg'],
    },
  },
}

export default meta
type Story = StoryObj<typeof meta>

export const Bear: Story = {
  args: {
    side: 'BEAR',
  },
}

export const Bull: Story = {
  args: {
    side: 'BULL',
  },
}

export const Small: Story = {
  args: {
    side: 'BEAR',
    size: 'sm',
  },
}

export const Large: Story = {
  args: {
    side: 'BULL',
    size: 'lg',
  },
}
