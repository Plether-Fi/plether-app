import type { Meta, StoryObj } from '@storybook/react-vite'
import { Card, CardHeader } from '../components/ui'
import { Button } from '../components/ui/Button'

const meta: Meta<typeof Card> = {
  title: 'UI/Card',
  component: Card,
  tags: ['autodocs'],
  argTypes: {
    padding: {
      control: 'select',
      options: ['none', 'sm', 'md', 'lg'],
    },
  },
}

export default meta
type Story = StoryObj<typeof meta>

export const Default: Story = {
  args: {
    children: 'This is a card with default padding.',
  },
}

export const SmallPadding: Story = {
  args: {
    padding: 'sm',
    children: 'This is a card with small padding.',
  },
}

export const LargePadding: Story = {
  args: {
    padding: 'lg',
    children: 'This is a card with large padding.',
  },
}

export const NoPadding: Story = {
  args: {
    padding: 'none',
    children: (
      <div className="p-4">
        Content with custom padding inside a no-padding card.
      </div>
    ),
  },
}

export const WithHeader: Story = {
  args: {
    padding: 'lg',
    children: (
      <>
        <CardHeader
          title="Card Title"
          subtitle="This is a subtitle"
          action={<Button size="sm" variant="secondary">Action</Button>}
        />
        <p className="text-cyber-text-secondary">
          This is the card content with a header component.
        </p>
      </>
    ),
  },
}

export const WithHeaderNoAction: Story = {
  args: {
    padding: 'lg',
    children: (
      <>
        <CardHeader
          title="Simple Header"
          subtitle="No action button"
        />
        <p className="text-cyber-text-secondary">
          Card content goes here.
        </p>
      </>
    ),
  },
}
