import { useState } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { Modal } from '../components/ui/Modal'
import { Button } from '../components/ui/Button'

const meta: Meta<typeof Modal> = {
  title: 'UI/Modal',
  component: Modal,
  tags: ['autodocs'],
  argTypes: {
    size: {
      control: 'select',
      options: ['sm', 'md', 'lg'],
    },
    isOpen: { control: 'boolean' },
  },
  decorators: [
    (Story) => (
      <div style={{ minHeight: '400px' }}>
        <Story />
      </div>
    ),
  ],
}

export default meta
type Story = StoryObj<typeof meta>

function ModalWithTrigger({ size, title }: { size?: 'sm' | 'md' | 'lg'; title?: string }) {
  const [isOpen, setIsOpen] = useState(false)
  return (
    <>
      <Button onClick={() => setIsOpen(true)}>Open Modal</Button>
      <Modal
        isOpen={isOpen}
        onClose={() => setIsOpen(false)}
        title={title}
        size={size}
      >
        <p className="text-cyber-text-secondary mb-4">
          This is the modal content. Press Escape or click outside to close.
        </p>
        <div className="flex justify-end gap-2">
          <Button variant="secondary" onClick={() => setIsOpen(false)}>
            Cancel
          </Button>
          <Button onClick={() => setIsOpen(false)}>
            Confirm
          </Button>
        </div>
      </Modal>
    </>
  )
}

export const Default: Story = {
  render: () => <ModalWithTrigger title="Modal Title" />,
}

export const Small: Story = {
  render: () => <ModalWithTrigger size="sm" title="Small Modal" />,
}

export const Large: Story = {
  render: () => <ModalWithTrigger size="lg" title="Large Modal" />,
}

function NoTitleModal() {
  const [isOpen, setIsOpen] = useState(false)
  return (
    <>
      <Button onClick={() => setIsOpen(true)}>Open Modal (No Title)</Button>
      <Modal isOpen={isOpen} onClose={() => setIsOpen(false)}>
        <p className="text-cyber-text-secondary mb-4">
          This modal has no title bar.
        </p>
        <Button onClick={() => setIsOpen(false)}>Close</Button>
      </Modal>
    </>
  )
}

function FormModal() {
  const [isOpen, setIsOpen] = useState(false)
  return (
    <>
      <Button onClick={() => setIsOpen(true)}>Open Form Modal</Button>
      <Modal isOpen={isOpen} onClose={() => setIsOpen(false)} title="Enter Details">
        <div className="space-y-4">
          <div>
            <label className="block text-sm text-cyber-text-secondary mb-1">Amount</label>
            <input
              type="text"
              placeholder="0.00"
              className="w-full px-4 py-2 bg-cyber-surface-light border border-cyber-border-glow/30 text-cyber-text-primary"
            />
          </div>
          <div className="flex justify-end gap-2">
            <Button variant="secondary" onClick={() => setIsOpen(false)}>
              Cancel
            </Button>
            <Button onClick={() => setIsOpen(false)}>
              Submit
            </Button>
          </div>
        </div>
      </Modal>
    </>
  )
}

export const NoTitle: Story = {
  render: () => <NoTitleModal />,
}

export const WithForm: Story = {
  render: () => <FormModal />,
}
