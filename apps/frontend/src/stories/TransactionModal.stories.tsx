import type { Meta, StoryObj } from '@storybook/react-vite'
import { expect, fn, userEvent, waitFor, within } from 'storybook/test'
import { useEffect } from 'react'
import { WagmiProvider } from 'wagmi'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { config } from '../config/wagmi'
import { LoadingScreen, type LoadingStep } from '../components/ui/LoadingScreen'
import { TransactionModal } from '../components/TransactionModal'
import { useTransactionModal } from '../hooks/useTransactionModal'
import { useTransactionStore } from '../stores/transactionStore'

const queryClient = new QueryClient()

const meta: Meta<typeof LoadingScreen> = {
  title: 'Components/TransactionModal',
  component: LoadingScreen,
  decorators: [
    (Story) => (
      <div className="bg-surface-panel border border-brand-border/50 max-w-md mx-auto">
        <Story />
      </div>
    ),
  ],
}

export default meta
type Story = StoryObj<typeof meta>

const buySteps: LoadingStep[] = [
  { label: 'Approve USDC', status: 'completed' },
  { label: 'Confirming approval', status: 'completed' },
  { label: 'Buy plDXY-BEAR', status: 'in_progress' },
  { label: 'Awaiting confirmation', status: 'pending' },
]

export const InProgress: Story = {
  render: () => (
    <LoadingScreen
      title="Buying plDXY-BEAR"
      steps={buySteps}
    />
  ),
}

export const SwapInProgress: Story = {
  render: () => (
    <LoadingScreen
      title="Selling plDXY-BULL"
      steps={[
        { label: 'Sell plDXY-BULL', status: 'in_progress' },
        { label: 'Awaiting confirmation', status: 'pending' },
      ]}
    />
  ),
}

export const Success: Story = {
  render: () => (
    <LoadingScreen
      title="Transaction complete!"
      steps={[
        { label: 'Approve USDC', status: 'completed' },
        { label: 'Confirming approval', status: 'completed' },
        { label: 'Buy plDXY-BEAR', status: 'completed' },
        { label: 'Awaiting confirmation', status: 'completed' },
      ]}
      transactionUrl="https://sepolia.etherscan.io/tx/0x1234567890abcdef"
      onClose={() => {}}
    />
  ),
}

export const Error: Story = {
  render: () => (
    <LoadingScreen
      title="Transaction failed"
      steps={[
        { label: 'Approve USDC', status: 'completed' },
        { label: 'Confirming approval', status: 'completed' },
        { label: 'Buy plDXY-BEAR', status: 'error' },
        { label: 'Awaiting confirmation', status: 'pending' },
      ]}
      errorMessage="Transaction reverted: insufficient liquidity in the pool."
      onClose={() => {}}
      onRetry={() => {}}
    />
  ),
}

export const StakingFlow: Story = {
  render: () => (
    <LoadingScreen
      title="Staking plDXY-BEAR"
      steps={[
        { label: 'Sign permit', status: 'completed' },
        { label: 'Stake plDXY-BEAR', status: 'in_progress' },
        { label: 'Awaiting confirmation', status: 'pending' },
      ]}
    />
  ),
}

export const MintFlow: Story = {
  render: () => (
    <LoadingScreen
      title="Minting token pairs"
      steps={[
        { label: 'Approve USDC', status: 'completed' },
        { label: 'Confirming approval', status: 'completed' },
        { label: 'Mint pairs', status: 'in_progress' },
        { label: 'Awaiting confirmation', status: 'pending' },
      ]}
    />
  ),
}

export const RedeemFlow: Story = {
  render: () => (
    <LoadingScreen
      title="Redeeming token pairs"
      steps={[
        { label: 'Approve plDXY-BEAR', status: 'completed' },
        { label: 'Confirming approval', status: 'completed' },
        { label: 'Approve plDXY-BULL', status: 'in_progress' },
        { label: 'Confirming approval', status: 'pending' },
        { label: 'Redeem pairs', status: 'pending' },
        { label: 'Awaiting confirmation', status: 'pending' },
      ]}
    />
  ),
}

function AnimatedDemo() {
  const modal = useTransactionModal()

  useEffect(() => {
    modal.reset()
    useTransactionStore.getState().clearTransactions()
    return () => {
      modal.reset()
      useTransactionStore.getState().clearTransactions()
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  return <TransactionModal />
}

const animatedSteps = ['Approve USDC', 'Confirming approval', 'Buy plDXY-BEAR', 'Awaiting confirmation'] as const

async function expectStep(dialog: HTMLElement, index: number, status: 'pending' | 'in_progress' | 'completed' | 'error') {
  await waitFor(() => {
    const row = within(dialog).getByText(`${String(index + 1)}. ${animatedSteps[index]}`).parentElement!
    const contents = within(row)
    if (status === 'in_progress') expect(row.querySelector('.animate-spin')).toBeVisible()
    else expect(row.querySelector('.animate-spin')).not.toBeInTheDocument()
    if (status === 'completed') expect(contents.getByText('check')).toBeVisible()
    else expect(contents.queryByText('check')).not.toBeInTheDocument()
    if (status === 'error') expect(contents.getByText('close')).toBeVisible()
    else expect(contents.queryByText('close')).not.toBeInTheDocument()
  })
}

async function openAnimatedTransaction(ownerDocument: Document, onRetry?: () => void) {
  const transactionId = crypto.randomUUID()
  useTransactionStore.getState().addTransaction({
    id: transactionId, type: 'swap', status: 'pending', title: 'Buying plDXY-BEAR', chainId: 1,
    steps: animatedSteps.map(label => ({ label, status: 'pending' })),
  })
  useTransactionModal.getState().open({ transactionId, onRetry })
  const dialog = await within(ownerDocument.body).findByRole('dialog', { name: 'Buying plDXY-BEAR' })
  await expect(dialog).toBeVisible()
  await expectStep(dialog, 0, 'pending')
  await expectStep(dialog, 3, 'pending')
  expect(within(dialog).queryByRole('link', { name: /Show transaction/ })).not.toBeInTheDocument()
  expect(within(dialog).queryByRole('alert')).not.toBeInTheDocument()
  return { transactionId, dialog }
}

const interactiveDecorators = [
  (Story: React.ComponentType) => (
    <WagmiProvider config={config}>
      <QueryClientProvider client={queryClient}>
        <Story />
      </QueryClientProvider>
    </WagmiProvider>
  ),
]

export const AnimatedSuccess: Story = {
  decorators: interactiveDecorators,
  render: () => <AnimatedDemo />,
  play: async ({ canvasElement, step }) => {
    const page = within(canvasElement.ownerDocument.body)
    const txStore = useTransactionStore.getState()
    const transactionHash = `0x${'ab'.repeat(32)}`
    let animation!: Awaited<ReturnType<typeof openAnimatedTransaction>>

    await step('Open the pending transaction dialog', async () => {
      animation = await openAnimatedTransaction(canvasElement.ownerDocument)
    })
    const { transactionId, dialog } = animation

    for (const [index, label] of animatedSteps.entries()) {
      await step(label, async () => {
        txStore.setStepInProgress(transactionId, index)
        await expectStep(dialog, index, 'in_progress')
        if (index > 0) await expectStep(dialog, index - 1, 'completed')
        expect(useTransactionStore.getState().transactions.find(tx => tx.id === transactionId)?.status).toBe('confirming')
      })
    }

    await step('Show completed steps and the confirmed transaction link', async () => {
      txStore.setStepSuccess(transactionId, transactionHash)
      for (let index = 0; index < animatedSteps.length; index += 1) await expectStep(dialog, index, 'completed')
      expect(useTransactionStore.getState().transactions.find(tx => tx.id === transactionId)?.status).toBe('success')
      const link = within(dialog).getByRole('link', { name: /Show transaction/ })
      await expect(link).toBeVisible()
      expect(link).toHaveAttribute('href', `https://etherscan.io/tx/${transactionHash}`)
      expect(within(dialog).queryByRole('alert')).not.toBeInTheDocument()
      await userEvent.click(within(dialog).getByRole('button', { name: 'Close transaction' }))
      await waitFor(() => expect(page.queryByRole('dialog')).not.toBeInTheDocument())
    })
  },
}

export const AnimatedError: Story = {
  decorators: interactiveDecorators,
  render: () => <AnimatedDemo />,
  play: async ({ canvasElement, step }) => {
    const page = within(canvasElement.ownerDocument.body)
    const txStore = useTransactionStore.getState()
    const onRetry = fn()
    let animation!: Awaited<ReturnType<typeof openAnimatedTransaction>>

    await step('Open the transaction dialog', async () => {
      animation = await openAnimatedTransaction(canvasElement.ownerDocument, onRetry)
      expect(within(animation.dialog).queryByRole('button', { name: 'Try again' })).not.toBeInTheDocument()
    })
    const { transactionId, dialog } = animation

    await step('Approve USDC', async () => {
      txStore.setStepInProgress(transactionId, 0)
      await expectStep(dialog, 0, 'in_progress')
    })

    await step('Confirming approval', async () => {
      txStore.setStepInProgress(transactionId, 1)
      await expectStep(dialog, 0, 'completed')
      await expectStep(dialog, 1, 'in_progress')
    })

    await step('Show the rejected step and retry the failed transaction', async () => {
      txStore.setStepError(transactionId, 1, 'User rejected the transaction')
      await expectStep(dialog, 0, 'completed')
      await expectStep(dialog, 1, 'error')
      await expectStep(dialog, 2, 'pending')
      await expect(within(dialog).getByRole('alert')).toHaveTextContent(/declined.*wallet/i)
      expect(useTransactionStore.getState().transactions.find(tx => tx.id === transactionId)?.status).toBe('failed')
      expect(within(dialog).queryByRole('link', { name: /Show transaction/ })).not.toBeInTheDocument()
      await userEvent.click(within(dialog).getByRole('button', { name: 'Try again' }))
      await expect(onRetry).toHaveBeenCalledOnce()
      await waitFor(() => expect(page.queryByRole('dialog')).not.toBeInTheDocument())
    })
  },
}
