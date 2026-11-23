import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { decodeFunctionData, parseAbi, parseEther, type Address, type Hex } from 'viem'
import { arbitrumSepolia } from 'viem/chains'
import { KernelEthFunding } from '../KernelEthFunding'
import { writeGasPaymentPreference } from '../../perps-aa/gasPaymentPreference'
import { parseKernelEthAmount } from '../../perps-aa/kernelEthFunding'
import { KernelObservedPendingError } from '../../perps-aa/errors'
import type { KernelPerpsAaDeploymentManifest } from '../../perps-aa/manifest'
import type { KernelPerpsAaSmartAccountRuntime } from '../../perps-aa/runtimeContext'

const mocks = vi.hoisted(() => ({
  verifyAccount: vi.fn(), getBalance: vi.fn(), readContract: vi.fn(), getAddresses: vi.fn(), getChainId: vi.fn(),
  estimateFeesPerGas: vi.fn(), sendTransaction: vi.fn(), waitForTransactionReceipt: vi.fn(), getTransaction: vi.fn(), getBlock: vi.fn(),
  quoteAction: vi.fn(), execute: vi.fn(),
  walletChain: vi.fn(), switchChain: vi.fn(),
}))

vi.mock('wagmi', async importOriginal => ({
  ...await importOriginal<typeof import('wagmi')>(),
  usePublicClient: () => ({ ...mocks, chain: arbitrumSepolia }),
  useWalletClient: () => ({ data: mocks }),
  useChainId: () => mocks.walletChain(),
  useSwitchChain: () => ({ switchChainAsync: mocks.switchChain, isPending: false }),
}))
vi.mock('../../hooks/usePerpsGasReview', () => ({ kernelQuoteIsFresh: () => true }))

const owner = '0x1111111111111111111111111111111111111111' satisfies Address
const sender = '0x2222222222222222222222222222222222222222' satisfies Address
const entryPoint = '0x433709009b8330fda32311df1c2afa402ed8d009' satisfies Address
const scope = { sender, ownerAddress: owner, chainId: arbitrumSepolia.id, deployment: 'kernel-test' }
const hash = `0x${'12'.repeat(32)}` satisfies Hex
const blockHash = `0x${'34'.repeat(32)}` satisfies Hex
const verifiedAccount = { address: sender, ownerAddress: owner, deployed: false, blockNumber: 1n, blockHash }
const receipt = { transactionHash: hash, blockHash, blockNumber: 2n, status: 'success' }
const transfer = { hash, blockHash, blockNumber: 2n, from: owner, to: sender, value: parseEther('0.01'), input: '0x' }
const networkFees = { maxFeePerGas: 24_000_000n, maxPriorityFeePerGas: 0n }
const runtime = {
  chainId: scope.chainId, ownerAddress: owner,
  smartAccount: { accountAddress: sender, entryPoint },
  gasPayment: { scope, readiness: { eth: true, sponsored: false } },
  kernel: { scope: { ...scope, entryPoint }, verifyAccount: mocks.verifyAccount, quoteAction: mocks.quoteAction, execute: mocks.execute },
} as unknown as KernelPerpsAaSmartAccountRuntime
const manifest = { version: 'perps-aa-kernel-v1:kernel-test' } as KernelPerpsAaDeploymentManifest

function renderPanel() {
  const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } })
  return { ...render(<QueryClientProvider client={queryClient}><KernelEthFunding runtime={runtime} manifest={manifest} /></QueryClientProvider>), queryClient }
}

describe('Kernel ETH funding', () => {
  beforeEach(() => {
    vi.resetAllMocks()
    localStorage.clear()
    mocks.walletChain.mockReturnValue(arbitrumSepolia.id)
    mocks.switchChain.mockResolvedValue(arbitrumSepolia)
    mocks.verifyAccount.mockResolvedValue(verifiedAccount)
    mocks.getBalance.mockResolvedValue(parseEther('0.02'))
    mocks.readContract.mockImplementation(({ functionName }: { functionName: string }) => Promise.resolve(
      functionName === 'balanceOf' ? parseEther('0.03') : { settlementBalanceAssets: 140_000_000n },
    ))
    mocks.getAddresses.mockResolvedValue([owner])
    mocks.getChainId.mockResolvedValue(arbitrumSepolia.id)
    mocks.estimateFeesPerGas.mockResolvedValue(networkFees)
    mocks.sendTransaction.mockResolvedValue(hash)
    mocks.waitForTransactionReceipt.mockResolvedValue(receipt)
    mocks.getTransaction.mockResolvedValue(transfer)
    mocks.getBlock.mockImplementation(async ({ blockNumber }) => ({ number: blockNumber, hash: blockHash }))
    mocks.quoteAction.mockResolvedValue({ mode: 'eth', maxNetworkGasWei: parseEther('0.001') })
    mocks.execute.mockResolvedValue({})
  })

  it('recognizes exact positive wei amounts and rejects rounding', () => {
    expect(parseKernelEthAmount('.000000000000000001')).toBe(1n)
    expect(parseKernelEthAmount('0.0000000000000000001')).toBeUndefined()
    expect(parseKernelEthAmount('0')).toBeUndefined()
  })

  it('does not expose a funding address or ask the wallet to transfer when verification fails', async () => {
    mocks.verifyAccount.mockResolvedValue({ ...verifiedAccount, ownerAddress: sender })
    renderPanel()
    expect(await screen.findByText('Trading Account ETH balances are temporarily unavailable.')).toBeVisible()
    expect(screen.queryByText(sender)).not.toBeInTheDocument()
    expect(screen.queryByRole('button', { name: 'Add ETH' })).not.toBeInTheDocument()
    expect(mocks.sendTransaction).not.toHaveBeenCalled()
  })

  it('verifies a counterfactual recipient again, transfers ETH from the owner, and refreshes latest balances', async () => {
    renderPanel()
    expect(await screen.findByText(sender)).toBeVisible()
    expect(screen.getByText('0.02 ETH')).toBeVisible()
    expect(screen.getByText('0.03 ETH')).toBeVisible()
    expect(screen.getByText('140 USDG')).toBeVisible()
    fireEvent.click(screen.getByRole('button', { name: 'Add ETH' }))
    fireEvent.change(screen.getByRole('textbox', { name: 'ETH to add' }), { target: { value: '.01' } })
    fireEvent.click(screen.getByRole('button', { name: 'Send ETH from Owner Wallet' }))
    expect(await screen.findByText(/ETH transfer confirmed/)).toBeVisible()
    expect(mocks.verifyAccount).toHaveBeenCalledTimes(3)
    expect(mocks.estimateFeesPerGas).toHaveBeenCalledWith({
      chain: expect.objectContaining({ id: arbitrumSepolia.id, fees: { baseFeeMultiplier: 2 } }), type: 'eip1559',
    })
    expect(mocks.sendTransaction).toHaveBeenCalledWith({ account: owner, chain: arbitrumSepolia, to: sender, value: parseEther('0.01'), ...networkFees })
    expect(mocks.getBalance).toHaveBeenCalledTimes(2)
    expect(mocks.getBalance).toHaveBeenLastCalledWith({ address: sender, blockNumber: verifiedAccount.blockNumber })
  })

  it('does not display balances when the verified account block changes', async () => {
    mocks.getBlock.mockResolvedValueOnce({ number: verifiedAccount.blockNumber, hash })
    renderPanel()
    expect(await screen.findByText('Trading Account ETH balances are temporarily unavailable.')).toBeVisible()
    expect(mocks.getBlock).toHaveBeenLastCalledWith({ blockNumber: verifiedAccount.blockNumber })
    expect(screen.queryByRole('button', { name: 'Add ETH' })).not.toBeInTheDocument()
  })

  it('never confirms an included transaction with a mismatched payment', async () => {
    mocks.getTransaction.mockResolvedValue({ ...transfer, to: owner })
    renderPanel()
    fireEvent.click(await screen.findByRole('button', { name: 'Add ETH' }))
    fireEvent.change(screen.getByRole('textbox', { name: 'ETH to add' }), { target: { value: '.01' } })
    fireEvent.click(screen.getByRole('button', { name: 'Send ETH from Owner Wallet' }))
    expect(await screen.findByRole('alert')).toHaveTextContent('Check the ETH transfer in the explorer before retrying.')
    expect(screen.queryByText(/ETH transfer confirmed/)).not.toBeInTheDocument()
  })

  it('requests a fresh fee quote after the network rejects a stale cap, without retrying automatically', async () => {
    mocks.sendTransaction.mockRejectedValueOnce(new Error('Transaction execution failed', {
      cause: Object.assign(new Error('max fee per gas less than block base fee'), { name: 'FeeCapTooLowError' }),
    }))
    renderPanel()
    fireEvent.click(await screen.findByRole('button', { name: 'Add ETH' }))
    fireEvent.change(screen.getByRole('textbox', { name: 'ETH to add' }), { target: { value: '.01' } })
    const send = screen.getByRole('button', { name: 'Send ETH from Owner Wallet' })
    fireEvent.click(send)
    expect(await screen.findByRole('alert')).toHaveTextContent('Arbitrum Sepolia rejected the gas fee cap.')
    expect(mocks.sendTransaction).toHaveBeenCalledTimes(1)
    expect(mocks.waitForTransactionReceipt).not.toHaveBeenCalled()

    const increasedFees = { ...networkFees, maxFeePerGas: 30_000_000n }
    mocks.estimateFeesPerGas.mockResolvedValueOnce(increasedFees)
    fireEvent.click(send)
    expect(await screen.findByText(/ETH transfer confirmed/)).toBeVisible()
    expect(mocks.sendTransaction).toHaveBeenLastCalledWith(expect.objectContaining(increasedFees))
  })

  it('keeps a declined top-up available to retry without reporting an unknown transfer', async () => {
    mocks.sendTransaction.mockRejectedValue(new Error('Transaction execution failed', {
      cause: Object.assign(new Error('User rejected the request'), { name: 'UserRejectedRequestError' }),
    }))
    renderPanel()
    fireEvent.click(await screen.findByRole('button', { name: 'Add ETH' }))
    fireEvent.change(screen.getByRole('textbox', { name: 'ETH to add' }), { target: { value: '.01' } })
    fireEvent.click(screen.getByRole('button', { name: 'Send ETH from Owner Wallet' }))
    expect(await screen.findByText('You declined the ETH transfer in your wallet.')).toBeVisible()
    expect(screen.getByRole('textbox', { name: 'ETH to add' })).toHaveValue('.01')
    expect(screen.queryByRole('alert')).not.toBeInTheDocument()
    expect(screen.queryByRole('link', { name: 'View ETH transfer' })).not.toBeInTheDocument()
    expect(mocks.waitForTransactionReceipt).not.toHaveBeenCalled()
  })

  it('offers the Arbitrum Sepolia switch before sending ETH from a different network', async () => {
    mocks.walletChain.mockReturnValue(11155111)
    renderPanel()
    fireEvent.click(await screen.findByRole('button', { name: 'Add ETH' }))
    fireEvent.change(screen.getByRole('textbox', { name: 'ETH to add' }), { target: { value: '.01' } })
    expect(screen.getByRole('button', { name: 'Send ETH from Owner Wallet' })).toBeDisabled()
    fireEvent.click(screen.getByRole('button', { name: 'Switch to Arbitrum Sepolia' }))
    expect(mocks.switchChain).toHaveBeenCalledWith({ chainId: arbitrumSepolia.id })
    await waitFor(() => expect(screen.getByRole('button', { name: 'Switch to Arbitrum Sepolia' })).toBeEnabled())
    expect(mocks.sendTransaction).not.toHaveBeenCalled()
  })

  it('blocks native ETH withdrawal in sponsored mode without changing the preference', async () => {
    renderPanel()
    fireEvent.click(await screen.findByRole('button', { name: 'Withdraw ETH' }))
    fireEvent.change(screen.getByRole('textbox', { name: 'ETH to withdraw' }), { target: { value: '.01' } })
    expect(screen.getByText('Turn off Gas sponsorship in Trading Account activity to withdraw ETH.')).toBeVisible()
    expect(screen.getByRole('button', { name: 'Review withdrawal' })).toBeDisabled()
    expect(mocks.quoteAction).not.toHaveBeenCalled()
    expect(localStorage.length).toBe(0)
  })

  it('reviews exact EntryPoint deposit recovery with ETH during a sponsorship outage', async () => {
    writeGasPaymentPreference(scope, 'eth')
    renderPanel()
    fireEvent.click(await screen.findByRole('button', { name: 'Withdraw EntryPoint deposit' }))
    fireEvent.change(screen.getByRole('textbox', { name: 'ETH to withdraw' }), { target: { value: '.01' } })
    fireEvent.click(screen.getByRole('button', { name: 'Review withdrawal' }))
    expect(await screen.findByText('Up to 0.001 ETH')).toBeVisible()
    expect(mocks.execute).not.toHaveBeenCalled()
    const captured = mocks.quoteAction.mock.calls[0][0] as { account: Address; calls: { data: Hex; to: Address; value: bigint }[] }
    expect(captured.account).toBe(sender)
    expect(captured.calls[0]).toMatchObject({ to: entryPoint, value: 0n })
    expect(decodeFunctionData({ abi: parseAbi(['function withdrawTo(address,uint256)']), data: captured.calls[0].data }))
      .toEqual({ functionName: 'withdrawTo', args: [owner, parseEther('0.01')] })
    fireEvent.click(screen.getByRole('button', { name: 'Continue to wallet' }))
    expect(await screen.findByText('ETH withdrawal confirmed.')).toBeVisible()
    await waitFor(() => expect(mocks.execute).toHaveBeenCalledWith(expect.objectContaining({ action: captured, ownerAddress: owner })))
    expect(mocks.sendTransaction).not.toHaveBeenCalled()
  })

  it('leaves a provisionally included ETH withdrawal in Activity without reporting confirmation or failure', async () => {
    writeGasPaymentPreference(scope, 'eth')
    mocks.execute.mockRejectedValueOnce(new KernelObservedPendingError())
    renderPanel()
    fireEvent.click(await screen.findByRole('button', { name: 'Withdraw ETH' }))
    fireEvent.change(screen.getByRole('textbox', { name: 'ETH to withdraw' }), { target: { value: '.01' } })
    fireEvent.click(screen.getByRole('button', { name: 'Review withdrawal' }))
    fireEvent.click(await screen.findByRole('button', { name: 'Continue to wallet' }))
    expect(await screen.findByText(/included and is awaiting confirmation/)).toBeVisible()
    expect(screen.queryByRole('alert')).not.toBeInTheDocument()
    expect(screen.queryByText('ETH withdrawal confirmed.')).not.toBeInTheDocument()
    expect(screen.queryByRole('button', { name: 'Continue to wallet' })).not.toBeInTheDocument()
  })

  it('preserves a reviewed ETH withdrawal when verification polling fails', async () => {
    writeGasPaymentPreference(scope, 'eth')
    const { queryClient } = renderPanel()
    fireEvent.click(await screen.findByRole('button', { name: 'Withdraw ETH' }))
    fireEvent.change(screen.getByRole('textbox', { name: 'ETH to withdraw' }), { target: { value: '.01' } })
    fireEvent.click(screen.getByRole('button', { name: 'Review withdrawal' }))
    const continueButton = await screen.findByRole('button', { name: 'Continue to wallet' })
    expect(continueButton).toBeEnabled()

    mocks.verifyAccount.mockRejectedValueOnce(new Error('RPC unavailable'))
    await act(() => queryClient.invalidateQueries({ queryKey: ['kernel-eth-funding'] }))
    expect(await screen.findByText(/Showing the last verified balances/)).toBeVisible()
    expect(screen.getByRole('textbox', { name: 'ETH to withdraw' })).toHaveValue('.01')
    expect(screen.getByText('Up to 0.001 ETH')).toBeVisible()
    expect(continueButton).toBeDisabled()
    expect(mocks.execute).not.toHaveBeenCalled()
  })
})
