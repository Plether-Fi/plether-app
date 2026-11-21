import { act, fireEvent, render, screen, waitFor } from '@testing-library/react'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { useState } from 'react'
import { parseEther } from 'viem'
import { writeGasPaymentPreference } from '../../perps-aa/gasPaymentPreference'
import type { KernelActionQuote } from '../../perps-aa/kernelRuntime'
import { KernelRpcError } from '../../perps-aa/kernelRpc'
import { usePerpsUiStore } from '../../stores/perpsUiStore'
import { KernelGasApprovalButtons, PerpsGasReview } from '../PerpsGasReview'
import { useKernelGasApproval, usePerpsGasReview } from '../../hooks/usePerpsGasReview'

const { scope, ownerAddress, runtime } = vi.hoisted(() => {
  const scope = { chainId: 421614, deployment: 'fresh', sender: '0x1000000000000000000000000000000000000000' as const }
  const ownerAddress = '0x2000000000000000000000000000000000000000'
  const kernel = { scope: { ...scope, entryPoint: '0x433709009b8330fda32311df1c2afa402ed8d009' } }
  return { scope, ownerAddress, runtime: { ownerAddress, kernel, gasPayment: { scope, readiness: { sponsored: true, eth: true } } } }
})

vi.mock('../../perps-aa/runtimeContext', () => ({ usePerpsAaRuntime: () => runtime }))
vi.mock('../../perps-aa/deadlineClock', () => ({ deadlineNow: () => Date.now() }))

function Review({ quote, error, approved, cancelled }: { quote: KernelActionQuote; error?: unknown; approved: () => void; cancelled: () => void }) {
  const gas = usePerpsGasReview()
  const approval = useKernelGasApproval(gas)
  const [started, setStarted] = useState(false)
  return (
    <>
      <PerpsGasReview gas={gas} quote={approval.quote} error={error} />
      {approval.quote ? <KernelGasApprovalButtons confirm={approval.confirm} cancel={approval.cancel} /> : null}
      {!started ? <button type="button" onClick={() => {
        setStarted(true)
        void approval.review(quote).then(approved, cancelled)
      }}>Review network gas</button> : null}
    </>
  )
}

const ethQuote = () => ({ mode: 'eth' as const, maxNetworkGasWei: parseEther('0.000047'), expiresAt: Math.floor(Date.now() / 1_000) + 60 })

afterEach(() => {
  localStorage.clear()
  usePerpsUiStore.setState({ activityRequest: null })
  runtime.gasPayment.readiness.sponsored = true
})

describe('Kernel gas review', () => {
  it('shows the selected payer and opens account activity during a sponsorship outage', () => {
    runtime.gasPayment.readiness.sponsored = false
    render(<Review quote={ethQuote()} approved={vi.fn()} cancelled={vi.fn()} />)
    expect(screen.getByText('Sponsored by Plether')).toBeVisible()
    expect(screen.getByText('Gas sponsorship is currently unavailable.')).toBeVisible()
    expect(screen.queryByRole('switch')).not.toBeInTheDocument()
    fireEvent.click(screen.getByRole('button', { name: 'Gas settings' }))
    expect(usePerpsUiStore.getState().activityRequest).toEqual({ id: 1, chainId: scope.chainId, accountAddress: scope.sender, ownerAddress })
  })

  it('displays the exact ETH maximum before allowing wallet approval', async () => {
    writeGasPaymentPreference(scope, 'eth')
    const approved = vi.fn()
    render(<Review quote={ethQuote()} approved={approved} cancelled={vi.fn()} />)
    fireEvent.click(screen.getByRole('button', { name: 'Review network gas' }))
    expect(screen.getByText('0.000047 ETH')).toBeVisible()
    expect(approved).not.toHaveBeenCalled()
    fireEvent.click(screen.getByRole('button', { name: 'Continue to wallet' }))
    await waitFor(() => { expect(approved).toHaveBeenCalledOnce() })
  })

  it('offers a top-up for an advisory shortfall without opening wallet approval', () => {
    writeGasPaymentPreference(scope, 'eth')
    const approved = vi.fn()
    const cause = new KernelRpcError('INSUFFICIENT_GAS_ETH', 'Not enough ETH', false, {
      balanceWei: parseEther('0.000009'), entryPointDepositWei: parseEther('0.000001'),
      maxNetworkGasWei: parseEther('0.000047'), callValueWei: 0n, shortfallWei: parseEther('0.000037'),
    })
    render(<Review quote={ethQuote()} error={new Error('Review failed', { cause })} approved={approved} cancelled={vi.fn()} />)
    expect(screen.getByText('Add at least 0.000037 ETH, then review a fresh gas quote.')).toBeVisible()
    expect(screen.getByText('EntryPoint ETH deposit')).toBeVisible()
    expect(screen.queryByRole('button', { name: 'Continue to wallet' })).not.toBeInTheDocument()
    fireEvent.click(screen.getByRole('button', { name: 'Add ETH' }))
    expect(usePerpsUiStore.getState().activityRequest).toMatchObject({ chainId: scope.chainId, accountAddress: scope.sender, ownerAddress })
    expect(approved).not.toHaveBeenCalled()
  })

  it('invalidates an unsigned quote when another tab changes the gas preference', async () => {
    writeGasPaymentPreference(scope, 'eth')
    const approved = vi.fn()
    const cancelled = vi.fn()
    render(<Review quote={ethQuote()} approved={approved} cancelled={cancelled} />)
    fireEvent.click(screen.getByRole('button', { name: 'Review network gas' }))
    act(() => {
      const key = localStorage.key(0)
      if (key === null) throw new Error('The test gas preference was not saved.')
      localStorage.removeItem(key)
      window.dispatchEvent(new StorageEvent('storage', { key }))
    })
    await waitFor(() => { expect(cancelled).toHaveBeenCalledOnce() })
    expect(approved).not.toHaveBeenCalled()
    expect(screen.queryByRole('button', { name: 'Continue to wallet' })).not.toBeInTheDocument()
    expect(screen.getByText('Sponsored by Plether')).toBeVisible()
  })
})
