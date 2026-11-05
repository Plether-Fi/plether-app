import type { ReactNode } from 'react'
import { act, render, screen } from '@testing-library/react'
import { QueryClient } from '@tanstack/react-query'
import { describe, expect, it, vi } from 'vitest'

const initializePrivy = vi.hoisted(() => {
  vi.stubEnv('VITE_PRIVY_APP_ID', '')
  return vi.fn()
})

vi.mock('@privy-io/react-auth', () => ({ PrivyProvider: initializePrivy }))
vi.mock('@privy-io/react-auth/ui', () => ({ WalletsDialog: () => null }))
vi.mock('@privy-io/wagmi', () => ({ WagmiProvider: ({ children }: { children: ReactNode }) => children }))
vi.mock('wagmi', () => ({ WagmiProvider: ({ children }: { children: ReactNode }) => children }))
vi.mock('../config/wagmi', () => ({ config: {}, walletChains: [{ id: 1 }, { id: 11155111 }, { id: 421614 }] }))
vi.mock('../utils/deployment', () => ({ isSepoliaDeployment: () => true }))

import { WalletProvider } from './WalletProvider'
import { useWalletActions } from './useWalletActions'

function ReadOnlyPage() {
  const { configured, connect } = useWalletActions()
  return <button disabled={!configured} onClick={connect}>Read markets</button>
}

describe('missing Privy configuration', () => {
  it('keeps the application readable without initializing Privy or enabling connection', () => {
    act(() => { render(<WalletProvider queryClient={new QueryClient()}><ReadOnlyPage /></WalletProvider>) })
    expect(screen.getByRole('button', { name: 'Read markets' })).toBeDisabled()
    expect(initializePrivy).not.toHaveBeenCalled()
  })
})
