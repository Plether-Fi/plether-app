import { renderHook } from '@testing-library/react'
import type { ReactNode } from 'react'
import { describe, expect, it, vi } from 'vitest'
import { WalletActionsContext, type WalletActions } from '../wallet/useWalletActions'
import { WagmiPerpsIdentityProvider } from './PerpsIdentityProvider'
import { usePerpsIdentity } from './usePerpsIdentity'

const wallet = vi.hoisted(() => ({
  address: '0x1111111111111111111111111111111111111111',
  chainId: 421614,
}))

vi.mock('wagmi', () => ({
  useAccount: () => wallet,
  usePublicClient: () => ({}),
}))

function actions(signerReady: boolean): WalletActions {
  return {
    configured: true,
    ready: true,
    signerReady,
    authenticated: false,
    modalOpen: false,
    wallets: [],
    error: null,
    connect: vi.fn(),
    openAccount: vi.fn(),
    selectWallet: vi.fn(),
    disconnect: vi.fn(),
    exportWallet: vi.fn(),
    setRecovery: vi.fn(),
    clearError: vi.fn(),
  }
}

describe('Perps identity during Privy owner changes', () => {
  it('withholds the old owner until the selected signer has synchronized', () => {
    let controls = actions(false)
    function wrapper({ children }: { children: ReactNode }) {
      return (
        <WalletActionsContext value={controls}>
          <WagmiPerpsIdentityProvider manifestUrl={null}>
            {children}
          </WagmiPerpsIdentityProvider>
        </WalletActionsContext>
      )
    }

    const { result, rerender } = renderHook(usePerpsIdentity, { wrapper })
    expect(result.current.ownerAddress).toBeUndefined()
    expect(result.current.accountAddress).toBeUndefined()

    controls = actions(true)
    rerender()
    expect(result.current.ownerAddress).toBe(wallet.address)

    controls = actions(false)
    rerender()
    expect(result.current.ownerAddress).toBeUndefined()
    expect(result.current.accountAddress).toBeUndefined()
  })
})
