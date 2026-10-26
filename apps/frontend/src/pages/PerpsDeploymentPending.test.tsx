import { render, screen } from '@testing-library/react'
import { describe, expect, it, vi } from 'vitest'

const mocks = vi.hoisted(() => ({ liveHook: vi.fn(() => { throw new Error('Prepared pages must not mount live hooks') }) }))

vi.mock('../contracts/perpsAddresses', async (importOriginal) => ({
  ...await importOriginal<typeof import('../contracts/perpsAddresses')>(),
  PERPS_RELEASE_IS_DEPLOYED: false,
}))
vi.mock('../hooks', async (importOriginal) => ({
  ...await importOriginal<typeof import('../hooks')>(),
  usePerpsMarket: mocks.liveHook,
  usePerpsAccount: mocks.liveHook,
  usePerpsHistory: mocks.liveHook,
  useSwitchToArbitrumSepolia: mocks.liveHook,
}))
vi.mock('../config/wagmi', () => ({ syncAppKitModalStyleOverrides: vi.fn(), openAppKit: vi.fn() }))

import { Perps } from './Perps'
import { Vaults } from './Vaults'
import { TestnetWelcomeModal } from '../components/TestnetWelcomeModal'

describe('prepared perps release', () => {
  it.each([['Perps', Perps], ['Vaults', Vaults]] as const)('keeps %s unavailable without mounting its live hooks', (_name, Page) => {
    render(<Page />)
    expect(screen.getByRole('status')).toHaveTextContent('Mock USDG perps are being prepared')
    expect(screen.getByRole('status')).toHaveTextContent('Trading, testnet funding, and vault deposits will be available after deployment.')
    expect(screen.queryByRole('button')).not.toBeInTheDocument()
    expect(mocks.liveHook).not.toHaveBeenCalled()
  })

  it('does not mount the funding modal or its wallet hooks', () => {
    const { container } = render(<TestnetWelcomeModal />)
    expect(container).toBeEmptyDOMElement()
  })
})
