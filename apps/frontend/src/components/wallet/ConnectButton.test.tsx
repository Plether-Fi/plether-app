import { fireEvent, render, screen } from '@testing-library/react'
import { beforeEach, expect, it, vi } from 'vitest'
import { MemoryRouter } from 'react-router-dom'
import { ConnectButton } from './ConnectButton'

const emailAddress = '0x1111111111111111111111111111111111111111'
const externalAddress = '0x2222222222222222222222222222222222222222'
const mocks = vi.hoisted(() => ({
  address: undefined as string | undefined,
  authenticated: true,
  selectWallet: vi.fn(),
}))
vi.mock('wagmi', () => ({
  useAccount: () => ({ address: mocks.address, isConnected: Boolean(mocks.address) }),
  useChainId: () => 421614,
}))
vi.mock('../../config/wagmi', () => ({ anvil: { id: 31337 } }))
vi.mock('../../hooks', () => ({ useSwitchToArbitrumSepolia: () => ({ clearSwitchError: vi.fn() }) }))
vi.mock('../../wallet/useWalletActions', () => ({ useWalletActions: () => ({
  ready: true, configured: true, authenticated: mocks.authenticated,
  wallets: [{ address: emailAddress, embedded: true, name: 'Social wallet' }],
  selectWallet: mocks.selectWallet,
}) }))

beforeEach(() => {
  mocks.address = undefined
  mocks.authenticated = true
  mocks.selectWallet.mockClear()
})

it.each([undefined, externalAddress])('restores the authenticated email owner from %s', address => {
  mocks.address = address
  render(<MemoryRouter initialEntries={['/privacy']}><ConnectButton /></MemoryRouter>)
  fireEvent.click(screen.getByRole('button', { name: 'Use email wallet' }))
  expect(mocks.selectWallet).toHaveBeenCalledWith(emailAddress)
})

it.each([true, false])('does not offer email switching when it is already selected or unauthenticated (%s)', authenticated => {
  mocks.authenticated = authenticated
  mocks.address = authenticated ? emailAddress : externalAddress
  render(<MemoryRouter initialEntries={['/privacy']}><ConnectButton /></MemoryRouter>)
  expect(screen.queryByRole('button', { name: 'Use email wallet' })).not.toBeInTheDocument()
})
