import { render, screen } from '@testing-library/react'
import { beforeEach, expect, it, vi } from 'vitest'
import { MemoryRouter } from 'react-router-dom'
import { ConnectButton } from './ConnectButton'

const emailAddress = '0x1111111111111111111111111111111111111111'
const externalAddress = '0x2222222222222222222222222222222222222222'
const mocks = vi.hoisted(() => ({
  address: undefined as string | undefined,
  authenticated: true,
  ready: true,
  connectionPending: false,
  connect: vi.fn(),
  selectWallet: vi.fn(),
}))
vi.mock('wagmi', () => ({
  useAccount: () => ({ address: mocks.address, isConnected: Boolean(mocks.address) }),
  useChainId: () => 421614,
}))
vi.mock('../../config/wagmi', () => ({ anvil: { id: 31337 } }))
vi.mock('../../hooks', () => ({ useSwitchToArbitrumSepolia: () => ({ clearSwitchError: vi.fn() }) }))
vi.mock('../../wallet/useWalletActions', () => ({ useWalletActions: () => ({
  ready: mocks.ready, configured: true, authenticated: mocks.authenticated, connectionPending: mocks.connectionPending,
  wallets: [{ address: emailAddress, embedded: true, name: 'Social wallet' }],
  selectWallet: mocks.selectWallet,
  connect: mocks.connect,
}) }))

beforeEach(() => {
  mocks.address = undefined
  mocks.authenticated = true
  mocks.ready = true
  mocks.connectionPending = false
  mocks.selectWallet.mockClear()
  mocks.connect.mockClear()
})

it.each([undefined, emailAddress, externalAddress])('does not show an email-wallet shortcut in the header for %s', address => {
  mocks.address = address
  render(<MemoryRouter initialEntries={['/privacy']}><ConnectButton /></MemoryRouter>)
  expect(screen.queryByRole('button', { name: 'Use email wallet' })).not.toBeInTheDocument()
  expect(screen.queryByRole('button', { name: 'Disconnect wallet' })).not.toBeInTheDocument()
  expect(screen.getByRole('button', { name: address ? /^Open wallet account / : 'Select Wallet' })).toBeEnabled()
})

it('keeps the header free of the shortcut throughout logout', () => {
  mocks.address = emailAddress
  const view = render(<MemoryRouter initialEntries={['/privacy']}><ConnectButton /></MemoryRouter>)
  mocks.address = undefined
  view.rerender(<MemoryRouter initialEntries={['/privacy']}><ConnectButton /></MemoryRouter>)
  expect(screen.queryByRole('button', { name: 'Use email wallet' })).not.toBeInTheDocument()
  expect(screen.getByRole('button', { name: 'Select Wallet' })).toBeEnabled()
  mocks.authenticated = false
  view.rerender(<MemoryRouter initialEntries={['/privacy']}><ConnectButton /></MemoryRouter>)
  expect(screen.queryByRole('button', { name: 'Use email wallet' })).not.toBeInTheDocument()
  expect(screen.getByRole('button', { name: 'Connect Wallet' })).toBeEnabled()
})

it.each(['auth-loading', 'wallet-loading', 'old-owner'])('shows a disabled loading state during %s instead of a wallet choice', state => {
  mocks.ready = state !== 'auth-loading'
  mocks.connectionPending = state !== 'auth-loading'
  if (state === 'old-owner') mocks.address = externalAddress
  const view = render(<MemoryRouter initialEntries={['/privacy']}><ConnectButton /></MemoryRouter>)
  expect(screen.getByRole('button', { name: 'Connecting…' })).toBeDisabled()
  expect(screen.getByRole('button', { name: 'Connecting…' })).toHaveAttribute('aria-busy', 'true')
  expect(screen.queryByRole('button', { name: 'Select Wallet' })).not.toBeInTheDocument()
  expect(screen.queryByRole('button', { name: 'Connect Wallet' })).not.toBeInTheDocument()
  expect(screen.queryByRole('button', { name: /^Open wallet account / })).not.toBeInTheDocument()
  mocks.ready = true
  mocks.connectionPending = false
  mocks.address = emailAddress
  view.rerender(<MemoryRouter initialEntries={['/privacy']}><ConnectButton /></MemoryRouter>)
  expect(screen.getByRole('button', { name: /^Open wallet account / })).toBeEnabled()
  expect(screen.queryByRole('button', { name: 'Connecting…' })).not.toBeInTheDocument()
})

it('shows the wallet choice once initialization finishes with no selected owner', () => {
  mocks.ready = false
  const view = render(<MemoryRouter initialEntries={['/privacy']}><ConnectButton /></MemoryRouter>)
  expect(screen.getByRole('button', { name: 'Connecting…' })).toBeDisabled()
  mocks.ready = true
  view.rerender(<MemoryRouter initialEntries={['/privacy']}><ConnectButton /></MemoryRouter>)
  expect(screen.getByRole('button', { name: 'Select Wallet' })).toBeEnabled()
})
