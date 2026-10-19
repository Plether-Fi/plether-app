import { act, screen } from '@testing-library/react'
import { beforeEach, expect, it, vi } from 'vitest'

vi.mock('@reown/appkit/react', () => ({
  useAppKit: () => ({ open: vi.fn() }),
}))

vi.mock('./config/wagmi', async () => {
  const { createConfig, http } = await import('wagmi')
  const { arbitrumSepolia } = await import('wagmi/chains')
  return {
    walletConnectionConfigured: true,
    wagmiConfig: createConfig({
      chains: [arbitrumSepolia],
      transports: { [arbitrumSepolia.id]: http() },
    }),
  }
})

vi.mock('./divisions/registration', async (importOriginal) => ({
  ...await importOriginal<typeof import('./divisions/registration')>(),
  getDivisionsRegistrationSession: vi.fn().mockResolvedValue({
    status: 'in_progress',
    csrfToken: 'test-csrf',
    expiresAt: '2026-10-19T12:00:00Z',
    steps: { discord: 'verified', wallet: 'pending', completed: false },
    identity: { username: 'Test trader' },
    requiredConsents: { rulesVersion: 'divisions-2026-10', privacyVersion: 'divisions-2026-10' },
  }),
}))

vi.mock('./components/Layout', () => ({
  Layout: ({ children }: { children: React.ReactNode }) => <>{children}</>,
}))

beforeEach(() => {
  document.body.innerHTML = '<div id="root"></div>'
  window.history.replaceState({}, '', '/register')
})

it('renders the wallet step after returning from Discord', async () => {
  await act(async () => { await import('./main') })
  expect(await screen.findByRole('heading', { name: 'Verify your wallet' })).toBeInTheDocument()
  expect(screen.getByText(/Discord connected as/)).toBeInTheDocument()
  expect(screen.getByRole('button', { name: 'Connect wallet' })).toBeInTheDocument()
})
