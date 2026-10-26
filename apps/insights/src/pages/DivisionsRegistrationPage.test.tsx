import { fireEvent, render, screen, waitFor } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { DivisionsRegistrationPage } from './DivisionsRegistrationPage'
import type { DivisionsRegistrationSession } from '../divisions/registration'

const api = vi.hoisted(() => ({ getDivisionsRegistrationSession: vi.fn(), completeDivisionsRegistration: vi.fn() }))
vi.mock('../divisions/registration', () => api)
vi.mock('../config/wagmi', () => ({ walletConnectionConfigured: true }))
vi.mock('@reown/appkit/react', () => ({ useAppKit: vi.fn() }))
vi.mock('wagmi', () => ({ useConnection: vi.fn(), useSignMessage: vi.fn(), useSwitchChain: vi.fn() }))
vi.mock('../components/TurnstileWidget', () => ({ TurnstileWidget: () => <div>Spam protection widget</div> }))

const session: DivisionsRegistrationSession = {
  status: 'in_progress', csrfToken: 'csrf', expiresAt: '2026-10-26T21:00:00Z',
  steps: { discord: 'verified', wallet: 'verified', completed: false },
  identity: { username: 'trader' },
  wallet: { ownerAddress: `0x${'1'.repeat(40)}`, tradingAccount: `0x${'2'.repeat(40)}` },
  requiredConsents: { rulesVersion: 'v1', privacyVersion: 'v1' },
}

beforeEach(() => {
  vi.clearAllMocks()
  api.getDivisionsRegistrationSession.mockResolvedValue(session)
  api.completeDivisionsRegistration.mockResolvedValue({ ...session, status: 'completed' })
})

describe('Discord leaderboard visibility', () => {
  it.each([false, true])('keeps publishing optional and submits the explicit choice %s', async (publicHandle) => {
    render(<MemoryRouter><DivisionsRegistrationPage /></MemoryRouter>)
    const checkbox = await screen.findByRole('checkbox', { name: /Show my Discord handle publicly/ })
    expect(checkbox).not.toBeChecked()
    fireEvent.click(screen.getByRole('checkbox', { name: /I accept the/ }))
    fireEvent.click(screen.getByRole('checkbox', { name: /I understand Plether stores/ }))
    if (publicHandle) fireEvent.click(checkbox)
    fireEvent.click(screen.getByRole('button', { name: 'Complete registration' }))
    await waitFor(() => expect(api.completeDivisionsRegistration).toHaveBeenCalledWith(session, publicHandle))
    expect(await screen.findByRole('heading', { name: 'Discord and wallet linked' })).toBeInTheDocument()
  })

  it('lets an existing trader restart verification to change visibility', async () => {
    api.getDivisionsRegistrationSession.mockResolvedValue({ ...session, status: 'completed' })
    render(<MemoryRouter><DivisionsRegistrationPage /></MemoryRouter>)
    fireEvent.click(await screen.findByRole('button', { name: 'Update leaderboard visibility' }))
    expect(screen.getByText('Spam protection widget')).toBeInTheDocument()
    expect(api.completeDivisionsRegistration).not.toHaveBeenCalled()
  })
})
