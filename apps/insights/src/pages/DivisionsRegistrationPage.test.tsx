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

describe('Discord registration', () => {
  it('explains public display and completes registration without a visibility choice', async () => {
    render(<MemoryRouter><DivisionsRegistrationPage /></MemoryRouter>)
    await screen.findByRole('heading', { name: 'Review your link' })
    expect(screen.getAllByRole('checkbox')).toHaveLength(2)
    expect(screen.queryByRole('checkbox', { name: /Show my Discord handle publicly/ })).not.toBeInTheDocument()
    expect(screen.getByText(/Registering makes my Discord handle public beside my Trading Account/)).toBeInTheDocument()
    fireEvent.click(screen.getByRole('checkbox', { name: /I accept the/ }))
    fireEvent.click(screen.getByRole('checkbox', { name: /I understand Plether stores/ }))
    fireEvent.click(screen.getByRole('button', { name: 'Complete registration' }))
    await waitFor(() => expect(api.completeDivisionsRegistration).toHaveBeenCalledWith(session))
    expect(await screen.findByRole('heading', { name: 'Discord and wallet linked' })).toBeInTheDocument()
  })

  it('shows completed traders their handle is displayed without requiring another registration', async () => {
    api.getDivisionsRegistrationSession.mockResolvedValue({ ...session, status: 'completed' })
    render(<MemoryRouter><DivisionsRegistrationPage /></MemoryRouter>)
    await screen.findByRole('heading', { name: 'Discord and wallet linked' })
    expect(screen.getByText(/Your Discord handle appears beside your Trading Account/)).toBeInTheDocument()
    expect(screen.queryByRole('button', { name: 'Update leaderboard visibility' })).not.toBeInTheDocument()
    expect(api.completeDivisionsRegistration).not.toHaveBeenCalled()
  })
})
