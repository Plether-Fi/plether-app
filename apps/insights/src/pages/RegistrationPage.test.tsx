import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { fireEvent, render, screen, waitFor } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { beforeEach, describe, expect, it, vi } from 'vitest'
import { safeXAuthorizationUrl } from '../utils/registration'
import { RegistrationPage } from './RegistrationPage'

const apiMocks = vi.hoisted(() => {
  class MockInsightsApiError extends Error {
    status = 409
    code: string
    retryAfterSeconds = null

    constructor(code = 'INVALID_REQUEST') {
      super(code)
      this.code = code
    }
  }

  return {
    MockInsightsApiError,
    useCurrentCompetition: vi.fn(),
    useInsightsStatus: vi.fn(),
    useRegistrationSession: vi.fn(),
    createRegistrationSession: vi.fn(),
    createXAuthorization: vi.fn(),
    confirmXFollow: vi.fn(),
    completeRegistration: vi.fn(),
  }
})

vi.mock('../api', () => ({
  ...apiMocks,
  queryKeys: {
    competition: ['insights', 'competition', 'current'],
    registration: (slug: string) => ['insights', 'registration', slug],
    status: ['insights', 'status'],
  },
  InsightsApiError: apiMocks.MockInsightsApiError,
}))

vi.mock('../components/TurnstileWidget', () => ({
  TurnstileWidget: () => <div>Spam protection widget</div>,
}))

vi.mock('../components/RegistrationWalletStep', () => ({
  RegistrationWalletStep: () => <div>Wallet verification controls</div>,
}))

const competition = {
  id: 'competition-2026-09',
  slug: 'testnet-trading-2026-09',
  name: 'September Testnet Competition',
  status: 'scheduled',
  startsAt: '2026-09-13T21:00:00Z',
  tradingCutoffAt: '2026-09-25T21:00:00Z',
  resultsAt: '2026-09-28T12:00:00Z',
  startingBalance: '100000000000',
  pnlEligibilityThreshold: '1000000000',
  minActiveDays: 5,
  prizes: [],
  latestIndexedBlock: null,
  latestIndexedAt: null,
  participantCount: 127,
  registration: {
    status: 'open',
    opensAt: '2026-08-28T10:00:00Z',
    closesAt: '2026-09-20T21:00:00Z',
    minimumXAccountAgeDays: 90,
    targetXHandle: 'plether_fi',
    rulesVersion: 'rules-v1',
    privacyVersion: 'privacy-v1',
  },
  fxSessionBoundaryUtc: '21:00',
}

function renderPage() {
  const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } })
  return render(
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={[`/competitions/${competition.slug}/register`]}>
        <Routes>
          <Route path="/competitions/:slug/register" element={<RegistrationPage />} />
        </Routes>
      </MemoryRouter>
    </QueryClientProvider>,
  )
}

beforeEach(() => {
  vi.clearAllMocks()
  apiMocks.useCurrentCompetition.mockReturnValue({
    data: competition,
    isLoading: false,
    isError: false,
  })
  apiMocks.useInsightsStatus.mockReturnValue({
    data: undefined,
    isLoading: false,
    isError: true,
  })
  apiMocks.useRegistrationSession.mockReturnValue({
    data: undefined,
    isLoading: false,
    isError: false,
    isFetching: false,
  })
})

describe('RegistrationPage', () => {
  it('accepts only the pinned X OAuth authorization endpoint and PKCE parameters', () => {
    const state = 'a'.repeat(43)
    const challenge = 'b'.repeat(43)
    const validUrl = `https://x.com/i/oauth2/authorize?response_type=code&client_id=client&redirect_uri=https%3A%2F%2Finsights.plether.com%2Fapi%2Finsights%2Fv1%2Fcompetitions%2Ftestnet-trading-2026-09%2Fregistrations%2Fx%2Fcallback&scope=users.read%20users.email%20follows.read&state=${state}&code_challenge=${challenge}&code_challenge_method=S256`
    expect(safeXAuthorizationUrl(validUrl)).toBe(validUrl)
    expect(() => safeXAuthorizationUrl(validUrl.replace('https://x.com', 'https://evil.example'))).toThrow('invalid authorization')
    expect(() => safeXAuthorizationUrl(validUrl.replace('/i/oauth2/authorize', '/attacker'))).toThrow('invalid authorization')
    expect(() => safeXAuthorizationUrl(validUrl.replace('code_challenge_method=S256', 'code_challenge_method=plain'))).toThrow('invalid authorization')
    expect(() => safeXAuthorizationUrl(`${validUrl}&state=${state}`)).toThrow('invalid authorization')
    expect(() => safeXAuthorizationUrl(validUrl.replace('users.email%20', ''))).toThrow('invalid authorization')
  })

  it('starts with the Turnstile spam-protection step', () => {
    renderPage()
    expect(screen.getByRole('heading', { name: 'Confirm you’re human' })).toBeInTheDocument()
    expect(screen.getByText('Spam protection widget')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Start registration' })).toBeDisabled()
  })

  it('trusts the server registration status instead of the visitor device clock', () => {
    apiMocks.useCurrentCompetition.mockReturnValue({
      data: {
        ...competition,
        registration: {
          ...competition.registration,
          status: 'open',
          opensAt: '2099-01-01T00:00:00Z',
          closesAt: '2100-01-01T00:00:00Z',
        },
      },
      isLoading: false,
      isError: false,
    })

    renderPage()
    expect(screen.getByRole('heading', { name: 'Confirm you’re human' })).toBeInTheDocument()
  })

  it('resumes at X identity verification', () => {
    apiMocks.useRegistrationSession.mockReturnValue({
      data: {
        status: 'in_progress',
        csrfToken: 'csrf',
        expiresAt: '2026-08-28T12:00:00Z',
        steps: { xIdentity: 'pending', xFollow: 'pending', wallet: 'pending', completed: false },
        requiredConsents: { rulesVersion: 'rules-v1', privacyVersion: 'privacy-v1' },
      },
      isLoading: false,
      isError: false,
      isFetching: false,
    })

    renderPage()
    expect(screen.getByRole('heading', { name: 'Verify your X identity' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Continue with X' })).toBeEnabled()
  })

  it('shows a sanitized OAuth callback failure restored from the session', () => {
    apiMocks.useRegistrationSession.mockReturnValue({
      data: {
        status: 'in_progress',
        csrfToken: 'csrf',
        expiresAt: '2026-08-28T12:00:00Z',
        oauthErrorCode: 'X_ACCOUNT_TOO_NEW',
        steps: { xIdentity: 'pending', xFollow: 'pending', wallet: 'pending', completed: false },
        requiredConsents: { rulesVersion: 'rules-v1', privacyVersion: 'privacy-v1' },
      },
      isLoading: false,
      isError: false,
      isFetching: false,
    })

    renderPage()
    expect(screen.getByRole('alert')).toHaveTextContent('This X account is too new')
    expect(screen.getByRole('button', { name: 'Continue with X' })).toBeEnabled()
  })

  it('requires the explicit follow action after X identity verification', () => {
    apiMocks.useRegistrationSession.mockReturnValue({
      data: {
        status: 'in_progress',
        csrfToken: 'csrf',
        expiresAt: '2026-08-28T12:00:00Z',
        steps: { xIdentity: 'verified', xFollow: 'pending', wallet: 'pending', completed: false },
        identity: { xHandle: 'alice', maskedEmail: 'a***@example.com' },
        requiredConsents: { rulesVersion: 'rules-v1', privacyVersion: 'privacy-v1' },
      },
      isLoading: false,
      isError: false,
      isFetching: false,
    })

    renderPage()
    expect(screen.getByRole('heading', { name: 'Follow @plether_fi' })).toBeInTheDocument()
    expect(screen.getByRole('link', { name: 'Open @plether_fi on X ↗' })).toHaveAttribute('href', 'https://x.com/plether_fi')
    expect(screen.getByRole('button', { name: 'Verify follow' })).toBeEnabled()
  })

  it('resumes a verified X session at wallet ownership', () => {
    apiMocks.useRegistrationSession.mockReturnValue({
      data: {
        status: 'in_progress',
        csrfToken: 'csrf',
        expiresAt: '2026-08-28T12:00:00Z',
        steps: { xIdentity: 'verified', xFollow: 'verified', wallet: 'pending', completed: false },
        identity: { xHandle: 'alice', maskedEmail: 'a***@example.com' },
        requiredConsents: { rulesVersion: 'rules-v1', privacyVersion: 'privacy-v1' },
      },
      isLoading: false,
      isError: false,
      isFetching: false,
    })

    renderPage()
    expect(screen.getByRole('heading', { name: 'Verify your wallet' })).toBeInTheDocument()
    expect(screen.getByText('Wallet verification controls')).toBeInTheDocument()
    expect(screen.getByText('Wallet ownership').parentElement).toHaveAttribute('aria-current', 'step')
  })

  it('shows the verified public X handle and trading link after completion', () => {
    apiMocks.useRegistrationSession.mockReturnValue({
      data: {
        status: 'completed',
        csrfToken: 'csrf',
        expiresAt: '2026-08-28T12:00:00Z',
        steps: { xIdentity: 'verified', xFollow: 'verified', wallet: 'verified', completed: true },
        identity: { xHandle: 'alice', maskedEmail: 'a***@example.com' },
        wallet: {
          ownerAddress: '0x1111111111111111111111111111111111111111',
          tradingAccount: '0x2222222222222222222222222222222222222222',
        },
        requiredConsents: { rulesVersion: 'rules-v1', privacyVersion: 'privacy-v1' },
      },
      isLoading: false,
      isError: false,
      isFetching: false,
    })

    renderPage()
    expect(screen.getByRole('heading', { name: 'Congratulations, you’re in.' })).toBeInTheDocument()
    expect(screen.getByText('127')).toBeInTheDocument()
    expect(screen.getByText('participants registered so far')).toBeInTheDocument()
    expect(screen.getAllByText('Registration complete')).toHaveLength(2)
    expect(screen.queryByText(/pending the standard integrity review/i)).not.toBeInTheDocument()
    expect(screen.getByRole('link', { name: '@alice ↗' })).toHaveAttribute('href', 'https://x.com/alice')
    expect(screen.getByRole('link', { name: 'Open Plether testnet ↗' })).toHaveAttribute('href', 'https://app.sepolia.plether.com')
  })

  it('shows the review step and requires both versioned consents', () => {
    apiMocks.useRegistrationSession.mockReturnValue({
      data: {
        status: 'in_progress',
        csrfToken: 'csrf',
        expiresAt: '2026-08-28T12:00:00Z',
        steps: { xIdentity: 'verified', xFollow: 'verified', wallet: 'verified', completed: false },
        identity: { xHandle: 'alice', maskedEmail: 'a***@example.com' },
        wallet: {
          ownerAddress: '0x1111111111111111111111111111111111111111',
          tradingAccount: '0x2222222222222222222222222222222222222222',
        },
        requiredConsents: { rulesVersion: 'rules-v1', privacyVersion: 'privacy-v1' },
      },
      isLoading: false,
      isError: false,
      isFetching: false,
    })

    renderPage()
    expect(screen.getByRole('heading', { name: 'Review your entry' })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Complete registration' })).toBeDisabled()
    expect(screen.getAllByRole('checkbox')).toHaveLength(2)
  })

  it('does not discard wallet ownership when a legacy backend returns the retired account-state error', async () => {
    const refetch = vi.fn().mockResolvedValue(undefined)
    apiMocks.useRegistrationSession.mockReturnValue({
      data: {
        status: 'in_progress',
        csrfToken: 'csrf',
        expiresAt: '2026-08-28T12:00:00Z',
        steps: { xIdentity: 'verified', xFollow: 'verified', wallet: 'verified', completed: false },
        identity: { xHandle: 'alice', maskedEmail: 'a***@example.com' },
        wallet: {
          ownerAddress: '0x1111111111111111111111111111111111111111',
          tradingAccount: '0x2222222222222222222222222222222222222222',
        },
        requiredConsents: { rulesVersion: 'rules-v1', privacyVersion: 'privacy-v1' },
      },
      isLoading: false,
      isError: false,
      isFetching: false,
      refetch,
    })
    apiMocks.completeRegistration.mockRejectedValue(
      new apiMocks.MockInsightsApiError('TRADING_ACCOUNT_EXISTS'),
    )

    renderPage()
    expect(screen.getByText(/clean starting state is checked at the competition baseline/i)).toBeInTheDocument()
    expect(screen.getByText('My X handle will be public.').tagName).toBe('LI')
    expect(screen.getByText(/confirmed email is encrypted and may be used for competition integrity, duplicate prevention, and competition-relevant messages/i)).toBeInTheDocument()
    expect(screen.getByText(/private owner-wallet-to-Trading-Account link is protected and retained indefinitely/i).tagName).toBe('LI')
    expect(screen.queryByText(/confirmed email is encrypted and retained indefinitely/i)).not.toBeInTheDocument()
    const consents = screen.getAllByRole('checkbox')
    fireEvent.click(consents[0])
    fireEvent.click(consents[1])
    fireEvent.click(screen.getByRole('button', { name: 'Complete registration' }))

    await waitFor(() => {
      expect(screen.getByRole('alert')).toHaveTextContent('Wallet verification changed. Verify the wallet again.')
    })
    expect(refetch).not.toHaveBeenCalled()
    expect(screen.getByRole('heading', { name: 'Review your entry' })).toBeInTheDocument()
  })

  it('fails closed when registration metadata is closed', () => {
    apiMocks.useCurrentCompetition.mockReturnValue({
      data: { ...competition, registration: { ...competition.registration, status: 'closed' } },
      isLoading: false,
      isError: false,
    })

    renderPage()
    expect(screen.getByRole('heading', { name: 'Registration is closed' })).toBeInTheDocument()
  })
})
