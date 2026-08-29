import { useCallback, useMemo, useState } from 'react'
import { useQueryClient } from '@tanstack/react-query'
import { Link, Navigate, useParams } from 'react-router-dom'
import {
  completeRegistration,
  confirmXFollow,
  createRegistrationSession,
  createXAuthorization,
  InsightsApiError,
  queryKeys,
  type Competition,
  type RegistrationSession,
  useCurrentCompetition,
  useInsightsStatus,
  useRegistrationSession,
} from '../api'
import { RegistrationWalletStep } from '../components/RegistrationWalletStep'
import { TurnstileWidget } from '../components/TurnstileWidget'
import { ErrorState, LoadingState, Panel } from '../components/ui'
import { formatUtc, shortAddress, xProfileUrl } from '../utils/format'
import { registrationErrorCodeMessage, registrationErrorMessage, safeXAuthorizationUrl } from '../utils/registration'

const configuredTurnstileSiteKey = typeof import.meta.env.VITE_TURNSTILE_SITE_KEY === 'string'
  ? import.meta.env.VITE_TURNSTILE_SITE_KEY.trim()
  : ''
// Cloudflare's documented always-pass test key is valid on localhost. It is
// never selected by production builds, which must provide the real site key.
const TURNSTILE_SITE_KEY = configuredTurnstileSiteKey.length > 0
  ? configuredTurnstileSiteKey
  : import.meta.env.DEV
    ? '1x00000000000000000000AA'
    : ''
const TRADING_APP_URL = 'https://app.sepolia.plether.com'
const PRIMARY_BUTTON = 'border border-brand-orange bg-brand-orange px-5 py-2.5 text-sm font-semibold text-content-primary transition-colors hover:bg-brand-peach hover:text-app-bg disabled:cursor-not-allowed disabled:opacity-50'

const STEPS = [
  'Spam protection',
  'X identity',
  'Follow Plether',
  'Wallet ownership',
  'Review',
] as const

function isSessionMissing(error: unknown): boolean {
  return error instanceof InsightsApiError
    && (error.status === 401 || error.status === 404 || error.code === 'EXPIRED_SESSION' || error.code === 'INVALID_REQUEST')
}

function localDevelopmentAuthorizationUrl(value: string, slug: string): string | null {
  if (!import.meta.env.DEV) return null
  const url = new URL(value)
  const expectedPath = `/api/insights/v1/competitions/${encodeURIComponent(slug)}/registrations/x/callback`
  if (
    url.origin !== window.location.origin
    || url.pathname !== expectedPath
    || url.searchParams.get('mock') !== '1'
    || [...url.searchParams.keys()].some((key) => key !== 'mock')
  ) return null
  return url.toString()
}

function completedStepCount(registration: RegistrationSession | undefined): number {
  if (!registration) return 0
  if (registration.steps.completed || registration.status === 'completed') return 5
  if (registration.steps.wallet === 'verified') return 4
  if (registration.steps.xFollow === 'verified') return 3
  if (registration.steps.xIdentity === 'verified') return 2
  return 1
}

function StepRail({ completed }: { completed: number }) {
  const currentIndex = Math.min(completed, STEPS.length - 1)
  const progressLabel = completed >= STEPS.length
    ? 'Registration complete'
    : `Step ${String(currentIndex + 1)} of ${String(STEPS.length)} · ${STEPS[currentIndex]}`

  return (
    <div>
      <div className="mb-4 flex items-center justify-between gap-4 text-xs">
        <span className="font-semibold uppercase tracking-[0.14em] text-content-tertiary">Registration progress</span>
        <span className="text-right font-medium text-content-secondary">{progressLabel}</span>
      </div>
      <ol className="grid grid-cols-5" aria-label="Registration progress">
        {STEPS.map((label, index) => {
          const isComplete = index < completed
          const isCurrent = completed < STEPS.length && index === currentIndex
          return (
            <li
              key={label}
              className="relative flex min-w-0 flex-col items-center text-center"
              aria-current={isCurrent ? 'step' : undefined}
            >
              {index < STEPS.length - 1 && (
                <span
                  aria-hidden="true"
                  className={`absolute left-1/2 top-[15px] h-0.5 w-full ${index < completed ? 'bg-positive' : 'bg-brand-border/30'}`}
                />
              )}
              <span
                aria-hidden="true"
                className={`relative z-10 flex h-8 w-8 items-center justify-center rounded-full border font-mono text-xs font-semibold transition-colors ${
                  isComplete
                    ? 'border-positive bg-positive text-app-bg'
                    : isCurrent
                      ? 'border-brand-peach bg-brand-peach text-app-bg ring-4 ring-brand-peach/20'
                      : 'border-brand-border/40 bg-app-bg text-content-tertiary'
                }`}
              >
                {isComplete ? '✓' : String(index + 1).padStart(2, '0')}
              </span>
              <span className={`mt-2 px-1 text-[10px] font-semibold leading-4 sm:text-xs ${isCurrent ? 'text-brand-peach' : isComplete ? 'text-content-primary' : 'text-content-tertiary'}`}>
                {label}
                <span className="sr-only"> — {isComplete ? 'complete' : isCurrent ? 'current step' : 'not started'}</span>
              </span>
            </li>
          )
        })}
      </ol>
    </div>
  )
}

function RegistrationUnavailable({ competition }: { competition: Competition }) {
  const metadata = competition.registration
  if (!metadata) {
    return <ErrorState title="Registration is unavailable" message="This competition does not accept first-party registrations." />
  }
  if (metadata.status === 'upcoming') {
    return <ErrorState title="Registration has not opened" message={`Registration opens ${formatUtc(metadata.opensAt)}.`} />
  }
  return <ErrorState title="Registration is closed" message={`Registration closed ${formatUtc(metadata.closesAt)}.`} />
}

function Completion({ registration, participantCount }: { registration: RegistrationSession; participantCount?: number }) {
  const handle = registration.identity?.xHandle
  const profileUrl = xProfileUrl(handle)
  return (
    <Panel className="overflow-hidden">
      <div className="border-b border-positive/30 bg-positive/10 px-5 py-6 sm:px-7">
        <p className="text-xs font-semibold uppercase tracking-[0.18em] text-positive">Registration complete</p>
        <h1 className="mt-2 text-3xl font-semibold">Congratulations, you’re in.</h1>
        <p className="mt-3 max-w-2xl text-sm leading-6 text-content-secondary">
          Registration is complete. Trade with the verified Plether Trading Account when the competition opens.
        </p>
        {participantCount !== undefined && (
          <p className="mt-5 flex items-baseline gap-2 text-content-secondary">
            <span className="font-mono text-2xl font-semibold tabular-nums text-positive">{participantCount.toLocaleString()}</span>
            <span className="text-sm">{participantCount === 1 ? 'participant registered so far' : 'participants registered so far'}</span>
          </p>
        )}
      </div>
      <div className="grid gap-4 p-5 sm:grid-cols-2 sm:p-7">
        <div className="border border-brand-border/20 bg-app-bg/40 p-4">
          <p className="text-xs font-semibold uppercase tracking-wider text-content-tertiary">Public X identity</p>
          {profileUrl && handle ? <a href={profileUrl} target="_blank" rel="noreferrer" className="mt-1 inline-block font-semibold text-brand-peach hover:underline">@{handle.replace(/^@/, '')} ↗</a> : <p className="mt-1">—</p>}
        </div>
        <div className="border border-brand-border/20 bg-app-bg/40 p-4">
          <p className="text-xs font-semibold uppercase tracking-wider text-content-tertiary">Trading Account</p>
          <p className="mt-1 font-mono text-sm" title={registration.wallet?.tradingAccount}>{registration.wallet ? shortAddress(registration.wallet.tradingAccount) : '—'}</p>
        </div>
      </div>
      <div className="flex flex-wrap gap-3 px-5 pb-6 sm:px-7">
        <a href={TRADING_APP_URL} className={PRIMARY_BUTTON}>Open Plether testnet ↗</a>
        <Link to="/" className="border border-brand-border/40 px-5 py-2.5 text-sm font-semibold hover:border-brand-peach">View leaderboard</Link>
      </div>
    </Panel>
  )
}

function RegistrationFlow({ slug, competition }: { slug: string; competition: Competition }) {
  const queryClient = useQueryClient()
  const sessionQuery = useRegistrationSession(slug)
  const [turnstileToken, setTurnstileToken] = useState<string | null>(null)
  const [turnstileReset, setTurnstileReset] = useState(0)
  const [pendingAction, setPendingAction] = useState<string | null>(null)
  const [actionError, setActionError] = useState<string | null>(null)
  const [acceptRules, setAcceptRules] = useState(false)
  const [acceptPrivacy, setAcceptPrivacy] = useState(false)
  const [editingWallet, setEditingWallet] = useState(false)
  const registration = sessionQuery.data
  const completed = completedStepCount(registration)
  const metadata = competition.registration
  // The backend/database clock is authoritative for the half-open window.
  // Client clocks are used only for presentation, never authorization gates.
  const registrationOpen = metadata?.status === 'open'
  const visibleActionError = actionError
    ?? (registration?.oauthErrorCode ? registrationErrorCodeMessage(registration.oauthErrorCode) : null)

  const updateRegistration = useCallback((next: RegistrationSession) => {
    queryClient.setQueryData(queryKeys.registration(slug), next)
    setActionError(null)
  }, [queryClient, slug])

  const replaceWallet = useCallback((next: RegistrationSession) => {
    updateRegistration(next)
    setEditingWallet(false)
  }, [updateRegistration])

  const receiveTurnstileToken = useCallback((token: string | null) => {
    setTurnstileToken(token)
  }, [])

  async function startRegistration() {
    if (!turnstileToken) return
    setPendingAction('session')
    setActionError(null)
    try {
      updateRegistration(await createRegistrationSession(slug, turnstileToken))
    } catch (caught) {
      setActionError(registrationErrorMessage(caught))
      setTurnstileToken(null)
      setTurnstileReset((value) => value + 1)
    } finally {
      setPendingAction(null)
    }
  }

  async function connectX() {
    if (!registration) return
    setPendingAction('x')
    setActionError(null)
    try {
      const authorizationUrl = await createXAuthorization(slug, registration.csrfToken)
      window.location.assign(
        localDevelopmentAuthorizationUrl(authorizationUrl, slug)
          ?? safeXAuthorizationUrl(authorizationUrl),
      )
    } catch (caught) {
      setActionError(registrationErrorMessage(caught))
      setPendingAction(null)
    }
  }

  async function verifyXFollow() {
    if (!registration) return
    setPendingAction('follow')
    setActionError(null)
    try {
      updateRegistration(await confirmXFollow(slug, registration.csrfToken))
    } catch (caught) {
      setActionError(registrationErrorMessage(caught))
      // Refresh the lease-backed server state: a retryable provider failure
      // may stay on this step, while an expired credential returns to X auth.
      await sessionQuery.refetch()
    } finally {
      setPendingAction(null)
    }
  }

  async function finishRegistration() {
    if (!registration || !acceptRules || !acceptPrivacy) return
    setPendingAction('complete')
    setActionError(null)
    try {
      const { rulesVersion, privacyVersion } = registration.requiredConsents
      const completedRegistration = await completeRegistration(
        slug,
        registration.csrfToken,
        rulesVersion,
        privacyVersion,
      )
      updateRegistration(completedRegistration)
      await Promise.all([
        queryClient.invalidateQueries({ queryKey: queryKeys.competition }),
        queryClient.invalidateQueries({ queryKey: queryKeys.status }),
      ])
    } catch (caught) {
      setActionError(registrationErrorMessage(caught))
      if (caught instanceof InsightsApiError && caught.code === 'TRADING_ACCOUNT_EXISTS') {
        // The account can become deployed or active between wallet proof and
        // the transactional completion recheck. Keep the flow resumable with
        // a different owner instead of trapping the participant on review.
        setEditingWallet(true)
        await sessionQuery.refetch()
      }
    } finally {
      setPendingAction(null)
    }
  }

  if (sessionQuery.isLoading) return <Panel><LoadingState rows={5} /></Panel>
  if (sessionQuery.isError && !isSessionMissing(sessionQuery.error)) {
    return <ErrorState title="Registration could not be loaded" message={registrationErrorMessage(sessionQuery.error)} onRetry={() => { void sessionQuery.refetch() }} />
  }
  if (registration?.status === 'completed' || registration?.steps.completed) {
    return (
      <div className="space-y-5">
        <StepRail completed={STEPS.length} />
        <Completion registration={registration} participantCount={competition.participantCount} />
      </div>
    )
  }
  if (!registrationOpen) return <RegistrationUnavailable competition={competition} />

  return (
    <div className="space-y-5">
      <StepRail completed={completed} />

      {visibleActionError ? <p className="border border-brand-orange/40 bg-brand-orange/10 p-3 text-sm text-brand-peach" role="alert">{visibleActionError}</p> : null}

      <Panel className="p-5 sm:p-7">
        {!registration ? (
          <div>
            <p className="text-xs font-semibold uppercase tracking-[0.16em] text-brand-peach">Step 1 of 5</p>
            <h2 className="mt-2 text-2xl font-semibold">Confirm you’re human</h2>
            <p className="mt-3 max-w-2xl text-sm leading-6 text-content-secondary">This one-time check protects the competition from automated and bulk registrations.</p>
            <div className="mt-5">
              <TurnstileWidget siteKey={TURNSTILE_SITE_KEY} onToken={receiveTurnstileToken} resetKey={turnstileReset} />
            </div>
            <button type="button" className={`mt-5 ${PRIMARY_BUTTON}`} disabled={!turnstileToken || pendingAction === 'session'} onClick={() => { void startRegistration() }}>
              {pendingAction === 'session' ? 'Starting…' : 'Start registration'}
            </button>
          </div>
        ) : registration.steps.xIdentity !== 'verified' ? (
          <div>
            <p className="text-xs font-semibold uppercase tracking-[0.16em] text-brand-peach">Step 2 of 5</p>
            <h2 className="mt-2 text-2xl font-semibold">Verify your X identity</h2>
            <p className="mt-3 max-w-2xl text-sm leading-6 text-content-secondary">
              Sign in with X so we can confirm your email and account age. Your handle will appear publicly; your confirmed email is encrypted and never shown on the leaderboard.
            </p>
            <p className="mt-2 text-xs text-content-tertiary">X accounts must be at least {String(metadata.minimumXAccountAgeDays)} days old.</p>
            <button type="button" className={`mt-5 ${PRIMARY_BUTTON}`} disabled={pendingAction === 'x'} onClick={() => { void connectX() }}>
              {pendingAction === 'x' ? 'Opening X…' : 'Continue with X'}
            </button>
          </div>
        ) : registration.steps.xFollow !== 'verified' ? (
          <div>
            <p className="text-xs font-semibold uppercase tracking-[0.16em] text-brand-peach">Step 3 of 5</p>
            <h2 className="mt-2 text-2xl font-semibold">Follow @{metadata.targetXHandle.replace(/^@/, '')}</h2>
            <p className="mt-3 max-w-2xl text-sm leading-6 text-content-secondary">
              Signed in as <a href={xProfileUrl(registration.identity?.xHandle) ?? '#'} target="_blank" rel="noreferrer" className="font-semibold text-brand-peach hover:underline">@{registration.identity?.xHandle.replace(/^@/, '')}</a> ({registration.identity?.maskedEmail}). Follow Plether on X yourself, then return here to verify it.
            </p>
            <div className="mt-5 flex flex-wrap gap-3">
              <a
                href={xProfileUrl(metadata.targetXHandle) ?? '#'}
                target="_blank"
                rel="noreferrer"
                className="border border-brand-border/40 px-5 py-2.5 text-sm font-semibold text-content-primary transition-colors hover:border-brand-peach"
              >
                Open @{metadata.targetXHandle.replace(/^@/, '')} on X ↗
              </a>
              <button type="button" className={PRIMARY_BUTTON} disabled={pendingAction === 'follow'} onClick={() => { void verifyXFollow() }}>
                {pendingAction === 'follow' ? 'Verifying follow…' : 'Verify follow'}
              </button>
            </div>
          </div>
        ) : registration.steps.wallet !== 'verified' ? (
          <div>
            <p className="text-xs font-semibold uppercase tracking-[0.16em] text-brand-peach">Step 4 of 5</p>
            <h2 className="mt-2 text-2xl font-semibold">Verify your wallet</h2>
            <div className="mt-4">
              <RegistrationWalletStep slug={slug} registration={registration} onVerified={updateRegistration} />
            </div>
          </div>
        ) : editingWallet ? (
          <div>
            <p className="text-xs font-semibold uppercase tracking-[0.16em] text-brand-peach">Step 4 of 5</p>
            <h2 className="mt-2 text-2xl font-semibold">Choose another wallet</h2>
            <div className="mt-4">
              <RegistrationWalletStep slug={slug} registration={registration} onVerified={replaceWallet} />
            </div>
            <button type="button" className="mt-4 border border-brand-border/40 px-4 py-2 text-sm font-semibold hover:border-brand-peach" onClick={() => { setEditingWallet(false) }}>
              Keep current wallet
            </button>
          </div>
        ) : (
          <div>
            <p className="text-xs font-semibold uppercase tracking-[0.16em] text-brand-peach">Step 5 of 5</p>
            <h2 className="mt-2 text-2xl font-semibold">Review your entry</h2>
            <dl className="mt-5 grid gap-px bg-brand-border/20 sm:grid-cols-2">
              <div className="bg-app-bg p-4"><dt className="text-xs uppercase tracking-wider text-content-tertiary">Public X handle</dt><dd className="mt-1 font-semibold">@{registration.identity?.xHandle.replace(/^@/, '')}</dd></div>
              <div className="bg-app-bg p-4"><dt className="text-xs uppercase tracking-wider text-content-tertiary">Confirmed email</dt><dd className="mt-1">{registration.identity?.maskedEmail}</dd></div>
              <div className="bg-app-bg p-4"><dt className="text-xs uppercase tracking-wider text-content-tertiary">Owner wallet (private)</dt><dd className="mt-1 font-mono text-sm" title={registration.wallet?.ownerAddress}>{registration.wallet ? shortAddress(registration.wallet.ownerAddress) : '—'}</dd></div>
              <div className="bg-app-bg p-4"><dt className="text-xs uppercase tracking-wider text-content-tertiary">Scored Trading Account</dt><dd className="mt-1 font-mono text-sm" title={registration.wallet?.tradingAccount}>{registration.wallet ? shortAddress(registration.wallet.tradingAccount) : '—'}</dd></div>
            </dl>
            <div className="mt-5 space-y-3 text-sm leading-6 text-content-secondary">
              <label className="flex items-start gap-3"><input type="checkbox" checked={acceptRules} onChange={(event) => { setAcceptRules(event.target.checked) }} className="mt-1 h-4 w-4 accent-brand-orange" /><span>I accept the <Link to="/methodology" className="font-semibold text-brand-peach hover:underline">competition rules</Link>, including the one-wallet and integrity-review requirements.</span></label>
              <label className="flex items-start gap-3"><input type="checkbox" checked={acceptPrivacy} onChange={(event) => { setAcceptPrivacy(event.target.checked) }} className="mt-1 h-4 w-4 accent-brand-orange" /><span>I accept the privacy notice: my X handle is public; my confirmed email is encrypted and retained indefinitely for competition integrity and duplicate prevention; the private owner-wallet-to-Trading-Account link is protected and retained indefinitely for integrity and scoring audits.</span></label>
            </div>
            <div className="mt-5">
              <button type="button" className={PRIMARY_BUTTON} disabled={!acceptRules || !acceptPrivacy || pendingAction === 'complete'} onClick={() => { void finishRegistration() }}>
                {pendingAction === 'complete' ? 'Completing registration…' : 'Complete registration'}
              </button>
            </div>
          </div>
        )}
      </Panel>

      <p className="text-xs leading-5 text-content-tertiary">Your secure registration session expires {registration ? formatUtc(registration.expiresAt) : 'after you begin'}. You can safely return in this browser before then to continue.</p>
    </div>
  )
}

export function RegistrationPage() {
  const { slug = '' } = useParams()
  const competitionQuery = useCurrentCompetition()
  const statusQuery = useInsightsStatus()
  const competition = competitionQuery.data

  if (competitionQuery.isLoading) return <Panel><LoadingState rows={6} /></Panel>
  if (competitionQuery.isError) return <ErrorState title="Competition data is unavailable" message={competitionQuery.error.message} onRetry={() => { void competitionQuery.refetch() }} />
  if (!competition) return <ErrorState title="Competition data is unavailable" />
  if (competition.slug !== slug) return <ErrorState title="Competition not found" message="Registration is available only for the current competition." />
  const competitionData = competition.participantCount === undefined && statusQuery.data?.participantCount !== undefined
    ? { ...competition, participantCount: statusQuery.data.participantCount }
    : competition

  return (
    <div className="mx-auto max-w-5xl space-y-6">
      <div>
        <p className="text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">September 2026 testnet competition</p>
        <h1 className="mt-3 text-3xl font-semibold sm:text-4xl">Register to compete</h1>
        <p className="mt-4 max-w-3xl text-sm leading-6 text-content-secondary sm:text-base">
          Verify one established X account and one unused owner wallet. Your deterministic Plether Trading Account becomes the address scored on the public leaderboard.
        </p>
      </div>
      <RegistrationFlow slug={slug} competition={competitionData} />
    </div>
  )
}

export function CurrentRegistrationRedirect() {
  const competition = useCurrentCompetition()
  const target = useMemo(
    () => competition.data ? `/competitions/${encodeURIComponent(competition.data.slug)}/register` : null,
    [competition.data],
  )

  if (competition.isLoading) return <Panel><LoadingState rows={3} /></Panel>
  if (competition.isError || !target) return <ErrorState title="Competition data is unavailable" message={competition.error?.message} onRetry={() => { void competition.refetch() }} />
  return <Navigate to={target} replace />
}
