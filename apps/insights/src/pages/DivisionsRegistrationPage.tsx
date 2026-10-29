import { useAppKit } from '@reown/appkit/react'
import { useCallback, useEffect, useRef, useState } from 'react'
import { Link } from 'react-router-dom'
import { useConnection, useSignMessage, useSwitchChain } from 'wagmi'
import { arbitrumSepolia } from 'wagmi/chains'
import { InsightsApiError } from '../api'
import { TurnstileWidget } from '../components/TurnstileWidget'
import { ErrorState, LoadingState, Panel } from '../components/ui'
import { walletConnectionConfigured } from '../config/wagmi'
import {
  completeDivisionsRegistration, createDiscordAuthorization,
  createDivisionsRegistrationSession, createDivisionsWalletChallenge,
  getDivisionsRegistrationSession, safeDiscordAuthorizationUrl,
  verifyDivisionsWallet, type DivisionsRegistrationSession,
} from '../divisions/registration'
import { formatUtc, shortAddress } from '../utils/format'
import { registrationErrorMessage, registrationNeedsRecovery } from '../utils/registration'

const siteKey = typeof import.meta.env.VITE_TURNSTILE_SITE_KEY === 'string'
  ? import.meta.env.VITE_TURNSTILE_SITE_KEY.trim() : ''
const TURNSTILE_SITE_KEY = siteKey || (import.meta.env.DEV ? '1x00000000000000000000AA' : '')
const PRIMARY_BUTTON = 'border border-brand-orange bg-brand-orange px-5 py-2.5 text-sm font-semibold text-content-primary transition-colors hover:bg-brand-peach hover:text-app-bg disabled:cursor-not-allowed disabled:opacity-50'
const SECONDARY_BUTTON = 'border border-brand-border/40 px-5 py-2.5 text-sm font-semibold text-content-primary transition-colors hover:border-brand-peach disabled:cursor-not-allowed disabled:opacity-50'
const STEPS = ['Spam protection', 'Discord identity', 'Wallet ownership', 'Review'] as const

function localDevelopmentDiscordUrl(value: string): string | null {
  if (!import.meta.env.DEV) return null
  const url = new URL(value)
  if (url.origin !== window.location.origin
    || url.pathname !== '/api/insights/v1/divisions/registrations/discord/callback'
    || url.search !== '?mock=1' || url.hash) return null
  return url.toString()
}

function StepRail({ completed }: { completed: number }) {
  return <ol className="grid grid-cols-4 gap-2" aria-label="Registration progress">
    {STEPS.map((step, index) => <li key={step} aria-current={index === completed ? 'step' : undefined}
      className={`border-t-2 pt-3 text-xs font-semibold ${index < completed ? 'border-positive text-content-primary' : index === completed ? 'border-brand-peach text-brand-peach' : 'border-brand-border/30 text-content-tertiary'}`}>
      <span className="block font-mono">{String(index + 1).padStart(2, '0')}</span>{step}
    </li>)}
  </ol>
}

function WalletStep({ session, onVerified }: { session: DivisionsRegistrationSession; onVerified: (value: DivisionsRegistrationSession) => void }) {
  const { address, chainId, isConnected } = useConnection()
  const { open } = useAppKit()
  const { mutateAsync: switchChain } = useSwitchChain()
  const { mutateAsync: signMessage } = useSignMessage()
  const [pending, setPending] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const currentWallet = useRef({ address, chainId })

  useEffect(() => { currentWallet.current = { address, chainId } }, [address, chainId])

  async function verify() {
    if (!address) return
    setPending(true)
    setError(null)
    try {
      if (chainId !== arbitrumSepolia.id) {
        await switchChain({ chainId: arbitrumSepolia.id })
        return
      }
      const owner = address.toLowerCase()
      const challenge = await createDivisionsWalletChallenge(session.csrfToken, owner)
      const signature = await signMessage({ message: challenge.message })
      if (currentWallet.current.address?.toLowerCase() !== owner || currentWallet.current.chainId !== arbitrumSepolia.id) {
        throw new Error('The connected wallet or network changed. Start wallet verification again.')
      }
      onVerified(await verifyDivisionsWallet(session.csrfToken, owner, signature))
    } catch (caught) {
      setError(registrationErrorMessage(caught))
      if (registrationNeedsRecovery(caught)) {
        try { onVerified(await getDivisionsRegistrationSession()) } catch { /* Keep the actionable error above. */ }
      }
    } finally {
      setPending(false)
    }
  }

  if (!walletConnectionConfigured) return <p role="alert" className="text-brand-peach">Wallet connection is not configured for this deployment.</p>

  return <div className="space-y-4">
    <p className="text-sm leading-6 text-content-secondary">Connect the owner wallet you use for Plether. You will sign a one-time message; no transaction or funds are involved.</p>
    {isConnected && address ? <div className="flex flex-wrap items-center justify-between gap-4 border border-brand-border/25 bg-app-bg/50 p-4">
      <div><p className="text-xs text-content-tertiary">Connected owner wallet</p><p className="mt-1 font-mono text-sm" title={address}>{shortAddress(address)}</p>
        <p className="mt-1 text-xs text-content-tertiary">{chainId === arbitrumSepolia.id ? 'Arbitrum Sepolia' : 'Network switch required'}</p></div>
      <button type="button" className={PRIMARY_BUTTON} disabled={pending} onClick={() => { void verify() }}>
        {pending ? 'Checking wallet…' : chainId === arbitrumSepolia.id ? 'Sign and verify wallet' : 'Switch to Arbitrum Sepolia'}
      </button>
    </div> : <button type="button" className={PRIMARY_BUTTON} onClick={() => { void open({ view: 'Connect' }) }}>Connect wallet</button>}
    {error && <p role="alert" className="border border-brand-orange/40 bg-brand-orange/10 p-3 text-sm text-brand-peach">{error}</p>}
  </div>
}

export function DivisionsRegistrationPage() {
  const [session, setSession] = useState<DivisionsRegistrationSession | null>(null)
  const [loading, setLoading] = useState(true)
  const [loadError, setLoadError] = useState<string | null>(null)
  const [actionError, setActionError] = useState<string | null>(null)
  const [pending, setPending] = useState<'session' | 'discord' | 'complete' | null>(null)
  const [turnstileToken, setTurnstileToken] = useState<string | null>(null)
  const [turnstileReset, setTurnstileReset] = useState(0)
  const [acceptRules, setAcceptRules] = useState(false)
  const [acceptPrivacy, setAcceptPrivacy] = useState(false)

  const load = useCallback(async () => {
    setLoading(true)
    setLoadError(null)
    try { setSession(await getDivisionsRegistrationSession()) }
    catch (caught) {
      if (caught instanceof InsightsApiError && caught.status === 401) setSession(null)
      else setLoadError(registrationErrorMessage(caught))
    } finally { setLoading(false) }
  }, [])
  useEffect(() => { queueMicrotask(() => { void load() }) }, [load])

  async function start() {
    if (!turnstileToken) return
    setPending('session')
    setActionError(null)
    try { setSession(await createDivisionsRegistrationSession(turnstileToken)) }
    catch (caught) {
      setActionError(registrationErrorMessage(caught))
      setTurnstileToken(null)
      setTurnstileReset((value) => value + 1)
    } finally { setPending(null) }
  }

  async function connectDiscord() {
    if (!session) return
    setPending('discord')
    setActionError(null)
    try {
      const url = await createDiscordAuthorization(session.csrfToken)
      window.location.assign(localDevelopmentDiscordUrl(url) ?? safeDiscordAuthorizationUrl(url))
    } catch (caught) {
      setActionError(registrationErrorMessage(caught))
      setPending(null)
    }
  }

  async function complete() {
    if (!session || !acceptRules || !acceptPrivacy) return
    setPending('complete')
    setActionError(null)
    try { setSession(await completeDivisionsRegistration(session)) }
    catch (caught) {
      setActionError(registrationErrorMessage(caught))
      if (registrationNeedsRecovery(caught)) await load()
    } finally { setPending(null) }
  }

  const completed = !session ? 0 : session.status === 'completed' || session.steps.completed ? 4
    : session.steps.wallet === 'verified' ? 3 : session.steps.discord === 'verified' ? 2 : 1

  return <div className="mx-auto max-w-5xl space-y-6">
    <header><p className="text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">Plether Divisions</p>
      <h1 className="mt-3 text-3xl font-semibold sm:text-4xl">Link Discord and wallet</h1>
      <p className="mt-4 max-w-3xl text-sm leading-6 text-content-secondary sm:text-base">Connect one Discord account to the owner wallet behind your Plether Trading Account. This prepares your account for future Discord roles. Every traded wallet can appear in Divisions rankings, with or without a link.</p>
    </header>
    <StepRail completed={completed} />
    {loading ? <Panel><LoadingState rows={4} /></Panel> : loadError ?
      <ErrorState title="Registration could not be loaded" message={loadError} onRetry={() => { void load() }} /> :
      <Panel className="p-5 sm:p-7">
        {actionError && <p role="alert" className="mb-5 border border-brand-orange/40 bg-brand-orange/10 p-3 text-sm text-brand-peach">{actionError}</p>}
        {!session ? <div><p className="text-xs font-semibold uppercase tracking-widest text-brand-peach">Step 1 of 4</p>
          <h2 className="mt-2 text-2xl font-semibold">Confirm you’re human</h2>
          <p className="mt-3 text-sm leading-6 text-content-secondary">This one-time check protects Discord and wallet linking from automated registrations.</p>
          <div className="mt-5"><TurnstileWidget siteKey={TURNSTILE_SITE_KEY} onToken={setTurnstileToken} resetKey={turnstileReset} /></div>
          <button type="button" className={`mt-5 ${PRIMARY_BUTTON}`} disabled={!turnstileToken || pending === 'session'} onClick={() => { void start() }}>{pending === 'session' ? 'Starting…' : 'Start registration'}</button>
        </div> : completed === 4 ? <div><p className="text-xs font-semibold uppercase tracking-widest text-positive">Link complete</p>
          <h2 className="mt-2 text-2xl font-semibold">Discord and wallet linked</h2>
          <p className="mt-3 text-sm leading-6 text-content-secondary">Your Discord account <strong className="text-content-primary">{session.identity?.username}</strong> is linked to owner wallet <span className="font-mono text-content-primary">{session.wallet && shortAddress(session.wallet.ownerAddress)}</span>. Your Discord handle appears beside your Trading Account on the leaderboard. This link does not change ranking eligibility. Discord roles will become available separately.</p>
          <div className="mt-5 flex flex-wrap gap-3"><Link to="/" className={PRIMARY_BUTTON}>View Divisions</Link><a href="https://plether.com/discord" target="_blank" rel="noreferrer" className={SECONDARY_BUTTON}>Join Discord ↗</a></div>
        </div> : completed === 1 ? <div><p className="text-xs font-semibold uppercase tracking-widest text-brand-peach">Step 2 of 4</p>
          <h2 className="mt-2 text-2xl font-semibold">Sign in with Discord</h2>
          <p className="mt-3 max-w-2xl text-sm leading-6 text-content-secondary">Discord will share your account ID and username. We use the ID to keep each account linked to one wallet. We do not request your email or permission to post.</p>
          <button type="button" className={`mt-5 ${PRIMARY_BUTTON}`} disabled={pending === 'discord'} onClick={() => { void connectDiscord() }}>{pending === 'discord' ? 'Opening Discord…' : 'Continue with Discord'}</button>
        </div> : completed === 2 ? <div><p className="text-xs font-semibold uppercase tracking-widest text-brand-peach">Step 3 of 4</p>
          <h2 className="mt-2 text-2xl font-semibold">Verify your wallet</h2>
          <p className="mt-3 text-sm text-content-secondary">Discord connected as <strong className="text-content-primary">{session.identity?.username}</strong>.</p>
          <div className="mt-5"><WalletStep session={session} onVerified={setSession} /></div>
        </div> : <div><p className="text-xs font-semibold uppercase tracking-widest text-brand-peach">Step 4 of 4</p>
          <h2 className="mt-2 text-2xl font-semibold">Review your link</h2>
          <dl className="mt-5 grid gap-px bg-brand-border/20 sm:grid-cols-2">
            <div className="bg-app-bg p-4"><dt className="text-xs text-content-tertiary">Discord account</dt><dd className="mt-1 font-semibold">{session.identity?.username}</dd></div>
            <div className="bg-app-bg p-4"><dt className="text-xs text-content-tertiary">Owner wallet</dt><dd className="mt-1 font-mono text-sm" title={session.wallet?.ownerAddress}>{session.wallet && shortAddress(session.wallet.ownerAddress)}</dd></div>
            <div className="bg-app-bg p-4 sm:col-span-2"><dt className="text-xs text-content-tertiary">Plether Trading Account</dt><dd className="mt-1 font-mono text-sm" title={session.wallet?.tradingAccount}>{session.wallet && shortAddress(session.wallet.tradingAccount)}</dd></div>
          </dl>
          <div className="mt-5 space-y-3 text-sm leading-6 text-content-secondary">
            <label className="flex items-start gap-3"><input type="checkbox" checked={acceptRules} onChange={(event) => { setAcceptRules(event.target.checked) }} className="mt-1 accent-brand-orange" /><span>I accept the <Link to="/methodology" className="text-brand-peach underline">Divisions rules</Link> and the one Discord account per wallet linking rule.</span></label>
            <label className="flex items-start gap-3"><input type="checkbox" checked={acceptPrivacy} onChange={(event) => { setAcceptPrivacy(event.target.checked) }} className="mt-1 accent-brand-orange" /><span>I understand Plether stores my Discord ID and username with the owner wallet and Trading Account for role linking. Linking does not automatically assign a role. Registering makes my Discord handle public beside my Trading Account on the leaderboard.</span></label>
          </div>
          <button type="button" className={`mt-5 ${PRIMARY_BUTTON}`} disabled={!acceptRules || !acceptPrivacy || pending === 'complete'} onClick={() => { void complete() }}>{pending === 'complete' ? 'Completing…' : 'Complete registration'}</button>
        </div>}
      </Panel>}
    {session && completed < 4 && <p className="text-xs text-content-tertiary">Your registration session expires {formatUtc(session.expiresAt)}. You can return in this browser before then to continue.</p>}
  </div>
}
