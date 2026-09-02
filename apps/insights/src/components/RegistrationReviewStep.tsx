import { Link } from 'react-router-dom'
import type { RegistrationSession } from '../api'
import { shortAddress } from '../utils/format'

const PRIMARY_BUTTON = 'border border-brand-orange bg-brand-orange px-5 py-2.5 text-sm font-semibold text-content-primary transition-colors hover:bg-brand-peach hover:text-app-bg disabled:cursor-not-allowed disabled:opacity-50'

interface RegistrationReviewStepProps {
  registration: RegistrationSession
  acceptRules: boolean
  acceptPrivacy: boolean
  acceptPromotionalEmail: boolean
  isCompleting: boolean
  onAcceptRulesChange: (accepted: boolean) => void
  onAcceptPrivacyChange: (accepted: boolean) => void
  onAcceptPromotionalEmailChange: (accepted: boolean) => void
  onComplete: () => void
}

export function RegistrationReviewStep({
  registration,
  acceptRules,
  acceptPrivacy,
  acceptPromotionalEmail,
  isCompleting,
  onAcceptRulesChange,
  onAcceptPrivacyChange,
  onAcceptPromotionalEmailChange,
  onComplete,
}: RegistrationReviewStepProps) {
  return (
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
        <label className="flex items-start gap-3"><input type="checkbox" checked={acceptRules} onChange={(event) => { onAcceptRulesChange(event.target.checked) }} className="mt-1 h-4 w-4 accent-brand-orange" /><span className="font-semibold text-content-primary">I accept the <Link to="/methodology" className="text-brand-peach hover:underline">competition rules</Link>, including the one-wallet and integrity-review requirements.</span></label>
        <div className="flex items-start gap-3">
          <input id="accept-registration-privacy" type="checkbox" checked={acceptPrivacy} onChange={(event) => { onAcceptPrivacyChange(event.target.checked) }} aria-describedby="registration-privacy-details" className="mt-1 h-4 w-4 shrink-0 accent-brand-orange" />
          <div>
            <label htmlFor="accept-registration-privacy" className="font-semibold text-content-primary">I accept the privacy notice:</label>
            <ul id="registration-privacy-details" className="mt-1 list-disc space-y-1" style={{ paddingInlineStart: '1rem' }}>
              <li>My X handle will be public.</li>
              <li>The private owner-wallet-to-Trading-Account link is protected and retained indefinitely for integrity and scoring audits.</li>
            </ul>
          </div>
        </div>
        <label className="flex items-start gap-3">
          <input
            type="checkbox"
            checked={acceptPromotionalEmail}
            onChange={(event) => { onAcceptPromotionalEmailChange(event.target.checked) }}
            className="mt-1 h-4 w-4 shrink-0 accent-brand-orange"
          />
          <span>
            Email me Plether Labs newsletters, product updates and competition news. <span className="text-content-tertiary">Optional; unsubscribe at any time.</span>
          </span>
        </label>
      </div>
      <div className="mt-5">
        <button type="button" className={PRIMARY_BUTTON} disabled={!acceptRules || !acceptPrivacy || isCompleting} onClick={onComplete}>
          {isCompleting ? 'Completing registration…' : 'Complete registration'}
        </button>
      </div>
    </div>
  )
}
