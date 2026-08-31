import type { Meta, StoryObj } from '@storybook/react-vite'
import { expect, within } from 'storybook/test'
import { useState } from 'react'
import { MemoryRouter } from 'react-router-dom'
import type { RegistrationSession } from '../../../insights/src/api'
import { RegistrationReviewStep } from '../../../insights/src/components/RegistrationReviewStep'
import { Panel } from '../../../insights/src/components/ui'
import '../../../insights/src/index.css'

interface RegistrationReviewPreviewArgs {
  acceptRules: boolean
  acceptPrivacy: boolean
  acceptPromotionalEmail: boolean
}

const registration: RegistrationSession = {
  status: 'in_progress',
  csrfToken: 'storybook-csrf',
  expiresAt: '2026-09-20T21:00:00Z',
  steps: {
    xIdentity: 'verified',
    xFollow: 'verified',
    wallet: 'verified',
    completed: false,
  },
  requiredConsents: {
    rulesVersion: '2026-09-13',
    privacyVersion: '2026-09-13',
  },
  identity: {
    xHandle: 'profile_trader',
    maskedEmail: 'p***@example.com',
  },
  wallet: {
    ownerAddress: '0x1111111111111111111111111111111111111111',
    tradingAccount: '0x2222222222222222222222222222222222222222',
  },
}

function RegistrationReviewPreview(args: RegistrationReviewPreviewArgs) {
  const [acceptRules, setAcceptRules] = useState(args.acceptRules)
  const [acceptPrivacy, setAcceptPrivacy] = useState(args.acceptPrivacy)
  const [acceptPromotionalEmail, setAcceptPromotionalEmail] = useState(args.acceptPromotionalEmail)

  return (
    <MemoryRouter>
      <main className="min-h-screen bg-app-bg px-4 py-8 text-content-primary sm:px-8">
        <div className="mx-auto max-w-5xl space-y-6">
          <div>
            <p className="text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">September 2026 testnet competition</p>
            <h1 className="mt-3 text-3xl font-semibold sm:text-4xl">Register to compete</h1>
            <p className="mt-4 max-w-3xl text-sm leading-6 text-content-secondary sm:text-base">
              Verify one established X account and prove ownership of one wallet. Review the verified details and choose whether to receive promotional email before completing registration.
            </p>
          </div>
          <Panel className="p-5 sm:p-7">
            <RegistrationReviewStep
              registration={registration}
              acceptRules={acceptRules}
              acceptPrivacy={acceptPrivacy}
              acceptPromotionalEmail={acceptPromotionalEmail}
              isCompleting={false}
              onAcceptRulesChange={setAcceptRules}
              onAcceptPrivacyChange={setAcceptPrivacy}
              onAcceptPromotionalEmailChange={setAcceptPromotionalEmail}
              onComplete={() => undefined}
            />
          </Panel>
        </div>
      </main>
    </MemoryRouter>
  )
}

const meta = {
  title: 'Insights/Registration/Review step',
  component: RegistrationReviewPreview,
  parameters: {
    layout: 'fullscreen',
  },
  argTypes: {
    acceptRules: { control: 'boolean' },
    acceptPrivacy: { control: 'boolean' },
    acceptPromotionalEmail: { control: 'boolean' },
  },
} satisfies Meta<typeof RegistrationReviewPreview>

export default meta
type Story = StoryObj<typeof meta>

export const OptionalPromotionUnchecked: Story = {
  args: {
    acceptRules: true,
    acceptPrivacy: true,
    acceptPromotionalEmail: false,
  },
  play: ({ canvasElement }) => {
    const canvas = within(canvasElement)
    expect(canvas.getByRole('checkbox', { name: /Plether Labs newsletters/i })).not.toBeChecked()
    expect(canvas.getByRole('button', { name: 'Complete registration' })).toBeEnabled()
  },
}

export const PromotionalEmailOptedIn: Story = {
  args: {
    acceptRules: true,
    acceptPrivacy: true,
    acceptPromotionalEmail: true,
  },
  play: ({ canvasElement }) => {
    const canvas = within(canvasElement)
    expect(canvas.getByRole('checkbox', { name: /Plether Labs newsletters/i })).toBeChecked()
    expect(canvas.getByRole('button', { name: 'Complete registration' })).toBeEnabled()
  },
}
