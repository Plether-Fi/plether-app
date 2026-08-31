import type { Meta, StoryObj } from '@storybook/react-vite'
import { MemoryRouter } from 'react-router-dom'
import type { Competition, RegistrationSession } from '../../../insights/src/api'
import { RegistrationConfirmation } from '../../../insights/src/pages/RegistrationPage'
import '../../../insights/src/index.css'

const competition: Competition = {
  id: 'competition-2026-09',
  slug: 'testnet-trading-2026-09',
  name: 'Plether September 2026 Testnet Trading Competition',
  status: 'scheduled',
  startsAt: '2026-09-13T21:00:00Z',
  tradingCutoffAt: '2026-09-25T21:00:00Z',
  resultsAt: '2026-09-28T12:00:00Z',
  startingBalance: '100000000000',
  pnlEligibilityThreshold: '1000000',
  minActiveDays: 5,
  prizes: [
    { place: 1, amount: '600000000' },
    { place: 2, amount: '500000000' },
    { place: 3, amount: '400000000' },
    { place: 4, amount: '300000000' },
    { place: 5, amount: '200000000' },
  ],
  latestIndexedBlock: null,
  latestIndexedAt: null,
  participantCount: 127,
}

const registration: RegistrationSession = {
  status: 'completed',
  csrfToken: 'storybook-csrf-token',
  expiresAt: '2026-09-01T12:00:00Z',
  steps: {
    xIdentity: 'verified',
    xFollow: 'verified',
    wallet: 'verified',
    completed: true,
  },
  requiredConsents: {
    rulesVersion: '2026-09-13',
    privacyVersion: '2026-09-13',
  },
  identity: {
    xHandle: 'plether_local_tester',
    maskedEmail: 't***@example.test',
  },
  wallet: {
    ownerAddress: '0x1111111111111111111111111111111111111111',
    tradingAccount: '0x8b3b735d1f629943362f2ae67bac89996571a7cc',
  },
}

const meta = {
  title: 'Insights/Competition registration/Confirmation',
  component: RegistrationConfirmation,
  decorators: [
    (Story) => (
      <MemoryRouter>
        <main className="min-h-screen bg-app-bg px-4 py-8 text-content-primary sm:px-8">
          <div className="mx-auto max-w-5xl">
            <Story />
          </div>
        </main>
      </MemoryRouter>
    ),
  ],
  parameters: {
    layout: 'fullscreen',
  },
  args: {
    competition,
    registration,
  },
} satisfies Meta<typeof RegistrationConfirmation>

export default meta
type Story = StoryObj<typeof meta>

export const CompletedRegistration: Story = {}
