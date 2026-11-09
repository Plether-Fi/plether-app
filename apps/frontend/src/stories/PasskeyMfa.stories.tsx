import { useState } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import type { PasskeyWithMetadata } from '@privy-io/react-auth'
import { expect, userEvent, within } from 'storybook/test'
import { Modal } from '../components/ui/Modal'
import { Button } from '../components/ui/Button'
import { WalletMfaSettingsView } from '../wallet/WalletMfaSettingsView'
import type { PasskeyNames } from '../wallet/passkeyNames'

const yubikeyId = 'storybook-yubikey'
const fixtures: PasskeyWithMetadata[] = [
  { type: 'passkey', credentialId: 'storybook-mac', enrolledInMfa: true, authenticatorName: 'Google Password Manager',
    createdWithOs: 'macOS', createdWithBrowser: 'Chrome', firstVerifiedAt: new Date('2026-11-07T08:45:00Z'), latestVerifiedAt: null },
  { type: 'passkey', credentialId: 'storybook-phone', enrolledInMfa: true, authenticatorName: 'Google Password Manager',
    createdWithOs: 'Android', createdWithBrowser: 'Chrome', firstVerifiedAt: new Date('2026-11-07T14:21:00Z'), latestVerifiedAt: null },
  { type: 'passkey', credentialId: yubikeyId, enrolledInMfa: false,
    createdWithOs: 'macOS', createdWithBrowser: 'Firefox', firstVerifiedAt: new Date('2026-11-09T10:13:00Z'), latestVerifiedAt: null },
]
interface PreviewProps { start: 'ready' | 'verification' | 'enabled'; narrow: boolean }

function MfaPreviewState({ start, narrow }: PreviewProps) {
  const [passkeys, setPasskeys] = useState(() => fixtures.map(key => ({ ...key, enrolledInMfa: key.enrolledInMfa || (start === 'enabled' && key.credentialId === yubikeyId) })))
  const [names, setNames] = useState<PasskeyNames>({ 'storybook-mac': 'Mac passkey', 'storybook-phone': 'Phone passkey', [yubikeyId]: 'YubiKey' })
  const [pending, setPending] = useState<string | null>(start === 'verification' ? yubikeyId : null)
  const [selectedKey, setSelectedKey] = useState<string | null>(start === 'ready' ? null : yubikeyId)
  const [updatedKey, setUpdatedKey] = useState<string | null>(start === 'enabled' ? yubikeyId : null)
  const [error, setError] = useState<string | null>(null)
  const [info, setInfo] = useState<'settings' | 'link' | null>(null)
  const enable = (credentialId: string) => {
    setSelectedKey(credentialId)
    setUpdatedKey(null)
    setError(null)
    setPending(credentialId)
  }
  const verify = () => {
    setPasskeys(keys => keys.map(key => key.credentialId === pending ? { ...key, enrolledInMfa: true } : key))
    setUpdatedKey(pending)
    setPending(null)
  }
  return <main className="min-h-screen bg-app-bg p-4 sm:p-8">
    <div className={`mx-auto ${narrow ? 'max-w-[360px]' : 'max-w-[480px]'}`}>
      <p className="mb-4 text-xs text-content-secondary">Storybook preview · verification is simulated. No account settings are changed.</p>
      <section aria-label="Wallet connected preview" className="border border-brand-border/30 bg-surface-panel p-4 sm:p-6">
        <h2 className="mb-5 text-xl font-semibold">Wallet connected</h2>
        <div className="bg-app-bg/40 p-4 text-sm">
          <p className="font-medium">Privy Wallet <span className="float-right text-xs text-content-secondary">Arbitrum Sepolia</span></p>
          <p className="mt-2 font-mono text-content-secondary">0x3f7…3642</p>
        </div>
        <WalletMfaSettingsView passkeys={passkeys} mfaEnabled labels={{ names, saving: false, error: null,
          retry: () => {}, rename: async (id, name) => { setNames(previous => ({ ...previous, [id]: name.trim() })); return null } }}
          updating={pending !== null} selectedKey={selectedKey} updatedKey={updatedKey} error={error}
          onEnablePasskey={enable} onOpenSettings={() => { setInfo('settings') }} onAddPasskey={() => { setInfo('link') }} />
      </section>
      <p className="mt-4 text-sm text-content-secondary">Choose <strong>Enable for wallet MFA</strong> on YubiKey, then simulate verification with an existing enabled passkey. The Mac and phone keys stay enabled.</p>
    </div>
    <Modal isOpen={pending !== null} onClose={() => { setPending(null) }} title="Verify with an existing MFA method"
      footer={<div className="flex flex-wrap gap-3">
        <Button variant="secondary" onClick={() => { setPending(null) }}>Cancel</Button>
        <Button variant="danger" onClick={verify}>Simulate verification</Button>
      </div>}>
      <span aria-hidden="true" className="material-symbols-outlined mb-4 text-4xl text-brand">security</span>
      <p>Use an already enabled passkey, such as your Mac or phone passkey, to allow this YubiKey for wallet MFA.</p>
      <p className="mt-4 text-sm text-content-secondary">The live app opens Privy’s verification prompt and your device’s passkey dialog. This preview simulates the result.</p>
      <button type="button" className="mt-5 min-h-11 text-sm text-brand underline" onClick={() => {
        setError('Could not complete the wallet MFA update. Verify with an existing MFA method and try again.')
        setPending(null)
      }}>Simulate verification failure</button>
    </Modal>
    <Modal isOpen={info !== null} onClose={() => { setInfo(null) }} title={info === 'link' ? 'Add passkey' : 'Wallet MFA settings'}
      footer={<Button variant="secondary" onClick={() => { setInfo(null) }}>Back to wallet</Button>}>
      <p>{info === 'link'
        ? 'In the live app, Privy registers a new login passkey here. This preview already includes a registered YubiKey; choose Enable for wallet MFA on its row to enroll it.'
        : 'Passkey MFA is enabled. The live app opens Privy’s settings for managing verification methods. Use the action on a registered passkey to enable that particular key.'}</p>
    </Modal>
  </main>
}

function MfaPreview(props: PreviewProps) { return <MfaPreviewState key={`${props.start}:${String(props.narrow)}`} {...props} /> }

const meta = {
  title: 'Wallet/Passkey MFA', component: MfaPreview,
  parameters: { layout: 'fullscreen', docs: { description: { component: 'Enable a registered YubiKey for wallet MFA after verifying an existing method. Uses the production wallet MFA view with sample data and simulated verification; no Privy account or backend is contacted by the demo.' } } },
  args: { start: 'ready', narrow: false },
  argTypes: { start: { options: ['ready', 'verification', 'enabled'], control: 'select' } },
} satisfies Meta<typeof MfaPreview>
export default meta
type Story = StoryObj<typeof meta>

function keyRow(root: HTMLElement, name: string) {
  const title = within(root).getByText(name, { exact: true })
  const row = title.closest('li')
  if (!row) throw new Error(`Passkey row not found: ${name}`)
  return within(row)
}

export const Interactive: Story = {
  play: async ({ canvasElement }) => {
    const button = keyRow(canvasElement, 'YubiKey').getByRole('button', { name: 'Enable for wallet MFA' })
    button.scrollIntoView({ block: 'nearest' })
    await expect(button).toBeEnabled()
  },
}
export const Verification: Story = { args: { start: 'verification' } }
export const Enabled: Story = {
  play: async ({ canvasElement }) => {
    await userEvent.click(keyRow(canvasElement, 'YubiKey').getByRole('button', { name: 'Enable for wallet MFA' }))
    await userEvent.click(within(canvasElement.ownerDocument.body).getByRole('button', { name: 'Simulate verification' }))
    for (const name of ['Mac passkey', 'Phone passkey', 'YubiKey']) {
      await expect(keyRow(canvasElement, name).getByText('Wallet MFA enabled', { exact: true })).toBeVisible()
    }
    await expect(keyRow(canvasElement, 'YubiKey').queryByRole('button', { name: 'Enable for wallet MFA' })).not.toBeInTheDocument()
  },
}
export const FailureAndRetry: Story = {
  play: async ({ canvasElement }) => {
    await userEvent.click(keyRow(canvasElement, 'YubiKey').getByRole('button', { name: 'Enable for wallet MFA' }))
    await userEvent.click(within(canvasElement.ownerDocument.body).getByRole('button', { name: 'Simulate verification failure' }))
    await expect(keyRow(canvasElement, 'YubiKey').getByRole('alert')).toBeVisible()
    await expect(keyRow(canvasElement, 'YubiKey').getByRole('button', { name: 'Enable for wallet MFA' })).toBeEnabled()
  },
}
export const CancelVerification: Story = {
  play: async ({ canvasElement }) => {
    await userEvent.click(keyRow(canvasElement, 'YubiKey').getByRole('button', { name: 'Enable for wallet MFA' }))
    await userEvent.click(within(canvasElement.ownerDocument.body).getByRole('button', { name: 'Cancel' }))
    await expect(keyRow(canvasElement, 'YubiKey').getByText('Not enabled for wallet MFA', { exact: true })).toBeVisible()
    await expect(keyRow(canvasElement, 'YubiKey').queryByRole('alert')).not.toBeInTheDocument()
  },
}
export const Mobile: Story = { ...Interactive, args: { narrow: true } }
