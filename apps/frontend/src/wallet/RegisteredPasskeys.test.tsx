import { act, fireEvent, render, screen, within } from '@testing-library/react'
import type { PasskeyWithMetadata } from '@privy-io/react-auth'
import { afterEach, beforeEach, expect, it, vi } from 'vitest'
import { RegisteredPasskeys } from './RegisteredPasskeys'

const mac: PasskeyWithMetadata = {
  type: 'passkey', credentialId: 'public-mac-key', enrolledInMfa: true,
  authenticatorName: 'iCloud Keychain', createdWithDevice: 'Desktop', createdWithOs: 'macOS', createdWithBrowser: 'Chrome',
  firstVerifiedAt: new Date('2026-11-07T08:00:00Z'), latestVerifiedAt: new Date('2026-11-08T08:00:00Z'),
}
const mobile: PasskeyWithMetadata = {
  ...mac, credentialId: 'public-mobile-key', enrolledInMfa: false,
  authenticatorName: 'Google Password Manager', createdWithDevice: 'Mobile', createdWithOs: 'Android',
}

const macChrome = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/142.0.0.0 Safari/537.36'
const androidChrome = 'Mozilla/5.0 (Linux; Android 10; K) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/142.0.0.0 Mobile Safari/537.36'
beforeEach(() => { window.localStorage.clear(); vi.spyOn(navigator, 'userAgent', 'get').mockReturnValue(macChrome) })
afterEach(() => { vi.restoreAllMocks() })

it('shows registration metadata and the MFA status of each individual passkey', () => {
  render(<RegisteredPasskeys passkeys={[mac, mobile]} mfaEnabled />)
  const rows = within(screen.getByRole('list', { name: 'Registered passkeys' })).getAllByRole('listitem')
  expect(rows).toHaveLength(2)
  expect(within(rows[0]).getByText('Registered on macOS · Chrome')).toBeVisible()
  expect(within(rows[0]).getByText('iCloud Keychain')).toBeVisible()
  expect(within(rows[0]).getByText('Wallet MFA enabled')).toBeVisible()
  expect(within(rows[1]).getByText('Registered on Android · Chrome')).toBeVisible()
  expect(within(rows[1]).getByText('Google Password Manager')).toBeVisible()
  expect(within(rows[1]).getByText('Not enabled for wallet MFA')).toBeVisible()
  // Registration date, not the last login date; no credential IDs or public keys.
  expect(rows[0].querySelector('time')).toHaveAttribute('datetime', mac.firstVerifiedAt!.toISOString())
  expect(screen.queryByText(mac.credentialId)).not.toBeInTheDocument()
  expect(within(rows[0]).getByText('Registration matches this browser')).toBeVisible()
  expect(within(rows[1]).queryByText('Registration matches this browser')).not.toBeInTheDocument()
})

it('does not claim a passkey is enabled when passkey MFA is disabled for the account', () => {
  render(<RegisteredPasskeys passkeys={[mac]} mfaEnabled={false} />)
  expect(screen.getByText('Not enabled for wallet MFA')).toBeVisible()
  expect(screen.queryByText('Wallet MFA enabled')).not.toBeInTheDocument()
})

it('uses authenticator and numbered fallbacks when device metadata is unavailable', () => {
  const old: PasskeyWithMetadata = {
    type: 'passkey', credentialId: 'public-older-key', enrolledInMfa: false,
    firstVerifiedAt: null, latestVerifiedAt: null,
  }
  render(<RegisteredPasskeys passkeys={[
    { ...old, authenticatorName: 'Security key', createdWithDevice: ' ', createdWithBrowser: '' },
    { ...old, credentialId: 'public-oldest-key', firstVerifiedAt: new Date('invalid') },
  ]} mfaEnabled />)
  expect(screen.getByText('Security key')).toBeVisible()
  expect(screen.getByText('Passkey 2')).toBeVisible()
  expect(screen.getAllByText('Registration device unavailable')).toHaveLength(2)
  expect(document.querySelector('time')).toBeNull()
})

it('updates the list when a passkey is enrolled, removed, or linked on another device', () => {
  const view = render(<RegisteredPasskeys passkeys={[mac, mobile]} mfaEnabled />)
  view.rerender(<RegisteredPasskeys passkeys={[{ ...mobile, enrolledInMfa: true }]} mfaEnabled />)
  expect(screen.queryByText('Registered on macOS · Chrome')).not.toBeInTheDocument()
  expect(screen.getAllByRole('listitem')).toHaveLength(1)
  expect(screen.getByText('Wallet MFA enabled')).toBeVisible()
  expect(screen.queryByText('Not enabled for wallet MFA')).not.toBeInTheDocument()
  view.rerender(<RegisteredPasskeys passkeys={[]} mfaEnabled />)
  expect(screen.getByText('No passkeys registered yet.')).toBeVisible()
  expect(screen.queryByRole('list')).not.toBeInTheDocument()
})

it('hides Chrome’s frozen Android model token and highlights the Android registration on mobile', () => {
  vi.spyOn(navigator, 'userAgent', 'get').mockReturnValue(androidChrome)
  render(<RegisteredPasskeys passkeys={[
    { ...mac, createdWithDevice: 'Macintosh', createdWithOs: 'Mac OS' },
    { ...mobile, createdWithDevice: 'K' },
  ]} mfaEnabled />)
  const rows = screen.getAllByRole('listitem')
  expect(within(rows[0]).getByText('Registered on macOS · Chrome')).toBeVisible()
  expect(within(rows[0]).queryByText('Registration matches this browser')).not.toBeInTheDocument()
  expect(within(rows[1]).getByText('Registered on Android · Chrome')).toBeVisible()
  expect(within(rows[1]).getByText('Registration matches this browser')).toBeVisible()
  expect(screen.queryByText(/^K ·/)).not.toBeInTheDocument()
})

it.each([
  { ua: macChrome.replace('Chrome/142.0.0.0', 'Firefox/144.0'), metadata: mac },
  { ua: 'Unknown browser', metadata: mac },
  { ua: macChrome, metadata: { ...mac, createdWithOs: undefined } },
  { ua: macChrome, metadata: { ...mac, createdWithBrowser: undefined } },
])('does not guess a match for a different browser or missing metadata ($ua)', ({ ua, metadata }) => {
  vi.spyOn(navigator, 'userAgent', 'get').mockReturnValue(ua)
  render(<RegisteredPasskeys passkeys={[metadata]} mfaEnabled />)
  expect(screen.queryByText('Registration matches this browser')).not.toBeInTheDocument()
})

it('keeps informative model names and highlights all matching OS/browser registrations', () => {
  render(<RegisteredPasskeys passkeys={[
    mac, { ...mac, credentialId: 'another-mac', createdWithDevice: 'MacBook Pro' },
    { ...mobile, createdWithDevice: 'Pixel 8' },
  ]} mfaEnabled />)
  expect(screen.getByText('Registered on MacBook Pro · macOS · Chrome')).toBeVisible()
  expect(screen.getByText('Registered on Pixel 8 · Android · Chrome')).toBeVisible()
  expect(screen.getAllByText('Registration matches this browser')).toHaveLength(2)
})

it('saves a custom name without replacing the explicit registration details', async () => {
  const rename = vi.fn().mockResolvedValue(null)
  const security = { ...mac, authenticatorName: undefined, createdWithBrowser: 'Firefox' }
  const view = render(<RegisteredPasskeys passkeys={[security]} mfaEnabled names={{}} onRename={rename} />)
  fireEvent.click(screen.getByRole('button', { name: 'Rename passkey', exact: true }))
  expect(screen.getByText(/Name syncs across your devices/)).toBeVisible()
  expect(screen.getByRole('textbox', { name: 'Passkey name' })).toHaveValue('Passkey 1')
  fireEvent.change(screen.getByRole('textbox', { name: 'Passkey name' }), { target: { value: 'YubiKey' } })
  await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Save', exact: true })) })
  expect(rename).toHaveBeenCalledExactlyOnceWith(mac.credentialId, 'YubiKey')
  view.rerender(<RegisteredPasskeys passkeys={[security]} mfaEnabled names={{ [mac.credentialId]: 'YubiKey' }} onRename={rename} />)
  expect(screen.getByText('YubiKey', { exact: true })).toBeVisible()
  expect(screen.getByText('Registered on macOS · Firefox')).toBeVisible()
})

it('can cancel an edit or clear a custom name to restore the default', async () => {
  const rename = vi.fn().mockResolvedValue(null)
  render(<RegisteredPasskeys passkeys={[mac]} mfaEnabled names={{ [mac.credentialId]: 'YubiKey' }} onRename={rename} />)
  fireEvent.click(screen.getByRole('button', { name: 'Rename passkey', exact: true }))
  expect(screen.getByRole('textbox', { name: 'Passkey name' })).toHaveValue('YubiKey')
  fireEvent.change(screen.getByRole('textbox', { name: 'Passkey name' }), { target: { value: 'Not saved' } })
  fireEvent.click(screen.getByRole('button', { name: 'Cancel', exact: true }))
  expect(rename).not.toHaveBeenCalled()
  expect(screen.getByText('YubiKey', { exact: true })).toBeVisible()
  fireEvent.click(screen.getByRole('button', { name: 'Rename passkey', exact: true }))
  fireEvent.change(screen.getByRole('textbox', { name: 'Passkey name' }), { target: { value: '' } })
  await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Save', exact: true })) })
  expect(rename).toHaveBeenCalledExactlyOnceWith(mac.credentialId, '')
})

it('keeps a failed save in the editor and permits retry', async () => {
  const rename = vi.fn().mockResolvedValueOnce('Could not save the name. Try again.').mockResolvedValue(null)
  render(<RegisteredPasskeys passkeys={[mac]} mfaEnabled onRename={rename} />)
  fireEvent.click(screen.getByRole('button', { name: 'Rename passkey', exact: true }))
  fireEvent.change(screen.getByRole('textbox', { name: 'Passkey name' }), { target: { value: 'YubiKey' } })
  await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Save', exact: true })) })
  expect(screen.getByRole('alert')).toHaveTextContent('Could not save the name')
  expect(screen.getByRole('textbox', { name: 'Passkey name' })).toHaveValue('YubiKey')
  await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Save', exact: true })) })
  expect(screen.queryByRole('textbox')).not.toBeInTheDocument()
})

it('disables rename during an MFA update and renders labels as text', () => {
  render(<RegisteredPasskeys passkeys={[mac]} mfaEnabled disabled names={{ [mac.credentialId]: '<script>My key</script>' }} onRename={vi.fn()} />)
  expect(screen.getByRole('button', { name: 'Rename passkey', exact: true })).toBeDisabled()
  expect(screen.getByText('<script>My key</script>', { exact: true })).toBeVisible()
  expect(document.querySelector('script')).toBeNull()
})

it('does not read inherited object properties as custom names', () => {
  render(<RegisteredPasskeys passkeys={[{ ...mac, credentialId: 'toString' }]} mfaEnabled names={{}} />)
  expect(screen.getByText('iCloud Keychain', { exact: true })).toBeVisible()
})
