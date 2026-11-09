import type { PasskeyWithMetadata } from '@privy-io/react-auth'

type RegistrationDevice = Pick<PasskeyWithMetadata, 'createdWithDevice' | 'createdWithOs' | 'createdWithBrowser'>
interface BrowserDevice { os: string | undefined; browser: string | undefined }

function osFamily(value: string) {
  if (/android/i.test(value)) return 'Android'
  if (/\bios\b|iphone|ipad|ipod/i.test(value)) return 'iOS'
  if (/chrome\s?os|chromium os|cros/i.test(value)) return 'ChromeOS'
  if (/windows/i.test(value)) return 'Windows'
  if (/mac\s?os|macintosh|os x/i.test(value)) return 'macOS'
  if (/linux/i.test(value)) return 'Linux'
}

function browserFamily(value: string) {
  if (/edge|edg(?:a|ios)?\//i.test(value)) return 'Edge'
  if (/samsung/i.test(value)) return 'Samsung Internet'
  if (/opera|op(?:r|t|ios)\//i.test(value)) return 'Opera'
  if (/firefox|fxios/i.test(value)) return 'Firefox'
  if (/chrome|chromium|crios/i.test(value)) return 'Chrome'
  if (/safari/i.test(value)) return 'Safari'
}

export function currentBrowserDevice(userAgent: string): BrowserDevice {
  // Use only the ordinary OS/browser families. Never probe passkeys or request
  // high-entropy device identifiers to decide how to decorate a list entry.
  return { os: osFamily(userAgent), browser: browserFamily(userAgent) }
}

export function matchesBrowserDevice(passkey: RegistrationDevice, current: BrowserDevice) {
  return current.os !== undefined && current.browser !== undefined
    && osFamily(passkey.createdWithOs ?? '') === current.os
    && browserFamily(passkey.createdWithBrowser ?? '') === current.browser
}

export function passkeyDeviceLabel(passkey: RegistrationDevice) {
  const rawOs = passkey.createdWithOs?.trim() ?? ''
  const os = osFamily(rawOs) ?? rawOs
  const rawBrowser = passkey.createdWithBrowser?.trim() ?? ''
  const browser = browserFamily(rawBrowser) ?? rawBrowser
  const rawDevice = passkey.createdWithDevice?.trim() ?? ''
  // Chrome freezes Android's model token to "K". Generic device categories
  // such as Macintosh/Desktop add no information beyond the OS label.
  const genericDevice = /^(k|macintosh|mac|desktop|mobile|tablet|pc|unknown|other|generic)$/i.test(rawDevice)
    || (os !== '' && osFamily(rawDevice) === os)
  const device = genericDevice ? '' : rawDevice
  return [...new Set([device, os, browser].filter(Boolean))].join(' · ')
}
