// WebAuthn requires a domain RP ID, even when a loopback IP is a secure context.
export function isPasskeyOriginSupported(location: Pick<Location, 'protocol' | 'hostname'> = window.location): boolean {
  const host = location.hostname
  if (/^\d+\.\d+\.\d+\.\d+$/.test(host) || host.includes(':')) return false
  return location.protocol === 'https:' || (location.protocol === 'http:'
    && (host === 'localhost' || host.endsWith('.localhost')))
}

export function localhostPasskeyUrl(location: Pick<Location, 'hostname' | 'port' | 'pathname'> = window.location): string | undefined {
  if (!/^127(?:\.\d+){3}$/.test(location.hostname) && location.hostname !== '[::1]') return undefined
  // Start a fresh login on the valid origin; never copy OAuth callback parameters.
  return `http://localhost${location.port ? `:${location.port}` : ''}${location.pathname}`
}
