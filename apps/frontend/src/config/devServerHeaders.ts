/** Vite's server headers are global; path-specific Cloudflare rules are not. */
export function parseGlobalHeaders(contents: string): Record<string, string> {
  const headers: Record<string, string> = {}
  let route: string | undefined
  for (const line of contents.split(/\r?\n/)) {
    const trimmed = line.trim()
    if (!trimmed || trimmed.startsWith('#')) continue
    if (/^\S/.test(line)) {
      route = trimmed
      continue
    }
    if (route !== '/*') continue
    const match = /^\s+([A-Za-z-]+):\s*(.+)$/.exec(line)
    if (match) headers[match[1]] = match[2]
  }
  return headers
}
