// Keep enough stack information to identify UI failures, never serialize the
// original exception/cause (RPC errors can embed keys and signed operations).
export function sanitizeExceptionText(value: string): string {
  return value
    .replace(/https?:\/\/[^\s)]+/gi, '[url-redacted]')
    .replace(/\bBearer\s+\S+/gi, 'Bearer [redacted]')
    .replace(/\b(?:token|secret|password|signature|authorization|calldata|body|params)\s*[=:]\s*.*/gi, '[sensitive details redacted]')
    .replace(/0x[a-f0-9]+/gi, '[hex-redacted]')
    .replace(/\bph[ctx]_[\w-]+\b/g, '[credential-redacted]')
    .replace(/[^\s@]+@[^\s@]+\.[^\s@]+/g, '[email-redacted]')
    .replace(/\b[A-Za-z0-9_-]{32,}\b/g, '[identifier-redacted]')
    .slice(0, 2048)
}

function sanitizeStack(stack: string): string {
  // Retain only frame lines, and only static application asset URLs. Drop the
  // message line and all other text, including provider error diagnostics.
  return stack.split('\n').filter(line => /^\s*(at |[\w.$]+@)/.test(line)).slice(0, 30).map(line => {
    const assets: string[] = []
    const masked = line.replace(/https?:\/\/[^\s)]+/gi, raw => {
      const location = /^(https?:\/\/[^\s]+?)(:\d+:\d+)?$/.exec(raw)
      try {
        const url = new URL(location?.[1] ?? raw)
        if (url.origin === globalThis.location.origin && /^\/assets\/[\w.-]+\.js$/.test(url.pathname) && !url.search && !url.hash) {
          assets.push(`${url.origin}${url.pathname}${location?.[2] ?? ''}`)
          return `STACKASSET${String(assets.length - 1)}END`
        }
      } catch { /* Keep only validated application URLs. */ }
      return '[url-redacted]'
    })
    return sanitizeExceptionText(masked).replace(/STACKASSET(\d+)END/g, (_, index) => assets[Number(index)])
  }).join('\n').slice(0, 8192)
}

export function sanitizedReactException(error: unknown, componentStack?: string | null) {
  const safe = new Error(error instanceof Error ? sanitizeExceptionText(error.message.split('\n')[0]) : 'Non-Error React exception')
  if (error instanceof Error) {
    safe.name = /^(Error|TypeError|RangeError|SyntaxError|ReferenceError|URIError|EvalError|DOMException|NotFoundError)$/.test(error.name) ? error.name : 'Error'
    safe.stack = `${safe.name}: ${safe.message}\n${sanitizeStack(error.stack ?? '')}`
  } else safe.stack = undefined
  return { error: safe, componentStack: sanitizeStack(componentStack ?? '') }
}
