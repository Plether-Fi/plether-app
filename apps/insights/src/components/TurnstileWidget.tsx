import { useEffect, useId, useRef, useState } from 'react'

const TURNSTILE_SCRIPT_ID = 'cloudflare-turnstile-script'
const TURNSTILE_SCRIPT_URL = 'https://challenges.cloudflare.com/turnstile/v0/api.js?render=explicit'

interface TurnstileApi {
  render: (container: HTMLElement, options: Record<string, unknown>) => string | null | undefined
  remove: (widgetId: string) => void
}

function getTurnstileApi(): TurnstileApi | undefined {
  // Keep this widget's minimal script contract local. Wallet SDKs may also
  // install Turnstile and declare a larger Window.turnstile interface.
  return (window as unknown as { turnstile?: TurnstileApi }).turnstile
}

export function TurnstileWidget({
  siteKey,
  onToken,
  resetKey,
}: {
  siteKey: string
  onToken: (token: string | null) => void
  resetKey: number
}) {
  const reactId = useId()
  const containerRef = useRef<HTMLDivElement>(null)
  const [error, setError] = useState<string | null>(null)

  useEffect(() => {
    const container = containerRef.current
    if (!container || siteKey.length === 0) return

    let disposed = false
    let widgetId: string | undefined
    let script: HTMLScriptElement | null = null

    const reportScriptError = () => {
      if (disposed) return
      onToken(null)
      setError('The spam-protection check could not load. Refresh the page and try again.')
    }

    const renderWidget = () => {
      const turnstile = getTurnstileApi()
      if (disposed || widgetId || !turnstile || !containerRef.current) return
      setError(null)
      widgetId = turnstile.render(containerRef.current, {
        sitekey: siteKey,
        action: 'competition_registration',
        theme: 'dark',
        callback: (token: string) => { onToken(token) },
        'expired-callback': () => { onToken(null) },
        'error-callback': () => {
          onToken(null)
          setError('The spam-protection check could not load. Refresh the page and try again.')
        },
      }) ?? undefined
      if (!widgetId) reportScriptError()
    }

    if (getTurnstileApi()) {
      renderWidget()
    } else {
      script = document.getElementById(TURNSTILE_SCRIPT_ID) as HTMLScriptElement | null
      if (!script) {
        script = document.createElement('script')
        script.id = TURNSTILE_SCRIPT_ID
        script.src = TURNSTILE_SCRIPT_URL
        script.async = true
        script.defer = true
        document.head.appendChild(script)
      }
      script.addEventListener('load', renderWidget)
      script.addEventListener('error', reportScriptError)
    }

    return () => {
      disposed = true
      script?.removeEventListener('load', renderWidget)
      script?.removeEventListener('error', reportScriptError)
      onToken(null)
      if (widgetId) getTurnstileApi()?.remove(widgetId)
    }
  }, [onToken, reactId, resetKey, siteKey])

  if (siteKey.length === 0) {
    return (
      <p className="border border-brand-orange/40 bg-brand-orange/10 p-3 text-sm text-brand-peach" role="alert">
        Registration spam protection is not configured.
      </p>
    )
  }

  return (
    <div>
      <div ref={containerRef} id={`turnstile-${reactId.replace(/:/g, '')}`} className="min-h-16" />
      {error ? <p className="mt-2 text-sm text-brand-peach" role="alert">{error}</p> : null}
    </div>
  )
}
