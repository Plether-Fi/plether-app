import { act, render, screen } from '@testing-library/react'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { TurnstileWidget } from './TurnstileWidget'

function setTurnstileApi(api: {
  render: (container: HTMLElement, options: Record<string, unknown>) => string | null | undefined
  remove: (widgetId: string) => void
}) {
  Object.defineProperty(window, 'turnstile', { configurable: true, writable: true, value: api })
}

afterEach(() => {
  Reflect.deleteProperty(window, 'turnstile')
  document.getElementById('cloudflare-turnstile-script')?.remove()
})

describe('TurnstileWidget', () => {
  it('uses the backend-pinned action and clears expired tokens', () => {
    let options: Record<string, unknown> = {}
    const remove = vi.fn()
    setTurnstileApi({
      render: vi.fn((_container, nextOptions) => {
        options = nextOptions
        return 'widget-id'
      }),
      remove,
    })
    const onToken = vi.fn()

    const view = render(<TurnstileWidget siteKey="site-key" onToken={onToken} resetKey={0} />)

    expect(options).toMatchObject({
      sitekey: 'site-key',
      action: 'competition_registration',
      theme: 'dark',
    })
    act(() => { (options.callback as (token: string) => void)('verified-token') })
    expect(onToken).toHaveBeenCalledWith('verified-token')
    act(() => { (options['expired-callback'] as () => void)() })
    expect(onToken).toHaveBeenLastCalledWith(null)

    view.unmount()
    expect(remove).toHaveBeenCalledWith('widget-id')
  })

  it('fails visibly when the Cloudflare script cannot load', () => {
    const onToken = vi.fn()
    const existingScript = document.createElement('script')
    existingScript.id = 'cloudflare-turnstile-script'
    document.head.appendChild(existingScript)
    render(<TurnstileWidget siteKey="site-key" onToken={onToken} resetKey={0} />)

    const script = document.getElementById('cloudflare-turnstile-script')
    expect(script).toBeInstanceOf(HTMLScriptElement)
    act(() => { script?.dispatchEvent(new Event('error')) })

    expect(screen.getByRole('alert')).toHaveTextContent('could not load')
    expect(onToken).toHaveBeenCalledWith(null)
  })

  it.each([null, undefined])('fails visibly when Turnstile returns no widget ID (%s)', (widgetId) => {
    const onToken = vi.fn()
    const remove = vi.fn()
    setTurnstileApi({ render: () => widgetId, remove })
    const view = render(<TurnstileWidget siteKey="site-key" onToken={onToken} resetKey={0} />)
    expect(screen.getByRole('alert')).toHaveTextContent('could not load')
    expect(onToken).toHaveBeenCalledWith(null)
    view.unmount()
    expect(remove).not.toHaveBeenCalled()
  })
})
