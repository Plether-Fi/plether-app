import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { ModalHistory } from '../modalHistory'

// Real history changes its URL/state before React renders the new route.
// Deliver traversals synchronously and deliberately omit a React notification
// so this race is deterministic instead of depending on CI rendering speed.
function browserHistory() {
  const entries: { state: unknown; href: string; key: string }[] = [{ state: { idx: 0 }, href: 'http://localhost/privacy', key: 'entry-0' }]
  let nextKey = 1
  let cursor = 0
  const listeners: ((event: PopStateEvent) => void)[] = []
  const delivered: string[] = []
  let beforePop = () => {}
  const location = { href: entries[0].href }
  function traverse(delta: number) {
    if (!entries[cursor + delta]) return
    cursor += delta
    location.href = entries[cursor].href
    beforePop()
    const event = new PopStateEvent('popstate', { state: entries[cursor].state })
    const stop = vi.spyOn(event, 'stopImmediatePropagation')
    for (const listener of listeners) {
      listener(event)
      if (stop.mock.calls.length) break
    }
    if (!stop.mock.calls.length) delivered.push(location.href)
  }
  const history = {
    get state() { return entries[cursor].state },
    pushState(state: unknown, _unused: string, url?: string | URL | null) {
      expect(this).toBe(history)
      const href = url ? new URL(url, location.href).href : location.href
      entries.splice(cursor + 1, entries.length, { state, href, key: `entry-${String(nextKey++)}` })
      cursor += 1
      location.href = href
    },
    replaceState(state: unknown, _unused: string, url?: string | URL | null) {
      expect(this).toBe(history)
      entries[cursor] = { ...entries[cursor], state, href: url ? new URL(url, location.href).href : location.href }
      location.href = entries[cursor].href
    },
    back() { traverse(-1) },
    forward() { traverse(1) },
    go(delta: number) { traverse(delta) },
  }
  vi.stubGlobal('window', {
    history, location,
    navigation: {
      get currentEntry() { return { key: entries[cursor].key, index: cursor, url: entries[cursor].href } },
      entries: () => entries.map((entry, index) => ({ key: entry.key, index, url: entry.href })),
    },
    addEventListener: (_type: string, callback: (event: PopStateEvent) => void) => { listeners.push(callback) },
    removeEventListener: (_type: string, callback: (event: PopStateEvent) => void) => {
      const index = listeners.indexOf(callback)
      if (index >= 0) listeners.splice(index, 1)
    },
  })
  return { history, location, delivered, beforePop: (callback: () => void) => { beforePop = callback } }
}

beforeEach(() => { vi.useFakeTimers() })
afterEach(() => { vi.useRealTimers(); vi.unstubAllGlobals() })

describe('modal history before React route effects run', () => {
  it('skips a closed modal backward and forward even when the latest route has not rendered', () => {
    const browser = browserHistory()
    const modals = new ModalHistory()
    modals.noteNavigation()
    const close = vi.fn()
    const unregister = modals.register('wallet', close)
    unregister() // X closes the modal, leaving its reusable history entry.
    browser.history.pushState({ idx: 1 }, '', '/terms')
    // No noteNavigation/React layout effect between pushState and Back.
    browser.history.back()
    expect(browser.location.href).toBe('http://localhost/privacy')
    expect(browser.delivered).toEqual(['http://localhost/privacy'])
    // Also omit the route effect between the two traversals.
    browser.history.forward()
    expect(browser.location.href).toBe('http://localhost/terms')
    expect(browser.delivered).toEqual(['http://localhost/privacy', 'http://localhost/terms'])
    expect(close).not.toHaveBeenCalled()
  })

  it('tracks history replacement before traversing a closed modal', () => {
    const browser = browserHistory()
    const modals = new ModalHistory()
    modals.noteNavigation()
    const unregister = modals.register('wallet', vi.fn())
    unregister()
    browser.history.pushState({ idx: 1 }, '', '/terms')
    browser.history.replaceState({ idx: 1, key: 'replaced' }, '', '/terms?updated=1')
    browser.history.back()
    expect(browser.location.href).toBe('http://localhost/privacy')
    browser.history.forward()
    expect(browser.location.href).toBe('http://localhost/terms?updated=1')
  })

  it('ignores a late route effect between a traversal changing the URL and delivering popstate', () => {
    const browser = browserHistory()
    const modals = new ModalHistory()
    modals.noteNavigation()
    modals.register('wallet', vi.fn())()
    browser.history.pushState({ idx: 1 }, '', '/terms')
    browser.beforePop(() => { modals.noteNavigation() })
    browser.history.back()
    expect(browser.location.href).toBe('http://localhost/privacy')
    browser.history.forward()
    expect(browser.location.href).toBe('http://localhost/terms')
  })

  it('closes only the top modal and rearms Back for its parent', () => {
    const browser = browserHistory()
    const modals = new ModalHistory()
    const parent = vi.fn()
    const child = vi.fn()
    modals.register('wallet', parent)
    modals.register('mfa', child)
    browser.history.back()
    expect(child).toHaveBeenCalledOnce()
    expect(parent).not.toHaveBeenCalled()
    expect(browser.delivered).toEqual([])
    vi.runAllTimers()
    browser.history.back()
    expect(parent).toHaveBeenCalledOnce()
    expect(browser.location.href).toBe('http://localhost/privacy')
  })

  it('does not reuse a closed modal entry after its URL has been replaced', () => {
    const browser = browserHistory()
    const modals = new ModalHistory()
    modals.register('wallet', vi.fn())()
    // An SDK can clean OAuth/query parameters while preserving history.state.
    browser.history.replaceState(browser.history.state, '', '/privacy?cleaned=1')
    const close = vi.fn()
    modals.register('wallet', close)
    browser.history.back()
    expect(close).toHaveBeenCalledOnce()
    expect(browser.location.href).toBe('http://localhost/privacy?cleaned=1')
    expect(browser.delivered).toEqual([])
  })

  it.each([true, false])('keeps an open modal protected when URL cleanup preserves history state: %s', preserveState => {
    const browser = browserHistory()
    const modals = new ModalHistory()
    const close = vi.fn()
    modals.register('wallet', close)
    browser.history.replaceState(preserveState ? browser.history.state : { idx: 0 }, '', '/privacy?cleaned=1')
    browser.history.back()
    expect(close).toHaveBeenCalledOnce()
    expect(browser.location.href).toBe('http://localhost/privacy?cleaned=1')
    expect(browser.delivered).toEqual([])
  })

  it('releases its listener and history wrappers before a hot replacement', () => {
    const browser = browserHistory()
    const originalPush = browser.history.pushState
    const originalReplace = browser.history.replaceState
    const oldModals = new ModalHistory()
    const oldClose = vi.fn()
    oldModals.register('wallet', oldClose)()
    oldModals.dispose()
    expect(browser.history.pushState).toBe(originalPush)
    expect(browser.history.replaceState).toBe(originalReplace)
    const newModals = new ModalHistory()
    const close = vi.fn()
    newModals.register('wallet', close)
    browser.history.back()
    expect(close).toHaveBeenCalledOnce()
    expect(oldClose).not.toHaveBeenCalled()
    expect(browser.delivered).toEqual([])
  })

  it.each([false, true])('dismisses only the top modal when browser Back skips its entry (nested: %s)', nested => {
    const browser = browserHistory()
    const modals = new ModalHistory()
    modals.noteNavigation()
    browser.history.pushState({ idx: 1 }, '', '/terms')
    const parent = vi.fn()
    const child = vi.fn()
    const unregister = modals.register('wallet', parent)
    if (nested) modals.register('mfa', child)
    // Unlike history.back(), a browser's toolbar can skip both the guard and
    // the underlying same-document entry in a single traversal.
    browser.history.go(-2)
    expect(browser.location.href).toBe('http://localhost/terms')
    expect(browser.delivered).toEqual([])
    if (nested) {
      expect(child).toHaveBeenCalledOnce()
      expect(parent).not.toHaveBeenCalled()
      vi.runAllTimers()
      browser.history.go(-2)
    }
    expect(parent).toHaveBeenCalledOnce()
    unregister()
    vi.runAllTimers()
    browser.history.back()
    expect(browser.location.href).toBe('http://localhost/privacy')
    expect(browser.delivered).toEqual(['http://localhost/privacy'])
  })

  it('keeps normal modal dismissal working without the Navigation API', () => {
    const browser = browserHistory()
    Reflect.deleteProperty(window, 'navigation')
    const modals = new ModalHistory()
    const close = vi.fn()
    modals.register('wallet', close)
    browser.history.back()
    expect(close).toHaveBeenCalledOnce()
    expect(browser.location.href).toBe('http://localhost/privacy')
    expect(browser.delivered).toEqual([])
  })

  it('handles another Back press before a nested dialog’s guard traversal completes', () => {
    const browser = browserHistory()
    const modals = new ModalHistory()
    modals.noteNavigation()
    browser.history.pushState({ idx: 1 }, '', '/terms')
    const parent = vi.fn()
    const child = vi.fn()
    modals.register('wallet', parent)
    modals.register('mfa', child)
    vi.spyOn(browser.history, 'forward').mockImplementationOnce(() => {})
    browser.history.back()
    expect(child).toHaveBeenCalledOnce()
    expect(parent).not.toHaveBeenCalled()
    browser.history.back()
    expect(parent).toHaveBeenCalledOnce()
    expect(browser.location.href).toBe('http://localhost/terms')
    expect(browser.delivered).toEqual([])
  })

  it('reuses the original guard without pushing another entry when a nested dialog closes', () => {
    const browser = browserHistory()
    const modals = new ModalHistory()
    modals.register('wallet', vi.fn())
    modals.register('mfa', vi.fn())
    const originalState = browser.history.state
    const push = vi.spyOn(browser.history, 'pushState')
    browser.history.back()
    modals.dispose()
    vi.runAllTimers()
    expect(push).not.toHaveBeenCalled()
    expect(browser.history.state).toEqual(originalState)
    expect(browser.location.href).toBe('http://localhost/privacy')
  })

  it('retains a modal that cannot be dismissed and does not navigate away', () => {
    const browser = browserHistory()
    const modals = new ModalHistory()
    const close = vi.fn(() => false)
    modals.register('signing', close)
    browser.history.back()
    vi.runAllTimers()
    browser.history.back()
    expect(close).toHaveBeenCalledTimes(2)
    expect(browser.delivered).toEqual([])
    expect(browser.location.href).toBe('http://localhost/privacy')
  })
})
