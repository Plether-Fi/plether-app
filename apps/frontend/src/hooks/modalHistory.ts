const markerKey = '__plether_modal'
interface Marker { id: string; href: string }
interface Entry { close: () => unknown }
interface NavigationEntry { key: string; index: number; url: string | null }
interface BrowserNavigation {
  currentEntry?: NavigationEntry
  entries: () => NavigationEntry[]
}
function browserNavigation() {
  return (window as Window & { navigation?: BrowserNavigation }).navigation
}

function routeIndex(state: unknown): number | undefined {
  if (state && typeof state === 'object' && 'idx' in state && typeof state.idx === 'number') return state.idx
}

function marker(state: unknown): Marker | undefined {
  if (!state || typeof state !== 'object' || !(markerKey in state)) return
  const value: unknown = state[markerKey]
  if (value && typeof value === 'object' && 'id' in value && typeof value.id === 'string'
    && 'href' in value && typeof value.href === 'string') return value as Marker
}

// One same-URL history entry protects the entire modal stack. Back dismisses
// the top layer; the entry remains armed while an underlying modal is open.
export class ModalHistory {
  private entries = new Map<string, Entry>()
  private guard: Marker | undefined
  private guardBackKey: string | undefined
  private restoringKey: string | undefined
  private rearmingGuard: Marker | undefined
  private listening = false
  private scheduled: ReturnType<typeof setTimeout> | undefined
  private restoreHistory: (() => void)[] = []
  private routeIndex: number | undefined

  noteNavigation() {
    // A late route effect can run after the URL changes but before popstate.
    // Once initialized, only actual history writes/traversals update the index.
    if (this.listening) return
    window.addEventListener('popstate', this.onPopState, true)
    this.listening = true
    // React Router changes browser history before its route layout effect
    // runs. Observe those writes synchronously or a quick Back can compare
    // against the old route index and skip a modal entry in the wrong direction.
    const history = window.history
    for (const method of ['pushState', 'replaceState'] as const) {
      const original = history[method]
      const wrapped = (...args: Parameters<History['pushState']>) => {
        original.apply(history, args)
        this.captureNavigation()
      }
      history[method] = wrapped
      this.restoreHistory.push(() => {
        if (history[method] === wrapped) history[method] = original
      })
    }
    this.captureNavigation()
  }

  dispose() {
    // A development hot update must not leave an older listener consuming
    // Back before the new controller can dismiss its currently open dialog.
    window.removeEventListener('popstate', this.onPopState, true)
    this.restoreHistory.reverse().forEach(restore => { restore() })
    this.restoreHistory = []
    if (this.scheduled !== undefined) clearTimeout(this.scheduled)
    this.scheduled = undefined
    this.entries.clear()
    this.guard = undefined
    this.guardBackKey = undefined
    this.restoringKey = undefined
    this.rearmingGuard = undefined
    this.listening = false
  }

  private captureNavigation() {
    const state: unknown = window.history.state
    this.routeIndex = routeIndex(state)
    if (!this.entries.size) { this.guard = marker(state); this.guardBackKey = undefined }
    else if (marker(state)?.href !== window.location.href) this.sync()
  }

  register(id: string, close: () => unknown) {
    if (!this.listening) this.noteNavigation()
    const entry = { close }
    this.entries.set(id, entry)
    this.sync()
    return () => {
      if (this.entries.get(id) !== entry) return
      this.entries.delete(id)
      this.sync()
    }
  }

  private schedule() {
    if (this.scheduled !== undefined) return
    this.scheduled = setTimeout(() => { this.scheduled = undefined; this.sync() }, 0)
  }

  private sync() {
    // A traversal back to the existing guard is already in flight. Creating
    // another entry here can make Chrome skip the whole stack on toolbar Back.
    if (this.rearmingGuard) return
    const current = marker(window.history.state)
    if (this.entries.size) {
      // URL cleanup can preserve history.state while changing the URL. That
      // entry no longer protects this page: reuse only a same-URL guard.
      if (current?.href === window.location.href) {
        if (this.guard?.id !== current.id || !this.guardBackKey) {
          const navigation = browserNavigation()
          const index = navigation?.currentEntry?.index
          const previous = navigation?.entries().find(entry => entry.index === (index ?? 0) - 1)
          this.guardBackKey = previous?.url === current.href ? previous.key : undefined
        }
        this.guard = current
        return
      }
      const state: unknown = window.history.state
      this.guardBackKey = browserNavigation()?.currentEntry?.key
      this.guard = { id: crypto.randomUUID(), href: window.location.href }
      window.history.pushState({ ...(state && typeof state === 'object' ? state : {}), [markerKey]: this.guard }, '', window.location.href)
    } else this.guard = current
  }

  private dismissTop(consumedGuard: Marker | undefined) {
    const top = Array.from(this.entries.entries()).at(-1)
    if (!top) return
    this.entries.delete(top[0])
    if (top[1].close() === false) this.entries.set(...top)
    if (this.entries.size && consumedGuard) {
      // After Back, Chrome no longer honors the earlier click's activation
      // for new pushState entries. Reuse the existing forward entry so another
      // Back dismisses the parent instead of skipping out of the app.
      this.rearmingGuard = consumedGuard
      window.history.forward()
    } else this.schedule()
  }

  private restoreSkippedGuard(event: PopStateEvent) {
    const navigation = browserNavigation()
    const index = navigation?.currentEntry?.index
    const target = navigation?.entries().find(entry => entry.key === this.guardBackKey)
    if (!target || index === undefined || target.index <= index || target.url !== this.guard?.href) return false
    // Browser toolbar/gesture Back can skip synthetic entries even though
    // history.back() visits them. Return to the exact underlying page entry,
    // then dismiss one layer without letting React Router change pages.
    event.stopImmediatePropagation()
    this.restoringKey = target.key
    window.history.go(target.index - index)
    return true
  }

  private onPopState = (event: PopStateEvent) => {
    if (this.rearmingGuard) {
      const rearmed = marker(event.state)?.id === this.rearmingGuard.id
      this.guard = this.rearmingGuard
      this.rearmingGuard = undefined
      if (rearmed) {
        event.stopImmediatePropagation()
        this.routeIndex = routeIndex(event.state as unknown)
        return
      }
    }
    if (this.restoringKey) {
      const restored = browserNavigation()?.currentEntry?.key === this.restoringKey
      this.restoringKey = undefined
      if (restored) {
        event.stopImmediatePropagation()
        const consumedGuard = this.guard
        this.guard = marker(event.state)
        this.routeIndex = routeIndex(event.state as unknown)
        this.dismissTop(consumedGuard)
        return
      }
    }
    const previousRouteIndex = this.routeIndex
    if (this.guard && marker(event.state)?.id !== this.guard.id) {
      const consumedGuard = this.guard
      const samePage = this.guard.href === window.location.href
      if (!samePage && this.entries.size && this.restoreSkippedGuard(event)) return
      this.guard = undefined
      if (samePage) {
        // React Router must not treat this modal dismissal as page navigation.
        event.stopImmediatePropagation()
        const top = Array.from(this.entries.entries()).at(-1)
        if (top) {
          // A SDK exit animation can outlive dismissal. Do not rearm history
          // for that closing layer while waiting for its React cleanup.
          this.dismissTop(consumedGuard)
        } else {
          // Closing with X leaves a reusable same-URL entry. Consume it and
          // the underlying duplicate in this one Back action, without racing
          // an asynchronous history.back() against a quick reopen.
          window.history.back()
        }
        return
      }
      Array.from(this.entries.values()).reverse().forEach(entry => { entry.close() })
    }
    const stale = marker(event.state)
    if (stale) {
      // A route change or reload can leave an old modal entry behind. Skip it
      // instead of reopening a modal or adding a duplicate Back step.
      event.stopImmediatePropagation()
      const targetIndex = routeIndex(event.state as unknown)
      const forward = targetIndex !== undefined && previousRouteIndex !== undefined && targetIndex >= previousRouteIndex
      this.guard = forward ? stale : undefined
      if (forward) window.history.forward()
      else window.history.back()
      return
    }
    // Record a real traversal before React renders, including immediately
    // reversing direction through a closed modal entry with Back then Forward.
    this.captureNavigation()
  }
}
