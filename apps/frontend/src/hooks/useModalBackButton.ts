import { useId, useLayoutEffect, useRef } from 'react'

const markerKey = '__plether_modal'
interface Marker { id: string; href: string }
interface Entry { close: () => unknown }

function marker(state: unknown): Marker | undefined {
  if (!state || typeof state !== 'object' || !(markerKey in state)) return
  const value: unknown = state[markerKey]
  if (value && typeof value === 'object' && 'id' in value && typeof value.id === 'string'
    && 'href' in value && typeof value.href === 'string') return value as Marker
}

// One same-URL history entry protects the entire modal stack. Back dismisses
// the top layer; the entry remains armed while an underlying modal is open.
class ModalHistory {
  private entries = new Map<string, Entry>()
  private guard: Marker | undefined
  private listening = false
  private scheduled = false
  private routeIndex: number | undefined

  noteNavigation() {
    if (!this.listening) {
      window.addEventListener('popstate', this.onPopState, true)
      this.listening = true
    }
    const state: unknown = window.history.state
    if (state && typeof state === 'object' && 'idx' in state && typeof state.idx === 'number') this.routeIndex = state.idx
    if (!this.entries.size) this.guard = marker(state)
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
    if (this.scheduled) return
    this.scheduled = true
    setTimeout(() => { this.scheduled = false; this.sync() }, 0)
  }

  private sync() {
    const current = marker(window.history.state)
    if (this.entries.size) {
      if (current) { this.guard = current; return }
      const state: unknown = window.history.state
      this.guard = { id: crypto.randomUUID(), href: window.location.href }
      window.history.pushState({ ...(state && typeof state === 'object' ? state : {}), [markerKey]: this.guard }, '', window.location.href)
    } else this.guard = current
  }

  private onPopState = (event: PopStateEvent) => {
    if (this.guard && marker(event.state)?.id !== this.guard.id) {
      const samePage = this.guard.href === window.location.href
      this.guard = undefined
      if (samePage) {
        // React Router must not treat this modal dismissal as page navigation.
        event.stopImmediatePropagation()
        const top = Array.from(this.entries.entries()).at(-1)
        if (top) {
          // A SDK exit animation can outlive dismissal. Do not rearm history
          // for that closing layer while waiting for its React cleanup.
          this.entries.delete(top[0])
          if (top[1].close() === false) this.entries.set(...top)
          this.schedule()
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
      const target: unknown = event.state
      const forward = target && typeof target === 'object' && 'idx' in target && typeof target.idx === 'number'
        && this.routeIndex !== undefined && target.idx >= this.routeIndex
      this.guard = forward ? stale : undefined
      if (forward) window.history.forward()
      else window.history.back()
    }
  }
}

const modalHistory = new ModalHistory()

export function syncModalHistoryNavigation() { modalHistory.noteNavigation() }

export function useModalBackButton(isOpen: boolean, onClose: () => unknown, key?: string) {
  const generatedId = useId()
  const id = key ?? generatedId
  const close = useRef(onClose)
  useLayoutEffect(() => { close.current = onClose }, [onClose])
  useLayoutEffect(() => {
    if (isOpen) return modalHistory.register(id, () => close.current())
  }, [isOpen, id])
}
