import { Component, type ErrorInfo, type ReactNode } from 'react'
import { captureFrontendLog, captureReactException } from '../analytics/client'
import { BUILD_COMMIT } from '../config/buildInfo'

function newSupportReference(): string {
  try {
    return `ui-${crypto.randomUUID()}`
  } catch {
    // A diagnostic label, never an authorization or recovery credential.
    return `ui-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 12)}`
  }
}

interface Props {
  children: ReactNode
}

// Deliberately outside wallet, query, router and trading providers. The fallback
// must not depend on any of the state that may have caused the application crash.
export class AppErrorBoundary extends Component<Props, { failed: boolean }> {
  state = { failed: false }
  private readonly supportReference = newSupportReference()
  private reported = false

  static getDerivedStateFromError() {
    return { failed: true }
  }

  componentDidCatch(error: unknown, info: ErrorInfo) {
    if (this.reported) return
    this.reported = true
    try {
      captureReactException(error, info, 'caught', this.supportReference)
      captureFrontendLog('error', 'react application recovery screen shown', {
        component: 'react_root', operation: 'render', outcome: 'failure',
        error_category: 'caught_error', support_reference: this.supportReference,
      })
    } catch { /* A telemetry failure must never break the recovery screen. */ }
  }

  render() {
    if (!this.state.failed) return this.props.children
    return (
      <main className="min-h-screen bg-surface flex items-center justify-center p-6 text-content-primary">
        <section role="alert" aria-labelledby="app-recovery-title" className="w-full max-w-xl rounded-xl border border-white/20 bg-surface-panel p-6 sm:p-8">
          <p className="mb-3 text-sm text-content-secondary">Plether</p>
          <h1 id="app-recovery-title" className="text-2xl font-semibold">The trading interface stopped unexpectedly</h1>
          <p className="mt-4 text-content-secondary">Try reloading the page. If this happens again, send support a screenshot of this screen.</p>
          <p className="mt-3 text-content-secondary">If you were signing or submitting a transaction, check its status in Transaction History before trying again. This screen does not confirm whether it was sent.</p>
          <p className="mt-3 text-content-secondary">Do not clear site data: it contains transaction-recovery records. Reloading does not clear those records.</p>
          <button type="button" onClick={() => { window.location.reload() }} className="mt-6 rounded-lg bg-primary-500 px-5 py-3 font-medium text-white focus-visible:outline-2 focus-visible:outline-offset-4">
            Reload page
          </button>
          <dl className="mt-6 space-y-2 text-sm text-content-secondary">
            <div><dt>Support reference</dt><dd className="break-all font-mono select-all">{this.supportReference}</dd></div>
            <div><dt>Build</dt><dd className="break-all font-mono select-all">{BUILD_COMMIT || 'unknown'}</dd></div>
          </dl>
        </section>
      </main>
    )
  }
}
