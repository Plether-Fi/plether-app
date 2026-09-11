import { captureAnalyticsEvent, captureFrontendLog, type AnalyticsProperties } from './client'

// Both keys and values are developer-owned. The public endpoint's categorical
// syntax validation alone is not a privacy boundary for external telemetry.
const components = new Set(['keeper', 'funding', 'oracle', 'readiness', 'sponsorship', 'reconciliation', 'bundler', 'paymaster', 'liquidation', 'protection', 'lp_settlement', 'alto'])
const reasons = new Set([
  'READY', 'FUNDING_LOW', 'FUNDING_UNVERIFIED', 'KEEPER_INSUFFICIENT_FUNDS',
  'READINESS_UNAVAILABLE', 'WORKER_HEARTBEAT_STALE', 'ORACLE_UNAVAILABLE',
  'OPEN_EXECUTION_UNAVAILABLE', 'EXIT_MODE_REQUIRES_VALIDATION', 'BUNDLER_UNAVAILABLE',
  'SPONSORSHIP_DISABLED', 'PAYMASTER_PAUSED', 'RECONCILIATION_STALE',
])
interface Observation { component: string; status: string; reason: string }
interface Incident { properties: AnalyticsProperties; startedAt: number; reportedAt: number; repeats: number }
type Emit = (phase: 'started' | 'summary' | 'resolved', properties: AnalyticsProperties) => void

export function createReadinessReporter(emit: Emit, clock = () => performance.now()) {
  // Maximum size is bounded by the categorical allowlists, independent of input
  // cardinality or wallet activity. Unknown evidence cannot resolve an incident.
  const active = new Map<string, Incident>()
  const publish: Emit = (phase, properties) => {
    try { emit(phase, properties) } catch { /* Telemetry never changes readiness. */ }
  }
  return (checks: readonly Observation[]) => {
    const now = clock()
    const current = new Map<string, AnalyticsProperties>()
    const unknownSnapshot = checks.some(check => check.component === 'readiness' && check.status === 'unknown')
    const healthy = new Set(checks.filter(check => check.status === 'ready').map(check => check.component))
    for (const check of checks) if (check.status !== 'ready') healthy.delete(check.component)
    if (!unknownSnapshot && checks.length > 0) healthy.add('readiness')
    for (const check of checks) {
      if (check.status === 'ready' && check.reason === 'READY') continue
      const component = components.has(check.component) ? check.component : 'readiness'
      const reason = reasons.has(check.reason) ? check.reason : 'READINESS_UNAVAILABLE'
      const outcome = ['ready', 'blocked', 'unknown'].includes(check.status) ? check.status : 'unknown'
      const key = `${component}:${reason}:${outcome}`
      current.set(key, { component, reason_code: reason, outcome, stage: 'readiness' })
    }
    for (const [key, incident] of active) {
      if (current.has(key) || unknownSnapshot || !healthy.has(String(incident.properties.component))) continue
      publish('resolved', { ...incident.properties, outcome: 'ready', occurrence_count: incident.repeats, duration_ms: Math.max(0, now - incident.startedAt) })
      active.delete(key)
    }
    for (const [key, properties] of current) {
      const incident = active.get(key)
      if (!incident) {
        active.set(key, { properties, startedAt: now, reportedAt: now, repeats: 0 })
        publish('started', { ...properties, occurrence_count: 1, duration_ms: 0 })
      } else {
        incident.repeats++
        if (now - incident.reportedAt >= 60_000) {
          publish('summary', { ...properties, occurrence_count: incident.repeats, duration_ms: Math.max(0, now - incident.startedAt) })
          incident.repeats = 0
          incident.reportedAt = now
        }
      }
    }
  }
}

export const reportReadiness = createReadinessReporter((phase, properties) => {
  captureAnalyticsEvent(`perps readiness incident ${phase}`, properties)
  captureFrontendLog(phase === 'resolved' ? 'info' : 'warn', `Trading readiness incident ${phase}`, properties)
})
