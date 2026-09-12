import { useEffect, useState } from 'react'
import { readinessMessage } from '../perps-aa/readiness'

export function OperationDiagnostic({ attemptId }: { attemptId: string }) {
  const [reason, setReason] = useState<string>()
  const valid = /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i.test(attemptId)
  useEffect(() => {
    if (!valid) return
    const controller = new AbortController()
    void fetch(`/api/perps/v1/aa/diagnostics?attemptId=${encodeURIComponent(attemptId)}`, { credentials: 'same-origin', cache: 'no-store', signal: controller.signal })
      .then(async (response): Promise<unknown> => response.ok ? await response.json() as unknown : undefined)
      .then((data: unknown) => {
        if (!data || typeof data !== 'object') return
        const diagnostic = data as { version?: number; reason?: string; provenance?: string }
        if (diagnostic.version === 1 && diagnostic.provenance === 'backend_observation'
          && diagnostic.reason && /^[A-Z_]{1,64}$/.test(diagnostic.reason)) setReason(diagnostic.reason)
      }).catch(() => { /* A diagnostic outage never changes an operation's outcome. */ })
    return () => { controller.abort() }
  }, [attemptId, valid])
  if (!valid) return null
  return <div className="mt-2 text-xs text-content-secondary">
    <p>Support reference: <code>{attemptId}</code></p>
    {reason ? <p>Recorded execution-attempt issue: {readinessMessage(reason)} This is historical evidence, not current trading status.</p> : <p>No verified historical failure cause is available.</p>}
  </div>
}
