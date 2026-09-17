import { useState } from 'react'
import { Button } from '../components/ui/Button'

export function RecoverySupport({ attemptId }: { attemptId: string }) {
  const [copied, setCopied] = useState(false)
  const [copyFailed, setCopyFailed] = useState(false)
  return <div className="space-y-3 border-t border-brand-border/20 pt-3">
    <p>For help, contact Plether on Discord and share this reference.</p>
    <code className="block break-all text-xs select-all">{attemptId}</code>
    <div className="flex flex-wrap items-center gap-3">
      <a href="https://plether.com/discord" target="_blank" rel="noopener noreferrer"
        className="inline-flex min-h-11 items-center border border-brand-border/30 bg-surface-muted px-3 font-medium text-content-primary hover:underline">
        Get help on Discord<span className="sr-only"> (opens in a new tab)</span>
      </a>
      <Button type="button" variant="ghost" size="sm" onClick={() => {
        void Promise.resolve().then(() => navigator.clipboard.writeText(attemptId)).then(() => {
          setCopied(true); setCopyFailed(false)
        }).catch(() => { setCopyFailed(true) })
      }}>{copied ? 'Reference copied' : 'Copy reference'}</Button>
    </div>
    {copyFailed && <p role="status">Select and copy the reference above.</p>}
  </div>
}
