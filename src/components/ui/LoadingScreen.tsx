import { Button } from './Button'
import { Spinner } from './Spinner'

export interface LoadingStep {
  label: string
  status: 'pending' | 'in_progress' | 'completed' | 'error'
}

interface LoadingScreenProps {
  title?: string
  steps: LoadingStep[]
  errorMessage?: string
  onClose?: () => void
  onRetry?: () => void
  transactionUrl?: string
}

export function LoadingScreen({
  title = 'Your request is being processed.',
  steps,
  errorMessage,
  onClose,
  onRetry,
  transactionUrl,
}: LoadingScreenProps) {
  const errorIndex = steps.findIndex((s) => s.status === 'error')
  const hasError = errorIndex !== -1
  const isComplete = steps.length > 0 && steps.every((s) => s.status === 'completed')
  const completedCount = steps.filter((s) => s.status === 'completed').length
  const progress = hasError || isComplete
    ? 100
    : ((1 + completedCount) / (steps.length + 1)) * 100

  return (
    <div className="w-full">
      <div className="py-2 -my-2 overflow-x-clip">
        <div className="h-1.5 w-full bg-cyber-border-glow/30">
          <div
            className={`h-full transition-all duration-500 ${
              hasError
                ? 'bg-cyber-electric-fuchsia shadow-[0_0_12px_rgba(255,0,204,0.8)]'
                : isComplete
                  ? 'bg-cyber-neon-green shadow-[0_0_12px_rgba(0,255,153,0.8)]'
                  : 'bg-cyber-warning-text shadow-[0_0_12px_rgba(255,215,0,0.8)]'
            }`}
            style={{ width: `${String(progress)}%` }}
          />
        </div>
      </div>

      <div className="p-8">
        <div className="flex items-start justify-between mb-8">
          <h2 className="text-2xl font-bold text-cyber-text-primary">
            {title}
          </h2>
          {onClose && (
            <button
              onClick={onClose}
              className="text-cyber-text-secondary hover:text-cyber-bright-blue transition-colors -mt-1 -mr-2"
            >
              <span className="material-symbols-outlined">close</span>
            </button>
          )}
        </div>

        <div className="space-y-5">
          {steps.map((step, index) => {
            const isAfterError = hasError && index > errorIndex
            return (
              <div key={`${step.label}-${String(index)}`}>
                <div className={`flex items-center gap-4 ${isAfterError ? 'opacity-20' : ''}`}>
                  <StepIndicator status={step.status} />
                  <span
                    className={
                      step.status === 'pending'
                        ? 'text-cyber-text-secondary'
                        : step.status === 'error'
                          ? 'text-cyber-electric-fuchsia'
                          : 'text-cyber-text-primary'
                    }
                  >
                    {index + 1}. {step.label}
                  </span>
                </div>
                {step.status === 'error' && errorMessage && (
                  <div className="mt-3 ml-10 p-3 bg-cyber-electric-fuchsia/10 border border-cyber-electric-fuchsia/30">
                    <p className="text-cyber-electric-fuchsia text-sm">{errorMessage}</p>
                  </div>
                )}
              </div>
            )
          })}
        </div>

        {hasError && onRetry && (
          <div className="mt-6">
            <Button variant="secondary" onClick={onRetry} className="w-full">
              Try again
            </Button>
          </div>
        )}

        {isComplete && transactionUrl && (
          <div className="mt-6">
            <a
              href={transactionUrl}
              target="_blank"
              rel="noopener noreferrer"
              className="flex items-center justify-center gap-2 w-full px-4 py-2 bg-cyber-neon-green hover:bg-cyber-neon-green/80 text-cyber-bg font-medium transition-all duration-200"
            >
              Show transaction
              <span className="material-symbols-outlined text-lg">open_in_new</span>
            </a>
          </div>
        )}
      </div>
    </div>
  )
}

function StepIndicator({ status }: { status: LoadingStep['status'] }) {
  if (status === 'completed') {
    return (
      <div className="w-6 h-6 rounded-full bg-cyber-neon-green flex items-center justify-center">
        <span className="material-symbols-outlined text-cyber-bg text-base font-bold">
          check
        </span>
      </div>
    )
  }

  if (status === 'error') {
    return (
      <div className="w-6 h-6 rounded-full bg-cyber-electric-fuchsia flex items-center justify-center">
        <span className="material-symbols-outlined text-cyber-bg text-base font-bold">
          close
        </span>
      </div>
    )
  }

  if (status === 'in_progress') {
    return <Spinner size="md" />
  }

  return (
    <div className="w-6 h-6 rounded-full border-2 border-cyber-text-secondary/50" />
  )
}
