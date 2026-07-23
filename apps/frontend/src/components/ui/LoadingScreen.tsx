import { Button } from './Button'
import { Spinner } from './Spinner'

export interface LoadingStep {
  label: string
  status: 'pending' | 'in_progress' | 'confirming' | 'completed' | 'error'
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
        <div className="h-1.5 w-full bg-brand-border/30">
          <div
            className={`h-full transition-all duration-500 ${
              hasError
                ? 'bg-brand-orange'
                : isComplete
                  ? 'bg-positive'
                  : 'bg-warning'
            }`}
            style={{ width: `${String(progress)}%` }}
          />
        </div>
      </div>

      <div className="p-4 sm:p-8">
        <div className="mb-6 flex min-w-0 items-start justify-between gap-3 sm:mb-8">
          <h2 className="min-w-0 text-xl font-bold text-content-primary sm:text-2xl">
            {title}
          </h2>
          {onClose && (
            <button
              type="button"
              aria-label="Close transaction"
              onClick={onClose}
              className="-mr-2 -mt-2 inline-flex h-11 w-11 shrink-0 items-center justify-center text-content-secondary transition-colors hover:text-[#FFAB96]"
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
                        ? 'text-content-secondary'
                        : step.status === 'error'
                          ? 'text-brand-orange'
                          : step.status === 'confirming'
                            ? 'text-positive'
                            : 'text-content-primary'
                    }
                  >
                    {index + 1}. {step.label}
                  </span>
                </div>
                {step.status === 'error' && errorMessage && (
                  <div className="mt-3 border border-brand-orange/30 bg-brand-orange/10 p-3 sm:ml-10">
                    <p className="break-words text-sm text-brand-orange">{errorMessage}</p>
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
              className="flex w-full items-center justify-center gap-2 bg-positive px-4 py-2 font-medium text-app-bg transition-colors hover:bg-[#00CC77] hover:underline hover:underline-offset-4"
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
      <div className="w-6 h-6 rounded-full bg-positive flex items-center justify-center">
        <span className="material-symbols-outlined text-app-bg text-base font-bold">
          check
        </span>
      </div>
    )
  }

  if (status === 'error') {
    return (
      <div className="w-6 h-6 rounded-full bg-brand-orange flex items-center justify-center">
        <span className="material-symbols-outlined text-app-bg text-base font-bold">
          close
        </span>
      </div>
    )
  }

  if (status === 'in_progress') {
    return <Spinner size="md" />
  }

  if (status === 'confirming') {
    return <Spinner size="md" variant="confirming" />
  }

  return (
    <div className="w-6 h-6 rounded-full border-2 border-content-secondary/50" />
  )
}
