import { isUpstreamApiError, useProtocolStatus } from '../../api'

export function ApiErrorBanner() {
  const { isError, error, failureCount, fetchStatus } = useProtocolStatus()

  // Show banner on first failure, even while retrying
  if (!isError && failureCount === 0) return null

  const isRetrying = fetchStatus === 'fetching'
  const hasRetriesLeft = !isError && failureCount > 0
  const errorMessage = error?.message ?? 'Connection failed'
  const isUpstreamError = isUpstreamApiError(error)
  const isNetworkError = !error ||
                         /fetch|network|connection|failed/i.test(errorMessage) ||
                         errorMessage.includes('NETWORK_ERROR')

  const baseMessage = isUpstreamError
    ? 'Protocol data is temporarily unavailable. Some prices and APY values may be stale.'
    : isNetworkError
    ? 'Unable to connect to backend API.'
    : `API Error: ${errorMessage}`

  return (
    <div className="border-b border-[#FFAB96] bg-[#FF572D] py-3">
      <div className="mx-auto flex max-w-7xl flex-col items-stretch gap-3 px-4 sm:flex-row sm:items-center sm:px-6 lg:px-8">
        <span className={`material-symbols-outlined text-[#FFF5F9] ${isRetrying ? 'animate-spin' : ''}`}>
          {isRetrying ? 'sync' : 'cloud_off'}
        </span>
        <p className="flex-1 text-sm text-[#FFF5F9]">
          {baseMessage}
          {isRetrying && ' Reconnecting...'}
          {hasRetriesLeft && !isRetrying && ' Retrying...'}
        </p>
        <button
          onClick={() => { window.location.reload() }}
          className="flex min-h-11 shrink-0 cursor-pointer items-center justify-center gap-1.5 border border-[#FFF5F9] bg-[#FFF5F9] px-3 py-2 text-sm font-medium text-[#250917] transition-colors hover:bg-[#250917] hover:text-[#FFAB96] hover:underline hover:underline-offset-4"
        >
          <span className="material-symbols-outlined text-base">refresh</span>
          Refresh
        </button>
      </div>
    </div>
  )
}
