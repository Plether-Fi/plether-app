import { useSettingsStore } from '../../stores/settingsStore'

export function SepoliaNoticeBar() {
  const openSepoliaWelcome = useSettingsStore((s) => s.openSepoliaWelcome)

  return (
    <div className="flex min-h-11 flex-col items-stretch justify-center gap-2 bg-brand-yellow px-3 py-2 text-center text-xs font-medium text-[#250917] sm:flex-row sm:items-center sm:gap-3 sm:px-4 sm:py-2.5 sm:text-sm">
      <span className="sm:hidden">
        Arbitrum Sepolia testnet · Mock funds only
      </span>
      <span className="hidden sm:inline">
        You are on Arbitrum Sepolia testnet. Try trading with mock funds. Registration for the
        trading competition is now closed. Check the current standings at{' '}
        <a
          href="https://insights.plether.com"
          target="_blank"
          rel="noopener noreferrer"
          className="underline underline-offset-4"
        >
          insights.plether.com
        </a>
        .
      </span>
      <button
        type="button"
        onClick={openSepoliaWelcome}
        className="min-h-11 shrink-0 border border-[#250917] px-3 py-2 text-xs font-semibold text-[#250917] transition-colors hover:bg-[#250917] hover:text-white hover:underline hover:underline-offset-4 sm:py-1"
      >
        Get mock USDC
      </button>
    </div>
  )
}
