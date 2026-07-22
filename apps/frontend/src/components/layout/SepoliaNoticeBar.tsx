import { useSettingsStore } from '../../stores/settingsStore'

export function SepoliaNoticeBar() {
  const openSepoliaWelcome = useSettingsStore((s) => s.openSepoliaWelcome)

  return (
    <div className="flex min-h-11 items-center justify-center gap-3 bg-brand-yellow px-4 py-2.5 text-center text-xs font-medium text-[#250917] sm:text-sm">
      <span>
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
        className="border border-[#250917] px-2 py-0.5 text-xs font-semibold text-[#250917] transition-colors hover:bg-[#250917] hover:text-white hover:underline hover:underline-offset-4"
      >
        Get mock USDC
      </button>
    </div>
  )
}
