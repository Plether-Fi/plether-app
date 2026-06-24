import { useSettingsStore } from '../../stores/settingsStore'

export function SepoliaNoticeBar() {
  const openSepoliaWelcome = useSettingsStore((s) => s.openSepoliaWelcome)

  return (
    <div className="flex min-h-8 items-center justify-center gap-3 bg-white px-4 py-1.5 text-center text-xs font-medium text-[#250917] sm:text-sm">
      <span>This is Sepolia testnet.</span>
      <button
        type="button"
        onClick={openSepoliaWelcome}
        className="border border-[#250917] px-2 py-0.5 text-xs font-semibold text-[#250917] transition-colors hover:bg-[#250917] hover:text-white hover:underline hover:underline-offset-4"
      >
        Open faucet
      </button>
    </div>
  )
}
