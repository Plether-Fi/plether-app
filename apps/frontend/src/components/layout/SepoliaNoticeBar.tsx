import { useSettingsStore } from '../../stores/settingsStore'
import { PERPS_RELEASE_IS_DEPLOYED, PERPS_SETTLEMENT_TOKEN_LABEL } from '../../contracts/perpsAddresses'

export function SepoliaNoticeBar() {
  const openSepoliaWelcome = useSettingsStore((s) => s.openSepoliaWelcome)

  if (!PERPS_RELEASE_IS_DEPLOYED) {
    return <div role="status" className="bg-brand-yellow page-gutter py-3 text-center text-sm text-[#250917]">{PERPS_SETTLEMENT_TOKEN_LABEL} perps are being prepared. Testnet funding will open after deployment.</div>
  }

  return (
    <div className="flex min-h-11 flex-col items-stretch justify-center gap-1 bg-brand-yellow page-gutter py-1.5 text-center text-[10px] font-medium leading-tight text-[#250917] sm:flex-row sm:items-center sm:gap-3 sm:py-2.5 sm:text-sm sm:leading-normal">
      <span>
        <span className="block sm:hidden">Arbitrum Sepolia testnet · USDG</span>
        <span className="hidden sm:inline">
          You are on Arbitrum Sepolia testnet. Try trading with USDG.{' '}
        </span>
        <strong className="font-bold">
          Climb the weekly ranks in Plether Divisions and earn badges. Show them off on Discord.
        </strong>{' '}
        <a
          href="https://insights.plether.com/register"
          target="_blank"
          rel="noopener noreferrer"
          className="font-bold underline underline-offset-4"
        >
          Link your wallet ↗
        </a>
      </span>
      <button
        type="button"
        onClick={openSepoliaWelcome}
        className="min-h-11 shrink-0 border border-[#250917] px-3 py-1 text-xs font-semibold text-[#250917] transition-colors hover:bg-[#250917] hover:text-white hover:underline hover:underline-offset-4"
      >
        Get {PERPS_SETTLEMENT_TOKEN_LABEL}
      </button>
    </div>
  )
}
