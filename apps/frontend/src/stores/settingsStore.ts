import { create } from 'zustand'
import { devtools, persist } from 'zustand/middleware'
import { STORAGE_KEYS, DEFAULT_SLIPPAGE, MAX_SLIPPAGE, DEFAULT_MAX_PRICE_IMPACT, MAX_PRICE_IMPACT } from '../config/constants'

interface SettingsState {
  slippage: number
  maxPriceImpact: number
  riskAccepted: boolean
  sepoliaWelcomeDismissed: boolean
  setSlippage: (slippage: number) => void
  setMaxPriceImpact: (maxPriceImpact: number) => void
  acceptRisk: () => void
  dismissSepoliaWelcome: () => void
  openSepoliaWelcome: () => void
}

export const useSettingsStore = create<SettingsState>()(
  devtools(
    persist(
      (set) => ({
        slippage: DEFAULT_SLIPPAGE,
        maxPriceImpact: DEFAULT_MAX_PRICE_IMPACT,
        riskAccepted: false,
        sepoliaWelcomeDismissed: false,

        setSlippage: (slippage) =>
          set({ slippage: Math.min(slippage, MAX_SLIPPAGE) }),

        setMaxPriceImpact: (maxPriceImpact) =>
          set({ maxPriceImpact: Math.min(maxPriceImpact, MAX_PRICE_IMPACT) }),

        acceptRisk: () => set({ riskAccepted: true }),

        dismissSepoliaWelcome: () => set({ sepoliaWelcomeDismissed: true }),

        openSepoliaWelcome: () => set({ sepoliaWelcomeDismissed: false }),
      }),
      {
        name: STORAGE_KEYS.SETTINGS,
      }
    ),
    { name: 'SettingsStore' }
  )
)
