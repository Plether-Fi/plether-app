import { create } from 'zustand'
import { devtools, persist } from 'zustand/middleware'
import { STORAGE_KEYS, DEFAULT_SLIPPAGE, MAX_SLIPPAGE, DEFAULT_MAX_PRICE_IMPACT, MAX_PRICE_IMPACT } from '../config/constants'

interface SettingsState {
  slippage: number
  maxPriceImpact: number
  setSlippage: (slippage: number) => void
  setMaxPriceImpact: (maxPriceImpact: number) => void
}

export const useSettingsStore = create<SettingsState>()(
  devtools(
    persist(
      (set) => ({
        slippage: DEFAULT_SLIPPAGE,
        maxPriceImpact: DEFAULT_MAX_PRICE_IMPACT,

        setSlippage: (slippage) =>
          set({ slippage: Math.min(slippage, MAX_SLIPPAGE) }),

        setMaxPriceImpact: (maxPriceImpact) =>
          set({ maxPriceImpact: Math.min(maxPriceImpact, MAX_PRICE_IMPACT) }),
      }),
      {
        name: STORAGE_KEYS.SETTINGS,
      }
    ),
    { name: 'SettingsStore' }
  )
)
