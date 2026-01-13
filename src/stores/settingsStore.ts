import { create } from 'zustand'
import { devtools, persist } from 'zustand/middleware'
import { STORAGE_KEYS, DEFAULT_SLIPPAGE, MAX_SLIPPAGE } from '../config/constants'

interface SettingsState {
  slippage: number
  setSlippage: (slippage: number) => void
}

export const useSettingsStore = create<SettingsState>()(
  devtools(
    persist(
      (set) => ({
        slippage: DEFAULT_SLIPPAGE,

        setSlippage: (slippage) =>
          set({ slippage: Math.min(slippage, MAX_SLIPPAGE) }),
      }),
      {
        name: STORAGE_KEYS.SLIPPAGE,
      }
    ),
    { name: 'SettingsStore' }
  )
)
