import { create } from 'zustand'

type PerpsMarginAction = 'deposit' | 'withdraw'

interface PerpsMarginActionRequest {
  id: number
  action: PerpsMarginAction
}

interface PerpsUiState {
  marginActionRequest: PerpsMarginActionRequest | null
  requestMarginAction: (action: PerpsMarginAction) => void
  clearMarginActionRequest: (id: number) => void
}

export const usePerpsUiStore = create<PerpsUiState>()((set) => ({
  marginActionRequest: null,

  requestMarginAction: (action) => {
    set((state) => ({
      marginActionRequest: {
        id: (state.marginActionRequest?.id ?? 0) + 1,
        action,
      },
    }))
  },

  clearMarginActionRequest: (id) => {
    set((state) => (
      state.marginActionRequest?.id === id
        ? { marginActionRequest: null }
        : state
    ))
  },
}))
