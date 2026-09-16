import { create } from 'zustand'

type PerpsMarginAction = 'deposit' | 'withdraw'

interface PerpsMarginActionRequest {
  id: number
  action: PerpsMarginAction
}

interface PerpsActivityTarget {
  chainId: number
  accountAddress: string
  ownerAddress: string
  operationId: string
}

interface PerpsUiState {
  activityRequest: (PerpsActivityTarget & { id: number }) | null
  requestActivity: (target: PerpsActivityTarget) => void
  clearActivityRequest: (id: number) => void
  marginActionRequest: PerpsMarginActionRequest | null
  requestMarginAction: (action: PerpsMarginAction) => void
  clearMarginActionRequest: (id: number) => void
}

export const usePerpsUiStore = create<PerpsUiState>()((set) => ({
  activityRequest: null,
  requestActivity: target => {
    set(state => ({ activityRequest: { ...target, id: (state.activityRequest?.id ?? 0) + 1 } }))
  },
  clearActivityRequest: id => {
    set(state => state.activityRequest?.id === id ? { activityRequest: null } : state)
  },
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
