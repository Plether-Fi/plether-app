import { useId, useLayoutEffect, useRef } from 'react'
import { ModalHistory } from './modalHistory'

const modalHistory = new ModalHistory()
if (import.meta.hot) import.meta.hot.dispose(() => { modalHistory.dispose() })

export function syncModalHistoryNavigation() { modalHistory.noteNavigation() }

export function useModalBackButton(isOpen: boolean, onClose: () => unknown, key?: string) {
  const generatedId = useId()
  const id = key ?? generatedId
  const close = useRef(onClose)
  useLayoutEffect(() => { close.current = onClose }, [onClose])
  useLayoutEffect(() => {
    if (isOpen) return modalHistory.register(id, () => close.current())
  }, [isOpen, id])
}
