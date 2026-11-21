import { useCallback, useEffect, useRef, useState } from 'react'
import { useGasPaymentPreference } from '../perps-aa/gasPaymentPreference'
import { deadlineNow } from '../perps-aa/deadlineClock'
import type { KernelActionQuote } from '../perps-aa/kernelRuntime'
import { usePerpsAaRuntime } from '../perps-aa/runtimeContext'
import { usePerpsUiStore } from '../stores/perpsUiStore'

export function usePerpsGasReview() {
  const runtime = usePerpsAaRuntime()
  const payment = runtime?.gasPayment
  const preference = useGasPaymentPreference(payment?.scope)
  const ownerAddress = runtime?.ownerAddress
  const openActivity = useCallback(() => {
    if (!payment || !ownerAddress) return
    const { chainId, sender } = payment.scope
    usePerpsUiStore.getState().requestActivity({ chainId, accountAddress: sender, ownerAddress })
  }, [payment, ownerAddress])
  return payment ? {
    preference,
    openActivity,
    available: payment.readiness[preference.mode],
    kernelScope: runtime.kernel.scope,
    chainId: payment.scope.chainId,
    deployment: payment.scope.deployment,
    sender: payment.scope.sender,
  } : undefined
}

export type PerpsGasReviewView = ReturnType<typeof usePerpsGasReview>

export function kernelQuoteIsFresh(quote: KernelActionQuote): boolean {
  try {
    return quote.expiresAt * 1_000 > deadlineNow()
  } catch {
    return false
  }
}

class KernelGasReviewCancelled extends Error {
  constructor() {
    super('Network gas review cancelled.')
    this.name = 'KernelGasReviewCancelled'
  }
}

interface PendingApproval {
  quote: KernelActionQuote
  scope: string
  resolve: () => void
  reject: (cause: Error) => void
}

export function useKernelGasApproval(gas: PerpsGasReviewView) {
  const scope = gas && [gas.chainId, gas.deployment, gas.sender, gas.preference.revision].join(':')
  const [pending, setPending] = useState<PendingApproval>()
  const active = useRef<PendingApproval>(undefined)
  const mounted = useRef(false)
  const cancel = useCallback(() => {
    const approval = active.current
    if (!approval) return
    active.current = undefined
    approval.reject(new KernelGasReviewCancelled())
    setPending(undefined)
  }, [])
  useEffect(() => {
    mounted.current = true
    return () => {
      mounted.current = false
      active.current?.reject(new KernelGasReviewCancelled())
      active.current = undefined
    }
  }, [])
  useEffect(() => {
    if (pending && pending.scope !== scope) {
      queueMicrotask(() => {
        if (active.current === pending) cancel()
      })
    }
  }, [cancel, pending, scope])
  const review = useCallback((quote: KernelActionQuote) => new Promise<void>((resolve, reject) => {
    if (!mounted.current || scope === undefined || active.current) {
      reject(new KernelGasReviewCancelled())
      return
    }
    const approval = { quote, scope, resolve, reject }
    active.current = approval
    setPending(approval)
  }), [scope])
  const confirm = () => {
    const approval = active.current
    if (!approval) return
    if (approval.scope !== scope || !kernelQuoteIsFresh(approval.quote)) {
      active.current = undefined
      approval.reject(new Error('The network gas quote expired or changed. Review again to continue.'))
      setPending(undefined)
      return
    }
    active.current = undefined
    approval.resolve()
    setPending(undefined)
  }
  return { quote: pending && pending.scope === scope ? pending.quote : undefined, review, confirm, cancel }
}
