import { useCallback, useLayoutEffect, useRef, useState } from 'react'
import { readGasPaymentPreference, useGasPaymentPreference } from '../perps-aa/gasPaymentPreference'
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
    paymentScope: payment.scope,
    openActivity,
    available: payment.readiness[preference.mode],
    paysEthFromWallet: payment.paysEthFromWallet,
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

export class KernelGasReviewCancelled extends Error {
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

function approvalMatches(gas: PerpsGasReviewView, quote: KernelActionQuote): boolean {
  if (!gas?.available || gas.preference.mode !== quote.mode) return false
  const current = readGasPaymentPreference(gas.paymentScope)
  return current.mode === quote.mode && current.revision === gas.preference.revision
}

export function useKernelGasApproval(gas: PerpsGasReviewView) {
  const scope = gas && [gas.chainId, gas.deployment, gas.sender, gas.preference.revision].join(':')
  const [pending, setPending] = useState<PendingApproval>()
  const active = useRef<PendingApproval>(undefined)
  const mounted = useRef(false)
  const current = useRef({ gas, scope })
  const cancel = useCallback(() => {
    const approval = active.current
    if (!approval) return
    active.current = undefined
    approval.reject(new KernelGasReviewCancelled())
    setPending(undefined)
  }, [])
  useLayoutEffect(() => {
    mounted.current = true
    return () => {
      mounted.current = false
      active.current?.reject(new KernelGasReviewCancelled())
      active.current = undefined
    }
  }, [])
  useLayoutEffect(() => {
    current.current = { gas, scope }
    const approval = active.current
    if (approval && (approval.scope !== scope || !approvalMatches(gas, approval.quote))) {
      queueMicrotask(() => {
        if (active.current === approval) cancel()
      })
    }
  }, [cancel, gas, scope])
  const review = useCallback((quote: KernelActionQuote) => new Promise<void>((resolve, reject) => {
    const { gas, scope } = current.current
    if (!mounted.current || scope === undefined || active.current || !approvalMatches(gas, quote)) {
      reject(new KernelGasReviewCancelled())
      return
    }
    if (!kernelQuoteIsFresh(quote)) {
      reject(new Error('The network gas quote expired. Review again to continue.'))
      return
    }
    const approval = { quote, scope, resolve, reject }
    active.current = approval
    setPending(approval)
  }), [])
  const confirm = () => {
    const approval = active.current
    if (!approval) return
    const { gas, scope } = current.current
    if (approval.scope !== scope || !approvalMatches(gas, approval.quote)) {
      cancel()
      return
    }
    if (!kernelQuoteIsFresh(approval.quote)) {
      active.current = undefined
      approval.reject(new Error('The network gas quote expired or changed. Review again to continue.'))
      setPending(undefined)
      return
    }
    active.current = undefined
    approval.resolve()
    setPending(undefined)
  }
  const quote = pending && pending.scope === scope ? pending.quote : undefined
  const canConfirm = quote !== undefined && gas?.available === true && gas.preference.mode === quote.mode
  return { quote, canConfirm, review, confirm, cancel }
}
