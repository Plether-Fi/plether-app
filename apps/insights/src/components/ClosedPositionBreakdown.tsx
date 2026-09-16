import { useId, useRef, useState, type ReactNode } from 'react'
import { createPortal } from 'react-dom'
import type { WalletActivity } from '../api/types'
import { formatPrice, formatUtc, shortAddress } from '../utils/format'
import { deriveTradeBreakdown, formatTradeUsdc } from '../utils/tradeBreakdown'
import { TradeBreakdownDetails } from './TradeBreakdown'

function unsigned(value: string | null | undefined): bigint | null {
  return value != null && /^\d+$/.test(value) ? BigInt(value) : null
}

function quantity(value: bigint | null): string {
  if (value === null) return 'Unavailable'
  const scale = 10n ** 18n
  // Keep at least six decimals, and retain any additional nonzero token precision.
  const fraction = (value % scale).toString().padStart(18, '0').replace(/0{1,12}$/, '')
  return `${new Intl.NumberFormat('en-US').format(value / scale)}.${fraction} plDXY`
}

function Detail({ label, children }: { label: string; children: ReactNode }) {
  return <div className="flex items-start justify-between gap-4 py-1.5">
    <dt className="text-content-secondary">{label}</dt>
    <dd className="min-w-0 text-right tabular-nums [overflow-wrap:anywhere]">{children}</dd>
  </div>
}

export function ClosedPositionBreakdown({ item }: { item: WalletActivity }) {
  const dialog = useRef<HTMLDialogElement>(null)
  const titleId = useId()
  const [isOpen, setIsOpen] = useState(false)
  const validReceipt = deriveTradeBreakdown(item.execution, item.type) !== null
  const receipt = validReceipt ? item.execution?.receipt : undefined
  const remaining = unsigned(receipt?.postPositionSize)
  const margin = unsigned(receipt?.postPositionMarginUsdc)
  const notional = unsigned(receipt?.executionNotionalUsdc)
  const status = remaining === null ? 'Unavailable' : remaining === 0n ? 'Fully closed' : 'Partially closed'

  return <>
    <button type="button" aria-haspopup="dialog" onClick={() => {
      setIsOpen(true)
      dialog.current?.showModal()
    }} className="min-h-10 cursor-pointer border border-brand-border/40 px-3 py-2 text-sm font-semibold text-brand-peach hover:bg-brand-peach/5 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-4">
      View breakdown
    </button>
    {createPortal(<dialog ref={dialog} aria-labelledby={titleId} onClose={() => { setIsOpen(false) }}
      className="fixed inset-0 m-auto max-h-[90dvh] w-[calc(100%-2rem)] max-w-2xl overflow-y-auto overscroll-contain border border-brand-border/40 bg-surface-panel p-0 text-content-primary shadow-2xl backdrop:bg-black/70">
      <header className="sticky top-0 z-10 flex items-center justify-between gap-4 border-b border-brand-border/25 bg-surface-panel px-4 py-4 sm:px-6">
        <h2 id={titleId} className="text-lg font-semibold">Position breakdown</h2>
        <button type="button" onClick={() => { dialog.current?.close() }} aria-label="Close position breakdown"
          className="min-h-10 cursor-pointer px-3 text-brand-peach focus-visible:outline focus-visible:outline-2">Close</button>
      </header>
      {isOpen && <div className="space-y-5 px-4 py-5 sm:px-6">
        <p className="text-xs leading-5 text-content-secondary">This breakdown covers this close execution. Opening costs and earlier increases or partial closes are shown on their own activity rows.</p>
        <section className="border border-brand-border/20 bg-app-bg/45 p-4">
          <h3 className="mb-2 text-xs font-semibold uppercase tracking-wide text-content-secondary">Execution</h3>
          <dl className="text-sm">
            <Detail label="Market">{item.market ?? 'Unavailable'}</Detail>
            <Detail label="Position side">{item.side?.toUpperCase() ?? 'Unavailable'}</Detail>
            <Detail label="Closed at">{formatUtc(item.occurredAt)}</Detail>
            <Detail label="Order ID">{validReceipt ? item.execution?.orderId : 'Unavailable'}</Detail>
            <Detail label="Final plDXY price">{formatPrice(item.price)}</Detail>
            <Detail label="Quantity closed">{quantity(unsigned(item.sizeDelta))}</Detail>
            <Detail label="Executed exposure">{notional === null ? 'Unavailable' : formatTradeUsdc(notional, false, true)}</Detail>
            <Detail label="Execution transaction">{item.txHash ? <a className="font-mono text-brand-peach hover:underline" href={`https://sepolia.arbiscan.io/tx/${item.txHash}`} target="_blank" rel="noreferrer">{shortAddress(item.txHash)} ↗</a> : 'Unavailable'}</Detail>
          </dl>
        </section>
        <section className="border border-brand-border/20 bg-app-bg/45 p-4">
          <h3 className="mb-3 text-xs font-semibold uppercase tracking-wide text-content-secondary">Close result and account change</h3>
          <TradeBreakdownDetails item={item} />
        </section>
        <section className="border border-brand-border/20 bg-app-bg/45 p-4">
          <h3 className="mb-2 text-xs font-semibold uppercase tracking-wide text-content-secondary">Position after this close</h3>
          <dl className="text-sm">
            <Detail label="Status">{status}</Detail>
            {remaining !== null && remaining > 0n && <>
              <Detail label="Remaining quantity">{quantity(remaining)}</Detail>
              <Detail label="Remaining margin">{margin === null ? 'Unavailable' : formatTradeUsdc(margin, false, true)}</Detail>
            </>}
          </dl>
        </section>
      </div>}
    </dialog>, document.body)}
  </>
}
