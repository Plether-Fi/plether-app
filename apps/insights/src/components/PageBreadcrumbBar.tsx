import type { ReactNode } from 'react'
import { Link } from 'react-router-dom'

export interface BreadcrumbItem {
  label: string
  to?: string
}

export interface PageBreadcrumbBarProps {
  items: readonly BreadcrumbItem[]
  rightSlot?: ReactNode
}

/**
 * Unified top-of-page hierarchy bar for all drill-down detail pages.
 * Replaces inconsistent ad-hoc `← Back to ...` and far-right cross-link strips.
 */
export function PageBreadcrumbBar({ items, rightSlot }: PageBreadcrumbBarProps) {
  return (
    <div className="flex flex-wrap items-center justify-between gap-x-4 gap-y-2 border-b border-brand-border/20 pb-3.5 text-xs">
      <nav aria-label="Breadcrumb" className="min-w-0">
        <ol className="flex flex-wrap items-center gap-1.5 font-mono">
          {items.map((item, idx) => {
            const isLast = idx === items.length - 1
            return (
              <li key={`${item.label}-${String(idx)}`} className="inline-flex items-center gap-1.5">
                {idx > 0 ? (
                  <span aria-hidden="true" className="text-content-tertiary/70 select-none">
                    ›
                  </span>
                ) : null}
                {item.to && !isLast ? (
                  <Link
                    to={item.to}
                    className="text-content-secondary transition-colors hover:text-brand-peach hover:underline hover:underline-offset-4"
                  >
                    {item.label}
                  </Link>
                ) : (
                  <span
                    aria-current={isLast ? 'page' : undefined}
                    className={
                      isLast
                        ? 'font-semibold text-content-primary'
                        : 'text-content-secondary'
                    }
                  >
                    {item.label}
                  </span>
                )}
              </li>
            )
          })}
        </ol>
      </nav>
      {rightSlot ? <div className="flex shrink-0 flex-wrap items-center gap-2">{rightSlot}</div> : null}
    </div>
  )
}

export type WalletPerspective = 'wallet' | 'smart-account' | 'divisions'

/**
 * Compact segmented entity switcher for navigating between the 3 views of a
 * single 0x trading account address without scattered ad-hoc links.
 */
export function WalletPerspectiveSwitcher({
  address,
  active,
  showDivisions = true,
}: {
  address: string
  active: WalletPerspective
  showDivisions?: boolean
}) {
  const normalized = address.trim().toLowerCase()
  const items: { id: WalletPerspective; label: string; to: string }[] = [
    { id: 'wallet', label: 'Wallet Ledger', to: `/wallets/${normalized}` },
    { id: 'smart-account', label: 'Kernel v4', to: `/trading-accounts/${normalized}` },
    ...(showDivisions
      ? [{ id: 'divisions' as const, label: 'Divisions', to: `/divisions/wallets/${normalized}` }]
      : []),
  ]

  return (
    <div
      className="inline-flex items-center border border-brand-border/25 bg-app-bg p-0.5 font-mono text-[11px]"
      role="navigation"
      aria-label="Account views"
    >
      {items.map((item) => {
        const isCurrent = item.id === active
        return isCurrent ? (
          <span
            key={item.id}
            aria-current="page"
            className="bg-brand-orange/20 px-2 py-0.5 font-semibold text-brand-peach"
          >
            {item.label}
          </span>
        ) : (
          <Link
            key={item.id}
            to={item.to}
            className="px-2 py-0.5 text-content-secondary transition-colors hover:text-content-primary"
          >
            {item.label}
          </Link>
        )
      })}
    </div>
  )
}
