import { lazy, Suspense, useState } from 'react'
import { Link } from 'react-router-dom'
import { BUILD_COMMIT } from '../../config/buildInfo'
import { isPrimaryAppDeployment } from '../../utils/deployment'

const BuildDetailsModal = lazy(() => import('./BuildDetailsModal').then((module) => ({ default: module.BuildDetailsModal })))

export function Footer() {
  const [showBuildDetails, setShowBuildDetails] = useState(false)
  const hasMobileNav = !isPrimaryAppDeployment()
  const footerLinkClass = "transition-colors hover:text-content-primary hover:underline hover:underline-offset-4"

  return (
    <>
      <footer className={`border-t border-brand-border/30 bg-surface-panel/50 ${hasMobileNav ? 'pb-[calc(var(--spacing-mobile-nav)+1px+env(safe-area-inset-bottom))] lg:pb-0' : ''}`}>
        <div className="mx-auto flex max-w-7xl flex-col items-center justify-between gap-3 page-gutter py-4 text-center text-xs text-content-secondary lg:flex-row lg:text-left">
          <span>&copy; 2026 Plether Labs Limited. All rights reserved.</span>
          <nav className="flex flex-wrap justify-center gap-x-4 gap-y-2 lg:justify-end">
            <a href="https://docs.plether.com" target="_blank" rel="noopener noreferrer" className={footerLinkClass}>
              Docs
            </a>
            <a href="https://plether.com/discord" target="_blank" rel="noopener noreferrer" className={footerLinkClass}>
              Discord
            </a>
            <a
              href="https://www.tradingview.com/"
              target="_blank"
              rel="noopener"
              className={footerLinkClass}
            >
              Charts by TradingView
            </a>
            <Link to="/terms" className={footerLinkClass}>
              Terms of Service
            </Link>
            <Link to="/privacy" className={footerLinkClass}>
              Privacy Policy
            </Link>
            <Link to="/risk" className={footerLinkClass}>
              Risk Disclosure
            </Link>
            <button
              type="button"
              className={`font-mono tabular-nums ${footerLinkClass}`}
              title={`Git commit ${BUILD_COMMIT}`}
              onClick={() => {
                setShowBuildDetails(true)
              }}
            >
              Build {BUILD_COMMIT}
            </button>
          </nav>
        </div>
      </footer>
      {showBuildDetails ? (
        <Suspense fallback={null}>
          <BuildDetailsModal
            isOpen={showBuildDetails}
            onClose={() => {
              setShowBuildDetails(false)
            }}
          />
        </Suspense>
      ) : null}
    </>
  )
}
