import { useState } from 'react'
import { Link } from 'react-router-dom'
import { BUILD_COMMIT } from '../../config/buildInfo'
import { BuildDetailsModal } from './BuildDetailsModal'

export function Footer() {
  const [showBuildDetails, setShowBuildDetails] = useState(false)
  const footerLinkClass = "transition-colors hover:text-content-primary hover:underline hover:underline-offset-4"

  return (
    <>
      <footer className="hidden lg:block border-t border-brand-border/30 bg-surface-panel/50">
        <div className="max-w-7xl mx-auto px-6 lg:px-8 py-4 flex flex-col sm:flex-row items-center justify-between gap-2 text-xs text-content-secondary">
          <span>&copy; 2026 Plether Labs Limited. All rights reserved.</span>
          <nav className="flex gap-4">
            <a href="https://docs.plether.com" target="_blank" rel="noopener noreferrer" className={footerLinkClass}>
              Docs
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
        <BuildDetailsModal
          isOpen={showBuildDetails}
          onClose={() => {
            setShowBuildDetails(false)
          }}
        />
      ) : null}
    </>
  )
}
