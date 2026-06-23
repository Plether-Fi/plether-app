import { useState } from 'react'
import { Link } from 'react-router-dom'
import { BUILD_COMMIT } from '../../config/buildInfo'
import { BuildDetailsModal } from './BuildDetailsModal'

export function Footer() {
  const [showBuildDetails, setShowBuildDetails] = useState(false)

  return (
    <>
      <footer className="hidden lg:block border-t border-cyber-border-glow/30 bg-cyber-surface-dark/50">
        <div className="max-w-7xl mx-auto px-6 lg:px-8 py-4 flex flex-col sm:flex-row items-center justify-between gap-2 text-xs text-cyber-text-secondary">
          <span>&copy; 2026 Plether Labs Limited. All rights reserved.</span>
          <nav className="flex gap-4">
            <a href="https://docs.plether.com" target="_blank" rel="noopener noreferrer" className="hover:text-cyber-text-primary transition-colors">
              Docs
            </a>
            <Link to="/terms" className="hover:text-cyber-text-primary transition-colors">
              Terms of Service
            </Link>
            <Link to="/privacy" className="hover:text-cyber-text-primary transition-colors">
              Privacy Policy
            </Link>
            <Link to="/risk" className="hover:text-cyber-text-primary transition-colors">
              Risk Disclosure
            </Link>
            <button
              type="button"
              className="font-mono tabular-nums hover:text-cyber-text-primary transition-colors"
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
