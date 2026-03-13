import { useEffect, useState } from 'react'
import { createPortal } from 'react-dom'
import { useSettingsStore } from '../stores/settingsStore'

export function RiskDisclaimer() {
  const riskAccepted = useSettingsStore((s) => s.riskAccepted)
  const acceptRisk = useSettingsStore((s) => s.acceptRisk)
  const [checked, setChecked] = useState(false)

  useEffect(() => {
    if (!riskAccepted) {
      document.body.style.overflow = 'hidden'
    }
    return () => {
      document.body.style.overflow = ''
    }
  }, [riskAccepted])

  if (riskAccepted) return null

  return createPortal(
    <div className="fixed inset-0 z-50 flex items-center justify-center">
      <div className="absolute inset-0 bg-cyber-bg/90 backdrop-blur-sm" />

      <div className="relative w-full max-w-lg mx-4 bg-cyber-surface-dark border border-cyber-border-glow/50 shadow-2xl shadow-cyber-border-glow/20">
        <div className="px-6 py-4 border-b border-cyber-border-glow/30">
          <h2 className="text-lg font-semibold text-cyber-text-primary">
            Risk Disclaimer
          </h2>
        </div>

        <div className="p-6 space-y-4 max-h-[60vh] overflow-y-auto text-sm text-cyber-text-secondary">
          <p>
            Plether is a decentralized finance protocol. By using this application,
            you acknowledge and accept the following risks:
          </p>

          <div className="space-y-3">
            <div>
              <h3 className="font-medium text-cyber-text-primary">Smart Contract Risk</h3>
              <p>
                Smart contracts may contain bugs or vulnerabilities. Funds deposited into
                smart contracts could be lost permanently.
              </p>
            </div>

            <div>
              <h3 className="font-medium text-cyber-text-primary">Financial Loss Risk</h3>
              <p>
                You may lose some or all of the funds you interact with through this protocol.
                Past performance does not guarantee future results.
              </p>
            </div>

            <div>
              <h3 className="font-medium text-cyber-text-primary">No Financial Advice</h3>
              <p>
                Nothing on this site constitutes financial, investment, legal, or tax advice.
                You are solely responsible for your own financial decisions.
              </p>
            </div>

            <div>
              <h3 className="font-medium text-cyber-text-primary">Jurisdictional Compliance</h3>
              <p>
                Decentralized finance protocols may be restricted or prohibited in certain
                jurisdictions. You are solely responsible for determining whether your use of
                this protocol complies with applicable laws and regulations in your jurisdiction.
              </p>
            </div>
          </div>

          <p className="text-xs text-cyber-text-secondary/70">
            For a complete overview of risks, please read our{' '}
            <a href="/risk" className="text-cyber-electric-fuchsia hover:underline">
              Risk Disclosure
            </a>.
          </p>
        </div>

        <div className="px-6 py-4 border-t border-cyber-border-glow/30 space-y-3">
          <label className="flex items-center gap-2 cursor-pointer text-sm text-cyber-text-secondary">
            <input
              type="checkbox"
              checked={checked}
              onChange={(e) => setChecked(e.target.checked)}
              className="accent-cyber-electric-fuchsia w-4 h-4"
            />
            I understand and accept the risks described above
          </label>
          <button
            onClick={acceptRisk}
            disabled={!checked}
            className="w-full py-3 bg-cyber-electric-fuchsia text-white font-semibold hover:bg-cyber-electric-fuchsia/80 transition-colors cursor-pointer disabled:opacity-40 disabled:cursor-not-allowed"
          >
            Proceed
          </button>
        </div>
      </div>
    </div>,
    document.body
  )
}
