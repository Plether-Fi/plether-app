import { useState } from 'react'
import { useSettingsStore } from '../stores/settingsStore'
import { Button, Modal } from './ui'

export function RiskDisclaimer() {
  const riskAccepted = useSettingsStore((s) => s.riskAccepted)
  const acceptRisk = useSettingsStore((s) => s.acceptRisk)
  const [checked, setChecked] = useState(false)

  return (
    <Modal
      isOpen={!riskAccepted}
      onClose={acceptRisk}
      title="Risk Disclaimer"
      size="lg"
      showCloseButton={false}
      closeOnBackdrop={false}
      closeOnEscape={false}
      bodyClassName="p-0"
    >
      <div className="space-y-4 p-6 text-sm text-content-secondary">
        <p>
          Plether is a decentralized finance protocol. By using this application,
          you acknowledge and accept the following risks:
        </p>

        <div className="space-y-3">
          <div>
            <h3 className="font-medium text-content-primary">Smart Contract Risk</h3>
            <p>
              Smart contracts may contain bugs or vulnerabilities. Funds deposited into
              smart contracts could be lost permanently.
            </p>
          </div>

          <div>
            <h3 className="font-medium text-content-primary">Financial Loss Risk</h3>
            <p>
              You may lose some or all of the funds you interact with through this protocol.
              Past performance does not guarantee future results.
            </p>
          </div>

          <div>
            <h3 className="font-medium text-content-primary">No Financial Advice</h3>
            <p>
              Nothing on this site constitutes financial, investment, legal, or tax advice.
              You are solely responsible for your own financial decisions.
            </p>
          </div>

          <div>
            <h3 className="font-medium text-content-primary">Jurisdictional Compliance</h3>
            <p>
              Decentralized finance protocols may be restricted or prohibited in certain
              jurisdictions. You are solely responsible for determining whether your use of
              this protocol complies with applicable laws and regulations in your jurisdiction.
            </p>
          </div>
        </div>

        <p className="text-xs text-content-secondary/70">
          For a complete overview of risks, please read our{' '}
          <a href="/risk" className="text-brand-orange hover:underline">
            Risk Disclosure
          </a>.
        </p>
      </div>

      <div className="space-y-3 border-t border-brand-border/30 px-6 py-4">
        <label className="flex cursor-pointer items-center gap-2 text-sm text-content-secondary">
          <input
            type="checkbox"
            checked={checked}
            onChange={(e) => {
              setChecked(e.target.checked)
            }}
            className="h-4 w-4 accent-brand-orange"
          />
          I understand and accept the risks described above
        </label>
        <Button
          type="button"
          variant="danger"
          onClick={acceptRisk}
          disabled={!checked}
          className="w-full"
        >
          Proceed
        </Button>
      </div>
    </Modal>
  )
}
