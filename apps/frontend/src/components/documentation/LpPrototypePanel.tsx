import { type ReactNode } from 'react'
import { Button, TokenAmount } from '../ui'

export type LpPrototypeView = 'overview' | 'deposit' | 'pending' | 'position' | 'withdraw'

interface LpPrototypePanelProps {
  view: LpPrototypeView
}

function Metric({ label, value, tone }: { label: string; value: ReactNode; tone?: string }) {
  return (
    <div className="border border-brand-border/20 bg-app-bg p-4">
      <div className="text-xs text-content-secondary">{label}</div>
      <div className={`mt-2 text-xl font-semibold ${tone ?? 'text-content-primary'}`}>{value}</div>
    </div>
  )
}

function SummaryRow({ label, value }: { label: string; value: ReactNode }) {
  return (
    <div className="flex items-center justify-between gap-4 border-b border-brand-border/15 py-3 last:border-b-0">
      <dt className="text-sm text-content-secondary">{label}</dt>
      <dd className="text-right text-sm font-semibold text-content-primary">{value}</dd>
    </div>
  )
}

function PrototypeLabel() {
  return (
    <div className="border border-warning/30 bg-warning/10 px-3 py-2 text-xs leading-5 text-warning">
      Documentation prototype — LP methods are not included in the trader gas-sponsorship promise.
    </div>
  )
}

export function LpPrototypePanel({ view }: LpPrototypePanelProps) {
  return (
    <section className="border border-brand-border/30 bg-surface-panel">
      <div className="border-b border-brand-border/20 px-5 py-4">
        <div className="text-xs font-medium uppercase tracking-wide text-content-secondary">HousePool</div>
        <h2 className="mt-1 text-xl font-semibold text-content-primary">
          {view === 'overview'
            ? 'Liquidity provider overview'
            : view === 'deposit'
              ? 'Deposit preview'
              : view === 'pending'
                ? 'Pending deposit request'
                : view === 'position'
                  ? 'LP position'
                  : 'Withdrawal preview'}
        </h2>
      </div>

      <div className="space-y-5 p-5">
        <PrototypeLabel />

        {view === 'overview' ? (
          <>
            <div className="grid gap-3 md:grid-cols-3">
              <Metric label="Canonical HousePool assets" value={<TokenAmount amount="6 300 000" />} />
              <Metric label="Free LP liquidity" value={<TokenAmount amount="1 420 000" />} tone="text-positive" />
              <Metric label="Aggregate trader claims" value={<TokenAmount amount="180 000" />} tone="text-warning" />
            </div>
            <div className="grid gap-3 md:grid-cols-3">
              <Metric label="Maximum live trader liability" value={<TokenAmount amount="4 550 000" />} />
              <Metric label="Total withdrawal reserve" value={<TokenAmount amount="4 880 000" />} />
              <Metric label="Senior / Junior max withdraw" value="1.20M / 220k" />
            </div>
            <div className="grid gap-3 md:grid-cols-2">
              <Metric label="Senior share price" value="1.0432 USDC" />
              <Metric label="Junior share price" value="0.9184 USDC" />
            </div>
          </>
        ) : null}

        {view === 'deposit' ? (
          <>
            <div className="grid grid-cols-2 gap-3">
              <button className="border border-[#FFAB96] bg-[#FFAB96] px-4 py-3 font-semibold text-app-bg">Senior</button>
              <button className="border border-brand-border/30 px-4 py-3 font-semibold text-content-secondary">Junior</button>
            </div>
            <dl className="border border-brand-border/20 bg-app-bg px-4">
              <SummaryRow label="Deposit amount" value={<TokenAmount amount="25 000" />} />
              <SummaryRow label="Route" value="Pending epoch" />
              <SummaryRow label="Estimated shares" value="23 964.12" />
              <SummaryRow label="Activation epoch" value="#48" />
              <SummaryRow label="Network gas" value="LP pays native gas" />
            </dl>
            <Button className="w-full">Approve and request deposit</Button>
          </>
        ) : null}

        {view === 'pending' ? (
          <>
            <div className="grid gap-3 md:grid-cols-4">
              {['Request submitted', 'Activation', 'Epoch finalization', 'Claim shares'].map((step, index) => (
                <div key={step} className={`border p-3 text-sm ${
                  index === 0
                    ? 'border-positive/40 bg-positive/10 text-positive'
                    : index === 1
                      ? 'border-warning/40 bg-warning/10 text-warning'
                      : 'border-brand-border/20 bg-app-bg text-content-secondary'
                }`}>
                  <div className="text-xs opacity-70">Step {index + 1}</div>
                  <div className="mt-1 font-semibold">{step}</div>
                </div>
              ))}
            </div>
            <dl className="border border-brand-border/20 bg-app-bg px-4">
              <SummaryRow label="Requested USDC" value={<TokenAmount amount="25 000" />} />
              <SummaryRow label="Tranche" value="Senior" />
              <SummaryRow label="Activation" value="In 17h 24m" />
              <SummaryRow label="Cancellation" value="Available before activation" />
            </dl>
          </>
        ) : null}

        {view === 'position' ? (
          <>
            <div className="grid gap-3 md:grid-cols-4">
              <Metric label="Senior shares" value="23 964.12" />
              <Metric label="Current NAV" value={<TokenAmount amount="25 382.70" />} />
              <Metric label="Unrealized return" value="+1.53%" tone="text-positive" />
              <Metric label="Withdrawal cooldown" value="Ready" tone="text-positive" />
            </div>
            <dl className="border border-brand-border/20 bg-app-bg px-4">
              <SummaryRow label="Loss priority" value="After Junior" />
              <SummaryRow label="Withdrawal priority" value="Before Junior" />
              <SummaryRow label="Active oracle-frozen fee" value="0.00%" />
            </dl>
          </>
        ) : null}

        {view === 'withdraw' ? (
          <>
            <dl className="border border-brand-border/20 bg-app-bg px-4">
              <SummaryRow label="Share balance" value="23 964.12" />
              <SummaryRow label="Tranche NAV" value={<TokenAmount amount="25 382.70" />} />
              <SummaryRow label="Maximum withdrawal" value={<TokenAmount amount="18 400.00" />} />
              <SummaryRow label="Cooldown" value="Ready" />
              <SummaryRow label="Protected reserves" value={<TokenAmount amount="4 880 000" />} />
              <SummaryRow label="Network gas" value="LP pays native gas" />
            </dl>
            <Button className="w-full">Request withdrawal</Button>
          </>
        ) : null}
      </div>
    </section>
  )
}
