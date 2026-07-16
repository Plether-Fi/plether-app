import { type ReactNode } from 'react'
import { PERPS_ARBITRUM_SEPOLIA } from '../../contracts/perpsAddresses'
import { Badge, Button, TokenAmount } from '../ui'

export type LpPrototypeView = 'overview' | 'deposit' | 'pending' | 'position' | 'withdraw'

interface LpPrototypePanelProps {
  view: LpPrototypeView
}

interface MetricProps {
  label: string
  value: ReactNode
  detail?: ReactNode
  tone?: string
}

function Metric({ label, value, detail, tone }: MetricProps) {
  return (
    <div className="border border-brand-border/20 bg-app-bg p-4">
      <div className="text-xs font-medium uppercase tracking-wide text-content-secondary">{label}</div>
      <div className={`mt-2 text-xl font-semibold ${tone ?? 'text-content-primary'}`}>{value}</div>
      {detail ? <div className="mt-1 text-xs leading-5 text-content-secondary">{detail}</div> : null}
    </div>
  )
}

function SummaryRow({
  label,
  value,
  detail,
}: {
  label: string
  value: ReactNode
  detail?: ReactNode
}) {
  return (
    <div className="flex items-start justify-between gap-6 border-b border-brand-border/15 py-3 last:border-b-0">
      <dt>
        <div className="text-sm text-content-secondary">{label}</div>
        {detail ? <div className="mt-0.5 text-xs leading-5 text-content-secondary/70">{detail}</div> : null}
      </dt>
      <dd className="max-w-[60%] text-right text-sm font-semibold text-content-primary">{value}</dd>
    </div>
  )
}

function PrototypeLabel() {
  return (
    <div className="flex gap-3 border border-warning/40 bg-warning/10 px-4 py-3 text-warning">
      <span className="material-symbols-outlined mt-0.5 !text-[20px]">construction</span>
      <div>
        <div className="text-xs font-semibold uppercase tracking-wide">Documentation prototype — not live</div>
        <div className="mt-1 text-xs leading-5">
          LP approvals, deposits, epoch actions, share claims and withdrawals are outside the trader
          gas-sponsorship promise. LP users pay native network gas.
        </div>
      </div>
    </div>
  )
}

function ViewHeader({ view }: { view: LpPrototypeView }) {
  const title =
    view === 'overview'
      ? 'Liquidity provider overview'
      : view === 'deposit'
        ? 'Deposit preview'
        : view === 'pending'
          ? 'Pending deposit request'
          : view === 'position'
            ? 'LP position'
            : 'Withdrawal preview'

  return (
    <div className="flex flex-wrap items-start justify-between gap-4 border-b border-brand-border/20 px-5 py-4">
      <div>
        <div className="text-xs font-medium uppercase tracking-wide text-content-secondary">HousePool</div>
        <h2 className="mt-1 text-xl font-semibold text-content-primary">{title}</h2>
      </div>
      <div className="text-right">
        <Badge variant="warning" size="md">Prototype UI</Badge>
        <div className="mt-1.5 text-xs text-content-secondary">Illustrative testnet values</div>
      </div>
    </div>
  )
}

interface TrancheCardProps {
  name: 'Senior' | 'Junior'
  assets: string
  sharePrice: string
  returnLabel: string
  returnValue: string
  risk: string
  riskVariant: 'info' | 'warning'
  withdrawal: string
}

function TrancheCard({
  name,
  assets,
  sharePrice,
  returnLabel,
  returnValue,
  risk,
  riskVariant,
  withdrawal,
}: TrancheCardProps) {
  return (
    <article className="border border-brand-border/25 bg-app-bg">
      <div className="flex items-start justify-between gap-4 border-b border-brand-border/15 px-5 py-4">
        <div>
          <div className="text-lg font-semibold text-content-primary">{name} Vault</div>
          <div className="mt-1 text-xs text-content-secondary">
            {name === 'Senior' ? 'Last-loss · withdraws first' : 'First-loss · residual upside'}
          </div>
        </div>
        <Badge variant={riskVariant}>{risk}</Badge>
      </div>
      <div className="grid grid-cols-2 gap-px bg-brand-border/15">
        <div className="bg-app-bg p-4">
          <div className="text-xs text-content-secondary">Total assets</div>
          <div className="mt-1 text-lg font-semibold text-content-primary">
            <TokenAmount amount={assets} />
          </div>
        </div>
        <div className="bg-app-bg p-4">
          <div className="text-xs text-content-secondary">Share price</div>
          <div className="mt-1 text-lg font-semibold text-content-primary">{sharePrice} USDC</div>
        </div>
        <div className="bg-app-bg p-4">
          <div className="text-xs text-content-secondary">{returnLabel}</div>
          <div className="mt-1 text-lg font-semibold text-positive">{returnValue}</div>
        </div>
        <div className="bg-app-bg p-4">
          <div className="text-xs text-content-secondary">Active oracle-frozen fee</div>
          <div className="mt-1 text-lg font-semibold text-content-primary">0.00%</div>
        </div>
      </div>
      <dl className="border-t border-brand-border/15 px-5">
        <SummaryRow label="Current deposit mode" value={<Badge variant="warning">Pending epoch</Badge>} />
        <SummaryRow
          label="Withdrawal availability"
          value={<TokenAmount amount={withdrawal} amountClassName="text-positive" />}
        />
      </dl>
    </article>
  )
}

function OverviewView() {
  return (
    <>
      <div className="grid gap-3 md:grid-cols-3">
        <Metric label="Canonical HousePool assets" value={<TokenAmount amount="6 300 000" />} />
        <Metric
          label="Maximum live trader liability"
          value={<TokenAmount amount="4 550 000" />}
          detail="Open-position liability ceiling"
        />
        <Metric
          label="Aggregate trader claims"
          value={<TokenAmount amount="180 000" />}
          detail="Recognized protocol liabilities"
          tone="text-warning"
        />
      </div>
      <div className="grid gap-3 md:grid-cols-4">
        <Metric
          label="Total withdrawal reserve"
          value={<TokenAmount amount="4 880 000" />}
          detail="Protected before LP withdrawals"
        />
        <Metric
          label="Free LP liquidity"
          value={<TokenAmount amount="1 420 000" />}
          detail="After reserved liabilities"
          tone="text-positive"
        />
        <Metric
          label="Senior maximum withdrawal"
          value={<TokenAmount amount="1 200 000" />}
          detail="First claim on free liquidity"
          tone="text-positive"
        />
        <Metric
          label="Junior maximum withdrawal"
          value={<TokenAmount amount="220 000" />}
          detail="Above the Senior claim"
          tone="text-positive"
        />
      </div>
      <div className="grid gap-4 lg:grid-cols-2">
        <TrancheCard
          name="Senior"
          assets="4 200 000"
          sharePrice="1.0432"
          returnLabel="Target coupon · not guaranteed"
          returnValue="5.00% APY"
          risk="Lower relative risk"
          riskVariant="info"
          withdrawal="1 200 000"
        />
        <TrancheCard
          name="Junior"
          assets="2 100 000"
          sharePrice="0.9184"
          returnLabel="Historical 30-day change"
          returnValue="+2.84%"
          risk="Higher relative risk"
          riskVariant="warning"
          withdrawal="220 000"
        />
      </div>
      <div className="flex items-center gap-2 border border-brand-border/20 bg-app-bg px-4 py-3 text-xs text-content-secondary">
        <span className="material-symbols-outlined !text-[17px] text-brand-peach">info</span>
        Positions are open, so new deposits use pending epochs. Returns are variable; share value and the
        amount currently withdrawable can both decrease.
      </div>
    </>
  )
}

function VaultVerification() {
  const address = PERPS_ARBITRUM_SEPOLIA.seniorVault

  return (
    <div className="border border-positive/30 bg-positive/10 p-4">
      <div className="flex items-center justify-between gap-3">
        <div className="flex items-center gap-2 text-sm font-semibold text-positive">
          <span className="material-symbols-outlined !text-[18px]">verified</span>
          Verified Senior Vault
        </div>
        <Badge variant="success">Official deployment</Badge>
      </div>
      <a
        className="mt-2 inline-flex items-center gap-1 break-all font-mono text-xs text-content-primary underline decoration-brand-peach/50 underline-offset-4"
        href={`https://sepolia.arbiscan.io/address/${address}`}
      >
        {address}
        <span className="material-symbols-outlined !text-[14px]">open_in_new</span>
      </a>
      <div className="mt-2 text-xs leading-5 text-content-secondary">
        Approval spender: Senior Tranche Vault, not the HousePool or Margin Account.
      </div>
    </div>
  )
}

function DepositView() {
  return (
    <>
      <div className="grid gap-5 lg:grid-cols-[0.8fr_1.2fr]">
        <div className="space-y-4">
          <div>
            <div className="mb-2 text-xs font-medium uppercase tracking-wide text-content-secondary">
              Selected tranche
            </div>
            <div className="grid grid-cols-2 gap-2">
              <button
                type="button"
                className="border border-brand-peach bg-brand-peach px-4 py-3 text-sm font-semibold text-app-bg"
              >
                Senior
                <span className="mt-0.5 block text-xs font-normal">Last-loss</span>
              </button>
              <button
                type="button"
                className="border border-brand-border/30 px-4 py-3 text-sm font-semibold text-content-secondary"
              >
                Junior
                <span className="mt-0.5 block text-xs font-normal">First-loss</span>
              </button>
            </div>
          </div>
          <div className="border border-brand-border/20 bg-app-bg p-4">
            <div className="text-xs text-content-secondary">Deposit amount</div>
            <div className="mt-2 flex items-baseline justify-between gap-3">
              <div className="text-2xl font-semibold text-content-primary">25 000</div>
              <Badge>USDC</Badge>
            </div>
            <div className="mt-3 flex justify-between text-xs text-content-secondary">
              <span>Owner-wallet balance</span>
              <span>42 850 USDC</span>
            </div>
            <div className="mt-1 flex justify-between text-xs text-content-secondary">
              <span>Balance after deposit</span>
              <span>17 850 USDC</span>
            </div>
          </div>
          <VaultVerification />
        </div>

        <div>
          <div className="flex items-center justify-between border border-b-0 border-brand-border/20 bg-app-bg px-4 py-3">
            <div>
              <div className="text-sm font-semibold text-content-primary">Senior deposit summary</div>
              <div className="mt-0.5 text-xs text-content-secondary">Preview before wallet approval</div>
            </div>
            <Badge variant="warning">Pending epoch</Badge>
          </div>
          <dl className="border border-brand-border/20 bg-app-bg px-4">
            <SummaryRow label="Selected tranche" value="Senior · lower relative risk" />
            <SummaryRow label="USDC deposited" value={<TokenAmount amount="25 000" />} />
            <SummaryRow label="Current share price" value="1.0432 USDC" />
            <SummaryRow label="Estimated shares received" value="23 964.12 Senior shares" />
            <SummaryRow label="Deposit mode" value="Pending deposit epoch" />
            <SummaryRow label="Expected activation" value="Epoch #48 · in 1h 24m" />
            <SummaryRow label="Active oracle-frozen surcharge" value="0.00%" />
            <SummaryRow
              label="MockUSDC approval"
              detail="Separate wallet transaction"
              value={<Badge variant="warning">Required</Badge>}
            />
            <SummaryRow label="Network gas" value="LP pays native gas" />
          </dl>
        </div>
      </div>
      <div className="grid gap-3 sm:grid-cols-2">
        <Button variant="secondary" className="w-full">
          <span className="text-content-secondary">1</span>
          Approve Senior Vault
        </Button>
        <Button className="w-full" disabled>
          <span>2</span>
          Request deposit after approval
        </Button>
      </div>
    </>
  )
}

function ProgressStep({
  number,
  label,
  state,
}: {
  number: number
  label: string
  state: 'current' | 'future'
}) {
  return (
    <div
      className={
        state === 'current'
          ? 'border border-warning/50 bg-warning/10 p-3 text-warning'
          : 'border border-brand-border/20 bg-app-bg p-3 text-content-secondary'
      }
    >
      <div className="flex items-center gap-2 text-xs">
        <span
          className={
            state === 'current'
              ? 'flex size-5 items-center justify-center rounded-full bg-warning font-semibold text-app-bg'
              : 'flex size-5 items-center justify-center rounded-full border border-brand-border/30'
          }
        >
          {number}
        </span>
        {state === 'current' ? 'Current state' : 'Next state'}
      </div>
      <div className="mt-2 text-sm font-semibold">{label}</div>
    </div>
  )
}

function PendingView() {
  return (
    <>
      <div>
        <div className="mb-2 text-xs font-medium uppercase tracking-wide text-content-secondary">
          Deposit lifecycle
        </div>
        <div className="grid gap-2 md:grid-cols-4">
          <ProgressStep number={1} label="Pending" state="current" />
          <ProgressStep number={2} label="Active" state="future" />
          <ProgressStep number={3} label="Finalized" state="future" />
          <ProgressStep number={4} label="Shares claimed" state="future" />
        </div>
      </div>

      <div className="grid gap-5 lg:grid-cols-[1.2fr_0.8fr]">
        <dl className="border border-brand-border/20 bg-app-bg px-4">
          <SummaryRow label="Current state" value={<Badge variant="warning">Pending · cancellable</Badge>} />
          <SummaryRow label="Requested USDC" value={<TokenAmount amount="25 000" />} />
          <SummaryRow label="Selected tranche" value="Senior Vault" />
          <SummaryRow label="Assigned epoch" value="#48" />
          <SummaryRow label="Estimated activation" value="In 1h 24m · 18:00 UTC" />
          <SummaryRow label="Funds now held in" value="Senior Vault escrow" />
          <SummaryRow label="Estimated shares" value="23 964.12 · repriced at finalization" />
        </dl>

        <div className="space-y-3">
          <div className="border border-brand-border/20 bg-app-bg p-4">
            <div className="text-xs font-medium uppercase tracking-wide text-content-secondary">
              Available action
            </div>
            <div className="mt-2 text-sm font-semibold text-content-primary">Cancel before activation</div>
            <div className="mt-1 text-xs leading-5 text-content-secondary">
              The escrowed 25 000 USDC returns to the owner wallet. No shares are issued.
            </div>
          </div>
          <div className="border border-brand-border/20 bg-app-bg p-4 text-xs leading-5 text-content-secondary">
            Each LP action is a conventional wallet transaction and uses native gas.
          </div>
        </div>
      </div>

      <div className="grid gap-3 md:grid-cols-3">
        <Button variant="secondary" className="w-full">Cancel request</Button>
        <Button variant="secondary" className="w-full" disabled>Finalize epoch</Button>
        <Button className="w-full" disabled>Claim shares</Button>
      </div>
    </>
  )
}

function PositionView() {
  return (
    <>
      <div className="flex flex-wrap items-center justify-between gap-4 border border-brand-border/20 bg-app-bg p-4">
        <div>
          <div className="flex items-center gap-2">
            <div className="text-lg font-semibold text-content-primary">Senior Vault</div>
            <Badge variant="success">Active</Badge>
          </div>
          <div className="mt-1 text-xs text-content-secondary">Last-loss tranche · withdraws before Junior</div>
        </div>
        <Badge variant="info" size="md">Lower relative risk</Badge>
      </div>

      <div className="grid gap-3 md:grid-cols-3">
        <Metric label="Vault shares held" value="23 964.12" />
        <Metric label="Current USDC value" value={<TokenAmount amount="25 382.70" />} />
        <Metric label="Current share price" value="1.0592 USDC" />
        <Metric
          label="Change since entry"
          value={<TokenAmount amount="+382.70" />}
          detail="+1.53% · unrealized"
          tone="text-positive"
        />
        <Metric
          label="Withdrawable now"
          value={<TokenAmount amount="18 400.00" />}
          detail="Limited by free LP liquidity"
          tone="text-positive"
        />
        <Metric label="Withdrawal cooldown" value="Ready" detail="No active cooldown" tone="text-positive" />
      </div>

      <div className="grid gap-5 lg:grid-cols-[1fr_1fr]">
        <dl className="border border-brand-border/20 bg-app-bg px-4">
          <SummaryRow label="Average entry value" value={<TokenAmount amount="25 000.00" />} />
          <SummaryRow label="Active oracle-frozen fee" value="0.00%" />
          <SummaryRow label="Pending deposit epochs" value="1" />
          <SummaryRow label="Network gas" value="LP pays native gas" />
        </dl>
        <div className="border border-warning/30 bg-warning/10 p-4">
          <div className="flex items-center justify-between gap-4">
            <div className="text-sm font-semibold text-content-primary">Pending epoch #51</div>
            <Badge variant="warning">Activates in 43m</Badge>
          </div>
          <div className="mt-3 flex items-end justify-between gap-4">
            <div>
              <div className="text-xs text-content-secondary">Requested deposit</div>
              <div className="mt-1 text-lg font-semibold text-content-primary">
                <TokenAmount amount="2 500" />
              </div>
            </div>
            <div className="text-right">
              <div className="text-xs text-content-secondary">Estimated shares</div>
              <div className="mt-1 text-sm font-semibold text-content-primary">2 397.44</div>
            </div>
          </div>
          <div className="mt-3 text-xs leading-5 text-content-secondary">
            These funds remain in vault escrow and are not included in the active position above.
          </div>
        </div>
      </div>

      <Button className="w-full">Review withdrawal</Button>
    </>
  )
}

function WithdrawalView() {
  return (
    <>
      <div className="grid gap-5 lg:grid-cols-[0.8fr_1.2fr]">
        <div className="space-y-4">
          <div className="border border-brand-border/20 bg-app-bg p-4">
            <div className="flex items-center justify-between gap-3">
              <div>
                <div className="text-sm font-semibold text-content-primary">Senior Vault</div>
                <div className="mt-0.5 text-xs text-content-secondary">Enter a USDC withdrawal amount</div>
              </div>
              <Badge variant="success">Cooldown ready</Badge>
            </div>
            <div className="mt-5 border-b border-brand-border/40 pb-3">
              <div className="flex items-baseline justify-between gap-3">
                <div className="text-3xl font-semibold text-content-primary">10 000</div>
                <Badge>USDC</Badge>
              </div>
            </div>
            <div className="mt-3 flex justify-between text-xs text-content-secondary">
              <span>Live maximum</span>
              <span className="font-semibold text-positive">18 400.00 USDC</span>
            </div>
            <div className="mt-3 h-1.5 bg-surface-panel">
              <div className="h-full w-[54%] bg-brand-peach" />
            </div>
            <div className="mt-2 flex justify-between text-xs text-content-secondary">
              <span>0</span>
              <span>54% of maximum</span>
              <span>18 400</span>
            </div>
          </div>
          <div className="border border-warning/30 bg-warning/10 p-4">
            <div className="flex items-center gap-2 text-sm font-semibold text-warning">
              <span className="material-symbols-outlined !text-[18px]">ac_unit</span>
              Oracle-frozen fee active
            </div>
            <div className="mt-1 text-xs leading-5 text-content-secondary">
              25.00 USDC remains in the Senior tranche for existing LPs.
            </div>
          </div>
        </div>

        <div>
          <div className="flex items-center justify-between border border-b-0 border-brand-border/20 bg-app-bg px-4 py-3">
            <div>
              <div className="text-sm font-semibold text-content-primary">Withdrawal summary</div>
              <div className="mt-0.5 text-xs text-content-secondary">Live preview before wallet confirmation</div>
            </div>
            <Badge variant="warning">Standard transaction</Badge>
          </div>
          <dl className="border border-brand-border/20 bg-app-bg px-4">
            <SummaryRow label="Total position value" value={<TokenAmount amount="25 382.70" />} />
            <SummaryRow label="Share balance" value="23 964.12 Senior shares" />
            <SummaryRow label="Current maximum withdrawal" value={<TokenAmount amount="18 400.00" />} />
            <SummaryRow label="Requested tranche value" value={<TokenAmount amount="10 000.00" />} />
            <SummaryRow label="Shares burned" value="9 441.09 Senior shares" />
            <SummaryRow label="Withdrawal cooldown" value={<Badge variant="success">Ready</Badge>} />
            <SummaryRow label="Active oracle-frozen fee" value="0.25% · 25.00 USDC" />
            <SummaryRow
              label="Expected owner-wallet receipt"
              value={<TokenAmount amount="9 975.00" amountClassName="text-positive" />}
            />
            <SummaryRow label="Network gas" value="LP pays native gas" />
          </dl>
        </div>
      </div>
      <Button className="w-full">
        Withdraw 9 975.00 USDC to owner wallet
      </Button>
    </>
  )
}

export function LpPrototypePanel({ view }: LpPrototypePanelProps) {
  return (
    <section className="border border-brand-border/30 bg-surface-panel">
      <ViewHeader view={view} />
      <div className="space-y-5 p-5">
        <PrototypeLabel />
        {view === 'overview' ? <OverviewView /> : null}
        {view === 'deposit' ? <DepositView /> : null}
        {view === 'pending' ? <PendingView /> : null}
        {view === 'position' ? <PositionView /> : null}
        {view === 'withdraw' ? <WithdrawalView /> : null}
      </div>
    </section>
  )
}
