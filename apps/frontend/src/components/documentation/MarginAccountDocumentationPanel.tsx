import { type ReactNode } from 'react'
import type { PerpsPendingOrder } from '../../hooks'
import { PerpsAccountPanel } from '../PerpsAccountPanel'
import { PerpsClaimPanel } from '../PerpsClaimPanel'
import { Button, Input, TokenAmount, TokenLabel } from '../ui'

export type MarginAccountDocumentationView =
  | 'overview'
  | 'deposit'
  | 'pending-reservations'
  | 'add-position-margin'
  | 'withdrawal'
  | 'trader-claim'

interface MarginAccountDocumentationPanelProps {
  view: MarginAccountDocumentationView
}

const USDC = 1_000_000n
const OWNER_WALLET = '0x9B2F4e0E78E36D97f91c80D5B1aED422d3C2e741'
const TRADING_ACCOUNT = '0x62A9c44fAbC68B6dE62059E827cE972bD09E6c18'

const pendingOrder = {
  orderId: 72n,
  side: 0,
  direction: 'long',
  sizeDelta: 1_200n * 10n ** 18n,
  marginDeltaUsdc: 1_200n * USDC,
  acceptablePrice: 102_400_000n,
  isReduceOnly: false,
  status: 1,
  estimatedNotionalUsdc: 5_900n * USDC,
  commitTime: BigInt(Math.floor(Date.now() / 1_000) - 42),
  expiryTime: BigInt(Math.floor(Date.now() / 1_000) + 2_958),
} satisfies PerpsPendingOrder

function truncateAddress(value: string): string {
  return `${value.slice(0, 8)}...${value.slice(-6)}`
}

function PanelHeader({
  eyebrow,
  title,
  status,
}: {
  eyebrow: string
  title: string
  status?: ReactNode
}) {
  return (
    <div className="flex items-start justify-between gap-5 border-b border-brand-border/20 px-5 py-4">
      <div>
        <div className="text-xs font-medium uppercase tracking-wide text-content-secondary">{eyebrow}</div>
        <h2 className="mt-1 text-xl font-semibold text-content-primary">{title}</h2>
      </div>
      {status ? <div className="shrink-0">{status}</div> : null}
    </div>
  )
}

function StatusBadge({
  children,
  tone = 'positive',
}: {
  children: ReactNode
  tone?: 'positive' | 'warning' | 'neutral'
}) {
  const toneClass = tone === 'positive'
    ? 'border-positive/40 bg-positive/10 text-positive'
    : tone === 'warning'
      ? 'border-warning/40 bg-warning/10 text-warning'
      : 'border-brand-border/30 bg-app-bg text-content-secondary'

  return (
    <span className={`inline-flex border px-3 py-1.5 text-xs font-semibold ${toneClass}`}>
      {children}
    </span>
  )
}

function Metric({
  label,
  value,
  tone,
  detail,
}: {
  label: string
  value: ReactNode
  tone?: 'positive' | 'warning' | 'negative'
  detail?: string
}) {
  const toneClass = tone === 'positive'
    ? 'text-positive'
    : tone === 'warning'
      ? 'text-warning'
      : tone === 'negative'
        ? 'text-brand-orange'
        : 'text-content-primary'

  return (
    <div className="border border-brand-border/20 bg-app-bg p-4">
      <div className="text-xs font-medium uppercase tracking-wide text-content-secondary">{label}</div>
      <div className={`mt-2 text-xl font-semibold ${toneClass}`}>{value}</div>
      {detail ? <p className="mt-2 text-xs leading-5 text-content-secondary">{detail}</p> : null}
    </div>
  )
}

function SummaryRow({
  label,
  value,
  tone,
}: {
  label: string
  value: ReactNode
  tone?: 'positive' | 'warning' | 'negative'
}) {
  const toneClass = tone === 'positive'
    ? 'text-positive'
    : tone === 'warning'
      ? 'text-warning'
      : tone === 'negative'
        ? 'text-brand-orange'
        : 'text-content-primary'

  return (
    <div className="grid grid-cols-[minmax(0,1fr)_minmax(0,1.35fr)] gap-4 border-b border-brand-border/15 py-3 last:border-b-0">
      <dt className="text-sm text-content-secondary">{label}</dt>
      <dd className={`min-w-0 text-right text-sm font-semibold ${toneClass}`}>{value}</dd>
    </div>
  )
}

function AddressValue({ value }: { value: string }) {
  return (
    <span className="font-mono text-xs" title={value}>
      {truncateAddress(value)}
    </span>
  )
}

function OperationStatus({
  stage,
  title,
  message,
  tone,
}: {
  stage: string
  title: string
  message: string
  tone: 'positive' | 'warning'
}) {
  const toneClass = tone === 'positive'
    ? 'border-positive/40 bg-positive/10'
    : 'border-warning/40 bg-warning/10'

  return (
    <article className={`border p-4 ${toneClass}`}>
      <div className="flex items-start justify-between gap-3">
        <div>
          <div className="text-xs font-medium uppercase tracking-wide text-content-secondary">{stage}</div>
          <h3 className="mt-1 text-base font-semibold text-content-primary">{title}</h3>
        </div>
        <span className={`mt-1 h-2.5 w-2.5 shrink-0 rounded-full ${
          tone === 'positive' ? 'bg-positive' : 'bg-warning'
        }`} />
      </div>
      <p className="mt-3 text-sm leading-5 text-content-secondary">{message}</p>
    </article>
  )
}

function AmountInput({
  label,
  value,
  max,
}: {
  label: string
  value: string
  max: string
}) {
  return (
    <div>
      <Input
        label={label}
        value={value}
        readOnly
        rightElement={<TokenLabel token="USDC" />}
      />
      <div className="mt-2 flex justify-end">
        <button
          type="button"
          className="text-xs font-semibold text-content-secondary"
        >
          Max: <TokenAmount amount={max} />
        </button>
      </div>
    </div>
  )
}

function Overview() {
  return (
    <div className="space-y-5">
      <section className="border border-brand-border/30 bg-surface-panel">
        <PanelHeader
          eyebrow="Trading Account"
          title="Margin Account"
          status={<StatusBadge>Account healthy</StatusBadge>}
        />
        <div className="space-y-4 p-5">
          <div className="grid gap-3 md:grid-cols-3">
            <Metric
              label="Available to Trade"
              value={<TokenAmount amount="848.25" />}
              tone="positive"
              detail="Free, unreserved collateral"
            />
            <Metric
              label="Portfolio value"
              value={<TokenAmount amount="1 248.25" />}
              detail="Account equity after unrealized PnL"
            />
            <Metric
              label="Withdrawable"
              value={<TokenAmount amount="648.25" />}
              detail="Maximum currently eligible for withdrawal"
            />
          </div>
          <dl className="border border-brand-border/20 bg-app-bg px-4">
            <SummaryRow label="Trading Account" value={<AddressValue value={TRADING_ACCOUNT} />} />
            <SummaryRow label="Pending order margin" value={<TokenAmount amount="0.00" />} />
            <SummaryRow label="Reserved execution reward" value={<TokenAmount amount="0.00" />} />
          </dl>
        </div>
      </section>

      <section className="border border-brand-border/30 bg-surface-panel">
        <PanelHeader
          eyebrow="Current Position"
          title="Long plDXY Perp"
          status={<StatusBadge tone="neutral">5.00x leverage</StatusBadge>}
        />
        <div className="grid gap-3 p-5 md:grid-cols-4">
          <Metric label="Position margin" value={<TokenAmount amount="400.00" />} />
          <Metric label="Maintenance margin" value={<TokenAmount amount="20.00" />} />
          <Metric label="Unrealized PnL" value={<TokenAmount amount="+48.25" />} tone="positive" />
          <Metric label="Pending carry" value={<TokenAmount amount="1.25" />} />
        </div>
      </section>
    </div>
  )
}

function Deposit() {
  return (
    <section className="border border-brand-border/30 bg-surface-panel">
      <PanelHeader
        eyebrow="Margin Account"
        title="Deposit USDC"
        status={<StatusBadge>Network gas sponsored</StatusBadge>}
      />
      <div className="space-y-5 p-5">
        <div className="grid gap-3 md:grid-cols-2">
          <Metric
            label="Owner-wallet balance"
            value={<TokenAmount amount="25 000.00" />}
            detail={truncateAddress(OWNER_WALLET)}
          />
          <Metric
            label="Trading Account balance"
            value={<TokenAmount amount="0.00" />}
            detail={truncateAddress(TRADING_ACCOUNT)}
          />
        </div>

        <AmountInput label="Deposit amount" value="10 000" max="25 000.00" />

        <div className="grid gap-3 md:grid-cols-2">
          <OperationStatus
            stage="Authorization status"
            title="Awaiting wallet authorization"
            message="Sign an exact 10 000 USDC transfer authorization from the owner wallet to the Trading Account."
            tone="warning"
          />
          <OperationStatus
            stage="Sponsored-operation status"
            title="Ready after authorization"
            message="Plether will receive, approve and deposit the complete amount into the Margin Account as one atomic batch."
            tone="positive"
          />
        </div>

        <dl className="border border-brand-border/20 bg-app-bg px-4">
          <SummaryRow label="Source" value="Owner wallet → Trading Account" />
          <SummaryRow label="Destination" value="Trading Account's Margin Account" />
          <SummaryRow label="Margin Account after deposit" value={<TokenAmount amount="15 000.00" />} />
        </dl>

        <Button className="w-full" size="lg">Authorize and Deposit</Button>
      </div>
    </section>
  )
}

function PendingReservations() {
  return (
    <div className="space-y-5">
      <section className="border border-brand-border/30 bg-surface-panel">
        <PanelHeader
          eyebrow="Margin Account"
          title="Pending reservations"
          status={<StatusBadge tone="warning">Order #72 pending</StatusBadge>}
        />
        <div className="grid gap-3 p-5 md:grid-cols-4">
          <Metric
            label="Available before commit"
            value={<TokenAmount amount="5 000.00" />}
          />
          <Metric
            label="Pending order margin"
            value={<TokenAmount amount="-1 200.00" />}
            tone="warning"
            detail="Reserved for the opening order"
          />
          <Metric
            label="Execution reward"
            value={<TokenAmount amount="-0.20" />}
            tone="warning"
            detail="Reserved for terminal processing"
          />
          <Metric
            label="Available to Trade"
            value={<TokenAmount amount="3 799.80" />}
            tone="positive"
            detail="After both reservations"
          />
        </div>
      </section>

      <PerpsAccountPanel
        initialTab="openOrders"
        isConnected
        pendingOrders={[pendingOrder]}
      />
    </div>
  )
}

function AddPositionMargin() {
  return (
    <section className="border border-brand-border/30 bg-surface-panel">
      <PanelHeader
        eyebrow="Position"
        title="Edit Position Margin"
        status={<StatusBadge>Sponsored account action</StatusBadge>}
      />
      <div className="space-y-5 p-5">
        <div className="grid gap-3 md:grid-cols-2">
          <Metric
            label="Available to Trade"
            value={<TokenAmount amount="848.25" />}
            tone="positive"
          />
          <Metric
            label="Current position margin"
            value={<TokenAmount amount="400.00" />}
          />
        </div>

        <AmountInput label="Amount to add" value="300" max="848.25" />

        <dl className="border border-brand-border/20 bg-app-bg px-4">
          <SummaryRow label="Amount being added" value={<TokenAmount amount="+300.00" />} tone="positive" />
          <SummaryRow label="Resulting position margin" value={<TokenAmount amount="700.00" />} />
          <SummaryRow label="Current leverage" value="5.00x" />
          <SummaryRow label="Resulting leverage" value="2.86x" tone="positive" />
          <SummaryRow label="Exposure" value="Unchanged" />
        </dl>

        <p className="border border-positive/30 bg-positive/10 px-4 py-3 text-sm leading-5 text-content-secondary">
          This action moves USDC from Available to Trade into Position margin. It is immediate after sponsored-operation confirmation and does not enter the delayed-order queue.
        </p>

        <Button className="w-full" size="lg">Authorize Add Margin</Button>
      </div>
    </section>
  )
}

function Withdrawal() {
  return (
    <section className="border border-brand-border/30 bg-surface-panel">
      <PanelHeader
        eyebrow="Margin Account"
        title="Withdraw USDC"
        status={<StatusBadge>Network gas sponsored</StatusBadge>}
      />
      <div className="space-y-5 p-5">
        <div className="grid gap-3 md:grid-cols-2">
          <Metric
            label="Withdrawable"
            value={<TokenAmount amount="3 200.00" />}
            tone="positive"
            detail="Current wallet-out limit"
          />
          <Metric
            label="Current Margin Account"
            value={<TokenAmount amount="8 750.00" />}
          />
        </div>

        <AmountInput label="Requested withdrawal" value="1 500" max="3 200.00" />

        <dl className="border border-brand-border/20 bg-app-bg px-4">
          <SummaryRow label="Trading Account" value={<AddressValue value={TRADING_ACCOUNT} />} />
          <SummaryRow label="Verified owner-wallet recipient" value={<AddressValue value={OWNER_WALLET} />} />
          <SummaryRow label="Requested amount" value={<TokenAmount amount="1 500.00" />} />
          <SummaryRow label="Resulting Margin Account balance" value={<TokenAmount amount="7 250.00" />} />
          <SummaryRow label="Transfer route" value="Margin Account → Trading Account → owner wallet" />
        </dl>

        <p className="border border-positive/30 bg-positive/10 px-4 py-3 text-sm leading-5 text-content-secondary">
          The withdrawal and transfer to the verified owner wallet execute atomically. If either step fails, neither step is applied.
        </p>

        <Button className="w-full" size="lg">Authorize Sponsored Withdrawal</Button>
      </div>
    </section>
  )
}

function TraderClaim() {
  return (
    <div className="grid gap-5 lg:grid-cols-[minmax(0,1.2fr)_minmax(320px,0.8fr)]">
      <PerpsClaimPanel
        claimUsdc="800.00"
        status="available"
        tradingAccountAddress={TRADING_ACCOUNT}
        marginAccountUsdc="2 450.00"
      />

      <section className="border border-brand-border/30 bg-surface-panel">
        <PanelHeader
          eyebrow="Settlement route"
          title="Claim destination"
          status={<StatusBadge>Eligible</StatusBadge>}
        />
        <div className="space-y-5 p-5">
          <dl className="border border-brand-border/20 bg-app-bg px-4">
            <SummaryRow label="Claim owner" value={<AddressValue value={TRADING_ACCOUNT} />} />
            <SummaryRow label="Complete claim" value={<TokenAmount amount="800.00" />} />
            <SummaryRow label="Settlement status" value="Available to settle" tone="positive" />
            <SummaryRow label="Destination" value="Trading Account's Margin Account" />
            <SummaryRow label="Margin Account after settlement" value={<TokenAmount amount="3 250.00" />} />
            <SummaryRow label="Network gas" value="Sponsored" tone="positive" />
          </dl>
          <p className="text-sm leading-6 text-content-secondary">
            The connected owner wallet authorizes settlement. The complete claim is credited to the Margin Account; withdrawal to the owner wallet remains a separate sponsored operation.
          </p>
        </div>
      </section>
    </div>
  )
}

export function MarginAccountDocumentationPanel({
  view,
}: MarginAccountDocumentationPanelProps) {
  if (view === 'overview') return <Overview />
  if (view === 'deposit') return <Deposit />
  if (view === 'pending-reservations') return <PendingReservations />
  if (view === 'add-position-margin') return <AddPositionMargin />
  if (view === 'withdrawal') return <Withdrawal />
  return <TraderClaim />
}
