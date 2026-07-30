import type { ReactNode } from 'react'
import { Link } from 'react-router-dom'
import { Panel } from '../components/ui'

function Rule({ number, title, children }: { number: string; title: string; children: ReactNode }) {
  return (
    <div className="grid gap-4 border-b border-brand-border/15 px-5 py-6 last:border-b-0 sm:grid-cols-[3rem_1fr] sm:px-7">
      <span className="font-mono text-sm font-semibold text-brand-orange">{number}</span>
      <div><h2 className="text-lg font-semibold">{title}</h2><div className="mt-2 space-y-2 text-sm leading-6 text-content-secondary">{children}</div></div>
    </div>
  )
}

export function MethodologyPage() {
  return (
    <article className="mx-auto max-w-4xl">
      <p className="text-xs font-semibold uppercase tracking-[0.18em] text-brand-peach">Transparent by design</p>
      <h1 className="mt-3 text-3xl font-semibold sm:text-4xl">Insights methodology</h1>
      <p className="mt-4 max-w-3xl text-base leading-7 text-content-secondary">How the protocol explorer distinguishes onchain facts from reconstruction, and how Plether calculates the 2026 testnet competition standings.</p>

      <div id="protocol-parameters">
        <Panel className="mt-8">
          <Rule number="A1" title="Evidence levels">
          <p><strong className="text-positive">Exact</strong> values come from confirmed transaction receipts, raw logs, decoded calldata, or a historical contract read at the stated block.</p>
          <p><strong className="text-brand-peach">Derived</strong> values name their formula and calculation version. <strong className="text-brand-yellow">Block-level delta</strong> compares state at block − 1 and block; it is not attributed to one transaction when several protocol transactions share that block.</p>
          <p>An unavailable field remains empty and carries a machine-readable reason. The explorer never substitutes zero for a failed, truncated, or unsupported historical read.</p>
          </Rule>
          <Rule number="A2" title="Current-release limits">
          <p>The Arbitrum Sepolia launch release predates the richer observability events planned for the next deployment. Its immutable event ledger is exact, while fee decomposition, six-component oracle publish times, some tranche lifecycle details, and transaction-exact pool waterfall allocation may be unreconstructable.</p>
          <p><code>HousePool.cancelPoolConfigProposal()</code> emits no event in this release. Direct pending-state reads remain authoritative, but historical HousePool cancellation rows are explicitly unavailable rather than inferred from a missing proposal.</p>
          <p>Archive-provider gaps are reported as unavailable and do not stop confirmed log indexing. Raw topics and data remain visible so independent analysts can verify or improve a projection.</p>
          </Rule>
          <Rule number="A3" title="Keeper economics">
          <p>“Active keeper” means an address that submitted at least one confirmed successful permissionless protocol action in the selected window.</p>
          <p>The current release exposes liquidation bounties, so those are labelled <strong className="text-content-primary">observed liquidation bounties</strong>. Available exact receipt gas costs and transaction-native values are summed separately in wei; aggregates are marked partial when a required receipt or value is missing. The Pyth component, USDC conversion, and net profit remain unavailable.</p>
          </Rule>
        <Rule number="A4" title="Confirmed state and reorgs">
          <p>Live state is read at one configured confirmed block. Release-scoped block checkpoints identify the newest common ancestor after a hash mismatch; the affected branch is removed and replayed idempotently.</p>
          <p>Every monetary integer is returned as a decimal string with an explicit unit and scale, avoiding browser-number rounding.</p>
          </Rule>
        </Panel>
      </div>

      <Panel className="mt-8">
        <Rule number="01" title="Starting point">
          <p>Every registered trader starts with exactly <strong className="text-content-primary">100,000.00 mock USDC</strong>. The competition is limited to one registered wallet per trader.</p>
        </Rule>
        <Rule number="02" title="Ranking metric">
          <p>Traders are ranked by final net P&amp;L at the closing snapshot. Open positions are marked using the protocol’s official final mark.</p>
          <div className="mt-3 border-l-2 border-brand-orange bg-app-bg/60 px-4 py-3 font-mono text-xs leading-6 text-content-primary sm:text-sm">final P&amp;L = closing account value − starting account value − deposits + withdrawals</div>
          <p>Account value includes collateral, marked open-position P&amp;L, incurred fees, carry, and outstanding trader claims. Pending orders that have not executed do not count.</p>
          <p>Trade-history “realized P&amp;L” and open-position “unrealized P&amp;L” are directional price results before execution fees, VPI, carry, and execution rewards. They can be positive while final net P&amp;L is negative.</p>
        </Rule>
        <Rule number="03" title="Prize eligibility">
          <p>A trader must finish with at least <strong className="text-positive">+1,000.00 USDC (+1%)</strong> and have activity on at least <strong className="text-content-primary">five distinct FX-session days</strong>.</p>
          <p>The threshold is evaluated at full six-decimal USDC precision, not from the rounded percentage shown in the interface.</p>
        </Rule>
        <Rule number="04" title="What counts as an active day">
          <p>An active day contains at least one successfully executed voluntary open, increase, reduction, or close during an FX session.</p>
          <p>Order submissions, failures, expiries, deposits, withdrawals, margin additions, and liquidation by itself do not qualify. Sessions run from 22:00 UTC on the preceding day to 22:00 UTC; markets close for the weekend in line with tracked FX markets.</p>
        </Rule>
        <Rule number="05" title="Timing and prizes">
          <p>Trading closes exactly 14 days after the start, on <strong className="text-content-primary">Monday, 3 August 2026 at 16:00 UTC (18:00 Warsaw)</strong>. Results are reviewed on 5 August.</p>
          <p>The top three eligible traders receive <strong className="text-content-primary">600.00 / 300.00 / 100.00 USDC</strong>, paid within one week of close. If a trader is disqualified, the next eligible trader moves up.</p>
          <p>Exact final-P&amp;L ties split the combined prizes for the occupied paid places equally. Wallet address is used only to keep tied rows in a stable display order.</p>
        </Rule>
        <Rule number="06" title="Integrity review">
          <p>Standings are provisional until review. Wash trading, wallets under common control, mirrored trading, sybil accounts, circular transfers, outside top-ups, and manufactured qualifying activity may make a participant ineligible.</p>
          <p>Onchain patterns are evidence, not an automatic verdict; final eligibility decisions are reviewed before prizes are announced.</p>
        </Rule>
      </Panel>

      <div className="mt-6 border border-brand-yellow/35 bg-brand-yellow/10 p-5 text-sm leading-6 text-brand-yellow">
        <strong>Data confirmation:</strong> Insights uses confirmed indexed protocol events at the configured confirmation depth; it does not call shallow confirmations finality. The “indexed through block” indicator shows the newest block included in the public view.
      </div>

      <div className="mt-8 flex flex-wrap gap-3">
        <Link to="/" className="inline-flex border border-brand-orange bg-brand-orange px-5 py-2.5 text-sm font-semibold hover:bg-brand-peach hover:text-app-bg">Protocol overview</Link>
        <Link to="/competitions/testnet-trading-2026" className="inline-flex border border-brand-border/40 px-5 py-2.5 text-sm font-semibold hover:border-brand-peach">View competition</Link>
      </div>
    </article>
  )
}
