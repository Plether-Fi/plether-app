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
      <h1 className="mt-3 text-3xl font-semibold sm:text-4xl">Competition methodology</h1>
      <p className="mt-4 max-w-3xl text-base leading-7 text-content-secondary">How Plether calculates standings and determines prize eligibility for the 2026 testnet trading competition.</p>

      <Panel className="mt-8">
        <Rule number="01" title="Starting point">
          <p>Every registered trader receives exactly <strong className="text-content-primary">100,000.00 mock USDC</strong>, either in the opening baseline or as one official allocation before their first trade. The competition is limited to one verified wallet per trader.</p>
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
          <p>Order submissions, failures, expiries, deposits, withdrawals, margin additions, and liquidation by itself do not qualify. Sessions run from 21:00 UTC on the preceding day to 21:00 UTC; markets close for the weekend in line with tracked FX markets.</p>
        </Rule>
        <Rule number="05" title="Timing and prizes">
          <p>Trading opens on <strong className="text-content-primary">Sunday, 13 September 2026 at 21:00 UTC</strong> and closes on <strong className="text-content-primary">Friday, 25 September 2026 at 21:00 UTC</strong>. There is no close-only period; opening and increasing positions remain available until the cutoff.</p>
          <p>Results are published on <strong className="text-content-primary">28 September at 12:00 UTC</strong>. The top five eligible traders receive <strong className="text-content-primary">600.00 / 500.00 / 400.00 / 300.00 / 200.00 USDC</strong>, paid by 3 October at 00:00 UTC. If a trader is disqualified, the next eligible trader moves up.</p>
          <p>Exact final-P&amp;L ties split the combined prizes for the occupied paid places equally. Wallet address is used only to keep tied rows in a stable display order.</p>
        </Rule>
        <Rule number="06" title="Integrity review">
          <p>Standings are provisional until review. Wash trading, wallets under common control, mirrored trading, sybil accounts, circular transfers, outside top-ups, and manufactured qualifying activity may make a participant ineligible.</p>
          <p>Onchain patterns are evidence, not an automatic verdict; final eligibility decisions are reviewed before prizes are announced.</p>
        </Rule>
      </Panel>

      <div className="mt-6 border border-brand-yellow/35 bg-brand-yellow/10 p-5 text-sm leading-6 text-brand-yellow">
        <strong>Data finality:</strong> Insights uses finalized indexed protocol events. The “indexed through block” indicator shows the newest block included in the public view.
      </div>

      <Link to="/" className="mt-8 inline-flex border border-brand-orange bg-brand-orange px-5 py-2.5 text-sm font-semibold hover:bg-brand-peach hover:text-app-bg">View leaderboard</Link>
    </article>
  )
}
