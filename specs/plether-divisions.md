# Plether Divisions

Divisions replaces the September competition as the active Insights experience.
The goal is to retain testnet traders through consistent, skilled trading and
public status. There is no cash-prize ranking or competition registration gate.

## Schedule

- Launch: Monday 12 October 2026.
- Week one: Sunday 11 October 21:00 UTC, inclusive, to Friday 16 October
  21:00 UTC, exclusive.
- First publication: Monday 19 October 2026 (corrected from the proposed
  Saturday 17 October).
- Subsequent windows repeat every seven days. The last finalized canonical
  block before Friday close is the closing snapshot. Baseline uses the last
  canonical block before Sunday open.
- Badges are calculated over the weekend; published results and announcements
  appear Monday. Publication is allowed from 00:00 UTC Monday, with no promise
  of a specific posting hour.

## Scoring

Use complete economic account value, including marked open positions, outstanding
claims, and incurred fees, carry, execution costs and other protocol settlement
adjustments. Values must come from canonical account state, not a sum of limited
trade-history rows. Do not subtract hypothetical future closing costs.

```text
adjusted_equity = opening_equity + external_deposits - external_withdrawals
weekly_pnl = closing_equity - adjusted_equity
weekly_return = weekly_pnl / adjusted_equity
```

The user selected adjustment of starting equity for both deposits and withdrawals.
There is no fixed 100k bankroll requirement and no inherited September fee-share
or cash-prize eligibility restriction. Snapshot economic values are nonnegative;
the exporter clamps negative economic value as the protocol valuation requires.

Rank all traded accounts with positive adjusted equity by exact weekly return,
descending. The integer cross-product comparator avoids rounding. Zero or
negative adjusted equity gives no percentage rank and cannot qualify. Exact
return ties share a competition rank (`1, 1, 3`); address breaks display ties.
Displayed percentages round to two decimals only after ranking.

## Qualification and tiers

Qualify with strictly positive weekly P&L after fees and at least two distinct
active Monday–Friday FX-session days. A session uses the UTC date of the execution
timestamp plus three hours. Count only successful nonzero voluntary opens,
increases, reductions and closes inside the weekly interval. Order commitments,
failures, expiry, deposits, withdrawals, margin additions, liquidation alone and
weekend events do not create active days.

| Tier | Requirement in a qualifying week |
| --- | --- |
| Iron | Automatic on first executed trade |
| Bronze | In profit |
| Silver | Top 50% of profitable traders |
| Gold | Top 20% of profitable traders |
| Platinum | Top 10% of profitable traders |
| Diamond | Top 5% of profitable traders |

The percentile population includes all profitable traders with a valid return,
including wallets that fail the active-day requirement. Round each cutoff up to
a whole trader; shared rank includes exact ties at the cutoff. This is the
implementation's explicit tie/small-population policy.

A qualifying account is promoted at most one tier if its finish meets the next
tier's threshold. Qualification resets its non-qualifying streak even when its
finish does not merit promotion. Iron to Diamond requires five qualifying weeks.
Two consecutive non-qualifying weeks demote one tier and reset the streak.
Inactivity counts, Iron is the floor, and higher-tier retention does not require
repeating the entry percentile every week.

Every prior wallet must have complete weekly data. Missing input aborts
publication; it is never inferred as inactivity. New wallets are initialized at
Iron. The current snapshot publisher exposes those badges at weekly publication;
immediate first-trade badges require the future live account feed.

## Public status and announcements

Leaderboard badges show earned persistent tiers, not the tier implied by one
week's percentile. Wallets may rank without Discord.

Matching Discord roles will require a linked wallet. The `/register` flow links
one Discord account to one owner wallet and its deterministic Trading Account,
with unique constraints on all three identities. It uses Discord OAuth with the
`identify` scope, Turnstile, and a signed one-time wallet challenge. Guild.xyz or
Collab.Land selection, role assignment, and production connection are deferred.
The UI must not imply roles are already assigned. This link does not gate ranking.

The ranking engine prepares:

- Traders of the Week: the first three profitable wallets by weekly percentage
  P&L (the same metric as the leaderboard). Ties use stable address order for
  these three announcement slots and retain shared leaderboard rank.
- Named promotions into Gold, Platinum and Diamond.
- Current Diamond roster, including retained members.

The X/Discord posting integration and dedicated `#ranked` channel are deferred.
Preparing data is not authorization to post it.

## Delivery boundary

The ranking implementation is in `apps/insights/src/divisions`: a pure engine,
validation, publication client, and tier badges. `scripts/publish-divisions.mjs` consumes a
complete finalized `WeeklyInput`, retains source boundary block identifiers in
the publication, and writes `weeks/YYYY-MM-DD.json` plus `current.json`.

An upstream all-account weekly exporter is required for production data. The
existing September API, registered roster and frozen snapshots cannot provide
this feed. The publisher checks format and sequence; it does not independently
verify blockchain hashes or data completeness against the chain. Do not supply
mock inputs to production. Keep source inputs for audit and replay. Changes to
canonicality after publication require an explicit correction process, rather
than silently rewriting public history.
