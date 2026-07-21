# Plether Testnet Trading Competition

This document is the canonical product and scoring specification for the first
Plether Insights competition. Public copy may be shorter, but the implementation
must preserve the rules and block semantics below.

## Identity

- Competition slug: `testnet-trading-2026`
- Network: Arbitrum Sepolia (`421614`)
- Starting bankroll: `100,000.000000` official mock USDC
- One registered Plether Trading Account per beneficial trader
- Registration data and private review evidence must not be exposed publicly

The scored address is the Plether Trading Account. When a controlling wallet and
Trading Account differ, the private participant record must retain that mapping
so one trader cannot enter through several accounts.

## Schedule

All boundaries are fixed UTC and use half-open intervals.

- Start, inclusive: `2026-07-20T16:00:00Z` (`18:00` Europe/Warsaw, CEST)
- New-risk trading ends: `2026-08-03T13:00:00Z`
- Risk-reduction grace window: `2026-08-03T13:00:00Z` through `2026-08-03T16:00:00Z`
- Scoring cutoff, exclusive: `2026-08-03T16:00:00Z` (`18:00` Europe/Warsaw, exactly 14 days after start)
- Results date: `2026-08-05`
- Payout deadline: `2026-08-10T16:00:00Z` (within one week of close)

Public wording: **Trading July 20–August 3; results August 5.**

The start block is the first canonical block whose timestamp is greater than or
equal to the start time. The final block is the last canonical block whose
timestamp is strictly less than the scoring cutoff. Store both block hashes and
do not publish final standings until the configured finality delay has passed.

## Ranking

Participants are ranked by unrounded final P&L in six-decimal mock-USDC units.
ROI and other statistics are explanatory and do not affect rank.

For an account snapshot at block `B`:

```text
base_equity(B) =
  if the account has an open position:
    signed netEquityUsdc(B)
  otherwise:
    terminalReachableUsdc(B)

economic_value(B) = max(base_equity(B) + traderClaimBalanceUsdc(B), 0)

final_pnl =
    economic_value(final_block)
  - economic_value(start_block - 1)
  - official_mock_usdc_deposits_during_window
  + official_mock_usdc_withdrawals_during_window
```

Deposits and withdrawals include only flows through the Plether Margin
Clearinghouse using the configured official mock-USDC asset. Adding position
margin is an internal accounting move and is not a deposit.

Each counted flow must retain both the indexed asset and the originating log's
contract address. The latter must equal the competition's configured Margin
Clearinghouse. Legacy activity without independently verifiable emitter
provenance is excluded rather than inferred.

Open positions are marked to the protocol state at the final block. Accrued
carry and already-incurred fees, execution rewards, VPI and frozen-market costs
are reflected in account state. Hypothetical costs of closing an open position
after the cutoff are not deducted.

An order that has not executed by the cutoff is not a competition trade. Its
reservations and the still-open position are valued exactly as represented by
the final account snapshot.

## Prize eligibility

A participant is prize-eligible only when all of these are true:

1. Final P&L is at least `1,000.000000` mock USDC (`+1%` of the fixed bankroll).
2. The account has at least five active FX-session days.
3. The account satisfies the fixed-bankroll and one-trader/one-account rules.
4. The post-competition integrity review marks the participant eligible.

Leaderboard standings remain provisional until the integrity review is closed.
Score and eligibility status are independent fields.

Recommended eligibility states:

- `pending`
- `eligible`
- `under_review`
- `ineligible`

## Active FX-session days

An active day is a distinct Monday–Friday FX session in which the account has
at least one successfully executed, non-zero, voluntary position change:

- open or increase;
- reduce or close.

Use the execution block timestamp, not commitment time. Define the session date
as the UTC date of `execution_timestamp + 2 hours`, matching the protocol's
22:00 UTC session boundary.

The following do not create an active day:

- commitments, pending orders, failures, expiry or cleanup;
- deposits, withdrawals or margin additions;
- liquidation alone;
- executions during the frozen weekend interval.

There are ten possible qualifying session dates: July 20–24 and July 27–31.

## Prizes and ties

- First place: `600` real USDC
- Second place: `300` real USDC
- Third place: `100` real USDC

An ineligible participant is removed from prize ranking and the next eligible
participant moves up. Exact P&L ties split the combined prizes for the occupied
places equally; wallet address is used only for stable display ordering.

## Integrity policy

External mock-USDC top-ups beyond the official allocation are prohibited.
Cash-flow-adjusted scoring prevents a direct score increase, but additional
capital can still provide unfair risk capacity.

Wash activity, substantially synchronized mirrored accounts, circular funding,
and multiple entries under common control trigger review. Correlation is a
review signal rather than an automatic public accusation. Private evidence and
review notes must be auditable; public responses expose only the resulting
eligibility state and an appropriate generic reason.

Before the starting block, every registered account must be verified as flat,
pending-order-free, and limited to the official bankroll, or explicitly reset.

## Data and API requirements

- Persist complete account-ledger snapshots at the baseline, periodically while
  live, and at the final block.
- Every snapshot is keyed by competition, chain, release router, account, block
  number and block hash.
- All wallets in a published snapshot use the same canonical block and mark.
- Preserve exact integers in storage and serialize large values as decimal
  strings in JSON.
- Every leaderboard response exposes its block number, generated time, scoring
  version, and provisional/final state.
- Raw indexed events remain immutable; scores are reproducible projections.
