# Plether Testnet Trading Competition

This document is the canonical product and scoring specification for the active
September 2026 Plether Insights competition. The immutable July competition
remains stored under `testnet-trading-2026`; it must never be re-seeded with the
September release addresses.

## Identity

- Competition slug: `testnet-trading-2026-09`
- Network: Arbitrum Sepolia (`421614`)
- Starting bankroll: `100,000.000000` official mock USDC
- One registered Plether Trading Account per beneficial trader
- Registration data and private review evidence must not be exposed publicly

The scored address is the Plether Trading Account. When a controlling wallet and
Trading Account differ, the private participant record must retain that mapping
so one trader cannot enter through several accounts.

## Schedule

All boundaries are fixed UTC and use half-open intervals.

- Start, inclusive: `2026-09-13T21:00:00Z`
- Registration cutoff, exclusive: `2026-09-20T21:00:00Z`
- New-risk and scoring cutoff, exclusive: `2026-09-25T21:00:00Z`
- There is no close-only period; opening and increasing positions remain allowed until cutoff.
- Results publication: `2026-09-28T12:00:00Z`
- Payout deadline: `2026-10-03T00:00:00Z`

Public wording: **Trading September 13–25; results September 28.**

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

1. Final P&L is at least `1.000000` mock USDC.
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
as the UTC date of `execution_timestamp + 3 hours`, matching this competition's
21:00 UTC session boundary. The archived July rules retain their +2-hour mapping.

The following do not create an active day:

- commitments, pending orders, failures, expiry or cleanup;
- deposits, withdrawals or margin additions;
- liquidation alone;
- executions during the frozen weekend interval.

There are ten possible qualifying session dates: September 14–18 and 21–25.

## Prizes and ties

The total prize pool is `2,000` real USDC, awarded across five places:

- First place: `600` real USDC
- Second place: `500` real USDC
- Third place: `400` real USDC
- Fourth place: `300` real USDC
- Fifth place: `200` real USDC

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

At baseline, every registered account must be flat and pending-order-free. It
must either contain exactly 100,000 official mock USDC or be zero and receive
exactly one official 100,000 allocation before its first trade. Extra or
unverifiable deposits are private integrity flags and preclude an eligible
review outcome. A post-baseline allocation is official only when a successful
faucet claim for the exact account, configured token, and amount has a persisted
mint receipt block strictly before the clearinghouse deposit block.

Registration proves ownership of the owner EOA and derives its deterministic
index-0 Plether Trading Account. Deployment or earlier use of that Trading
Account is not itself disqualifying. Only its canonical baseline state and the
competition-period funding provenance determine clean-start integrity.

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
