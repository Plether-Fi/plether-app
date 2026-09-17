# Close review arithmetic panic reproduction

The screenshot's order values can reproduce the same UI error with an explicitly
synthetic low-settlement account snapshot. This demonstrates a contract failure
path; it does not establish the screenshot account's balance, entry price,
reserves, pending orders, or actual historical cause.

## Screenshot reconstruction

| Input/display | Reconstructed value |
|---|---:|
| Close direction | Short, assumed full close |
| Quantity | 15,927,400 plDXY (159,274 protocol lots) |
| Added margin | 0 USDC |
| Raw basket price | 0.98895064 |
| Displayed dollar-index price | 1.01104936 → 1.0110 |
| Exposure | 16,103,387.576464 → 16,103,387.58 USDC |
| Slippage | 0.1% |
| Raw close limit | 0.98796168 |
| Displayed close limit | 1.01203832 → 1.0120 |
| Estimated 4-bps execution fee at current price | 6,300.564969 → 6,300.6 USDC |

The raw price is inferred from rounded exposure and quantity. Zero added margin
does not imply zero existing position margin. Neither the screenshot nor this
calculation supplies the missing account state. Unavailable maintenance margin
and liquidation price also occur because those rows read opening-preview data;
remaining leverage becomes unavailable when preparation fails.

## Contract calls and result

The script calls the deployed **v1.2.3** planner's `planClose` followed by the
policy evaluator's `evaluateClose` at block **308891788**. Both are read-only
calls; no wallet signature, transaction, fork mutation, or protocol change is
needed. Bytecode and the ABI bundle are checked against the repository's release
manifest. `assessOrder`, which the frontend uses, gathers a real chain snapshot
and invokes the same planning/evaluation internals. The reproduction supplies a
synthetic snapshot directly rather than reproducing that account-state read.

Assumptions: full short close, entry price equal to the inferred current price,
no carry, claims, pending orders, action reserve or liquidation reserve, a
one-sided position and a pool with 1 billion USDC. Position margin is fully
locked; free settlement is zero. The cases vary total settlement and locked
position margin together. Reachability of this complete synthetic state through
normal trading has not been established.

| Synthetic settlement/position margin | Current price | Midpoint | Adverse 0.1% limit |
|---|---|---|---|
| 3,000,000 USDC | Pass | Pass | Pass |
| 10,000 USDC | Pass | Pass | Panic 0x11 |

At the adverse limit, the second case has a price loss of approximately
15,751.56 USDC. The full-close planner consumes its available 10,000 USDC pledge,
plans the remaining loss write-off, and returns `valid=true`. The evaluator then
subtracts the 0.20 USDC execution bounty from zero remaining settlement. Checked
unsigned arithmetic reverts with `Panic(0x11)`.

The deployed source is commit `ffe45937b7f38133133ad292c5435828bf99357d`,
`packages/perps/src/CfdOrderPolicyEvaluator.sol:422`:

```solidity
assessment.postSettlementBalanceUsdc = postSettlementBalanceUsdc - executionBountyUsdc;
```

This hypothetical account's planner waives the displayed execution fee because
the synthetic state has no free settlement or action reserve. The 6,300.6 USDC
display therefore matches an estimated fee, not a collected debit in this test.

The RPC returned a specific error:

```text
Panic(17) / Panic(0x11): arithmetic underflow or overflow
0x4e487b710000000000000000000000000000000000000000000000000000000000000011
```

The frontend's error mapper has no user-facing Panic mapping and falls back to
the screenshot's “Commit reverted ... RPC did not return a contract error” text.
A regression test in `src/utils/__tests__/perpsErrors.test.ts` constructs viem's
error from these captured bytes and verifies the exact displayed fallback.
Thus that message can appear even when the RPC supplies decodable revert data.

## Run

From `apps/frontend`, with installed dependencies and the checksum-verified
v1.2.3 ABI release bundle:

```sh
node scripts/reproduce-close-review.mjs /path/to/perps-v1.2.3-arbitrum-sepolia.tar.gz
```

The script asserts that the funded control and the first two low-settlement
prices succeed, while the final case returns a valid plan followed by the
arithmetic panic. Recorded output is in
`close-review-reproduction-2026-09-14.json`.

To determine whether this explains the user's screenshot, replay the actual
Trading Account at the failure block and inspect its position, settlement
buckets, reserves, and the three reviewed assessments. The screenshot was taken
around 18:32 UTC; block 308891788 is the investigation's verification block,
not a claim to be the screenshot's block. Order size alone did not cause the
failure in the funded control.
