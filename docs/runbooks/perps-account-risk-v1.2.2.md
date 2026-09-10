# Perps account-risk display verification

> Historical v1.2.2 verification record. The v1.2.3 carry priority and current
> integration are documented in [the release configuration](../../config/perps/README.md).
> v1.2.3 pays carry from position margin first, then free settlement; the
> free-settlement-only rules below do not apply to the current stack.

## Contract baseline and accounting

Source: `Plether-Fi/plether-core` commit
`d704122c779d4d681d0fa2be517707b7f7df3902` (v1.2.2).
The official `perps-arbitrum-sepolia-v1.2.2.zip` release bundle was verified
against SHA-256
`685a309a2a0296a0ecef99a6efc77ba85d86de7132f3ee992c3cac46efa50b7a`.
ABI compatibility fixtures include the verified `positionEntryCostUsdcAtoms`
and `vpiRebateReserveUsdc` bindings.

The old display included free settlement in price collateral and deducted
negative lifetime VPI. The canonical `buildExactPriceRiskState` instead uses:

```
lots = position size / 1e20
raw notional = lots × min(raw price, CAP_PRICE)
long PnL = exact entry cost − raw notional
short PnL = raw notional − exact entry cost
position equity = position margin + same-account claim + exact PnL
maintenance = floor(raw notional × active maintenance bps / 10,000)
price liquidatable = position equity <= maintenance
```

Displayed plDXY price is `CAP_PRICE − raw price`, with eight decimals. The
integer search includes raw zero, cap, zero maintenance, and equality. A
successful search with no crossing produces `out-of-range`; missing or failed
inputs produce `unavailable`. A boundary retains its raw bigint precision for
the chart and stop-loss validation, with four displayed decimals and an exact
eight-decimal tooltip in the position panel.

Settlement balance is clearinghouse ledger custody, including locked buckets.
Position equity is signed and shown separately only while a position exists.
Position leverage uses raw notional / assigned position margin; Equity
leverage uses raw notional / positive position equity. Neither uses free funds.

Carry is covered only by eligible free settlement. Negative accumulated VPI
must have a sufficient dedicated rebate reserve. Those independent checks can
make an account liquidatable even without a price boundary. The contract's
liquidatable flag remains authoritative; the UI explains verified carry and
reserve deficiencies separately. Liquidation execution uses the applicable
adverse oracle-confidence price, so the threshold is not a promised fill price.

## Snapshot and action behavior

Position, exact entry cost, ledger, engine mark, reserve backing, carry inputs,
risk parameters and FAD state use one dynamic multicall (`batchSize: 0`).
CAP_PRICE is read as an immutable deployment value. Account health uses the
engine mark; the chart's market feed remains independent. Regular background
polling can retain a good snapshot. A failed read, query error or explicit
invalidation removes current risk values. Explicit account refreshes invalidate
before refetching, including after transactions. Account switches cannot reuse
another account's retained position, and a confirmed flat account clears it.

New or replacement stop losses wait for valid risk data. Cancellation and
position closing remain accessible. Add Margin reserves projected carry from
free settlement before offering a maximum and refreshes after execution.
Withdrawal limits still come from the contract and transaction-time validation
remains in place. Settling an open account's claim transfers claim backing to
position margin; a flat account receives free settlement.

## Regression evidence

For 10,000 tokens, 1.0000 entry, 250 USDC margin, 750 USDC free settlement,
no claim and 10 bps maintenance, the verified displayed thresholds are:

| Direction | Raw threshold | Displayed threshold |
| --- | --- | --- |
| Long | 102397603 | 0.97602397 |
| Short | 97597597 | 1.02402403 |

`apps/frontend/src/utils/__tests__/fixtures/perpsRisk.reference.sol` invokes
the unmodified pinned Solidity risk library. Its emitted boundaries are stored
in `perpsRisk.v1.2.2.json`; expected values are not calculated by TypeScript.
The 13 cases cover both sides, claims, added margin, FAD, zero maintenance,
both endpoints, no crossing, and entry-cost dust. The TypeScript tests also
check each boundary and its adjacent healthy tick.

To reproduce the reference vectors, create a temporary Forge project with
Solidity 0.8.35. Copy the pinned `CfdMath.sol`, `CfdTypes.sol` and
`libraries/PositionRiskAccountingLib.sol` into its `src/` directory and the
reference harness into `test/`. Configure these remappings:

```toml
remappings = [
  "@plether/perps/=src/",
  "@openzeppelin/contracts/=/path/to/pinned-core/lib/openzeppelin-contracts/contracts/"
]
```

Run `forge test --match-contract PerpsRiskReferenceTest -vv`. The uint256
maximum sentinel denotes no boundary. Compare each named log with the fixture.

The [local fork suite](../../apps/frontend/scripts/local-perps/README.md)
also executes actual increases and partial reductions that retain entry-cost
dust beyond rounded entry price. Each resulting boundary is checked against
the deployed `previewLiquidation` on adjacent ticks, and signed equity against
the account lens. The profitable partial-close scenario creates a real claim
through a temporary fork-only pool cash shortfall, then verifies that settling
it increases position margin and preserves the combined backing and threshold.

## Completed validation

- Full frontend unit/component suite on master `78e90fb` plus this patch:
  105 files, 1,260 tests passed, including the missing-mark sentinel regression.
- Final snapshot-hook verification after memoization cleanup: 44 tests passed.
- Release ABI compatibility is included in those suites.
- Worker tests: 22 passed; mainnet redirect: 3 passed; deployment validator: 7 passed.
- Solidity reference execution: 13 emitted cases, boundary/adjacent-tick assertions passed.
- Disposable pinned-deployment Anvil fork: both directions passed open-preview
  agreement, exact boundaries and adjacent ticks, deposits versus margin
  additions, withdrawal ceilings, and actual entry-cost rounding after increases
  and partial reductions. Real partial-close claim settlement passed.
- Documentation diagram suite: 9 passed.
- Frontend production build and lint passed.
- Storybook visual review: regression, connected position, margin controls,
  unavailable risk, carry liquidation without a price boundary, and separate balances.

One unrestricted-concurrency unit run timed out while a build was running;
the complete rerun with `--maxWorkers=2` passed. The build retains existing
large-chunk warnings. Fork prerequisites include a public RPC that can serve
the selected fork state; an expired upstream snapshot requires a fresh fork.

This patch changes the perps trader frontend, fixtures and trader guidance.
It does not change contracts, backend behavior, competition accounting or
legacy spot leverage. Deployment is a separate rollout step.
