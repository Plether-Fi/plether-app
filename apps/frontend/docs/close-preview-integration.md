# Reservation-aware close review

The frontend uses the additive `CfdClosePreview` at
`0x202A2C5156563Ec4fEF7D3997771bBCa90e98117` on Arbitrum Sepolia.
The execution evaluator, AA manifest and historical v1.2.3 release remain unchanged.
The source deployment packet is pinned separately under `config/perps/close-preview`.
Its exact ABI SHA-256 is checked by both the binding generator and release tests.

## Review behavior

Every prospective close/reduce sample, including the final-bounds pass, calls
`previewClose(engine, order, executor, price, publishTime, bounds)`. The new lens
reads pool depth and the bounty itself. The frontend requires all returned bounties
to match the same-block router read and all commitment-carry values to agree.
Opens retain their original evaluator and leverage/margin adjustment policy.

Close activation requires the expected chain, bytecode hash, existing deployment
bindings and a preview address distinct from the execution evaluator. A failed
verification or preview blocks confirmation; there is no legacy close fallback.
The actual router commit simulation still runs after the assessments.

Commitment carry is shown separately at full USDC precision. Bounty is included
once. Execution bounds use execution economics, without adding commitment carry.
Post-execution settlement is labeled as total internal custody, not free or
withdrawable funds; trader claims are separate. Close funding remains authoritative
in the lens and commitment simulation, including carry paid from position margin.

Slippage edits and new oracle state invalidate preparation. Pending or failed close
reviews do not present engine-lens cost estimates as confirmed close economics.
The engine lens remains available for instantaneous sizing/risk fields.

Failures distinguish post-carry funding shortfalls, bounty accounting invariants,
Solidity arithmetic panics and unavailable review data. Original RPC error causes
and pinned request context remain local via `getPreparationDebugContext`; analytics
receives only stage, function, error classification and a bounded sample label.

## Reproducible checks

From `apps/frontend`:

```sh
node scripts/generate-close-preview-abi.mjs
npx vitest run --project unit
npm run build
ARB_SEPOLIA_RPC_URL=https://sepolia-rollup.arbitrum.io/rpc npx vitest run --project perps-fork src/contracts/closePreview.perps-fork.test.ts
PLETHER_CORE_PATH=/path/to/plether-core ARB_SEPOLIA_RPC_URL=https://sepolia-rollup.arbitrum.io/rpc npm run test:close-preview-fork
```

The fork harness uses core ABI types and forge-std from the close-preview source
checkout. It executes the existing deployed bytecode at block **308941947** (override with `CLOSE_PREVIEW_FORK_BLOCK`);
it does not deploy substitute engines or accounting implementations. Its wallet
funding and oracle responses are controlled only inside the local fork.

The LONG case validates normal opening, close commitment and execution. In the
actual pool snapshot, its VPI rebate offsets action charges, so it is a control,
not the reservation counterexample. The SHORT case reproduces the old evaluator
collecting an extra **200,000 USDC atomic units (0.20 USDC)** from the new bounty.
The new preview matches assessment after actual commitment exactly, and actual
full-close post-state is compared without a rounding tolerance. Self-execution
uses the canonical position account as executor. Partial-close cases cover gains and
losses on both sides, compare the commitment-time assessment exactly, then compare
execution against a fresh committed assessment after the required one-second delay.

The read-only frontend compatibility test uses the packet's public account
`0x434034d4706173a9f9902eb00f1f80e59da3b82a` and full 1,200-token LONG close. It
verifies the deployed lens, runs every frontend price assessment and simulates
commitment at the pinned block. Expected commitment carry is 100 USDC atomic units,
execution carry is zero, and the bounty is 200,000 atomic units.

These checks establish compatibility with the deployed graph. They do not identify
the historical screenshot account or prove all historical preparation failures had
this cause. Oracle construction is stubbed only in the stateful fork tests; production
Pyth integration is unchanged and remains covered by its existing suites.

## Activation record

Frontend activation and post-deployment smoke verification are pending. Do not
interpret the supplemental packet's lens-deployment success as frontend activation.
If unavailable, block close confirmation and investigate; reverting to unreserved
assessment is not a validated recovery path.
