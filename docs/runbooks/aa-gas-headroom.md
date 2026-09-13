# Native AA execution-gas headroom

Policy: `execution-headroom-v3-sepolia-cap3000000-150pct-min100000`. Fresh native preparation applies
`max(ceil(estimate * 1.5), estimate + 100000)` once to Alto's execution gas. A result
above 3,000,000 is rejected, not clipped. The raw estimate must be at most 2,000,000.
This owner-approved increase is restricted
to native Arbitrum Sepolia issuance and frontend response validation. Other gas fields and ETH spending caps remain
unchanged. The padded payload is persisted before reservation/signing. Old signed
operations continue exact-payload submission/recovery; they are never upgraded in
place. Persisted v2 work can resume unchanged when the intent and every non-gas
configuration binding still match. Empty v2 work cannot be resumed under the
retired policy. Applying v3 headroom requires a newly reviewed preparation ID.
Final authorization and frontend validation also enforce Alto's unchanged
5,000,000 aggregate **per-operation** gas ceiling (including verification, PVG
and paymaster gas); the bundle ceiling is separate and unchanged.

## Incident and regression fixture

Reported transaction:
`0xa2be35e8ce226cf9c183d5937b9bd873cde7a956b33262c71f3004093362ed16`,
Arbitrum Sepolia block 308224346. The account's deposit batch exhausted execution
gas inside TerminalNavBookV2. Read-only historical execution failed at 419155 gas
and succeeded at 628733 gas. Neither low worker reserves nor current trading
readiness is evidence of that historical cause.

`scripts/fixtures/aa-deposit-gas-20260912.json` contains public contract bytecode
and touched storage at the end of that block. Additional successful-deposit and
order/protection trace paths expand the touched-slot set. It is NOT a complete
chain state snapshot. No production keys, owner signatures or paymaster
authorizations are stored. Addresses and balances in this test asset are public
historical chain data, not analytics payloads.

The offline test starts its own loopback-only Anvil, installs this public state,
and substitutes deterministic test account/paymaster signers locally. It executes
real handleOps and verifies UserOperationEvent success plus the exact EntryPoint
deposit deduction. Captured Core runtime hashes must match the v1.2.3 manifest.
EntryPoint and paymaster runtime hashes are pinned separately. No live network or
wallet credential is needed by CI. Missing Anvil or fixtures fails rather than skips.

Run `node --test scripts/aa-execution-regression.test.mjs` after installing frontend
dependencies and Foundry v1.5.1. The existing Tests/test job now requires this suite.
Use `ARBITRUM_SEPOLIA_RPC_URL=... node scripts/aa-capture-deposit-regression.mjs
--capture --refresh` only when deliberately refreshing public historical fixtures;
the capture tool never broadcasts. Review fixture changes, not just regenerated
success expectations.

## Pre-verification gas and fees

Alto v1.2.7's `preVerificationGasCalculator.ts` fills fixed-width gas words with
maximum values when computing execution calldata overhead. Arbitrum's L1-fee
estimate separately randomizes gas quantities below 10,000,000, above our reviewed
3,000,000 limit. Padding does not change encoded lengths. Preserve this reviewed
estimator, including its existing PVG buffers and validation, rather than adding
a second preparation RPC. Arbitrum compression/fee variability still requires a
live compatibility gate; local Anvil is not an Arbitrum fee oracle.

## Historical 2.1M execution-cap increase and action-cost audit

The observed valid partial-close estimate was 1,349,330, requiring 2,023,995
after headroom. It exceeded the former 2,000,000 cap. The owner approved 2,100,000
on September 13. The multiplier/minimum are unchanged, so the maximum admitted
fresh raw estimate is 1,400,000; 1,400,001 is rejected rather than clipped.
The versioned preparation fingerprint changes, not existing signed operations.
Backend final authorization and frontend exact-hash validation enforce the same
ceiling for every allowed action, not a close-only bypass. Keeper finalization
has separate gas/funding and is not changed.

The owner chose **offline testing only** for this audit. No live trace profiling
was performed. The mandatory real-EntryPoint regression now profiles each
operation's exact account-execution call separately from total UserOperation gas
(which includes verification, pre-verification and unused-gas penalties).
Prerequisite deposits are separate transactions, not unsupported three-call batches.

Representative pinned-state observations, in execution gas:

| Successful action | Observed execution gas |
| --- | ---: |
| Deposit after prerequisite funding | 174,327 |
| Withdraw margin and transfer to owner | 334,110 |
| Add position margin | 352,825–445,812 |
| Full-close commit | 998,516 |
| Partial-close commit | 1,013,882 |
| Partial-close commit with 1–4 pending orders | 920,657–1,006,108 |
| Counterfactual first deposit | 78,659 (399,438 total UserOperation gas) |

The recorded deployment permits five pending orders. The sixth attempt must
revert `OrderRouter__TooManyPendingOrders()`, not exhaust gas. The test asserts
the queue configuration rather than assuming it matches future deployments.
It also asserts that a close is the most expensive **successful action in this
pinned-state matrix**. Amounts, carry/position state, repeat deposits, factory
creation and storage warming are covered by the accompanying execution tests.

This is **not proof that close commits are globally the most expensive action**.
The snapshot rejects opens, protection creation and protected opens in its frozen
market state; their failure gas must not be ranked as successful-path costs.
Claim-bearing state and active-protection replace/cancel are now covered by the
separate offline lifecycle suite below. Queue depth, storage/account state and market mode can change rankings.
Protected opens add protection work to an order commit, so cannot be assumed
cheaper without positive execution evidence. All action families remain subject
to the same gas and ETH-liability caps; no broader ceiling is approved implicitly.

## September 13 lifecycle and Firefox verification

Run `node scripts/aa-lifecycle-regression.mjs /path/to/plether-core` with Foundry
and solc 0.8.35 already installed. This offline runner exports Core commit
`ffe45937b7f38133133ad292c5435828bf99357d` and its pinned local Forge/OpenZeppelin
submodules into a new temporary directory. It does not modify the Core checkout,
read its `.env`, fetch RPC state, use live keys or deploy to a network.

Six lifecycle tests passed, alongside all 15 captured-bytecode regressions:

- Protected LONG open, pending protection replacement, keeper fill, arming,
  trigger and final close; assert position size, terminal status and released reserves.
- SHORT protection creation, replacement and cancellation; assert exact bounty refund.
- Real claim payout with and without an existing position; assert pool debit,
  clearinghouse credit, and cleared individual/aggregate claim liabilities.
- Duplicate claim rejection and insufficient-liquidity rejection followed by
  successful payout after locally restoring liquidity.
- Explicit demonstration that a protected open fails at the maximum admissible
  raw estimate of 1,400,000 gas. The local trace reports out-of-gas.

These use real captured EntryPoint/account implementation/proxy/factory/paymaster
bytecode and signatures from public deterministic test keys. Core is freshly
instantiated from the exact release's `BasePerpTest` harness (including its legacy
router test adapter); Pyth feeds, mock USDC and authenticated initial claim state
are synthetic. This is a positive Core execution check, **not** live Alto
estimation, KMS, provider or byte-for-byte deployed-Core-state qualification.
The oracle basket is 1.20 rather than 1.00. Key accounts/storage are explicitly
cooled before sponsored execution; these are not global worst-case measurements.

**Protected-open qualification remains blocked by the current gas policy.**
The successful protected-open account execution used 1,545,240 gas in this
fixture (1,709,599 total UserOperation gas including overhead/penalties).
Even 50% headroom on that observed work is 2,317,860, above 2,100,000. A separate
execution at 1,400,000 fails, so this is not just confusing total receipt gas
with execution gas. Passing execution at the cap does not prove the backend can
issue a correctly padded preparation. Do not call closes universally most
expensive or silently increase the cap/decrease headroom. Optimization or another
explicitly reviewed policy change is required before this case qualifies.

Firefox smoke on deployed build `c5c2aec0585e`, Arbitrum Sepolia, closed hours:

- Order 12 reduced SHORT size from 23,900 to 22,800 plDXY; the keeper completed
  execution. Commit: `0x8e2116aac7497a755ca2fce0fb61d6098dd69ab012e1a5a5503ab219f04de542`;
  finalization: `0x6ae03ecce5d58e29ec8dc1cbebb3b70ae8ad65f70ec80d85338f9b1a9df7e35d`.
  Signed execution allowance: 2,089,914. Support reference:
  `a010c4ab-4a9f-4e17-8c06-9501ff7b1bf8`.
- Sponsored withdrawal of 1 mock USDC credited the connected owner (8 to 9)
  and decreased free settlement by 1. Transaction:
  `0xc5b71b39cf5042b7f98d316c7c8e37c6d073fbab0553b52fa1b06736dc2d7e01`;
  support reference: `58590869-c1c3-4b1c-abdf-65953185a052`.
- Public chain receipts contain successful matching UserOperation events for
  both operations; their inclusion blocks were below observed safe block
  `0x1262cc27`. This does not independently attest database reconciliation.

**Refresh/recovery qualification failed.** After refresh, the successful close
became `outcome-unknown` despite retained transaction linkage and completed trade.
A read-only call to the application's AA gateway returned `null` for its exact
UserOperation receipt. `getRecoverySnapshot` relies on the bundler receipt and
does not use the persisted transaction hash to retrieve/validate the canonical
EntryPoint receipt. With an advanced safe nonce, `resolveProtocolOperation`
therefore chooses unknown. Infrastructure config has a 500-block Alto lookup
range and no durable Redis receipt cache; its exact eviction mechanism was not
independently traced. Repair must recover hash-bound canonical/safe event evidence
from the retained transaction or durable backend record, not assume success from
nonce advancement or merely loosen confirmation rules. No automatic resubmission
was performed. The existing historical deposit correctly displayed its verified
out-of-gas diagnostic.

No live protected open was attempted during the frozen window. No spending cap,
Core configuration, provider, funding allowance or deployment changed during
this verification. The two qualification failures above prevent an all-green
release claim despite successful closed-hours execution.

Structured `aa_preparation_gas_headroom` logs contain estimated/prepared call gas
and headroom basis points. `aa_recovery_outcome` exports total gas utilization
against the operation's total gas allowance, not callGasLimit alone. Receipt gas
includes verification, PVG and EntryPoint unused-gas penalties: headroom is not
necessarily free. CloudWatch retains operational data; PostHog receives only
allowlisted ratios, durations, reasons and opaque attempt references.

## Historical failure enrichment

The API background diagnostic worker claims at most four safely reconciled failed
operations, with a five-minute retry lease and a four-second per-item timeout.
It checks the receipt and canonical block around a bounded callTracer request.
Only one exact EntryPoint-to-account calldata match with a continuously failed
out-of-gas call path supports `USER_OPERATION_OUT_OF_GAS`. Caught errors, empty
revert data and high gas use alone do not establish this cause. Unknown execution
reverts stay `USER_OPERATION_REVERTED`. Database compare-and-set completion
deduplicates emission; no budget lock spans RPC. This uses existing observability
v1/v2 columns and introduces no migration or authorization-table change.

`aa_execution_diagnosed` plus `USER_OPERATION_OUT_OF_GAS` drives the new operations
alarm. Traces, payloads, addresses, exact balances and fees are never exported to
PostHog. Diagnostic outages leave signing, recovery and reconciliation unchanged.

## Qualification and rollout

Implemented tests include the exact gas failure/success, repeat deposits, several
amounts, existing-position/carry state, counterfactual creation, two operations
warming storage in one bundle, token balances changing after signing, and genuine
insufficient-token failures. A valid full-position close commit succeeds. Historical
opens reject with `CloseOnlyWindow`, and protection creation rejects with
`ConditionalTriggerFrozen`; those tests must not silently pass on an unrelated
revert. A successful close commit is NOT a completed trade. A full positive trading
and protection matrix, broader claim/debt cases and live qualification must not be
inferred from these tests. A paired identical-state test also confirms that more
unused execution allowance increases the real EntryPoint charge.

Before release readiness, complete the remaining positive execution matrix;
deploy the reviewed backend and frontend to Singapore Sepolia through protected
workflows; verify representative deposits and permitted open/close outcomes and
safe reconciliation; verify telemetry ingestion; then run three 100-preparation
mixed-action runs within existing budgets. Preserve failures/timeouts and report
market-closed gates as pending. Require p50 <=500 ms and p95 <=1 s per warm run.
The builder test requires exactly one nonce RPC and two Alto RPCs, with no extra
simulation in signing/submission. No Core deployment, spending-cap increase,
automatic funding, provider-policy change or removal of recovery is authorized.

If compatibility fails, do not claim release readiness or silently remove the
headroom/raise gas caps. Pause new issuance through the existing reviewed operator
process if necessary while preserving exact-payload recovery and liabilities.

## 3M protected-open and durable-recovery implementation

The owner approved a 3M Sepolia-only execution ceiling after the two qualification
failures above. The historical findings remain recorded; they are not current
claims that this implementation has been deployed.

- The offline Core v1.2.3 lifecycle fixture now executes the protected open at
  **2,317,860**, the exact 50%-padded result for the observed 1,545,240 account-call
  gas. All six lifecycle cases passed, including claims with/without positions,
  liquidity-shortfall recovery, pending/armed protection replacement, cancellation
  and triggering/finalization. The old 1.4M raw-cap failure remains a regression.
  Protected open is the most expensive account call measured in these fixtures,
  not a universal upper bound. The builder additionally verifies padded 2M raw
  estimates reach exactly 3M with unchanged RPC counts. Fake-Alto builder tests
  and offline execution do not establish live Alto/Arbitrum estimation compatibility.
- `canonicalRecovery.ts` verifies a retained transaction against the exact
  EntryPoint event, hash, sender, verified nonce and recorded paymaster. It checks
  the canonical inclusion header and stable safe boundary. A bundle's success
  alone is insufficient. Provider failures/missing or contradictory evidence stay
  inconclusive; they do not erase observed inclusion or authorize another send.
- Native `eth_getUserOperationReceipt` retains the existing client/hash recovery
  authorization. After Alto returns null or an error, it looks up the finalized
  authorization/event linkage, then verifies the chain receipt and canonical/safe
  headers. Unsettled/missing indexed evidence for a retained authorization returns
  an inconclusive error. Both RPC modes retain their configured verification
  behavior; single-provider mode performs one logical check. Recovery remains
  available while issuance is paused and never changes ledger accounting.
- Reconstructed receipts include EntryPoint identity and only that operation's
  logs between its bundle boundaries. Stored JSON extensions are not proof;
  semantic event identity must match. Reads hold no database transaction over RPC.
- `aa-recovery-smoke-20260913.json` contains public receipts and canonical headers
  for the already-submitted close and withdrawal. Both frontend and backend replay
  them without signing or resubmitting. A fresh read also checked both inclusion
  headers below the safe head. This is **not** live database reconciliation proof.
- Recovery diagnostics export an opaque attempt reference, bounded source,
  verification result and stable reason through the existing sanitized pipeline.
  Backend export uses the bounded background queue with 60-second deduplication;
  receipt reads do not await telemetry persistence. Raw receipts, hashes,
  addresses and provider exceptions are excluded from PostHog. Gas-cap rejection
  is identified as `EXECUTION_GAS_CAP_EXCEEDED`.

Release requires separate approval. Deploy the Sepolia frontend supporting the
larger cap/recovery **first**, then the backend; old tabs remain fail-closed and
may need refreshing. No deployment, Core change, mainnet activation, funding
increase or spending-cap increase is part of this implementation.

Local verification on September 13: 1,141 backend unit examples, 1,438 frontend
unit tests, 14 native-AA PostgreSQL integration cases, 15 historical real-AA
execution regressions, six Core lifecycle cases and four real Fluent Bit routing
tests passed. TypeScript build, scoped ESLint, Lua projection privacy fixtures
and whitespace checks passed. The PostgreSQL tests used an isolated local
database, stopped afterward. The new GitHub CI lifecycle step is configured but
has not yet run remotely; local test results are not a deployment result.

Outstanding live gates: refresh and verify the two existing activities become
confirmed with matching backend reconciliation; perform a new permitted
closed-hours close/withdrawal smoke; qualify a live protected open and protection
lifecycle when the market permits; verify PostHog ingestion; repeat the three
100-preparation performance runs within current budgets, retaining failures and
timeouts. Do not bypass weekend restrictions or call offline results a live pass.

Rollback must first pause new issuance and keep the 3M validators and durable
recovery support until every already-issued liability is resolved. Do not deploy
an older 2.1M validator that strands already-signed 3M operations. No migration is
needed; existing authorization, preparation and recovery tables are retained.
