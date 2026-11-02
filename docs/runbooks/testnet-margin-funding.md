# Sepolia direct margin funding

New welcome allocations use three backend transactions: mint 100,000 MockUSDG to
its dedicated faucet signer, approve exactly that amount to the clearinghouse,
then `depositFor(canonicalTradingAccount, amount)`. This is one user action with
no owner ETH, signature, UserOperation, paymaster or account deployment. The
signer needs Arbitrum Sepolia ETH. Manual deposits and existing wallet-mint
allocations retain their sponsored deposit flow.

## Deployment and activation

The additive migration is
`apps/backend/config/migrations/testnet-margin-funding-v1.sql`. Startup applies
it after the legacy faucet schema. The serving database role needs the same
schema creation privileges as existing startup migrations; a restricted runtime
role needs SELECT/INSERT/UPDATE on both funding tables and the legacy allocation
table plus sequence USAGE. The operator audit table also contains sensitive
transaction references; restrict it alongside the signed transaction journal.
Never print raw signed transactions, RPC credentials or database URLs.

`TESTNET_MARGIN_FUNDING_ENABLED` defaults to `false`. In Terraform the input is
`testnet_margin_funding_enabled`; Sepolia backend deployment takes its value from
the GitHub repository variable `TESTNET_MARGIN_FUNDING_ENABLED_SEPOLIA` (strictly
`true` or `false`). Keep Terraform's authoritative value aligned with that
variable. Apply `infra/terraform/sepolia-margin-funding.tfvars` alongside the
current Core v2.0.0 inputs to retain the approved Sepolia activation in future
Terraform deployments. Activation requires the dedicated `FAUCET_PRIVATE_KEY` and existing
faucet origin authentication. Do not share this signer with other services.
The processor validates chain 421614 and the release-pinned token and
clearinghouse runtime hashes before signing each phase.

Deploy the backend first, with issuance disabled, then the frontend, following
AGENTS.md and the GitHub CLI deployment workflow. The frontend deployment must
be dispatched separately for Sepolia. Enable issuance for a controlled canary;
verify one zero-ETH, counterfactual owner/account pair receives exactly
100,000,000,000 `balanceAssets` units. Verify three journaled receipts and one
credited allocation. Repeated clicks must return the same claim ID. Verify
normal manual deposits, withdrawals and legacy claim completion still work.
The AA status response publishes `testnetMarginFundingEnabled` for operations.

To roll back issuance, set the flag false and redeploy the backend. Existing
pending jobs continue; polling and legacy completion remain available. Never
delete allocation markers, repurpose historical mint blocks as deposit blocks,
or remove pending journal rows to force a retry. Disabling issuance does not
allow a second wallet allocation for direct-funded accounts. Keep the additive
schema when rolling back binaries. No foreign key references the legacy faucet
primary key, which older supported binaries recreate at startup.

## API and durable state

Both routes require the existing official-app proxy secret and a validated
client IP. Start requests share the legacy hourly quotas. Status polling has an
independent budget of 60 requests/client/minute and 6,000 globally/minute. Both
routes return `Cache-Control: no-store`. Status accepts only UUID references.
The proxy forwards its secret only on the exact supported paths.

* `POST /api/testnet/margin-funding` accepts exactly
  `{ "version": 1, "owner": "0x…", "address": "0x…" }`. The backend verifies
  that `address` is the canonical derived Trading Account for the EOA owner.
* `GET /api/testnet/margin-funding/:claimId` reads the same durable allocation.
  The versioned response includes destination `margin`, beneficiary, token,
  clearinghouse, amount, phase (`mint`, `approval`, `deposit`), status
  (`pending`, `funded`, `failed`, `recovery`), bounded reason and final deposit
  hash. The deposit block is returned in the response metadata after funding.
* An existing wallet allocation returns explicit destination `wallet`, status
  `legacy_wallet`. Only this response permits the frontend's legacy flow.
  An error never falls back to another mint.

Reservation atomically writes a shared eligibility marker and direct claim.
A shared PostgreSQL advisory lock serializes legacy and direct signing. Legacy
reservations block this queue only when their verified signed intent uses the
current signer and chain, or when its scope cannot be determined. Historical
intents proven to use another signer or chain keep their original recovery path
without blocking the current signer. Signed
chain, signer, nonce, target, calldata and raw bytes are committed before
broadcast. Only one submitted direct intent per signer is allowed. Restart or
RPC timeout resumes the identical intent. Receipt advancement requires the
stored hash, canonical block, confirmation depth and exact phase-specific logs.
The final receipt must include one custody transfer, one `Deposit` credit and
one `DepositFor` payer event. `DepositFor` is metadata for that same credit; the
history indexer already records `Deposit` and does not create a second credit.
This change does not activate or reset an Insights competition baseline.

A known reverted transaction becomes `failed`; only an explicit start request
resumes it. Successful minting is never repeated. A deposit retry first refreshes
its exact allowance. Uncertain intent or chain evidence becomes `recovery`,
retains its nonce reservation and requires operator investigation. Do not infer
failure from age, a missing receipt, `nonce too low`, or a wallet balance.

## Proven nonce replacement

Use `recover-margin-funding-nonce.mjs` for direct claims; the older
`recover-faucet-nonce.mjs` remains exclusively for legacy mint transactions.
Run in a short-lived reviewed backend task with DATABASE_URL, PERPS_RPC_URL and
its PERPS_RPC_AUTH_TOKEN. No signing key is needed. The independent second RPC
is fixed to public Arbitrum Sepolia, and the primary must have a different host.

```sh
node /app/scripts/recover-margin-funding-nonce.mjs ORIGINAL_HASH REPLACEMENT_HASH --check
node /app/scripts/recover-margin-funding-nonce.mjs ORIGINAL_HASH REPLACEMENT_HASH --apply
```

The tool decodes and recovers the original signed intent for its exact phase,
chain, release, signer, nonce, token, beneficiary and amount. Both providers must
prove that the original has no receipt and a different same-signer/same-nonce
transaction is canonical at or below `safe`. Only reverted replacements with
no logs, or empty zero-value self-cancellations with no logs, are accepted.
Successful arbitrary calls remain blocked, including calls that could mint,
approve or credit margin. Obtain separate operator evidence for those cases;
never reset them through this tool.

`--check` is read-only. `--apply` repeats verification, takes the shared signer
lock and atomically records proof, marks the exact unchanged submitted intent
`replaced`, and sets its parent allocation `failed`. The raw intent and all
previous confirmed phases remain intact. Audit insertion failure rolls back
both updates. Repeating a completed repair is a no-op. A later explicit user
retry resumes the failed phase (refreshing approval for a deposit), rather than
minting another allocation. The tool does not submit any transaction.

## Local verification

```sh
PSQL=/path/to/psql FAUCET_TEST_DATABASE_URL=postgresql://… \
  node --test apps/backend/scripts/recover-margin-funding-nonce.test.mjs
# From apps/backend, using an isolated PostgreSQL database and local Anvil fork:
MARGIN_FUNDING_FORK_RPC_URL=http://127.0.0.1:8545 \
PERPS_CRITICAL_PATH_DATABASE_URL=postgresql://… \
  cabal test plether-api-integration-test --test-option=--match \
  --test-option='direct margin funding'
```

The optional fork test refuses a non-localhost or non-Anvil endpoint, snapshots
and restores its local chain, and isolates its database schema. It tests
concurrent reservation, a blocked legacy signer, dropped transactions and
restarts in all three phases, zero owner ETH, an undeployed account and exactly three transactions. Its local proxy drops Anvil's unused
numeric `l1BlockNumber` field; production RPC parsing stays strict.
