# Singapore Sepolia native-AA release

Target: account 932542905614, ap-southeast-1, existing plether-sepolia cluster
and state key plether/sepolia/terraform.tfstate. Core is the existing v1.2.3
deployment in config/perps/arbitrum-sepolia-v2.json. No new Core contracts.
This document prepares a release; it is not deployment or spending approval.

## Policy and merge boundary

Public sponsorship is supported only on chain 421614. The existing
AA_NATIVE_GLOBAL_ROLLOUT_ENABLED setting selects public access; false retains
AA_NATIVE_CANARY_OWNERS. Mainnet remains prohibited. Alchemy-only verification
is explicitly accepted for Sepolia, not independent cross-provider verification.
Alto safe mode and normal simulation/validation are required. RPC tracing or
paymaster compatibility failure blocks public activation; never disable checks
to make the release pass. API ingress remains authenticated and Alto private.

Keep KMS, budgets, rate limits, Core's 60-second window and the maximum
600-second safe-head age. Do not increase caps, clear liabilities or transfer
funds as an activation shortcut. Readiness remains observation-only until tested.

Merging does not deploy the backend. The existing master-push frontend workflow
publishes the mainnet redirect; the actual Sepolia app is manually deployed.
Do not alter these triggers or publish a native manifest during merge preparation.

## Preflight before any approved deployment

Sepolia's sole maintainer may approve their own workflow dispatch. Provision
`backend-admin-sepolia`, `alto-admin-sepolia` and `aa-admin-sepolia` with
Stanley as a required reviewer, `prevent_self_review=false`,
`can_admins_bypass=false`, and exactly the custom branch policy `master`.
This is an explicit Sepolia approval-policy change, not automatic approval:
inspect the exact run SHA and inputs, then require the maintainer's approval
for that run. Do not auto-approve pending deployments or reuse one gate's
approval for another. Mainnet policy is unchanged.

1. Use gh authentication, remote-SHA verification and duplicate-run checks from
   AGENTS.md. Verify AWS_PROFILE=plether identity and the exact Singapore state.
2. Snapshot the existing database and record service/image revisions and deployed
   release bindings. Do not restore another database over the active database.
   Compare a saved plan: reject unexpected delete/replace actions on existing
   VPC, RDS, ALB, IAM, SSM or services. New AA resources require explicit review.
3. Apply sepolia-core-v1.2.3.tfvars.json only as part of the coordinated release,
   after verifying it still matches the canonical manifest. Stop new old-release
   orders, drain outstanding orders, and retain old deployment-scoped history.
   Do not relabel old positions, checkpoints, competition data or journal rows
   as belonging to new contracts. Conflicting unscoped data blocks cutover until
   an explicit migration is reviewed. Start new release indexing at its anchor.
4. Review unresolved paymaster authorizations and pending signed operations,
   including retained recovery records from prior use of the same paymaster.
   Reconcile/import required records without changing their identity or amounts.
   Record a complete canonical deployment anchor; never reset the ledger or
   choose a newer start block merely to skip outstanding liabilities.
5. Attest a Singapore KMS key and verify its recovered signer address. The
   paymaster owner must separately approve any on-chain signer rotation.
   Do not rotate while unexpired old-signer authorizations remain unresolved.
   Verify owner, runtime hash, policy, deposit and signing address on-chain.
6. Revalidate dedicated /plether/sepolia/ Alto keys and existing backend Alchemy
   references without exporting secrets. Review all six funding roles, execution
   reserve bounds and pending liabilities. Funding stays manual.

## Database and deployment ordering

With the database-owner/operator role, validate existing sponsorship schema, then
apply these idempotent additive migrations from apps/backend/config/migrations:

1. aa-preparation-v1.sql
2. aa-observability-v1.sql
3. aa-observability-v2.sql
4. aa-funding-v1.sql

Run against a restored test snapshot first, including a second application.
Existing authorization/recovery tables and records must survive unchanged.
Grant runtime SELECT/INSERT/UPDATE on preparation/diagnostic tables as required,
keeper writes to aa_worker_readiness, API SELECT on aa_funding_observations,
and observer SELECT/INSERT/DELETE on its observation table plus read-only access
to existing worker journals. Runtime roles must not gain schema-owner authority.
Do not grant deletion of unresolved recovery or diagnostic mappings.

Build the approved SHA's backend/log-router images and record immutable digests.
Provision/configure AA and the funding observer with issuance/preparation disabled.
The normal deploy-backend workflow promotes every non-log-router application
container, including plether-funding-monitor. Use deploy-alto for the pinned
bundler image and its safe-mode preflight. No ad-hoc regional build workflow.

Validate reconciler freshness, idle funding, API readiness, origin authentication,
KMS and safe-mode estimation. Test allowlisted issuance before enabling the
explicit public Sepolia setting. Preserve the standard Pimlico manifest until
backend qualification passes; then manually deploy the matching Sepolia frontend
and native manifest. Never auto-fallback after ambiguous native preparation.

## Qualification and rollback

Public exposure still requires explicit security qualification: unsigned
preparation can consume reservation capacity; the API's direct KMS Sign
permission does not enforce database budgets against a compromised API; and
upgradeable accounts can change state between safe verification and inclusion.
Safe mode does not eliminate these risks. Preserve these as release blockers
until reviewed for public testnet use; do not silently waive historical
known-account assumptions or imply mainnet safety. Changes to signer isolation
or on-chain policy, if required by that review, need a separate implementation.

Record sponsored deposit, open and close through final execution and safe budget
reconciliation. Verify telemetry redaction/ingestion after browser closure.
Run three batches of 100 fresh mixed preparations within existing budgets:
each p50 <=500 ms and p95 <=1 second; retain failures/timeouts, measure cold and
retry paths separately. Verify the two-minute idle-modal RPC reduction gate.
Unknown funding or missing tracing support is not a passing result.

Rollback disables new issuance/preparation and readiness blocking, restores
compatible images/manifest, and retains recovery submission plus additive tables.
Changing public access back to an allowlist must not strand existing public
signed operations. Do not delete backups, keys, journals or unresolved mappings.
