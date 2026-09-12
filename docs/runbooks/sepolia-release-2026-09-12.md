# Singapore Sepolia deployment record — 2026-09-12

Target: account `932542905614`, region `ap-southeast-1`, existing
`plether-sepolia` database and cluster. Application commit:
`cd417902fa69259d8adc1ef1945ac889615cbb2f` (Core v1.2.3 bindings).

## Completed preparation

- Retained encrypted pre-release snapshot `plether-sepolia-pre-v123-20260912`.
- Reviewed and applied staged AA infrastructure with native configuration,
  issuance, preparation and submission disabled; Alto/reconciler desired counts
  remain zero. No Core transactions, signer changes or funding transfers.
- Applied the native AA base schema and the preparation, observability v1/v2
  and funding migrations atomically to the existing database.
- Rehearsed the migrations twice on a private snapshot clone. Existing history
  and a synthetic signed liability/reservation survived the second application.
  The synthetic fixture was created only in the clone.
- Live migration verified unchanged history: 36,481 keeper records, 36,561 order
  records and 11 indexer records. The new live authorization table is empty and
  issuance is paused. This does **not** waive reconciliation of retained prior
  authorizations for the existing paymaster before native activation.
- Deleted the temporary rehearsal clone after successful tests; retained the
  source snapshot and operational logs. Deregistered the three completed
  one-off preflight/rehearsal/migration task definitions.

## Backend release

[Run 34683855800](https://github.com/Plether-Fi/plether-app/actions/runs/34683855800)
uses `environment=sepolia`, `bootstrap=false`, `deployment_scope=all`.
Stanley explicitly approved this exact run in the deployment task.

The first attempt stopped before building: the deployment role lacked
`ssm:DescribeParameters`. The Terraform repair adds region-bound metadata
listing, `kms:DescribeKey` for the exact RPC encryption keys, and read-only
inspection of the exact API/reconciler execution-role policies. No decrypt,
signing, secret-value or mutation grants were added. The reviewed repair plan
also restored standard Terraform tags on one existing alarm; it replaced no
resources. Both live preflights passed on retry. Regression verification:
14 mocked Terraform cases and 69 Node release/approval tests passed (including
the frontend health-response regression added below).

The backend workflow completed successfully, including all seven deployment
jobs. The public API reports verified v1.2.3 bindings, manifest version
`perps-aa-arbitrum-sepolia-20260910-v2`, and deployment block `307397196`.
Its health endpoint and load-balancer target pass checks. Expected active
services reached steady state; standalone consolidated-worker duplicates and
the native reconciler remain at their configured zero desired counts.

Frontend [run 34685097355](https://github.com/Plether-Fi/plether-app/actions/runs/34685097355)
was dispatched from the same application commit after backend success.
The frontend run failed **before publication** at its origin health parser:
the existing API returns a JSON-encoded string containing the health object.
The workflow now accepts one legacy JSON wrapper or the proper object, while
still requiring HTTP 200 and `status=ok`. Malformed, unhealthy and multiply
encoded bodies are covered by regression tests and remain rejected. The fix
must land on master before a new frontend dispatch. Local authenticated
malformed-request probes return the intended `400 / INVALID_REQUEST`; the
unauthenticated probe returns `403 / PROXY_AUTH_FAILED`.

Post-deployment logs also show repeated LP settlement simulation failures with
`HousePool__NoLpEpochProgress()` (`0x86cca6b8`). The worker refuses to send that
transaction. A block-pinned read at L2 block `308063656` confirmed a frozen
oracle/FAD window, matured deposits, no matured redemptions, fresh mark, live
withdrawals, zero dependency/operational blocker masks and deposit-deferral
masks `1028` for both tranches. Core v1.2.3 blocks deposit settlement when
`oracleFrozen`; with no redemption epochs to process, its no-progress revert
is consistent with the weekend waiting state. The worker's repeated invariant
error classification remains an observability issue, not a funding diagnosis.
Do not bypass simulation, suppress unrelated failures or assume all exits are
blocked. This PR does not change that worker classification.

## Frontend completion

PR #257 merged as `ade35bcb2ac4c0ef47aab5ef1be8f7889480590c`.
Manual Sepolia frontend [run 34688156651](https://github.com/Plether-Fi/plether-app/actions/runs/34688156651)
completed successfully. The master-push workflow is a separate mainnet-redirect
job, not the Sepolia publication.

Post-publication HTTP smoke checks at `https://app.sepolia.plether.com`:

- Page, referenced JavaScript asset and AA manifest return HTTP 200.
- Direct and proxied API status both return HTTP 200 with verified v1.2.3
  bindings matching the hosted manifest, including deployment block `307397196`.
- The AA proxy accepts a malformed request through authentication and rejects
  it with HTTP 400 / `INVALID_REQUEST`, without preparing or signing anything.
- Backend health returns HTTP 200, and expected active ECS services remain
  steady. Alto and native reconciler desired/running counts remain zero.
- Hosted manifest still selects Pimlico. `/api/perps/v1/readiness` returns
  HTTP 403 because the backend route requires `cfgNativeAaConfig`, which is
  intentionally absent while native configuration is disabled. This is an
  outstanding Trading status limitation, not a passed readiness check.

No new wallet-signed transaction or full sponsored trade was performed during
these HTTP smoke checks. Do not treat deployment completion as proof of
native-AA qualification. Native activation still requires dedicated Alto funding,
signer attestation/owner approval, retained-liability reconciliation and the
release gates in `singapore-sepolia-aa-release.md`.

## Native activation preparation after owner approval

The owner approved a total 0.08 Arbitrum Sepolia ETH allocation to the five
dedicated Alto wallets, with a 0.001 ETH transfer-gas ceiling, and the subsequent
paymaster pause/signer-change/unpause sequence conditioned on successful
attestation and reconciliation. After the requested deployer top-up, all five
transfers confirmed: four executors received 0.01 ETH each and the utility
wallet received 0.04 ETH. Actual aggregate transfer gas was
0.000031068214054 ETH. No additional principal was transferred.

The Singapore KMS public signer is
`0x714F8C5A4e585c1887eA2CC73b64E1cB75c75779`. The reviewed Terraform update
changed only the dormant attestation task definition's expected signer and
the deployment guard. It did not change running services or enable issuance.
Fixed-digest attestation [run 34692630069](https://github.com/Plether-Fi/plether-app/actions/runs/34692630069)
was dispatched from `ade35bcb2ac4c0ef47aab5ef1be8f7889480590c`. Stanley
explicitly approved this exact run. It stopped before attestation at two
configuration compatibility checks:

- The API service configured `LATEST`, although its task was already running
  Fargate 1.4.0. A reviewed Terraform plan pinned only the API service platform
  to 1.4.0, retaining task definition `plether-sepolia:50`, its image and scale.
  The service reached steady state: one running task, zero pending tasks and
  one completed deployment. Mainnet/non-AA platform selection is unchanged.
- The same approved run was retried and passed that check, but ECS omitted
  the disabled optional `enableFaultInjection` field. The workflow incorrectly
  required an explicit `false`. The compatibility repair accepts absence or
  boolean false only; explicit true, null and malformed values remain rejected.
  AWS documents the field as optional and disabled by default in the
  [TaskDefinition API](https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_TaskDefinition.html).

The complete topology filter now has executable regression cases against a
sanitized ECS-shaped fixture, including wrong roles, tags, container topology,
secrets and signer configuration. The corrected workflow must land on master;
a new attestation run requires its own explicit owner approval. No attestation
task or paymaster owner transaction was started by either failed preflight.
Local verification passed 70 targeted Node tests and all 14 mocked Terraform
cases with the canonical Core v1.2.3 overlay. Live qualification is outstanding.

The retained encrypted AA ledger snapshot was copied into Singapore as
`plether-sepolia-aa-recovery-20260912`; a separate private database
`plether-sepolia-aa-recovery-audit` completed restoration. Its read-only audit
found 33 terminal authorizations (19 settled, 14 expired), zero active reserved,
signed or submitted authorizations, zero unexpired signed authorizations and
zero outstanding ledger liability. Ledger reserves equal releases plus actual
charges. It retains 19 UserOperation events, 19 Alto recovery records, 17
preparations and the original reconciler cursor at block 307892415. This is
snapshot evidence, not a fresh canonical-chain reconciliation or an import.
Recovery records and historical diagnostics still need verification/import
before activation; the live AA ledger must not start empty and ignore them.
The live database and original retained snapshot are untouched. Clean up the
temporary audit database after evidence/import verification; retain recovery
assets until the activation procedure is complete. No paymaster owner
transaction has been sent yet, and the hosted manifest still uses Pimlico.

## Continuation after PR #258

PR #258 merged as `ddfb259ea00c7579e17a637eeb6d2acfb32a0560`.
Fixed-digest attestation [run 34695432668](https://github.com/Plether-Fi/plether-app/actions/runs/34695432668)
was dispatched from that exact commit and is awaiting its own explicit owner
approval. The previous run's approval has not been reused.

A private, immutable-image recovery importer was prepared with a read-only
source transaction, exact live/source database guards, lossless PostgreSQL JSON
row transfer, per-authorization ledger checks, canonical receipt verification,
short destination table locks, conflict rejection and unchanged issuance-pause
control. It rehearses inserts inside a transaction that is rolled back before
any live import is permitted. It does not copy stale readiness/funding permission
or overwrite current control records.

Two rehearsal attempts stopped before a destination transaction began. The first
needed an exact UserOperationEvent topic filter because other EntryPoint events
can share the operation hash. The second refused an Alchemy safe head older
than the normal 600-second ceiling. A follow-up read measured a current latest
head and a 760-second-old safe head. This is not permission to extend the
ceiling; fresh canonical evidence and a successful rehearsal are still required.
No recovery rows have been imported and no signer-rotation transaction was sent.

After the safe boundary advanced, the third rehearsal succeeded and rolled
back. It verified all 19 canonical UserOperation receipts and exact preservation
of 33 authorizations, 85 ledger entries, 19 events, 17 preparations, 19 retained
recovery records, both cursor/health rows and six diagnostic mappings. The five
existing Singapore recovery records were preserved; a successful import would
produce 24 recovery records in total. Issuance remained paused.

The commit-mode attempt used the same rehearsed program hash but stopped at
safe-head freshness before opening the destination transaction. No live import
committed. Both attempts have terminal ECS status (rehearsal exit 0, commit
attempt exit 1), and their private evidence is retained for a guarded retry.
Attestation run 34695432668 still awaits its distinct owner approval. The private
audit database remains available pending import; the encrypted Singapore
recovery snapshot is available and must be retained during later clone cleanup.

Stanley subsequently approved attestation run 34695432668 explicitly. The
protected preflight passed, including the prior platform and optional-field
repairs. ECS accepted the exact task definition without a request override but
returned three name-only `containerOverrides` entries (main, init and log router),
plus an empty inference-accelerator list. The workflow incorrectly required an
empty container-override array and rejected this response. Cleanup stopped the
owned task and deregistered its temporary revision; the workflow finished failed
without successful attestation evidence. The paymaster signer remains unchanged.

The follow-up repair validates absence of effective overrides at both startup and
terminal readback: only unique, known container names are accepted, without any
additional container fields. Task-level role/resource changes, unknown fields,
nonempty accelerator changes, null/malformed values and all command/environment
overrides are rejected. No owner gate, capability boundary, command, image,
network rule or signing policy changes. The repaired workflow must land on master
before a new, separately approved attestation run can execute it.
