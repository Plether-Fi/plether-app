# Singapore Sepolia deployment record — 2026-09-12

Target: account `932542905614`, region `ap-southeast-1`, existing
`plether-sepolia` database and cluster. Application commit:
`cd417902fa69259d8adc1ef1945ac889615cbb2f` (Core v1.2.3 bindings).

## Latest activation status

### Owner-approved Sepolia tracing exception — implementation pending deployment

The owner explicitly approved disabling Alto safe mode only on Arbitrum Sepolia
instead of buying custom-tracer infrastructure. Normal operation validation and
all gateway/reconciliation controls stay enabled. The
[exception runbook](alto-sepolia-validation-exception-2026-09-12.md) records the
scope and rollout checks. This supersedes the mandatory-safe-mode instruction
in the earlier status below; it does not yet record a successful deployment or
transaction, and does not clear the previously retained liability.

### Native signing enabled; public cutover blocked by RPC tracing

The owner renewed authorization to continue through activation. The on-chain
paymaster unpause succeeded in transaction
`0xda15ec500ca80fef0c719dff0759c2715f1f07e76af6423c2d006ad47008533c`.
Its block `308176322` was subsequently covered by canonical safe block
`308177077`; `paused()` was false at that safe block. Signer, policy, runtime,
deposit and maximum cost were verified unchanged.

AA admin [run 34707194416](https://github.com/Plether-Fi/plether-app/actions/runs/34707194416)
is **failed**, despite the resume task exiting zero: its short-lived FireLens
container received termination immediately after startup and the expected
CloudWatch audit event was missing. Do not rerun the mutation blindly.
An independent read-only database task `582f6d2bb0954448b3a6019014ca41de`
verified `issuance_paused=false`, null pause reason, a durable resume audit event
at `2026-09-12T17:09:06.685Z` with the exact old reason/operator-note length,
zero outstanding liability, and a four-second reconciliation heartbeat.

Native-enabled API [run 34707457327](https://github.com/Plether-Fi/plether-app/actions/runs/34707457327)
succeeded, but startup attestation remained unavailable while safe confirmation
caught up with the owner unpause. After verifying the safe-state unpause,
[run 34708116849](https://github.com/Plether-Fi/plether-app/actions/runs/34708116849)
succeeded at the same tested `6c0b1a0` image. API revision `plether-sepolia:57`
has issuance, preparation and submission enabled for the existing owner
allowlist; public rollout remains false. Public readiness reports all deposit
components ready. This health snapshot does **not** prove tracer compatibility.

A single one-USDC deposit smoke attempt used existing smart-account test USDC.
The first two harness attempts were explicitly denied (incorrect selector, then
startup unavailable); no signatures were issued for those. The corrected attempt
prepared successfully with KMS signing and client-side binding/hash validation.
Observed cold request time was 3,981 ms, including 3,208 ms gateway time; this is
not a passing latency result. Its exact signed payload is retained privately.
UserOperation `0x4c4111cb5c1cce7ea3a5a25bfbd37f59ce41288906f6da4bde60c85bdbf0e43f`
was rejected during Alto submission, and receipt lookups found no receipt.
Do not discard its reservation or create an automatic replacement; normal safe
reconciliation owns expiry/release.
Read-only task `74771a29b62447f79e6efee48b2ee544` confirmed one submitted
authorization with `235832246496000` wei reserved and a one-second reconciler
heartbeat at safe block `308180310`; no liability was manually cleared.

The reconstructed restricted Alto error is HTTP 400, RPC `-32600`,
`invalid tracer value`. Independent harmless probes confirmed:

- Configured Alchemy: `debug_traceCall` with `callTracer` succeeds; a minimal
  custom JavaScript tracer returns the same `invalid tracer value` error.
- Arbitrum public RPC: `debug_traceCall` is unavailable (`-32601`).
- Pinned Alto v1.2.7 SafeValidator invokes its custom `bundlerCollectorTracer`;
  replacing it with a built-in call tracer would not preserve validation.

Public activation therefore requires a custom-tracer-capable RPC. Do not disable
safe mode or ordinary validation. No new provider purchase or node provisioning
has been authorized in this record. Alchemy documents custom tracers as a
[dedicated-cluster capability](https://www.alchemy.com/blog/introducing-dedicated-clusters).

PR #263 passed all eight checks and merged as
`fae3039d737f2edf4d36aff0fe40e183f009e511`. It selects the native manifest and
preparation version 1 in source; **no manual Sepolia frontend deployment was
dispatched**. The existing master-push run only published the unchanged mainnet
redirect. The hosted Sepolia manifest remains Pimlico pending successful native
submission qualification.

A private six-role/nine-signer funding-observer inventory and saved public
activation plan were reviewed but **not applied**. The observer receives only
database/RPC credentials, no keys. Reserves use the Alto/keeper whole-batch caps,
the live 32-million chain transaction gas limit plus deployed gas buffers for
other workers, and the current six-feed update fee (zero). These are monitoring
estimates, not spending authority. Public rollout, observer live qualification,
telemetry ingestion and performance gates remain outstanding. No funds were
transferred, no caps were increased, and Core was not changed.

### Sepolia safe-head policy correction

After reviewing the shared-provider readings and Arbitrum's parent-chain safe
head derivation, the owner requested fixing the policy. The reviewed fix allows
an explicit 1800-second bound only for Arbitrum Sepolia, independent of cohort
and provider mode; other chains and unset defaults retain 600 seconds. API and
reconciler share the chain-specific ceiling, with matching Terraform/deployment
validation. Existing canonical, future-time, ledger and durable-pause rules are
unchanged. No automatic pause reset or switch to `latest` is introduced.

Local validation passed: 1102 backend unit examples, 76 AA deployment tests and
16 mocked Terraform plans. PR #262 merged as
`6c0b1a02d6fa7db1f9460ff781e66c9bbcd05716`; all eight PR checks passed, including
native-AA and perps integration. A saved, inspected Terraform plan changed only
the API/reconciler task-definition age setting from 600 to 1800. Their running
services were not changed until the matching application image was deployed.

Backend [run 34705459352](https://github.com/Plether-Fi/plether-app/actions/runs/34705459352)
completed successfully at this commit after applying the owner's continuing
Singapore deployment approval to its protected gate. All seven deployment jobs
passed. API revision `plether-sepolia:54`, reconciler revision
`plether-sepolia-aa-reconciler:7` and workers revision `plether-sepolia-workers:49`
are deployed. The application image digest is
`sha256:85105dcf51d388f6453e55284ad04ae04ed834a3b611b750eaf5e257a4efb543`.
Both API and reconciler definitions specify `1800`; Alto remains healthy on
revision 3. Public API health and hosted readiness HTTP checks passed.

Reconciler task `125cdfa972684b929cc0d6790a544b05` started at 16:43:41 UTC and
emitted six healthy heartbeats without error events during the observation.
It advanced from safe block 308167879 to 308169431. At 16:45:32 UTC both RPCs
reported the same canonical safe block about 770–771 seconds old while this
task continued healthy, demonstrating the old false-failure condition no longer
causes a restart. This bounded observation is not a guarantee against future
chain stalls. The existing durable issuance pause and on-chain paymaster pause
were not cleared, native capability was not enabled and the hosted frontend was
not redeployed. Remaining funding-observer/activation qualification is separate.

### Alto startup after explicit continuation approval

The owner subsequently authorized continuing the Singapore deployment and
activation. A newly saved and inspected Terraform plan changed only
`aws_ecs_service.alto[0].desired_count` from zero to one, retaining the exact
workflow-staged revision `plether-sepolia-alto:3`. It applied successfully.
Task `85faf3daac4043c389fc12caa508f336` is running and healthy, its scratch
initializer exited zero, and the private ALB target is healthy. Safe mode remains
true, simulation deployment false and the Alto root filesystem read-only.
Hosted readiness now reports the bundler `READY`.

Activation is still blocked by the existing safe-head age policy. A read-only
comparison at 2026-09-12 15:38:59 UTC found both Alchemy and Arbitrum's public
Sepolia RPC reporting canonical safe block `308152486`, hash
`0x505510a810321dd9ab8d1145b8ea56ef3553e7aa64563b4148d27d1189233753`,
619–620 seconds old. Both latest heads were current (0–1 seconds old).
The second endpoint was used only for diagnosis, not configured as a provider.
This observed pause cannot be attributed solely to Alchemy lag. Do not claim a
provider switch would resolve it or silently extend the approved 600-second
ceiling. The paymaster and issuance remain paused; native capability and hosted
frontend cutover, full funding inventory and qualification are still unfinished.

### Continuation after PR #261

PR #261 merged as `36d97405b36927eb60e50da164916541ef9918c5`.
All eight PR checks subsequently passed. The owner explicitly approved Singapore
Alto staging from this commit with action `deploy`, API hostname
`api.sepolia.plether.com` and unchanged utility cushion `5000000000000000` wei,
keeping Alto stopped and sponsorship disabled. The protected approval was
applied only after the run commit and dispatch fingerprint matched those inputs.

Alto [run 34702304111](https://github.com/Plether-Fi/plether-app/actions/runs/34702304111)
completed successfully. The exact temporary image exception was visible as a
warning; configuration, immutable image, runtime bytecode and wallet funding
checks passed. No simulation contracts were deployed. ECS readback confirmed
`plether-sepolia-alto:3` selected on `plether-alto`, rollout `COMPLETED`, with
desired/running/pending counts all zero. Internal running-service and public AA
smoke checks were intentionally skipped for this stopped-service staging run.

Public API health remains successful. Hosted readiness still reports
`BUNDLER_UNAVAILABLE`, `PAYMASTER_PAUSED` and `SPONSORSHIP_DISABLED`, with
readiness enforcement disabled. No new owner transaction, funding transfer,
Core change or frontend deployment occurred. Native AA is **not activated**.
Starting Alto and verifying safe-mode runtime health are separate next steps.
Reconciler logs reached safe block 308147790 but again reported a stale safe
boundary and restarted; stable reconciliation remains an activation blocker.
The normal 600-second ceiling is unchanged.

The following sections retain the preceding deployment history; statements
about pending approval and failed staging there describe those earlier runs.

### Continuation after PR #260

Subsequent owner decision: the owner explicitly accepted a temporary exception
for CVE-2024-5535 on the exact Sepolia image. The reviewed boundary, expiration,
visibility and remaining risks are recorded in
[the image exception](alto-sepolia-image-exception-2026-09-12.md). The deployment
gate implementation still needs to land and execute successfully; this approval
does not mean Alto or native sponsorship is already running.

PR #260 merged after all eight checks passed, at
`5461d54f3579c53e3ba2583dc8a57df43234eef4`. The owner explicitly approved
Alto [run 34699632732](https://github.com/Plether-Fi/plether-app/actions/runs/34699632732)
and backend [run 34699634144](https://github.com/Plether-Fi/plether-app/actions/runs/34699634144)
at that exact commit.

- Backend deployment completed successfully, including all seven service jobs.
  API revision `plether-sepolia:52`, workers revision
  `plether-sepolia-workers:48`, and reconciler revision
  `plether-sepolia-aa-reconciler:5` use the new approved commit. The application
  image digest is `sha256:6929d05c1a3627787225f23414ac3e78230f84f9d7058eb2d9264a0ff0eb4638`.
  Public API health is HTTP 200, v1.2.3 bindings remain verified, and the hosted
  proxy now returns a versioned readiness snapshot rather than HTTP 403.
- Native configuration is present, but sponsorship, preparation, submission and
  public rollout remain false. Readiness enforcement remains observation-only.
  The paymaster remains paused with the attested Singapore signer. No new owner
  transaction, funding transfer or Core change occurred in this continuation.
- Alto passed the repaired raw-manifest verification and mirrored its exact
  config/ordered-layer identity into ECR as
  `sha256:9db94fbd439a26f01b0ece3cc5f76b3791c1e1660b990d74892274267096f12a`.
  Staging then stopped at the unchanged critical-finding gate, before updating
  the service: ECR reports CVE-2024-5535 for OpenSSL package 3.1.4-r5 as critical.
  [OpenSSL's advisory](https://openssl-library.org/news/secadv/20240627.txt)
  rates the issue low severity, but this is not proof of non-applicability.
  The image runs Node v20.12.2 with OpenSSL 3.0.13+quic; the probe did not establish
  that runtime to be unaffected. No scanner exception or replacement image has
  been approved or implemented. Alto remains at desired/running count zero.
- After the backend workflow staged its reviewed image, a separate Terraform
  plan changed only the reconciler service desired count from zero to one,
  retaining revision 5. Its scratch initializer exited zero and both the
  reconciler and log router started successfully. Canonical cursor advancement
  reached block 308002415 (110,000 blocks beyond the imported cursor), then
  `aa_reconciler_timestamp_invalid` and `aa_reconciler_crashed` were emitted when
  Alchemy's safe head exceeded the normal age ceiling. Saved progress is retained;
  full catch-up and a healthy heartbeat have **not** been verified. The ECS
  service remains configured at one desired task for normal recovery; do not
  describe this as healthy reconciliation or waive the freshness rule.
  Its batch size is the already-supported 10,000 blocks, verified by a read-only
  Alchemy log-range probe; the original cursor/anchor, five-second polling,
  600-second safe-head ceiling and all event/canonical checks are unchanged.
- The owner has been asked to choose between a patched and qualified Alto
  image or a narrowly scoped Sepolia-only exception for the exact finding/image.
  Neither path is presumed approved. Do not scale Alto or enable issuance via
  another route to bypass the failed scan.

The earlier status below describes preceding milestones, not the current
service scale or the final outcome of these two runs.

PR #259 merged as `f5708225e6da9ce0ba559e538a7036a45df0e697`.
The owner approved merging further deployment bugfixes after their checks pass;
this does not replace the exact-run protected deployment approvals below.

- KMS attestation [run 34696997565](https://github.com/Plether-Fi/plether-app/actions/runs/34696997565)
  passed after explicit approval. The isolated task verified both the public key
  and fixed-digest signature for Singapore signer
  `0x714F8C5A4e585c1887eA2CC73b64E1cB75c75779`.
- The retained recovery import committed successfully, then passed a separate
  read-only exact-row check. It preserves 33 terminal authorizations, 85 ledger
  entries, 19 canonical UserOperation events, 17 preparations, 19 retained
  recovery records, six diagnostic records and the original cursor/health rows.
  The destination has 28 recovery records, including its nine pre-existing
  records. Outstanding sponsorship liability is zero; issuance remains paused.
  The immutable importer hash is
  `d33042e914460ed21217d6f37782d1a4aecbcce7d0fa00753e6d3f2b2d72afa1`.
  Import task: `746429c1c6eb4f339937e59c7ad8f265`; successful readback task:
  `51c59e8b195e47b4b7297b6325995b7a`. Both exited zero. Task-scoped CloudWatch
  evidence is retained under `/ecs/plether-sepolia`.
- Canonical verification retained the normal 600-second safe-head ceiling.
  Earlier failed attempts stopped before writes; the committed import verified
  all 19 events and cursor hash using safe block `308127943`. The subsequent
  database readback used read-only transactions without requiring another RPC
  verification or changing the previously verified importer.
- The owner-approved paymaster pause confirmed in transaction
  `0x0b8ed87c3526762828a2aa318a4ca878e235c54b71e2f92717e2313fb3dfb8fb`.
  Signer rotation confirmed in
  `0xf79d740cb2d308d33376ddeb65f14910dbcc7ec13c4d154b7eebb2db3b47c078`.
  Readback verifies the Singapore signer and `paused=true`. The paymaster
  deposit remains 0.547650471194660308 ETH and the on-chain per-operation cap
  remains 0.01 ETH. No Core changes or further funding transfers occurred.
- Alto staging [run 34696999402](https://github.com/Plether-Fi/plether-app/actions/runs/34696999402)
  initially failed before image publication because simulation addresses were
  unset. The reviewed Terraform repair changes only the dormant Alto task
  definition's two bindings to existing contracts:
  `0x9c3c25a084AE8B1df3B2e82bb07Dac4E115C9Ae1` and
  `0x95CC02A7B69dD46c6DD6Bd56132A24a235D58948`. Both have deployed bytecode.
  No simulation contract was deployed; `ALTO_DEPLOY_SIMULATIONS_CONTRACT`
  remains false. The owner approved the renewed protected gate. That retry
  passed configuration/RPC preflight, then failed before image publication:
  Docker's legacy `manifest inspect` rejected the OCI manifest digest.
  Independent registry-byte hashing and `buildx imagetools inspect --raw`
  both reproduce the exact approved digest. The workflow repair uses the raw
  reader and explicitly hashes both root and selected child manifests; it does
  not change the pinned image or relax mirror/runtime/scanner checks.
  Both viem-based probes also require `/app/src`, the pinned image's package
  workspace, rather than `/app`. The repaired runtime probe verified all three
  expected on-chain bytecode hashes locally; wallet derivation imports pass
  with networking disabled. All 75 targeted Node tests pass, including actual-shell cases for correct
  OCI image/index bytes, mismatches, mutable/malformed references and failed
  registry reads. Successful corrected staging remains outstanding.
- A native-configuration plan has been prepared but **not applied**. Alto and
  reconciler still have desired counts zero; native capability and issuance are
  not enabled, and the hosted app still uses Pimlico. Unpause requires healthy
  reconciliation and bundler qualification, not merely successful attestation.
- Cleanup of the separate temporary audit database and its SSM credential was
  blocked by the execution safety review; neither was deleted. Both the source
  recovery snapshot and encrypted Singapore copy remain retained. This cleanup
  is independent of activation and requires resolving that deletion approval.

The sections below preserve chronological evidence, including earlier states;
they are not a claim that all activation or performance gates have passed.

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
