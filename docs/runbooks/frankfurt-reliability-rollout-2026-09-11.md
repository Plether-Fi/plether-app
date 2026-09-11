# Frankfurt reliability testing rollout — 2026-09-11

User approved this testing deployment. Target: AWS account `932542905614`,
`eu-central-1`, cluster `plether-sepolia-aa-temp`, source
`205d747682259b5d74a96a37d7416d4fff2e997a`. The source was pushed to the existing
feature branch and verified through GitHub. No master merge or Actions deployment
was dispatched: the Frankfurt workflow is validation-only, and the usual Sepolia
deployment would target the wrong environment.

## Scope and immutable artifacts

- API: `plether-sepolia-aa-temp:11` → `:12`.
- Shared workers: `plether-sepolia-aa-temp-workers:6` → `:7`; only the keeper
  container image and the task's log-router image change. Its other application
  containers retain their prior images and configuration.
- Backend/keeper image:
  `sha256:631c89b2b6e9b34800ffa05e0013bfba211402cfddeae60c2fb37452ae110500`.
- API/shared-worker log-router image:
  `sha256:de9936038b75c0a1492478fa4355fe2ee50cd827306054a68d9fe8283b01f66e`.

Both images were built for Linux ARM64 from an isolated git archive, labeled
with the source commit, and published only to Frankfurt ECR repositories. Their
remote manifest configuration digests matched the local images. The actual
log-router image passed the offline Fluent Bit test preserving CloudWatch fields
and sanitizing its separate PostHog projection. API/keeper dynamic dependencies
were verified offline. Temporary Docker login credentials were removed.

Readiness enforcement is explicitly **false**. Existing native preparation and
sponsorship flags, allowlist, budgets, KMS, provider mode, safe-head allowance,
roles, secrets, service counts and Core v1.2.3 bindings are preserved. Alto,
reconciler, liquidation and protection services are not replaced. Their existing
log routers are also unchanged; all-worker telemetry qualification remains pending.
No funding, blockchain transaction, hosted frontend, DNS, Singapore or Core
configuration/deployment change was made.

## Additive database migration

Applied `aa-observability-v1.sql` then `aa-observability-v2.sql` in one transaction,
with a five-second lock timeout and thirty-second statement timeout. The task
verified the Frankfurt RDS hostname, database and TLS CA configuration before SQL,
then checked table privileges and correlation columns. Existing sponsorship
authorization/ledger data was not changed by the migration.

Successful task: `8a30b1402a284dcf803e4bab7b53cd5e`, both containers exit zero.
CloudWatch recorded `aa_observability_migration_verified`. Combined SQL SHA-256,
after removing the files' individual BEGIN/COMMIT wrappers:
`66d389bb6b892484419a4a81545aa621446143658ca630d8f022e7322360ae3c`.

An earlier wrapper task (`431d8588c0b5480481a7771b4906d0d8`) failed at JavaScript
parsing before SQL execution. The corrected wrapper was syntax-validated before
launch. Initial API registration rejected an empty tags list; omitting it
succeeded without changing any tags or service settings.

## Local testing and checks

The existing frontend remains at `http://127.0.0.1:5173`, using the fixed SSM
tunnel on port 18081. Its manifest retains sponsorship=true,
preparationRpcVersion=1 and the existing Frankfurt paymaster.

The tunnel preflight needed a local correction: the earlier API-only validator
rejected the subsequently approved worker-client ingress rule. It now accepts
only the matching deployment-tagged, ingress-free worker group in the same VPC,
alongside the relay. Arbitrary groups/CIDRs/ports and public access remain denied.
All four tunnel tests pass. No security group was changed.

The source commit's Backend, Tests, Frankfurt mocked-plan and AA observability
GitHub checks passed. Thirty-one local mocked Terraform plans pass with the new
image pins. Terraform was not applied; task-definition promotions were explicit,
and the checked-in pins record the corresponding desired configuration.

API health, AA discovery and fee retrieval passed through localhost; foreign
browser origins remained rejected with HTTP 403. The new readiness route returned
HTTP 200 with enforcement=false and ready sponsorship, reconciliation, bundler
and oracle evidence.

Final readback: API `:12` and shared workers `:7` both reached ECS `COMPLETED`,
desired/running 1/1 with no pending tasks. All six shared-task containers were
running. Alto `:4` and reconciler `:4` remained stable and unchanged. The worker
rollout honored its existing stop-before-start policy; its predecessor stopped
before ECS started `cc371eddcdf545cdbc18af5196a06e8f`.

The keeper publishes fresh readiness observations. Idle funding is
`unknown / FUNDING_UNVERIFIED` until a fresh execution-cost quote exists; this
does not claim insufficient funds and is nonblocking. Readiness and the
client-bound diagnostic endpoint both returned HTTP 200 with `Cache-Control:
no-store`; an unknown diagnostic reference returned `unavailable`, and an
untrusted readiness origin returned 403. Readiness snapshots were fresh,
enforcement=false and free of address fields. No browser storage was cleared.

This is interactive testing of the implemented slice, **not full release
qualification**. No transaction was signed/submitted as a deployment smoke test.
The 300-preparation, RPC-reduction, comprehensive fault-injection, all-worker
funding and final-trade diagnostic gates remain outstanding.

## Rollback

If necessary, explicitly promote the prior API `:11` and shared-worker `:6`
definitions, keeping their existing desired counts. Do not delete the additive
tables/columns, signed preparations or recovery journals. Keep readiness blocking
disabled. The private operator workspace containing image/promotion records is
`/private/tmp/plether-reliability-deploy-5G30tB`; it contains no exported runtime
credentials, but is local operational state and should not be published wholesale.

## Approved idle-funding follow-up

Source `f94d781d06ed51be4567f6e9802ca7dd85c89a3c` was committed and pushed
with the preview-status removal, tunnel fixes and initial rollout records.
GitHub Backend, Tests, Frankfurt mocked-plan and AA observability checks passed.
The backend suite contains 1,088 examples, including sixteen idle-funding tests.
The updated keeper pin also passed all thirty-one mocked Frankfurt plans.

Only the `plether-keeper` image changed in shared-worker revision `:7` → `:8`:

- Image: `932542905614.dkr.ecr.eu-central-1.amazonaws.com/plether-api-sepolia-aa-temp@sha256:9a825fddd0e8b942cc68b83349ca25a21b5b37dde8c586ec2d1ae11258859a79`.
- Image configuration: `sha256:8a5bae64e00c082e839d5ae7133a6f9f4ed40a3177a3c915971d0718aefb6967`.
- Task: `cf51fe80c32f47fab07c3864473fe8c4`.
- ECS rollout: `COMPLETED`, desired/running 1/1, pending zero; all six containers running.

The shared task restarted under its existing stop-before-start policy. All
other application images, the log router, environment, secrets, roles, network
configuration and desired count were compared with revision `:7` and preserved.
The API remains `:12`; Alto and the reconciler remain `:4`. Readiness enforcement
remains false. No Core/Singapore deployment, funding or allowance change occurred.

Fifteen readiness samples at ten-second intervals over 141 seconds were fresh
and reported keeper `ready / READY`. This includes more than two idle minutes
without any transaction quote. The localhost activity panel showed “Trading
status — ready”; the trade preview no longer displayed the status panel.

### Sponsored trade smoke test

The canary owner used the localhost Firefox app on Arbitrum Sepolia. A
1,100-plDXY reduce-only order was reviewed against the existing 25,000-plDXY
Short position; no opening trade or margin transfer was requested.

The first unsigned attempt was cancelled after the wallet-approval guard flagged
the commit screen's “Long” label. A read-only database diagnostic decoded the
exact saved calldata matching the wallet's client order ID and verified
`side=Short`, `isClose=true`, quantity 1,100 and the v1.2.3 router. Task
`385938c3de9047ef84ef400922105065` exited zero and logged
`aa_smoke_intent_verified`, without exporting raw operations or credentials.
The label is a UI defect, not an opening Long payload; it remains follow-up work.

The second preparation was denied because reconciliation was briefly stale
while catching up to an advanced safe head. Logs show safe-cursor progress and
a recovered heartbeat about three seconds after the denial. No signing,
submission, restart or safety-rule change occurred for that attempt.

A fresh third attempt was signed and executed successfully:

- Order: **11**, close/reduction, **FAD** execution mode.
- Quantity: **1,100 plDXY**; remaining Short position: **23,900 plDXY**.
- Commit: `0x94c6019f85029b1dcd12b87047b4c71263ad3f6afb8302db36ac8b0d39f39d12` (block 307885678).
- Keeper execution: `0x0ae612f83d19b31e6d957d371972149bd9bfab16fd1ba0b2dbb9c7c64ed8be3f` (block 307885702).
- UserOperation: `0xe097db581811335f6c6501b82095d5dad0d66c3093528b988d835566b6cd72f8`.
- Commit-to-execution: **6 seconds**, from canonical block timestamps.
- Both receipts succeeded and their block hashes matched canonical reads.
  The EntryPoint event reported successful execution with the configured Plether
  paymaster and expected smart-account nonce 118. The lifecycle event and indexed
  receipt both reported Executed. The browser showed the final reduction result.
- Actual sponsored gas cost: `416949139912672` wei. Owner network gas was sponsored.
- At receipt verification, the safe head was block 307883247, behind this order.
  Safe confirmation/budget settlement therefore remained pending; no early
  liability release or confirmation-rule relaxation was performed.

For this follow-up, rollback restores worker revision `:7` only and its prior
keeper image; the API and additive database records remain unchanged. Private
operator evidence is in `/private/tmp/plether-idle-keeper-deploy-lH6ar7`.
