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
