# Isolated recovery qualification

This drill qualifies the November 17 incident hardening without stopping a
financial worker, deleting an image, changing a live sponsorship pause or giving
a fixture access to signing keys, production databases or RPC credentials.
Follow `AGENTS.md`: all Actions operations use `gh`; local AWS checks use
`--profile plether`. Review each concrete infrastructure plan and exact protected
workflow run before approval. Do not auto-approve a pending deployment.

## Separate the evidence types

Record these as separate qualifications in the incident evidence manifest:

| Exercise | Actual runtime | Boundary that remains simulated |
| --- | --- | --- |
| Unavailable image and protected recovery | ECS/Fargate, ECR, CloudWatch, IAM, the shared recovery engine and protected GitHub workflow | The workload is a fixed non-signing heartbeat, not a financial worker |
| Observer failure, silence and notification paths | Separate Lambda/EventBridge/CloudWatch/SNS/SQS resources | No production observer configuration is changed; SNS publication does not establish email receipt |
| Backlog, interruption and guarded resume | Production reconciler process and PostgreSQL on loopback | Deterministic isolated chain/RPC fixtures, no live chain or live financial database |
| Browser stale/refusal recovery | Chromium, production execution/recovery modules, Web Locks and browser storage | Loopback RPC/receipt fixtures and synthetic non-signable data, no live wallet or transaction |

Policy/unit fixtures and successful CI are prerequisites, not substitutes for
these process-level observations. A drill is incomplete until every required
runtime row has evidence and cleanup is verified.

## Fixed AWS fixture

`config/operations/backend-recovery-drill.json` is the reviewed capability:

- Cluster `plether-sepolia-recovery-drill`, service
  `plether-image-recovery-drill`, family
  `plether-sepolia-image-recovery-drill`.
- Dedicated execution role can pull only the qualified application/router images
  and write only `/ecs/plether-sepolia-recovery-drill`. There is **no task role**,
  secret, database, RPC environment, ingress port, host mount or exec access.
- Separate security group has no ingress and HTTPS egress only. Use the reviewed
  existing public subnets; preserve the existing VPC and production services.
- Every container has a read-only root, drops capabilities and runs as UID 65534.
  Main runs the exact built-in Node heartbeat command; init exits successfully;
  the router image runs a fixed idle sleep instead of Fluent Bit. Logging uses
  the execution role through the `awslogs` driver.
- Both app/init images initially reference an intentionally nonexistent all-zero
  digest. No artifact is deleted. One desired task, `0/100` deployment and a
  circuit breaker with rollback disabled produce an initial failed sole PRIMARY
  deployment after the real pull failures.

Provision a separate observer from the reviewed observer package for this
cluster/service only. Its IAM role cannot inspect or mutate production services.
Route its clearly labelled drill notification to the existing operations topic
and an isolated SNS-to-SQS evidence subscriber. Keep the fixture's async retry
and event-age settings explicit in evidence; shortened drill windows are not
claims about production defaults.

The recovery OIDC role is
`plether-sepolia-image-recovery-drill-deploy`. It can update only this fixture,
register only its task-definition family, pass only its execution role, read the
two image repositories and read this log group. It has no production service
update, financial task-role pass, signing, database, image-deletion or pause
capability. Set `AWS_RECOVERY_DRILL_ROLE_ARN` in the protected
`backend-admin-sepolia` environment to this exact role.

## Observe the failure before recovery

After the reviewed isolated plan starts the missing-image fixture, retain:

1. Initial selected task definition, missing digest, zero running tasks and real
   task stop/pull failure. Wait for the sole PRIMARY rollout to be `FAILED` and
   pending count zero; the recovery action refuses an in-progress rollout.
2. Sanitized observer evidence containing the fixture task ARN, image-pull
   classification and timestamps. It must contain no raw environment, secret,
   signed payload or credential-bearing provider URL.
3. Independent service-deficit observation and real CloudWatch alarm history.
   Do not use `SetAlarmState` or inject metric data to make a check pass.
4. Actual SNS publication and isolated SQS receipt of the SNS envelope. Record
   human email acknowledgement separately if obtained; delivery and
   acknowledgement are different facts.

Use read-only inspection to bind the recovery approval to the failed fixture:

```bash
aws --profile plether sts get-caller-identity
AWS_PROFILE=plether node scripts/drill-backend-recovery.mjs inspect
```

The output is an exact fixture task-definition ARN and configuration fingerprint.
It cannot inspect a production service by input substitution.

## Exercise protected compatible-image recovery

The production entrypoint and isolated entrypoint both execute
`scripts/backend-recovery-engine.mjs`. They use distinct fixed capability
policies and IAM roles. The production three-service allowlist is unchanged;
there is no workflow input or environment switch that selects the isolated
profile in the production workflow.

After the normal authentication, remote master SHA and duplicate-run checks:

```bash
gh workflow run drill-backend-recovery.yml \
  --repo Plether-Fi/plether-app --ref master \
  -f expected_task_definition='EXACT_FAILED_FIXTURE_TASK_DEFINITION_ARN' \
  -f expected_configuration_sha256='EXACT_FIXTURE_CONFIGURATION_FINGERPRINT' \
  -f operator_note='Isolated image recovery qualification and incident reference' \
  -f confirmation='RECOVER ISOLATED FIXTURE ON SEPOLIA'
```

Before approval verify the exact head SHA and preflight fingerprint. The
fingerprint includes the complete fixed fixture capability, reviewed commands,
replacement digests and expected baseline. Require the maintainer's approval of
this run; the workflow revalidates protection before assuming the fixture-only
role.

The common engine checks immutable manifest bytes and Linux/ARM64 children,
preserves the complete baseline apart from image/version fields, rechecks the
selected revision/configuration immediately before update, and verifies actual
running app/init/router digests. Success requires one running task, zero pending,
a completed sole PRIMARY and more than 120 seconds of actual non-signing
heartbeats. The artifact is `isolated-backend-recovery-RUN-ATTEMPT/recovery.json`.
Record runtime timing including the real approval delay.

An ambiguous update is not retried and no rollback is automatic. Inspect the
credential-free artifact and exact live fixture selection before any retry.
The workflow cannot unpause sponsorship or operate a production service.

## Qualify observer failure paths

While the isolated service is healthy, run the reviewed fixture-only observer
fault driver. Preserve configurations before each fault and restore them in
unconditional cleanup. Keep these cases separate:

- A fixed out-of-scope service name in the isolated Lambda environment must
  produce a real ECS permission denial, Lambda error/silence and async-drop
  evidence, with IAM unchanged. Restore the exact environment.
- Remove only the isolated poll target's invoke permission: EventBridge delivery
  failure and observer silence must be real metrics. Lambda acceptance/execution
  errors are not interchangeable with failed invocation delivery. Restore the
  exact permission.
- Disable only the isolated poll rule: absence of observations must produce the
  silence alarm without fabricated metrics. Restore the rule and verify recovery.

Never alter the production observer, IAM policies, financial workers or alarm
states for these tests. Retain source event IDs, alarm state/action histories,
SNS/SQS receipts and restored resource fingerprints. An action published to SNS
does not prove that a human read an email.

## Local reconciler and browser qualification

Run the process-level PostgreSQL/RPC exercise described in
[reconciler recovery drill](reconciler-recovery-drill.md). Retain the output
`evidence.json` and RPC coverage trace. It exercises hard interruption between
settlement and cursor publication, transient outage and durable pause, multi-hour
backlog, gateway range limits, exact pause identity and stale/mismatched health.
Do not apply its synthetic fixtures to a live database or chain.

From `apps/frontend`, run:

```bash
node scripts/drill-incident-recovery-browser.mjs
```

Retain its JSON evidence with source hashes and browser version. It uses a real
browser, reloads and production modules, with loopback boundary fixtures. Record
which uncertain-preparation and already-signed recovery cases actually ran;
never describe synthetic receipt handling as a verified live-chain transaction.

## Cleanup and completion

The isolated recovery workflow leaves its fixture running for observer failure
qualification. Immediately after evidence collection, review/apply the isolated
Terraform change setting desired count to zero. Verify no running or pending
fixture task before destroying transient roles, security group, observer,
schedule and queue resources. Preserve the dedicated log group/evidence for 90
days with `skip_destroy`; never delete a live/rollback ECR image during cleanup.

Record `cleanupComplete` only after independent readback shows no active fixture
tasks or enabled schedules and all temporary fault changes restored. Verify the
production service revisions, actual digests, readiness, pause identity and
budgets are unchanged by the isolated AWS drill. Financial state must not be
modified as an acceptance shortcut.
