# Sepolia backend image recovery

Use this procedure when an existing reconciler, liquidation worker or position
protection worker cannot restart because its runtime image is unavailable, or
when a still-running instance references an unavailable image. It changes image
references and service-version metadata only. It cannot activate a service with
desired count zero, change its configuration, run an arbitrary command, clear
liabilities or resume sponsorship.

The recovery workflow is dispatched from `master` and requires approval of the
exact run in `backend-admin-sepolia`. Follow `AGENTS.md`: use `gh` for every
Actions operation and the named `plether` AWS profile for local AWS checks.
Do not auto-approve a pending deployment or bypass the gate with a direct service
update. The workflow uses the normal backend mutation concurrency group.

## Qualify the replacement first

The replacement must be present in
`config/operations/backend-recovery-releases.json` on the reviewed master commit.
That catalog records exact application and logging digests, their source commits,
compatible services, the Core manifest hash and qualification evidence. Add a
record through review; a tag, ACTIVE task definition or successful build alone
is not qualification.

For a previously qualified release, verify that the targeted service code,
dependencies, configuration format, ABI and schema are compatible with the live
deployment. For a new candidate, qualify its startup and progress in an isolated
environment. An older application image can be suitable for these three services
without being suitable for the API or other workers. Do not relabel it as the
missing source commit.

The workflow verifies the complete OCI manifest graph and Linux ARM64 descriptor,
then verifies the actual task image digests after startup. Image retention must
protect catalog entries as well as active service and approved rollback roots.
Keep automatic image expiry disabled until reference-aware cleanup is qualified.

## Inspect the exact service baseline

First verify identity:

```bash
aws --profile plether sts get-caller-identity
```

Require account `932542905614`, region `ap-southeast-1` and the intended service.
Run the read-only inspection from the reviewed checkout:

```bash
AWS_PROFILE=plether RECOVERY_SERVICE=plether-aa-reconciler \
  node scripts/recover-backend.mjs inspect
```

It emits only the selected task-definition ARN and configuration fingerprint.
Review the current service/task configuration through the restricted AWS CLI
path; never paste credentials or full task environments into an issue or chat.
Copy both values into the dispatch. They bind approval to the service's selected
revision, roles, secrets, commands, runtime, network and deployment settings.
Changes between inspection and promotion refuse the recovery. The workflow
requires an ACTIVE singleton Fargate service with a completed/failed sole PRIMARY
deployment, no pending placement and `0/100` stop-before-start settings. Resolve
an in-progress rollout or wait for pending placement to finish before dispatch.

The three service choices are `plether-aa-reconciler`,
`plether-liquidation-worker`, and `plether-position-protection-worker`. Recover
one per run. The reconciler's image recovery does not reset or advance its stored
cursor; it must catch up through the complete verified history.

## Dispatch and approve

Perform the authentication, master commit and duplicate-run checks in `AGENTS.md`.
Record the intended full master SHA. Substitute the inspected ARN/fingerprint:

```bash
gh workflow run recover-backend.yml \
  --repo Plether-Fi/plether-app --ref master \
  -f service=plether-aa-reconciler \
  -f release_id=sepolia-recovery-2026-11-17 \
  -f expected_task_definition='EXACT_INSPECTED_TASK_DEFINITION_ARN' \
  -f expected_configuration_sha256='EXACT_INSPECTED_CONFIGURATION_FINGERPRINT' \
  -f operator_note='Incident reference and reviewed recovery purpose' \
  -f confirmation='RECOVER BACKEND ON SEPOLIA'
```

Before maintainer approval, verify `headSha` and the preflight's input fingerprint.
The fingerprint includes the service, expected baseline, incident note and exact
qualified release record. Approval is specific to this run and does not authorize
sponsorship resume. Recheck protected master-only environment policy before the
workflow obtains AWS credentials.

Watch the exact run to completion with `gh run watch --exit-status`. The recovery
artifact records the selected revision, registered revision, requested mutation,
qualified digests and worker progress without task environments or credentials.
The private task-definition scratch file is always removed.

## Verify recovery and handle failure

Success requires one running task, zero pending tasks, a completed sole PRIMARY
deployment and the exact qualified app/init/router digests. Reconciler success
also requires caught-up heartbeats spanning at least 120 seconds. Liquidation
requires a completed sweep; protection requires `caughtUp=true` and zero lag.
Evidence must be fresh. Public API liveness and Core binding checks follow.

If the workflow fails or the runner is lost, inspect its artifact and the live
service's selected revision before any retry. Registration and service updates
are not automatically retried; an interrupted response can leave an accepted
mutation. ECS lacks a compare-and-swap update API, so the shared workflow lock and
the immediate final baseline check are essential. Restrict other mutation paths
through the existing deployment-role policy and operational controls.

Do not blindly restore the previous revision: its image may be the missing one.
Do not deregister a possibly selected revision, start a second active signer,
reset the cursor, release reservations or clear a financial circuit breaker to
make the workflow pass. Unused registered revisions can be reviewed and removed
after proving that no service or task references them.

Check public readiness after recovery. A durable sponsorship pause can remain
even though all workers are healthy. Use the separately protected `aa-admin.yml`
`resume-issuance` procedure only after its fresh reconciliation and exact-pause
checks pass and the maintainer approves that distinct operation. Never treat
image recovery as automatic permission to resume financial issuance.

Qualification tests: `node --test scripts/recover-backend.test.mjs`. Run a
controlled missing-image/restart drill in an isolated environment before relying
on this workflow for an incident. The policy tests do not replace that runtime
qualification or prove a successful sponsored transaction.
