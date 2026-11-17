# Isolated image-recovery drill

This independent module creates the fixed nonfinancial fixture described in
`config/operations/backend-recovery-drill.json`. It never loads the application
Terraform backend, changes a financial service, deletes an image, reads a
secret, or grants a container AWS task credentials.

The dedicated cluster is `plether-sepolia-recovery-drill`, its only service is
`plether-image-recovery-drill`, and its task family is
`plether-sepolia-image-recovery-drill`. A Node init exits successfully, the Node
application emits a non-signing heartbeat, and the router image runs only
`sleep`. Their exact commands come from the shared fixture profile. All three
containers run as UID 65534 with read-only roots, dropped capabilities, no ports,
mounts, secrets, database or RPC configuration. A separate execution role can
read only the application/router ECR repositories and write this fixture's log
group. There is no task role. The new security group has no ingress and permits
HTTPS egress; it does not permit PostgreSQL. HTTPS routing is not an ECR-only
network boundary.

The workflow role trusts only the repository's existing protected
`backend-admin-sepolia` environment. Its registration/tagging permission covers
only the fixture family, `UpdateService` covers only the fixture service, and
`PassRole` covers only the fixture execution role. It cannot run or stop tasks,
update financial services, pass their roles, use KMS, or read SSM. The workflow
also enforces `master`, exact input evidence and maintainer approval. Set its
protected `AWS_RECOVERY_DRILL_ROLE_ARN` variable to the reviewed
`recovery_workflow_role_arn` output. AWS documents family-level registration
authorization in the [ECS service authorization reference](https://docs.aws.amazon.com/service-authorization/latest/reference/list_ecs.html).

## Review and initialize

Follow repository `AGENTS.md` for all identity, GitHub Actions and deployment
operations. Use `AWS_PROFILE=plether` and the pinned Terraform CLI. Create a
private directory with mode 0700 and `umask 077`; retain its local state until
the drill has been completely torn down. Never pass the application's S3
backend configuration to this directory.

Package the reviewed observer with `scripts/package-incident-observer.py` and
record its source/code hash. Initialize this module with its own `TF_DATA_DIR`
and `-backend-config=path=/absolute/private/directory/terraform.tfstate`.
Save a plan supplying the absolute `observer_package_path` and
`fixture_desired_count=1`. Review every action: the initial plan must create only
the isolated resources and read existing public subnet metadata. It must not
update, replace or destroy any existing resource. Record public readiness,
financial service definitions/counts and the absence of the all-zero fixture
digest before applying the reviewed plan.

The source defaults to desired count zero. The deliberate fault plan explicitly
uses one task and the nonexistent all-zero application digest for both app and
init; it never removes a real manifest. Initial circuit breaker settings are
enabled with rollback disabled, minimum healthy zero and maximum 100. Wait for
the sole PRIMARY deployment to become FAILED, desired one, running/pending
zero. `wait_for_steady_state=false` lets Terraform return while this intentional
failure is observed. Do not first create a healthy deployment or stop a
financial task to induce the fault.

## Observe and recover

A separate copy of the reviewed observer watches only the fixture cluster and
service. It preserves the existing operations SNS destination and mirrors the
same sanitized notifications to a new SNS-to-SQS evidence channel. It never
changes production observer configuration or existing SNS subscriptions. Record
the real stopped-task classification, deficit alarm, matching queue evidence,
operations notification receipt and human acknowledgement.

For bounded fault qualification, this fixture alone uses EventBridge age 60
seconds/retries zero and Lambda asynchronous age 60 seconds/retries one.
Production retry settings remain unchanged. The isolated fault driver may
temporarily alter only this observer's configuration, poll permission or poll
rule and must restore them. Verify real alarm transitions and async-drop
evidence; do not call `SetAlarmState` or publish synthetic health metrics.

Use only the protected isolated recovery workflow, its own role and the fixed
reviewed fixture release. Follow `AGENTS.md` with `gh`: check authentication,
verify the exact remote commit and avoid duplicate dispatches. Preserve its run
ID, input fingerprint, approval, image/configuration comparison and terminal
result. Recovery may change only the fixture's compatible immutable images.
Require actual running digests, successful init, at least two minutes of healthy
non-signing heartbeats and restored deficit/failure/silence alarms. Compare the
financial service/readiness baseline again. The fixture grants no authority to
recover an additional production service.

## Teardown and evidence

Retain the private state and reviewed code. After restoring any faulted fixture
observer configuration, review a plan using `fixture_desired_count=0` and apply
it; only the isolated service capacity should decrease. The task definition is
ignored to preserve the protected recovery workflow's selection, while desired
count remains Terraform-managed for this cleanup. Save all workflow, queue,
alarm, task, image and timing evidence before destroying the isolated module.
Do not apply a capacity plan that also replaces a task definition because of
provider normalization. In that case, use a reviewed, exact-fixture CLI capacity
update before the scoped destroy plan; do not relax task-role or topology checks.
Review the destroy plan against the same private local state; no production
resource may appear. Remove the temporary GitHub role variable after teardown.

Three scoped log groups use 90-day retention and `skip_destroy=true`, preserving
incident evidence after the costly fixture resources have gone. Record their
names and remove them deliberately after evidence retention when appropriate.
Before repeating the same fixed-name module, import retained log groups into
the new private state rather than attempting to recreate or delete them. The
queue is temporary, so export its evidence before teardown. A receipt in SQS
does not prove human acknowledgement of the existing operations email; record
that separately.
