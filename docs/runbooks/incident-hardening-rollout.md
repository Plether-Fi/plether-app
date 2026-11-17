# Incident hardening rollout

This release addresses the November 17 Sepolia image-retention outage and the
RPC range mismatch exposed during recovery. Code qualification does not apply
Terraform, activate observers, approve deployments or clear sponsorship pauses.
Use AGENTS.md for the `plether` profile and all GitHub Actions operations.

## 1. Preserve the recovered services

Record selected task definitions, actual running image digests, public readiness
and recent worker progress. The recovery baseline was reconciler revision 69,
liquidation revision 100 and protection revision 70; use fresh readback instead
of assuming those revisions remain current. Preserve the 1,000-block reconciler
batch setting in the operator's authoritative tfvars.

The initial runtime audit intentionally includes obsolete dormant services and
administration definitions. Their historical images may already be missing.
Explicitly decommission obsolete topology or select a verified replacement;
do not add exclusions merely to obtain a green audit.

## 2. Apply artifact protection first

After review and CI, save and review a named-profile Terraform plan removing the
API/Alto image-expiry policies and enabling repository protections. Verify the
plan does not delete images, repositories, service state or application data.
The backend workflow must be the new SHA-only, immutable-tag version before
building into immutable repositories; old workflows still try to overwrite
`latest`. Removing an expiry policy cannot restore a previously deleted image.

Read back all three runtime repository policies and tag-mutation settings after
apply. Do not build additional releases while unsafe expiry remains enabled.
No garbage collector is enabled by this release.

## 3. Qualify and deploy the compatible application release

Use the protected normal backend workflow from master with `bootstrap=false`.
Keep each run's exact SHA and input fingerprint, require its own maintainer
approval, and monitor it to terminal success using `gh`. Verify root and child
manifests, Linux/ARM64 pullability, source labels, selected service revisions,
actual running digests and public readiness.

Persist the verified application/router digests in the operator's release
configuration before subsequent Terraform plans. Terraform ignores service
task-definition drift so configuration changes must still be promoted through
the reviewed deployment workflow; registering a definition alone does not
change a service's selected revision.

Apply the new AA-admin command placeholders and deploy a backend image with the
new resume CLI before using the revised `aa-admin.yml`. Mixed old/new commands
fail closed. Never clear an existing pause as a deployment shortcut. A necessary
resume binds the exact pause event ID, reason and configured paymaster before
approval, and independently requires fresh matching reconciliation under locks.

See [guarded AA operations](self-hosted-aa-rollout.md) and the narrowly scoped
[image recovery workflow](backend-image-recovery.md). Recovery cannot change
limits, signatures, commands, secrets, network settings, cursors or liabilities.

## 4. Enable image availability and incident evidence

Provision the read-only audit role and set `AWS_RUNTIME_IMAGE_AUDIT_ROLE_ARN` to
the reviewed Terraform output. Run the complete inventory and resolve every
missing dependency. Enable its existing hourly/post-deployment workflow only
with an operational owner and tested failure notifications. The auditor never
pushes, deletes, changes a service or grants cleanup permission.

Package the incident observer with `scripts/package-incident-observer.py`, then
review a saved plan with `enable_incident_observer=true`, the exact package path
and the existing same-account/region operations SNS topic. Follow
[observer qualification](../operations/incident-observer.md). Test sanitized
STOPPED-task evidence, service deficits, observer silence and delivery failures.
Retain only allowlisted evidence for 90 days.

There is currently one confirmed email subscriber and no pager integration.
Use that existing destination; do not claim email delivery is acknowledgement
or that automatic escalation has been configured. An optional pager SNS topic
can be integrated later with its own delivery/acknowledgement qualification.

## 5. Exercise recovery in isolation

CI runs the real ECR-policy mocked plans, image/recovery policy fixtures, RPC
range/backlog fixtures, frontend refusal/recovery tests, observer privacy tests
and PostgreSQL guarded-resume cases. These checks do not replace a controlled
runtime drill. Before calling the rollout operationally qualified:

- Start an isolated non-signing service with an intentionally unavailable image;
  verify detection and the protected compatible-image recovery path.
- Restart the reconciler fixture with a multi-hour backlog, range rejection,
  transient failures and interrupted batches. Verify no cursor skips, duplicate
  settlement, early freshness or changed liabilities.
- Verify a newer same-reason pause invalidates an earlier approved resume, and
  that missing/stale/mismatched reconciliation prevents resume.
- Confirm fresh readiness blocks new preparation before wallet signing, while
  uncertain earlier authorizations remain recoverable.
- Verify evidence and notifications reach the existing operations destination;
  record detection, acknowledgement and recovery times, including approval delay.

Do not stop an active financial worker or delete a live/rollback image to
perform a drill. Qualify in Sepolia before extending configuration to another
environment; this release does not enable mainnet sponsorship.
