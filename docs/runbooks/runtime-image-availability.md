# Runtime image availability

Every running service, pending/draining task, launchable administration task,
scheduled task and approved recovery release must retain its required images.
Digest identity and availability are separate requirements: a pinned digest can
still be deleted. Runtime repositories have no automatic count/age expiration.
Do not restore expiration or authorize deletion from the inventory report.

## Release identity

`deploy-backend.yml` builds a full-commit tag once, records its digest and OCI
source/revision labels, and promotes only `repository@sha256:...` references.
Retries reuse an existing tag after verifying its source labels, ARM64 image,
index children and pullability. They never replace the contents of a commit
tag. Unlabelled historical images are not silently trusted by this path; use a
separately reviewed qualified recovery catalog entry when restoring one.

Application, scratch-init and FireLens images are all runtime dependencies.
The workflow retains application/router release records for 90 days. A release
record is provenance, not proof of service or contract compatibility; recovery
still requires the approved service-specific catalog and protected gate.

Terraform requires `api_image_digest` and `log_router_image_digest` before any
application service is started. Obtain these from a verified release record.
The deliberately unavailable `bootstrap-unqualified` placeholder is allowed only
when every application/Alto/reconciler service is stopped and workers are
consolidated. No running task definition uses `latest` as its default identity.
Review the existing per-service pins when supplying these values; a scoped
deployment may intentionally leave different services on different releases.

Before normal service replacement, the workflow reads the revision currently
selected by the service, verifies the active images remain available, then
rechecks that selected revision immediately before updating. A missing active
image requires the protected recovery operation, not an unavailable rollback.

## Read-only availability audit

Provision the reviewed `runtime_image_audit_role_arn` output and set the GitHub
variable `AWS_RUNTIME_IMAGE_AUDIT_ROLE_ARN` to that exact role. Its capability is
read-only: it cannot push/delete images, change services, start tasks, read SSM
secrets or sign. The workflow runs hourly and after deployment/recovery runs.
GitHub workflow failure notifications must be routed to the incident owner;
workflow success without an acknowledged alert route is not operational
qualification.

For a manual read-only check, follow AGENTS.md authentication and use:

```bash
aws --profile plether sts get-caller-identity
node scripts/runtime-image-audit.mjs \
  --config config/runtime-image-audit-sepolia.json \
  --profile plether \
  --output /tmp/runtime-image-audit.json
```

The report records its account/region/cluster/repository coverage and fails on
missing manifests, partial responses, permission errors or incomplete
pagination. It inventories all selected service revisions, including dormant
services and in-progress deployments; actual running/pending/draining task
digests; configured admin families; discovered EventBridge/Scheduler ECS
targets; and catalogued recovery images. OCI indexes and child manifests are
verified by digest. Configuration cannot silently exclude an unknown image.

The report does not test execution-role layer downloads or network pullability.
Release verification and an isolated non-signing pull qualification cover those
separate requirements. A healthy report is never cleanup authorization.

An obsolete dormant service still has a deployable image dependency. Explicitly
decommission it or select a verified replacement definition; do not add an
exclusion merely to turn a report green. Existing obsolete Sepolia definitions
with deleted historical images must be resolved before the initial audit can
pass. Add other environments only with their own complete reviewed scope.

Future garbage collection needs a separate reviewed capability, complete
reference inventory, rollback/grace retention, a final reference recheck and a
deletion audit. Any failed discovery step must prevent deletion.
