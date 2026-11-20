# Plether AWS infrastructure

The opt-in Arbitrum Sepolia node and private gateway are documented in the
[RPC pilot runbook](../rpc/README.md). They default to disabled and do not
change application RPC URLs. Follow repository `AGENTS.md` for the current
`plether` CLI profile and deployment authentication instructions.

The company AWS account is `932542905614`. Regional infrastructure and state
live in Asia Pacific (Singapore), `ap-southeast-1`.

## Authenticate

Use an MFA-protected console administrator and temporary AWS CLI credentials.
Do not create an IAM access key for a human operator.

```bash
aws login --profile plether-company --region ap-southeast-1
aws sts get-caller-identity --profile plether-company
```

The checked identity must belong to account `932542905614` and must not be the
root user. The pinned Terraform AWS provider does not consume the CLI's
`login_session` profile directly, so export the refreshed temporary credentials
into the current shell before running Terraform:

```bash
eval "$(aws configure export-credentials --profile plether-company --format env)"
```

## Bootstrap state

Run this once after authenticating:

```bash
PLETHER_AWS_PROFILE=plether-company ./bootstrap-state.sh
```

The script refuses any account other than `932542905614`, then creates and
secures the versioned Singapore state bucket.

## Initialize and validate

Terraform `1.16.x` is required.

```bash
terraform init -backend-config=backend.sepolia.hcl
terraform fmt -check -recursive
terraform validate
```

Keep the real Sepolia tfvars outside Git. Every plan must be saved and reviewed
before applying:

```bash
terraform plan -var-file=terraform.tfvars.sepolia -out=sepolia.tfplan
terraform show sepolia.tfplan
```

## Fresh Kernel AA deployment

First set `provision_kernel_canary_repositories=true` while
`provision_kernel_canary=false`, review the plan, and apply only the two
immutable ECR repositories and their exact GitHub deploy-role image grants.
These repositories have no count-based expiration policy, so later live builds
cannot delete canary digests. Build and record their image digests, then set
`provision_kernel_canary=true` for the second, isolated Sepolia stack. Keep
the existing `alto_entrypoint_version="0.8"`, live ECS task definitions, and
live Pages project intact. The canary creates its own Alto, API, reconciler,
IAM roles, internal Alto ALB, API target group, and paymaster KMS signer. Every
canary desired count, payment flag and public route defaults to zero or false.
Provisioning the skeleton with the three desired counts at zero and
`kernel_canary_route_enabled=false` does not start tasks or change public
routing. Review the saved Terraform plan and verify that no live resource is
replaced before applying it.

Build the patched Alto image using
[`build-kernel-alto.yml`](../../.github/workflows/build-kernel-alto.yml) and the
Kernel API image using
[`build-kernel-api.yml`](../../.github/workflows/build-kernel-api.yml). Both
must be reviewed ARM64 images in account `932542905614`, stored in separate
`plether-kernel-alto-sepolia` and `plether-kernel-api-sepolia` repositories,
and recorded by immutable ECR `@sha256:` digest in `kernel_canary_alto_image` and
`kernel_canary_api_image`. Neither workflow deploys ECS or updates `latest`.
The API build workflow can build a reviewed same-repository PR head through a
protected master-dispatched workflow, so the candidate does not need to be
merged into the live frontend release first. The Alto image workflow follows
the same reviewed candidate SHA and protected environment sequence.
The protected environment reviewer must inspect the exact PR head SHA and
image Dockerfile before approving the build; the workflow rechecks that SHA
before obtaining AWS credentials.

Create separate SecureStrings for the canary Alto executor and utility keys
under the `kernel-canary-alto-*` SSM names and a dedicated
`/plether/sepolia/kernel-canary-aa-origin-token` (64 lowercase hex characters).
The patched Alto reads `/plether/sepolia/app-rpc-url` and the existing private
gateway bearer token, injecting the token only into its primary RPC transport.
Do not use the live Alto executor or utility keys. The Terraform output
`kernel_canary_paymaster_kms_key_arn` identifies the separate signer; derive its
public Ethereum address before deploying the new paymaster.

Deploy and independently attest the fresh Kernel factory, validator, Staker,
both v0.9 simulation contracts and paymaster before starting tasks. The
Staker and paymaster each require at least 1 ETH stake with the required delay;
the paymaster also requires its own deposit. Keep Alto simulator auto-deploy off
and set its two reviewed simulation addresses explicitly. Apply the additive
Kernel database tables and funding index before API startup, reviewing the
non-concurrent index lock impact in a quiet window. Set
`kernel_canary_simulators_attested`, `kernel_canary_db_schema_confirmed` and
`kernel_canary_profile_attested` only after their respective checks. The canary
API sets `KERNEL_CANARY_API_MODE=true` and
`API_BACKGROUND_WORKERS_ENABLED=false` so it cannot run a second live indexer
or other background writer against the shared Sepolia database.
An existing Plether testnet deployer may fund these fresh contracts only after
its public address, balance, signer custody and reviewed broadcast path are
verified; no deployer private key belongs in Terraform inputs or logs.

Start Alto with `kernel_canary_alto_desired_count=1`, keeping the route dark.
Verify its actual outbound bearer-authenticated v0.9 SafeMode trace through the
private gateway; a direct gateway fetch alone is insufficient. The gateway's
default trace timeout is 8 seconds and response cap is 8 MiB, so record measured
trace duration and size. Start the API with
`kernel_canary_api_desired_count=1` only after the profile and database gates.
Before the public route is enabled, probe the API's private ECS task address
from a bounded in-VPC task or ECS Exec; no ALB target is attached yet.

Set `kernel_canary_nitro_safemode_qualified=true`, an exact HTTPS
`kernel_canary_preview_origin`, and `kernel_canary_route_enabled=true` only
after the signed Nitro/Alto gate. The HTTPS listener rule matches only backend
`/api/aa/kernel/rpc` (priority 180); the dedicated Pages preview Worker maps
browser `/api/perps/v1/aa/kernel/rpc` to it and supplies the distinct canary
origin token. Both ETH-paid and sponsored modes are limited to
`kernel_canary_owners_json`; `AA_KERNEL_PUBLIC_ADMISSION` remains false.
Live gas and external plus Privy wallet flows are then checked on the fresh
preview before any broader rollout. To test sponsorship, configure the fresh
paymaster profile and start the canary reconciler before enabling its flag.

For rollback, first close canary admission and the route. Keep the canary
reconciler running until accepted operations and sponsorship reservations have
settled or expired, then scale all canary tasks to zero. Keep the additive
database schema and KMS key so pending evidence and old binaries remain safe.

## Retained runtime images

Application, log-router and Alto repositories have no automatic image-expiration
policy. Their release tags are immutable, forced deletion is disabled, and
Terraform refuses repository destruction. Independently deployed services can
retain older digests across many releases. Digest pins and immutable tags do
not protect an image from ECR lifecycle deletion.

Supply qualified `api_image_digest` and `log_router_image_digest` values when
planning a running stack. Never use a moving tag to initialize a service.
The read-only runtime audit checks live, dormant, scheduled, administration and
approved rollback dependencies, including OCI index children. Its report does
not authorize deletion. Cleanup remains disabled until a separate reviewed
collector can prove complete reference coverage and rollback/grace retention.

Removing the old lifecycle policy preserves remaining images; it does not
restore images that ECR already deleted. Affected services also need a reviewed
deployment using an available image.

Follow the staged [incident-hardening rollout](../../docs/runbooks/incident-hardening-rollout.md)
and [runtime image availability](../../docs/runbooks/runtime-image-availability.md)
instructions before enabling the new operational checks.

## Alchemy credential rollout

Create separate Alchemy apps for Sepolia Ethereum backend traffic, Arbitrum
Sepolia backend traffic, and production browser traffic. Store the two backend
app keys as externally managed SSM SecureStrings (for example,
`/plether/sepolia/rpc-auth-token` and
`/plether/sepolia/perps-rpc-auth-token`) and set the matching `*_ssm_parameter_name`
variables. Configure `rpc_url` and `perps_rpc_url` with the keyless Alchemy
`/v2` base URLs. Terraform injects the keys as bearer tokens without copying
their values into Terraform state.

Apply network and contract-address restrictions to both backend apps. The
allowlist must include every address configured by the deployment: protocol
contracts, both lenses, Multicall3, supported tokens, and the Pyth contract.
Apply exact production-origin restrictions to the browser app. Do not enable
IP restrictions while Fargate tasks use changing public IPs.

Before rotation, configure Alchemy budget/CU alerts at 50%, 75%, and 90%.
After deploying the new credentials, soak for at least 60 minutes while checking
indexer cursors, keeper execution, Pyth endpoints, RPC request/failure alarms,
and the consolidated-worker topology. Revoke the old unrestricted key only
after the soak succeeds, then verify a request with the old key receives an
authentication failure. Keep URL-embedded keys only for the first compatibility
release; remove that path in the next release.

The provider's `allowed_account_ids` guard rejects credentials for the source
account or any account other than `932542905614`.

## LP settlement rollout

LP epoch settlement is restricted to Sepolia. Keep `lp_settlement_mode = "off"`
for the first deployment, provision a dedicated signer key and operations SNS
topic, fund the signer, and then apply `observe`. Active modes require exactly
one consolidated `plether-workers` task and
`lp_settlement_signer_funding_confirmed = true`; set that attestation only
after funding and a successful read-only preflight. Terraform rejects any
other topology.

Before changing from `observe` to `execute`, you must run the deployed keeper
image once with the same environment and secrets as the `plether-keeper`
container in `plether-sepolia-workers`, overriding its command to:

```bash
plether-keeper --lp-settlement-preflight
```

This preflight is read-only: it checks configuration, database/RPC access, and
contract bindings without acquiring the long-lived keeper lock or mutating the
database. Run it as a separate one-shot process; do not replace or stop the
live consolidated worker task. Require exit code zero and the
`lp_settlement_preflight_succeeded` log event. Then review observe-mode
heartbeats, ready backlog, signer balance, and simulated transaction costs;
configure a reviewed non-zero `lp_settlement_max_tx_cost_wei` before applying
`execute`. Do not use a different signer for the preflight, and do not bypass a
failing preflight by changing the task command permanently.
