# Frankfurt AA preparation evidence — 2026-09-10

## Scope and result

User initially approved prerequisite preparation and a live plan, then explicitly
approved the reviewed dormant apply, local plugin installation and tunnel check.
**Dormant apply completed: 211 added, 0 changed, 0 destroyed.** No application
service or sponsorship activation was approved or performed.
Target: account `932542905614`, region `eu-central-1`, identity `sepolia-aa-temp`,
Arbitrum Sepolia `421614`, existing Core **v1.2.3**. Singapore, public frontend,
Core contracts and ownership were not changed. No funds were transferred.

Created outside Terraform stack state:

- S3 bucket `plether-sepolia-aa-temp-tfstate-932542905614`, with AES256 default
  encryption, versioning, BucketOwnerEnforced ownership and all four public-access
  blocks enabled. Verified `IsPublic=false`; policy requires TLS and denies access
  except `arn:aws:iam::932542905614:user/plether-admin` and account-root recovery.
- Operator-only SecureString `/plether/bootstrap/sepolia-aa-temp/terraform-inputs`.
  Fresh database/origin credentials and keeper/oracle/liquidation keys are stored
  here until Terraform creates their runtime parameters; this prevents conflicting
  ownership/imports and excludes the bootstrap bundle from ECS runtime wildcards.
- External SecureStrings under `/plether/sepolia-aa-temp/`:
  `alto-executor-private-keys`, `alto-utility-private-key`, `pyth-api-key`, each
  version 1, encrypted with regional `alias/aws/ssm`. Alto executor serialization
  is exactly four comma-separated lowercase hex keys, matching existing preflight.
- ACM request for `aa-temp-api.sepolia.plether.com`:
  `arn:aws:acm:eu-central-1:932542905614:certificate/475a38ec-f78a-4ad4-bbb7-227c5bd33754`.
  Last observed status: **PENDING_VALIDATION**, not issued. This request is now
  **unused and superseded** by private access; the replacement profile does not
  reference it. No validation or API DNS record is required.

RPC inputs are the user-approved existing Alchemy endpoints. Required Pyth and
PostHog inputs were copied without changing their source parameters. Shared
credentials retain shared quota/rotation impact. No PostHog/Pyth traffic starts
until services are separately approved and activated.

## No DNS or certificate action

The user selected localhost-only access without Cloudflare or DNS changes. The
replacement design is an internal ALB reached through an authenticated Session
Manager relay, with no inbound relay ports or SSH keys. Only the AWS-generated
internal ALB hostname is used inside the VPC. No DNS record was changed.

## Private plan review

The replacement live Terraform 1.16.1 / AWS provider 5.100.0 plan succeeded:
**211 creates, 0 updates, 0 deletes** in `eu-central-1`, account `932542905614`,
namespace `sepolia-aa-temp`. All **10 ECS services** have `desired_count=0`;
sponsorship, submission and write-capable workers remain off. Core router,
engine and deployment block remain bound to v1.2.3. The API ALB is internal,
accepts only relay HTTP, and has no HTTPS listener. The relay has no ingress,
uses IMDSv2 and encrypted storage, and has no Parameter Store permissions.
No shared OIDC provider, certificate or DNS resource is managed by this plan.

Replacement plan SHA-256:
`b78e0cd0af9e2430a657931f17577d1fdb2f737a92e8e3f05228f39959440a85`.
Operator-local workspace:
`/var/folders/xz/scmmjjld2pq8k763z54mzjtr0000gn/T/plether-frankfurt-plan-RGtpjn`.
The binary is `infra/terraform/frankfurt.tfplan` beneath that workspace, mode
0600. **Contains secrets: do not commit/upload/share the binary or raw JSON.**
This exact plan was applied once on 2026-09-10 (approximately 19:06–19:13 UTC).
The relay EC2 instance and chargeable infrastructure exist; ECS services remain
stopped. Do not replay the saved initial plan.

### Superseded plan — do not apply

The earlier live plan (204 creates) is **superseded and must not be applied**.
Its public HTTPS listener/certificate requirement does not meet the revised
request. The plan reviewer now rejects it.

Superseded plan SHA-256:
`b26ea45aa58d79065f4477a425f0ccd1b6821e66b69e4ba4430c882cbd39f727`.
Operator-local workspace: `plether-frankfurt-plan-oClXUR`, inside the macOS
temporary directory. Binary plan: `infra/terraform/frankfurt.tfplan` within that
workspace, mode 0600. **Contains secrets: do not commit/upload/share the binary
or raw JSON.** It has not been applied and is not an activation authorization.

### Review and verification

`scripts/review-frankfurt-aa-plan.mjs <private-workspace>` reads the plan in memory
and emits only its hash, action counts, resource types, target and service counts.
It rejects updates/deletes, nonzero ECS services, incorrect target/core bindings,
public ALB access, custom hostname/certificate requirements, relay ingress,
and management of shared OIDC/DNS resources. Re-run the plan before an eventual
apply if configuration or remote state has changed.

The pre-apply inventory returned **no Frankfurt ECS clusters** and an empty
dedicated state prefix. The completed apply created the isolated cluster and
Session Manager relay with the dormant backend resources. Infrastructure charges
now accrue. Its Amazon-owned ARM64
AMI `ami-08295554222e9a438` was verified available in Frankfurt, name
`al2023-ami-2023.12.20260909.0-kernel-6.18-arm64`, owner `137112412989`.

Local verification passed:

- Terraform validation and all 15 mocked Frankfurt/legacy plans, including
  rejection of certificates, custom hostnames and service activation.
- 20 Node preparation/package/RPC-mode/tunnel tests, plus the real Vite HTTP
  integration test. The RPC-mode test also exercises its offline Terraform guards.
- TypeScript checking for the standalone Vite config.
- Workflow YAML parsing and Terraform formatting checks.

The HTTP test uses synthetic credentials and a local mock backend; it proves
rewrite/token routing, foreign-origin rejection and failure without fallback
when the tunnel is closed. It is **not** a live AWS SSM connectivity test.
The subsequent live verification below covers SSM connectivity, not API health
or sponsorship qualification.

## Dormant apply and live tunnel verification

Applied only the reviewed plan with SHA-256
`b78e0cd0af9e2430a657931f17577d1fdb2f737a92e8e3f05228f39959440a85`, after checking
the `plether-admin` identity, isolated state and no existing Frankfurt cluster.
Terraform reported **211 added, 0 changed, 0 destroyed**. No deployment workflow,
Core deployment, funding, DNS write or frontend deployment was dispatched.

Read-back checks:

- All 10 ECS services: desired, running and pending counts **0**; cluster running
  and pending task lists empty.
- RDS `plether-sepolia-aa-temp`: **available**, encrypted, not publicly accessible,
  deletion protection enabled.
- VPC `vpc-0427b0cbdd6a3eb65`; relay `i-0e437f16b86527e48`, online with SSM agent
  `3.3.4624.0`; relay security group `sg-0026690ceeff4cb3c` has no ingress.
- Internal API ALB is active and accepts only port 80 from the relay group.
- Custom document `plether-sepolia-aa-temp-api-tunnel` is fixed to this backend
  and ports, with no arbitrary host/port parameters.

Installed Session Manager plugin **1.2.835.0** into
`/Users/stan/.local/lib/sessionmanagerplugin-1.2.835.0`, with command symlink
`/Users/stan/.local/bin/session-manager-plugin`. No system daemon was installed.
The [official AWS ARM64 package](https://docs.aws.amazon.com/systems-manager/latest/userguide/install-plugin-macos-overview.html)
passed `pkgutil --check-signature` (AMZN Mobile LLC, Apple-notarized). Its
SHA-256 is `1392dde1e7c91c4e66996e8a8374c9be2a1907847cf96259311cf9c53fdff900`.
The unmodified extracted executable runs and reports the expected version;
standalone `codesign --verify --strict` did not pass, so executable code-signature
verification is not claimed separately from the signed package verification.

Opened session `plether-admin-bj28xvpvqjl8iqxyau7drcivp4`. The plugin listened only
on `127.0.0.1:18081`. A request to `/api/health` returned **HTTP 503** with server
`awselb/2.0` at 19:12:37 UTC: this proves tunnel-to-private-ALB connectivity with
the API stopped, **not application readiness**. Closed the session with Ctrl-C;
the active-session query and local listener check were empty afterwards.

Saved `frankfurt-outputs.json` alongside the private plan workspace, mode 0600,
containing only `deployment_target` and `frankfurt_tunnel`. Its values were
checked against applied Terraform outputs. No credentials are in this file.

The first post-apply plan exposed four task-definition replacements caused only
by ECS materializing empty `linuxParameters.capabilities.add` arrays and an empty
environment list on the Alto init container. Made those defaults explicit in
`aa_admin.tf`, `aa_reconciler.tf` and `alto.tf`, retaining `drop = ["ALL"]` and
adding no capabilities. No task-definition replacement was applied. Added a
regression test; Terraform validation and all 15 mocked plans pass.
The final live convergence plan returned exit code **0**:
`No changes. Your infrastructure matches the configuration.` No second apply
was necessary.

## New public service addresses

These are dedicated Frankfurt identities, not the Core owner or test wallet.
Private keys remain in encrypted SSM. No funding was performed. Read-only Alchemy
checks verified zero balances and pending nonces for all eight addresses, chain
IDs `11155111` (spot) and `421614` (Perps), and nonempty code at the v1.2.3 router.

| Role | Address |
| --- | --- |
| Keeper | `0x1092D102f615d451d3f82eF8CD283eB1a3e2c051` |
| Oracle updater | `0x12Fb09e86d263Cd43a118fC25bAde50931d1484B` |
| Liquidation worker | `0x1a0Fc970E63EaE447bc1E4ba54f0c960E696643b` |
| Alto executor 1 | `0x02Acf1CC80CFA5Cc4662F16458c85E3654F137A8` |
| Alto executor 2 | `0x413B58C8bfe7A8c40Fbc421DFBC35B710F6c6428` |
| Alto executor 3 | `0xb335AeB4088949f3804Dc4A0c6c00950F5E4e320` |
| Alto executor 4 | `0x9270a95327A2A458e190FD28007363E56014c3C2` |
| Alto utility | `0xff2fA3D3beC7c99038Da45094A4A988516a62B7C` |

## Remaining gates

Target-specific image/admin deployment
workflow support, operator alarm destination, explicit funding allocations,
on-chain writer inventory, and paymaster/simulation qualification are still
required before testing sponsorship. No Core or frontend deployment is needed.
