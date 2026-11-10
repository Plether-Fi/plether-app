# Arbitrum Sepolia RPC pilot

This is the first implementation increment of the
[RPC replacement plan](../../docs/plans/self-hosted-arbitrum-rpc.md).
It provisions an isolated node and private gateway when explicitly enabled.
Existing `PERPS_RPC_URL`, historical routing, browser transports, AA verification,
worker topology and polling intervals are unchanged by provisioning the pilot.

## AWS prerequisites checked on 29 October 2026

- Named profile `plether` resolves to the intended account `932542905614`.
- Singapore standard On-Demand quota: 16 vCPUs; no running/pending EC2 instances.
  This covers one `i4i.4xlarge` without headroom for another standard EC2 host.
- The selected type is offered in all three Singapore AZs. Actual capacity
  remains subject to availability at launch.
- Existing SSM parameter names contain no dedicated parent execution/beacon
  configuration for this node. Secret values were not inspected.
- The issued Singapore certificate covers only `api.sepolia.plether.com`.
  Obtain a separate certificate for `rpc-internal.sepolia.plether.com`, with
  public DNS validation at the authoritative DNS provider. Service DNS itself
  is private, in a VPC-associated Route 53 zone.

Use `AWS_PROFILE=plether` with Terraform and the named CLI profile for AWS,
following [AGENTS.md](../../AGENTS.md). Terraform 1.16.x is required. Do not
use a default profile or change the existing authentication configuration.

## Minimal provider setup selected 30 October 2026

Reuse the existing Alchemy account for Ethereum Sepolia execution and beacon
data, and for Arbitrum backup, historical access and gateway health comparison.
No QuickNode, Chainstack or other paid subscription is required for this pilot.
Use one beacon endpoint; a second endpoint is optional. Leave
`reference_url_parameter` and `reference_token_parameter` empty to reuse the
backup URL and authentication. Shared probe results are reused rather than
making duplicate backup/reference health calls.

The own node answers ordinary application reads from local Arbitrum state.
It still depends on Alchemy for Ethereum data and its health reference. This
pilot reduces direct application RPC dependence but does not survive every
Alchemy outage. In particular, a new/unqualified node cannot be promoted
without its reference; an already qualified node can continue while its head
is recent. Native AA retains its existing separate-provider checks and fails
closed on provider disagreement/loss. The gateway does not replace those checks
or enable a single-provider AA exception.

Alchemy publishes an Ethereum Sepolia Beacon API. Account/network entitlement,
historical blobs needed from the selected snapshot, full data-subnet coverage
and available quota must be qualified before launching AWS. Existing usage may
increase; reusing the account is not a guarantee of zero marginal API charges.
If current access is insufficient, report the exact gap before any paid upgrade.
Do not add a new provider or upgrade Alchemy under this pilot authorization.

Sources checked 30 October 2026:

- [Alchemy supported networks and endpoint formats](https://www.alchemy.com/docs/reference/node-supported-chains)
- [Alchemy Sepolia Beacon](https://www.alchemy.com/rpc/ethereum-sepolia-beacon)
- [Arbitrum blob/subnet requirements](https://docs.arbitrum.io/run-a-node/beacon-nodes-historical-blobs)

The earlier two-vendor recommendation is deferred. A separately operated
reference and beacon backup can be added later without changing AA policy.

## Inputs and secrets

Copy `../terraform/sepolia-rpc.tfvars.example` outside Git and fill every
placeholder. Terraform refuses missing image/snapshot pins, unbounded history,
wrong environment/region/chain and a missing operations SNS topic.

Resolve and review an Amazon-owned x86_64 Amazon Linux 2023 AMI in Singapore:

```bash
aws --profile plether --region ap-southeast-1 ssm get-parameter \
  --name /aws/service/ami-amazon-linux-latest/al2023-ami-kernel-default-x86_64 \
  --query Parameter.Value --output text
```

The example Nitro digest was resolved from official release
`v3.12.2-5f02f3f`. Recheck support/version and snapshot compatibility at launch.
Use a pinned `full-path` snapshot URL, not `--init.latest`. Inspect its metadata,
record the SHA256 of the metadata bytes and confirm the `path` scheme,
`full-path` kind and `sepolia-rollup` chain. Nitro verifies each snapshot part
during import. Qualification must also verify a canonical checkpoint.

### Snapshot and upstream preflight checked 5 November 2026

The example now pins the verified metadata SHA256 for
`sepolia-rollup/2026-10-20-dbd26a00/`. This snapshot is available: its metadata
and manifest return HTTP 200, all four parts have the advertised Content-Length,
and the manifest's SHA256 entries match the metadata. The 2,007,048,622,080-byte
archive fits the selected data disk and the bootstrap's staging/headroom checks.
The snapshot's `v3.12.2-rc.2-5f02f3f` build shares commit `5f02f3f` with the
pinned official stable `v3.12.2` image; no RC runtime is selected.

The earlier claim that this snapshot was unavailable was incorrect. The
unidentified Python download returned an error while curl, Go's user agent and
an explicitly identified Python request returned HTTP 200. Bootstrap now uses
`User-Agent: plether-rpc-bootstrap/1` and still checks the metadata digest and
rejects redirects. Recheck availability before every launch.

The official `latest-full-path.txt` pointer currently names
`2026-10-31-5cf7a67f/`, built with `v3.12.3-rc.4-7116cfc`. That build's commit
and matching image were unavailable from the official GitHub repository and
Docker registry during this check, so the reviewed example retains the
version-matched 20 October snapshot instead of adopting an unverified runtime.

Existing Alchemy credentials returned the correct Ethereum/Arbitrum Sepolia
chain IDs. Eight dated samples from 19 October through 5 November returned
16 requested 131,072-byte blobs through Nitro's
`/eth/v1/beacon/blobs/{slot}?versioned_hashes=...` API, plus their corresponding
Ethereum execution blocks. For the sampled `gloas` beacon blocks, commitments
are in `body.signed_execution_payload_bid.message.blob_kzg_commitments`; the
older direct `body.blob_kzg_commitments` field is absent. Its absence does not
mean the block has no blobs. The earlier empty samples were inconclusive
because they inspected the old field.

The [sanitized preflight evidence](../../docs/plans/evidence/sepolia-rpc-preflight-2026-11-05.json)
records public block/slot identifiers, versioned hashes and snapshot pins; it
contains no credentials. These are availability samples, not a full restore,
continuous batch replay or KZG proof validation. Those checks, canonical/safe
state agreement and load/quota qualification still run on the isolated node
before traffic cutover. No new provider subscription is required by these
preflight results, and no resources were provisioned during the checks.

Create externally managed SSM **SecureStrings** without putting values in
Terraform or command-line literals:

| Parameter | Value |
| --- | --- |
| `/plether/sepolia/rpc-node/parent` | JSON object shown below |
| `/plether/sepolia/rpc-node/gateway-token` | Random token of at least 32 characters |
| `/plether/sepolia/rpc-node/backup-url` | Dedicated Alchemy Arbitrum Sepolia origin URL |
| `/plether/sepolia/rpc-node/backup-token` | Optional bearer token if the backup URL is keyless |
| `/plether/sepolia/rpc-node/reference-url` | Optional separate Arbitrum Sepolia origin; omit for the pilot |
| `/plether/sepolia/rpc-node/reference-token` | Optional token, only when a separate reference URL is configured |

Do not use `/plether/sepolia/perps-rpc-url` as the backup parameter: changing
that live alias during cutover would make the gateway point back to itself.
Optional reference URLs must be actual upstream origins, not another gateway
alias. When omitted, the backup origin and its token serve both roles.

Parent JSON (example structure, no credentials):

```json
{
  "execution_url": "https://eth-sepolia.g.alchemy.com/v2/YOUR_KEY",
  "beacon_url": "https://eth-sepoliabeacon.g.alchemy.com/v2/YOUR_KEY"
}
```

The initial adapter accepts HTTPS endpoints with URL-based provider credentials.
It does not implement per-provider custom authentication headers. Check that
the existing account's endpoints fit this contract before provisioning. An
optional `secondary_beacon_url` must differ from the first endpoint. The minimal
pilot has no automatic failover between L1 execution providers and retains its
existing Alchemy dependency.

For a SecureString, use a protected local file and `--value file://...`:

```bash
aws --profile plether --region ap-southeast-1 ssm put-parameter \
  --name /plether/sepolia/rpc-node/parent --type SecureString \
  --value file:///absolute/protected/path/parent.json
```

Use `kms_key_arns` for any customer-managed keys protecting these parameters.
Secret values are fetched by the node/ECS execution role at runtime. Terraform
stores only names/ARNs and non-secret node configuration. Nitro's root-owned
runtime directory contains a UID-1000-readable private configuration file.
Redeploy gateway tasks after rotating their SSM values; restart the Nitro
process after changing its parent credentials or runtime configuration.
Treat restricted Nitro logs as potentially sensitive provider diagnostics;
gateway logs contain only bounded diagnostics: readiness, fixed reason codes,
allowlisted method names, counts, routing attempts, HTTP status and duration.
They never include parameters, caller RPC IDs, addresses, transaction payloads,
headers, endpoint URLs or parser/provider error messages.

Every gateway still probes chain ID, sync status and latest/safe/finalized heads
every five seconds, requires a recent latest head, compares canonical hashes at
all three common heights, and requires two passing probes before promotion.
Fresh tagged blocks are reused for comparison only within the same probe. A
numeric lookup is made only when the common height was not fetched already on
that upstream. Equal-height conflicting tagged responses fail closed. There is
no cross-probe cache or shared verification state between gateway replicas.
When all tagged heights align, this reduces each reference probe from eight RPC
calls to five (six block calls to three); unequal heights may need extra reads.

`rpc_request` retains `upstream` and `duration_ms` and adds:

| Field | Meaning |
| --- | --- |
| `route_reason` | `primary_healthy`, planned `managed_method`/`historical_state`, availability fallback `primary_unhealthy`, retry fallback `primary_upstream_failure`, or an unselected/strict-primary route |
| `failover` | True only when a non-managed request selects backup because primary is unavailable or a retryable primary call failed; includes failed backup attempts |
| `attempts` | Upstreams attempted, including any latest-log normalization lookup; empty for requests rejected before upstream access |
| `status`, `outcome` | HTTP status and `success`, `rpc_error`, `rejected` or `upstream_error`; HTTP 200 alone does not imply RPC success |
| `rejection_reason`, `stage` | Fixed rejection code and stage, including authentication, validation, concurrency limits, stale health and log-range normalization |
| `methods`, `batch_size` | Up to 20 allowlisted names (`unsupported` for unknown names), with oversized batch size capped at 21 |
| `primary_error`, `upstream_error` | Internal error category, never provider response text |

Successful GET `/healthz` requests stay outside application request counts.
Other route/authentication/admission rejections now produce `rpc_request` too;
compare rejection rates across releases with that expanded coverage in mind.
`rpc_probe` adds a readiness `reason`, `checkpoint_fetches` and
`checkpoint_reuses` to make the optimization and unready periods observable.

CloudWatch Logs Insights examples for `/plether/sepolia/rpc-gateway`:

```text
filter event = "rpc_request"
| stats count(*) as requests by route_reason, failover, status, outcome

filter event = "rpc_request" and outcome = "rejected"
| stats count(*) as requests by rejection_reason, stage, upstream

filter event = "rpc_probe"
| stats count(*) as probes, sum(checkpoint_fetches) as fetched,
    sum(checkpoint_reuses) as reused by reason
```

The initial deployed identifiers and reviewed pins are recorded in
`infra/terraform-rpc/sepolia.tfvars.json`; it contains only non-secret context
and parameter names. Use it with the standalone backend, never the application
entry point. After staging removal, its `staging_volume_gib` must be zero so a
future plan does not recreate a temporary import volume.

The gateway caps requests at 512 KiB, responses at 8 MiB, batches at 20 calls,
and each task at 16 concurrent requests / two trace simulations. Log scans
require an explicit block hash or numeric range of at most 2,001 blocks. Only
allowlisted methods are accepted; historical transaction traces and signing
methods are excluded. Raw transaction sends, including mixed batches, are
never automatically replayed after an ambiguous failure. Reverts are preserved.

## Provisioning and publishing

Use `infra/terraform-rpc` for the live pilot. Its separate backend key is
`plether/sepolia/rpc/terraform.tfstate` in the existing state bucket. The entry
point reads the existing VPC, public subnets, application security group,
cluster and GitHub OIDC provider as data sources; it does not manage them or
require database, application signer or telemetry credentials. RPC definitions
are symlinked from `infra/terraform` so the two entry points share their actual
resource definitions, bootstrap and guard tests.

Keep `provision_sepolia_rpc = false` in the application stack. Enable it only
in the standalone RPC stack; never manage the same RPC resources in both states.
The standalone context inputs are `existing_vpc_id`,
`existing_public_subnet_ids` (two public AZs), `existing_ecs_cluster_name` and
`existing_ecs_security_group_id`, plus `operations_alarm_sns_topic_arn` and the
reviewed RPC overlay. The initial plan must contain only new RPC resources;
reject application changes or existing network/cluster ownership.

```bash
AWS_PROFILE=plether terraform -chdir=infra/terraform-rpc init \
  -backend-config=backend.sepolia.hcl -lockfile=readonly
AWS_PROFILE=plether terraform -chdir=infra/terraform-rpc plan \
  -var-file=/absolute/protected/path/rpc-only.tfvars.json \
  -out=/absolute/protected/path/rpc.tfplan
```

1. Review a saved Terraform plan using only the standalone network/cluster
   context and reviewed RPC overlay. Do not supply application tfvars. Keep
   `gateway_image = ""` initially. The first
   apply creates the ECR repository and node, without starting gateway tasks.
   No application URL is switched by either apply.
2. Configure GitHub environment `rpc-admin-sepolia` with reviewers and master-
   only deployment branches. Set `SEPOLIA_RPC_ADMIN_ROLE_ARN` and
   `SEPOLIA_RPC_NODE_INSTANCE_ID` from the `sepolia_rpc_pilot` output.
3. After the workflow reaches master, publish the gateway using `gh`, following
   all auth, commit and duplicate-run checks in AGENTS.md:

   ```bash
   gh workflow run rpc-node-admin.yml --repo Plether-Fi/plether-app \
     --ref master -f operation=publish-gateway
   ```

   Verify the run's `headSha`, watch it to success and record the ECR digest
   printed in the run summary. The workflow publishes the image only.
4. Put that digest in `gateway_image`, review a second saved plan and apply it
   to start two ARM64 gateway tasks. Their internal ALB accepts TLS/443 from
   the existing ECS application security group. AA's dedicated security groups
   are deliberately not admitted until its separate credential/routing change.
5. From an authorized in-VPC consumer, test authenticated `/` and `/primary`.
   `/` permits ordinary fallback; `/primary` never routes to Alchemy. `/healthz`
   is an unauthenticated ALB readiness check with no endpoint/credential details.

`provision_sepolia_rpc` defaults to false. The actual stateful launch requires
reviewed inputs and a saved plan. An instance replacement is blocked by
`prevent_destroy`; explicitly review recovery before removing that guard.

## Bootstrap, storage and recovery

The host identifies the 3.75 TB **instance-store** disk by exact model and size,
formats only an empty device without existing signatures/partitions and records
its filesystem UUID. Service restarts refuse missing or unexpected mounts.
The database cannot silently spill onto the boot volume.

PathDB's historical RPC index is enabled with an explicit 345,600-block recent
window. The Nitro flag is named `archive`, but the finite window still prunes
state online; it is not unbounded archive retention. Qualify historical calls at
the actual common `safe` block before accepting AA traffic.

The default **4 TiB encrypted gp3 staging disk is temporary**. The inspected
snapshot is about 2 TB uncompressed; parts plus their assembled archive can
need roughly twice that space. Import checks at least 30% extraction headroom
and 2.1 times snapshot size on staging. Larger snapshots may require a larger
staging volume. Nitro imports with checksums and `then-quit`, records completion,
then starts serving. An interrupted/unknown database requires operator
inspection; the service never deletes it automatically.

Staging uses 500 MiB/s throughput to avoid a 125 MiB/s bottleneck during parts
concatenation and extraction. At the Singapore rates checked on 5 November,
this adds $18/month in provisioned throughput to the approximately $393/month
capacity charge: **$411/month, or $0.56/hour**, only while the disk exists.
The throughput increase is about $0.025/hour and does not require stopping the
host. This is separate from the steady-
state node budget. After a successful import, unmount staging on the host,
remove its UUID entry from `/etc/fstab`, then set `staging_volume_gib = 0` and
review the plan to detach/delete the staging volume. This updates the runtime
SSM configuration without replacing the node. A detached volume still incurs
charges until deleted. Inspect data-volume IDs carefully before cleanup.

Never stop/start the **EC2 host** as an upgrade or restart procedure: local
instance-store data is lost on stop/termination. Restart the Nitro process.
Bootstrap user data is create-only and ignored for existing hosts; reviewed
helper/service repairs are applied through SSM, preserving the existing disk
guards. AMI/instance replacement is still blocked by `prevent_destroy`.
On host loss, keep live traffic on the qualified backup and perform an explicit
snapshot re-seed on a replacement. Do not treat image rollback as database
rollback. This pilot includes no automatic host replacement or data deletion.

The admin workflow permits only `status`, a nonblocking process `restart`, or
gateway image publication. Its role cannot stop EC2, apply Terraform or send
arbitrary shell. Use `gh` to dispatch and monitor it. A restart request succeeding
means SSM accepted the process restart; inspect readiness afterwards. It refuses
to interrupt a service still importing its snapshot.

## Qualification and follow-up

The bounded operator helper `infra/rpc/ops/finish_bootstrap.py` waits for a
completed import and head catch-up, then runs `qualify.mjs` as a short-lived VPC
task using the gateway execution role's existing secret injection. It verifies
the direct primary chain, syncing status, fresh latest head, common canonical
latest/safe/finalized hashes and safe-state reads against Alchemy. Trace
capabilities are recorded separately and do not authorize an AA cutover.

With explicit `--cleanup-staging`, the helper saves and checks a cleanup plan
before unmounting anything. The only permitted mutations are deleting this
deployment's named staging attachment/volume and clearing that volume ID in the
RPC config parameter. It checks the volume serial and import marker on the
host, unmounts staging, removes only its fstab entry, applies the checked plan
and repeats qualification after a process restart. Any unexpected plan change
or failed check stops completion with staging preserved whenever still mounted.
The data disk and EC2 host are never deletion targets. Completion status and
canary results are written as credential-free JSON in the operator directory.
If the deployed config still matches its checked-in copy, staging cleanup sets
that copy to zero; commit this small configuration update after completion.

The helper is specific to this pilot's node/volume IDs; review them before
using it for a different deployment. Run it from an authenticated operator
session with the pinned SDK in `ops/requirements.txt`, a protected directory
containing the prepared non-secret `rpc-only.tfvars.json` inputs, and
Terraform 1.16.x. It has a bounded wait and does not recover an interrupted
database by deleting it.

```bash
python infra/rpc/ops/finish_bootstrap.py \
  --directory /absolute/protected/path \
  --terraform /absolute/path/to/terraform \
  --hours 24 --cleanup-staging
```

During the first launch, cloud-init recorded the original invalid XFS-label
failure. The corrected helper and unit were installed through SSM on the same
host, and the existing cloud-init result remains as an audit record. Use the
current systemd state, import marker, gateway probes and completion report to
assess the repaired service; do not restart cloud-init with the old user data.

Offline CI covers gateway behavior, bootstrap policy and Terraform mock plans.
These checks do not prove a multi-terabyte restore or Nitro RPC compatibility.

Before cutover, require a measured import/restart/rebuild, checkpoint and safe-
state agreement, parent/blob outages, Alchemy outage, malformed/stale gateway
probes, realistic simulation and transaction canaries, capacity at the new
polling rates, and seven days of stable primary operation. Observe node status,
memory, disk headroom and gateway primary-unhealthy alarms routed to operations
SNS; inspect fallback request counts and request-duration logs.

Follow-up changes still required: V3 event enrichment and historical routing,
legacy receipt compatibility, authenticated AA/direct-primary wiring and
independent verification, consumer/browser cutover, public gateway controls,
adaptive faster polling and workload-specific dashboards/SLO qualification.
Do not enable new polling or AA traffic merely because infrastructure exists.

## Sepolia application primary cutover

After initial qualification on 7 November 2026, the owner authorized moving the
Sepolia application to the node with Alchemy backup while the stability soak
continues. This testnet authorization does not authorize a mainnet cutover.

The browser uses its same-origin `/api/perps/v1/rpc` endpoint. The Pages Worker
authenticates to the API with the existing origin credential, and the API uses
the private gateway token from SSM. No node token enters a browser bundle.
The public route bounds body/batch size, gas, logs, fee history, methods and
per-IP/global request rates; debug methods and state overrides remain private.
Ordinary requests prefer Nitro. Old state outside the retained window, bounded
historical transaction traces and legacy UserOperation receipts explicitly use
Alchemy; contract reverts and ambiguous writes are not retried across providers.

The existing, explicitly configured `single-provider-sepolia` AA policy is
preserved. AA security reads use `/primary`, which cannot fall back; both reads
in that mode remain reads from one provider. This is not dual independent
verification. The initial application cutover leaves Alto and its Alchemy parameter
`/plether/sepolia/perps-rpc-url` unchanged. Application task revisions
use the separate `/plether/sepolia/app-rpc-url`, so they cannot redirect Alto
on a later task restart. This cutover neither restarts nor redeploys Alto.
After the RPC cutover, the frontend refreshes positions, pending orders and live
market state every two seconds, and order/trade history every five seconds.
Transaction history remains lazy-loaded while its tab is active, and timelocked
configuration keeps its slower cache. Sponsorship and transaction policies are
unchanged.

The Sepolia own-RPC overlay also sets the order keeper to one second while
active or idle, and liquidation, SL/TP, history indexing and AA reconciliation
to two seconds. Oracle refresh/health checks and LP settlement retain their
existing cadence. Confirmation depths, fee/gas limits, batch sizes and execution
modes are preserved.

For an existing deployment, use `ops/prepare_worker_cadence.py prepare --directory
<private-operator-directory>` after the reviewed change is merged. It registers
cadence-only task revisions for the four running services and their inactive
keeper/indexer counterparts; it neither starts dormant services nor reads secret
values. The normal backend workflow deploys those revisions from `master` with
`bootstrap=false` and close-assistance mode `preserve`. Then run the operator's
`verify` operation: it checks actual running revisions and a fingerprint of every
non-cadence setting, allowing only deployment image/log-router version updates.
The `rollback` operation prepares the recorded previous definitions for another
normal backend deployment; it does not update services itself. Do not apply the
whole application Terraform root with synthetic secrets to change these timers.

Use `ops/prepare_app_cutover.py prepare --directory <private-operator-directory>`
only after the code has passed checks and merged. It verifies the exact AWS
account, stable services and task families, copies only RPC credentials in
memory to dedicated SecureStrings, adds narrowly scoped token access, and
registers task definitions that change only RPC secret references. It does not
start tasks or change the live ordinary URL. Its journal contains identifiers
and changed reference names, never credential values.
It also prepares the inactive standalone keeper family, whose environment and
secret references must match the consolidated keeper for deployment preflight.
The standalone service must remain at zero tasks; rollback restores both families.

Publish the reviewed gateway image with `rpc-node-admin.yml`, then use a saved,
reviewed plan in `infra/terraform-rpc` to update only its pinned image/task/service.
Run `ops/qualify_app.mjs` in a
short-lived gateway task: it proves actual primary and historical routing,
bounded concurrent reads, and fallback using an isolated failed-primary fixture.
Save its successful, fresh result as `app-routing-qualification.json` in the
operator directory. Repeat from the reconciler's separate security group and
save `reconciler-routing-qualification.json`, including its `security_group`.
The standalone RPC state owns only a TCP/443 gateway ingress rule from that
existing group. The live Nitro node is not interrupted for either probe.

`ops/prepare_app_cutover.py activate --directory <private-operator-directory>`
requires that qualification and unchanged original Alchemy parameter, then
marks the prepared task revisions ready without changing any shared URL.
Deploy backend from reviewed
`master` with `bootstrap=false`, followed by the Sepolia frontend; verify workflow
SHAs/results, application endpoints, AA readiness and actual gateway primary
requests before declaring the cutover complete. Existing running tasks keep
their injected RPC configuration until the deployment replaces them.

Include `infra/terraform/sepolia-own-rpc.tfvars` after the current release and AA
overlays in future application Terraform plans. Do not apply the application
root with fabricated signer/database inputs to perform this cutover.
For rollback, `ops/prepare_app_cutover.py rollback` registers the prior RPC
references to the preserved Alchemy parameter, then the backend workflow must
redeploy from reviewed `master`.
The browser can keep its proxy URL during that rollback.

## Alto RPC migration

Alto can use the same private gateway for simulation, chain reads and bundle
submission, with Alchemy retained as its fallback. Its pinned CLI cannot configure
HTTP headers. The derivative image in `infra/alto/Dockerfile` preserves the pinned
Alto application and adds a Node preload that authenticates only the exact gateway
origin. Credentials remain in SSM and headers; they are never placed in URLs.

Before the cutover, publish the reviewed gateway image through
`rpc-node-admin.yml` and review the standalone RPC Terraform plan. The plan may
update the gateway image/task/service and add HTTPS ingress from the existing
Alto security group. It must not alter the Nitro instance, its database or other
application services. The gateway pins Alto's addressed `latest` log requests to
a fresh numeric head and retains the 5,000-block bound. Ambiguous write failures
are returned to Alto for reconciliation; the gateway never replays those writes
against the backup.

Prepare the exact runtime SSM grants and protected deployment-role permissions
declared in Terraform. Retain access to the original Alchemy parameter for
rollback. Use the dedicated protected workflow from reviewed `master`:

```bash
gh workflow run alto-rpc-cutover.yml \
  --repo Plether-Fi/plether-app --ref master -f environment=sepolia
```

The workflow builds an immutable authentication image and requires a fresh,
complete scan with zero critical findings. It does not renew the old image
exception. A canary without signer credentials checks the new path from Alto's
own network, including simulation overrides, primary routing, bounded log scans
and isolated Alchemy fallback.

The operator temporarily returns a retryable 503 for new Alto POST requests. A
separate observer uses the API network to verify that admission is paused, wait
for the API acknowledgement timeout, and observe all executor wallets available
twice from the same process. Alto 1.2.7's outstanding-operation gauge accumulates
normal queue pops, so its value cannot establish whether the queue is empty.
Instead, the observer checks the complete durable submission journal in read-only
database snapshots. Every operation marked submitted must have canonical terminal
event evidence or an expired validity window, verified against a fresh reconciler
boundary on both RPC providers. Unaccounted submissions or inconsistent evidence
block the cutover. Every journal record is scanned. Historical terminal
operations whose signed validity expired before an independently verified safe
cutoff use complete SQL validation and aggregate counts; only the still-actionable
frontier needs fresh receipt checks. This proves drain safety without replaying
all historical receipts. The operator also verifies that Alto accepts submissions
only from the expected journal-backed API path. The observer receives only the database
and RPC credentials needed for these reads, with no task role or signer secrets;
it emits check results and aggregate counts only.
Operations whose submission outcome is unknown retain the existing journal and
same-hash recovery path. The old signer task must reach actual `STOPPED` before
the replacement starts. Signers, gas budgets, validation settings and all other
task configuration remain unchanged apart from the reviewed image, preload and
RPC credential bindings.

Promotion requires healthy ECS targets and public AA entrypoint/gas-price checks.
Failure triggers restoration of the exact recorded baseline task definition with
the same stop-before-start rule. The workflow retains identifier-only rollout and
qualification records for seven days. A `rollback_failed` result requires operator
attention: retain the recorded admission barrier and task identifiers, and do not
start a second signer instance manually.

After successful qualification, pin the published gateway image in the standalone
RPC inputs and set `alto_rpc_auth_image`, `alto_rpc_auth_token_ssm_parameter_name`
and `alto_rpc_url_ssm_parameter_name` in the Sepolia application overlay. This
records the deployed configuration for later Terraform use; it does not require
applying the whole application root.
