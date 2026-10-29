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

## Provider recommendation

Recommend **QuickNode Build, month-to-month**, for Ethereum Sepolia execution
and beacon access. Its Ethereum APIs include Sepolia, and its blob-sidecar
documentation explicitly states complete historical coverage. The public Build
price is $49/month on monthly billing; 80M credits and 50 requests/second are
included. Do not commit annually before measuring catch-up and steady-state use.

Recommend **Chainstack Growth, month-to-month**, as the second operator: an
Ethereum Sepolia beacon backup and an Arbitrum Sepolia reference endpoint.
The advertised base is $49/month, 20M request units, 250 requests/second and
10 endpoints. Its Arbitrum endpoint can later be evaluated as the independent
AA verifier. That AA migration is separate from this pilot.

Allow approximately **$98/month in base external-provider fees**, plus method-
weighted usage, overages and any separately quoted beacon/blob entitlement.
This is outside the modeled AWS bill and is a recommendation, not a purchase
or confirmation of the account-specific service entitlement.

Before purchasing/launching, verify Ethereum Sepolia chain ID, beacon network,
historical blobs from the chosen snapshot's parent-chain checkpoint, complete
data-subnet coverage, request limits, and primary/backup behavior with actual
credentials. The published Arbitrum table distinguishes mainnet historical
support from Sepolia beacon support; it does not prove Sepolia historical
coverage for every provider. Qualify that coverage on both beacon endpoints.

Sources checked 29 October 2026:

- [QuickNode networks and APIs](https://www.quicknode.com/docs/ethereum/api-overview)
- [QuickNode historical blob API](https://www.quicknode.com/docs/ethereum/eth-v1-beacon-blob_sidecars-id)
- [QuickNode pricing](https://www.quicknode.com/pricing)
- [Chainstack pricing](https://chainstack.com/pricing/)
- [Arbitrum provider matrix](https://docs.arbitrum.io/run-a-node/l1-ethereum-beacon-chain-rpc-providers)
- [Blob/subnet requirements](https://docs.arbitrum.io/run-a-node/beacon-nodes-historical-blobs)

The gateway's reference endpoint is separate from its Alchemy backup, so an
Alchemy outage does not block primary-node promotion. Native AA must still
perform its own independent safe-block verification; the router's health checks
are not a replacement for that security property.

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

Create externally managed SSM **SecureStrings** without putting values in
Terraform or command-line literals:

| Parameter | Value |
| --- | --- |
| `/plether/sepolia/rpc-node/parent` | JSON object shown below |
| `/plether/sepolia/rpc-node/gateway-token` | Random token of at least 32 characters |
| `/plether/sepolia/rpc-node/backup-url` | Dedicated Alchemy Arbitrum Sepolia origin URL |
| `/plether/sepolia/rpc-node/backup-token` | Optional bearer token if the backup URL is keyless |
| `/plether/sepolia/rpc-node/reference-url` | Independent non-Alchemy Arbitrum Sepolia HTTPS origin |
| `/plether/sepolia/rpc-node/reference-token` | Optional bearer token for that reference |

Do not use `/plether/sepolia/perps-rpc-url` as the backup parameter: changing
that live alias during cutover would make the gateway point back to itself.
The reference must be a separately operated origin, not another gateway alias.
The code rejects Alchemy reference hostnames and URLs identical to the backup.

Parent JSON (example structure, no credentials):

```json
{
  "execution_url": "https://execution-provider.example/PRIVATE_ENDPOINT/",
  "beacon_url": "https://beacon-provider.example/PRIVATE_ENDPOINT/",
  "secondary_beacon_url": "https://second-beacon-provider.example/PRIVATE_ENDPOINT/"
}
```

The initial adapter accepts HTTPS endpoints with URL-based provider credentials.
It does not implement per-provider custom authentication headers. Check that
the selected provider endpoints fit this contract before provisioning. Configure
an HA execution origin, and qualify its provider-level failover; this increment
does not implement automatic switching between two L1 execution origins.

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
gateway logs contain only readiness flags, routing role and duration.

The gateway caps requests at 512 KiB, responses at 8 MiB, batches at 20 calls,
and each task at 16 concurrent requests / two trace simulations. Log scans
require an explicit block hash or numeric range of at most 2,001 blocks. Only
allowlisted methods are accepted; historical transaction traces and signing
methods are excluded. Raw transaction sends, including mixed batches, are
never automatically replayed after an ambiguous failure. Reverts are preserved.

## Provisioning and publishing

1. Review a saved Terraform plan using the existing Sepolia base variables
   plus the reviewed RPC overlay. Keep `gateway_image = ""` initially. The first
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

Staging adds approximately **$393/month, or $0.54/hour**, at the inspected
Singapore gp3 storage price while it exists. This is separate from the steady-
state node budget. After a successful import, unmount staging on the host,
remove its UUID entry from `/etc/fstab`, then set `staging_volume_gib = 0` and
review the plan to detach/delete the staging volume. This updates the runtime
SSM configuration without replacing the node. A detached volume still incurs
charges until deleted. Inspect data-volume IDs carefully before cleanup.

Never stop/start the **EC2 host** as an upgrade or restart procedure: local
instance-store data is lost on stop/termination. Restart the Nitro process.
On host loss, keep live traffic on the qualified backup and perform an explicit
snapshot re-seed on a replacement. Do not treat image rollback as database
rollback. This pilot includes no automatic host replacement or data deletion.

The admin workflow permits only `status`, a nonblocking process `restart`, or
gateway image publication. Its role cannot stop EC2, apply Terraform or send
arbitrary shell. Use `gh` to dispatch and monitor it. A restart request succeeding
means SSM accepted the process restart; inspect readiness afterwards. It refuses
to interrupt a service still importing its snapshot.

## Qualification and follow-up

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
