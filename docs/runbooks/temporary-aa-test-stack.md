# Temporary AA test stack outside Singapore

## Operator intent

The approved target is the temporary **backend stack in Frankfurt
(`eu-central-1`)**, with the frontend run on **localhost**. The user explicitly
requires reuse of the existing Core v1.2.3 contracts: **do not deploy Core or
the frontend**. Singapore and the existing public frontend must stay untouched.
The user approved preparation of dedicated state, secrets and a live plan, then
replaced the original HTTPS proposal with private Session Manager tunnel access.
**No Cloudflare, custom hostname, certificate or DNS record is required.** The
user subsequently approved applying the reviewed dormant stack, installing the
local Session Manager plugin and verifying connectivity. That apply is complete.
Service activation, funding and changes to Singapore remain outside this approval.

Warsaw is AWS Local Zone `eu-central-1-waw-1a`, parent region Frankfurt
(`eu-central-1`), not a standalone AWS region. ECS on Fargate is not supported
in Local Zones, and the Local Zones feature matrix does not list RDS for Warsaw.
The current Fargate/RDS stack therefore cannot be placed wholly in Warsaw
unchanged. The user approved Frankfurt instead; no EC2/Local Zone redesign is
needed for this temporary test.
Sources: [AWS Local Zones](https://docs.aws.amazon.com/local-zones/latest/ug/available-local-zones.html),
[ECS Local Zone restrictions](https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-regions-zones.html),
[service availability](https://aws.amazon.com/about-aws/global-infrastructure/localzones/features/).

The current single-provider implementation is a separate source change. Its
`sepolia-aa-canary-preparation.tfvars` file still describes Singapore and is
**not** the temporary stack's deployment profile. Do not apply that file to
satisfy this request. The new Frankfurt profile describes dormant infrastructure;
all ECS service counts, issuance and submission remain off. The dormant Frankfurt
infrastructure is applied; the relay and database are running and incur costs.

## Implemented preparation

The target tuple is `932542905614 / eu-central-1 / sepolia-aa-temp`, with
logical environment `sepolia` and chain `421614`.

- `infra/terraform/deployment_target.tf` separates resource identity from network.
  Legacy names stay unchanged when `deployment_id` is empty. The shared GitHub
  OIDC provider remains owned by the legacy state; Frankfurt references its ARN
  without creating or importing it. A `moved` block preserves its legacy state
  address when adding the conditional count.
- `infra/terraform/frankfurt-aa-preparation.tfvars` plans the isolated backend,
  Alto and KMS resources but **hard-blocks starting any service**. Core bindings
  are checked against the committed v1.2.3 release. Activation requires a further
  reviewed change; overriding desired counts is not sufficient.
- `infra/terraform/frankfurt.backend.hcl` selects a dedicated bucket and key:
  `plether-sepolia-aa-temp-tfstate-932542905614`,
  `plether/sepolia-aa-temp/terraform.tfstate`, in Frankfurt. The dedicated bucket
  is now created, encrypted, versioned and private, with access limited to the
  verified operator and account-root recovery. It is managed outside stack state;
  its reviewed policy is `infra/terraform/frankfurt-state-bucket-policy.json`.
- `.github/workflows/aa-frankfurt-preparation.yml` runs **mocked** AWS plans on
  source changes. It has no AWS authentication, deployment dispatch or apply step.
  Negative cases cover Singapore, worker activation, cross-stack secret paths,
  older Core bindings and public CORS; a legacy plan checks existing names.
- Existing backend/Alto/admin deployment workflows remain Singapore-only. Do not
  dispatch them for Frankfurt. The new IAM trust restricts future temporary-stack
  deployments to the separate `sepolia-aa-temp` GitHub environment; required
  reviewers and master-only deployment rules must be configured before use.

### Authenticated plan

The original 204-resource public-HTTPS plan is **superseded; do not apply it**.
See the [preparation evidence](frankfurt-aa-preparation-evidence.md) for the
replacement private-access plan and review hash. All 10 ECS services remain at
zero tasks. The 211-resource plan has now been applied once; do not replay it.
Do not use the worktree's initialized Terraform directory for
Frankfurt. To regenerate from repository root:

```bash
AWS_PROFILE=plether FRANKFURT_USE_PREPARED_INPUTS=1 node scripts/plan-frankfurt-aa.mjs
```

Use Terraform 1.16.x (`TERRAFORM_BIN` can select its absolute path). The helper
checks the named AWS profile/account, stages only source configuration into a
fresh private directory, initializes the fixed Frankfurt backend and runs only
`plan`. It never copies local `.terraform`, auto-tfvars or Singapore state, and
never imports, migrates, applies or deploys. With the explicit prepared-inputs
flag, it loads the operator-only encrypted
`/plether/bootstrap/sepolia-aa-temp/terraform-inputs` parameter into child-process
`TF_VAR_*` environment variables. This path is outside runtime ECS wildcard
permissions. It contains the approved RPC inputs, fresh distinct worker keys,
database password, fresh origin token and existing PostHog project token.
Without that flag, supply `TF_VAR_*` securely yourself. Keep credentials out of
shell history and logs. The planner never generates or rotates keys.

`scripts/prepare-frankfurt-aa-secrets.mjs --prepare` performed the separate,
user-approved bootstrap. It is non-overwriting and uses encrypted SSM. The macOS
AWS CLI Python driver accepts secret JSON through stdin because `/dev/stdin`
cannot be reopened on these child-process pipes; secret payloads never enter OS
arguments or local files. The existing `plether` credential bridge must retain
its login session's own region: only Terraform resources/backend select Frankfurt,
not global `AWS_REGION` / `AWS_DEFAULT_REGION` overrides.

The saved binary plan contains secrets: keep it private, never upload it as a
CI artifact, and inspect it only in a trusted terminal. Review creations and
IAM policies against the exact temporary namespace before any separately
approved apply. The unused earlier certificate request is not referenced by the
replacement profile and needs no validation. Alarm routing, Alchemy shared-quota qualification and funding allocations remain
activation prerequisites. Pyth/PostHog credentials are reused from the backend;
the dormant plan emits no traffic to those services. Confirm analytics isolation
or accept shared-project logs before starting workers.
The applied dormant stack incurs database/load-balancer costs,
plus the small relay instance, disk and public IPv4 address.

### Private access design

`frankfurt_tunnel.tf` creates one Amazon Linux 2023 ARM64 `t4g.nano` relay with
an encrypted 8 GiB root volume, mandatory IMDSv2, no SSH key and **no inbound
security-group rules**. Its public IPv4 address is used for outbound Session
Manager TLS connections, not public API access. Its role grants only instance
heartbeat and session-channel permissions: no backend secrets, signing or
Parameter Store reads. The AMI is pinned and checked against Amazon-owned ARM64
images. The relay may send HTTPS outbound and HTTP to the API ALB security group;
it has no database access.

The API ALB is internal. Its only inbound rule is HTTP port 80 from the relay
security group. Laptop-to-relay traffic uses authenticated Session Manager;
relay-to-ALB HTTP stays inside the VPC. The AWS-generated internal ALB name is
resolved inside AWS; there is no custom DNS setup. A dedicated Session document
fixes the destination to this ALB, remote port 80 and local port 18081, with no
caller-supplied host/port parameters. This is an operator test path, not a public
production endpoint. Existing administrators can still use their other AWS
permissions; the document is not a restriction on an account administrator.

Sources: [Session Manager](https://docs.aws.amazon.com/systems-manager/latest/userguide/session-manager.html),
[remote-host forwarding requirements](https://docs.aws.amazon.com/systems-manager/latest/userguide/session-manager-working-with-sessions-start.html).

### Localhost frontend, after backend qualification

The local-only config lives under `scripts/`, outside the deployed frontend's
path filter. Neither the public manifest nor the frontend deployment workflow
was changed for this setup. No frontend deployment is required.

1. Export only `deployment_target` and `frankfurt_tunnel` from the **applied Frankfurt** Terraform
   outputs into a private JSON file outside the served frontend directory.
   Its shape is Terraform's `output -json` envelope, retaining each `value`.
   Do not use unknown values from a plan or outputs from the superseded stack.
   Verified outputs for this applied stack are saved at
   `/var/folders/xz/scmmjjld2pq8k763z54mzjtr0000gn/T/plether-frankfurt-plan-RGtpjn/frankfurt-outputs.json`
   (mode 0600, no credentials). This is temporary storage; regenerate from the
   isolated remote state if the local directory is removed.
2. Install the official [AWS Session Manager plugin](https://docs.aws.amazon.com/systems-manager/latest/userguide/session-manager-working-with-install-plugin.html)
   on the operator machine and authenticate the named `plether` AWS profile.
   Version 1.2.835.0 is now installed at `~/.local/bin/session-manager-plugin` on
   this operator machine. The verification session has been closed.
   With Node 25, from repository root in a dedicated terminal run:

   ```bash
   node scripts/aa-frankfurt-tunnel.mjs /absolute/private/path/frankfurt-outputs.json
   ```

   The helper verifies account, running instance, online agent (at least
   3.1.1374.0), internal ALB, security groups, fixed Session document and vacant
   local port before starting the session. Leave it running; Ctrl-C closes it.
   No credentials are copied onto the relay. After separate API activation,
   `curl --fail http://127.0.0.1:18081/api/health` must pass before AA testing.
   A dormant zero-task API cannot pass this check.
3. In another terminal, set `AA_FRANKFURT_OUTPUTS_FILE` to that file, `AA_PROXY_ORIGIN_TOKEN` to the
   dedicated Frankfurt origin token, and `AA_FRANKFURT_PAYMASTER_ADDRESS` to the
   separately deployed and verified paymaster. Use server-side environment
   injection, not `VITE_*` credentials. Never use the Singapore origin token.
4. Remove `VITE_API_URL`, `VITE_API_PROXY_TARGET` and
   `VITE_API_PROXY_PRESERVE_PATH` overrides. From `apps/frontend`, run:

   ```bash
   npm run dev -- --config ../../scripts/aa-frankfurt.vite.config.mts
   ```

5. Open `http://127.0.0.1:5173`. The local config pins the port and loopback bind,
   rejects foreign request origins and public-host overrides, and serves an
   in-memory native manifest at `/perps-aa-manifest.frankfurt.json`. Its only
   upstream is the fixed tunnel at `http://127.0.0.1:18081`.
   Both native AA URLs route through the local `/api/perps/v1/aa/rpc` proxy,
   which rewrites to `/api/aa/rpc` and supplies the origin token server-side.
6. Sponsorship stays off unless `AA_FRANKFURT_SPONSORSHIP_ENABLED=true` is
   explicitly set **after** backend qualification. This cannot bypass backend
   allowlisting, spending limits, pause state or deployment guards. The config
   refuses to start without the endpoint/token/paymaster; it does not fall back
   to the live manifest or Pimlico.

Tests: `node --test scripts/aa-frankfurt.test.mjs` checks fail-closed profile
selection. With frontend dependencies installed,
`node --test scripts/aa-frankfurt-vite.test.mjs` loads the real Vite config and
checks routing using real loopback HTTP requests, token injection, spoof removal,
credential non-injection on unrelated routes, loopback enforcement, foreign-origin
rejection and closed-tunnel failure without fallback. Tunnel metadata tests reject
public/SSH access and target drift; mocked Terraform plans preserve legacy access
and reject certificates/custom hostnames. These tests do not establish live SSM
connectivity. A separate live post-apply check reached the internal ALB and got
the expected 503 with no API tasks; application health and sponsorship remain
unqualified. The existing frontend
test workflow runs the HTTP test and typechecks the separate config.

## Isolation requirements before a deployment plan

- Introduce a distinct deployment identity (for example `sepolia-aa-temp`)
  separate from the blockchain network (`arbitrum-sepolia`, chain 421614).
  Resolve the exact AWS account/region/identity as a reviewed target tuple.
- Use a separate Terraform state key and lock, with an explicitly selected
  backend. Never reuse `plether/sepolia/terraform.tfstate`, migrate the existing
  state, or import Singapore resources into the temporary state.
- Create a new VPC, database, ECS cluster/services, load balancers, log groups,
  signer KMS key, secret namespace, alarms, and regional image repositories.
  Account-global IAM role/policy names must also include the deployment identity;
  a different region alone does not isolate IAM.
- Provision separate database credentials, edge-to-origin credentials, and
  service wallets. The Alto keys already created under Singapore's
  `/plether/sepolia/` namespace are not automatically portable or reusable.
  No key may be used by two running services/stacks concurrently.
- The user approved Alchemy as the current RPC provider. Sharing its exact key
  also shares quota/rate-limit and credential-rotation impact with Singapore.
  For strict non-impact, use a separate Alchemy application/key or approve and
  measure a bounded shared quota; do not call regional separation sufficient.
- Use separate GitHub protected environments, OIDC subjects and deployment
  target mapping. Preserve exact account/region/resource checks. Replace the
  hardcoded Singapore assumptions with reviewed per-target bindings, not broad
  wildcards or disabled safety checks.
- Do not deploy a frontend, preview project, or Worker, and do not overwrite
  `plether-testnet`, its domains, manifests or secrets. Configure only a local
  development frontend/proxy to target the Frankfurt backend and a local
  native-AA manifest bound to existing v1.2.3 contracts. Keep origin credentials
  in the server-side local proxy environment, never browser `VITE_*` variables
  or committed files. Explicitly test the AA route/proxy path; merely changing
  `VITE_API_URL` is not sufficient evidence that AA uses the temporary backend.
  Configure exact localhost development CORS origins on the temporary backend
  only; do not widen Singapore CORS or assume CORS provides authentication.
- Isolate scheduled jobs, keeper identities, notification routing, analytics
  and other outbound integrations. Keep write-capable workers off until their
  on-chain scope is explicitly resolved.
- Review the saved plan for **only** the temporary target's creations/changes;
  any Singapore resource update, deletion, DNS change or IAM alteration outside
  the temporary namespace is a blocker. Record tested rollback and teardown
  targets before applying. Do not use a broad teardown command.

## Selected on-chain target: existing Core v1.2.3

The user selected existing Core **v1.2.3**, as pinned in
`config/perps/arbitrum-sepolia-v2.json`. Do not deploy fresh Core contracts,
change their ownership, or update the existing Singapore frontend.

The user reports that the current frontend uses v1.2.2. A read-only public
manifest check on 2026-09-10 confirmed it still serves the older
`perps-aa-arbitrum-sepolia-20260906-v2` manifest, router
`0xbd2f286efca5F761E21452673ab9b8C14e17aad7`, and engine
`0x9611E643aC4691E8fDeD8a0c2C22c56438B6f352`. The localhost frontend and temporary backend
must instead use the existing v1.2.3 bindings (router
`0x6215d36fcbd610ca1525252eebcbfd8b223a6072`, engine
`0xafece93321be41aa73474457e2f47cf7b2fb738f`) without repointing the live site.

AWS regions do not isolate chain state. Test transactions will modify existing
v1.2.3 state and may affect any other consumers of that release. Before enabling
write workers, inventory their exact contract targets and prevent duplicate
keepers/oracle/settlement activity; do not infer every Singapore backend target
from the frontend manifest alone. A separate paymaster isolates its sponsorship
deposit, not Core state. Any later paymaster deployment is separate from Core
and remains subject to the existing approval and qualification gates.

A local fork can support isolated preliminary tests but is not evidence that
the public Arbitrum Sepolia deployment path has been qualified.

## Proposed sequence

1. Merge the explicitly gated single-provider-mode PR after CI passes.
2. Use Frankfurt (`eu-central-1`), confirm the isolated deployment identity and
   RPC-quota choice, and inventory writers against existing v1.2.3.
3. Implement target-aware Terraform/workflow and localhost proxy bindings in a separate PR,
   with negative tests proving Singapore targets cannot be selected accidentally.
4. Prepare an isolated dormant-stack plan and explicit funding allocations.
5. Review before apply; reuse existing Core v1.2.3 and deploy only separately
   approved AA contracts paused/unfunded and services stopped as appropriate.
   Complete the existing AA qualification gates for this exact deployment.
   No frontend deployment, public-manifest switch, Core deployment or mainnet rollout.
6. After testing, tear down only the reviewed temporary resources, preserving
   required audit evidence and accounting for on-chain funds/stake delays.
