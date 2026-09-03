# Self-hosted Account Abstraction Rollout

This runbook rolls out the Arbitrum Sepolia account-abstraction stack described
in [ADR 0002](../adr/0002-self-hosted-account-abstraction.md): a single-active
Alto bundler behind an internal load balancer and a native Plether verifying
paymaster implemented by the Haskell API. It is deliberately Sepolia-only.

The rollout is fail-closed and reversible. Provisioning the dormant stack,
deploying the paymaster, starting Alto, enabling native sponsorship issuance,
and qualifying or later activating the native transport field set in a v2
manifest are separate gates. The public manifest is already v2 but still uses
Pimlico; a version suffix alone never selects native routing. Passing one gate
does not authorize the next one.

## Fixed deployment identity

The following values are part of the reviewed Sepolia profile. Stop if chain
reads disagree with them:

| Item | Reviewed value |
| --- | --- |
| Chain | Arbitrum Sepolia (`421614`) |
| EntryPoint | `0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108` (v0.8) |
| EntryPoint runtime hash | `0xe3f30f78ae55058acdefea00952c8e44f2263215cf720fe1b27b6f148add0278` |
| SimpleAccount factory | `0x13E9ed32155810FDbd067D4522C492D6f68E5944` |
| Factory runtime hash | `0xa2e635152a61e180383c7afc045620b7461ef6f43ba27d592262513106b991b7` |
| SimpleAccount implementation | `0x28426d752372d68d34340bd94390950dce3c9ec3` |
| Implementation runtime hash | `0x689a90eff03926a12aedad2fc6d4fdbcbdd9ffac86e7d0d70ce6355961305c74` |
| SimpleAccount proxy runtime hash | `0x41ee894da413cc99e8dec0a1784470eceb736845ad1591e06ff0ecdf0aca26c9` |
| Reviewed deployed proxy sample | `0x81237a8Fc8D6F616d5F151c69865f365dF5fF052` (factory-derived index zero) |
| Current public manifest | `perps-aa-arbitrum-sepolia-20260830-v2`, exact Pimlico transport shape |
| USDC | `0x1647e41f49ED6D688936092B5a291c4B28106343` |
| Order router | `0x97A901dE2B267c307E264FD5F71403F8072F73e7` |
| CFD engine | `0x3dc9C0A1f9C745A4B08BD5C2E6c7aE613561c20D` |
| Margin clearinghouse | `0x2f98787F6dCC3b1f2E4a2AFa5acf410159b9F211` |
| Order lifecycle book | `0xa210928a7E0AE27626B8d0E67Bbd82305438aB9E` |
| Policy evaluator | `0xaa4703B190684b5A57b8a9aA432fA043B169D171` |
| Policy ID | `0x8dd77324b94da492342191f762a32cdf99e828a7f24d77c8ed5ace90cf4f5ae3` |
| `paymasterData` envelope | 157 bytes |
| Packed v0.8 `paymasterAndData` | 209 bytes (20-byte address + two 16-byte gas fields + envelope) |
| Paymaster version | `plether-verifying-v1` |
| Alto image | `ghcr.io/pimlicolabs/alto:v1.2.7@sha256:28cee87ea6b58ba10a37273e58602b50321516c36a81d0c35d50526d1f06995d` |
| Alto CREATE2 deployer | `0x4e59b44847b379578588920cA78FbF26c0B4956C` |
| Public native gateway | `/api/perps/v1/aa/rpc` |
| Backend native gateway | `/api/aa/rpc` |
| Approved Sepolia API origin | `https://api.sepolia.plether.com` |
| Admin-task OTLP destination | `eu.i.posthog.com:443/i/v1/logs` |
| AWS deployment boundary | Account `932542905614`, region `ap-southeast-1` |

The EIP-712 fixture type hash is
`0x5835c142c681b663470a1a53c34b0ba256a8283b7b9f9560aadb85711d252918`,
and the reference viem digest is
`0xd92042495de3ae32c76391a73aeb6bfaf515af2dd3da45c9a8921b5310cde1ea`.
Both must continue to pass in the contract, backend, and client test suites.

## Non-negotiable safety rules

- Use the GitHub CLI for every workflow authentication, dispatch, approval,
  status, and log operation. Do not use the GitHub Actions web UI.
- Use the named AWS profile `plether` for every local AWS and Terraform
  command. Verify `aws --profile plether sts get-caller-identity` before work.
- Do not use `bootstrap=true` for an ordinary backend deployment. That mode
  only builds images and does not deploy services.
- Keep `enable_native_aa_sponsorship=false` and native submission disabled
  until every pre-issuance gate passes. Keep the standard public manifest on
  its exact v2 Pimlico transport shape throughout this reviewed canary
  rollout; do not add any native transport field to it.
- Keep the legacy Pimlico route and credentials available during rollout and
  drain. They are required to recover operations already recorded in browser
  journals even after new issuance moves to the native provider.
- Never treat an Alto rejection, timeout, missing receipt, API restart, or
  task replacement as proof that a signed authorization is unused. Only the
  safe-chain reconciler may settle or expire signed liability.
- A stub response must not reserve a budget or invoke KMS. A final response
  must reserve maximum liability transactionally and persist its signature
  before returning it.
- Treat the canary owner list as an admission cohort, not caller
  authentication. ERC-7677 final paymaster requests are unsigned; a spoofed
  request can reserve budget for an allowlisted owner even though it cannot
  produce that account's signature or spend the authorization.
- Run exactly one Alto task. Its ECS deployment configuration must remain
  minimum healthy `0`, maximum `100`, with horizontal scaling and Redis
  disabled. Never allow old and new executor sets to run concurrently.
- `ALTO_SAFE_MODE=false` is approved only for explicitly allowlisted,
  low-liability Arbitrum Sepolia canaries using known accounts. With
  `ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION=false`, Alto still performs
  simulation, signature, and runtime validation, but it omits the ERC-7562
  tracer, entity-role, reputation, and related safe-mode protections. Keep the
  public v2 manifest on its Pimlico transport and hard-block Sepolia global
  access and every mainnet rollout until a safe-mode-equivalent configuration
  has been implemented and qualified.
- Executor keys, the utility key, the paymaster KMS key, deployer, treasury,
  faucet, oracle, and keeper keys must all be distinct.
- Both external Alto RPC secrets must be authenticated `https://` endpoints on
  TCP `443`, matching the task security-group egress. `ALTO_RPC_URL` must be
  write-capable: v1.2.7 uses it for simulation-contract deployment and utility
  wallet refills as well as reads. The optional send RPC is only for private
  bundle submission and cannot substitute for that capability.
- Reconciliation must use two authenticated HTTPS/443 Arbitrum Sepolia RPCs
  operated independently. They must agree on chain ID, canonical headers, the
  conservative `safe` boundary, and this paymaster's complete EntryPoint event
  set before the reconciler may advance its cursor.
- The dormant/recovery API task role has no paymaster-signing-key permission and
  must not initialize the signer adapter or call `GetPublicKey`. Only a
  protected operator attestation/admin principal and an issuance-enabled API
  task role may receive exact-paymaster-key `DescribeKey`, `GetPublicKey`, and
  `Sign` permissions. If the secondary-RPC SecureString uses a customer-managed
  key, the API execution role may separately receive narrowly scoped
  `kms:Decrypt` through SSM; that is not signer access.
- The issuance-enabled API task role currently holds direct `kms:Sign` on the
  paymaster key. A compromised API process or stolen task credentials could
  therefore bypass off-chain budgets and policy checks and sign arbitrary
  otherwise-valid paymaster digests until the small on-chain deposit is spent;
  the contract's per-operation cap is the remaining bound. Accept that blast
  radius only for this low-funded, staffed Sepolia canary. Public/global use
  and mainnet require an isolated signer/policy service or equivalent on-chain
  semantic/global controls, with direct signing removed from the API role.
- Do not destroy the KMS key, paymaster contract, database rows, old frontend
  journal decoder, or legacy recovery route during rollback or drain.
- Do not casually rotate `AA_PROXY_ORIGIN_TOKEN`. The current implementation
  also uses it as the HMAC key for durable client/account pseudonyms, so a
  unilateral or early rotation breaks submission, idempotency, and recovery
  correlation for outstanding operations. Follow the dedicated rotation
  procedure below.
- This profile accepts upgradeable SimpleAccounts only on Sepolia. Before
  stub, final sponsorship, and submission, the API must verify the ERC-1967
  implementation slot and an empty beacon slot. Do not enable this profile on
  mainnet.
- Every allowlisted account must be Plether-controlled, and its ownership
  transfer and UUPS upgrade paths must remain administratively frozen and
  monitored from issuance through maximum validity and safe drain. The API
  authorizes owner/runtime/slots at a common safe block, while inclusion runs
  against newer state; a post-safe transfer or upgrade can use or reinterpret
  a still-valid authorization. A `latest` read would only move, not eliminate,
  that mempool TOCTOU. Accept it only for the low-cap trusted Sepolia canary;
  public/global and mainnet require immutable execution or on-chain live-policy
  enforcement plus a new review.

## Operator setup and evidence

Use one fresh strict shell for the entire run. Do not paste a trailing command
from a guarded block into a different or non-strict shell: failed `test`,
`jq -e`, `cast`, `gh`, `aws`, or Terraform commands must stop the sequence
before any later mutation or broadcast. Keep secrets out of the command line, shell history,
workflow inputs, Terraform plan/output evidence, and incident notes. Terraform
currently manages some sensitive SSM values, including the origin token, so use
only the established encrypted, access-controlled state backend and never
export or attach its state:

```bash
set -euo pipefail
umask 077

cleanup_aa_runbook() {
  local temporary_path
  for temporary_path in \
    "${AA_TF_PLAN:-}" \
    "${KMS_PUBLIC_DER:-}" \
    "${AA_SIGNER_TF_PLAN:-}" \
    "${AA_ALARM_EVIDENCE:-}" \
    "${AA_RECON_ALARM_EVIDENCE:-}" \
    "${MANIFEST_HEADERS:-}" \
    "${MANIFEST_BODY:-}"; do
    test -z "$temporary_path" || rm -f -- "$temporary_path"
  done
}
trap cleanup_aa_runbook EXIT
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

alto_dispatch_fingerprint() {
  jq -cn \
    --arg action "$1" \
    --arg api_hostname "$2" \
    --arg environment "$3" \
    --arg rollback_task_definition "$4" \
    --arg utility_gas_cushion_wei "$5" \
    '{schema:1, action:$action, apiHostname:$api_hostname,
      environment:$environment,
      rollbackTaskDefinition:$rollback_task_definition,
      utilityGasCushionWei:$utility_gas_cushion_wei}' |
    shasum -a 256 | awk '{print $1}'
}

aa_admin_dispatch_fingerprint() {
  jq -cn \
    --arg action "$1" \
    --arg confirmation "$2" \
    --arg expected_reason "$3" \
    --arg operator_note "$4" \
    '{schema:1, action:$action, confirmation:$confirmation,
      expectedReason:$expected_reason, operatorNote:$operator_note}' |
    shasum -a 256 | awk '{print $1}'
}

export AWS_PROFILE=plether
export AWS_REGION=ap-southeast-1
export AWS_PAGER=
export EXPECTED_AWS_ACCOUNT_ID=932542905614
export APP_ROOT=/absolute/path/to/reviewed/plether-app
export PLETHER_CORE_ROOT=/absolute/path/to/reviewed/plether-core
export APP_REPOSITORY=Plether-Fi/plether-app
export TARGET_ENVIRONMENT=sepolia
export CHAIN_ID=421614
export ENTRY_POINT=0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108
export ENTRY_POINT_CODE_HASH=0xe3f30f78ae55058acdefea00952c8e44f2263215cf720fe1b27b6f148add0278
export SIMPLE_ACCOUNT_FACTORY=0x13E9ed32155810FDbd067D4522C492D6f68E5944
export SIMPLE_ACCOUNT_FACTORY_CODE_HASH=0xa2e635152a61e180383c7afc045620b7461ef6f43ba27d592262513106b991b7
export SIMPLE_ACCOUNT_IMPLEMENTATION=0x28426d752372d68d34340bd94390950dce3c9ec3
export SIMPLE_ACCOUNT_IMPLEMENTATION_CODE_HASH=0x689a90eff03926a12aedad2fc6d4fdbcbdd9ffac86e7d0d70ce6355961305c74
export APPROVED_ACCOUNT_CODE_HASH=0x41ee894da413cc99e8dec0a1784470eceb736845ad1591e06ff0ecdf0aca26c9
export REVIEWED_LIVE_PROXY=0x81237a8Fc8D6F616d5F151c69865f365dF5fF052
export POLICY_ID=0x8dd77324b94da492342191f762a32cdf99e828a7f24d77c8ed5ace90cf4f5ae3
export ALTO_CREATE2_DEPLOYER=0x4e59b44847b379578588920cA78FbF26c0B4956C
export ECS_CLUSTER=plether-sepolia
export API_SERVICE=plether-api
export ALTO_SERVICE=plether-alto
export AA_RECONCILER_SERVICE=plether-aa-reconciler
export SEPOLIA_SAFE_ADDRESS=0x_REPLACE_WITH_APPROVED_SAFE_ADDRESS
```

Replace the Safe placeholder from the approved governance record before any
contract operation and require `cast to-check-sum-address
"$SEPOLIA_SAFE_ADDRESS"` to return the exact configured value. Never infer the
owner from a deployer address or a previous shell.

Generate the origin credential exactly once with `openssl rand -hex 32`, but
run that command only inside the approved secret-capture environment so its
stdout is not retained in a terminal transcript, CI log, ticket, or evidence
bundle. The stored value must be exactly 64 lowercase hexadecimal characters.
Do not copy the deliberately invalid example, invent a human-readable value,
or use all zeroes, all `f`, repeated `0123456789abcdef`, repeated `deadbeef`, or
any other placeholder. Terraform and the backend reject the known placeholders
and any value that does not match `^[0-9a-f]{64}$`. Install that same generated
value through the approved Terraform/SSM and Cloudflare secret paths without
printing it. Rotation is not a routine credential swap because the current
backend also derives durable client keys from it; use the pause-and-drain
procedure below.

Create an evidence record containing the operator, UTC start time, AWS account
and role, release SHA, Terraform plan checksum, `plether-core` SHA, paymaster
address and deployment transaction, KMS key ARN and derived address, Safe
owner, simulation-contract addresses, EntryPoint deposit/stake, ECS task
definition revisions and image digests, database counts, reconciler cursor,
workflow run URLs, canary UserOperation hashes, and each gate decision. Record
addresses and digests, never private keys, signatures, RPC credentials, origin
tokens, or full signed UserOperations.

Until the AA package is merged into `plether-core`, its reviewed import artifact
in `plether-app` is `.codex-artifacts/plether-core-self-hosted-aa.patch`. It is
defined only against `plether-core` base
`bc8f6290c540665e4ff61328ea83a4c3d421a8d4` and its SHA-256 must be
`44410efdc9eccc81d3f558782b39514c8323abda70dbd6a3c89eef17ea994a82`:

```bash
test "$(git -C "$PLETHER_CORE_ROOT" rev-parse HEAD)" = \
  bc8f6290c540665e4ff61328ea83a4c3d421a8d4
test "$(shasum -a 256 \
  "$APP_ROOT/.codex-artifacts/plether-core-self-hosted-aa.patch" | awk '{print $1}')" = \
  44410efdc9eccc81d3f558782b39514c8323abda70dbd6a3c89eef17ea994a82
git -C "$PLETHER_CORE_ROOT" apply --check \
  "$APP_ROOT/.codex-artifacts/plether-core-self-hosted-aa.patch"
```

Those checks establish handoff provenance, not deployment approval. Import,
review, and merge the patch into `plether-core`, then qualify and deploy from a
clean reviewed commit. Never broadcast from the dirty applied artifact tree.

### GitHub and AWS preflight

Run before every dispatch:

```bash
gh auth status
gh api user --jq .login
export RELEASE_SHA="$(gh api repos/$APP_REPOSITORY/commits/master --jq .sha)"
export SEPOLIA_BACKEND_URL="$(gh variable get SEPOLIA_BACKEND_URL \
  --repo "$APP_REPOSITORY")"
test "$SEPOLIA_BACKEND_URL" = "https://api.sepolia.plether.com"
export API_HOSTNAME="${SEPOLIA_BACKEND_URL#https://}"
gh run list --repo "$APP_REPOSITORY" --limit 30

test "$AWS_REGION" = ap-southeast-1
[[ "$EXPECTED_AWS_ACCOUNT_ID" =~ ^[0-9]{12}$ ]]
test "$(aws --profile plether sts get-caller-identity \
  --query Account --output text)" = "$EXPECTED_AWS_ACCOUNT_ID"
test "$(gh variable get AWS_ACCOUNT_ID --repo "$APP_REPOSITORY")" = \
  "$EXPECTED_AWS_ACCOUNT_ID"
test "$(gh variable get AWS_REGION --repo "$APP_REPOSITORY")" = \
  "$AWS_REGION"

test "$(git -C "$APP_ROOT" rev-parse HEAD)" = "$RELEASE_SHA"
test -z "$(git -C "$APP_ROOT" status --porcelain)"
export PLETHER_CORE_SHA="$(git -C "$PLETHER_CORE_ROOT" rev-parse HEAD)"
test -z "$(git -C "$PLETHER_CORE_ROOT" status --porcelain)"
```

If either GitHub command fails with DNS, connection, or timeout errors in a
restricted shell, repeat the same read-only checks with network access. Request
authentication only after a network-enabled request reaches the service and
returns an actual credential error.
Apply the same rule to the named-profile STS check: repeat
`aws --profile plether sts get-caller-identity` with network access after a
connectivity failure, and request credential help only for an explicit AWS
credential error. Never substitute the default profile or infer expiry from
DNS/timeout failures.

Confirm that the release SHA contains the reviewed contract/client import,
native backend, reconciler, infrastructure, workflow, worker-route, and
manifest changes. Reject a detached, dirty, unreviewed, or partially merged
release. Before dispatching any workflow, check that no successful or running
run already represents the same workflow, action/inputs, and intended target
state for this SHA and environment:

```bash
gh run list \
  --repo "$APP_REPOSITORY" \
  --commit "$RELEASE_SHA" \
  --limit 30 \
  --json databaseId,displayTitle,workflowName,event,status,conclusion,headSha,url
```

Inspect any candidate with `gh run view`; do not treat a different protected
action as a duplicate merely because it has the same release SHA. GitHub's run
JSON does not expose arbitrary `workflow_dispatch` inputs. The reviewed
workflows therefore put the operation/environment/SHA in `run-name`; Alto and
AA-admin additionally emit a secret-safe SHA-256 of the complete ordered input
object from their credential-free preflight. Compute the expected value with
`alto_dispatch_fingerprint` or `aa_admin_dispatch_fingerprint`, then compare it
to the exact `Dispatch input fingerprint: sha256:...` line from the completed
credential-free preflight job. For a run waiting on environment approval, use
`gh run view RUN_ID --json jobs` to select that job's `databaseId`, then
`gh run view RUN_ID --job JOB_ID --log`; do not rely on logs from the still-
pending protected job. Never print the unhashed AA-admin reason/note during
that comparison. This runbook intentionally deploys the backend once while
native AA is absent and again after the dormant task definitions are fully
configured, and it uses distinct Alto bootstrap/deploy actions. Record that
intent, input fingerprint, and both run URLs. Never repeat an ambiguous
dispatch until its actual GitHub/AWS state is resolved; if the preflight log is
not yet available, resolve the pending run and AWS state instead of guessing.

Before Gate 1, pre-provision the GitHub Environment
`backend-admin-sepolia`. It must set `can_admins_bypass=false`, have exactly one
required-reviewer protection rule with at least one reviewer and
`prevent_self_review=true`, and enable a custom deployment branch policy that
contains exactly the branch `master`. Keep the Sepolia backend workflow AWS
credential scope unchanged, but require this single protected gate to succeed
before any later job is allowed to consume those credentials. Environment
secrets are job-scoped and are not forwarded from the gate job. This is a
Sepolia-specific gate and must not change the existing mainnet deployment
semantics. Verify it with `gh`:

```bash
gh api \
  "repos/$APP_REPOSITORY/environments/backend-admin-sepolia" \
  --jq '{name,can_admins_bypass,protection_rules,deployment_branch_policy}'
gh api \
  "repos/$APP_REPOSITORY/environments/backend-admin-sepolia/deployment-branch-policies?per_page=100" \
  --jq '.branch_policies | map({name,type})'
test "$(gh variable get EXPECTED_AWS_ACCOUNT_ID \
  --repo "$APP_REPOSITORY" --env backend-admin-sepolia)" = \
  "$EXPECTED_AWS_ACCOUNT_ID"
```

Every Sepolia `deploy-backend.yml` run in this runbook must use `--ref master`,
pass an explicit `deployment_scope`, pass its credential-free protection
preflight, and wait for this environment. Use `all` for the Gate 2/Gate 6
release and reconciler staging deployments; use `api` for a later API-only
native configuration/flag rollout. An image rollback that must also replace
the reconciler uses `all` and requires the corresponding drain analysis.
After selecting the run by exact SHA, a different authenticated reviewer must
fetch the single matching object from
`actions/runs/RUN_ID/pending_deployments` and approve it with `gh api --method
POST`, as shown in full for `aa-admin-sepolia` below. Self-approval and
administrator bypass must fail. Do not inspect, approve, or run a deployment
through the Actions web UI.

Set the non-secret environment variable `EXPECTED_AWS_ACCOUNT_ID` on
`backend-admin-sepolia` to the same independently approved 12-digit value used
above. Do not derive the expected value from the credentials being checked or
from resources they return. The workflow must pin region `ap-southeast-1` and compare
STS `Account` with that protected value before every AWS-credentialed Sepolia
job; a missing/malformed/mismatched value is a hard failure.

### Chain preflight

Set `ARBITRUM_SEPOLIA_RPC_URL` through the approved secret-injection mechanism,
then verify the chain and reviewed deployments:

```bash
test "$(cast chain-id --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL")" = "$CHAIN_ID"
test "$(cast code "$ENTRY_POINT" --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL" | cast keccak)" \
  = "$ENTRY_POINT_CODE_HASH"
test "$(cast code "$SIMPLE_ACCOUNT_FACTORY" --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL" | cast keccak)" \
  = "$SIMPLE_ACCOUNT_FACTORY_CODE_HASH"
test "$(cast code "$SIMPLE_ACCOUNT_IMPLEMENTATION" --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL" | cast keccak)" \
  = "$SIMPLE_ACCOUNT_IMPLEMENTATION_CODE_HASH"
```

Pass only if the chain ID is `421614`, all three code reads are non-empty, and
their bytecode hashes match the values recorded in the reviewed release
evidence. Record fresh safe and latest blocks as the reconciliation baseline.

First set `CANARY_ACCOUNT="$REVIEWED_LIVE_PROXY"`; later repeat the same checks
for every rollout canary. Each account must be an already deployed,
factory-derived index-zero SimpleAccount. Verify the fourth live code hash plus
its UUPS implementation and beacon slots:

```bash
export CANARY_ACCOUNT="$REVIEWED_LIVE_PROXY"
export IMPLEMENTATION_SLOT=0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc
export BEACON_SLOT=0xa3f0ad74e5423aebfd80d3ef4346578335a9a72aeaee59ff6cb3582b35133d50

test "$(cast code "$CANARY_ACCOUNT" --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL" | cast keccak)" \
  = "$APPROVED_ACCOUNT_CODE_HASH"
cast storage "$CANARY_ACCOUNT" "$IMPLEMENTATION_SLOT" \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"
cast storage "$CANARY_ACCOUNT" "$BEACON_SLOT" \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"
```

The implementation word must end in the reviewed SimpleAccount implementation
address, the beacon word must be zero, and the runtime hash must equal
`aa_paymaster_account_code_hash`. Any disagreement stops issuance.

## Gate 1: qualify the release artifacts

Run the repository tests before infrastructure or chain mutation. At minimum:

```bash
cd "$APP_ROOT/apps/backend"
cabal build all
cabal test plether-api-test

cd "$APP_ROOT/apps/frontend"
npm ci
npm run lint
npx tsc -b --pretty false
npx vitest run --project unit \
  src/perps-aa/manifest.test.ts \
  src/perps-aa/__tests__/paymasterValidity.test.ts \
  src/perps-aa/__tests__/managedPimlicoRuntime.test.ts \
  src/perps-aa/__tests__/paymasterRpcShape.test.ts \
  src/perps-aa/__tests__/execution.test.ts \
  src/api/workerCache.test.ts
npm run test:worker
npm test
npm run build

AWS_PROFILE=plether terraform -chdir="$APP_ROOT/infra/terraform" fmt -check -recursive
AWS_PROFILE=plether terraform -chdir="$APP_ROOT/infra/terraform" init
AWS_PROFILE=plether terraform -chdir="$APP_ROOT/infra/terraform" validate
```

In the `plether-core` checkout containing the imported AA package, run:

```bash
cd "$PLETHER_CORE_ROOT"
forge build --root packages/perps-aa
forge test --root packages/perps-aa
forge test --root packages/perps-aa \
  --match-contract PletherVerifyingPaymasterForkTest -vv

cd packages/perps-aa-client
npm ci
npm run typecheck
npm test
npm run build
```

Pass criteria:

- the contract, Haskell, frontend, worker-route, and Terraform suites pass at
  the exact SHAs being deployed;
- the contract/client tests cover the 157-byte `paymasterData`, 209-byte packed
  v0.8 `paymasterAndData`, type-hash/digest fixtures, and official viem-derived
  EntryPoint v0.8 UserOperation hash; the backend suite must also pass its
  157-byte envelope, complete 209-byte packed form, type-hash/digest fixture,
  low-`s` normalization, and recovery-value assertions so envelope drift cannot
  preserve one fixture by accident;
- frontend tests accept the exact legacy v1 Pimlico shape, the current public
  v2 Pimlico shape, and the exclusive native-v2 shape; reject partial/hybrid
  field sets; and prove that transport dispatch follows the validated fields,
  never the `-v2` suffix alone;
- `apps/backend/.env.example` keeps every native endpoint/address/key blank and
  all feature flags false, while its variable names and the backend README's
  Sepolia-only `/api/aa/rpc` contract match Terraform and this runbook;
- `terraform validate` succeeds with no mainnet path that can set
  `provision_self_hosted_aa=true` or native sponsorship true;
- Alto remains pinned by immutable source digest and the ECR repository is
  immutable with scan-on-push enabled.

The last Forge command is the existing opt-in, no-broadcast Arbitrum Sepolia
fork integration; it skips when `ARBITRUM_SEPOLIA_RPC_URL` is absent. Require
one passed and zero skipped tests. It pins the official EntryPoint v0.8 and
SimpleAccount stack, proves the not-yet-valid rejection, counterfactual sender
creation, `handleOps`, canonical `UserOperationEvent`, and exact paymaster
deposit/beneficiary charging. Unit digest agreement is necessary but is not a
substitute for this integration gate.

## Gate 2: provision dormant AWS resources

Ensure the following externally managed `SecureString` parameters exist before
applying Terraform; create any missing one through the approved secret-injection
process. Do not put their values in `.tfvars` or Terraform state:

| Parameter | Required value |
| --- | --- |
| `/plether/sepolia/alto-rpc-url` | Dedicated authenticated, write-capable Arbitrum Sepolia HTTPS/443 RPC used for reads, simulation, CREATE2 bootstrap, and utility-wallet refills |
| `/plether/sepolia/alto-executor-private-keys` | Exactly four comma-separated dedicated executor private keys |
| `/plether/sepolia/alto-utility-private-key` | Dedicated utility/refill private key |
| `/plether/sepolia/aa-reconciler-secondary-rpc-url` | Authenticated HTTPS/443 Arbitrum Sepolia RPC from an operator independent of the primary `PERPS_RPC_URL`; it must support `safe` block reads, historical block reads, and EntryPoint log scans |
| configured `alto_send_transaction_rpc_url_ssm_parameter_name` | Optional authenticated HTTPS/443 RPC used only for private bundle submission |

Verify metadata, not values:

```bash
inspect_secure_parameter() {
  local parameter_name="$1"
  local parameter_metadata
  parameter_metadata="$(aws --profile plether ssm describe-parameters \
    --parameter-filters \
      "Key=Name,Option=Equals,Values=$parameter_name" \
      Key=Type,Option=Equals,Values=SecureString \
    --query 'Parameters' --output json)"
  test "$(jq 'length' <<<"$parameter_metadata")" = 1
  jq '.[0] | {Name,Type,KeyId,Version}' <<<"$parameter_metadata"
}

inspect_secure_parameter /plether/sepolia/alto-rpc-url
inspect_secure_parameter /plether/sepolia/alto-executor-private-keys
inspect_secure_parameter /plether/sepolia/alto-utility-private-key
inspect_secure_parameter /plether/sepolia/aa-reconciler-secondary-rpc-url

export ALTO_SEND_TRANSACTION_RPC_PARAMETER_NAME=
# If the reviewed tfvars configures the optional send parameter, set the exact
# name above and require this query to return exactly one SecureString.
if test -n "$ALTO_SEND_TRANSACTION_RPC_PARAMETER_NAME"; then
  inspect_secure_parameter "$ALTO_SEND_TRANSACTION_RPC_PARAMETER_NAME"
fi
```

The Alto workflow decrypts only the Alto values inside its protected job and
rejects a wrong chain or non-HTTPS URL. ECS resolves the reconciler secondary
RPC from SSM through its exact execution role. Confirm operationally that each
configured host accepts TCP `443`, that `ALTO_RPC_URL` permits the controlled
bootstrap/refill transactions, and that an optional send RPC supports private
bundle submission. Never assume the send RPC can carry bootstrap or refill
traffic.

All configured external Alto SecureStrings must have a reviewed, consistent
encryption mode. When they use the AWS-managed `alias/aws/ssm` key, keep
`alto_secrets_kms_key_arn=""`. When any uses a customer-managed key, this
release supports only one exact customer key ARN for the set: set
`alto_secrets_kms_key_arn` to it, require every customer-encrypted Alto
parameter to report that same enabled symmetric key, and grant the Alto
execution role and protected workflow principal decrypt on only that key. Use
a dedicated key containing no unrelated ciphertext. The current Alto policy
is exact-key scoped but does not add the per-parameter `kms:ViaService` and
encryption-context conditions used for the secondary RPC; adding those
conditions is required before this secret boundary is reused beyond the
bounded Sepolia canary. A mixture of customer-managed key ARNs is unsupported
and must stop the rollout.

Inspect the secondary-RPC parameter's `KeyId`. If it uses the AWS-managed
`alias/aws/ssm` key, keep
`aa_reconciler_secondary_rpc_url_kms_key_arn=""`. If it uses a
customer-managed key, set that Terraform variable to the exact key ARN and
verify the key policy plus the generated API/reconciler execution-role policies
permit `kms:Decrypt` only through SSM and only with the exact parameter ARN as
encryption context. Never leave the variable empty for a customer-managed key:
ECS secret injection will fail before either container starts.
Create or update the SecureString only through the approved non-logging secret
injection path: omit a key selection for the AWS-managed SSM key, or select the
same exact customer key ARN recorded in Terraform. The backend workflow reads
metadata only and must reject a non-`SecureString`, wrong parameter ARN/name,
key-mode mismatch, disabled/non-symmetric customer key, or decrypt policy that
lacks the exact `kms:ViaService=ssm.<region>.amazonaws.com` and
`kms:EncryptionContext:PARAMETER_ARN=<exact parameter ARN>` constraints.

Through the approved secret-injection/preflight environment, independently
verify the existing primary Perps RPC and the new reconciler secondary RPC
without logging either URL:

```bash
test "$(cast chain-id --rpc-url "$PERPS_RPC_URL")" = "$CHAIN_ID"
test "$(cast chain-id --rpc-url "$AA_RECONCILER_SECONDARY_RPC_URL")" = "$CHAIN_ID"
test "$PERPS_RPC_URL" != "$AA_RECONCILER_SECONDARY_RPC_URL"
cast block safe --rpc-url "$PERPS_RPC_URL" >/dev/null
cast block safe --rpc-url "$AA_RECONCILER_SECONDARY_RPC_URL" >/dev/null
```

Both URLs must be authenticated HTTPS endpoints on port 443, controlled by
different provider operators/control planes, and able to read the deployment
block and the EntryPoint logs from that block. A different SSM parameter name
alone is not evidence of provider independence. Only the dedicated reconciler
execution role and, once native API configuration is enabled, the dedicated
API execution role may read the exact secondary parameter; neither RPC secret
belongs in a task role. The existing primary is already managed as the sensitive
`perps_rpc_url` Terraform input, so protect the established encrypted state;
the new external secondary value itself must not enter that state.

Use the environment's established Terraform state backend and complete
Sepolia variable file. Never initialize a new state for an existing stack.
The first plan must use:

```text
environment = "sepolia"
aws_region = "ap-southeast-1"
expected_aws_account_id = "932542905614"
perps_chain_id = "421614"
posthog_otlp_host = "eu.i.posthog.com"
posthog_otlp_logs_uri = "/i/v1/logs"
provision_self_hosted_aa = true
configure_native_aa_backend = false
enable_native_aa_sponsorship = false
enable_native_aa_submission = false
aa_native_global_rollout_enabled = false
alto_desired_count = 0
aa_reconciler_desired_count = 0
alto_rpc_url_ssm_parameter_name = "/plether/sepolia/alto-rpc-url"
alto_send_transaction_rpc_url_ssm_parameter_name = "" # or one exact reviewed SecureString name
alto_executor_private_keys_ssm_parameter_name = "/plether/sepolia/alto-executor-private-keys"
alto_utility_private_key_ssm_parameter_name = "/plether/sepolia/alto-utility-private-key"
alto_secrets_kms_key_arn = "" # or the one exact dedicated customer-managed key ARN
aa_reconciler_secondary_rpc_url_ssm_parameter_name = "/plether/sepolia/aa-reconciler-secondary-rpc-url"
aa_reconciler_secondary_rpc_url_kms_key_arn = "" # or the exact customer-managed key ARN
```

Keep legacy `provision_aa_proxy=true` and its sponsorship setting unchanged.
The same complete variable source must supply sensitive
`aa_proxy_origin_token` from the approved encrypted secret-injection path as
the exact generated 64-character lowercase-hex value. Never put the literal
token in a checked-in example, plan evidence, workflow input, or this runbook;
the deliberately invalid Sepolia example placeholder must never reach apply.
Supply the reviewed Alto source digest and SSM parameter names. Leave no
placeholder values in addresses or hashes; if the bootstrap guard supports a
dormant first phase, only fields explicitly allowed by that guard may be empty.
The self-hosted-AA Terraform guard must reject any `aws_region` other than
`ap-southeast-1`; a valid plan in another region is not an alternative deployment.
Keep the PostHog host/path pinned exactly as shown while either AA-admin
workflow is in use: its protected topology preflight deliberately rejects a
different Terraform-valid OTLP destination.

Create, inspect, and apply one complete saved plan:

```bash
export AA_TF_PLAN="$(mktemp /tmp/plether-aa-sepolia.XXXXXX)"

AWS_PROFILE=plether terraform -chdir="$APP_ROOT/infra/terraform" plan \
  -var-file=terraform.tfvars.sepolia \
  -out="$AA_TF_PLAN"
AWS_PROFILE=plether terraform -chdir="$APP_ROOT/infra/terraform" show "$AA_TF_PLAN"
shasum -a 256 "$AA_TF_PLAN"
AWS_PROFILE=plether terraform -chdir="$APP_ROOT/infra/terraform" apply "$AA_TF_PLAN"
export ALTO_TARGET_GROUP_ARN="$(AWS_PROFILE=plether terraform \
  -chdir="$APP_ROOT/infra/terraform" output -raw alto_target_group_arn)"
rm -f -- "$AA_TF_PLAN"
unset AA_TF_PLAN
```

Reject any unexplained replacement or destruction, public Alto listener,
second Alto task, mutable Alto image/reference, broad KMS signing permission, secret value in
a displayed plan or nonsensitive output, unapproved state-backend change,
database mutation, or unrelated service change.

Expected dormant resources include:

- KMS alias `alias/plether-sepolia-aa-paymaster-signer` backed by an enabled
  `ECC_SECG_P256K1`, `SIGN_VERIFY` key;
- immutable ECR repository `plether-alto-sepolia`;
- internal ALB and target group `plether-sepolia-alto`;
- ECS family `plether-sepolia-alto`, service `plether-alto`, desired count `0`,
  and container `plether-alto`;
- ECS family `plether-sepolia-aa-reconciler`, service and container
  `plether-aa-reconciler`, desired count `0`;
- Terraform-owned, service-less source task families
  `plether-sepolia-aa-admin-kms-attest` and
  `plether-sepolia-aa-admin-resume-issuance`, with distinct task/execution
  roles and capability-specific security groups;
- no paymaster-signing-key permission on the API task role while the native
  backend is dormant or sponsorship is off; enabling sponsorship must grant
  `DescribeKey`, `GetPublicKey`, and `Sign` only for the paymaster key, with
  signing restricted to `ECDSA_SHA_256`. A configured API execution role may
  separately decrypt the exact secondary-RPC SecureString through SSM;
- dedicated API execution role `plether-sepolia-api-execution`, with SSM access
  limited to the exact base/optional secrets actually injected; it must not
  retain the shared wildcard execution role;
- an Alto execution role limited to the exact SSM parameters and optional
  customer-managed decrypt key;
- security-group flow from the API task to the internal Alto ALB and from that
  ALB to port `3000` on the Alto task, with no public ingress.

Before the first Alto workflow operation, separately pre-provision the GitHub
Environment `alto-admin-sepolia`. It must set `can_admins_bypass=false`, have
exactly one required-reviewer protection rule with at least one reviewer and
`prevent_self_review=true`, and use a custom deployment branch policy that
contains exactly the branch `master`. Store the workflow AWS credentials only
in that protected environment. This is distinct from `aa-admin-sepolia`; do
not let either environment's approval satisfy the other capability. Verify the
environment with the CLI before every bootstrap, deploy, or rollback:

```bash
gh api \
  "repos/$APP_REPOSITORY/environments/alto-admin-sepolia" \
  --jq '{name,can_admins_bypass,protection_rules,deployment_branch_policy}'
gh api \
  "repos/$APP_REPOSITORY/environments/alto-admin-sepolia/deployment-branch-policies?per_page=100" \
  --jq '.branch_policies | map({name,type})'
test "$(gh variable get AWS_ACCOUNT_ID \
  --repo "$APP_REPOSITORY" --env alto-admin-sepolia)" = \
  "$EXPECTED_AWS_ACCOUNT_ID"
test -n "$(gh variable get AWS_DEPLOY_ROLE_ARN \
  --repo "$APP_REPOSITORY" --env alto-admin-sepolia)"
```

The first response must show administrator bypass disabled, one independent-
review rule with self-review prevented, and custom branch policies enabled;
the second must contain exactly `[{"name":"master","type":"branch"}]`.
The workflow performs a credential-free preflight and repeats the checks in
the protected job before AWS authentication. Every `deploy-alto.yml` action
must be dispatched with `--ref master`; after resolving the exact run ID and
checking `headSha`, a different authenticated reviewer must approve the single
pending `alto-admin-sepolia` deployment with `gh api`, using the same
`pending_deployments` procedure shown for `aa-admin-sepolia` later in Gate 2. Never
approve or operate an Alto run through the web UI.

Set `AWS_ACCOUNT_ID=932542905614` and the exact reviewed
`AWS_DEPLOY_ROLE_ARN` on `alto-admin-sepolia`. The workflow-level `AWS_REGION`
resolves from the repository/organization `vars.AWS_REGION` or its
`ap-southeast-1` default before environment-level variables are available, and
the current workflow does not independently compare that effective region to
the approved value. The repository-level `AWS_REGION` check in the common
preflight is therefore mandatory before dispatch; an environment-level value
is not a substitute, and any other effective region is a hard operator stop.
The workflow does reject a missing/different account or STS mismatch before
mutation.

The protected Alto workflow principal needs `sts:GetCallerIdentity`;
`ecr:GetAuthorizationToken` plus read, push, scan, and pull permissions for
exactly `plether-alto-sepolia` and `plether-otel-log-router-sepolia`; read and
mutation access to the exact `plether-alto` ECS service, cluster, task family,
and one-off tasks; read-only `DescribeServices` for the exact `plether-api`
service during the optional origin-boundary smoke (never API-service mutation);
read access to the fixed-name target group, with the operator comparing its
returned ARN to `ALTO_TARGET_GROUP_ARN` from the reviewed Terraform output;
`logs:FilterLogEvents` on only `/ecs/plether-sepolia` (the workflow then
post-filters every record by its exact task ARN); `ssm:GetParameter` only for
the external Alto secrets and, for the authenticated smoke, the managed
origin-token parameter; and `iam:PassRole` only for the Alto task and execution
roles. ECS requires tightly bounded `Resource: "*"` for
`ListTaskDefinitions`, `DescribeTaskDefinition`, `RegisterTaskDefinition`, and
the applicable create-time `TagResource`. `ecs:ListTasks` also requires
`Resource: "*"`; constrain it with `ecs:cluster` equal to the exact
`plether-sepolia` cluster ARN and retain the workflow's exact family,
started-by, ownership-tag, and task-ARN validation. ELBv2
`DescribeTargetGroups` and `DescribeTargetHealth` have no resource-level
authorization and therefore require read-only `Resource: "*"`. The workflow
resolves the fixed `plether-sepolia-alto` name in the pinned account and region;
the operator must compare that returned ARN to the Terraform output before
accepting the run as evidence. Keep `RunTask`/`StopTask`/`UpdateService`/
`DeregisterTaskDefinition` resource-scoped where supported. Require the exact
`Capability=simulation-bootstrap` and
`WorkflowOwner=alto-b-<run-id>-<run-attempt>` tags for bootstrap, and
`Capability=alto-deployment` with
`WorkflowOwner=alto-deploy-<run-id>-<run-attempt>` for a normal registration.
The bootstrap owner is also its `clientToken` and `startedBy`; retain the
workflow's family/topology validation. Bootstrap/one-off work must use
exact-owner discovery and unconditional stop/deregister cleanup, including
ambiguous API responses. For a normal deploy registration with an ambiguous
response, cleanup may deregister only an exact-owner revision that the service
did not select; the selected deployment revision remains persistent, and an
ambiguously selected revision requires explicit verification or rollback.
Never sweep unrelated family revisions. Do not grant general ECS, SSM, KMS,
ECR, Logs, or PassRole administration.

### Bootstrap Alto simulation contracts while dark

The reviewed workflow has a one-time `bootstrap-simulations` action. Keep the
Alto service at desired/running count zero. Fund only the dedicated utility
address above the approved gas cushion. Before dispatch, independently attest
the chain and the universal CREATE2 deployer that Alto will use:

```bash
test "$(cast chain-id --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL")" = "$CHAIN_ID"
export ALTO_CREATE2_DEPLOYER_CODE="$(cast code "$ALTO_CREATE2_DEPLOYER" \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL")"
test "$ALTO_CREATE2_DEPLOYER_CODE" != 0x
test "$ALTO_CREATE2_DEPLOYER_CODE" != 0x0
cast keccak "$ALTO_CREATE2_DEPLOYER_CODE"
unset ALTO_CREATE2_DEPLOYER_CODE
```

Record the non-empty deployer-code hash. The workflow repeats both chain-ID
and deployer-code checks before it permits the CREATE2 bootstrap. Then dispatch
explicitly:

```bash
export EXPECTED_ALTO_INPUT_FINGERPRINT="$(alto_dispatch_fingerprint \
  bootstrap-simulations '' sepolia '' 5000000000000000)"
gh workflow run deploy-alto.yml \
  --repo "$APP_REPOSITORY" \
  --ref master \
  -f environment=sepolia \
  -f action=bootstrap-simulations \
  -f utility_gas_cushion_wei=5000000000000000
```

Locate the run by exact `RELEASE_SHA`, inspect its `headSha`, URL, and run-name
with `gh run view`, and require its credential-free preflight log to
contain exactly
`Dispatch input fingerprint: sha256:$EXPECTED_ALTO_INPUT_FINGERPRINT`. Then
watch it to terminal success with
`gh run watch --exit-status`. Capture the run URL and fingerprint in the evidence
record; inspect failures only with `gh run view --log-failed`. The action must run
exactly one temporary Fargate task with bundle mode manual, wallet refill and
monitoring off, `ALTO_DEPLOY_SIMULATIONS_CONTRACT=true`, and
`ALTO_FLUSH_STUCK_TRANSACTIONS_DURING_STARTUP=false`; cleanup must stop the task
and deregister its temporary task definition even on failure.

The workflow must poll only the exact Sepolia log group for the structured
record whose `msg` or `message` is `Contracts used for simulation`, then accept
only a record whose `ecs_task_arn` equals the task it started. It extracts both
addresses from that task-bound record, checks non-empty code
itself, and emits exactly one non-secret compact stdout record prefixed
`ALTO_BOOTSTRAP_RESULT=`. Retrieve it only through `gh`, require cardinality and
shape, and extract the two addresses:

```bash
export ALTO_BOOTSTRAP_RUN_ID=REPLACE_WITH_EXACT_RUN_ID
export ALTO_BOOTSTRAP_RESULT_LINES="$(gh run view "$ALTO_BOOTSTRAP_RUN_ID" \
  --repo "$APP_REPOSITORY" --log | \
  rg -o 'ALTO_BOOTSTRAP_RESULT=\{[^[:cntrl:]]*\}$')"
test "$(printf '%s\n' "$ALTO_BOOTSTRAP_RESULT_LINES" | \
  awk 'NF { count++ } END { print count + 0 }')" = 1
export ALTO_BOOTSTRAP_RESULT_JSON="${ALTO_BOOTSTRAP_RESULT_LINES#ALTO_BOOTSTRAP_RESULT=}"
jq -e --arg account "$EXPECTED_AWS_ACCOUNT_ID" '
  (keys | sort) == (["entrypointSimulationContractV8",
                     "pimlicoSimulationContract","schema","taskArn"] | sort)
  and .schema == 1
  and (.taskArn | test("^arn:aws:ecs:ap-southeast-1:" + $account
                       + ":task/plether-sepolia/[0-9a-f-]+$"))
  and (.entrypointSimulationContractV8 | test("^0x[0-9A-Fa-f]{40}$"))
  and ((.entrypointSimulationContractV8 | ascii_downcase)
       != "0x0000000000000000000000000000000000000000")
  and (.pimlicoSimulationContract | test("^0x[0-9A-Fa-f]{40}$"))
  and ((.pimlicoSimulationContract | ascii_downcase)
       != "0x0000000000000000000000000000000000000000")
' <<<"$ALTO_BOOTSTRAP_RESULT_JSON"
export ALTO_ENTRYPOINT_SIMULATION_CONTRACT_V8="$(jq -r \
  .entrypointSimulationContractV8 <<<"$ALTO_BOOTSTRAP_RESULT_JSON")"
export ALTO_PIMLICO_SIMULATION_CONTRACT="$(jq -r \
  .pimlicoSimulationContract <<<"$ALTO_BOOTSTRAP_RESULT_JSON")"
```

Copy the parsed record verbatim into the evidence record and the two addresses into
`alto_entrypoint_simulation_contract_v8` and
`alto_pimlico_simulation_contract`. Independently wait for a safe block, read
non-empty code at both addresses, hash the runtime code, and record those
hashes. A retry must return the same deterministic addresses or stop for
review. Do not use a normal long-running Alto task to deploy them.

### Deploy the release backend image while AA remains dark

The protected AA-admin workflow deliberately takes its application image from
the currently deployed Sepolia API task. It never trusts an unselected `latest`
tag. Therefore deploy this reviewed release before any admin operation, while
the native configuration is still absent and both new services remain at zero:

```text
configure_native_aa_backend = false
enable_native_aa_sponsorship = false
enable_native_aa_submission = false
aa_native_global_rollout_enabled = false
alto_desired_count = 0
aa_reconciler_desired_count = 0
```

This release adds tables during API startup, so prove RDS point-in-time
recovery before the first dark deployment rather than waiting for Gate 6:

```bash
aws --profile plether rds describe-db-instances \
  --db-instance-identifier plether-sepolia \
  --query 'DBInstances[0].{Status:DBInstanceStatus,EngineVersion:EngineVersion,RetentionDays:BackupRetentionPeriod,LatestRestorableTime:LatestRestorableTime,DeletionProtection:DeletionProtection}'
```

Require `available`, the Terraform-pinned PostgreSQL major version `16`,
nonzero retention, a recent restorable time, and deletion protection. The
version check detects stack drift; the backend also preserves PostgreSQL 14
compatibility by probing for `pg_index.indnullsnotdistinct` and selecting a
fallback fingerprint query when that column is absent. Record the result
without exposing `DATABASE_URL`.

After repeating the GitHub preflight and duplicate-run check, dispatch the
ordinary backend workflow—not image-only bootstrap mode—from `master`:

```bash
gh workflow run deploy-backend.yml \
  --repo "$APP_REPOSITORY" \
  --ref master \
  -f environment=sepolia \
  -f bootstrap=false \
  -f deployment_scope=all
```

Resolve the run by exact `RELEASE_SHA`, verify `headSha`, and watch it to
success with `gh run watch --exit-status`. Require `/api/health` and legacy AA
smoke checks to pass. Inspect the deployed `plether-api` task definition and
record its immutable application-image digest; that image must be this release
and contain `/usr/local/bin/plether-aa-admin`. Native variables must still be
absent from the API definition, its task role must have no paymaster-signing-key
permission, and neither
Alto nor the reconciler may start. This is an intentional dark release-image
deployment, not authorization to configure or issue native sponsorships.

### Derive and record the KMS Ethereum address

Fetch only the public key and derive the Ethereum address from the uncompressed
65-byte SEC1 point. The SPKI document must end in `04 || X || Y`:

```bash
export PAYMASTER_KMS_KEY=alias/plether-sepolia-aa-paymaster-signer
export KMS_PUBLIC_DER="$(mktemp /tmp/plether-aa-kms.XXXXXX)"

aws --profile plether kms get-public-key \
  --key-id "$PAYMASTER_KMS_KEY" \
  --query PublicKey \
  --output text | openssl base64 -d -A > "$KMS_PUBLIC_DER"

export KMS_PUBLIC_POINT="$(tail -c 65 "$KMS_PUBLIC_DER" | xxd -p -c 65)"
test "${#KMS_PUBLIC_POINT}" -eq 130
test "${KMS_PUBLIC_POINT:0:2}" = 04
export KMS_PUBLIC_HASH="$(cast keccak "0x${KMS_PUBLIC_POINT#04}")"
export SPONSOR_SIGNER="$(cast to-check-sum-address "0x${KMS_PUBLIC_HASH: -40}")"
printf '%s\n' "$SPONSOR_SIGNER"
rm -f -- "$KMS_PUBLIC_DER"
```

Independently derive the same address with a second reviewed implementation.
Record the KMS key ARN, key spec, enabled state, public-key fingerprint, and
checksummed signer address. Do not record the DER payload. Configure
`aa_paymaster_signer_address` only after the two derivations agree.

Apply that expected signer to the dormant Terraform topology before running
the protected command. Use a complete saved plan and require that the only
intended AA changes are the signer value in the rollout-guard input and a new
source revision of the signer-attestation family; all native flags remain false
and both desired counts remain zero:

```bash
export AA_SIGNER_TF_PLAN="$(mktemp /tmp/plether-aa-signer.XXXXXX)"
AWS_PROFILE=plether terraform -chdir="$APP_ROOT/infra/terraform" plan \
  -var-file=terraform.tfvars.sepolia \
  -out="$AA_SIGNER_TF_PLAN"
AWS_PROFILE=plether terraform -chdir="$APP_ROOT/infra/terraform" show \
  "$AA_SIGNER_TF_PLAN"
shasum -a 256 "$AA_SIGNER_TF_PLAN"
AWS_PROFILE=plether terraform -chdir="$APP_ROOT/infra/terraform" apply \
  "$AA_SIGNER_TF_PLAN"
rm -f -- "$AA_SIGNER_TF_PLAN"
unset AA_SIGNER_TF_PLAN
```

Reject a plan that configures the native API, starts a service, grants KMS to
the API role, or changes unrelated infrastructure. Read back the newest
`plether-sepolia-aa-admin-kms-attest` source definition and require its
`AA_PAYMASTER_SIGNER_ADDRESS` to equal the independently derived address. The
source definition may still use the repository tag because the workflow
replaces it with, and verifies, the immutable digest of the currently deployed
release API before starting its one-off task.

### KMS signing attestation

Use the backend's production signer adapter to sign its fixed 32-byte admin
attestation digest with KMS using `MessageType=DIGEST` and
`ECDSA_SHA_256`. The release image installs the command at:

```text
/usr/local/bin/plether-aa-admin attest-kms
```

Run it only through `.github/workflows/aa-admin.yml`. Before the first use,
provision the GitHub Environment `aa-admin-sepolia` with
`can_admins_bypass=false`, exactly one required-reviewer protection rule with
`prevent_self_review=true` and at least one reviewer, and a custom deployment
branch policy containing exactly the branch `master`. Store the workflow AWS
credentials only in that protected environment, and set its non-secret
`AWS_ACCOUNT_ID=932542905614` and exact reviewed `AWS_DEPLOY_ROLE_ARN`
variables. As with Alto, the workflow-level region resolves from the
repository/organization variable before environment-level variables are
available and is not independently asserted by this workflow. The common
repository-level `AWS_REGION=ap-southeast-1` CLI readback is mandatory before
dispatch; a protected-environment region value is not proof. The workflow
must compare STS identity to the account value before its topology or mutation
work.

The workflow accepts only `action=attest-kms` or
`action=resume-issuance`. `expected_reason` and `operator_note` must both be
empty for attestation; for resume they must be printable ASCII strings of
1–512 characters and the reason must exactly match the active database pause.
Never put a secret, token, RPC URL, signed operation, or KMS response in either
free-form input.

Verify the policy with `gh`, not the GitHub web UI:

```bash
gh api \
  "repos/$APP_REPOSITORY/environments/aa-admin-sepolia" \
  --jq '{name,can_admins_bypass,protection_rules,deployment_branch_policy}'
gh api \
  "repos/$APP_REPOSITORY/environments/aa-admin-sepolia/deployment-branch-policies?per_page=100" \
  --jq '.branch_policies | map({name,type})'
test "$(gh variable get AWS_ACCOUNT_ID \
  --repo "$APP_REPOSITORY" --env aa-admin-sepolia)" = \
  "$EXPECTED_AWS_ACCOUNT_ID"
test -n "$(gh variable get AWS_DEPLOY_ROLE_ARN \
  --repo "$APP_REPOSITORY" --env aa-admin-sepolia)"
```

The protected workflow AWS principal needs read access for its STS, ECS, ECR,
KMS-key-metadata, and task-definition/topology checks. Its EC2 topology
preflight requires read-only `Resource: "*"` for exactly
`ec2:DescribeSubnets`, `ec2:DescribeRouteTables`,
`ec2:DescribeSecurityGroups`, and `ec2:DescribeSecurityGroupRules`, because
those Describe APIs do not support resource-level authorization;
`logs:FilterLogEvents` only on `/ecs/plether-sepolia`, with workflow-side exact
task-ARN filtering; `RunTask`, `StopTask`, and
`DeregisterTaskDefinition` limited to the exact cluster, tasks, and two
AA-admin family ARN patterns where those APIs support resource scoping;
`iam:GetRole`, `ListAttachedRolePolicies`, `ListRolePolicies`, and
`GetRolePolicy` for the exact capability roles; and `iam:PassRole` only for
their exact execution/task roles. `ecs:ListTaskDefinitions`,
`ecs:DescribeTaskDefinition`, and `ecs:RegisterTaskDefinition` require tightly
bounded `Resource: "*"`; `ecs:ListTasks` also requires `Resource: "*"` and
must be constrained with `ecs:cluster` equal to the exact
`plether-sepolia` cluster ARN plus the workflow's exact family, owner-tag, and
task-ARN validation. The
new-resource `TagResource` permission may also require it. Constrain tagged
creates with the reviewed `Capability` and per-run `WorkflowOwner` request tags
where supported, and resource-scope definition reads/mutations otherwise. The
workflow must still validate the exact family/topology, attach its unique
ownership tags, and unconditionally stop/deregister what it created. Each
RunTask uses `clientToken` and `startedBy` equal to
`aa-admin-<run-id>-<run-attempt>` and explicit request tags containing that
exact `WorkflowOwner` plus `Capability=fixed-digest-kms-attestation` or
`Capability=resume-aa-issuance`; it permits no task or container override.
The workflow principal itself needs neither `ssm:GetParameter` nor `kms:Sign`;
the split ECS execution/task roles own the exact secret and runtime-signing
permissions. Terraform separates the capabilities: the
`plether-sepolia-aa-admin-kms-attest` task role has exact-key
`DescribeKey`/`GetPublicKey`/`Sign` (sign restricted to `ECDSA_SHA_256`) and no
database access; the `plether-sepolia-aa-admin-resume-issuance` task has the
exact database/log secret and RDS network path but no paymaster-key permission.
Both source definitions must retain the fixed OTLP destination
`eu.i.posthog.com:443/i/v1/logs`; the workflow rejects a different host/path.

Dispatch the KMS operation exactly from the reviewed `master` SHA:

```bash
export EXPECTED_AA_ADMIN_INPUT_FINGERPRINT="$(aa_admin_dispatch_fingerprint \
  attest-kms 'RUN ATTEST-KMS ON SEPOLIA' '' '')"
gh workflow run aa-admin.yml \
  --repo "$APP_REPOSITORY" \
  --ref master \
  -f action=attest-kms \
  -f confirmation='RUN ATTEST-KMS ON SEPOLIA'
```

Resolve its run ID by exact `RELEASE_SHA`, inspect the pending run, then have a
different authenticated required reviewer approve the single protected
environment deployment with the CLI:

```bash
gh run list \
  --repo "$APP_REPOSITORY" \
  --workflow aa-admin.yml \
  --event workflow_dispatch \
  --commit "$RELEASE_SHA" \
  --limit 10
export ADMIN_RUN_ID=REPLACE_WITH_EXACT_RUN_ID
gh run view "$ADMIN_RUN_ID" \
  --repo "$APP_REPOSITORY" \
  --json displayTitle,event,headBranch,headSha,status,conclusion,url
export ADMIN_PREFLIGHT_JOB_ID="$(gh run view "$ADMIN_RUN_ID" \
  --repo "$APP_REPOSITORY" --json jobs \
  --jq '.jobs[] | select(.name == "Require pre-provisioned protected environment") | .databaseId')"
test -n "$ADMIN_PREFLIGHT_JOB_ID"
gh run view "$ADMIN_RUN_ID" --repo "$APP_REPOSITORY" \
  --job "$ADMIN_PREFLIGHT_JOB_ID" --log | \
  rg -F "Dispatch input fingerprint: sha256:$EXPECTED_AA_ADMIN_INPUT_FINGERPRINT"
test "$(gh run view "$ADMIN_RUN_ID" \
  --repo "$APP_REPOSITORY" --json headSha --jq .headSha)" = "$RELEASE_SHA"
export ADMIN_ENVIRONMENT_ID="$(gh api \
  "repos/$APP_REPOSITORY/actions/runs/$ADMIN_RUN_ID/pending_deployments" \
  --jq 'map(select(.environment.name == "aa-admin-sepolia")) |
        if length == 1 then .[0].environment.id else error("expected one pending environment") end')"
gh api --method POST \
  "repos/$APP_REPOSITORY/actions/runs/$ADMIN_RUN_ID/pending_deployments" \
  -F "environment_ids[]=$ADMIN_ENVIRONMENT_ID" \
  -f state=approved \
  -f comment='Approved reviewed Sepolia KMS fixed-digest attestation'
gh run watch "$ADMIN_RUN_ID" \
  --repo "$APP_REPOSITORY" \
  --exit-status
```

The dispatcher and approver must not be the same person/account. The workflow
must bind the currently deployed release API and log-router image digests,
register one owned temporary definition, run exactly one Fargate task without
a command override, verify exit zero and the task-scoped
`aa_admin_kms_attested` INFO record, and clean up every owned task/revision even
on failure. Require signer-address equality, signature length `65`, recovery
parity `0` or `1`, and no logged DER or compact signature. Retain the run URL
and evidence, and verify no AA-admin task remains PENDING/RUNNING and only the
Terraform source revision remains ACTIVE:

```bash
for AA_ADMIN_FAMILY in \
  plether-sepolia-aa-admin-kms-attest \
  plether-sepolia-aa-admin-resume-issuance; do
  for AA_ADMIN_STATUS in PENDING RUNNING; do
    test "$(aws --profile plether ecs list-tasks \
      --cluster "$ECS_CLUSTER" \
      --family "$AA_ADMIN_FAMILY" \
      --desired-status "$AA_ADMIN_STATUS" \
      --query 'length(taskArns)' --output text)" = 0
  done
  test "$(aws --profile plether ecs list-task-definitions \
    --family-prefix "$AA_ADMIN_FAMILY" \
    --status ACTIVE \
    --query 'length(taskDefinitionArns)' --output text)" = 1
done
unset AA_ADMIN_FAMILY AA_ADMIN_STATUS ADMIN_ENVIRONMENT_ID ADMIN_RUN_ID \
  ADMIN_PREFLIGHT_JOB_ID
```

Separately retain the Gate 1 tests proving DER parsing, positive secp256k1
scalars, low-`s` normalization, both parity inputs producing an exact 65-byte
signature with final `v` equal to `27`/`28`, invalid parity/length rejection,
sponsorship digest parity with `getSponsorshipHash`, and the fixed EIP-712
fixture. The protected runtime attestation must independently prove the same
assembly against live KMS. Do not grant
KMS to the dark API role, use ECS Exec, or substitute an ad hoc signature
script. The on-chain signer comparison becomes mandatory immediately after
Gate 3 deploys the paymaster.

## Gate 3: deploy and attest `PletherVerifyingPaymaster`

Deploy from the reviewed `plether-core` SHA, not from copied bytecode or an
uncommitted worktree. The package's exact commands are:

```bash
cd "$PLETHER_CORE_ROOT"
forge build --root packages/perps-aa
forge test --root packages/perps-aa
```

Inject `DEPLOYER_PRIVATE_KEY` from the approved signer process without writing
it to disk or shell history. Set the non-secret deployment values:

```bash
export PAYMASTER_OWNER="$SEPOLIA_SAFE_ADDRESS"
test -n "$SPONSOR_SIGNER"
export MAX_SPONSORED_COST_WEI=10000000000000000
export SIMPLE_ACCOUNT_PROXY_RUNTIME_CODE_HASH="$APPROVED_ACCOUNT_CODE_HASH"
export INITIAL_PAYMASTER_DEPOSIT_WEI=0
export INITIAL_PAYMASTER_STAKE_WEI=0
export PAYMASTER_UNSTAKE_DELAY_SEC=86400

forge script \
  packages/perps-aa/script/DeployPletherVerifyingPaymaster.s.sol:DeployPletherVerifyingPaymaster \
  --root packages/perps-aa \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"

forge script \
  packages/perps-aa/script/DeployPletherVerifyingPaymaster.s.sol:DeployPletherVerifyingPaymaster \
  --root packages/perps-aa \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL" \
  --broadcast
unset DEPLOYER_PRIVATE_KEY
```

The first command is the mandatory no-broadcast simulation. Compare its
address, constructor arguments, gas, and state changes with the reviewed
record before authorizing the second command. The script rejects an unstake
delay above `uint32`. Use zero initial funding when the Safe differs from the
deployer. Capture the transaction hash and deployed address, wait for the
environment's safe block, and verify source code through the repository's
standard explorer process.
Fill the evidence template at
`deployments/arbitrum-sepolia-perps-aa.template.json` in `plether-core`; do not
commit secrets or signed operation payloads to it. Record the attested address
under `governance.sponsorSigner`. The script itself rejects a chain other than
`421614`, pins EntryPoint/factory/implementation addresses and runtime hashes,
the proxy runtime hash, and policy ID, and deploys the paymaster paused.

Read back every immutable/security value:

```bash
export PAYMASTER=0x_REPLACE_WITH_DEPLOYED_ADDRESS

cast call "$PAYMASTER" 'entryPoint()(address)' \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"
cast call "$PAYMASTER" 'owner()(address)' \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"
cast call "$PAYMASTER" 'pendingOwner()(address)' \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"
cast call "$PAYMASTER" 'sponsorSigner()(address)' \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"
cast call "$PAYMASTER" 'policyId()(bytes32)' \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"
cast call "$PAYMASTER" 'approvedAccountCodeHash()(bytes32)' \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"
cast call "$PAYMASTER" 'accountFactory()(address)' \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"
cast call "$PAYMASTER" 'accountFactoryCodeHash()(bytes32)' \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"
cast call "$PAYMASTER" 'accountImplementation()(address)' \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"
cast call "$PAYMASTER" 'accountImplementationCodeHash()(bytes32)' \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"
cast call "$PAYMASTER" 'maxSponsoredCost()(uint256)' \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"
cast call "$PAYMASTER" 'MAX_VALIDITY_WINDOW()(uint48)' \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"
cast call "$PAYMASTER" 'paused()(bool)' \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"
cast call "$PAYMASTER" 'getDeposit()(uint256)' \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"
export PAYMASTER_CODE_HASH="$(cast code "$PAYMASTER" \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL" | cast keccak)"
printf '%s\n' "$PAYMASTER_CODE_HASH"
```

Capture the last value as the deployed instance's
`aa_paymaster_code_hash`. Unlike the four fixed account-stack hashes, this hash
depends on the paymaster constructor immutables; compute it from the live
deployed address after a safe block and do not substitute a generic artifact
runtime hash. Independently repeat the read before feeding it to Terraform.

Pass only if EntryPoint, Safe owner, zero pending owner, the sole configured
signer equals the KMS-derived address, policy ID, approved proxy hash,
factory/implementation addresses and runtime hashes, maximum cost, the
600-second maximum validity window, and deployed bytecode all match
the reviewed record. The contract must start paused. Exercise `unpause()` and
then `pause()` through the Safe while the deposit is zero and native issuance
is off; verify state after each transaction and leave it paused. Contract tests
must prove that a nonzero `paymasterPostOpGasLimit` is rejected; this profile
uses empty context and no post-op accounting.

### Confirm the pre-attested signer on chain

The protected fixed-digest KMS attestation in Gate 2 must already have passed
against the currently deployed release image. After deployment, independently
compare the paymaster's sole signer with that attested and Terraform-configured
address:

```bash
test "$(cast call "$PAYMASTER" 'sponsorSigner()(address)' \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL" | tr '[:upper:]' '[:lower:]')" = \
  "$(printf '%s' "$SPONSOR_SIGNER" | tr '[:upper:]' '[:lower:]')"
```

Any mismatch blocks funding and requires a fresh signer-derivation and
protected attestation; do not redeploy around it. Whenever native sponsorship
is later enabled, the API must also perform its KMS-public-key/on-chain signer
attestation at startup and refuse new issuance if it fails.

## Gate 4: deposit, stake, and fund Alto executors

Size a deliberately small Sepolia deposit from the configured hourly/daily
budgets and low-balance alert threshold. Deposit pays UserOperation gas; stake
is separate and supports reputation. Never confuse the two balances.

For Safe execution, prepare and review these calls:

```bash
cast calldata 'deposit()'
cast calldata 'addStake(uint32)' "$PAYMASTER_UNSTAKE_DELAY_SEC"
```

- `deposit()`: target the paymaster, attach only the approved deposit value.
- `addStake(uint32)`: target the paymaster, attach only the approved stake
  value, and use the recorded delay.

After both Safe transactions reach a safe block, verify via both paymaster and
EntryPoint:

```bash
cast call "$PAYMASTER" 'getDeposit()(uint256)' \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"
cast call "$ENTRY_POINT" \
  'getDepositInfo(address)((uint256,bool,uint112,uint32,uint48))' "$PAYMASTER" \
  --rpc-url "$ARBITRUM_SEPOLIA_RPC_URL"
```

Fund each of the four executor addresses above the configured
`alto_min_executor_balance_wei` floor plus a measured outage buffer, and fund
the utility wallet enough to refill them. Record addresses and balances only.
Prove no executor address is reused elsewhere. During the staffed Alto canary,
exercise one bounded utility-to-executor refill and record its transaction hash
to prove that the write-capable `ALTO_RPC_URL` path works through HTTPS/443;
then restore the approved balances. Do not use the optional private-bundle send
RPC for this test.

Pass only if the deposit, stake, delay, executor count, and balances equal the
approved values. At service start the workflow checks all five wallet
balances; at runtime Alto refills executors and emits the exact wallet-fault
signals alarmed by Terraform. The reconciler reads EntryPoint deposit/stake,
durably pauses issuance below `aa_paymaster_min_deposit_wei`, and drives the
low-deposit and unstaked alarms. There is no independent `/metrics` scraper,
so verify all of these log-based signals and their notification path before
enabling issuance, and retain the direct `cast` readbacks for diagnosis.

## Gate 5: activate complete native configuration with issuance off

Calculate and independently verify the SimpleAccount proxy runtime hash. Add
the following complete Sepolia values to the environment Terraform variables:

```text
provision_self_hosted_aa = true
aws_region = "ap-southeast-1"
expected_aws_account_id = "932542905614"
configure_native_aa_backend = false
enable_native_aa_sponsorship = false
enable_native_aa_submission = false
alto_desired_count = 0
aa_reconciler_desired_count = 0
aa_reconciler_start_block = "<paymaster-deployment-block>"
aa_reconciler_start_block_hash = "0x<canonical-lowercase-deployment-block-hash>"
aa_reconciler_secondary_rpc_url_ssm_parameter_name = "/plether/sepolia/aa-reconciler-secondary-rpc-url"
aa_reconciler_secondary_rpc_url_kms_key_arn = "" # or the exact customer-managed key ARN
aa_reconciler_max_safe_lag_seconds = "600"
alto_rpc_url_ssm_parameter_name = "/plether/sepolia/alto-rpc-url"
alto_send_transaction_rpc_url_ssm_parameter_name = "" # or one exact reviewed SecureString name
alto_executor_private_keys_ssm_parameter_name = "/plether/sepolia/alto-executor-private-keys"
alto_utility_private_key_ssm_parameter_name = "/plether/sepolia/alto-utility-private-key"
alto_secrets_kms_key_arn = "" # or the one exact dedicated customer-managed key ARN
alto_entrypoint_address = "0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108"
alto_entrypoint_simulation_contract_v8 = "0x..."
alto_pimlico_simulation_contract = "0x..."
aa_paymaster_address = "0x..."
aa_paymaster_code_hash = "0x<deployed-runtime-hash>"
aa_paymaster_policy_id = "0x8dd77324b94da492342191f762a32cdf99e828a7f24d77c8ed5ace90cf4f5ae3"
aa_paymaster_signer_address = "0x..."
aa_paymaster_account_code_hash = "0x41ee894da413cc99e8dec0a1784470eceb736845ad1591e06ff0ecdf0aca26c9"
aa_native_canary_owners = "0x<owner-eoa>"
aa_native_global_rollout_enabled = false
operations_alarm_sns_topic_arn = "arn:aws:sns:ap-southeast-1:932542905614:<approved-operations-topic>"
posthog_otlp_host = "eu.i.posthog.com"
posthog_otlp_logs_uri = "/i/v1/logs"
```

Set `aa_reconciler_start_block_hash` to the exact lowercase hash read from both
providers for the deployment block, and `aa_paymaster_code_hash` to the exact
lowercase deployed runtime hash. Both must be canonical nonzero `0x`-prefixed
32-byte values; Terraform and the executable reject placeholders or drift.
Set `operations_alarm_sns_topic_arn` to the existing approved staffed-on-call
topic, not an example ARN. This dark plan attaches it to the unconditional AA
alarms. The desired-count-conditional Alto and reconciler task/heartbeat alarms
are created during their respective scale-up applies, so inspect their exact
actions immediately after each service starts and keep issuance off until both
checks pass. That shared variable also updates action lists on existing non-AA
service/RDS alarms. If it was previously empty or different,
treat those changes as an explicit, separately reviewed part of the plan (or
configure the approved topic in a prior scoped change); do not misclassify them
as unexplained drift.

Review all admission, gas, and wei-denominated controls explicitly:

```text
alto_min_executor_balance_wei = "5000000000000000"
alto_max_gas_per_user_operation = "5000000"
alto_max_gas_per_bundle = "20000000"
aa_paymaster_validity_seconds = "300"
aa_paymaster_verification_gas_limit = "100000"
aa_paymaster_post_op_gas_limit = "0"
aa_paymaster_min_deposit_wei = "150000000000000000"
aa_paymaster_max_cost_wei = "10000000000000000"
aa_paymaster_account_outstanding_wei = "20000000000000000"
aa_paymaster_client_outstanding_wei = "20000000000000000"
aa_paymaster_global_outstanding_wei = "100000000000000000"
aa_paymaster_account_hourly_wei = "30000000000000000"
aa_paymaster_global_hourly_wei = "100000000000000000"
aa_paymaster_global_daily_wei = "250000000000000000"
aa_paymaster_final_rate_limit_per_minute = "6"
aa_max_request_bytes = "262144"
aa_ip_rate_limit_per_minute = "120"
aa_account_rate_limit_per_minute = "30"
aa_sponsored_gas_alert_wei_per_hour = "10000000000000000"
```

Treat the admission and spend values as initial ceilings, not targets; the
sponsored-gas value is the reviewed hourly alert threshold, not an allowance.
Reduce the ceilings if the intentionally small paymaster deposit cannot cover
the worst permitted outstanding liability.
The invariant is per-operation maximum <= both account and client outstanding
<= global outstanding <= minimum deposit, and account hourly <= global hourly
<= global daily.

Create and apply another complete saved Terraform plan. Reject a plan unless
it keeps the native backend unconfigured, issuance and submission off, and
both services stopped while updating only the expected AA configuration and
explainable task-definition revisions. Terraform intentionally rejects
`configure_native_aa_backend=true` until both Alto and the reconciler have
desired count `1`.

## Gate 6: re-deploy the backend and stage durable state

The native tables are additive and must exist before any final sponsorship:

- `aa_sponsorship_authorizations`;
- `aa_sponsorship_ledger`;
- `aa_user_operation_events`;
- `aa_reconciler_cursor`;
- `aa_reconciler_health`;
- `aa_recovery_operations`;
- `aa_rate_windows`;
- `aa_sponsorship_control`;
- `aa_sponsorship_control_events`.

The release API image was already deployed dark in Gate 2 so protected admin
work could use the correct binary. After Gate 5 registers the complete stopped
reconciler/admin definitions, intentionally run the same backend release again
with issuance off. This second deployment is not an accidental duplicate: it
must point the stopped reconciler service at the newest fully configured task
revision while leaving the API native environment absent.

Before dispatch, prove RDS point-in-time recovery is current:

```bash
aws --profile plether rds describe-db-instances \
  --db-instance-identifier plether-sepolia \
  --query 'DBInstances[0].{Status:DBInstanceStatus,EngineVersion:EngineVersion,RetentionDays:BackupRetentionPeriod,LatestRestorableTime:LatestRestorableTime,DeletionProtection:DeletionProtection}'
```

Require `available`, the Terraform-pinned PostgreSQL major version `16`,
nonzero retention, a recent restorable time, and deletion protection. This is
a stack-drift check; the backend has a probed PostgreSQL 14 fallback for the
catalog field described above. The API runs idempotent
`ensureAaSponsorshipSchema` during startup;
`apps/backend/schema.sql` contains the same additive schema. In one PostgreSQL
transaction, startup migrates a legacy `request_key`, replaces the obsolete
full uniqueness rule with the active-only partial unique index, relaxes the
expected-hash column for pre-send intent, installs the composite invariants,
and bootstraps the fail-closed control row/audit event. It then verifies an
exact catalog fingerprint: column types/nullability; parsed CHECK
definitions; PK/UNIQUE/FK columns and semantics, including
`indnullsnotdistinct=false` on PostgreSQL 15+ or the equivalent probed
PostgreSQL 14 fallback; backing-index validity;
named index columns/uniqueness/predicate; and the fail-closed control defaults. Any mismatch
or schema error must roll back and fail startup. Retain the passing runtime
fingerprint, the selected static migration/invariant parity tests, and a
reviewed `schema.sql`/runtime-migration comparison as release evidence; the
real-PostgreSQL pre-canary test below is the exact end-to-end parity proof.

```bash
gh workflow run deploy-backend.yml \
  --repo "$APP_REPOSITORY" \
  --ref master \
  -f environment=sepolia \
  -f bootstrap=false \
  -f deployment_scope=all
```

Locate this new run, distinguish it from the recorded Gate 2 dark-image run,
confirm its exact `headSha`, and watch it to a terminal success:

```bash
gh run list \
  --repo "$APP_REPOSITORY" \
  --workflow deploy-backend.yml \
  --event workflow_dispatch \
  --commit "$RELEASE_SHA" \
  --limit 10

gh run view RUN_ID \
  --repo "$APP_REPOSITORY" \
  --json displayTitle,event,headBranch,headSha,status,conclusion,url

gh run watch RUN_ID \
  --repo "$APP_REPOSITORY" \
  --exit-status
```

Because the reconciler service ignores Terraform task-definition drift, this
backend workflow must update the stopped service to the newly registered
release revision before any scale-up. Verify desired/running are still zero
and capture the task definition; inspect that revision and require the
qualified backend image plus exact
`/usr/local/bin/plether-aa-reconciler` command:

```bash
aws --profile plether ecs describe-services \
  --cluster "$ECS_CLUSTER" \
  --services "$AA_RECONCILER_SERVICE" \
  --query 'services[0].{desired:desiredCount,running:runningCount,taskDefinition:taskDefinition}'
```

Do not combine reconciler configuration and `aa_reconciler_desired_count=1`
in one Terraform apply: that can start the older blank revision before the
workflow points the service to this release.

Verify `/api/health`, then query database metadata through the approved
read-only database path. Do not print the database URL. The runtime catalog
fingerprint is the mechanical constraint/index check; the following operator
queries are a supplemental table/control/liability readback, not a substitute
for it. Confirm no `reserved`, `signed`, or `submitted` rows predate the rollout
unless they are explicitly migrated evidence.

```sql
SELECT name, to_regclass('public.' || name) AS relation
FROM (VALUES
  ('aa_sponsorship_authorizations'), ('aa_sponsorship_ledger'),
  ('aa_user_operation_events'), ('aa_reconciler_cursor'),
  ('aa_reconciler_health'), ('aa_recovery_operations'),
  ('aa_rate_windows'), ('aa_sponsorship_control'),
  ('aa_sponsorship_control_events')
) AS expected(name)
ORDER BY name;

SELECT issuance_paused, paused_reason, updated_at
FROM aa_sponsorship_control
WHERE singleton = TRUE;

SELECT action, reason, operator_note, created_at
FROM aa_sponsorship_control_events
ORDER BY id ASC LIMIT 1;

SELECT state, COUNT(*), COALESCE(SUM(max_cost_wei), 0)
FROM aa_sponsorship_authorizations
GROUP BY state ORDER BY state;

SELECT entry_type, COUNT(*), COALESCE(SUM(amount_wei), 0)
FROM aa_sponsorship_ledger
GROUP BY entry_type ORDER BY entry_type;
```

Every `relation` must be non-null. The control row must exist; on a fresh
rollout it must be fail-closed with `issuance_paused=true` and exact
`paused_reason='uninitialized or control row recreated'`. Its first audit row
must be a `pause` event with the same reason and operator note
`automatic fail-closed control row bootstrap`. The environment feature flag
also keeps sponsorship off. If this singleton is ever deleted, schema startup
recreates it in that same paused state; recreation is an incident signal, not
an automatic authorization to resume.

The reconciler prunes `aa_rate_windows` older than 48 hours and expired
`aa_recovery_operations` after verified cycles, so these tables are no longer
permanently unbounded. Its current statements are unbatched deletes, however.
Before Gate 11 or any sustained load, replace them with a reviewed, bounded,
retention-aware cleanup path and add table-growth/deletion-latency alerts.
Never delete live recovery rows; retain the rows needed by the seven-day
gateway recovery authorization, the longest browser-journal recovery window,
and rollout evidence.

The current API also has no dedicated structured events/counters for budget
denials, final-issuance or account-rate denials, or recovery-authorization
denials. The low-volume canary must prove those controls from the exact RPC
response and read-only database state; generic access logs are not an abuse
signal. Add reason-specific, non-sensitive metrics and alarms before sustained
traffic. Do not claim those denials are monitored merely because the aggregate
native fault alarm exists.

At this gate, the deployed API task must not yet contain the native-AA
environment block. Pass only if startup creates/validates the additive tables,
the dormant reconciler task revision uses the release image, API health is
unchanged, and legacy sponsorship/recovery still works.

## Gate 7: mirror and start Alto

Use only `.github/workflows/deploy-alto.yml` to mirror, scan, register, and
deploy Alto. The workflow authenticates the immutable upstream digest, pushes
an immutable ECR reference, preserves the non-image task definition, and forces
a stop-before-start deployment. It makes a best-effort automatic rollback for
an ordinary failed job after its mutation marker is recorded; cancellation,
runner loss, a failure before that output is durable, or rollback-step failure
still requires protected manual recovery and AWS readback.

Before dispatch, verify there is no duplicate run for the release and target
digest. With Alto still at desired count zero, stage the image:

```bash
export EXPECTED_ALTO_INPUT_FINGERPRINT="$(alto_dispatch_fingerprint \
  deploy '' sepolia '' 5000000000000000)"
gh workflow run deploy-alto.yml \
  --repo "$APP_REPOSITORY" \
  --ref master \
  -f environment=sepolia \
  -f action=deploy \
  -f utility_gas_cushion_wei=5000000000000000
```

Use `gh run list`, `gh run view`, `gh run watch`, and
`gh run view --log-failed` as shown for the backend. Confirm the run's exact
`headSha` and expected input fingerprint. The stopped-service run must report
that it registered the immutable
revision without starting it. Never dispatch an unreviewed image tag or a
digest that does not match the fixed identity table.

Next apply a complete saved Terraform plan containing the expected service
scale-up and alarm changes:

```text
alto_desired_count = 1
aa_reconciler_desired_count = 0
configure_native_aa_backend = false
enable_native_aa_sponsorship = false
enable_native_aa_submission = false
aa_native_global_rollout_enabled = false
```

Both simulation addresses must already be populated. Terraform starts only the
immutable Alto task revision staged by the workflow; the release-pinned
reconciler revision remains stopped until its deployment anchor is re-attested
in Gate 8. Do not combine the Gate 5 configuration apply with this scale-up:
because both ECS services ignore task-definition drift, doing so can start an
older blank revision. After Alto stabilizes, rerun `deploy-alto.yml` with the
same explicit `action=deploy` and gas-cushion inputs so its running-service
health gates prove the exact deployment. Leave `api_hostname` empty until the
API native route is configured.

After the workflow succeeds, verify from AWS:

```bash
aws --profile plether ecs describe-services \
  --cluster "$ECS_CLUSTER" \
  --services "$ALTO_SERVICE" \
  --query 'services[0].{desired:desiredCount,running:runningCount,pending:pendingCount,taskDefinition:taskDefinition,deployments:deployments}'

export ALTO_RESOLVED_TARGET_GROUP_ARN="$(aws --profile plether elbv2 \
  describe-target-groups \
  --names plether-sepolia-alto \
  --query 'TargetGroups[0].TargetGroupArn' \
  --output text)"
test "$ALTO_RESOLVED_TARGET_GROUP_ARN" = "$ALTO_TARGET_GROUP_ARN"

aws --profile plether elbv2 describe-target-health \
  --target-group-arn "$ALTO_RESOLVED_TARGET_GROUP_ARN"
unset ALTO_RESOLVED_TARGET_GROUP_ARN
```

Pass only if desired/running are exactly `1`, pending is `0`, exactly one
target is healthy, no prior task still holds an executor key, and the running
container digest equals the qualified ECR digest. Verify its environment keeps
these critical values:

```text
ALTO_CHAIN_TYPE=arbitrum
ALTO_SAFE_MODE=false
ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION=false
ALTO_DEPLOY_SIMULATIONS_CONTRACT=false
ALTO_FLUSH_STUCK_TRANSACTIONS_DURING_STARTUP=false
ALTO_ENABLE_DEBUG_ENDPOINTS=false
ALTO_ENABLE_CORS=false
ALTO_WEBSOCKET=false
ALTO_ENABLE_HORIZONTAL_SCALING=false
ALTO_ENABLE_REDIS_RECEIPT_CACHE=false
ALTO_MAX_EXECUTORS=4
```

For this reviewed image, `SAFE_MODE=false` does not disable all validation:
the dangerous skip remains false, so simulation, account signature, runtime,
and other normal validation still execute. It does disable Alto's ERC-7562
tracing, entity-role separation, reputation, and related protections. Treat
that reduction as a known-account Sepolia-canary exception, not a generally
safe bundler posture. Do not set the global rollout switch, do not replace the
public v2 manifest's Pimlico field set with the native field set, and do not
reuse this task definition on mainnet.

The two simulation contract addresses must already be pinned. Starting a
production task with self-deployment enabled is not an acceptable bootstrap
path. They must be the outputs of the reviewed dark
`bootstrap-simulations` action above and match the independently recorded
runtime hashes.

Verify the implemented service/log alarms and their notification actions:

```bash
export OPERATIONS_ALARM_SNS_TOPIC_ARN='arn:aws:sns:ap-southeast-1:932542905614:<approved-operations-topic>'
export AA_ALARM_EVIDENCE="$(mktemp /tmp/plether-aa-alarms.XXXXXX)"
aws --profile plether cloudwatch describe-alarms \
  --alarm-names \
    plether-sepolia-aa-sponsored-gas-alert \
    plether-sepolia-alto-task-missing \
    plether-sepolia-alto-unhealthy-target \
    plether-sepolia-alto-target-5xx \
    plether-sepolia-alto-fatal \
    plether-sepolia-alto-wallet-fault \
    plether-sepolia-alto-gas-price-fault \
    plether-sepolia-alto-executor-insufficient-funds \
    plether-sepolia-alto-cpu-high \
    plether-sepolia-alto-memory-high \
    plether-sepolia-aa-reconciler-fatal \
    plether-sepolia-aa-reconciler-cpu-high \
    plether-sepolia-aa-reconciler-memory-high \
    plether-sepolia-aa-paymaster-low-deposit \
    plether-sepolia-aa-paymaster-unstaked \
    plether-sepolia-aa-reconciler-rpc-unavailable \
    plether-sepolia-aa-native-api-fault \
  --query '{Count:length(MetricAlarms),Alarms:MetricAlarms[].{Name:AlarmName,State:StateValue,Actions:AlarmActions}}' \
  --output json > "$AA_ALARM_EVIDENCE"
test "$(jq -r .Count "$AA_ALARM_EVIDENCE")" = 17
jq -e --arg topic "$OPERATIONS_ALARM_SNS_TOPIC_ARN" \
  'all(.Alarms[]; .Actions == [$topic])' "$AA_ALARM_EVIDENCE"
test "$(aws --profile plether sns list-subscriptions-by-topic \
  --topic-arn "$OPERATIONS_ALARM_SNS_TOPIC_ARN" \
  --query "length(Subscriptions[?SubscriptionArn!='PendingConfirmation'])" \
  --output text)" -ge 1
rm -f -- "$AA_ALARM_EVIDENCE"
unset AA_ALARM_EVIDENCE
```

At this point the response count must be exactly `17`. The
`aa-reconciler-task-missing` and `aa-reconciler-heartbeat-missing` alarms do not
exist while `aa_reconciler_desired_count=0`; their absence is intentional, not
evidence that `describe-alarms` validated them. Gate 8 creates and explicitly
checks those two alarms when it scales the reconciler to one.

The topic variable must equal the value applied in Gate 5. Before Gate 10, send
a clearly labelled test notification and require a staffed on-call to
acknowledge it in the change record:

```bash
aws --profile plether sns publish \
  --topic-arn "$OPERATIONS_ALARM_SNS_TOPIC_ARN" \
  --subject 'TEST Plether Sepolia AA rollout alarm path' \
  --message "TEST only: Sepolia AA rollout ${RELEASE_SHA}; acknowledge in the approved change record."
```

Sepolia Terraform does not itself require a non-empty topic, so the exact
action/subscription/delivery checks remain an operator gate.

The aggregate `plether-sepolia-aa-native-api-fault` alarm must fire within one
60-second period on any of the exact API events
`aa_native_issuance_unavailable` (WARN), `aa_native_reconciler_stale`,
`aa_native_signer_failure`, `aa_native_bundler_hash_mismatch`, or
`aa_native_sponsorship_database_failure` (ERROR), or
`aa_native_security_attestation_failure` (ERROR). Verify every metric filter
matches the JSON `container_name`, `level`, and `event` emitted by the release,
then test the alarm-to-on-call path without signing or submitting an operation.
The startup event must use the exact message `Native AA startup attestation
failed; affected methods remain fail-closed` and expose only
`failure_class=startup-attestation`, never the raw profile/KMS error, RPC URL,
KMS identifier, or secret. The security event is rate-limited to
one record per 30 seconds, uses the exact message `Independent RPC security
attestation failed closed` and `method=native-aa`, and covers initial snapshot,
revalidation, provider disagreement, runtime/code read failure, and identical
retryable identity-RPC failures. An identical nonretryable account/policy denial
must remain an ordinary client denial, not a security fault.
The reconciler fatal aggregate must likewise cover configuration/schema
failure, unknown operations, cursor discontinuity, cost overflow, chain
mismatch, provider disagreement, invalid timestamp, sustained dual-provider
failure, and unexpected process crash.

Do not count `plether-sepolia-alto-fatal` as the only crash detector: the pinned
Alto v1.2.7 code does not emit the fatal records that filter expects. Task
missing, target health/5xx, exact wallet/gas-price errors, and deployment smoke
checks carry current runtime detection; test each path.

## Gate 8: start and prove safe-chain reconciliation

Keep issuance and native submission off. Start the dedicated reconciler only
after the deployment-anchor and empty-state checks below. There must then be
exactly one active writer for the `(421614, paymaster)` cursor while the
PostgreSQL lock session is healthy.
The image installs the executable at
`/usr/local/bin/plether-aa-reconciler`; the ECS task uses that exact absolute
path as its command and takes a PostgreSQL advisory lock before scanning.
It uses two independently operated RPC providers and attests chain ID `421614`
on each every cycle. It chooses the lower of their advertised `safe` heights,
requires both providers to return the same canonical header at that numeric
height, and requires exact agreement on canonical headers and the complete
own-paymaster `UserOperationEvent` set for every scanned range. It verifies
every event block header and re-reads both the stored-cursor and target headers
after the log scan, before applying event state and compare-and-swap advancing
the cursor. It must initialize from the explicitly recorded deployment boundary,
persist the block hash, scan EntryPoint events globally rather than only known
API submissions, and trip the database issuance circuit breaker on a gap,
hash/timestamp mismatch or reorg detected at those checkpoints, provider
disagreement, unknown sponsored operation, or actual cost above the
reservation. It has no paymaster-signing KMS or Alto runtime dependency. When
the secondary RPC uses a customer-managed key, its execution role performs the
scoped secret decryption during ECS startup; the running reconciler never signs
or calls the paymaster KMS key.

These second header reads close the previously identified between-read reorg
gap at the scanner's explicit checkpoints. Each event settlement is
transactional, exact, and idempotent, so an ordinary crash or cursor-CAS loss
without a chain change leaves the cursor old and safely replays the same
canonical events; it can pause availability but does not create an accounting
gap. A different residual remains: `safe` is not an irreversible `finalized`
boundary, and a reorg after the final external header reads and event commits
can leave a stale settlement. No database-only transaction can atomically
fence a later external-chain reorg, and this release has no reversible orphan
accounting/repair tool.

That residual is accepted only for the one-at-a-time, Plether-controlled,
low-deposit/low-cap Sepolia canary with independent providers, frozen account
ownership/upgrades, functioning alerts, and operators ready to disable
issuance, pause the paymaster, and reconcile manually. Do not claim complete
reorg safety. Public/global Sepolia and every mainnet rollout remain blocked
until finalized-boundary reconciliation or reviewed reversible orphan
accounting and fenced repair are implemented.

Only `StepCaughtUp` may refresh `aa_reconciler_health`; intermediate
`StepAdvanced` batches are progress logs, not issuance authority. The health
row records the caught-up block number/hash, and reserve, signature-store, and
stored-signature delivery predicates join it to the matching
`(chain_id,paymaster)` cursor with equal block number/hash and a timestamp less
than 120 seconds old. Qualification must include a regression that starts far
behind, advances one batch, and proves reserve/sign/delivery remain denied
until full catch-up, plus a cursor/health mismatch regression. A merely recent
`last_success_at` is never sufficient.

If either provider is unavailable, the cursor does not advance. The process
warns with `aa_reconciler_rpc_unavailable`; after the continuously failing
interval reaches `AA_RECONCILER_FAILURE_PAUSE_SECONDS`, it durably pauses
issuance and emits `aa_reconciler_failure_threshold_exceeded`. The current ECS
definition deliberately omits that environment variable, so the executable's
reviewed default is 30 seconds. That timer begins only after the first failed
reconcile cycle has completed; it is not a 30-second wall-clock guarantee from
the first bad RPC. Each RPC has a 20-second total timeout and some reads are
sequential, so the first and next completed failed cycles can make the observed
pause materially later. The 120-second database freshness check and missing
heartbeat alarm remain independent backstops. Do not change the threshold by
patching a task definition.
Different valid observations are not treated as a transient outage: provider
disagreement is immediately fatal and fail-closed.

All configuration below except the desired count must already have been
applied while the service remained stopped. The start block is the paymaster
deployment block, not the current head and not an estimate. On first run the
cursor is initialized to the canonical block immediately before it so the
first continuous scan includes the deployment block:

```text
aa_reconciler_desired_count = 0
aa_reconciler_start_block = "<paymaster-deployment-block>"
aa_reconciler_start_block_hash = "0x<canonical-lowercase-deployment-block-hash>"
aa_reconciler_poll_seconds = "5"
aa_reconciler_batch_blocks = "1000"
aa_reconciler_secondary_rpc_url_ssm_parameter_name = "/plether/sepolia/aa-reconciler-secondary-rpc-url"
aa_reconciler_secondary_rpc_url_kms_key_arn = "" # or the exact customer-managed key ARN
aa_reconciler_max_safe_lag_seconds = "600"
aa_paymaster_min_deposit_wei = "150000000000000000"
```

Before allowing that first run, take the recorded paymaster deployment
transaction from Gate 3 and query it independently through both reconciler
providers:

```bash
export PAYMASTER_DEPLOYMENT_TX=0x_REPLACE_WITH_RECORDED_TRANSACTION_HASH
export PAYMASTER_DEPLOYMENT_BLOCK=REPLACE_WITH_RECORDED_DECIMAL_BLOCK
export PAYMASTER_DEPLOYMENT_BLOCK_HASH=0x_REPLACE_WITH_CANONICAL_LOWERCASE_BLOCK_HASH
cast receipt "$PAYMASTER_DEPLOYMENT_TX" --json \
  --rpc-url "$PERPS_RPC_URL" |
  jq '{transactionHash,status,blockNumber,blockHash,contractAddress}'
cast receipt "$PAYMASTER_DEPLOYMENT_TX" --json \
  --rpc-url "$AA_RECONCILER_SECONDARY_RPC_URL" |
  jq '{transactionHash,status,blockNumber,blockHash,contractAddress}'
test "$(cast block "$PAYMASTER_DEPLOYMENT_BLOCK" --json \
  --rpc-url "$PERPS_RPC_URL" | jq -r .hash)" = \
  "$PAYMASTER_DEPLOYMENT_BLOCK_HASH"
test "$PAYMASTER_DEPLOYMENT_BLOCK_HASH" = \
  "$(cast block "$PAYMASTER_DEPLOYMENT_BLOCK" --json \
  --rpc-url "$AA_RECONCILER_SECONDARY_RPC_URL" | jq -r .hash)"
test "$(cast code "$PAYMASTER" \
  --block "$((PAYMASTER_DEPLOYMENT_BLOCK - 1))" \
  --rpc-url "$PERPS_RPC_URL")" = 0x
test "$(cast code "$PAYMASTER" \
  --block "$((PAYMASTER_DEPLOYMENT_BLOCK - 1))" \
  --rpc-url "$AA_RECONCILER_SECONDARY_RPC_URL")" = 0x
test "$(cast code "$PAYMASTER" \
  --block "$PAYMASTER_DEPLOYMENT_BLOCK" \
  --rpc-url "$PERPS_RPC_URL" | cast keccak)" = "$PAYMASTER_CODE_HASH"
test "$(cast code "$PAYMASTER" \
  --block "$PAYMASTER_DEPLOYMENT_BLOCK" \
  --rpc-url "$AA_RECONCILER_SECONDARY_RPC_URL" | cast keccak)" = \
  "$PAYMASTER_CODE_HASH"
```

Both receipts must be identical for the selected fields, successful, name the
recorded paymaster as `contractAddress`, and place it at exactly
`aa_reconciler_start_block`; the canonical lowercase block hash must equal
`aa_reconciler_start_block_hash` and the deployment evidence. Both providers
must show no code immediately before that block and
the deployment-specific runtime hash at that block. If the cursor is absent,
also require this read-only query to return `(true,true)` before starting:

```sql
SELECT
  NOT EXISTS (
    SELECT 1 FROM aa_reconciler_cursor
    WHERE chain_id = 421614
      AND paymaster = lower('0x_REPLACE_WITH_DEPLOYED_PAYMASTER')
  ) AS cursor_absent,
  NOT EXISTS (SELECT 1 FROM aa_sponsorship_authorizations)
    AND NOT EXISTS (SELECT 1 FROM aa_sponsorship_ledger)
    AND NOT EXISTS (SELECT 1 FROM aa_user_operation_events)
    AS sponsorship_state_empty;
```

The executable requires the positive deployment block, its canonical nonzero
hash, and the deployment-specific paymaster runtime hash. On a missing cursor,
it dual-reads blocks `N-1` and `N`, requires no paymaster code at `N-1`, requires
the exact configured code hash at `N`, and initializes the cursor to the agreed
`N-1` header so the first scan includes every event in deployment block `N`.
The database initializer takes the budget advisory lock, atomically rechecks
that authorization/ledger/event state is empty, and validates its cursor
readback. Qualification must pass clean-bootstrap, wrong-block/hash/code,
deployment-block event, concurrent initialization, and partial-restore
regressions. The executable does not accept a deployment transaction hash, so
the dual-provider receipt comparison above remains the required operator
evidence tying the configured block/hash to the recorded successful deploy.

After the manual anchor and empty-state checks pass, apply one complete saved
Terraform plan changing `aa_reconciler_desired_count` from `0` to `1` while
keeping `alto_desired_count=1`, native configuration false, both feature flags
false, and the canary/global settings unchanged. This starts the exact stopped
release revision staged by the Gate 6 backend workflow; reject any plan that
changes its task definition or unrelated service state.

Verify the single-active ECS service before testing settlement:

```bash
aws --profile plether ecs describe-services \
  --cluster "$ECS_CLUSTER" \
  --services "$AA_RECONCILER_SERVICE" \
  --query 'services[0].{desired:desiredCount,running:runningCount,pending:pendingCount,taskDefinition:taskDefinition,deployments:deployments}'

test "$(AWS_PROFILE=plether terraform \
  -chdir="$APP_ROOT/infra/terraform" \
  output -raw aa_reconciler_max_safe_lag_seconds)" = 600

export AA_RECON_ALARM_EVIDENCE="$(mktemp /tmp/plether-aa-recon-alarms.XXXXXX)"
aws --profile plether cloudwatch describe-alarms \
  --alarm-names \
    plether-sepolia-aa-reconciler-task-missing \
    plether-sepolia-aa-reconciler-heartbeat-missing \
  --output json > "$AA_RECON_ALARM_EVIDENCE"
test "$(jq '.MetricAlarms | length' "$AA_RECON_ALARM_EVIDENCE")" = 2
jq -e --arg topic "$OPERATIONS_ALARM_SNS_TOPIC_ARN" \
  'all(.MetricAlarms[]; .AlarmActions == [$topic])' "$AA_RECON_ALARM_EVIDENCE"
rm -f -- "$AA_RECON_ALARM_EVIDENCE"
unset AA_RECON_ALARM_EVIDENCE
```

Desired/running must be exactly `1`, pending `0`, and only one deployment may
own a running task. For reconciliation, the container must receive
`DATABASE_URL`, primary `PERPS_RPC_URL`, independent
`AA_RECONCILER_SECONDARY_RPC_URL`, `PERPS_CHAIN_ID`,
`AA_PAYMASTER_ADDRESS`, `AA_PAYMASTER_CODE_HASH`,
`AA_RECONCILER_START_BLOCK`, `AA_RECONCILER_START_BLOCK_HASH`,
`AA_RECONCILER_POLL_SECONDS`, `AA_RECONCILER_BATCH_BLOCKS`, and
`AA_RECONCILER_MAX_SAFE_LAG_SECONDS=600`,
`AA_PAYMASTER_MIN_DEPOSIT_WEI`, plus `HOME=/tmp`, `TMPDIR=/tmp`, and logging
configuration. Verify both URLs resolve from SSM only at task start, differ in
value and provider operator, use HTTPS/443, report chain `421614`, support the
`safe` tag, and can scan the full required history. The reconciler must reject
the agreed advertised safe-boundary timestamp if it is more than 600 seconds
behind its wall clock or more than 60 seconds in the future, durably pausing
issuance rather than reporting a healthy heartbeat. During historical
catch-up, do not apply the 600-second age limit to each intermediate batch
target: those targets are expected to be old. Each target must instead be at
or below the already attested safe height, have a timestamp no earlier than
the canonical cursor and no more than 60 seconds in the future, agree at both
providers, and survive the post-log header re-read. The configured deposit
minimum must be below the more conservative of the two live deposit reads with
the approved outage buffer; stake is healthy only when both providers report
it staked. The shared RPC transport must reject redirects, enforce a 20-second
total deadline and a 1 MiB response-body limit, and treat timeout, oversize,
non-2xx, or malformed JSON as unavailable rather than buffering indefinitely.
Exercise those failure cases in the qualified backend tests.

Initial pass criteria over at least one full validity window plus the
operational observation window:

- the cursor advances monotonically, the advertised dual-provider safe boundary
  remains within the configured 600-second maximum lag, and historical batch
  targets obey monotonic/future rules without preventing catch-up;
- a cursor discontinuity, unknown paymaster event, or excessive actual charge
  pauses new issuance and alerts without destroying evidence;
- an unavailable provider prevents cursor advance and durably pauses issuance
  after 30 seconds measured from the first completed failed cycle; observed
  wall-clock delay, the 120-second database freshness cutoff, and the heartbeat
  alarm are recorded separately. A deliberate header or log-set disagreement
  pauses immediately and emits `aa_reconciler_provider_disagreement`;
- `aa_reconciler_safe_block_advanced` proves successful progress, and
  `aa_reconciler_heartbeat` appears at least once per minute only when caught
  up to the dual-provider safe boundary;
- a controlled second process cannot acquire the advisory lock while the first
  lock session is healthy. The current process rechecks that its session holds
  an advisory lock at the start of every cycle, but the query does not identify
  the exact lock key and there is no mid-cycle or just-before-commit fencing
  epoch. Before relying on it for unattended or higher-volume operation, check
  the exact key and fence event/cursor writes against leadership loss; a long
  RPC cycle can otherwise outlive a connection loss detected only next cycle.

The current mismatch logs retain a generic reason, not both raw provider
responses. On an alert, preserve what was emitted and immediately re-query both
providers into bounded, redacted incident evidence; a transient mismatch may
otherwise be irreproducible. Add non-secret structured provider/reason fields
or another bounded forensic capture path before treating the alert stream as a
complete audit record. Never log authenticated RPC URLs or full signed
operations.

Treat any of these structured events as fatal and preserve the associated
cursor, block, operation, and database evidence:

```text
aa_reconciler_configuration_invalid
aa_reconciler_schema_invalid
aa_reconciler_unknown_operation
aa_reconciler_cursor_discontinuity
aa_reconciler_cost_exceeds_reservation
aa_reconciler_chain_mismatch
aa_reconciler_provider_disagreement
aa_reconciler_timestamp_invalid
aa_reconciler_failure_threshold_exceeded
aa_reconciler_crashed
```

Record the cursor and table aggregates without exporting operation payloads:

```sql
SELECT chain_id, paymaster, safe_block, safe_block_hash, updated_at
FROM aa_reconciler_cursor;

SELECT chain_id, paymaster, safe_block, safe_block_hash, last_success_at
FROM aa_reconciler_health;

SELECT state, COUNT(*), COALESCE(SUM(max_cost_wei), 0)
FROM aa_sponsorship_authorizations
GROUP BY state ORDER BY state;

SELECT entry_type, COUNT(*), COALESCE(SUM(amount_wei), 0)
FROM aa_sponsorship_ledger
GROUP BY entry_type ORDER BY entry_type;

WITH ledger_by_digest AS (
  SELECT digest,
    COUNT(*) FILTER (WHERE entry_type = 'reserve') AS reserve_count,
    COUNT(*) FILTER (WHERE entry_type = 'release') AS release_count,
    COUNT(*) FILTER (WHERE entry_type = 'actual_charge') AS charge_count,
    COALESCE(SUM(amount_wei) FILTER (WHERE entry_type = 'reserve'), 0) AS reserve_wei,
    COALESCE(SUM(amount_wei) FILTER (WHERE entry_type = 'release'), 0) AS release_wei,
    COALESCE(SUM(amount_wei) FILTER (WHERE entry_type = 'actual_charge'), 0) AS charge_wei
  FROM aa_sponsorship_ledger GROUP BY digest
), events_by_digest AS (
  SELECT digest, COUNT(*) AS event_count,
    COALESCE(SUM(actual_gas_cost_wei), 0) AS event_cost_wei
  FROM aa_user_operation_events GROUP BY digest
)
SELECT a.digest, a.state, a.max_cost_wei
FROM aa_sponsorship_authorizations a
LEFT JOIN ledger_by_digest l USING (digest)
LEFT JOIN events_by_digest e USING (digest)
WHERE COALESCE(l.reserve_count, 0) <> 1
   OR COALESCE(l.reserve_wei, 0) <> a.max_cost_wei
   OR (a.state IN ('reserved','signed','submitted') AND (
        COALESCE(l.release_count, 0) <> 0 OR COALESCE(l.charge_count, 0) <> 0
        OR COALESCE(e.event_count, 0) <> 0))
   OR (a.state = 'settled' AND (
        COALESCE(l.release_count, 0) <> 1 OR COALESCE(l.charge_count, 0) <> 1
        OR COALESCE(e.event_count, 0) <> 1
        OR COALESCE(l.release_wei, 0) + COALESCE(l.charge_wei, 0) <> a.max_cost_wei
        OR COALESCE(l.charge_wei, 0) <> COALESCE(e.event_cost_wei, 0)))
   OR (a.state IN ('expired','cancelled') AND (
        COALESCE(l.release_count, 0) <> 1 OR COALESCE(l.charge_count, 0) <> 0
        OR COALESCE(e.event_count, 0) <> 0
        OR COALESCE(l.release_wei, 0) <> a.max_cost_wei));

SELECT action, reason, operator_note, created_at
FROM aa_sponsorship_control_events
ORDER BY id DESC LIMIT 20;
```

The per-digest invariant query must return zero rows; aggregate totals alone
cannot prove that charges and releases belong to the correct authorization.
Outstanding liability must equal reservations for `reserved`, `signed`, and
`submitted` authorizations. A settled authorization has exactly one canonical
event and one actual charge plus one release totaling its reservation; an
expired/cancelled authorization has one full release and no charge/event.

The reconciler has no HTTP admin/metrics endpoint. The release image does have
an explicit database command:

```text
/usr/local/bin/plether-aa-admin resume-issuance \
  --expected-reason '<exact current paused_reason>' \
  --operator-note '<incident/change reference and evidence summary>'
```

It requires `DATABASE_URL`, validates the schema, takes the sponsorship budget
advisory lock, refuses a stale or mismatched reason, clears the breaker with a
compare-and-set update, and appends an `aa_sponsorship_control_events` audit
row. The command does not itself prove chain cursor continuity or recompute the
ledger invariant. Before invoking it, independently verify the current/safe
block hashes, continuous scan, heartbeat age, all affected operations, and
ledger/deposit reconciliation; afterwards read back both the control and event
rows. A trip remains fail-closed and must never be cleared with an ad hoc SQL
`UPDATE`.

After the full Gate 8 observation window and invariant checks pass, explicitly
clear the initial bootstrap pause while the environment sponsorship flag is
still false. Read the current row through the approved database path and
require its exact reason to be `uninitialized or control row recreated`. Set a
non-secret, printable audit note that identifies the reviewed evidence, then
dispatch the capability-separated protected workflow:

```bash
export EXPECTED_PAUSE_REASON='uninitialized or control row recreated'
export ISSUANCE_OPERATOR_NOTE='CHANGE_RECORD: dual-provider continuity, freshness, and per-digest ledger/deposit checks passed'
export EXPECTED_AA_ADMIN_INPUT_FINGERPRINT="$(aa_admin_dispatch_fingerprint \
  resume-issuance 'RUN RESUME-ISSUANCE ON SEPOLIA' \
  "$EXPECTED_PAUSE_REASON" "$ISSUANCE_OPERATOR_NOTE")"
gh workflow run aa-admin.yml \
  --repo "$APP_REPOSITORY" \
  --ref master \
  -f action=resume-issuance \
  -f expected_reason="$EXPECTED_PAUSE_REASON" \
  -f operator_note="$ISSUANCE_OPERATOR_NOTE" \
  -f confirmation='RUN RESUME-ISSUANCE ON SEPOLIA'
```

Resolve the new `ADMIN_RUN_ID` by exact `RELEASE_SHA` with the same `gh run
list`/`gh run view` checks used for KMS and require the expected input
fingerprint in its credential-free preflight log. A different authenticated required
reviewer must again fetch the single pending `aa-admin-sepolia` environment ID
and approve it with the exact `gh api --method POST .../pending_deployments`
command above; self-approval must fail. Watch the run with `gh run watch
--exit-status`. Require exit zero and the workflow's task-scoped
`aa_admin_issuance_resumed` readback with the exact previous reason and supplied
note length, then query the database and require `issuance_paused=false`, a null
reason, and exactly one corresponding `resume` audit row containing the
supplied note. Verify neither AA-admin family has a PENDING/RUNNING task and
its temporary revision was deregistered.

```sql
SELECT issuance_paused, paused_reason, updated_at
FROM aa_sponsorship_control WHERE singleton = TRUE;
SELECT action, reason, operator_note, created_at
FROM aa_sponsorship_control_events
WHERE action = 'resume' ORDER BY id DESC LIMIT 1;
```

This only clears the database circuit breaker; it does not enable issuance while
`AA_NATIVE_SPONSORSHIP_ENABLED=false`. If the row is missing/recreated, the
reason changes, or any later circuit breaker trips, stop: repeat the relevant
chain/accounting diagnosis and use a new compare-and-set resume with the exact
current reason. Never make initial or recreated state automatically unpaused.

### Configure the API dark

The backend profile attestation intentionally treats an on-chain paused
paymaster as unavailable. With the reconciler and Alto healthy but both native
feature flags still false, have the Safe call `unpause()`, wait for a safe
block, and verify `paused()==false`, `sponsorSigner()` and every immutable are
unchanged, and deposit/stake did not move. This cannot authorize spending while
issuance and submission are disabled, but it allows the dark API to prove the
complete live profile. If the following deployment fails, pause again through
the Safe.

Now apply a complete saved Terraform plan with:

```text
configure_native_aa_backend = true
alto_desired_count = 1
aa_reconciler_desired_count = 1
enable_native_aa_sponsorship = false
enable_native_aa_submission = false
aa_native_global_rollout_enabled = false
```

This does not grant the API task role access to the paymaster signing key. If
the secondary-RPC parameter uses a customer-managed key, the API execution
role does receive only the previously attested, SSM-scoped decrypt capability
needed for container startup. Run `deploy-backend.yml` again with
`deployment_scope=api` so the service adopts the newly registered task
definition. Do not patch the live task definition.

The deployed API task definition must now contain these names with reviewed
values:

```text
AA_NATIVE_SPONSORSHIP_ENABLED=false
AA_NATIVE_SUBMISSION_ENABLED=false
AA_NATIVE_CANARY_OWNERS=0x<owner-eoa>
AA_NATIVE_GLOBAL_ROLLOUT_ENABLED=false
AA_MAX_REQUEST_BYTES=262144
AA_SPONSORED_GAS_ALERT_WEI_PER_HOUR=10000000000000000
AA_ALTO_RPC_URL=http://<internal-alto-alb>
AA_RECONCILER_SECONDARY_RPC_URL=<injected from the exact external SecureString>
AA_PAYMASTER_ADDRESS
AA_PAYMASTER_CODE_HASH
AA_PAYMASTER_POLICY_ID
AA_PAYMASTER_SIGNER_ADDRESS
AA_PAYMASTER_KMS_KEY_ID
AA_PAYMASTER_ACCOUNT_CODE_HASH
AA_PAYMASTER_VALIDITY_SECONDS
AA_PAYMASTER_VERIFICATION_GAS_LIMIT
AA_PAYMASTER_POST_OP_GAS_LIMIT
AA_PAYMASTER_MAX_COST_WEI
AA_PAYMASTER_ACCOUNT_OUTSTANDING_WEI
AA_PAYMASTER_CLIENT_OUTSTANDING_WEI
AA_PAYMASTER_GLOBAL_OUTSTANDING_WEI
AA_PAYMASTER_ACCOUNT_HOURLY_WEI
AA_PAYMASTER_GLOBAL_HOURLY_WEI
AA_PAYMASTER_GLOBAL_DAILY_WEI
AA_PAYMASTER_FINAL_RATE_LIMIT_PER_MINUTE
```

Pass only if startup has both independent RPCs attest chain `421614`, choose
their lower common `safe` height, agree on that explicit block header, and at
that same block independently verify the paymaster runtime hash, policy ID,
approved proxy hash, factory/implementation addresses and code hashes,
`factory.accountImplementation()`, sole signer, and on-chain maximum
cost/validity ceilings. It must re-read the explicit header after all profile
reads and reject a changed, more-than-600-second-old, or implausibly future
snapshot. There is no fallback to `latest` or one provider. While sponsorship
is disabled the API must not initialize the signer or call KMS; retain the
protected KMS attestation evidence from Gate 2. A persistent dual-provider
profile-attestation failure blocks stub, final sponsorship, and
`eth_sendUserOperation`, because submission deliberately re-attests the live
account/profile state; native read/status recovery remains usable. By contrast,
a KMS signer startup/attestation failure blocks only new issuance: an exact
already-signed submission may continue if submission is enabled and its
dual-provider profile checks pass.
An issuance request rejected for that startup condition must expose only the
fixed `SIGNER_UNAVAILABLE` / `Native sponsorship startup attestation failed`
client error, never the underlying profile or AWS response.
Both paymaster methods must reject with the expected disabled error, create no
authorization, ledger, event, or recovery rows, and invoke no KMS signing;
rate-window bookkeeping may still be written before the disabled gate. Legacy
reads must still work. Exercise the workflow's authenticated origin-boundary
smoke check:

```bash
export EXPECTED_ALTO_INPUT_FINGERPRINT="$(alto_dispatch_fingerprint \
  deploy "$API_HOSTNAME" sepolia '' 5000000000000000)"
gh workflow run deploy-alto.yml \
  --repo "$APP_REPOSITORY" \
  --ref master \
  -f environment=sepolia \
  -f action=deploy \
  -f utility_gas_cushion_wei=5000000000000000 \
  -f api_hostname="$API_HOSTNAME"
```

The operator's CLI preflight value of `SEPOLIA_BACKEND_URL` must still equal
`https://${API_HOSTNAME}`; the protected workflow independently reads the
repository variable and repeats that equality check before its authenticated
smoke. Again resolve the run by exact SHA,
require the expected input fingerprint, and watch it to success.

## Gate 9: shadow the gateway

Keep the current public
`perps-aa-arbitrum-sepolia-20260830-v2` Pimlico transport shape active.
Exercise the native route only with controlled requests. The Cloudflare worker
must add `X-Plether-AA-Proxy-Token` only on the
exact native and legacy AA paths, strip any client-supplied copy, never cache
AA POST responses, reject/manual-handle backend redirects without following
them, and serve `/perps-aa-manifest.json` with `no-store`. A backend 3xx must
never carry the origin token to another host. Include an exact redirect
non-follow/exfiltration regression in the deployed Worker artifact tests.

Basic read probes through the public native route:

```bash
export PUBLIC_AA_URL=https://app.sepolia.plether.com/api/perps/v1/aa/rpc

curl --fail-with-body --silent --show-error \
  --header 'content-type: application/json' \
  --data '{"jsonrpc":"2.0","id":1,"method":"eth_supportedEntryPoints","params":[]}' \
  "$PUBLIC_AA_URL" | jq

curl --fail-with-body --silent --show-error \
  --header 'content-type: application/json' \
  --data '{"jsonrpc":"2.0","id":2,"method":"pimlico_getUserOperationGasPrice","params":[]}' \
  "$PUBLIC_AA_URL" | jq
```

The first response must contain only the reviewed EntryPoint. Run the same
unsigned, non-submitting canary corpus through native and legacy estimation,
and compare acceptance, gas fields, and error classes. Do not require identical
gas prices. Never shadow `eth_sendUserOperation` and never send the same smart
account nonce to two providers. Include every supported Plether action sequence plus malformed
calldata, unknown target/selector, non-zero value, wrong account, wrong UUPS
implementation/beacon, oversized body, wrong EntryPoint/chain, and rate/budget
boundary cases.

The API-to-Alto relay must likewise use no redirects, a 20-second response
timeout, a 1 MiB streamed body bound, and strict JSON-RPC version/id/result-or-
error validation. Its timeout, redirect, oversized, non-2xx, malformed, and
mismatched-ID tests must fail closed without journaling a false success.

While sponsorship is off, both `pm_getPaymasterStubData` and
`pm_getPaymasterData` must fail closed before reservation or signing. All
methods outside the nine-method public allowlist must be rejected.

### Pre-canary integration qualification

Gate 10 is blocked until a release-candidate environment with real PostgreSQL
and controllable HTTP/RPC peers has exercised the database, loop, and transport
properties below. The absence of a local PostgreSQL/Docker service in an
individual workspace is not by itself a source-code defect, but unit-only
evidence is not sufficient to authorize spending. Attach the CI/integration
run and database/RPC fixture versions to the change record. Require:

- additive migration from the exact pre-native Sepolia schema, idempotent
  restart, exact expected table/column/default/check/foreign-key/unique-index
  definitions, runtime/static-schema parity, and fail-closed startup on renamed
  named CHECK/index objects or weaker objects under the fingerprinted key
  names; PK/UNIQUE/FK constraint names themselves are not part of the catalog
  fingerprint, so this evidence must compare their semantics rather than require
  their original names;
- real concurrent final-paymaster calls proving one durable reservation and
  signature per request/digest, all per-account/per-client/global caps under
  the global lock, exact retry behavior, and no signature returned before its
  row is durable. A KMS or signature-persistence failure must leave the maximum
  liability durably `reserved` with its reserve-ledger entry and deliver no
  signature; only the reconciler may cancel an unsigned reservation older than
  ten minutes under the same lock with an exact release-ledger readback;
- fail-closed initial/recreated control state and compare-and-set admin resume,
  including a stale expected-reason race and an auditable pause/resume event;
- clean and concurrent cursor bootstrap from the pinned deployment block/hash,
  deployment-block log inclusion, wrong anchor/code rejection, partial-restore
  rejection, historical catch-up without a fresh health authorization,
  cursor/health mismatch denial, crash/CAS replay, and the per-digest accounting
  query returning zero violations;
- dual-provider chain/header/log agreement, unavailable/timeout/redirect/
  oversized/malformed responses, stale/future safe heads, provider
  disagreement, post-read header changes, and both a normal leadership
  exclusion test and loss-of-lock test across a long scan;
- real HTTP checks for exact JSON-RPC IDs, the 1 MiB streaming bound, 20-second
  total deadline, redirect refusal, malformed/non-2xx responses, and no secret
  or signed-operation leakage in logs; and
- protected-workflow negative tests for wrong branch, missing/changed
  environment protections, wrong AWS account or region, unexpected topology or
  image digest, duplicate/ambiguous run recovery, task ownership/cleanup, and
  failure, cancellation, and manual rollback paths.

Any missing assertion is a release-qualification gap to close or execute in
that environment before Gate 10; do not silently reinterpret it as a later
mainnet-only requirement.

## Gate 10: enable low-cap native issuance and submission canaries

This is the first spend-authorizing step. Have the on-call, database, chain,
and Safe operators present. Verify alarms and rollback access immediately
before proceeding.

With issuance still off, take a fresh safe-block read and verify
`paused()==false`, `sponsorSigner()` remains the attested address, and the
deposit/stake are unchanged since Gate 8. Any readback mismatch stops the
rollout.

For every proposed owner, prove the derived index-zero account is
Plether-controlled and record its owner, runtime hash, implementation slot,
zero beacon, and absence of pending ownership/upgrade action at both the common
safe block and current head. Freeze the owner-transfer and UUPS-upgrade
authority operationally for the entire issuance window, maximum authorization
validity, and safe drain. Monitor and re-read these values before preparation,
again before submission, and after safe inclusion for the one-at-a-time
canaries. Any transfer, upgrade, beacon activation, or unexplained drift turns
issuance off and pauses the paymaster if an authorization may be reinterpreted.
This monitoring bounds the safe-block/latest TOCTOU but cannot remove it.

Apply a complete saved Terraform plan with an exact, non-empty
`aa_native_canary_owners` comma-separated allowlist and
`enable_native_aa_sponsorship=true` and `enable_native_aa_submission=true`
while keeping `aa_native_global_rollout_enabled=false`, the exact public
`perps-aa-arbitrum-sepolia-20260830-v2` Pimlico transport shape active, both
desired counts at one, and all initial wei caps deliberately low. This
adds narrowly scoped `DescribeKey`,
`GetPublicKey`, and `Sign` permissions for the single KMS key, with signing
restricted to `ECDSA_SHA_256`, and registers the enabled API definition. Deploy
it using `deploy-backend.yml` with `deployment_scope=api` and require both the
on-chain profile and KMS public-key signer startup attestations to pass. Do not patch task
definitions manually. Direct API-role signing makes the small funded deposit
the compromise blast-radius ceiling rather than the database limits; confirm
that this accepted Sepolia-only exception and staffed pause access are recorded
before approval.

The native endpoint is authenticated at the edge-to-origin boundary but is a
public product endpoint; retaining the public v2 Pimlico field set is not a
canary access control for direct native-route callers. The
non-empty `AA_NATIVE_CANARY_OWNERS` value is therefore mandatory for this
gate. Prove an owner on the list can use stub, final sponsorship, and
submission, and that an otherwise valid owner outside the list is denied
before reservation, KMS signing, or Alto submission. Run the gate in a staffed
maintenance window with strict budgets and rate limits. Emptying the allowlist
does not mean global access: with the explicit global switch false, it must
deny every owner. The reviewed Terraform profile also rejects
`aa_native_global_rollout_enabled=true`; global access is not approved by this
runbook.

This does not prove that the caller controls the allowlisted owner:
`pm_getPaymasterData` intentionally carries no account signature for
viem/ERC-7677 compatibility. An attacker can copy an allowlisted owner into an
otherwise valid request and consume a reservation, although it cannot sign the
account UserOperation and therefore cannot spend paymaster gas. Verify that
the globally locked `AA_PAYMASTER_CLIENT_OUTSTANDING_WEI` cap, the durable
`AA_PAYMASTER_FINAL_RATE_LIMIT_PER_MINUTE=6` gate applied before reserve or KMS,
short validity, small global outstanding cap, and small deposit bound this
Sepolia denial-of-budget risk. Caller owner/session proof is required hardening
before any mainnet rollout.

For every stub, final, and submission request carrying an operation, require
one shared request-scoped security snapshot: both providers must attest chain
`421614` and the same explicit lower `safe` header. At that exact block, a
counterfactual account must have no runtime code and both providers must agree
that the reviewed factory implementation and owner/index-zero derivation yield
the sender. A deployed account must have identical approved runtime code, the
same nonzero owner and EntryPoint, the reviewed ERC-1967 implementation, a zero
beacon, and the expected owner/index-zero factory derivation on both providers.
Re-read that exact header after the state reads. For final sponsorship, use the
`baseFeePerGas` in that same explicit header—which has been obtained and matched
independently from both providers—and require `maxFeePerGas` no greater than
`min(10 gwei, max(1 gwei, 3 * baseFeePerGas))`; retain the absolute 10 gwei
max-fee and 2 gwei priority-fee bounds. There is no separate `eth_gasPrice`
fallback. Re-read the same header again after KMS signing. Any unavailable
provider, disagreement, stale/future snapshot, or changed header fails closed
with no single-provider or `latest` fallback.

An exact final retry may return a stored signature only while the database
circuit breaker is clear, reconciler health is newer than 120 seconds and
matches the current cursor block number/hash for this chain/paymaster, and
`validUntil` remains more than the 30-second delivery margin in the future.
Reject a stale delivery without releasing its reservation or mutating a signed
authorization. This delivery check must not revoke submission of an exact
signature already returned to the client, nor provider-bound receipt/status
recovery; those paths remain available under their separate submission and
read controls.

First call `pm_getPaymasterStubData` for one allowlisted canary. Verify a fixed
157-byte `paymasterData` envelope (209 bytes only after packing the paymaster
address and two 16-byte v0.8 gas fields) with the exact paymaster address and
gas limits, a deliberately invalid 65-byte signature, and no database
reservation or KMS call.

Then issue a final sponsorship for the same allowlisted canary account and,
before signing the account operation, inspect the database plus the qualified
instrumented transition evidence:

- exactly one resulting `signed` row and one `reserve` ledger entry exist;
- the transition evidence proves the maximum-liability row and reserve ledger
  committed before KMS, and that signing changed that same row to `signed` and
  stored exactly one signature before the response escaped;
- an exact retry returns the persisted authorization without another reserve;
- a conflicting retry fails closed.

Submit that exact operation. The API must persist send intent and expected hash
before calling Alto. The client must durably journal the exact signed operation
and locally computed EntryPoint v0.8 hash through
`journalSignedUserOperation({operation, entryPoint})` before invoking
`eth_sendUserOperation`. Both layers must reject a different Alto-returned hash
and retry only the exact payload. Wait for a safe-chain receipt and reconciler settlement. Repeat
for each supported user action, one deliberately reverted user call, one API
restart, one Alto restart, and one duplicate request. Keep volumes at one
operation at a time until every invariant and alert is verified.

The canary set must additionally prove that:

- a settled operation records one immutable event, one actual charge, one
  release, and a `settled` authorization exactly once;
- a failed user call still records its actual gas charge;
- duplicate log delivery and reconciler restart do not double-charge or
  double-release;
- an operation submitted outside Plether's Alto is still found and settled;
- an unused signed authorization is not expired before its validity deadline
  is behind the safe cursor;
- an Alto rejection alone does not release its reservation;
- controlled unsigned requests that name an allowlisted owner cannot exceed
  the per-client outstanding cap or final-issuance rate, and their resulting
  liabilities expire only through the safe reconciler.

Stop and roll back issuance immediately on signer mismatch, unexpected target,
hash mismatch, cursor stall/discontinuity, unknown event, actual cost above
reservation, budget overrun, duplicate ledger effect, persistent Alto error,
or deposit/executor balance alarm.

## Gate 11: qualify the native-v2 field set without public activation

Only the exclusive transport field set—not the `-v2` suffix—changes which
provider prepares new browser operations. The checked-in public manifest is
already `perps-aa-arbitrum-sepolia-20260830-v2`, using exact
`pimlicoRpcUrl` and no native fields. Because the static manifest has no cohort
selector, the standard Sepolia frontend must retain that reviewed Pimlico
shape while Alto runs with `SAFE_MODE=false`. Keep an exact, non-empty canary
owner allowlist and
`aa_native_global_rollout_enabled=false`; current Terraform intentionally
rejects the global switch and refuses issuance with an empty allowlist. Do not
edit `apps/frontend/public/perps-aa-manifest.json` to use the native field set,
dispatch the standard Sepolia frontend workflow with that field set, or expose
a native-shape preview publicly in this gate.

Exercise the native-v2 shape only through a controlled client harness that
injects the candidate manifest directly and is available solely to the staffed
canary operators. A
separately access-controlled preview is acceptable only after that access
boundary is implemented and reviewed; an obscure URL is not access control.
The harness/preview must use an owner already present in the backend allowlist,
must not alter the standard public artifact, and must preserve the exact
same-origin AA route and Worker token boundary. Unit tests alone do not prove
the live browser, edge, API, Alto, and reconciler path. If no such controlled
harness or access-controlled preview exists, this gate is blocked and that
surface must be implemented before claiming end-to-end native qualification.

The reviewed Sepolia native-v2 candidate must preserve the current deployment
generation and every common binding, changing only the exclusive transport
field set and adding the paymaster metadata. It must be this strict object:

```json
{
  "version": "perps-aa-arbitrum-sepolia-20260830-v2",
  "chainId": 421614,
  "entryPoint": "0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108",
  "entryPointVersion": "0.8",
  "smartAccountMode": "simple",
  "smartAccountVersion": "permissionless-simple-v0.8",
  "smartAccountIndex": "0",
  "smartAccountFactory": "0x13E9ed32155810FDbd067D4522C492D6f68E5944",
  "usdc": "0x1647e41f49ED6D688936092B5a291c4B28106343",
  "usdcSupportsEip3009": false,
  "usdcEip712Name": null,
  "usdcEip712Version": null,
  "marginClearinghouse": "0x2f98787F6dCC3b1f2E4a2AFa5acf410159b9F211",
  "cfdEngine": "0x3dc9C0A1f9C745A4B08BD5C2E6c7aE613561c20D",
  "orderRouter": "0x97A901dE2B267c307E264FD5F71403F8072F73e7",
  "orderLifecycleBook": "0xa210928a7E0AE27626B8d0E67Bbd82305438aB9E",
  "policyEvaluator": "0xaa4703B190684b5A57b8a9aA432fA043B169D171",
  "bundlerRpcUrl": "/api/perps/v1/aa/rpc",
  "paymasterRpcUrl": "/api/perps/v1/aa/rpc",
  "paymasterAddress": "0x_REPLACE_WITH_DEPLOYED_ADDRESS",
  "paymasterVersion": "plether-verifying-v1",
  "userOperationExplorerUrlTemplate": "https://arbitrum-sepolia.blockscout.com/op/{userOperationHash}",
  "transactionExplorerUrlTemplate": "https://arbitrum-sepolia.blockscout.com/tx/{transactionHash}",
  "testnetFaucet": null,
  "sponsorshipEnabled": true
}
```

Replace only the paymaster placeholder with the checksummed deployed address.
The resulting nonzero address must equal the live attested paymaster, and the
version must remain exactly
`perps-aa-arbitrum-sepolia-20260830-v2`. This candidate is native because it
contains all four validated native fields and no `pimlicoRpcUrl`, not because
its version ends in `-v2`. The parser permits a v2 version with either the
exact Pimlico field set or this exact native field set, never a partial or
hybrid object. Both native URLs must be the same-origin exact route. Do not
append a query or fragment. Preserve parsing, Pimlico validity decoding, and
recovery for legacy-v1 and current-v2 Pimlico-shaped journals, and do not add
provider fallback after preparation or submission.

Rerun the frontend qualification suite from Gate 1 against the candidate
object, then use the controlled harness/preview for new-operation and recovery
canaries. Independently prove the public manifest remains uncached and on the
exact current v2 Pimlico transport shape:

```bash
export MANIFEST_HEADERS="$(mktemp /tmp/plether-aa-manifest-headers.XXXXXX)"
export MANIFEST_BODY="$(mktemp /tmp/plether-aa-manifest-body.XXXXXX)"
curl --fail-with-body --silent --show-error \
  --dump-header "$MANIFEST_HEADERS" \
  --output "$MANIFEST_BODY" \
  https://app.sepolia.plether.com/perps-aa-manifest.json
rg -i '^cache-control:.*no-store' "$MANIFEST_HEADERS"
jq -e '
  .version == "perps-aa-arbitrum-sepolia-20260830-v2" and
  .pimlicoRpcUrl == "/api/perps/v1/aa/pimlico" and
  .usdc == "0x1647e41f49ED6D688936092B5a291c4B28106343" and
  .marginClearinghouse == "0x2f98787F6dCC3b1f2E4a2AFa5acf410159b9F211" and
  .cfdEngine == "0x3dc9C0A1f9C745A4B08BD5C2E6c7aE613561c20D" and
  .orderRouter == "0x97A901dE2B267c307E264FD5F71403F8072F73e7" and
  .orderLifecycleBook == "0xa210928a7E0AE27626B8d0E67Bbd82305438aB9E" and
  .policyEvaluator == "0xaa4703B190684b5A57b8a9aA432fA043B169D171" and
  (has("bundlerRpcUrl") | not) and
  (has("paymasterRpcUrl") | not) and
  (has("paymasterAddress") | not) and
  (has("paymasterVersion") | not)
' "$MANIFEST_BODY"
test "$(shasum -a 256 "$MANIFEST_BODY" | awk '{print $1}')" = \
  7ff3816cda0d9f41e5897067233dc15309d5c3f00f653b9e40b1c29a0c376059
rm -f -- "$MANIFEST_HEADERS" "$MANIFEST_BODY"
```

Use an isolated browser profile for controlled native-shape new-operation
canaries and existing profiles with both legacy-v1 and current-v2
Pimlico-shaped journals for recovery canaries. Pass only if:

- new operations use native paymaster and Alto endpoints with no Pimlico
  request;
- prepared/signed operations never switch provider;
- each native-shape journal hash-binds the exact signed operation to immutable
  manifest version, transport/paymaster address/version, and validity deadline,
  and recovery rejects conflicting or tampered authority metadata;
- Pimlico-shaped pending/recovery operations, whether their version suffix is
  v1 or v2, continue to query the legacy route;
- owner and account identity remain unchanged;
- every canary reaches one correct safe-chain settlement and ledger outcome;
- p95 latency, error rates, cursor lag, KMS throttles, deposit consumption, and
  executor balances remain within the approved observation thresholds;
- an ordinary public browser still fetches the exact dated-v2 Pimlico shape,
  uses Pimlico for newly prepared work, and cannot discover or enter the
  controlled native-shape harness/preview.

Ramp capacity only through separately reviewed rate/budget Terraform changes
and observation gates. Do not increase budgets merely to suppress an alarm.

Public activation of the native-v2 transport field set is a later, separately
approved change and is not authorized while this profile has
`ALTO_SAFE_MODE=false`. Before that change,
implement and qualify an Alto configuration that restores equivalent ERC-7562
tracing, entity-role, reputation, and anti-abuse protections; retain dangerous
skip as false; resolve the unsigned-final-request ownership/admission risk;
update the ADR and Terraform guard through review; and repeat Gates 1 through
10 against the new immutable image/task definition. Only after those controls
pass may a later change explicitly enable reviewed global eligibility, replace
the public manifest's exact Pimlico transport fields with the Gate 11 native
field set in a commit on `master`, deploy the standard Sepolia frontend with
`deploy-frontend.yml`, and verify the uncached deployed object. The version is
already v2; never treat its suffix as activation or switch transport with an
ad hoc workflow variable or Cloudflare edit.

### Deferred public native transport activation (currently blocked)

The current release cannot execute this subsection: Terraform rejects global
eligibility, Alto lacks the qualified safe-mode protections, final ownership
proof is absent, and the static manifest cannot select a cohort. After those
gaps are implemented and a new security/operations approval is recorded, use a
separate release and change. First enable its explicit, reviewed global backend
gate at low caps, deploy the backend before the frontend, and repeat the
startup plus per-request dual-provider attestations. Treat that interval as
live native exposure and stop issuance if the frontend cut cannot proceed.

Then replace `apps/frontend/public/perps-aa-manifest.json` with the exact Gate
11 native-v2 object and deployed paymaster address in a reviewed commit on
`master`; preserve the dated-v2 release identity and common bindings and do
not use a runtime substitution. Set `RELEASE_SHA` to that commit, rerun the
full frontend/Worker qualification suite, repeat the GitHub auth, remote-SHA,
and duplicate-run checks, and only then dispatch:

```bash
gh workflow run deploy-frontend.yml \
  --repo "$APP_REPOSITORY" \
  --ref master \
  -f environment=sepolia
```

Resolve the run by exact SHA, inspect with `gh run view`, and watch with
`gh run watch --exit-status`. Fetch the public manifest with `cache: no-store`
semantics and require the exact native-v2 object, same-origin routes, nonzero
attested paymaster, and no `pimlicoRpcUrl`. Run new-browser native canaries and
legacy-v1 plus current-v2 Pimlico-journal recovery canaries before increasing
any cap. If any cutover check fails, publish the last reviewed dated-v2
Pimlico-shape commit, turn new native issuance off, and leave native
submission/read/reconciliation online for any native-shape operation that was
already prepared or signed.

## Reviewed release boundary and remaining surfaces

This release can support only the explicit, one-at-a-time,
Plether-controlled, low-cap Sepolia canary after the pre-canary integration
qualification passes. The security review accepts the following bounded risks
for that cohort only: Alto's reduced safe-mode checks, unsigned final-request
reservation abuse, safe-block/latest owner-and-upgrade TOCTOU, a possible
post-read `safe` reorg leaving stale settlement, and direct API-role KMS
signing. Keep the deposit and all caps small, both providers independent,
account ownership/upgrades frozen, alerts delivered to staffed operators, and
Safe/database/chain responders ready to stop issuance and reconcile. None of
those exceptions is evidence of production readiness.

Public/global Sepolia and every mainnet rollout remain blocked until a new
reviewed release provides all of:

- safe-mode-equivalent ERC-7562 tracing, entity-role, reputation, and abuse
  protections;
- authenticated owner/session proof before final reservation rather than
  allowlist membership inferred from unsigned request fields;
- finalized-boundary settlement or reversible orphan accounting plus a fenced,
  audited repair path;
- immutable account execution or on-chain enforcement of the live
  implementation/action/global-spend policy across the authorization window;
- an isolated policy signer, or equivalent on-chain semantic/global limits,
  so the public API task role cannot use raw `kms:Sign` to bypass off-chain
  controls; and
- a newly reviewed Terraform/global-eligibility path and public native-v2
  transport activation, followed by the full qualification sequence in this
  runbook.

The following recovery and operability surfaces are also intentionally absent
from the current release and must not be improvised during an incident:

- a versioned client-key HMAC key ring and dual-accepted edge-token rotation;
- a staged replacement-KMS-key selector that retains the old key;
- a protected one-off Alto executor-nonce flush action;
- lifecycle-exempt or separately archived Alto and log-router rollback digests;
- cursor rewind, orphan-event repair, and post-PITR signed-authorization import;
- exact-key, mid-cycle leadership fencing rather than cycle-start advisory-lock
  health only;
- bounded cleanup with explicit retention and growth/latency alerts for
  `aa_rate_windows` and expired `aa_recovery_operations` rows;
- bounded reason-safe provider-response evidence sufficient to reconstruct a
  transient mismatch;
- structured reason-specific counters and alarms for budget/cap, final-rate,
  account/IP-rate, and recovery-authorization denials, plus active liability by
  authorization state; and
- legacy Pimlico pre-send durability plus bounded, no-redirect strict
  transport.

Gate 11 additionally requires a controlled native-shape client harness or
independently access-controlled preview. Its absence blocks native transport
qualification, not the public Pimlico-shaped Gate 10 canary. Record every
still-open item and its owner in the change record; do not translate
“deferred” into operator discretion.

## Legacy drain and completion

This section is deferred until the separately approved public native transport
activation; the canary rollout above must retain the current dated-v2 Pimlico
shape and all Pimlico credentials. The Pimlico relay has a known recovery-availability weakness: it records durable
recovery only after Pimlico reports send success and currently swallows a
database-write failure, so a database outage plus API restart can strand
receipt/status authorization for a submission that escaped. Its upstream
transport also retains default redirect handling, no streaming response-body
cap, and weaker response validation than the native relay. Keep legacy drain
staffed, preserve browser journals and upstream evidence, and do not cite the
native transport/journal-first guarantees as applying to the Pimlico path.
Before relying on Pimlico recovery for an unattended migration, add a pre-send durable intent or
outbox and the same bounded, no-redirect, strict JSON-RPC transport.
After that future activation, wait until the last Pimlico-backed operation's
validity and recovery window have passed and all Pimlico-shaped journals—both
legacy v1 and current v2—are terminal, then stop Pimlico issuance while
preserving its read/recovery route. Prove from logs and browser-recovery
telemetry that no legitimate operation still needs Pimlico before removing its
API key or policy credential.

Full public-provider migration completion requires:

- the public native-v2 transport shape is stable for the approved Sepolia
  observation window;
- no unresolved `reserved`, `signed`, or `submitted` native authorization is
  older than policy permits;
- the safe cursor is healthy and all accounting invariants reconcile;
- the paymaster deposit and Alto wallets have tested alerting and replenishment
  procedures;
- legacy-v1 and current-v2 Pimlico-shaped journals still recover, while new
  native-shape operations make no Pimlico calls;
- the release record contains all workflow URLs, SHAs, addresses, hashes,
  balances, cursor evidence, canary hashes, and approvals.

Removing Pimlico-shaped manifest parsing, its validity decoder, or journal
recovery support is a separate product/data migration and is not part of this
rollout.

## Rollback and drain

Rollback is ordered to prevent new bearer authorizations without stranding
ones that may already have escaped.

### Universal first response

1. Turn native sponsorship issuance off through
   `enable_native_aa_sponsorship=false`, apply the reviewed Terraform plan,
   then run `deploy-backend.yml` with `deployment_scope=api` so ECS adopts the
   resulting API task definition. Keep `enable_native_aa_submission=true` and reads available
   until outstanding signed operations are resolved unless continuing
   submission is itself unsafe. The sponsorship switch gates only
   `pm_getPaymasterStubData` and `pm_getPaymasterData`; the submission switch
   gates only `eth_sendUserOperation`. Do not remove an owner from
   `aa_native_canary_owners` while that owner has an outstanding signed
   operation: the owner gate also protects submission. Keep
   `aa_native_global_rollout_enabled=false` and preserve the current cohort
   through drain.
2. Publish or retain the last reviewed
   `perps-aa-arbitrum-sepolia-20260830-v2` Pimlico-shape manifest so new public
   clients do not prepare native operations. Rollback changes the exclusive
   transport field set, not the already-v2 suffix.
3. Keep the native read/status route, database, reconciler, KMS key resource,
   paymaster, and Alto available for drain unless the incident specifically
   compromises that component. The recovery API task role still gets no
   paymaster-signing-key permission and does not call `GetPublicKey`; draining
   already signed operations needs no new signature. An execution-role decrypt
   grant for the exact secondary-RPC SecureString may remain so recovery reads
   can start.
4. Record the maximum `validUntil`, current safe cursor, and counts/sums by
   authorization state. Do not release signed/submitted liability manually.
5. Wait until the reconciler settles or safely expires every possibly escaped
   authorization. Reconcile the ledger, EntryPoint events, and deposit delta.

### Alto failure or bad release

Disable new submission and leave issuance off. For an ordinary workflow
failure after a recorded mutation, `deploy-alto.yml` attempts to restore the
previous task definition and wait for service stability. Treat that as
best-effort, not a guarantee: cancellation, runner loss, missing step output,
or rollback failure can bypass or interrupt it. Always verify the selected task
definition, desired/running counts, target health, and running image digests
with `gh` and AWS. If the prior revision was not restored, use the protected
explicit rollback below.

For a post-success or failed-automatic rollback, select the exact previously
qualified ACTIVE `plether-sepolia-alto:<revision>` from the rollout evidence.
`ACTIVE` alone is insufficient because the ECR lifecycle retains only a bounded
number of images. Inspect the definition and prove that both referenced image
digests still exist before dispatch:

```bash
aws --profile plether ecs describe-task-definition \
  --task-definition "plether-sepolia-alto:<revision>" \
  --query 'taskDefinition.{arn:taskDefinitionArn,status:status,runtime:runtimePlatform,containers:containerDefinitions[].{name:name,image:image}}'

export ALTO_ROLLBACK_IMAGE="$(aws --profile plether ecs describe-task-definition \
  --task-definition "plether-sepolia-alto:<revision>" \
  --query 'taskDefinition.containerDefinitions[?name==`plether-alto`].image | [0]' \
  --output text)"
export ROUTER_ROLLBACK_IMAGE="$(aws --profile plether ecs describe-task-definition \
  --task-definition "plether-sepolia-alto:<revision>" \
  --query 'taskDefinition.containerDefinitions[?name==`otel-log-router`].image | [0]' \
  --output text)"
export ALTO_ROLLBACK_DIGEST="${ALTO_ROLLBACK_IMAGE##*@}"
export ROUTER_ROLLBACK_DIGEST="${ROUTER_ROLLBACK_IMAGE##*@}"
test "$ALTO_ROLLBACK_DIGEST" != "$ALTO_ROLLBACK_IMAGE"
test "$ROUTER_ROLLBACK_DIGEST" != "$ROUTER_ROLLBACK_IMAGE"
aws --profile plether ecr describe-images \
  --repository-name plether-alto-sepolia \
  --image-ids imageDigest="$ALTO_ROLLBACK_DIGEST" >/dev/null
aws --profile plether ecr describe-images \
  --repository-name plether-otel-log-router-sepolia \
  --image-ids imageDigest="$ROUTER_ROLLBACK_DIGEST" >/dev/null
```

The current ECR lifecycle keeps only the newest ten images and has no protected
rollback tag/exemption, so it cannot guarantee those digests for an arbitrary
window. Treat both successful lookups immediately before dispatch as the
minimum rollback gate. If either fails, that revision is not rollback-capable;
do not attempt to recreate an unproven log-router digest during the incident.
For a guaranteed rollback window, first add a reviewed lifecycle exemption or
separate immutable archival repository for both images and prove pull access.

Then use the workflow's reviewed rollback action, never a direct
`aws ecs update-service`:

```bash
export EXPECTED_ALTO_INPUT_FINGERPRINT="$(alto_dispatch_fingerprint \
  rollback "$API_HOSTNAME" sepolia \
  'plether-sepolia-alto:<revision>' 5000000000000000)"
gh workflow run deploy-alto.yml \
  --repo "$APP_REPOSITORY" \
  --ref master \
  -f environment=sepolia \
  -f action=rollback \
  -f rollback_task_definition="plether-sepolia-alto:<revision>" \
  -f utility_gas_cushion_wei=5000000000000000 \
  -f api_hostname="$API_HOSTNAME"
```

The workflow must reject a non-family/inactive revision, any topology change
beyond the immutable Alto/log-router image fields, or a non-ARM64/non-ECR
digest. Require the run's credential-free log to contain the expected input
fingerprint before approval. Because ECS is configured `0/100`, rollback has intentional downtime
but cannot run old and new executor sets concurrently. Resolve the run by SHA,
watch it to success, and verify exactly one healthy task/target. If the
rollback fails, keep Alto dark. Signed operations may be submitted elsewhere,
so continue chain reconciliation through both RPC providers and retain their
full reservations until safe expiry.

### Alto executor nonce recovery

`ALTO_FLUSH_STUCK_TRANSACTIONS_DURING_STARTUP` must remain `false` in every
normal and simulation-bootstrap task. Alto v1.2.7 starts that flush without
awaiting it, so enabling it on an accepting task can race executor traffic.

If an executor nonce is demonstrably stuck, disable sponsorship and
submission, restore the dated-v2 Pimlico-shape frontend, wait for in-flight API requests to
end, scale Alto to zero through a reviewed Terraform plan, and verify no Alto
task is running. Reconcile each executor's latest/pending nonce and known
bundle transactions before authorizing a flush. Preserve all signed-operation
reservations.

The current workflow deliberately has no nonce-flush action and rejects a
normal task with the flag enabled. Add a separately reviewed one-off
maintenance action before attempting recovery. It must run exactly one dark
task with no service listener, manual bundle mode, simulation deployment and
wallet refilling off, the startup flush enabled only for that task, task-scoped
logs, bounded completion, and unconditional stop/deregister cleanup. After
verifying the expected replacement transactions and nonces, restore the
hard-false configuration, start one normal task, and re-run all health and
submission canaries. Never toggle the flag on the long-running ECS service.

### Paymaster policy/backend failure

Disable issuance first. If the on-chain contract is accepting invalid policy
or the signer is suspected compromised, submit `pause()` through the Safe.
Pausing makes outstanding authorizations unusable onchain but does not justify
releasing them until the pause transaction and their expiry are behind the
safe cursor. Roll back the backend image through a reviewed commit and the
normal `deploy-backend.yml` workflow; do not edit a live ECS definition or
database row by hand.

### Origin credential and client-key HMAC rotation

`AA_PROXY_ORIGIN_TOKEN` currently serves two roles: the Worker-to-API bearer
credential and the HMAC key for `client_key`/`account_key`, sponsorship request
idempotency, submission ownership, and recovery-read authorization. The API
accepts one token, ECS resolves the SSM version only when a task starts, and
Cloudflare stores a separate copy. There is no atomic or zero-downtime rotation:
changing either side first produces temporary `403` responses, and discarding
the old token makes its HMAC pseudonyms impossible to reproduce.

For a planned rotation, first disable issuance and keep the old token on both
sides. Leave exact signed submission, both native and legacy recovery paths,
and the reconciler online while draining. Drain all `reserved`, `signed`, and
`submitted` authorizations behind the safe cursor, then wait until every row in
`aa_recovery_operations` protected by the old pseudonym has passed `expires_at`.
Verify zero remaining liability and
zero unexpired recovery rows created under the old token:

```sql
SELECT COUNT(*) FROM aa_sponsorship_authorizations
WHERE state IN ('reserved','signed','submitted');
SELECT COUNT(*) FROM aa_recovery_operations
WHERE expires_at > clock_timestamp();
```

Both results must be zero. A caught-up reconciler deletes expired recovery rows,
but rows can remain until its next successful verified cleanup cycle and the
current delete is unbatched; the safety gate is zero *unexpired* rows, not a
zero total table count. Schedule a brief AA-route outage and update the backend
SSM/task and Cloudflare Worker secret as one staffed change, minimizing but not
denying the expected `403` interval. Redeploy through the normal backend and
frontend workflows, prove both exact AA paths accept the new Worker-injected
token and reject a browser-supplied token, then restore submission/issuance in
that order. Never change only Terraform, only the GitHub/Cloudflare secret, or
a live task.

If the origin token is compromised, disable issuance and submission, rotate
both copies immediately, and treat outstanding client-bound submission and
recovery as stranded until an incident-specific recovery path is reviewed.
Document the lost native and legacy recovery horizon. Keep every reservation
for safe-chain reconciliation; token rotation is not evidence that an
authorization is unused.

Before claiming rotation continuity, separate the concerns in code and IaC:
add a versioned `AA_CLIENT_KEY_HMAC_SECRET` key ring, store/recognize key
versions, accept old and new pseudonyms for retry, submission, and recovery,
write only the newest version, and retain old HMAC keys for at least maximum
authorization validity plus safe drain and the recovery TTL. Then rotate the
edge origin token independently with a dual-token acceptance window. That
versioned key ring and dual-token path do not exist in the current release.

### KMS signer compromise or rotation

The paymaster intentionally has exactly one authorized signer; there is no
overlap rotation. For a planned rotation, disable issuance but leave
submission, reads, reconciliation, and the paymaster available. Record the
maximum `validUntil` of every authorization signed by the old key, then wait
until each is `settled` or `expired` and that deadline is behind the continuous
safe cursor. Reconcile the ledger, EntryPoint events, and deposit delta before
continuing, then pause the paymaster.

For suspected compromise, disable issuance and the KMS key immediately and
submit `pause()` through the Safe without waiting for drain. Leave reads and
reconciliation available; keep submission available only if incident analysis
shows it is safe. The pause deliberately makes outstanding signatures
unusable. Do not release their reservations until the pause transaction and
their maximum `validUntil` are behind the continuous safe cursor and every
authorization is terminal. Do not schedule KMS deletion.

Create or select the replacement KMS key, derive its Ethereum address twice,
and complete the same public-key/signature attestation used at Gate 2. Once the
drain condition for the applicable path holds and the paymaster is paused,
have the Safe call `setSponsorSigner(address)` with the new address. Verify
`sponsorSigner()` equals the replacement address. With issuance still false,
update `aa_paymaster_signer_address` and the KMS key selection in Terraform and
apply a reviewed plan so the next API revision contains the new profile. The
backend profile attestation rejects a paused paymaster, so now have the Safe
unpause it while the old API still has issuance disabled; wait for the shared
safe boundary and recheck the signer. Deploy the newly registered API revision
and require its dual-provider on-chain startup attestation to pass. Finally set
the canary allowlist, enable sponsorship in a separate reviewed plan, deploy
again, and require the KMS public-key/startup signer attestation before the
first new signature. If any step fails, turn issuance off and pause through the
Safe again. Keep the retired KMS key disabled but recoverable for the
incident-retention period; do not delete it as part of rotation.

Current Terraform injects the ARN of its one fixed
`aws_kms_key.aa_paymaster_signer` resource directly and exposes no staged
replacement-key selector. Before a real rotation, add a reviewed IaC path that
can provision a second protected key, select its ARN for the API, and scope
`GetPublicKey`/`Sign` to that selection without destroying the old key. Alias
repointing alone is insufficient because the task receives the key ARN. Until
that path exists, keep issuance paused rather than rotating out of band.

### Reconciler or database failure

Disable issuance and keep the paymaster/Alto evidence intact. If database
availability can be restored without losing committed rows, bring back exactly
one reconciler only after its stored cursor hash agrees with both providers and
the per-digest invariant query returns zero rows. An unknown event,
inconsistent cursor, actual cost above reservation, or broken ledger invariant
requires incident reconciliation before restart.

PITR is more dangerous than ordinary availability recovery: a restored point
can omit a signature that already escaped and remains submit-able through any
bundler. Before any restore with a nonzero or uncertain RPO, disable submission
as well as issuance, preserve the damaged database/WAL and task evidence, and
normally have the Safe pause the on-chain paymaster. Record the restore cutoff,
last provably durable authorization, pause transaction, and the latest time at
which a signature could have escaped. Reconstruct every post-cutoff signed row
and ledger entry from independent durable evidence before accepting events. If
complete reconstruction is impossible, keep the paymaster paused until the
maximum possible authorization validity and the pause transaction are behind a
continuous dual-provider safe scan, then reconcile all EntryPoint events and
the deposit delta before considering unpause. Never infer absence from the
restored database.

The current release has no supported cursor rewind, orphan-event repair, or
post-PITR authorization import/full-rescan command. Do not update cursor or
ledger rows with ad hoc SQL and do not “replay from a predecessor” by editing
the cursor. Implement and review a fenced repair tool that validates both
providers, reconstructs per-digest state transactionally, and preserves an
audit trail before attempting such a recovery. Until it exists, a canonical
cursor mismatch or lost signed row is a hard recovery blocker and the
paymaster remains paused.

### Frontend failure

Publish the last reviewed dated-v2 Pimlico-shape manifest/frontend commit with
`deploy-frontend.yml`. Do not make an in-session provider fallback. Users with
already prepared or signed native-shape operations must continue on the native
provider or receive an explicit retry/support state; users with Pimlico-shaped
journals, whether legacy v1 or current v2, remain on the legacy route. The
Pimlico shape does not carry the Plether paymaster address, so the current
manifest cannot by itself expiry-parse an outstanding native operation.
Preserve the shipped dual-shape compatibility code, the prior native manifest
identity in operational records, and the native read/submission route until
all native-shape journals are terminal.

### Complete shutdown

Only after every native authorization is terminal at the safe cursor may Alto
be scaled to zero with `alto_desired_count=0`. Keep the reconciler and read
path until the recovery window closes. With Safe approval, withdraw excess
EntryPoint deposit separately from stake. Stake withdrawal is two phase:
`unlockStake()`, wait the on-chain unstake delay, then `withdrawStake(address)`.
Do not destroy Terraform state, the paymaster, KMS key, or database tables as
part of an operational rollback.

## Alert response matrix

| Signal | Immediate action | Recovery gate |
| --- | --- | --- |
| KMS signer/on-chain signer mismatch or KMS errors | Disable issuance; pause paymaster if compromise is possible | Two independent address derivations, KMS test signature, on-chain readback, clean startup attestation |
| Reconciler lag, cursor gap, block-hash mismatch, or invalid timestamp | Disable issuance; preserve cursor/evidence; restart only from the unchanged, dual-provider-verified stored cursor | Both providers agree throughout a continuous safe scan and accounting reconciliation; a canonical cursor mismatch remains blocked pending a reviewed fenced repair |
| Reconciler provider disagreement | Disable issuance; preserve the emitted reason and task/database evidence; immediately make bounded, redacted independent re-queries because raw provider responses are not retained | Root cause independently explained and both providers agree on chain ID, headers, safe boundary, and complete event sets across the observation window |
| Reconciler RPC unavailable or heartbeat missing | Disable issuance; keep reservations; verify both independent RPCs' HTTPS/443 connectivity, `safe` support, and required history | Continuous dual-provider cursor advance and heartbeat across the observation window |
| Native request security attestation failure | Disable issuance; preserve the rate-limited `aa_native_security_attestation_failure` event, method and task/request timing, then immediately make bounded, redacted independent re-queries—the gateway does not retain the underlying provider reason/responses | Both providers agree on chain, explicit safe header, profile, account identity/runtime/slots, and applicable fee state; post-read header revalidation and an allowlisted canary pass; add bounded non-secret provider/reason evidence before treating the alert as a complete forensic record |
| Native API fault aggregate | Disable issuance; identify the exact event before restarting anything | The signer, reconciliation freshness, bundler hash, or database invariant relevant to the event passes and API startup attestation is clean |
| Unknown paymaster event | Disable issuance and page security/on-call | Event explained and indexed; no missing or forged authorization |
| Actual cost exceeds reservation | Disable issuance; do not alter ledger manually | Prefund formula and event proven, deposit delta reconciled, code fix reviewed |
| Outstanding/hourly/daily budget reached | Leave request denied; investigate demand/abuse | Budget window clears or separately approved policy change |
| Unsigned final-request reservation abuse | Disable issuance; retain every escaped signature/reservation and capture exact denial responses plus read-only per-client/global DB evidence | Safe expiry/settlement complete; liability reconciled; reason-specific abuse telemetry or an explicitly approved low-volume exception exists |
| Paymaster deposit low | Disable issuance before exhaustion; refill through approved Safe flow | Safe deposit and readback above threshold plus buffer |
| Paymaster unstaked | Leave issuance off; inspect EntryPoint deposit info and stake through the Safe | `staked=true`, approved stake/delay, safe-block readback, alert cleared |
| Executor or utility balance low | Disable new submission if bundling is impaired; refill dedicated address | All five wallet balances and refill behavior verified |
| Alto gas-price initialization or refresh failure | Disable new native traffic even if `/health` remains green; inspect read RPC and fee methods | Initialization and periodic refresh recover; estimate/send canaries pass |
| Alto unhealthy/no targets | Disable new native traffic; roll back to qualified digest | Exactly one healthy task/target and successful read/estimate probes |
| Alto returned hash differs from local hash | Disable submission and issuance; preserve request/response evidence without signatures | Root cause fixed and exact-hash canary passes |
| Duplicate ledger charge/release or invariant failure | Disable issuance; stop reconciler writer after evidence capture | Database repair reviewed and invariant/replay tests pass |
| Database restore/PITR or missing durable signature | Disable issuance and submission; pause paymaster; preserve old database/WAL and establish the exact RPO | Every escaped authorization reconstructed, or its maximum validity and the pause are behind continuous safe coverage; events/deposit and per-digest ledger reconcile |
| UUPS implementation or beacon mismatch | Reject that account and disable issuance if systemic | Reviewed implementation restored/proven; all canary slot checks pass |
| API/worker route leaks origin token or accepts extra methods | Disable route/native issuance; rotate token | Exact-path tests pass and deployed worker verified |

For workflow failures, inspect logs only with `gh`:

```bash
gh run view RUN_ID --repo "$APP_REPOSITORY" --log-failed
```

For ECS and CloudWatch evidence, use `aws --profile plether`; never retrieve or
print secret values while diagnosing. Every alert resolution must append its
evidence and explicit go/no-go decision to the rollout or incident record.
