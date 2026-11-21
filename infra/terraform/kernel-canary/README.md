# Isolated Sepolia Kernel canary

This Terraform root owns the new Kernel API, Alto, reconciler, image repositories, build role, and KMS keys in `plether/sepolia/kernel-canary/terraform.tfstate`. The live EntryPoint v0.8 application and Nitro RPC stacks remain in their separate states. Existing VPC, subnets, ECS cluster, API listener, Nitro ALB, database access group, and pinned task definitions are read only data sources. The root reads no SSM SecureString values. All opt-ins, task desired counts, payment modes, and the public route default to off.

Use Terraform 1.16 with `AWS_PROFILE=plether` after `aws --profile plether sts get-caller-identity` reports account `932542905614`. The AWS provider also pins `ap-southeast-1` and the account. Review a saved plan before each phase. A core Sepolia `terraform.tfvars.sepolia` file is not needed. The [Kernel qualification runbook](../../../scripts/kernel-qualification/deploy-sepolia.md) covers contract deployment and attestation.

## Phase 1: image build and sponsor signer

Set `provision_image_build_resources=true` and `provision_paymaster_key=true`. The reviewed saved plan must add only two immutable ECR repositories without expiration rules, the dedicated `plether-sepolia-kernel-canary-image-build` OIDC role/policy, and the separate secp256k1 paymaster KMS key/alias. The role trusts only the `backend-admin-sepolia` and `alto-admin-sepolia` protected GitHub environments and can push/scan only the two canary repositories. Set `KERNEL_CANARY_BUILD_ROLE_ARN` in those protected environments to the output `kernel_canary_build_role_arn`; the image workflows neither change ECS nor tag `latest`. Record their reviewed `@sha256:` image URIs and zero-CRITICAL scan evidence. The KMS signing key policy permits `ECDSA_SHA_256` signing only through the future exact canary API task role; derive the public sponsor address through KMS without exporting key material.

## Phase 1.5: dedicated SecureStrings

Set `provision_ssm_encryption_key=true` and review a plan adding only the symmetric `alias/plether-sepolia-kernel-canary-ssm` key/alias. Provision three separate SSM SecureStrings under that key: `/plether/sepolia/kernel-canary-alto-executor-private-keys`, `/plether/sepolia/kernel-canary-alto-utility-private-key`, and `/plether/sepolia/kernel-canary-aa-origin-token`. The origin token is 64 lowercase hex characters. The API execution role may decrypt only the origin parameter; the Alto execution role may decrypt only executor and utility parameters. Both are restricted to SSM service access and exact parameter encryption contexts. The Nitro URL/token parameters `/plether/sepolia/app-rpc-url` and `/plether/sepolia/rpc-gateway-token` already exist and are referenced by ARN, without reading their values into Terraform state. Do not reuse live v0.8 Alto signing keys or origin token.

The key policy delegates explicit key management and recovery actions to authorized Plether IAM administrators, including `kms:PutKeyPolicy` for future repairs. Authorized IAM operators can provision and recover the three configured service parameters and the dedicated `/plether/sepolia/kernel-canary-deployer-private-key` through Singapore SSM before the ECS roles exist. Canary execution, task, and image-build roles are excluded from this operator access. Only the exact Alto and API execution roles have separate decrypt allowances for their own parameter contexts.

## Phase 2: dark service skeleton

After both image digests exist in the dedicated ECR repositories, keep `provision_image_build_resources=true`, `provision_paymaster_key=true`, and `provision_ssm_encryption_key=true` on every later plan. Set `provision_services=true`, `kernel_canary_api_image`, and `kernel_canary_alto_image` to exact account-local `@sha256:` URIs. Terraform looks up each digest and fails the plan if it does not exist. The read-only bindings pin live API task revision `plether-sepolia:154`, Alto `plether-sepolia-alto:12`, reconciler `plether-sepolia-aa-reconciler:72`, and the reviewed Sepolia network IDs in `services_variables.tf`. Recheck those metadata bindings if any live resource is replaced. The Nitro node is `i-04a6af37b4583a4ee`; verify it remains running and SSM Online before using it as a tunnel target.

Review that a skeleton plan adds only canary IAM roles, task definitions, security groups, internal Alto ALB/target groups, log group, and dormant ECS services. Keep `kernel_canary_alto_desired_count=0`, `kernel_canary_api_desired_count=0`, `kernel_canary_reconciler_desired_count=0`, and `kernel_canary_route_enabled=false`. The plan must not change or destroy live resources. The canary API and reconciler attach the existing narrow native-AA database client group, whose two outbound rules and RDS admission are checked by exact rule ID. The isolated state owns no RDS SG rule, so a later live core apply cannot revoke the canary database path.

Deploy and independently attest the fresh Kernel factory, validator, Staker, paymaster, and both EntryPoint v0.9 simulation contracts. Stake and deposit the new contracts as described in the qualification runbook. Apply the additive Kernel database migration before API startup, reviewing the non-concurrent funding index lock impact in a quiet window. The canary API sets `KERNEL_CANARY_API_MODE=true` and `API_BACKGROUND_WORKERS_ENABLED=false`, preventing a second live indexer or other generic database writer. Set `kernel_canary_simulators_attested`, `kernel_canary_db_schema_confirmed`, and `kernel_canary_profile_attested` only after their checks. Alto simulator auto-deployment remains off; set both reviewed simulator addresses explicitly.

## Private Nitro SafeMode check

Start only Alto with `kernel_canary_alto_desired_count=1` and temporarily set `kernel_canary_ssm_tunnel_enabled=true`; its SG gets the exact TCP 443 permission to the private Nitro gateway. The canary internal ALB then accepts TCP 80 from the canary API and the SSM-managed Nitro node SG. The node has outbound access to that ALB. Obtain `kernel_canary_alto_internal_rpc_url` from Terraform output, extract its hostname, and open a local port forward through the reviewed node:

```bash
aws --profile plether ssm describe-instance-information --region ap-southeast-1 \
  --filters Key=InstanceIds,Values=i-04a6af37b4583a4ee \
  --query 'InstanceInformationList[].PingStatus' --output text
aws --profile plether ssm start-session --region ap-southeast-1 \
  --target i-04a6af37b4583a4ee \
  --document-name AWS-StartPortForwardingSessionToRemoteHost \
  --parameters '{"host":["<internal-Alto-ALB-hostname>"],"portNumber":["80"],"localPortNumber":["8765"]}'
```

Use `http://127.0.0.1:8765` for `eth_supportedEntryPoints`, `eth_estimateUserOperationGas`, and a locally signed public `eth_sendUserOperation` smoke operation. Keep private wallet keys on the operator machine; do not place them in ECS task overrides, SSM session commands, or logs. Capture Alto evidence that its own v0.9 SafeMode trace reached the bearer-authenticated Nitro gateway, including duration and response size against the gateway's 8-second and 8-MiB limits. A direct gateway fetch does not satisfy this gate. Close the SSM session and set `kernel_canary_ssm_tunnel_enabled=false` after the smoke; the guard forbids opening the public route with the tunnel left active.

## API, wallet checks, and rollback

Start the API only after its profile and additive database gates. Confirm separately operated HTTPS services behind `/plether/sepolia/perps-rpc-url` and `/plether/sepolia/aa-reconciler-secondary-rpc-url` before starting the API or reconciler. Both tasks reject matching RPC hostnames at startup. Keep all URL credentials out of verification logs. The tasks use the external Perps URL's provider credentials. Only Alto receives the private Nitro bearer with its reviewed Nitro URL.

Before opening its public path, check the dark API task from an approved in-VPC probe. Then set `kernel_canary_nitro_safemode_qualified=true`, an exact HTTPS `kernel_canary_preview_origin` outside `plether.com` and its subdomains, and `kernel_canary_route_enabled=true`. The new listener rule uses priority 180 and matches only host `api.sepolia.plether.com` plus backend path `/api/aa/kernel/rpc`. The dedicated Pages preview Worker maps browser `/api/perps/v1/aa/kernel/rpc` to that path and supplies its separate origin token. Both ETH-paid and sponsored modes require nonempty exact external and Privy owner addresses in `kernel_canary_owners_json`. Public admission remains false. Verify live gas estimates, both wallet flows, and their included receipts before any broader rollout. Sponsored mode additionally requires the fresh paymaster profile and active canary reconciler.

To roll back, set `kernel_canary_route_enabled=false`, `kernel_canary_ssm_tunnel_enabled=false`, and both payment flags to false. This closes ingress and removes the API task role's paymaster KMS signing grant. Scale the canary API and Alto to zero. Keep the reconciler at `kernel_canary_reconciler_desired_count=1` with the attested Kernel and paymaster profiles until accepted operations and sponsorship reservations settle or expire. Then scale the reconciler to zero. Retain the additive database schema and signing keys for durable evidence and old-binary compatibility. The canary root never changes live v0.8 service definitions.
