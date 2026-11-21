# Dedicated Sepolia canary deployer

Use an isolated signer for [Arbitrum Sepolia Kernel deployments](./deploy-sepolia.md). A distinct deployer lets its deployment nonce advance independently of the live OrderRouter.

Preserve the historical Staker canary manifests and the public nonce reconciliation for the unrelated OrderRouter call. Start a separate manifest and use `kernel-sepolia-direct-v1` for the direct-factory deployment and its Pages preview. Bind the reviewed dedicated deployer and fresh direct-factory paymaster to that manifest. Recheck the code of any reused pinned CREATE2 contract and derive the simulator addresses from the exact reviewed image and utility address.

## Create the isolated signer

The [deployer provisioner](./provision-canary-deployer.mjs) targets only account `932542905614`, region `ap-southeast-1`, the already provisioned canary symmetric KMS key, and new SSM SecureString `/plether/sepolia/kernel-canary-deployer-private-key`. Its default mode is read-only. Explicit apply generates one key in process memory, writes it to SSM via CLI stdin, and prints only its public address. A failed or ambiguous SSM write must be inspected before any retry; the parameter is never overwritten.

```sh
aws --profile plether sts get-caller-identity
node scripts/kernel-qualification/provision-canary-deployer.mjs \
  --region ap-southeast-1 \
  --kms-key-arn arn:aws:kms:ap-southeast-1:932542905614:key/65a4abd7-44fb-46f3-821d-71dab7d4efe3
node scripts/kernel-qualification/provision-canary-deployer.mjs \
  --region ap-southeast-1 \
  --kms-key-arn arn:aws:kms:ap-southeast-1:932542905614:key/65a4abd7-44fb-46f3-821d-71dab7d4efe3 \
  --apply --confirm kernel-sepolia-dedicated-deployer
```

Record the printed public `deployerAddress`. It must differ from the old signer, sample owner, sponsor KMS signer, Alto utility/executor EOAs, and eventual external/Privy wallet owners. Keep the new signer key out of files, logs, CLI arguments, and Git.

## Fund on Arbitrum Sepolia

Use an independently funded Sepolia wallet on **Arbitrum Sepolia (421614)**. Fund the dedicated deployer with the reviewed paymaster deposit and a 0.5 ETH contract and simulator deployment reserve. A 0.25 ETH deposit requires 0.75 ETH before deployment. Fund the Alto utility, executor and ETH-paid Kernel accounts separately.

Record the public transaction hash and check the exact sender, recipient, amount, nonce and successful receipt with both independent RPCs at a common safe height. The [read-only funding attestation](./fund-dedicated-deployer.mjs) accepts an existing public record with `schemaVersion: 1`, `deployment`, `source`, `recipient`, decimal `valueWei`, decimal `nonce` and `hash`. Its public config identifies the same `deployment`, `sourceDeployer`, `dedicatedDeployer`, and decimal `valueWei`, plus `primaryRpc`. Supply the secondary provider through `KERNEL_SECONDARY_RPC_URL` as described in the deployment runbook.

```sh
node scripts/kernel-qualification/fund-dedicated-deployer.mjs \
  --config "$PUBLIC_FUNDING_CONFIG" --manifest "$PUBLIC_FUNDING_MANIFEST" --attest
```

## Deploy the direct-factory contracts

Prepare a new public config and a distinct manifest using `kernel-sepolia-direct-v1` and the direct profile's artifact pins. The deployment checks any existing Kernel, validator and simulator code, creates the sample account and paused paymaster as needed, and funds the configured EntryPoint deposit. Use the [scoped SSM signing procedure](./deploy-sepolia.md#funding-and-broadcast-gate) after reviewing the exact configuration.

Use `--attest` after both safe heads include every new receipt. The initial paymaster owner must match the dedicated deployer, which is the signer for any later, separately approved `unpause()` or `pause()` action. Keep the paymaster paused through contract attestation, managed Alto qualification and the ETH gas checks. Transfer ownership to the planned Safe as a separately reviewed post-canary action.
