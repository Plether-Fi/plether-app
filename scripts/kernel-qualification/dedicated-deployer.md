# Dedicated Sepolia canary deployer

Use an isolated signer for [Arbitrum Sepolia Kernel deployments](./deploy-sepolia.md). A distinct deployer lets its deployment nonce advance independently of the live OrderRouter.

The first canary manifest records eight successful contract/simulator/Staker/factory-approval transactions and a disputed stake nonce that was later verified as an unrelated OrderRouter call. Preserve that manifest and its public nonce reconciliation evidence. Before abandoning the old Staker, independently recheck its EntryPoint `getDepositInfo` on both RPCs and require zero stake and deposit; the last check showed both zero. No paymaster was deployed from that manifest. The old Staker must not be bound to the final canary profile. Never reuse or overwrite the old manifest file.

The same logical `kernel-sepolia-canary-20261121` deployment ID can be retained for the staged Pages preview if the **new** contract manifest uses a distinct path and binds the new deployer address. Only publish the new manifest's independently attested profile and sponsorship JSON. Its pinned CREATE2 Kernel, validator, and simulator contracts already exist, and their addresses/code are independent of the EOA that sent the deployment transaction. The script validates all code again before using it.

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

Use an independently funded Sepolia wallet and select **Arbitrum Sepolia (421614)**. Fund the dedicated deployer with the two 1 ETH stakes, the reviewed paymaster deposit, and a 0.5 ETH contract and simulator deployment reserve. A 0.25 ETH deposit requires 2.75 ETH before deployment. Alto utility and executor balances, as well as balances for the ETH-paid Kernel accounts, are separate.

Record the public transaction hash and check the exact sender, recipient, amount, nonce and successful receipt with both independent RPCs at a common safe height. The [read-only funding attestation](./fund-dedicated-deployer.mjs) accepts an existing public record with `schemaVersion: 1`, `deployment`, `source`, `recipient`, decimal `valueWei`, decimal `nonce` and `hash`. Its public config identifies the same `deployment`, `sourceDeployer`, `dedicatedDeployer`, and decimal `valueWei`, plus `primaryRpc`. Supply the secondary provider through `KERNEL_SECONDARY_RPC_URL` as described in the deployment runbook.

```sh
node scripts/kernel-qualification/fund-dedicated-deployer.mjs \
  --config "$PUBLIC_FUNDING_CONFIG" --manifest "$PUBLIC_FUNDING_MANIFEST" --attest
```

## Resume contracts under the new signer

Copy the previous public config and set the dedicated deployer, retaining the logical deployment ID and reviewed artifact pins. Use a fresh, distinct contract manifest. The script rechecks any existing Kernel, validator and simulator code and creates a new Staker, sample account and paused paymaster as needed. Stake and deposit amounts come from the reviewed config. Use the [scoped SSM signing procedure](./deploy-sepolia.md#funding-and-broadcast-gate) to submit the deployment.

Use `--attest` after both safe heads include every new receipt. The final Staker and paymaster owners must equal the new deployer. The same SSM key is then the paymaster owner for the bounded canary's later `unpause()`/`pause()` actions; do not use the old shared Core signer for those. Keep the paymaster paused until Nitro SafeMode, live ETH gas, private API and wallet admission gates pass. Transfer ownership to the planned Safe as a separately reviewed post-canary action.
