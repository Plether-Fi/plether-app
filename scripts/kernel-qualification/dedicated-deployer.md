# Dedicated Sepolia canary deployer fallback

Use this path only if the existing Sepolia deployer continues submitting Perps orders while the [fresh Kernel deployment](./deploy-sepolia.md) is paused. The new signer avoids repeated nonce collisions for the remaining Staker, sample account, and paymaster transactions. It requires **one** 4.6 test ETH funding transaction from the existing Plether signer. No live Perps service is paused.

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

## Fund exactly 4.6 test ETH

Prepare a public-only JSON config at a new local path:

```json
{
  "deployment": "kernel-sepolia-canary-20261121",
  "sourceDeployer": "0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B",
  "dedicatedDeployer": "<public deployerAddress from provisioning>",
  "primaryRpc": "https://sepolia-rollup.arbitrum.io/rpc"
}
```

The [funding script](./fund-dedicated-deployer.mjs) requires two independent Sepolia RPCs, a zero-balance/code recipient, clean latest and pending source nonce on both RPCs, a 25-second stable-nonce observation, at least 1 ETH left in the existing signer after funding, and a 0.01 ETH maximum fee. It signs an EIP-1559 transfer with no tip and a capped gas estimate. Before sending, it fsyncs a **public-only** intent containing the exact signed transaction hash, then rechecks both nonces. The command sends at most one transaction; it never automatically retries after a race or uncertain RPC response. Its separate `--attest` command verifies exact sender, recipient, nonce, value, calldata, successful receipt, canonical safe block, and 4.6 ETH recipient balance increase on both RPCs. **The final nonce guard is not atomic with network submission: a newly sent OrderRouter transaction could still race and be replaced. Run the funding send only in a genuinely quiet or explicitly coordinated keeper window; otherwise use a separate funding source.** The existing Sepolia faucet signer holds only about 0.436 test ETH, below this transfer amount.

Supply the Alchemy RPC from the existing SSM parameter without echoing its credential. Use a new funding manifest path that does not exist yet:

```sh
set +x
KERNEL_SECONDARY_RPC_URL="$(aws --profile plether --region ap-southeast-1 ssm get-parameter \
  --name /plether/sepolia/alto-rpc-url --with-decryption --query Parameter.Value --output text)"
export KERNEL_SECONDARY_RPC_URL
node scripts/kernel-qualification/fund-dedicated-deployer.mjs \
  --config "$PUBLIC_FUNDING_CONFIG" --manifest "$NEW_PUBLIC_FUNDING_MANIFEST"
```

For the one-time send, source only the existing ignored Core signer environment in a subshell, as in the original deployment runbook:

```sh
(
  set +x
  set -e
  source "$PLETHER_CORE_SIGNER_ROOT/.env"
  test -n "${TEST_PRIVATE_KEY:-}"
  KERNEL_CANARY_FUNDING_PRIVATE_KEY="$TEST_PRIVATE_KEY"
  export KERNEL_CANARY_FUNDING_PRIVATE_KEY
  unset TEST_PRIVATE_KEY
  unset PRIVATE_KEY ETHERSCAN_API_KEY SEPOLIA_RPC_URL MAINNET_RPC_URL INFURA_RPC_URL
  unset TREASURY PYTH_UPDATE_DATA ARB_SEPOLIA_RPC_URL PYTH_API_KEY
  node scripts/kernel-qualification/fund-dedicated-deployer.mjs \
    --config "$PUBLIC_FUNDING_CONFIG" --manifest "$NEW_PUBLIC_FUNDING_MANIFEST" \
    --broadcast --confirm kernel-sepolia-canary-20261121
)
```

If this stops after writing an intent, **do not retry with that manifest or a new one** until both RPCs have been checked for its hash and the source nonce. A transaction may have reached the network even if the send call failed. If the hash is absent and an OrderRouter transaction consumed the exact nonce, record both providers' canonical receipt/target/selector/value and require the disputed block to be safe before creating any new funding attempt. Once the funding receipt is on the common safe chain:

```sh
node scripts/kernel-qualification/fund-dedicated-deployer.mjs \
  --config "$PUBLIC_FUNDING_CONFIG" --manifest "$NEW_PUBLIC_FUNDING_MANIFEST" --attest
```

## Resume contracts under the new signer

Copy the old **public config**, set only `deployer` to the new public address, and keep the same logical deployment ID, sponsor signer, utility address, policy, sample owner, artifact pins, and RPC hosts. Use a **fresh, distinct** contract manifest path. The deployment script sees the pinned CREATE2 code and simulators already present; it creates a new Staker owned by the dedicated signer, approves/stakes it, deploys the sample proxy and paused paymaster, deposits 2 ETH, and stakes 1 ETH in the paymaster. The abandoned old Staker is never bound to the new paymaster or canary profile.

Read the dedicated key only into the local process environment from SSM. Keep tracing off and never print the value:

```sh
(
  set +x
  set -e
  KERNEL_CANARY_DEPLOYER_PRIVATE_KEY="$(aws --profile plether --region ap-southeast-1 ssm get-parameter \
    --name /plether/sepolia/kernel-canary-deployer-private-key \
    --with-decryption --query Parameter.Value --output text)"
  export KERNEL_CANARY_DEPLOYER_PRIVATE_KEY
  node scripts/kernel-qualification/deploy-sepolia.mjs \
    --config "$NEW_PUBLIC_CONTRACT_CONFIG" --manifest "$NEW_PUBLIC_CONTRACT_MANIFEST" \
    --broadcast --confirm kernel-sepolia-canary-20261121
)
```

Use `--attest` after both safe heads include every new receipt. The final Staker and paymaster owners must equal the new deployer. The same SSM key is then the paymaster owner for the bounded canary's later `unpause()`/`pause()` actions; do not use the old shared Core signer for those. Keep the paymaster paused until Nitro SafeMode, live ETH gas, private API and wallet admission gates pass. Transfer ownership to the planned Safe as a separately reviewed post-canary action.
