# Private Sepolia managed Alto smoke

Use [`nitro-sepolia-smoke.mjs`](./nitro-sepolia-smoke.mjs) after the `kernel-sepolia-direct-v1` factory, validator, paused paymaster and simulators have passed [deployment attestation](./deploy-sepolia.md) and the isolated Alto canary is running. The default `plan` reads chain and bundler state. The separate `send-eth` stage requires authorization for the exact deployment, a matching throwaway owner signer and a new evidence file.

## Access and validation configuration

Canary Alto is reachable through its internal ALB from the canary API client security group. Keep its browser route closed for this check. Use an approved short-lived in-VPC probe or an approved operator tunnel to reach that ALB. For a signed probe, sign locally and pass only the signed public UserOperation to the approved private probe. Keep the owner private key on the operator machine.

Verify that the running task uses the exact reviewed Alto v1.2.8 digest, `ALTO_SAFE_MODE=false`, `ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION=false`, `ALTO_EXPIRATION_CHECK=true`, EntryPoint 0.9 only, and the two attested simulator addresses with `ALTO_DEPLOY_SIMULATIONS_CONTRACT=false`. Check that its primary is the managed HTTPS Sepolia RPC bound from `/plether/sepolia/perps-rpc-url`. Use the managed primary and the independently operated security RPC for a common safe block. The signed first operation qualifies normal managed Alto validation and direct factory deployment. Prior Nitro custom-tracer runs belong to the historical Staker profile.

Set RPC URLs in the local environment, never in CLI arguments or an evidence file:

```sh
export KERNEL_SMOKE_ALTO_RPC_URL="http://127.0.0.1:<alto-local-port>"
export KERNEL_SMOKE_PRIMARY_RPC_URL="<reviewed-managed-https-sepolia-rpc>"
export KERNEL_SMOKE_SECONDARY_RPC_URL="<independent-https-sepolia-rpc>"
```

From an approved in-VPC probe, set `KERNEL_SMOKE_ALTO_RPC_URL` to the reviewed private ALB and set `KERNEL_SMOKE_EXPECTED_ALTO_ALB_HOST` to its exact hostname. Use distinct HTTPS hosts for the managed primary and independent chain RPC. Load credential-bearing provider paths into the environment with shell tracing disabled and keep them out of task overrides and logs. Leave `KERNEL_SMOKE_NITRO_BEARER` unset when using the managed endpoint. The legacy Nitro variables support the existing tunnel transport when authorized. Confirm the AWS identity with `aws --profile plether sts get-caller-identity` before AWS work.

## Fresh ETH-paid first operation

Create a throwaway EOA in the operator's secure local wallet and retain its key locally. It must differ from the contract deployer, sample owner, Alto utility and KMS sponsor signer, and have no code or delegation. Pass only its public address to `--owner`. The app's index-zero adapter puts the pinned KernelFactory and its direct deployment calldata in the first operation. The operation executes a zero-value `EntryPoint.balanceOf(sender)` read.

Run the default plan against the independently attested public deployment manifest:

```sh
node scripts/kernel-qualification/nitro-sepolia-smoke.mjs plan \
  --manifest "$KERNEL_CANARY_DEPLOYMENT_MANIFEST" \
  --owner "$THROWAWAY_OWNER_ADDRESS"
```

The plan checks chain ID `421614`, the common safe-block hash, pinned EntryPoint, Kernel, paymaster and simulator code, the owner EOA, the derived undeployed account, Alto's supported EntryPoint and the live `eth_estimateUserOperationGas` quote. A balance-only state override yields an advisory estimate for an unfunded account. The plan prints public addresses, safe block hash, gas limits and Wei values. For the actual ETH test, fund the counterfactual sender and rerun the plan until it estimates against funded state and `fundingShortfallWei` is zero. The script caps pre-verification gas at 12 million and prefund at 0.05 ETH. Check the address still has no code after funding.

For the first public send, inject the throwaway key only into the **local** process environment through the operator's approved secure wallet process. Disable shell tracing. The script checks the derived public address against `--owner` and writes a 0600 public-only intent before making `eth_sendUserOperation`:

```sh
(
  set +x
  set -e
  test -n "${KERNEL_SMOKE_OWNER_PRIVATE_KEY:-}"
  export KERNEL_SMOKE_OWNER_PRIVATE_KEY
  node scripts/kernel-qualification/nitro-sepolia-smoke.mjs send-eth \
    --manifest "$KERNEL_CANARY_DEPLOYMENT_MANIFEST" \
    --owner "$THROWAWAY_OWNER_ADDRESS" \
    --evidence "$NEW_ETH_SMOKE_EVIDENCE" \
    --confirm "$KERNEL_CANARY_DEPLOYMENT_ID"
)
```

Do not pass the key in a CLI argument or ECS task override. The evidence file holds only the public owner, sender, hash, gas quote and receipt fields; it never holds the key, signature, calldata, RPC URL or gateway recovery token. The send verifies the returned operation hash, one successful EntryPoint `UserOperationEvent`, nonce zero, no paymaster, account code/root/owner/implementation on both providers at inclusion, and exact debit of the throwaway account's ETH plus EntryPoint deposit against the event gas cost. It waits for both providers' safe heads to include the transaction. If the command exits after intent creation, **do not rerun `send-eth`**. Use its public hash with:

```sh
node scripts/kernel-qualification/nitro-sepolia-smoke.mjs verify-eth \
  --manifest "$KERNEL_CANARY_DEPLOYMENT_MANIFEST" \
  --owner "$THROWAWAY_OWNER_ADDRESS" \
  --evidence "$NEW_ETH_SMOKE_EVIDENCE"
```

The recovery verifier decodes the included UserOperation from canonical EntryPoint calldata on both RPCs. It checks the exact zero-value call, direct factory deployment data, ETH payer and reviewed gas cap. Supply the public bundle hash via `--transaction-hash` if Alto's recent-receipt lookup has aged out and the evidence file lacks the hash. Correlate public operation and transaction hashes in Alto logs.

## Sponsored follow-up

Obtain sponsored authorization through the canary API's allowlisted preparation flow, dedicated KMS signer, reservation and budget checks. After the managed Alto and ETH checks, attest the paused paymaster and its configured EntryPoint deposit. Unpause only during the separately approved bounded canary. Submit a real sponsored operation through the canary UI while general public admission stays closed. Use the resulting public UserOperation hash to verify the included operation:

```sh
node scripts/kernel-qualification/nitro-sepolia-smoke.mjs verify-sponsored \
  --manifest "$KERNEL_CANARY_DEPLOYMENT_MANIFEST" \
  --owner "$SPONSORED_CANARY_OWNER_ADDRESS" \
  --sponsorship "$PUBLIC_CANARY_SPONSORSHIP_JSON" \
  --user-op-hash "$SPONSORED_USER_OPERATION_HASH"
```

The checker verifies the paymaster's pinned runtime hash and matches the successful event, UserOperation, signer, policy, cost cap, unpaused state, and exact EntryPoint deposit debit on both RPCs. It checks the 157-byte signed envelope, the account-code hash, the inclusion timestamp, the budget, and zero post-operation gas. Supply `--transaction-hash` with the public bundle hash if Alto no longer returns the receipt. If another paymaster operation or deposit changes that balance in the same block, reconcile the separate changes before repeating the check. The verifier prints public evidence and submits no transaction.

Use [`live-gas.mjs`](./live-gas.mjs) around the later external and Privy wallet approvals to record live Arbitrum gas estimates and receipt deltas for both payment modes. Qualify both wallets before rollout.
