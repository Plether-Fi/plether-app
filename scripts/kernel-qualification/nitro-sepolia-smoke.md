# Public Sepolia Nitro SafeMode smoke

Use [`nitro-sepolia-smoke.mjs`](./nitro-sepolia-smoke.mjs) only after the fresh Kernel, Staker, paymaster and simulator contracts have passed [`deploy-sepolia.mjs`](./deploy-sepolia.mjs) independent attestation and the **isolated** Alto canary is running. The default `plan` reads chain and bundler state. `send-eth` is the only command that transmits a UserOperation, and it requires an exact deployment confirmation, a matching throwaway owner signer and a new evidence file. No public send is part of preparing this harness.

## Access and SafeMode gate

The canary Alto service will be on its internal ALB, port 80, reachable only from the canary API/probe security group and the reviewed SSM-managed Nitro EC2 node security group. It has no browser route during this check. Run the script on the operator machine over controlled localhost SSM port forwards into the VPC. After Terraform outputs the exact internal ALB DNS name and the reviewed Nitro instance ID, use an SSM remote-host tunnel such as:

```sh
aws --profile plether ssm start-session \
  --target "$REVIEWED_NITRO_INSTANCE_ID" \
  --document-name AWS-StartPortForwardingSessionToRemoteHost \
  --parameters '{"host":["<canary-alto-internal-alb-dns>"],"portNumber":["80"],"localPortNumber":["18080"]}'
```

Replace the host placeholder with the exact Terraform output before opening the session. Point `KERNEL_SMOKE_ALTO_RPC_URL` to `http://127.0.0.1:18080`. An ECS probe may carry a **signed public** UserOperation JSON if a tunnel is unavailable, but never receive the owner private key in task overrides, secrets or logs. The owner key stays on the operator machine.

Before sending, verify the running Alto ECS task definition and service use the exact reviewed patched image digest, `ALTO_SAFE_MODE=true`, `ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION=false`, EntryPoint 0.9 only, predeployed attested v0.9/Pimlico simulator addresses, and `ALTO_DEPLOY_SIMULATIONS_CONTRACT=false`. Confirm its primary Nitro gateway bearer comes from the dedicated canary SSM reference. The read-only plan checks the private Nitro RPC and an independent RPC at one common safe block and requires both to agree on the code and account state. An accepted, signed first UserOperation through this Alto task is the live Nitro SafeMode execution check. The earlier standalone Nitro `debug_traceCall` capability probe remains separate evidence; this harness does not claim to reproduce its custom tracer.

Set RPC URLs in the local environment, never in CLI arguments or an evidence file:

```sh
export KERNEL_SMOKE_ALTO_RPC_URL="http://127.0.0.1:<alto-local-port>"
export KERNEL_SMOKE_NITRO_RPC_URL="http://127.0.0.1:<nitro-local-port>"
export KERNEL_SMOKE_SECONDARY_RPC_URL="<independent-https-sepolia-rpc>"
# KERNEL_SMOKE_NITRO_BEARER may be set locally if the Nitro gateway tunnel requires it.
```

The Alto and Nitro RPCs must be localhost SSM tunnels or private AWS ALBs over HTTP. For direct private ALB access, set `KERNEL_SMOKE_EXPECTED_ALTO_ALB_HOST` and `KERNEL_SMOKE_EXPECTED_NITRO_ALB_HOST` from the independently reviewed Terraform/AWS outputs; the script requires exact hostname matches. Direct Nitro also requires its bearer. The independent chain RPC must use HTTPS on a different host. RPC URLs cannot contain userinfo, a query or a fragment; the HTTPS provider's credential-bearing path remains supported. Keep the Alchemy URL and bearer out of task overrides and logs. Confirm AWS identity with `aws --profile plether sts get-caller-identity` before setting up an SSM session.

## Fresh ETH-paid first operation

Create a new throwaway EOA in the operator's secure local wallet and retain its private key locally. It must differ from the contract deployer, sacrificial sample owner, Alto utility and KMS sponsor signer; it must have no code or delegation. Use only its public address in `--owner`. The script uses the app's Kernel index-zero adapter and first-operation Staker factory data. Its sole call is a zero-value read of `EntryPoint.balanceOf(sender)`. This deploys the throwaway Kernel account without moving tokens or changing application state.

Run the default plan against the independently attested public deployment manifest:

```sh
node scripts/kernel-qualification/nitro-sepolia-smoke.mjs plan \
  --manifest "$KERNEL_CANARY_DEPLOYMENT_MANIFEST" \
  --owner "$THROWAWAY_OWNER_ADDRESS"
```

The plan checks chain ID `421614`, common safe-block hash, pinned EntryPoint/Kernel/Staker/paymaster/simulator code, both one-day stakes, owner EOA status, the derived undeployed account, Alto's single supported EntryPoint and live gas quote. It calls `eth_estimateUserOperationGas` in Alto SafeMode. An unfunded account uses a balance-only state override for an **advisory** estimate; this changes no chain state. The output is limited to public addresses, safe-block hash, gas limits and Wei amounts. If the account is unfunded, send ETH to the reported counterfactual `sender` from a separate operator wallet, then rerun the plan until the estimate is against actual funded state and `fundingShortfallWei` is zero. The script allows at most 12 million preverification gas, covering the recorded Nitro SafeMode estimate of 5,861,566, and separately caps planned prefund at 0.05 ETH. An initial 0.2 ETH test funding amount gives headroom, with leftover ETH recoverable through the owner after the check. Check that funding did not deploy code at the address.

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

The recovery verifier decodes the included UserOperation from canonical EntryPoint calldata on both RPCs. It checks its exact no-value call, Staker deployment data, absence of paymaster fields, and reviewed gas cap. Supply `--transaction-hash` with the public bundle hash if Alto's recent-receipt lookup has aged out and the evidence file lacks the hash. Check the task definition's SafeMode flag and correlate only public operation and transaction hashes in Nitro and Alto logs.

## Sponsored follow-up

Do not sign a paymaster envelope directly from this script. A sponsored authorization must come through the canary API's allowlisted preparation flow, dedicated KMS signer, reservation and budget checks. After the ETH/Nitro check, start the private canary API and reconciler, attest the paused and staked paymaster, unpause only for the bounded canary cohort, and submit a real sponsored operation through the API/UI. Keep the public route closed until the required wallet checks pass. Use the resulting **public** UserOperation hash to verify the included operation:

```sh
node scripts/kernel-qualification/nitro-sepolia-smoke.mjs verify-sponsored \
  --manifest "$KERNEL_CANARY_DEPLOYMENT_MANIFEST" \
  --owner "$SPONSORED_CANARY_OWNER_ADDRESS" \
  --sponsorship "$PUBLIC_CANARY_SPONSORSHIP_JSON" \
  --user-op-hash "$SPONSORED_USER_OPERATION_HASH"
```

The checker verifies the paymaster's pinned runtime hash and matches the successful event, UserOperation, signer, policy, cost cap, unpaused state, and exact EntryPoint deposit debit on both RPCs. It checks the 157-byte signed envelope, the account-code hash, the inclusion timestamp, the budget, and zero post-operation gas. Supply `--transaction-hash` with the public bundle hash if Alto no longer returns the receipt. If another paymaster operation or deposit changes that balance in the same block, reconcile the separate changes before repeating the check. The verifier prints public evidence and submits no transaction.

Use [`live-gas.mjs`](./live-gas.mjs) around the later external and Privy wallet approvals to capture live Nitro L1/L2 estimates and receipt deltas for **both** payment modes. This smoke checks the infrastructure's signed first operation before the browser route opens; it does not replace those real wallet flows.
