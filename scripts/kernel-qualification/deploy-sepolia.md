# Direct-factory Sepolia Kernel canary contracts

Use [`deploy-sepolia.mjs`](./deploy-sepolia.mjs) for `kernel-sepolia-direct-v1` on Arbitrum Sepolia. The default command verifies artifacts and reads two independent RPCs at a common safe block. Broadcasting requires the matching dedicated canary signer, `--broadcast`, and `--confirm kernel-sepolia-direct-v1`. Keep the canary services stopped during contract attestation. Start the private Alto canary only after attestation and review. Keep public admission closed through the wallet checks.

## Inputs and provenance

| Input | Required source |
| --- | --- |
| Kernel repo | Clean checkout at `c960b42d2ed4adb0d5328f6e762962debdf8e57a`, with `releases/v0.4.0.json` and its dependencies. The script verifies the release's canonical CREATE2 addresses and creation hashes. |
| Validator repo | Clean checkout of `kernel-7579-plugins` at `332deed6eeef3d6279cde50aa1d51eff53728bd4`, including pinned dependencies. Build is reproduced with Solidity `0.8.30+commit.73712a01`; the script checks creation and runtime hashes. |
| Core repo | Build `packages/perps-aa` at the direct-factory `corePaymaster.commit` in [pins.json](./pins.json), using Foundry and the pinned Solidity `0.8.35+commit.47b9dedd`. Pass the resulting `PletherKernelVerifyingPaymaster.json`. The script checks its source, compiler, artifact metadata and bytecode. RPC modes require the exact clean Core checkout. Before a broadcast or rebroadcast, record a reviewed `refs/remotes/origin/...` ref containing the pinned commit in `corePaymaster.reviewedRef`. |
| Alto artifacts | Extract `EntryPointSimulations09.json` and `PimlicoSimulations.json` from the exact reviewed Alto v1.2.8 candidate image. The script pins their creation and runtime templates, including the immutable layout. |
| Signers | Use the [dedicated canary deployer](./dedicated-deployer.md). Configure its public EOA address, the separate Alto utility address and the separate paymaster sponsor KMS address. |

The direct paymaster's nine-argument constructor takes `entryPoint`, `owner`, `initialSponsorSigner`, `maxSponsoredCost`, `policyId`, `approvedAccountCodeHash`, `accountFactory`, `accountImplementation`, and `rootValidator` in that order. A new account's first UserOperation calls the pinned KernelFactory through EntryPoint 0.9. The factory and paymaster use zero stake. The paymaster’s EntryPoint deposit funds sponsored gas.

The script deploys the v0.9 and Pimlico simulators before canary Alto starts. It computes each CREATE2 address using `keccak256(Alto utility address)` as the salt and sends the transactions from the approved contract deployer. Set `ALTO_DEPLOY_SIMULATIONS_CONTRACT=false` and give Terraform the two attested addresses. The candidate image and deployed simulators must match the same pins.

An operator can extract local artifacts without starting Alto or contacting a chain:

```sh
docker run --rm --network none --entrypoint cat "$REVIEWED_ALTO_IMAGE" \
  /app/src/contracts/EntryPointSimulations.sol/EntryPointSimulations09.json > "$ENTRYPOINT09_SIMULATION_ARTIFACT"
docker run --rm --network none --entrypoint cat "$REVIEWED_ALTO_IMAGE" \
  /app/src/contracts/PimlicoSimulations.sol/PimlicoSimulations.json > "$PIMLICO_SIMULATION_ARTIFACT"
```

Use `--check-artifacts` to verify the candidate digest’s simulator files and the exact Core build inputs before using either RPC. The v1.2.8 simulators must be verified separately from the earlier v1.2.7 Staker candidate.

## Public configuration

Prepare a local JSON file with the following shape. Use the operator's own pinned checkouts and build artifacts. Supply `KERNEL_SECONDARY_RPC_URL` through the environment when the secondary URL contains a credential.

```json
{
  "deployment": "kernel-sepolia-direct-v1",
  "primaryRpc": "https://sepolia-rollup.arbitrum.io/rpc",
  "kernelRepo": "/absolute/path/to/zerodev-kernel",
  "validatorRepo": "/absolute/path/to/kernel-7579-plugins",
  "solc": "/absolute/path/to/solc-0.8.30",
  "coreRepo": "/absolute/path/to/plether-core",
  "paymasterArtifact": "/absolute/path/to/plether-core/packages/perps-aa/out/PletherKernelVerifyingPaymaster.sol/PletherKernelVerifyingPaymaster.json",
  "entryPoint09SimulationArtifact": "/absolute/path/to/EntryPointSimulations09.json",
  "pimlicoSimulationArtifact": "/absolute/path/to/PimlicoSimulations.json",
  "deployer": "<dedicated canary deployer EOA address>",
  "sampleOwner": "<fresh throwaway EOA address>",
  "altoUtilityAddress": "<new canary Alto utility public address>",
  "sponsorSigner": "<new canary paymaster KMS public address>",
  "policyId": "<reviewed 32-byte policy id>",
  "maxSponsoredCostWei": "10000000000000000",
  "paymasterDepositWei": "250000000000000000"
}
```

Replace every angle-bracket value before running. The sample owner must be a separate throwaway EOA with no deployed or delegated code, distinct from the deployer, Alto utility signer, KMS sponsor signer, external canary wallet, and Privy canary wallet. It needs no ETH and never signs an operation. `externalCanaryOwner` and `privyCanaryOwner` may be added to this config once those wallets connect; the script then checks they are distinct EOAs and differ from the sample owner. Do not deploy the sample proxy for either real canary wallet: their first UserOperation must deploy their own account during the wallet-flow check.

The configured deployer initially owns the new paymaster, which is created **paused**. The deployment script leaves it paused. A later ownership transfer to a Safe requires separate review and an updated attestation expectation.

Load a reviewed secondary RPC without echoing the URL. Its operator and host must differ from the configured primary. This example uses the canary's managed RPC setting as the independent observer for the public Arbitrum primary in the example config. Verify both provider identities before using the pair.

```sh
aws --profile plether sts get-caller-identity
(
  set +x
  set -eu
  KERNEL_SECONDARY_RPC_URL="$(aws --profile plether --region ap-southeast-1 ssm get-parameter \
    --name /plether/sepolia/perps-rpc-url --with-decryption --query Parameter.Value --output text)"
  export KERNEL_SECONDARY_RPC_URL
  node scripts/kernel-qualification/deploy-sepolia.mjs --config "$PUBLIC_CONFIG" --check-artifacts
  node scripts/kernel-qualification/deploy-sepolia.mjs --config "$PUBLIC_CONFIG"
)
```

The second RPC must serve contract state at an exact numeric safe block. The script stops on unavailable state or disagreement between the two providers.

The read-only plan reports the common safe block, exact code status, derived simulation addresses, and the deployer's independently compared balance. Derive the simulator addresses using the actual canary utility address and exact v1.2.8 artifacts. Preserve the earlier Staker deployment manifest as historical evidence.

## Funding and broadcast gate

Fund the deployer for the outstanding paymaster deposit and a 0.5 ETH contract and simulator gas reserve. The example 0.25 ETH deposit requires 0.75 ETH before deployment. Verify that the deployer has no pending transaction. Fund the Alto executor and utility separately for bundling. Fund each ETH-paid wallet's counterfactual Kernel account before its first operation. A sponsored account can start unfunded. During sponsorship, keep the EntryPoint deposit above the configured `minDepositWei` floor.

After recording the public utility, sponsor, deployer and sample owner addresses, image digest and deployment window, run the read-only plan. Use a clean operator shell and retrieve only the dedicated deployer SSM SecureString and the approved secondary RPC. The AWS identity must have permission to decrypt those parameters.

```sh
aws --profile plether sts get-caller-identity
(
  set +x
  set -eu
  KERNEL_SECONDARY_RPC_URL="$(aws --profile plether --region ap-southeast-1 ssm get-parameter \
    --name /plether/sepolia/perps-rpc-url --with-decryption --query Parameter.Value --output text)"
  KERNEL_CANARY_DEPLOYER_PRIVATE_KEY="$(aws --profile plether --region ap-southeast-1 ssm get-parameter \
    --name /plether/sepolia/kernel-canary-deployer-private-key \
    --with-decryption --query Parameter.Value --output text)"
  KERNEL_SECONDARY_RPC_URL="$KERNEL_SECONDARY_RPC_URL" \
    KERNEL_CANARY_DEPLOYER_PRIVATE_KEY="$KERNEL_CANARY_DEPLOYER_PRIVATE_KEY" \
    node scripts/kernel-qualification/deploy-sepolia.mjs \
      --config "$PUBLIC_CONFIG" --manifest "$PUBLIC_MANIFEST" \
      --broadcast --confirm "$DEPLOYMENT_ID"
)
```

Keep the key out of command arguments, output, JSON and operator files.

Before constructing a wallet client, the script verifies that the signer resolves to `deployer` and checks chain ID `421614`, canonical EntryPoint and SenderCreator code, the CREATE2 deployer, artifact pins, both safe-block responses, owner EOAs, funding and pending nonce. It deploys the missing pinned Kernel contracts and simulators, the sample proxy and a fresh paused direct-factory paymaster, then funds the configured deposit. A public-only 0600 manifest records the action, nonce and locally computed signed transaction hash before each send. The receipt is recorded after the providers agree. On restart, the script recovers a confirmed paymaster creation from its transaction receipt and checks the deployer's latest and pending nonce before another send. An unresolved intent or unexpected nonce stops the run. A clean resume needs only the remaining deposit and the gas reserve.

### Recover a recorded deployment hash

If a send times out, the process stops, or the transaction reverts, retain the public manifest and use the recorded transaction hash to reconcile. Both independent RPCs must confirm the exact reviewed deployment action at that nonce and its canonical receipt at a common safe height:

```sh
node scripts/kernel-qualification/deploy-sepolia.mjs \
  --config "$PUBLIC_CONFIG" --manifest "$PUBLIC_MANIFEST" \
  --reconcile-inflight --confirm "$DEPLOYMENT_ID"
```

A success completes the reported action. A revert records the receipt, advances the nonce and leaves the action pending for the next approved broadcast. If neither provider has mined a newly journaled transaction and both providers still report its exact nonce as available, use the same scoped signing procedure above with `--rebroadcast-inflight --confirm "$DEPLOYMENT_ID"` in place of the broadcast arguments. The command reconstructs the original EIP-1559 transaction, verifies its signature produces the original hash, and sends only that transaction with the original nonce and fees. Use `--reconcile-inflight` after it reaches the common safe block. Legacy intents without recorded gas and fee fields support mined-hash reconciliation only. Retain those intents for operator review if they remain unmined.

### Reconcile an external deployer nonce

Use the original reviewed script revision and config to reconcile a historical manifest from the former shared Sepolia deployer. If an old Perps order consumed an `inFlight` nonce without a recorded transaction hash, identify the transaction at that exact nonce and verify it independently. The reconciliation command accepts successful zero-value `executeOrder(uint64,bytes[])` calls to the reviewed Sepolia OrderRouter. Pass explicit transaction hashes in nonce order beginning at `expectedNonce`:

```bash
node scripts/kernel-qualification/deploy-sepolia.mjs \
  --config "$PUBLIC_CONFIG" --manifest "$PUBLIC_MANIFEST" \
  --reconcile-external-nonces \
  --external-tx 0xFIRST_HASH --external-tx 0xNEXT_HASH \
  --confirm "$DEPLOYMENT_ID"
```

This command never reads a private key or broadcasts. It requires two independent RPCs to agree on each transaction, successful receipt, sender, target, selector, value, nonce, and canonical block hash. The disputed `inFlight` nonce must also be included in both RPCs' common safe block before the command clears the intent. Later explicitly supplied order hashes can be reconciled at the confirmed tip, since the live order executor may keep moving ahead of the safe head. The command advances the checkpoint only through supplied hashes, records the public evidence, and may be repeated for a later gap. Both RPCs must agree on the latest nonce and agree on the pending nonce; pending orders may be ahead during reconciliation. Keep the reconciled manifest as historical evidence and use a fresh manifest for a [dedicated deployer](./dedicated-deployer.md).

Run `--attest` after both RPCs' safe heads include the deployment receipts. It checks source pins, exact code, factory implementation, the sample account's root, owner and implementation, paymaster immutables and storage, both simulation runtimes, paused state, zero stake, deposit and paymaster deployment block hash. Use the independently attested `profile` for `kernel_canary_profile_json`, the simulator addresses for `kernel_canary_entrypoint_simulation_contract_v9` and `kernel_canary_pimlico_simulation_contract`, and the new paymaster's fields for `kernel_canary_sponsorship_json`.

## Activation after later gates

Keep the paymaster paused while contract attestations, managed Alto qualification and ETH-paid live gas checks run. The canary route and payment flags require `kernel_canary_managed_alto_qualified=true` and `kernel_canary_managed_alto_qualified_image` matching the exact reviewed Alto ECR digest. Sponsored wallet checks require a separately approved bounded canary with general Kernel admission closed. Before activation, run `--attest` and confirm the KMS sponsor signer, policy budget, deposit floor and canary owner allowlist. The owner-only onchain action for that approved stage is `PletherKernelVerifyingPaymaster.unpause()` at the new manifest's paymaster address. From the reviewed owner signing route, one interactive Foundry form is:

```sh
cast send "$KERNEL_PAYMASTER" 'unpause()' --interactive --rpc-url "$PRIMARY_RPC"
```

Confirm the transaction succeeded, then read `paused()` from both independent RPCs at a common safe block. Admit only the two canary owners for sponsored tests. Run the external and Privy wallet flows in both payment modes and record live estimates and receipts before considering wider rollout. The pre-activation `--attest` command requires a paused paymaster. Capture the post-activation pause state and deposit separately. If the canary fails, pause the canary paymaster, disable canary admission and stop the canary Alto service.
