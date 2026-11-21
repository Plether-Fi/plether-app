# Fresh Sepolia Kernel canary contracts

Use [`deploy-sepolia.mjs`](./deploy-sepolia.mjs) for the new Arbitrum Sepolia deployment. It does not change the live SimpleAccount stack or the local [`run.mjs`](./run.mjs) qualification chain. By default it only verifies artifacts and reads two RPCs at one common safe block. The broadcast path requires a matching injected Plether signer, `--broadcast`, and `--confirm` with the deployment ID. Keep all Kernel admission flags and Alto canary desired count at zero until the chain evidence and later wallet checks pass.

## Inputs and provenance

| Input | Required source |
| --- | --- |
| Kernel repo | Clean checkout at `c960b42d2ed4adb0d5328f6e762962debdf8e57a`, with `releases/v0.4.0.json` and its dependencies. The script verifies the release's canonical CREATE2 addresses and creation hashes. |
| Validator repo | Clean checkout of `kernel-7579-plugins` at `332deed6eeef3d6279cde50aa1d51eff53728bd4`, including pinned dependencies. Build is reproduced with Solidity `0.8.30+commit.73712a01`; the script checks creation and runtime hashes. |
| Core repo | Checkout of `plether-core` at the reviewed Staker-aware paymaster commit `469c40dd5757041dd57cafc35d0feb01d77a1aa2`. Build `packages/perps-aa` with Foundry and pass its `PletherKernelVerifyingPaymaster.json` artifact. The script checks the source Keccak, compiler, artifact source metadata and creation-code hash. |
| Alto artifacts | Extract `EntryPointSimulations09.json` and `PimlicoSimulations.json` from the exact patched Alto image selected for the canary. The script pins both creation-code hashes, the Pimlico runtime hash, and the v0.9 runtime template outside its address-dependent immutable slots. |
| Signers | Plether's existing Sepolia deployer is `0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B`. Its address was derived from the existing ignored Core `TEST_PRIVATE_KEY` without printing the key, matching the checked-in release manifest. Use that local Core signer through the secure handoff below; never use live Alto executor/utility SSM entries or the legacy paymaster KMS key. The canary Alto utility address and paymaster sponsor KMS address are public config fields. |

The pinned Staker uses generic ZeroDev release creation bytecode with the configured Plether deployer as constructor owner. Do not deploy the release's ZeroDev-owned Staker `init_code`. The script pins generic creation hash `0x89f185a3ea592ce85bf90f573b1864317505fa4d8d0906098645e1182f15b216`, release initialization hash `0x15ffa4371818c1b8e2ce98d157e1d6c0c2e230c7d473af14ddb621cd4cb09bae`, and locally reproduced runtime hash `0xc8c723ed62cabf8190cb76172e38d6d6c2e0b2b4d031fdcb89543d9b2682ced5`.

The script deploys the v0.9 and Pimlico simulators *before* canary Alto starts. It computes each CREATE2 address with salt `keccak256(Alto utility address)`, exactly as Alto does, but sends their deployment transactions from the approved contract deployer. Alto stays configured with `ALTO_DEPLOY_SIMULATIONS_CONTRACT=false`; Terraform receives the two attested simulator addresses. The patched image must contain the same pinned artifacts. No Alto service startup should silently deploy them.

An operator can extract local artifacts without starting Alto or contacting a chain:

```sh
docker run --rm --network none --entrypoint cat plether-alto-kernel-safemode:local \
  /app/src/contracts/EntryPointSimulations.sol/EntryPointSimulations09.json > "$ENTRYPOINT09_SIMULATION_ARTIFACT"
docker run --rm --network none --entrypoint cat plether-alto-kernel-safemode:local \
  /app/src/contracts/PimlicoSimulations.sol/PimlicoSimulations.json > "$PIMLICO_SIMULATION_ARTIFACT"
```

Use an image digest from the candidate build during rollout, and inspect the exact artifact hashes with `--check-artifacts`. The local image used to test this path was `sha256:8397e4091316abf5857f63560576804144ffc09212235df2e8e1e23a983b0a6c` on 2026-11-20. The creation hashes checked by the script are `0xf14a39f60bb7cad29bee4f2e4459ce0ab412e2d8f29205575881cf2cf500a937` (v0.9) and `0xfdb3e336f78a7bcb01c7b3d1dd930f939e15ea17f00b763eda06700687c6f94d` (Pimlico).

## Public configuration

Prepare a local JSON file with the following shape. The paths are to the operator's own pinned checkouts/build artifacts; none of the temporary paths used during qualification are part of the procedure. `secondaryRpc` may be omitted when `KERNEL_SECONDARY_RPC_URL` is supplied through the environment. That is preferable for the existing Alchemy URL because it contains a credential. Do not commit a config containing private RPC credentials.

```json
{
  "deployment": "kernel-sepolia-canary-20261120",
  "primaryRpc": "https://sepolia-rollup.arbitrum.io/rpc",
  "kernelRepo": "/absolute/path/to/zerodev-kernel",
  "validatorRepo": "/absolute/path/to/kernel-7579-plugins",
  "solc": "/absolute/path/to/solc-0.8.30",
  "coreRepo": "/absolute/path/to/plether-core",
  "paymasterArtifact": "/absolute/path/to/plether-core/packages/perps-aa/out/PletherKernelVerifyingPaymaster.sol/PletherKernelVerifyingPaymaster.json",
  "entryPoint09SimulationArtifact": "/absolute/path/to/EntryPointSimulations09.json",
  "pimlicoSimulationArtifact": "/absolute/path/to/PimlicoSimulations.json",
  "deployer": "0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B",
  "sampleOwner": "<fresh throwaway EOA address>",
  "altoUtilityAddress": "<new canary Alto utility public address>",
  "sponsorSigner": "<new canary paymaster KMS public address>",
  "policyId": "<reviewed 32-byte policy id>",
  "maxSponsoredCostWei": "50000000000000000",
  "paymasterDepositWei": "2000000000000000000"
}
```

Replace every angle-bracket value before running. The sample owner must be a separate throwaway EOA with no deployed or delegated code, distinct from the deployer, Alto utility signer, KMS sponsor signer, external canary wallet, and Privy canary wallet. It needs no ETH and never signs an operation. `externalCanaryOwner` and `privyCanaryOwner` may be added to this config once those wallets connect; the script then checks they are distinct EOAs and differ from the sample owner. Do not deploy the sample proxy for either real canary wallet: their first UserOperation must deploy their own account during the wallet-flow check.

The initial paymaster owner and Staker owner are the configured deployer so the same approved signer can perform both one-day stakes. The paymaster is created **paused** and remains paused after this script. A later ownership transfer to a Safe is a separate reviewed administrative operation; update the attestation expectation before doing that.

Use the named AWS profile and the correct Sepolia region to supply the second RPC without echoing it:

```sh
aws --profile plether sts get-caller-identity
set -e
KERNEL_SECONDARY_RPC_URL="$(aws --profile plether --region ap-southeast-1 ssm get-parameter \
  --name /plether/sepolia/alto-rpc-url --with-decryption --query Parameter.Value --output text)"
export KERNEL_SECONDARY_RPC_URL
node scripts/kernel-qualification/deploy-sepolia.mjs --config "$PUBLIC_CONFIG" --check-artifacts
node scripts/kernel-qualification/deploy-sepolia.mjs --config "$PUBLIC_CONFIG"
unset KERNEL_SECONDARY_RPC_URL
```

The second RPC must serve contract state at an exact numeric safe block. The free PublicNode endpoint advertised safe blocks but rejected numeric historical state; dRPC passed once then hit free-plan errors. The existing Plether Alchemy SSM RPC passed an exact common-block read with the official Arbitrum endpoint. Any RPC disagreement or unavailable state aborts qualification.

The no-broadcast plan reports the common safe block, exact code status, derived simulation addresses, and the deployer's independently compared balance. A 2026-11-20 read-only check at block `331180351` (`0x4d694849d56fa260ee54f6f6304271b0005083228769489fe0cc17fff550e176`) found the pinned Kernel contracts and both derived simulators absent and Plether's deployer balance `6.732529508238720373 ETH` on both providers. Those derived simulator addresses were based on a *placeholder utility address*; recalculate with the actual new utility address before any broadcast.

## Funding and broadcast gate

With the default 2 ETH paymaster deposit, the script requires at least **4.5 ETH** in the deployer EOA immediately before any transaction: 1 ETH Staker stake, 1 ETH paymaster stake, 2 ETH deposit, and a 0.5 ETH gas reserve for all contract and simulator deployments. It also requires no pending deployer transaction. The observed 6.73 ETH balance passed this minimum on 2026-11-20, but recheck at broadcast time. This is *only* contract deployment funding. Fund the new canary Alto executor and utility EOAs separately for bundling, and fund each ETH-paid wallet's counterfactual Kernel account before its own first paid UserOperation. The sponsored account can remain unfunded. Keep the paymaster deposit above the separate `minDepositWei` budget floor after sponsorship gas starts; the initial deposit is not a perpetual floor.

After the actual public utility/sponsor addresses, throwaway sample owner, image and deployment window are established, run the default read-only plan again. Confirm the deployer balance still covers the remaining stakes, deposit and 0.5 ETH gas reserve; confirm its pending and latest nonces match. The script repeats both checks immediately before its first send. Core's `packages/perps/DEPLOYMENT.md` uses `TEST_PRIVATE_KEY` from a local ignored environment file. Set `PLETHER_CORE_SIGNER_ROOT` to the existing checkout that holds that `.env`; the config's `coreRepo` may instead point to the reviewed paymaster-commit checkout and artifact. Source only the existing signer's `.env` in a subshell, map the key to the deploy script's input, and let the script verify its public address matches `deployer` before broadcasting:

```sh
(
  set +x
  set -e
  set -a
  source "$PLETHER_CORE_SIGNER_ROOT/.env"
  set +a
  test -n "${TEST_PRIVATE_KEY:-}"
  KERNEL_CANARY_DEPLOYER_PRIVATE_KEY="$TEST_PRIVATE_KEY"
  export KERNEL_CANARY_DEPLOYER_PRIVATE_KEY
  unset TEST_PRIVATE_KEY
  node scripts/kernel-qualification/deploy-sepolia.mjs \
    --config "$PUBLIC_CONFIG" \
    --manifest "$PUBLIC_MANIFEST" \
    --broadcast --confirm "$DEPLOYMENT_ID"
)
```

Do not pass the key on the command line, print it, or put it in the JSON config or manifest. The subshell discards the sourced environment when it exits. This is a future broadcast procedure; the current qualification remains read-only.

The script verifies the supplied key resolves to `deployer`, verifies chain ID `421614`, canonical EntryPoint/SenderCreator/CREATE2 code, pinned source and artifacts, both independent safe-block responses, the EOA owners, live deployer funding and pending nonce **before constructing a wallet client**. It then deploys missing pinned Kernel contracts, validator, simulators, Plether-owned Staker, sample proxy, and paused Staker-aware Core paymaster. It approves the factory from the Staker, stakes the Staker and paymaster 1 ETH with a one-day unstake delay, and deposits the configured amount to the paymaster. It never calls `unpause`. A public-values-only 0600 manifest records each nonce intent before a send, then its hash and confirmed receipt. On restart, a confirmed Staker/paymaster creation is recovered from its transaction receipt. Before each send, the script checks the deployer's latest and pending nonce again. An unresolved intent or unexpected deployer nonce stops before another send. The funding gate charges only remaining stake/deposit obligations plus the gas reserve on a clean resume.

### Reconcile an external deployer nonce

The existing Sepolia deployer also runs Perps orders. If that process consumes a nonce while this script is running, stop the deployment and retain the manifest. For an `inFlight` intent without a transaction hash, identify the transaction that consumed that exact nonce and verify it independently. The reconciliation command only accepts successful zero-value `executeOrder(uint64,bytes[])` calls to the reviewed Sepolia OrderRouter. Pass **explicit transaction hashes in nonce order**, beginning at `expectedNonce`:

```bash
node scripts/kernel-qualification/deploy-sepolia.mjs \
  --config "$PUBLIC_CONFIG" --manifest "$PUBLIC_MANIFEST" \
  --reconcile-external-nonces \
  --external-tx 0xFIRST_HASH --external-tx 0xNEXT_HASH \
  --confirm "$DEPLOYMENT_ID"
```

This command never reads a private key or broadcasts. It requires two independent RPCs to agree on each transaction, successful receipt, sender, target, selector, value, nonce, and canonical block hash. The disputed `inFlight` nonce must also be included in both RPCs' common safe block before the command clears the intent. Later explicitly supplied order hashes can be reconciled at the confirmed tip, since the live order executor may keep moving ahead of the safe head. The command advances the checkpoint only through supplied hashes, records the public evidence, and may be repeated for a later gap. Both RPCs must agree on the latest nonce and agree on the pending nonce; pending orders may be ahead during reconciliation. Before resuming, `--broadcast` rechecks every recorded hash and requires the checkpoint to equal the current latest and pending nonce. If the order executor is still using the deployer, this may require a quiet deployment window or a dedicated deployer; no nonce is skipped automatically. Do not edit the manifest by hand or retry `--broadcast` while `inFlight` remains unresolved.

Run `--attest` after both RPCs' safe heads include the deployment receipts. It checks source pins, exact code, factory implementation, Staker owner/approval/stake, the sample account's root/owner/implementation, all paymaster immutables and storage, both simulation runtimes, paused state, deposit/stake, and paymaster deployment block hash. The manifest contains an exact `profile` object for `kernel_canary_profile_json`, simulator addresses for `kernel_canary_entrypoint_simulation_contract_v9` and `kernel_canary_pimlico_simulation_contract`, and paymaster fields for the separate `kernel_canary_sponsorship_json`. Only use those values after independent attestation. Never use the live v0.8 account/paymaster bindings for the canary.

## Activation after later gates

Keep the paymaster paused while contract attestations, Nitro SafeMode and ETH-paid live gas checks run. Sponsored wallet checks require an explicit **bounded canary** activation; keep general Kernel admission closed during those checks. Before activation, run `--attest` and confirm the KMS sponsor signer, policy budget, deposit floor, Alto stake checks, and canary owner allowlist match the candidate configuration. The exact owner-only onchain action is `PletherKernelVerifyingPaymaster.unpause()` at the manifest's paymaster address. From the approved owner signing route, one interactive Foundry form is:

```sh
cast send "$KERNEL_PAYMASTER" 'unpause()' --interactive --rpc-url "$PRIMARY_RPC"
```

Confirm the transaction succeeded, then read `paused()` from both independent RPCs at a common safe block before admitting only the two canary owners for sponsored test operations. Run the external and Privy wallet flows in both payment modes, record live estimates and receipts, and only then consider wider rollout. The pre-activation `--attest` command intentionally rejects an unpaused paymaster; capture the post-activation paused state, stake and deposit as separate rollout evidence. A failed canary can be stopped by the owner-only `pause()` call, followed by disabling the canary admission flags and stopping its Alto service. Do not unpause the live SimpleAccount paymaster in this sequence.
