# Local Kernel v4 and Alto qualification

This harness creates an isolated Prague Anvil chain with chain ID `421614` and starts Alto against it. It uses ZeroDev’s pinned release code, a reproducibly compiled ZeroDev ECDSA validator and the direct-factory Core Kernel paymaster. A first UserOperation uses the pinned KernelFactory directly through EntryPoint 0.9. The factory and paymaster have zero EntryPoint stake. Sponsorship draws from the paymaster’s EntryPoint deposit. All signed actions and funding are local. The reference bytecode comes from read-only Arbitrum Sepolia requests. [Artifact pins](./pins.json)

The harness runs the app’s Kernel address verification and signing adapter through simulated external and embedded signing boundaries in both gas modes. It verifies the counterfactual address before the wallet sends ETH and confirms that the transfer changes only its balance. The first approved operation deploys that address. At the inclusion block, the browser verifier checks the deployed proxy, UUPS implementation, root validator and signer. It also exercises payer changes, same-nonce cancellations, and owner withdrawals of liquid ETH and EntryPoint deposits. The local sponsored deposit sweep uses a locally signed paymaster envelope and direct Alto RPC to exercise the contracts and bundler. Gateway sponsorship policy and real-wallet approval require separate checks.

At zero balance, the ETH-paid first-operation estimate returns `AA21 didn't pay prefund` with RPC code `-32500`. A sender balance override gives an advisory prefund without changing the account’s code, balance, deposit or nonce. The funded first operation is then estimated again against real state. Operation hashes agree across Viem, EntryPoint and Alto, and the harness verifies each operation’s nonce and exact payer debit from its inclusion event.

## Requirements

- Node 25 with the app’s frontend dependencies installed, Foundry, Solidity `0.8.30+commit.73712a01`, and Docker Desktop. The Docker container reaches the isolated host Anvil port through `host.docker.internal`. The two RPC listeners bind to loopback.
- ZeroDev’s [Kernel v4 release commit](https://github.com/zerodevapp/kernel/tree/c960b42d2ed4adb0d5328f6e762962debdf8e57a) and a clean checkout of the [ECDSA validator commit](https://github.com/zerodevapp/kernel-7579-plugins/tree/332deed6eeef3d6279cde50aa1d51eff53728bd4). The validator checkout includes its pinned dependencies. A build hash mismatch stops execution.
- The direct-factory Core `PletherKernelVerifyingPaymaster.json` Foundry artifact from `packages/perps-aa/out/PletherKernelVerifyingPaymaster.sol/` in the reviewed Core checkout.
- The exact reviewed Alto image for this run. The public Alto v1.2.8 ARM64 digest is `sha256:b37c5697bc5331391cb8bc5ac6e828d15019fc786c58a33a8650ff3c434ac2aa`. This image provides an upstream baseline for local qualification. Pin and verify the deployment candidate separately. [Alto container package](https://github.com/pimlicolabs/alto/pkgs/container/alto)

## Capture the reference code

Use an Arbitrum Sepolia RPC with access to block `331101957`. Check its block hash against the pin before saving the runtime. These calls can be skipped if the three files are already available.

```sh
qualify_runtime_dir=$(mktemp -d)
qualify_rpc=https://sepolia-rollup.arbitrum.io/rpc
qualify_block=331101957

(
  set -e
  test "$(cast block "$qualify_block" --field hash --rpc-url "$qualify_rpc")" = \
    0xe4cf42a69df9e9f9b4164feeeea467093b82237808a6dc6de13121ebd00300fd

  cast code 0x433709009B8330FDa32311DF1C2AFA402eD8D009 --block "$qualify_block" \
    --rpc-url "$qualify_rpc" > "$qualify_runtime_dir/entrypoint-runtime.hex"
  cast code 0x0A630a99Df908A81115A3022927Be82f9299987e --block "$qualify_block" \
    --rpc-url "$qualify_rpc" > "$qualify_runtime_dir/sender-creator-runtime.hex"
  cast code 0x4e59b44847b379578588920cA78FbF26c0B4956C --block "$qualify_block" \
    --rpc-url "$qualify_rpc" > "$qualify_runtime_dir/create2-runtime.hex"
)
```

Run from the app repository root. The runner verifies each captured runtime hash before starting Anvil.

```sh
node scripts/kernel-qualification/run.mjs \
  --kernel-repo /path/to/zerodev-kernel \
  --validator-repo /path/to/kernel-7579-plugins \
  --solc /path/to/solc-0.8.30 \
  --runtime-dir "$qualify_runtime_dir" \
  --paymaster-artifact /path/to/PletherKernelVerifyingPaymaster.json \
  --alto-image "$REVIEWED_ALTO_IMAGE"
```

## Qualification boundaries

On 2026-11-21 the pinned upstream Alto v1.2.8 ARM64 image passed 11 operations on Anvil with a direct unstaked factory and unstaked paymaster. The run verified both simulated wallet boundaries, both gas modes, payer switches, cancellations and exact payer debits. Normal validation rejected a wrong owner signature with RPC code `-32507`. Its nonce and account deployment state stayed unchanged. The local paymaster started paused and had a 0.25 ETH EntryPoint deposit.

The direct-factory profile uses Alto’s normal simulation and signature validation with `ALTO_SAFE_MODE=false` and `ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION=false`. It relies on private Alto ingress, the Kernel gateway’s operation policy, and the dedicated sponsorship signer and budget. Trace-based ERC-7562 validation and reputation checks are outside this profile. The 2026-11-20 patched Alto tracer results are historical evidence for the earlier Staker profile. [Historical SafeMode evidence](../../docs/runbooks/evidence/kernel-nitro-safemode-2026-11-20.json)

For `kernel-sepolia-direct-v1`, attest the pinned Kernel factory, implementation, validator and simulators and a fresh paused direct-factory paymaster with its configured deposit. The local chain exercises generic EVM gas estimates with an isolated call target and simulated `personal_sign` transports. Live Arbitrum gas estimates, app gateway policy, KMS signing, durable budget accounting and real transactions from external and Privy wallets each require separate canary evidence before rollout.

For the public canary, see the [fresh Sepolia deployment](./deploy-sepolia.md),
[isolated credential provisioning](./provision-canary-credentials.md),
[live gas evidence](./live-gas.md), and [Pages preview](./pages-canary.md).
