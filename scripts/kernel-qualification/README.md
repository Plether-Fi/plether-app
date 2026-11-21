# Local Kernel v4 and Alto qualification

This harness creates an isolated Prague Anvil chain with chain ID `421614` and starts the selected Alto image against it. It uses ZeroDev’s release creation code, a reproducibly compiled ZeroDev ECDSA validator and the Core Kernel paymaster. It deploys a ZeroDev Staker owned by the local utility account, approves KernelFactory, and stakes both Staker and paymaster with the local EntryPoint. All signed actions and funding are local. The captured reference bytecode comes from read-only Arbitrum Sepolia requests. [Artifact pins](./pins.json) · [Alto test config](https://github.com/pimlicolabs/alto/blob/e90957d8136d06087474d11cdcf4fa25bc120898/test/e2e/alto-config.json)

The harness runs the app’s Kernel address verification and signing adapter through both wallet signing boundaries and both gas modes. It verifies the counterfactual address before the wallet sends ETH and confirms that the transfer changes only its balance. The first approved operation deploys that address. At the inclusion block, the browser verifier checks the deployed proxy, UUPS implementation, root validator and signer. The harness also exercises payer changes on the same account, same-nonce cancellations, and owner withdrawals of liquid ETH and EntryPoint deposits while the paymaster is paused. After sponsorship resumes, a sponsored withdrawal recovers the entire remaining deposit.

At zero balance, the ETH-paid first-operation estimate returns `AA21 didn't pay prefund` with RPC code `-32500`. A sender balance override gives an advisory prefund without changing the account’s code, balance, deposit or nonce. The funded first operation is then estimated again against real state. Operation hashes agree across Viem, EntryPoint and Alto, and the harness verifies each operation’s nonce and exact payer debit from its inclusion event.

## Requirements

- Node 25 with the app’s frontend dependencies installed, Foundry, Solidity `0.8.30+commit.73712a01`, and Docker Desktop. The Docker container reaches the isolated host Anvil port through `host.docker.internal`. The two RPC listeners bind to loopback.
- ZeroDev’s [Kernel v4 release commit](https://github.com/zerodevapp/kernel/tree/c960b42d2ed4adb0d5328f6e762962debdf8e57a) and a clean checkout of the [ECDSA validator commit](https://github.com/zerodevapp/kernel-7579-plugins/tree/332deed6eeef3d6279cde50aa1d51eff53728bd4). The validator checkout includes its pinned dependencies. A build hash mismatch stops execution.
- The Core `PletherKernelVerifyingPaymaster.json` Foundry artifact from `packages/perps-aa/out/PletherKernelVerifyingPaymaster.sol/`. Build it from the Staker-aware Core implementation under test before running this harness.
- Plether’s `ghcr.io/pimlicolabs/alto:v1.2.7@sha256:28cee87ea6b58ba10a37273e58602b50321516c36a81d0c35d50526d1f06995d` image already pulled locally. For SafeMode qualification, build the patched image from [the pinned source patch](../alto-kernel-patch/README.md).

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
  --paymaster-artifact /path/to/PletherKernelVerifyingPaymaster.json

# For the local SafeMode qualification, add the exact built image and flag.
node scripts/kernel-qualification/run.mjs \
  --kernel-repo /path/to/zerodev-kernel \
  --validator-repo /path/to/kernel-7579-plugins \
  --solc /path/to/solc-0.8.30 \
  --runtime-dir "$qualify_runtime_dir" \
  --paymaster-artifact /path/to/PletherKernelVerifyingPaymaster.json \
  --alto-image plether-alto-kernel-safemode:local \
  --safe-mode
```

## Qualification boundaries

The original pinned Alto image passes with normal validation and `safeMode=false`. Its `--safe-mode` path fails on the first submission with HTTP 500 because Alto decodes EntryPoint 0.9’s inner `DelegateAndRevert(bool,bytes)` frame instead of the top-level simulation result. The pinned source patch corrects that decoder. With a staked Staker and paymaster, the patched image passed all local scenarios under `--safe-mode` on 2026-11-20. The local chain uses Alto’s generic EVM estimation path. Actual Nitro SafeMode operation validation and live Arbitrum L1 data-fee estimates remain separate qualification gates. [v1.2.8 source comparison](https://github.com/pimlicolabs/alto/compare/v1.2.7...v1.2.8)

At the reference block the upstream Kernel implementation, factory and selected validator addresses had no Arbitrum Sepolia code. A fresh release must deploy and attest the exact Kernel artifacts, validator, Staker, paymaster and simulation contracts, including the Staker approval and both one-day stakes, before activating the profile. This harness uses an isolated call target, direct bundler RPC and local wallet signers behind a simulated `personal_sign` transport. Real Privy and external wallet flows, app gateway, KMS credentials and durable budget accounting require their own rollout checks.

For the public canary, see the [fresh Sepolia deployment](./deploy-sepolia.md),
[isolated credential provisioning](./provision-canary-credentials.md),
[live gas evidence](./live-gas.md), and [Pages preview](./pages-canary.md).
