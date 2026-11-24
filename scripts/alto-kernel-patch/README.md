# Historical Alto v0.9 SafeMode image patch

These sources reproduce the previous Nitro-backed Staker canary. The current `kernel-sepolia-direct-v1` design mirrors the pinned upstream Alto ARM64 image for managed RPC and does not use this patch. [Kernel rollout procedure](../../docs/runbooks/kernel-aa.md)

This patch is based on upstream Alto v1.2.7 commit `e90957d8136d06087474d11cdcf4fa25bc120898` and its pinned ARM64 image digest. The custom tracer captures the top-level output and EVM error of `PimlicoSimulations.simulateValidation`. For EntryPoint 0.9, SafeValidator decodes a failed `DelegateAndRevert` from its nested return data. It requires a successful outer call with nonempty output before decoding a `ValidationResult`. Malformed frames return an ERC-7769 simulation validation error. The v0.7 and v0.8 branches keep their original decoding path. The ERC-7562 trace parser and checks remain in place.

The canary image also reads the raw `PLETHER_ALTO_RPC_AUTH_TOKEN` environment variable. It adds `Authorization: Bearer <token>` to the primary `ALTO_RPC_URL` for public, wallet and simulation-deployment RPC requests. It does not send that token to a distinct `ALTO_SEND_TRANSACTION_RPC_URL`. A malformed token prevents startup, and the token is never included in validation errors. Store the token as a task secret, not a public environment value.

Run a source check, then build an isolated local image with the pinned checkout and Docker Desktop.

```sh
scripts/alto-kernel-patch/build.sh /path/to/alto-v1.2.7 plether-alto-kernel-safemode:local --check
scripts/alto-kernel-patch/build.sh /path/to/alto-v1.2.7 plether-alto-kernel-safemode:local
```

The builder reads the five original files from the exact Git commit, applies the reviewed patch within a temporary directory and rebuilds from the pinned upstream image. It prevents Git discovery above that directory, and the image copies all patched sources with mode `0644`. Local source edits, other files in the checkout and local credentials are absent from the Docker build context.

The builder compiles TypeScript in the image. The targeted `SafeValidatorV09.test.mjs` test runs against that compiled image and checks tracer error capture, a successful wrapper return, malformed or empty trace data, delegated `AA24`, `AA34` and `AA31` failures, and signature failure in a returned validation result. `RpcAuth.test.mjs` checks the actual outgoing HTTP headers for the primary RPC and simulation wallet transports, token isolation from a distinct relay, and malformed-token rejection. The previous protected release workflow tested the patch and scanned its isolated ECR image.

To repeat the old SafeMode suite, use its qualification harness and source pins from App commit `1fbe1b839f8759270986a6b0d1715634175265b6`. Local image ID `sha256:8397e4091316abf5857f63560576804144ffc09212235df2e8e1e23a983b0a6c` completed all 11 signed operations with the staked Kernel Staker deployment wrapper and staked Plether paymaster on 2026-11-20. The first retest rejected direct deployment through the unstaked Kernel factory under ERC-7562. A separate container startup with a malformed token exited before any RPC call, without printing the token.
