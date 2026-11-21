# Alto v0.9 SafeMode image patch

This patch is based on upstream Alto v1.2.7 commit `e90957d8136d06087474d11cdcf4fa25bc120898` and its pinned ARM64 image digest. The change captures the top-level output of `PimlicoSimulations.simulateValidation` in the existing custom tracer. For EntryPoint 0.9, SafeValidator checks that the nested `DelegateAndRevert` call succeeded and decodes that top-level output as `ValidationResult`. The v0.7 and v0.8 branches keep their original decoding path. The ERC-7562 trace parser and checks remain in place.

The canary image also reads the raw `PLETHER_ALTO_RPC_AUTH_TOKEN` environment variable. It adds `Authorization: Bearer <token>` to the primary `ALTO_RPC_URL` for public, wallet and simulation-deployment RPC requests. It does not send that token to a distinct `ALTO_SEND_TRANSACTION_RPC_URL`. A malformed token prevents startup, and the token is never included in validation errors. Store the token as a task secret, not a public environment value.

Run a source check, then build an isolated local image with the pinned checkout and Docker Desktop.

```sh
scripts/alto-kernel-patch/build.sh /path/to/alto-v1.2.7 plether-alto-kernel-safemode:local --check
scripts/alto-kernel-patch/build.sh /path/to/alto-v1.2.7 plether-alto-kernel-safemode:local
```

The builder reads the five original files from the exact Git commit, applies the reviewed patch in a temporary directory and rebuilds from the pinned upstream image. Local source edits, other files in the checkout and local credentials are absent from the Docker build context.

The builder compiles TypeScript in the image. The targeted `SafeValidatorV09.test.mjs` test runs against that compiled image and checks a successful wrapper return, malformed output, a failed delegated validation and a signature failure. `RpcAuth.test.mjs` checks the actual outgoing HTTP headers for the primary RPC and simulation wallet transports, token isolation from a distinct relay, and malformed-token rejection. The manual `build-kernel-alto.yml` workflow is dispatched from master with the exact head SHA and number of an open, same-repository `kernel-canary/*` PR. It rechecks that head after building and immediately before ECR publication. It runs both tests, scans the image and records its immutable digest in the separate `plether-kernel-alto-sepolia` ECR repository under the protected Sepolia Alto environment. This repository must exist before workflow dispatch. The workflow does not update the existing v0.8 ECS service.

Use `scripts/kernel-qualification/run.mjs --alto-image plether-alto-kernel-safemode:local --safe-mode` to check the resulting image with the pinned Kernel operation suite. Local image ID `sha256:8397e4091316abf5857f63560576804144ffc09212235df2e8e1e23a983b0a6c` completed all 11 signed operations with the staked Kernel Staker deployment wrapper and staked Plether paymaster on 2026-11-20. The first retest correctly rejected direct deployment through the unstaked Kernel factory under ERC-7562. A separate container startup with a malformed token exited before any RPC call, without printing the token. A debug-enabled Nitro Sepolia provider, live gas estimates and real wallet operations in both payment modes remain release gates.
