# Fresh Sepolia Kernel gas check

Run [live-gas.mjs](./live-gas.mjs) against the isolated Kernel v0.9 Alto canary and an independent Arbitrum Sepolia Nitro RPC. It makes read-only RPC calls and never signs or submits an operation. The only input is an already prepared, unsigned gateway operation for each payment mode. Capture each estimate **before** its wallet approves and sends the operation. Then verify both inclusion receipts. The captured evidence contains no calldata, signature, recovery token or RPC URL.

The canary must use the patched SafeMode Alto image, the pinned EntryPoint 0.9 code, an active ETH and sponsorship gateway, and a funded staked Staker and paymaster. The operation must have a real execution path that the wallet will approve. Keep the source preparation private. This script accepts a sanitized JSON file containing only `mode`, `entryPoint`, `operation`, `maxNetworkGasWei`, `expiresAt` and optionally `deployment`. The gateway's unsigned operation normally omits `signature`; the checker accepts that or `signature: "0x"`.

## Capture from the live browser

The frontend calls `plether_prepareUserOperation` when it creates the gas quote, **before** `execute` asks the external or Privy wallet to sign. The raw gateway response contains a `recoveryToken`; do not copy the raw Network response, take a CUA screenshot of it, or save it. The read-only canary database has `aa_kernel_preparations.unsigned_operation` without that token, but a database export would need a separate reconstruction of the quote and expiry. The operator browser observer is simpler and does not replay the preparation API.

1. Set up both private RPC tunnels and the CLI command before opening the transaction review. In the canary browser tab, open Chrome DevTools **Sources → Snippets**, create a snippet with the contents of [browser-preparation-observer.js](./browser-preparation-observer.js), and run it. The Console should say `Kernel gas preparation observer armed`. It observes only the same-origin Kernel preparation call and returns the original `fetch` promise unchanged.
2. Select **Pay with ETH** in the sponsored transaction modal's persistent gas toggle, then start the canary action that requests its quote. Wait for the Console line `Kernel gas preparation captured: eth ...`. Keep the review open and **do not approve in the wallet yet**. In DevTools Console run `copy(window.__pletherKernelGasCapture.take('eth'))`. This copies only the allowlisted unsigned operation and quote fields; `take` removes its in-memory copy. On the operator's Mac, immediately write the sanitized clipboard to a private file and clear it:

```sh
umask 077
pbpaste > /private/tmp/kernel-eth-gas-input.json
printf '' | pbcopy
```

3. Run the ETH `capture` command below while the quote is still fresh. If it passes, approve that same reviewed action in the external or Privy wallet, wait for inclusion, and record its UserOperation hash. If capture fails or the quote expires, discard the input and ask the UI for a new quote before approving.
4. Select **Sponsored** in the same persistent toggle before starting the next canary action. Wait for `Kernel gas preparation captured: sponsored ...`, then run `copy(window.__pletherKernelGasCapture.take('sponsored'))`. Save and clear the sanitized clipboard, run the sponsored `capture` command, and only then approve that action. Each mode needs its own quote and included operation.

```sh
umask 077
pbpaste > /private/tmp/kernel-sponsored-gas-input.json
printf '' | pbcopy
```

After both captures, run `window.__pletherKernelGasCapture.restore()` in DevTools Console. This restores the original `fetch` and erases any captured entries. Reloading the page also removes the observer; install it again before requesting another quote. The observer never stores or logs `recoveryToken`, signed owner data, or response fields outside its explicit allowlist. The sanitized operation does contain its required call data and, in sponsored mode, paymaster data; keep the temporary files private and remove them after verification.

The two `capture` calls must finish before the corresponding preparation expires or the wallet sends it. Set `KERNEL_LIVE_GAS_ALTO_RPC_URL` to the private canary's Alto tunnel and `KERNEL_LIVE_GAS_NITRO_RPC_URL` to the Nitro RPC. For a protected Nitro gateway, supply its scoped token through `KERNEL_LIVE_GAS_NITRO_BEARER` with tracing off. The token is sent only to the private Sepolia HTTPS gateway or its root localhost tunnel. Use distinct endpoints for Alto and Nitro. Each evidence file is published atomically with mode `0600`.

```sh
node scripts/kernel-qualification/live-gas.mjs capture \
  --input /private/tmp/kernel-eth-gas-input.json \
  --output /private/tmp/kernel-eth-gas-evidence.json

node scripts/kernel-qualification/live-gas.mjs capture \
  --input /private/tmp/kernel-sponsored-gas-input.json \
  --output /private/tmp/kernel-sponsored-gas-evidence.json
```

After both wallets' actual UserOperations are included, verify both modes. The checker requires exactly one EntryPoint 0.9 from Alto, the pinned EntryPoint runtime at a safe block, a fresh `eth_estimateUserOperationGas` result that fits the reviewed gas caps, and agreement between Nitro `eth_estimateGas` and `NodeInterface.gasEstimateComponents` on a fixed safe block. At that block it also checks the account's EntryPoint deposit plus native ETH in ETH mode, or the paymaster's EntryPoint deposit in sponsored mode, against the maximum prefund. It checks each final operation hash against the onchain `UserOperationEvent` and requires the actual gas used and charged to fit the captured maximum. A reverted operation, receipt mismatch, wrong payer, underquoted gas field, prefund shortfall, stale preparation or missing inclusion fails. It reports the quote usage ratio for each mode.

```sh
node scripts/kernel-qualification/live-gas.mjs verify \
  --eth-evidence /private/tmp/kernel-eth-gas-evidence.json \
  --sponsored-evidence /private/tmp/kernel-sponsored-gas-evidence.json
```

Verify at safe inclusion height. If the recent Alto receipt has aged out, supply the corresponding public bundle hash via `--eth-transaction-hash` or `--sponsored-transaction-hash`. The checker matches the captured operation hash and payer against the canonical EntryPoint event. Kernel's reviewed limits include 12 million pre-verification gas, 17 million aggregate gas, zero sponsored post-operation gas, a 0.05 ETH maximum prefund for ETH-paid operations, and the configured sponsorship budget up to 0.01 ETH per operation.

The optional `bundleL1` field estimates the *whole* eventual `handleOps` transaction from historical pre-inclusion state. A bundler can batch several UserOperations, and an archive RPC may be unable to replay a historical bundle. Therefore this field is context, not a per-operation L1 charge. The captured `nitroFeeControl` proves the live Nitro L1 and L2 estimate path was functioning at the safe block, while the onchain event proves the actual per-operation ETH charge. Arbitrum documents that the L1 posting cost is folded into the L2 gas limit, and the value can change between quoting and inclusion. [Arbitrum gas estimation](https://docs.arbitrum.io/arbitrum-essentials/how-to-estimate-gas) · [NodeInterface reference](https://docs.arbitrum.io/arbitrum-essentials/nodeinterface/reference)

Local fixture check:

```sh
node --test scripts/kernel-qualification/live-gas.test.mjs
```
