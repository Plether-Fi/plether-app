# Fresh Sepolia Kernel gas check

Run [live-gas.mjs](./live-gas.mjs) against the isolated EntryPoint 0.9 Alto canary and its reviewed managed Arbitrum Sepolia RPC. The script makes read-only calls with an unsigned gateway preparation in each mode. Capture each estimate before wallet approval, then verify the inclusion receipts. The saved evidence contains no calldata, signature, recovery token or RPC URL.

Use the attested `kernel-sepolia-direct-v1` profile, exact reviewed managed Alto image and pinned EntryPoint 0.9. Enable gas modes only for the approved canary owners. An ETH-paid operation needs sufficient account ETH or EntryPoint deposit. A sponsored operation needs an unpaused canary paymaster with sufficient EntryPoint deposit. Keep the source preparation private. The script accepts sanitized JSON containing `mode`, `entryPoint`, `operation`, `maxNetworkGasWei`, `expiresAt` and optionally `deployment`. Omit the signature or set it to `"0x"`.

## Capture from the live browser

The frontend calls `plether_prepareUserOperation` when it creates the gas quote, **before** `execute` asks the external or Privy wallet to sign. The raw gateway response contains a `recoveryToken`; do not copy the raw Network response, take a CUA screenshot of it, or save it. The read-only canary database has `aa_kernel_preparations.unsigned_operation` without that token, but a database export would need a separate reconstruction of the quote and expiry. The operator browser observer is simpler and does not replay the preparation API.

1. Set up the approved private Alto access, managed RPC and CLI command before opening the transaction review. In the canary browser tab, open Chrome DevTools **Sources → Snippets**, create a snippet with the contents of [browser-preparation-observer.js](./browser-preparation-observer.js), and run it. The Console should say `Kernel gas preparation observer armed`. It observes the same-origin Kernel preparation call and returns the original `fetch` promise unchanged.
2. Select **Pay with ETH** in the sponsored transaction modal's persistent gas toggle (note: this applies to the Alto ETH-paid path; the two-wallet homepage opt-in instead submits **Pay from wallet** directly to EntryPoint without an Alto `eth` preparation), then start the canary action that requests its quote. Wait for the Console line `Kernel gas preparation captured: eth ...`. Keep the review open and **do not approve in the wallet yet**. In DevTools Console run `copy(window.__pletherKernelGasCapture.take('eth'))`. This copies only the allowlisted unsigned operation and quote fields; `take` removes its in-memory copy. On the operator's Mac, immediately write the sanitized clipboard to a private file and clear it:

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

Complete the two `capture` calls before the corresponding preparation expires or the wallet sends it. Set `KERNEL_LIVE_GAS_ALTO_RPC_URL` to the approved private Alto endpoint and `KERNEL_LIVE_GAS_PRIMARY_RPC_URL` to the reviewed managed HTTPS chain endpoint. Load credential-bearing URLs through the environment with tracing off, and leave `KERNEL_LIVE_GAS_NITRO_BEARER` unset for the managed endpoint. The legacy Nitro variables support the existing tunnel transport when authorized. Each evidence file is written atomically with mode `0600`.

```sh
node scripts/kernel-qualification/live-gas.mjs capture \
  --input /private/tmp/kernel-eth-gas-input.json \
  --output /private/tmp/kernel-eth-gas-evidence.json

node scripts/kernel-qualification/live-gas.mjs capture \
  --input /private/tmp/kernel-sponsored-gas-input.json \
  --output /private/tmp/kernel-sponsored-gas-evidence.json
```

After one wallet's two UserOperations are included, verify both modes. The checker compares Alto's fresh `eth_estimateUserOperationGas` results with the reviewed caps and requires `eth_estimateGas` and `NodeInterface.gasEstimateComponents` to agree at a fixed safe block. It checks the payer's available prefund and matches each operation hash with the onchain `UserOperationEvent`, payer, actual gas and captured maximum. Repeat with separate evidence files for the other wallet so the external and Privy EOAs each have a first deployment and a payer switch.

```sh
node scripts/kernel-qualification/live-gas.mjs verify \
  --eth-evidence /private/tmp/kernel-eth-gas-evidence.json \
  --sponsored-evidence /private/tmp/kernel-sponsored-gas-evidence.json
```

Verify at safe inclusion height. If the recent Alto receipt has aged out, supply the corresponding public bundle hash via `--eth-transaction-hash` or `--sponsored-transaction-hash`. The checker matches the captured operation hash and payer against the canonical EntryPoint event. Kernel's reviewed limits include 12 million pre-verification gas, 17 million aggregate gas, zero sponsored post-operation gas, a 0.05 ETH maximum prefund for ETH-paid operations, and the configured sponsorship budget up to 0.01 ETH per operation.

The optional `bundleL1` field estimates the entire historical `handleOps` transaction and may include several UserOperations. It requires access to the historical pre-inclusion state. The `nitroFeeControl` field records the live Arbitrum L1 and L2 estimate at the safe block. The onchain event records the actual per-operation ETH charge. Arbitrum folds L1 posting cost into the L2 gas limit, and that cost can change between quote and inclusion. [Arbitrum gas estimation](https://docs.arbitrum.io/arbitrum-essentials/how-to-estimate-gas) · [NodeInterface reference](https://docs.arbitrum.io/arbitrum-essentials/nodeinterface/reference)

Local fixture check:

```sh
node --test scripts/kernel-qualification/live-gas.test.mjs
```
