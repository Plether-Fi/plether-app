# Read-only Arbitrum Sepolia canary top-up verification

After the separate 2.48 ETH bridge is attested, verify a **direct L2 ETH transfer** of at least 0.27 ETH to the dedicated canary EOA. This script makes no wallet, signer, SSM, or transaction call.

```sh
node scripts/kernel-qualification/verify-topup.mjs --hash 0xPUBLIC_ARBITRUM_SEPOLIA_TX_HASH
```

It checks Arbitrum Sepolia chain ID 421614 with two independent HTTPS RPCs, a common safe block hash, the same successful transaction and receipt, canonical inclusion, exact recipient, no calldata, a non-deposit transaction type, and at least 0.27 ETH value. The inclusion block must credit the transfer value. The dedicated EOA must still have zero L2 nonce and no code. Both RPCs must report safe and latest balances of at least 2.75 ETH before contract deployment.

The defaults are the official Arbitrum Sepolia RPC and Tenderly's public Arbitrum Sepolia gateway. `--primary` and `--secondary` can override them. URLs cannot include credentials, queries, or fragments. Recheck both providers' safe block and historical state availability when running the verifier.

This verifier accepts a direct transfer from a faucet wallet or an external wallet. It deliberately rejects a faucet contract call that credits the EOA internally; that requires a separate verified internal transfer trace and review. If a faucet uses an internal transfer, claim to an external wallet and send at least 0.27 ETH directly to the dedicated address on Arbitrum Sepolia, leaving enough test ETH in that wallet for gas. Preserve the bridge attestation and this top-up attestation as distinct funding evidence.
