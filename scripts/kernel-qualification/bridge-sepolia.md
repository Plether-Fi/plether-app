# One-use Ethereum Sepolia to Arbitrum Sepolia funding bridge

This procedure moves up to 2.49 test ETH from the dedicated Kernel canary EOA on Ethereum Sepolia to the **same address** on Arbitrum Sepolia. It is pinned to the successful 2.5 ETH L1 funding transaction, the Arbitrum Sepolia Delayed Inbox and Bridge, and the no-argument `depositEth()` call. The default command is read-only. No bridge transaction is sent while preparing or reviewing the plan.

The contract and deposit-message rules come from [Offchain Labs Nitro contracts](https://github.com/OffchainLabs/nitro-contracts/blob/main/src/bridge/Inbox.sol) and the [Offchain Labs SDK](https://github.com/OffchainLabs/arbitrum-sdk/blob/main/packages/sdk/src/lib/message/ParentToChildMessage.ts). Keep the public intent and attestation as evidence for the canary deployment.

## Read-only plan

Install the frontend dependencies with the repository lockfile first. Run from the repository root:

```sh
node scripts/kernel-qualification/bridge-sepolia.mjs \
  --config scripts/kernel-qualification/bridge-public.example.json \
  --intent /private/tmp/kernel-canary-bridge-intent.public.json
```

The example uses two independent HTTPS RPC hosts per chain. Their chain IDs, `safe` heads, and required historical reads were checked on 21 November 2026; check them again before execution. Override the secondary URLs with `KERNEL_L1_SECONDARY_RPC_URL` and `KERNEL_L2_SECONDARY_RPC_URL` if needed. Do not put keys in RPC URLs. PublicNode's L1 endpoint intermittently rejected the funding block's archive balance read; dRPC's public L2 endpoint returned a `safe` block but could not serve its historical state. Neither is suitable for this check.

The plan checks the funding receipt on the common L1 safe chain; zero L1 and L2 signer nonces and code; pinned Inbox and Bridge identity; matching safe blocks and balances from both RPCs; exactly 2.5 ETH on L1 and zero on L2; live `eth_call` and gas estimates; and at least 0.01 ETH left on L1 after the **maximum** gas cost. It prints public fields and RPC hostnames only.

The example bridges 2.48 ETH. At the fixed caps of 250,000 gas and 10 gwei, it leaves at least 0.0175 ETH on L1. If fees exceed a cap or a precondition changes, the script stops rather than changing the amount.

## Broadcast after review

Use the approved Plether signer process to inject `KERNEL_CANARY_DEPLOYER_PRIVATE_KEY` into the process environment. Never put the key in the config, command line, shell history, logs, or intent. Review the current read-only plan and exact public config before running the following command:

```sh
node scripts/kernel-qualification/bridge-sepolia.mjs \
  --config scripts/kernel-qualification/bridge-public.example.json \
  --intent /private/tmp/kernel-canary-bridge-intent.public.json \
  --broadcast \
  --confirm bridge:kernel-sepolia-canary-20261121:2480000000000000000
```

The script repeats the read-only checks, verifies the signer address, writes and fsyncs a public signed intent containing the transaction hash, checks both chains again, and sends the serialized L1 transaction **once**. It does not retry an ambiguous send. If an intent already exists, inspect its hash and the signer nonce on both L1 RPCs. Do not remove it to force another broadcast.

## Read-only attestation

After broadcast, use `--attest` with the same `--config` and `--intent`. The script checks the L1 transaction and receipt at a common safe block on two RPCs, parses the Inbox and Bridge events, derives the L2 type-0x64 deposit transaction hash, verifies the matching L2 transaction and receipt at a common safe block on two RPCs, checks the credited balance, and confirms the L1 reserve and no further signer transactions. It updates the public intent only when those checks pass. If the L2 deposit is still pending, wait and rerun `--attest`; never repeat the L1 transaction.

The bridge alone gives the L2 signer 2.48 ETH. For the lower-cost canary deployment, keep `paymasterDepositWei` at 0.25 ETH and obtain an independently verified L2 top-up so the signer has at least 2.75 ETH. A 0.5 ETH Arbitrum Sepolia faucet transfer would give 2.98 ETH before deployment gas. The existing direct-L2-funding verifier expects one 4.6 ETH transfer and does not apply to this route; use the bridge attestation and verify the top-up transaction separately.
