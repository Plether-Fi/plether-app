# Sepolia faucet signer isolation and nonce recovery

The Arbitrum Sepolia mock USDC deployed at
`0xf7cbfcc74f2d9eb6fa7dc11941b3bef9fd7f8eb8` has unrestricted `mint(address,uint256)`.
It has no owner/minter-role requirement. The faucet needs a dedicated EOA and
Arbitrum Sepolia ETH for gas. Do not transfer token ownership or reuse the keeper
key. Other deployments must have their mint permissions verified separately.

The faucet database lock coordinates faucet requests, not keeper, oracle,
liquidation, bundler or settlement transactions. Sharing a signer allows another
worker to consume a nonce saved by the faucet. A missing receipt or `nonce too
low` alone cannot safely release that saved claim.

## Isolate the signer

1. Generate a dedicated key in secure storage. Derive and compare its public
   address against every runtime signer, including keeper, liquidation, oracle,
   LP settlement, protection, Alto utility/executors, paymaster, treasury and
   deployer where configured. Never print key values.
2. Fund its address with test ETH on chain 421614. Use an independent wallet;
   do not race another service's signing key to fund it.
3. Update the existing faucet SecureString and the deployment's authoritative
   secret inputs. Terraform rejects faucet reuse of the configured keeper,
   oracle, liquidation or LP settlement key (including case/prefix variants).
   It must not overwrite a rotation with stale inputs. Verify other signer roles
   operationally; not every key is supplied to Terraform.
4. Deploy through the backend GitHub workflow on the reviewed master commit.
   Confirm every serving API task has restarted with the updated secret, the
   faucet address is the intended address, and its balance is sufficient.
   Updating SSM alone does not change a running task's environment.
5. Keep the existing recovery-registry enforcement and recovery flags enabled.
   No preparation issuance fence may be rolled back during a faucet repair.

## Repair a proven nonce conflict

This is an operator tool; the public API does not accept replacement hashes.
Run it in a short-lived task using the reviewed backend image, database secret,
primary RPC secret and primary bearer token. It needs no wallet key or task IAM
role. The secondary endpoint is fixed to the public Arbitrum Sepolia RPC. The
primary must be independent. Restrict logs to the bounded result/error output.

First apply the additive migration
`apps/backend/config/migrations/testnet-faucet-nonce-recovery-v1.sql` (also present
in `/app/config/migrations/` in the backend image). It creates an audit table and
changes no claims. Restrict access to its signed transaction payloads as with
`testnet_faucet_claims`; never export those payloads to logs.

Obtain the candidate replacement hash from transaction history. The candidate
is only a hint: the tool independently verifies the signed original hash,
chain, token, recipient, amount, sender and nonce; both RPCs must confirm a
different transaction at that nonce in a canonical block at or below `safe`.
It rejects inconsistent, unavailable or incomplete evidence, an existing
original receipt, a replacement mint to the recipient, or an already-funded
recipient. It does not infer failure from wall-clock age or a missing receipt.

```sh
node /app/scripts/recover-faucet-nonce.mjs ORIGINAL_HASH REPLACEMENT_HASH --check
node /app/scripts/recover-faucet-nonce.mjs ORIGINAL_HASH REPLACEMENT_HASH --apply
```

`--check` is read-only. `--apply` repeats chain verification, then takes the
faucet advisory lock and atomically archives the signed transaction and proof
while marking only the exact unchanged submitted claim as retryable. Audit
insertion failure rolls back the state transition. Repeating a completed repair
is a no-op; concurrent claim changes are never overwritten. A future retry uses
the normal faucet claim reservation and isolated signer. The tool never signs,
submits, mints or deposits funds.

After `recovered_retry_available`, the user can select **Check confirmation**
again. That explicit action calls the backend and receives the new transaction
hash. No browser-storage deletion is required. Existing automatic deposit
behavior following a user-requested faucet claim is unchanged; the recovery
administration itself must not open the wallet or initiate a deposit.

## Verification

```sh
FAUCET_TEST_DATABASE_URL=postgresql://postgres:postgres@localhost:5432/faucet_test \
  node --test apps/backend/scripts/recover-faucet-nonce.test.mjs
```

Use an isolated test database. The integration test creates/drops only its
`faucet_recovery_test` schema. Unit tests exercise wrong scope, unsafe/reorged
blocks, existing receipts, provider disagreement/failure, replacement mints and
funded recipients. Database tests cover compare-and-swap, idempotency and rollback.
Frontend tests check receipt timeout, claim preservation, manual backend retry,
account changes and no deposit after failed confirmation.
