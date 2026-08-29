# Managed Pimlico gas sponsorship

Plether Perps currently uses a deterministic permissionless.js SimpleAccount
for managed testing on Arbitrum Sepolia:

- EntryPoint version: `0.8`
- EntryPoint: `0x4337084D9E255Ff0702461CF8895CE9E3b5Ff108`
- SimpleAccount factory: `0x13E9ed32155810FDbd067D4522C492D6f68E5944`
- Account index: `0`
- Nonce key: `0`

The connected wallet owns the Trading Account, but all Perps state and
transactions remain under the Trading Account address. There is no owner-EOA
transaction fallback.

This is a testnet-only integration. The official SimpleAccount is
UUPS-upgradeable, so it does not satisfy the core handoff's production
requirement for immutable execution semantics. Do not enable this account
stack on mainnet; replace it with a reviewed immutable account deployment.
Use fresh, disposable testnet identities and value: a replacement factory will
derive different Trading Account addresses, and this test profile has no state
migration path.

## Frontend flow

1. Fetch and strictly validate the deployment manifest.
2. Derive the SimpleAccount from the connected wallet, factory and index.
3. Compare the derived identity with the locally persisted identity.
4. Build calls with the existing Plether action builders.
5. Let permissionless.js prepare the fully estimated, Pimlico-sponsored
   UserOperation.
6. Ask the owner wallet to sign the final operation.
7. Compute the EntryPoint UserOperation hash and atomically persist it with
   the signed UserOperation preimage before `eth_sendUserOperation`.
8. Submit that exact operation through the same-origin Pimlico proxy.
9. Require Pimlico's returned hash to match the local hash.
10. Reconcile through `pimlico_getUserOperationStatus` and
    `eth_getUserOperationReceipt`.

`not_found` and `not_submitted` are not proof that retrying is safe. Recovery
checks the exact UserOperation receipt first and distinguishes a typed
not-found response from transport or decoding failures. Pimlico status is
queried only after that receipt miss and remains diagnostic; vendor
`not_found`, `not_submitted`, rejected, or failed states do not prove an
onchain outcome or make a retry safe. A Pimlico receipt is accepted only after
its transaction and matching EntryPoint event are verified on the canonical
RPC at or below the `safe` head. A Pimlico transport failure remains
inconclusive and does not prevent the independent safe-chain nonce and expiry
proof below from resolving the operation.

Interactive order progress is deliberately separate from that durable
resolution. An above-`safe` receipt may advance the trade ticket only after
the same exact transaction and EntryPoint event pass canonical RPC validation
and the expected protocol event is decoded from that receipt. This
latest-chain inclusion callback never records a verified transaction hash,
clears authorization state, releases the sponsored-operation lane, or permits
a retry. Safe-head reconciliation continues independently.

For a new record, recovery parses the persisted signed preimage, recomputes
the exact EntryPoint hash, and requires it to match the persisted hash and
Trading Account. The operation nonce and sponsorship validity are trusted
only from that verified preimage. The deadline must come from the pinned
Pimlico v0.8 paymaster format and be nonzero.

Recovery reads the timestamp and EntryPoint nonce at one `safe` block, using
the nonce key encoded in the operation nonce's upper 192 bits. If the nonce is
unchanged and that timestamp is past the verified `validUntil`, the operation
is retry-safe expired. If the nonce advanced without an exact canonical
inclusion event, its past outcome is unknown and non-retryable, but the local
lane is released because the old nonce can no longer land.

Blockscout supplies only a positive full-history event locator. An empty,
missing, or failed explorer result is never evidence of absence. Every
Blockscout hit must be verified by an exact, one-block
`UserOperationEvent` query against the canonical RPC, including its hash,
sender, transaction, and execution result. Third-party, RPC, or corrupt
persisted-data failures fail closed; only the hash-bound protocol nonce or
expiry proofs above can release a lane without a verified event.

Legacy hash-only records cannot bind a nonce or validity deadline and never
auto-expire. Storage migration converts old unverified terminal labels back to
a locked, unknown submission state, removes diagnostic transaction hashes, and
backfills every unresolved hash into the directly addressed lane head while
holding that lane's browser lock. Each legacy hash exposes an explicit
“Force-release stale local lock” escape hatch with a confirmation that the old
action may already have executed or may still execute later. It marks only
that hash outcome unknown and non-retryable; another ambiguous hash in the same
lane remains guarded until it is reviewed separately. Before retrying after a
manual release, close or reload every other Plether tab so an already-open
legacy client cannot restore the obsolete shared-store lock.

The browser-wide Web Lock covers submission, abandoned-record recovery, and
manual force release. Before network send, execution also requires the store
to confirm that it durably journaled the exact hash and signed preimage under a
dedicated per-operation storage key and published a directly addressable
chain/account/lane head. While holding that lane's Web Lock, submission and
recovery read the head and its journal directly instead of relying on mutable
storage-key enumeration. The conservative lane head is written before its
journals, so a crash between durable writes fails closed. Cross-tab hydration
merges the legacy shared snapshot with journals by identity and monotonic
evidence, but version 1 treats the whole-store key as a read-only legacy inbox.
All current mutable persistence writes use per-ID journals; only the locked
migration and submission paths mutate lane heads. This prevents an unrelated
current tab from erasing a version-0 operation through a stale shared-state
read/modify/write. Manual and canonical lane-releasing outcomes are also
written first to append-only ID/hash/status resolution tombstones. Every
journal, lane-head, migration, and raw-inbox check overlays those tombstones,
so a stale tab cannot rewrite a resolved operation back into a blocker and
24-hour history cleanup cannot resurrect it. After all status callbacks,
execution restores the lane once more and exact-checks that singleton head,
signed journal, raw legacy inbox, and persistence revision immediately before
invoking Pimlico. Repeated identical recovery errors are idempotent and do not
resurrect acknowledged alerts.

## Manifest

The testnet deployment publishes
`apps/frontend/public/perps-aa-manifest.json`; set
`VITE_PERPS_AA_MANIFEST_URL` to `/perps-aa-manifest.json` locally. The
`pimlicoRpcUrl` must remain a same-origin `/api/perps/v1/aa/...` path.

The manifest intentionally contains neither the Pimlico API key nor the
sponsorship-policy ID.

## Backend proxy contract

The current Pages worker already maps
`/api/perps/v1/aa/pimlico` to the backend's `/api/aa/pimlico` path. That
backend endpoint is implemented in `Plether.AA.Pimlico` and:

- require the Pages Worker secret sent as `X-Plether-AA-Proxy-Token`;
  configure the same value as the Pages secret `AA_PROXY_ORIGIN_TOKEN`;
- provide that value to the frontend deploy workflow as the GitHub Actions
  secret `AA_PROXY_ORIGIN_TOKEN`, and configure the matching backend secret;
- trust `CF-Connecting-IP` for AA rate limits only after that secret is
  verified, and reject direct unauthenticated access to `/api/aa/pimlico`;
- use a fixed server-side upstream for Arbitrum Sepolia;
- attach the Pimlico API key only on the server;
- replace, rather than merge, paymaster context with the approved
  `sponsorshipPolicyId`;
- reject JSON-RPC batches and unknown methods;
- validate chain `421614`, EntryPoint v0.8, factory, owner/index factory data,
  sender and every decoded SimpleAccount call;
- allow only zero-value calls to the approved Plether contracts;
- enforce account/IP rate limits and an authoritative kill switch;
- avoid logging API keys, signatures or full calldata.

Required methods:

- `pimlico_getUserOperationGasPrice`
- `pm_getPaymasterStubData`
- `pm_getPaymasterData`
- `eth_estimateUserOperationGas`
- `eth_sendUserOperation`
- `eth_getUserOperationReceipt`
- `eth_getUserOperationByHash`
- `pimlico_getUserOperationStatus`
- `eth_supportedEntryPoints`

The proxy must validate both sponsorship requests and final
`eth_sendUserOperation` requests. Destination-only validation is insufficient:
decode `execute(address,uint256,bytes)` and
`executeBatch((address,uint256,bytes)[])`, then validate each nested selector
and sensitive argument.

Validation must be action/sequence-specific and match the vendored Plether
builders, including:

- Trading Account balance deposit:
  `USDC.approve(clearinghouse, amount)` followed by
  `clearinghouse.depositMargin(amount)`;
- order placement, add-margin and claim settlement only with the encoded
  account equal to the UserOperation sender;
- withdrawal to Owner Wallet:
  `clearinghouse.withdrawMargin(amount)` followed by
  `USDC.transfer(verifiedOwner, amount)`;
- rejection of EIP-3009 calls while `usdcSupportsEip3009` is false.

Do not interpret “approved Plether contracts” as a destination-only allowlist:
USDC is a required configured target for approved deposit/withdraw sequences,
but arbitrary USDC selectors or recipients remain denied.

Manual order finalization and cleanup remain disabled in the frontend. Keeper
health and sponsored-gas usage need operational monitoring and alerts.

For deployed testing, provision the three backend credentials while keeping
issuance controlled separately by `AA_SPONSORSHIP_ENABLED`. The Pages-to-origin
hop must use the certificate-backed HTTPS API hostname; do not send the shared
origin token to the public HTTP ALB listener.
