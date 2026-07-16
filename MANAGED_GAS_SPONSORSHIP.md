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
7. Compute and persist the EntryPoint UserOperation hash locally.
8. Submit that exact operation through the same-origin Pimlico proxy.
9. Require Pimlico's returned hash to match the local hash.
10. Reconcile through `pimlico_getUserOperationStatus` and
    `eth_getUserOperationReceipt`.

`not_found` and `not_submitted` are not proof that retrying is safe. An
operation with a persisted hash remains locked until receipt/status or
protocol state proves a terminal outcome.

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
