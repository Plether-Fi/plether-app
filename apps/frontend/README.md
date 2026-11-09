# Plether Frontend

Current perps source selects Core v2.0.0 and the checksum-pinned SDK 0.4.0.
See `../../config/perps/ORDER_V3_TIMING.md` for provenance. The public AA manifest
binds the new MockUSDG market to the reviewed existing paymaster. Backend
readiness remains authoritative for sponsored funding and order submission.

React web application for trading plDXY-BEAR and plDXY-BULL tokens.

## Tech Stack

- React 19 + TypeScript + Vite
- Privy + wagmi + viem + permissionless.js
- Tailwind CSS v4
- Zustand + TanStack Query

## Development

The frontend installs `@plether-fi/perps-aa-client` from GitHub Packages.
Authenticate npm before installing, even though the package is public. Use a
read-only `read:packages` credential in your user npm configuration; never
commit a token. The checked-in `.npmrc` selects the registry. CI uses its
repository-scoped `GITHUB_TOKEN` with `packages: read` access.

The exact client release, Core commit, and tarball integrity are recorded in
`../../config/perps-aa-client-release.json` and `package-lock.json`. This client
version is independent of the deployed Core contract release. The old vendored
client and Core patch are no longer used.

Perps imports `permissionless-v03`, an npm alias pinned to `permissionless@0.3.7`.
This keeps the existing SimpleAccount v0.8 runtime separate from Privy's optional
`permissionless@0.2` integration, which is not installed or used. Keep the alias
and its `ox@0.11.3` peer pin together; viem carries its own newer `ox` dependency.
Normal `npm ci` resolves the dependency graph without
`--force` or `--legacy-peer-deps`.

```bash
npm install
npm run dev        # Dev server at http://localhost:5173
npm run build      # TypeScript check + production build
npm run lint       # ESLint
npm test           # Unit, wallet integration, worker, and diagnostic tests
npm run test:wallet-integration # Actual Privy/Wagmi bridge and recoverable owner signatures
npm run anvil      # Start Anvil fork (port 8546, requires SEPOLIA_RPC_URL)
npm run test:integration  # Integration tests (requires Anvil)
npm run test:e2e:wallet  # Offline signing and wallet-event browser checks
npm run test:e2e:privy   # Read-only browsing with Privy deliberately unconfigured
npm run test:e2e:privy-sdk # Actual Privy modal and Wagmi lifecycle with offline service fixtures
npm run storybook  # Component explorer at http://localhost:6006
npm run perps:oracle-worker -- --once  # Update the perps mark from backend-cached Pyth data
npm run perps:oracle-worker -- --loop  # Keep updating the perps mark from backend cache
```

## Login and wallet connection

Set the public `VITE_PRIVY_APP_ID` in `.env.local` to enable email, Google, and passkey login
and external EVM wallets. `VITE_PRIVY_CLIENT_ID` is optional. With no app ID,
the application remains available for browsing and wallet connection is disabled.
Never put a Privy app secret, OAuth client secret, or signing key in a `VITE_`
variable: Vite includes these values in browser assets.

The hosted Arbitrum Sepolia trading app (`https://app.sepolia.plether.com`) and
Insights (`https://insights.plether.com`) must use the **same Privy application**
so the same email login resolves the same embedded owner wallet. Insights is
hosted as a production site but connects to the testnet, so do not select a
different Privy application based only on the deployment's production label.
Use a separate shared Privy application for local development or isolated previews.
Separate app client IDs can scope origins and session settings within each app;
sharing the application does not imply that browser sessions are shared across origins.

In the Privy dashboard, enable the login methods shown by the application,
configure their OAuth credentials/callbacks, and allow each exact application
origin. Include the local Vite and Storybook origins in the development app when
using them. External wallets connect without an extra Privy sign-in message;
Insights still requires its own one-time ownership signature. Embedded wallets
are EOA owners for the existing Perps trading account implementation, not a
replacement Privy smart-account or sponsorship service. The connection popup offers email, Google, and passkey login, followed by the complete searchable
WalletConnect catalog and detected extensions. Email is optional for external-wallet
users. Users explicitly select the active owner wallet when more than one is connected.

This is a clean provider switch: Reown sessions and embedded-wallet keys are
not imported. Privy email login uses a newly provisioned Privy wallet.

Deployment configuration uses the shared GitHub Actions variable `PRIVY_APP_ID`
and optional `PRIVY_TRADING_CLIENT_ID`; **Deploy Insights** uses that same app ID
and optional `PRIVY_INSIGHTS_CLIENT_ID`. The Sepolia frontend workflow validates
the public app ID before building. Its mainnet target publishes the existing
redirect and does not require Privy configuration.

Before merging, test both applications against the same Privy development app:
external desktop/mobile wallets, email/Google/passkey login, account linking, returning sessions, explicit owner
selection, logout, message/typed-data signatures, and a sponsored Perps operation.
Use the local registration service or an isolated backend with a matching
canonical registration origin for the full Insights flow. A Pages preview with
production backend settings cannot bypass the registration origin restriction.
Check CSP enforcement in a browser; the checked-in policies follow
[Privy's EVM CSP guidance](https://docs.privy.io/security/implementation-guide/content-security-policy).
Do not record login credentials, OAuth callbacks, signatures, or wallet export UI
in analytics or session replay.

`npm test` requires the deterministic wallet integration suite. It runs the
installed Privy/Wagmi bridge, Wagmi connectors/actions, and viem signature
verification; only remote authentication/modal callbacks are controlled by the
fixture. Browser CI also requires `test:e2e:privy-sdk`, which runs the actual
Privy modal and application with injected public test wallets and synthetic
public app configuration. External HTTP/WebSocket requests are blocked.
Each Playwright server uses a suite/port-specific `PLETHER_VITE_CACHE_DIR` under
`node_modules`, so forced dependency optimization cannot invalidate chunks in an
open local app. Vite also ignores generated test/report directories: HTML trace
snapshots must never reload a tab during OAuth. Keep these settings when adding
new browser suites. These suites
cover connection timing, saved owners, cancellation, reconnection, owner/network
switching, email-wallet creation callbacks, and stale signer revocation. They
do not send email OTPs, create/recover a wallet through the live embedded-key
service, exercise real extension UI, or pair a physical mobile wallet; those
remain hosted-service/device smoke checks for deployment.

Every connection picker offers **Log in with Email** when signed out or
**Use email wallet** for an authenticated session, including **Connect new
wallet** inside Privy's native account dialog. Selecting an email wallet closes
the picker through Privy's own close path and updates the guarded signer.

Google and passkeys must both be enabled in **Privy Dashboard → Login methods**
for the shared app. Enable standard Google OAuth (not custom OAuth), configure
its provider credentials and callback URL as shown by Privy (development apps
can use Privy's default Google credentials), and allow each exact
application origin, including local development. Passkeys require HTTPS or a
browser-trusted localhost origin. IP addresses (including `127.0.0.1` and
`[::1]`) cannot be used as WebAuthn relying-party IDs: open `http://localhost`
on the same port for local passkey testing. The picker disables passkey login
and enrollment on unsupported origins and offers a localhost link on loopback. Credentials are scoped to the configured relying
party: do not assume a passkey enrolled on localhost works on a hosted origin.

Signed-out pickers offer **Log in with Google** and **Log in with passkey**.
The Google button starts OAuth directly, without an intermediate Privy login
screen. Returning users reuse their linked embedded wallet; new Google users
get a wallet after Privy installs the verified session. Wallet preparation
failures can be retried with **Use email wallet** without signing in again.
The passkey button also calls the headless login API directly, opening the
device prompt without a second Privy confirmation screen. Dismissing that prompt
leaves the selected owner unchanged and shows no error. The connection picker remains open during passkey sign-in; failures update an inline error in the same modal, alongside retry and other login methods. Passkey signup remains
disabled; users enroll a passkey after signing in with email or Google.
Sign in to the existing account with email first, then open the connection picker
(or **Connect new wallet** in the trading app's native wallet dialog) and choose
**Link Google account** or **Add passkey**. Link before using another method to
preserve the existing embedded wallet. An account already belonging to another
Privy user cannot be silently merged. Linking never selects a different trading
owner or migrates funds. Already-linked Google accounts are labeled **Google linked**;
additional passkeys may be added through Privy's native management modal.

Google redirects retain a short-lived, owner-bound request marker in session
storage. Only a verified Privy login callback for that explicit request may select
the embedded wallet. Cancellation, disconnect, and owner changes invalidate it.
Automated integration tests cover Google/passkey success, cancellation, linking,
conflicts, and Google redirect restoration with the installed wallet bridge.
Live Google consent and device passkey enrollment remain hosted-service checks.

For a selected, authenticated Privy wallet, the native wallet account dialog
includes **Wallet MFA settings**. It opens Privy's own enrollment/management UI
using `useMfaEnrollment().showMfaEnrollmentModal()`. Dismissing it returns to the
wallet dialog without changing the selected owner. Enable the desired methods
in **Privy Dashboard → User management → Authentication → MFA** first; adding
a login passkey alone does not enroll it for wallet MFA. Enrollment is an explicit
user action, and existing enrolled wallets remain protected if app MFA is later disabled.

If passkey MFA is already enabled and another linked passkey is not enrolled,
each unenrolled passkey offers **Enable for wallet MFA** in its own row.
On the device holding an existing MFA passkey (for example, the Mac), click this
action and complete Privy's native MFA verification. The app refreshes the server's
passkey list, then enrolls only the selected key, preserving every existing MFA key
and keeping them available for login (`removeForLogin: false`). The mobile passkey
must already be linked to the same Privy account. This enables that linked key for
wallet MFA without copying its private key between devices. Cancellation, logout,
owner changes, or conflicting passkey changes stop the update before submission.
Unit, installed-wallet-bridge, and offline real-SDK/WebAuthn browser tests cover
preserving both keys, verification before enrollment, failure/retry, and cancellation.

The registered passkey list separates the authenticator/custom name from
**Registered on OS · Browser** metadata. **Rename** stores a cosmetic label in
the API's `passkey_names` table, scoped to the authenticated Privy user and
credential. Names sync when the wallet modal opens or the app regains focus;
clearing a name restores the default label. They do not change the credential
stored on a YubiKey or password manager, login, or wallet MFA enrollment.

Deploy the backend before the frontend for this feature. The API creates the
table idempotently on startup and requires the public `PRIVY_APP_ID` environment
variable to match the frontend app. The Sepolia backend deployment workflow
upserts the existing public GitHub Actions `PRIVY_APP_ID` variable into the API
task definition before deploying its image. When applying Terraform separately,
set `privy_app_id` to that same public app ID; normal image deployments do not run Terraform.
No Privy app secret is required: access tokens are verified against the fixed
app JWKS endpoint with ES256, issuer, audience and expiry checks. API responses
are never cached. Missing authentication/configuration or a failed save remains
visible in the wallet modal; there is no browser-only persistence fallback.

Privy 3.51.2 does not expose native-picker description/account-action slots or a
public connection-modal close hook. Both apps' `postinstall` runs
`scripts/patch-privy-wallet-dialog.mjs` to add these UI extension points.
The patch checks the exact SDK version and complete upstream file checksums,
is idempotent, and fails installation on drift. Review/update it alongside any
Privy upgrade; its node tests and actual-SDK browser tests are required in CI.

The trading app emits bounded `wallet_provider_ready` (including initialization
duration), `wallet_connection_state_changed`, and `wallet_action_failed` events.
These contain no owner addresses, user IDs, login credentials, or raw provider
errors. During rollout, compare readiness latency and connection failures with
the preview baseline. Stop the rollout on any owner mismatch, failed ownership
verification, or inability to sign with the selected wallet. A frontend rollback
does not migrate Privy wallets to Reown; validate the replacement build with a
Privy account before changing the public deployment.

The Insights workflow publishes on matching pushes to `master`; the trading
frontend requires a manual Sepolia dispatch from `master`. Complete preview
validation before merging, then use `gh` to check for existing runs, dispatch
only missing deployments, monitor both workflows, and smoke-test the public
applications. See the repository `AGENTS.md` for authentication and dispatch rules.

## Maximum opening size

The trading ticket calculates Max in one `eth_call`. viem executes the checked-in
`contracts/max-open/PerpsMaxOpenQuoter.sol` helper deploylessly: its creation and
read-only call exist only inside the RPC simulation. There is no persistent
deployment, transaction, wallet signature, or gas payment. It reserves the current
router bounty and optional protection rewards, asks the deployed canonical lens
for its maximum, and applies selected leverage in 100 plDXY increments. Every
internal read uses the same block. Invalid planner candidates, nonpositive equity,
search exhaustion, and provider errors fail without an approximate fallback.

Equivalent quotes share a ten-second frontend cache. A publication timestamp
change alone does not clear or restart the quote; account, side, price, balance,
protection rewards, leverage, and deployment addresses remain part of its key.
Quotes still refresh every fifteen seconds and after trading invalidation. Max
remains disabled during refresh, and a failed refresh hides the previous quote.
Automatic price and balance refreshes retain the last number for display only,
so Max, a filled maximum, and shortcut styling stay steady while fetching. This
display value never supplies executable size or margin; changing account,
direction, leverage, or protection settings clears it immediately.
Review recalculates with its verified block and funding budget, then retains the
existing adverse-price assessments, funding checks, and final simulation.

Order quantity shortcuts show 25%, 50%, 75%, and Max as four equal-width outlined
buttons spanning the full input width. Max fills the full available quantity
without displaying an amount in the button.
It stays highlighted while automatic Max sizing is active, including leverage
changes and quote refreshes; manual entry or a partial shortcut clears selection.
Partial percentages round down to 100 plDXY increments and fill a fixed quantity;
Max keeps automatic sizing. Reductions use the size remaining after pending close
orders, with exact full closes preserved.

The generated ABI and bytecode use Solidity 0.8.35 with pinned settings. Foundry
is needed to rebuild or verify this artifact, but normal frontend builds consume
the checked-in artifact without requiring a compiler:

```bash
node ../../scripts/generate-perps-max-open-quoter.mjs
npm run verify:max-open-quoter
npm run test:max-open-quoter
```

An optional read-only comparison against the canonical contracts verifies full
quote parity and reports RPC counts and timing:

```bash
npm run test:max-open-quote-live -- --help
```

## TradingView Advanced Charts

The plDXY Perpetual panel uses TradingView Advanced Charts exclusively. If its
licensed runtime assets are unavailable, the panel shows an explicit unavailable
state instead of switching chart engines.

Clone the private TradingView repository outside this public repository, then
install the runtime assets locally:

```bash
git clone --depth 1 --branch v32.0.0 git@github.com:tradingview/charting_library.git /tmp/tradingview-charting_library
npm run tradingview:install -- /tmp/tradingview-charting_library
npm run dev
```

`public/charting_library/` is intentionally ignored by Git. Never commit,
publish as source, or attach those files to a public CI artifact. Production
deployments fetch version `v32.0.0` (pinned to commit
`f2a61ba473ec254b69f9c1377c67e4b81eff853b`) with the read-only
`TRADINGVIEW_GITHUB_TOKEN` GitHub Actions secret and copy only the runtime
assets into the deployed site.

Before enabling Advanced Charts in production:

- have counsel review the public Terms of Service and the rights to redistribute
  the market data displayed by the chart;
- have counsel confirm that the proprietary runtime can be loaded alongside
  this AGPL-licensed frontend under the intended deployment model, or document
  an approved licensing exception or isolation strategy;
- publish the required announcement about the licensed TradingView integration, or
  obtain TradingView's written approval for an alternative promotional
  placement that includes a contextual backlink;
- keep both the chart's built-in attribution and the site-wide "Charts by
  TradingView" link visible; and
- ensure TradingView can access the public implementation for compliance review.

## Managed gas sponsorship

The Arbitrum Sepolia integration currently uses a deterministic
permissionless.js SimpleAccount v0.8 as the Trading Account. The connected
wallet is its owner and is never used as a fallback transaction sender.

This account choice is for managed testnet testing only. The official
SimpleAccount is upgradeable and does not satisfy the core handoff's
production requirement for immutable execution semantics. Production remains
disabled until a reviewed immutable account deployment replaces it. Treat
these testnet Trading Accounts as disposable because a replacement factory
will derive different addresses.

Local and hosted testnet deployments use the bundled
`/perps-aa-manifest.json` by default. Set `VITE_PERPS_AA_MANIFEST_URL` only to
override that path. The manifest suffix identifies a deployment generation,
not the AA provider. The current bundled manifest is
`perps-aa-arbitrum-sepolia-20261012-v131-v2` and deliberately uses Pimlico through
the exact same-origin `pimlicoRpcUrl` field.

Transport is selected only after exact-key validation. A v1 suffix accepts
only the Pimlico field set. A v2 suffix accepts either that same exclusive
Pimlico field set or the native field set: exact same-origin `bundlerRpcUrl`
and `paymasterRpcUrl` values plus `paymasterAddress` and
`paymasterVersion`, with no `pimlicoRpcUrl`. Partial and hybrid shapes are
rejected; the `-v2` suffix alone never selects the native provider. For the
native shape, the frontend also validates every sponsorship envelope against
the reviewed policy ID, SimpleAccount proxy runtime hash, 100,000/0 paymaster
gas limits, and 600-second maximum validity window pinned in the client. The
backend validates Plether call targets and relays only the required bundler,
paymaster, and diagnostic-status methods.

Configure `AA_PROXY_ORIGIN_TOKEN` as a Cloudflare Pages secret and require its
`X-Plether-AA-Proxy-Token` value on the legacy `/api/aa/pimlico` and native
`/api/aa/rpc` backend routes. This keeps direct callers from spoofing the
client IP used for rate limits. The testnet deploy workflow provisions the
Pages value from the GitHub Actions secret with the same name; the backend
must receive the matching secret. Generate it with `openssl rand -hex 32`;
both sides require exactly 64 lowercase hexadecimal characters and reject
known placeholder values.

Configure a separate `FAUCET_PROXY_ORIGIN_TOKEN` Pages secret for the exact
`/api/perps/v1/testnet/faucet` route. The Worker removes any caller-supplied
`X-Plether-Faucet-Proxy-Token`, injects the Pages secret, and preserves
Cloudflare's client-IP header for the backend's pseudonymous hourly quotas.
Never reuse the AA token for the faucet.

Before `eth_sendUserOperation`, the frontend atomically persists the locally
computed hash with the signed UserOperation preimage. Recovery parses that
preimage, recomputes and matches the exact hash, and derives the nonce and
nonzero deadline from an exact trusted legacy or Plether paymaster envelope.
Bundler statuses are diagnostic only after an exact receipt miss. A receipt
becomes terminal evidence only
after its transaction and EntryPoint event are verified against the canonical
RPC at or below the safe head. Bundler outages remain inconclusive but do not
prevent independent safe-chain nonce and expiry recovery.

The trade ticket may progress earlier from an exact canonical-latest receipt
after its matching EntryPoint event and expected protocol event are validated.
That interactive inclusion signal does not mark the sponsored operation
confirmed, persist a verified transaction hash, clear authorization state, or
release its lane; all retry and recovery decisions continue to require the
safe-chain evidence above.

At one safe block, recovery reads the timestamp and EntryPoint nonce for the
key encoded in the nonce's upper 192 bits. An unchanged nonce after the
verified deadline is retry-safe expired. An advanced nonce without a verified
event releases the lane as outcome unknown and non-retryable because the old
nonce cannot land.

The backend's Alchemy receipt lookup locates historical UserOperations. Every
receipt is independently bound to the exact hash, sender, EntryPoint,
transaction receipt, safe block, and unique canonical `UserOperationEvent`.
Null receipts are not treated as retry authorization, and provider or corrupt-
data failures remain inconclusive. Legacy hash-only records and old diagnostic terminal labels are
re-locked by storage migration, backfilled into their directly addressed lane
head while holding that lane's browser lock, and never auto-expire. Each one
instead exposes a force-release escape hatch warning that the old action may
already have executed or may still execute later. It requires reviewing the
Trading Account and operation hash and explicitly accepting the risk before
repeating the action. Before retrying, close or reload every other Plether tab
so an already-open legacy client cannot restore the obsolete shared-store lock.

Submission, stale-record recovery, and manual release share a browser-wide
lane lock. The app refuses to send unless its exact hash and signed preimage
were durably written to a dedicated per-operation recovery journal and a
directly addressable chain/account/lane head. Under the lane lock, submission
and recovery read that head directly rather than depending on storage-key
enumeration. The conservative head is written before its journals, making a
crash between durable writes fail closed. Cross-tab hydration merges journals
with the old shared snapshot, but version 1 treats that whole-store key as a
read-only legacy inbox. Mutable current persistence uses per-operation
journals; only the locked migration and submission paths mutate lane heads.
Manual and canonical resolutions are written first to append-only
ID/hash/status tombstones and overlaid on every recovery read. This prevents
any current tab—not just the submitting one—from erasing legacy evidence or
rewriting a resolved operation into a blocker through a stale write.
Hash-verified records can continue recovery across a metadata-only manifest
version bump; a changed EntryPoint still fails hash verification closed.
Operations are never automatically rebuilt or sent through the owner EOA.

## Networks

- Mainnet (chainId 1)
- Sepolia (chainId 11155111)
- Arbitrum Sepolia (chainId 421614)
- Anvil local fork (chainId 31337) via `npm run anvil`

## License

AGPL-3.0-only

### Preparing a MockUSDG perps release

The frontend selects `config/perps/arbitrum-sepolia-v2.0.0.json`: canonical schema 4,
order interface 3 and six-decimal MockUSDG metadata, with the complete deployed
contract graph. AA trading/funding requires a separately configured paymaster
and backend rollout. Spot remains separately configured. Historical USDC manifests are archived evidence; current perps
consumers do not accept their keys or ABI.

The breaking SDK 0.4.0 and current APIs use `settlementToken`, `Assets` and
`AssetAtoms`, including `settlementTokenSupportsEip3009: false` and null EIP-712
domain settings. No compatibility aliases remain. After a separately reviewed
activation, the funding flow mints 100,000 MockUSDG to the confirmed Trading
Account, checks the token runtime hash, and deposits with exact approval. The
owner wallet pays testnet ETH and confirms the mint; a failed deposit can reuse
the minted balance. Mock bridge funding and GDN rewards are unavailable.

Use `node scripts/prepare-perps-release.mjs` from the repository root to generate
a candidate only after verified deployed evidence exists. Read
[`docs/runbooks/perps-mock-usdg.md`](../../docs/runbooks/perps-mock-usdg.md) for
package/ABI provenance, activation guards and the unapplied SQL migration.
