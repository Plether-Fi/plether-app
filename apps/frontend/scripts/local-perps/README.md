# Local TP/SL market

A disposable Anvil fork for manually testing real PositionProtectionBook
transitions with the production `ProtectionInputs`, `PositionProtectionManager`,
and `preparePerpsOrderV2` code. This is **not** an end-to-end test of the full
trade ticket, managed AA sponsorship, Haskell API, PostgreSQL history indexer,
or production protection worker. The local controller replaces those services
with direct test-account transactions and explicit keeper controls.

## Start

Requirements: Node/npm dependencies installed in `apps/frontend`, Foundry
(Anvil and Solc 0.8.35), and the official v1.2.2 release ABI bundle.

Download the bundle using `gh release download v1.2.2 --repo
Plether-Fi/plether-core --pattern perps-arbitrum-sepolia-v1.2.2.zip --dir
/your/temporary/directory`. The controller verifies its pinned SHA-256 digest.

Start a **new**, dedicated fork:

```sh
anvil --fork-url https://sepolia-rollup.arbitrum.io/rpc \
  --chain-id 421614 --host 127.0.0.1 --port 18545 \
  --allow-origin http://127.0.0.1:5182 --block-time 2 --silent
```

In another terminal, from `apps/frontend`:

```sh
LOCAL_PERPS_ABI_BUNDLE=/absolute/path/perps-arbitrum-sepolia-v1.2.2.zip \
  node scripts/local-perps.mjs
```

Open <http://127.0.0.1:5182/local-market.html>. Do not substitute `localhost`:
the controller enforces the exact loopback Host and Origin for mutations.

The controller checks the Anvil client, chain ID and pinned contract code
hashes before writing. It never accepts a configurable execution RPC. It
impersonates accounts **only on the fork**, replaces only Pyth's code with a
local price fixture, advances the chain clock to a Monday, seeds the pool if
necessary, activates local trading, and deposits 100,000 mock USDC for the
default Anvil trader. Production oracle, router, engine and Book code remain
unchanged. No real wallet, keys or public-chain writes are used.

## Try it

1. Start at displayed price 1.00, long size 2,000 and margin 500 USDC. Change
   either price or percentage input and submit **Open with TP/SL on Anvil**.
2. Wait for **Active**. Set the market price to 1.11 to cross the default TP,
   or 0.89 to cross the default SL. Auto-execution closes the position.
3. To inspect intermediate states, disable auto-execution after the position
   becomes Active. Crossing a trigger now leaves **Close queued** visible.
4. Select **Expire queued order** to advance past expiry and make the actual
   close attempt fail. The Book becomes **Close delayed** (Latched).
5. Select **Retry latched close**, then **Execute queued order** to close it.
6. Use **Reset test market** to restore this controller's funded starting
   snapshot. This discards only transactions made on this disposable fork
   after initialization. Restarting the controller reinitializes funding and
   its baseline; start a fresh Anvil process for a fully clean deployment fork.

The protection manager also supports actual create/replace/cancel transactions.
The transaction log contains local hashes; do not look them up on Sepolia.
The frontend schedule banner is not used: the sandbox displays contract state.

## Verify

```sh
node scripts/local-perps-smoke.mjs
```

This resets the sandbox and tests long TP, short SL, and expiry/latched/retry
against on-chain state. It restores the starting snapshot on success. Do not
run it concurrently with a manual test you want to preserve.

Mock Pyth intentionally bypasses signature/freshness verification at the Pyth
boundary and synthesizes historical ticks from the selected test price. This
is appropriate for deterministic frontend/Book lifecycle tests, not oracle,
historical-price integrity, MEV, sponsorship or production keeper validation.
The executor supplies a 15-million gas limit because the router's low-gas
pending path makes bare gas estimation insufficient for deterministic execution.

Stop both terminals with Ctrl-C when finished. The Anvil state is ephemeral.

## Account-risk regression suite

With the same dedicated fork and controller running, use Node 22.18 or newer
(native TypeScript stripping) and run from `apps/frontend`:

```sh
LOCAL_PERPS_ABI_BUNDLE=/absolute/path/perps-arbitrum-sepolia-v1.2.2.zip \
  npm run test:local-perps-risk
```

This suite checks the checksum-verified v1.2.2 deployed bytecode and compares
the frontend threshold with `previewLiquidation` at the boundary and adjacent
healthy tick. It covers both directions, open-preview agreement, deposits,
margin additions, withdrawal limits, exact entry-cost rounding after real
increases and partial reductions, and settlement of a real partial-close claim
into live position margin. It resets this disposable sandbox before each
scenario and on completion. Do not run it concurrently with other fork tests.

The claim scenario temporarily removes pool cash by impersonating the pool on
Anvil, then restores that cash before settlement. Execution never accepts a
configurable RPC and is restricted to `127.0.0.1:18545`; it cannot write to the
public deployment. If the public RPC no longer serves the fork's original
state, restart both test processes with a fresh fork.

See [the accounting and verification record](../../../../docs/runbooks/perps-account-risk-v1.2.2.md)
for the pinned Solidity vectors and the frontend snapshot policy.
