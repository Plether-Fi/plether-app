# Zero-close keeper and owner fallback

This release accompanies plether-core `codex/zero-close-bounties`, based on core master `fd4e0395505079dadfafc5e3a5a2ec55135d8f35`. The app branch starts at `7d4205c5a2ab09004c9ceead535a7e9b1e06716b`. Publish a fresh verified core manifest before activation; do not rewrite archived release evidence or rotate only part of the accounting stack.

Close execution rewards are permanently zero in the new stack. Open/increase execution rewards, the one-time protection trigger reward, liquidation rewards and trading charges remain. There is no prepaid close credit, automatic protocol subsidy contract or LP-backed keeper expense.

## Required operator configuration

Set a positive `KEEPER_MAX_TX_COST_WEI` for the Haskell keeper and `PROTECTION_WORKER_MAX_TX_COST_WEI` for the protection worker. **Both default to zero and therefore refuse new signed transactions until configured**, including if this service code is deployed against an older stack. Budget is a per-transaction cap on native value plus gas limit times maximum gas price, not a daily allowance. Existing journals still reconcile/rebroadcast previously signed bytes. Fund independent primary/standby native wallets and retain wallet-runway alerts. The contract never checks this operating budget when admitting a close.

Monitor canonical FIFO heads including unpaid closes. Do not skip a head for a paid successor. A mismatch between indexed and canonical queue state requires indexer catch-up and alerting. Simulate oracle-independent terminal cleanup separately before historical oracle work. Expiry is strictly after the stored deadline; equality is live. An oracle fee failure cannot block expiry cleanup. Monitor protection status Triggered until its linked attempt actually finalizes. Latched expiry retries use fresh orders/deadlines and do not repay the trigger reward. Other failures require reason-specific review.

Use existing nonce and signed-transaction journals on restart/failover. An ambiguous provider response does not authorize a different replacement payload. Reconcile chain inclusion, reorgs and terminal receipts. A successful transaction with Pending or partial batch progress does not mean all orders executed. Keep separate primary/standby identities rather than uncontrolled concurrent writers on one nonce lane.

## Owner fallback

The review ticket offers native-gas close commitment through the existing owner-authorized SimpleAccount v0.8. It verifies the account's factory-derived address, owner, EntryPoint and implementation plus core bindings. The owner EOA pays gas but the Router sees the original Trading Account. No USDC approval, deposit, protocol API, sponsor or bundler is required. Opens retain the normal sponsored path.

Finalization is a second permissionless transaction using the committed order ID, exact oracle native fee and independent public Hermes historical six-feed payload. It authenticates the Book receipt. A preceding live FIFO head must execute or expire first; the ticket offers an explicit separate processing action and does not mark the user's order complete when that predecessor finishes. After strict expiry, cleanup requires no oracle bytes. Protection controls offer owner-authorized armed cancellation and permissionless retry/finalization. Cancellation leaves the position open; retry only queues an attempt.

The owner must have native gas. RPC or chain outages remain blockers; live execution also needs valid historical oracle data. Arbitrary wallet implementations and undeployed accounts are unsupported by this fallback. Unknown submissions must be reconciled by transaction hash and immutable client intent before creating another reviewed order. Wallet rejection and insufficient native funds do not withdraw collateral or change the trader identity.

## Activation and incidents

Local unit, PostgreSQL recovery and real-account fork regressions are implementation evidence, not a funded production rehearsal. Before activation: verify fresh deployed bindings; configure caps; fund primary and standby; measure gas/oracle costs under normal, peak, high-fee, low-opening-volume and repeated-failure workloads; select wallet thresholds and latency targets; disable keeper/sponsor/API and demonstrate same-account close/expiry/retry through independent RPC/Hermes. Exercise primary loss and journal recovery. Keep old-stack service until old entitlements drain.

If operator funding runs out, alert, use a funded standby or replenish, and expose the native fallback. Do not introduce a mandatory close bounty as an emergency workaround. If overloaded, use existing open-pause controls where appropriate while continuing close/cleanup service. No unlimited-load liveness or unmeasured fee-revenue sustainability is promised. Full accounting and queue rules remain authoritative.
