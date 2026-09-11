# Frankfurt trading activation — 2026-09-11

User requested the full localhost trading experience after a successful sponsored
margin deposit. Scope remains Frankfurt `eu-central-1`, Arbitrum Sepolia, existing
Core v1.2.3 (`ffe45937b7f38133133ad292c5435828bf99357d`), and the single approved
owner `0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B`. No Core deployment, hosted
frontend, DNS, Singapore mutation, or global AA rollout.

Read-only ECS inventory confirmed Singapore's active router, oracle, token and
pool are different from Frankfurt's v1.2.3 targets. The pinned worker image was
inspected offline: its oracle script and embedded manifest target v1.2.3.

## Funding

Owner approved a MetaMask transfer of 0.0085 Arbitrum Sepolia test ETH to the
existing setup wallet `0x4dB6E8d6f9cd43D3e3e1cdC2eB9C9A4C41D1795c`; its balance
increased from `946163882386000` to `9446163882386000` wei. Owner funding remains
below the existing 0.1 test-ETH ceiling (prior recorded outflow approximately
0.0900478 plus this transfer and fees; retrieve exact owner transfer receipt for
the final ledger). No private wallet keys left MetaMask.

Dedicated runtime keys are distinct and stored in encrypted Frankfurt SSM.
Operator-only input envelope: `/plether/bootstrap/sepolia-aa-temp/trading-inputs`.
Bounded funding helper distributed 0.008 test ETH from the existing setup wallet:

| Worker | Address | Test ETH | Transaction |
| --- | --- | --- | --- |
| Oracle | `0x12Fb09e86d263Cd43a118fC25bAde50931d1484B` | 0.003 | `0x753337bdf429cd75690cfd4abe06a0aa98d695453e0068d488b989b03b6e5dce` |
| Keeper | `0x1092D102f615d451d3f82eF8CD283eB1a3e2c051` | 0.002 | `0x67546b4b67b0be70715062254505cd07ebbf657e55a213cc912c407d7adeddb9` |
| Liquidation | `0x1a0Fc970E63EaE447bc1E4ba54f0c960E696643b` | 0.001 | `0xf6754ec6b25e3c3428d196705bba181a3ae661931cf0d4871a6804c1d38b9664` |
| Faucet | `0x4F4fB042d10737BAb20c3e1C195575a4cDD36285` | 0.0005 | `0xf516e7a7c1912e93e156b56e0086801deb75b972019c16db9c267bfe3224a65f` |
| LP settlement | `0x37492D21a64Ed2Fc1E841EA2FC294cC0aaff6D4b` | 0.001 | `0x23d1271ef430d42d6424a0767e1306bd0e70a1d799f04b1ff7e083c76526967f` |
| Protection | `0xf6A3034e81F4cf3D408E3e67195A29121882deFB` | 0.0005 | `0x2c5a6c1ffeab7a621085ac817b37b443177274f2e8a5099884b7f1df9d619158` |

## Staged infrastructure

Added explicit `trading-prepared` and `trading-canary` stages. Prepared creates
credentials/task definitions but keeps new workers stopped. Canary starts exactly
one consolidated worker, liquidation worker and protection worker; native AA stays
single-wallet. Faucet uses a distinct server-side origin token, five accepted
requests per client/hour and ten globally/hour. Registration stays off.

Prepared plan SHA `f682eced0a555482ee71b0d9adc396f4639bf0ef9f02825eb3618c40306dd605`
applied task definitions and four SSM parameters, with exact API secret access.
Verified/promoted API 7, workers 3, liquidation 2, protection 3 while new workers
remained at zero replicas; all secrets and images point only to Frankfurt.

Activation plan SHA `049a17d0595bd07fbc6dc05fc577977c7eec586ea42c5d1c654c37f866651195`
started those workers and created alarms. Deep comparison verified API 8 and
protection 4 each changed only its protection-enable flag before promotion.

LP preflight task `173e0329d741419fa2cb21eecb8a6cd0`, definition
`plether-sepolia-aa-temp-lp-preflight:1`, ran only the read-only keeper preflight
and log router; both exited zero. LP signer has 0.001 test ETH versus required
0.0008 reserve at the configured 0.0001 per-transaction cap. Actual matured
settlement affordability remains to be exercised.

Oracle initially failed to fetch its private API. The cached Pyth endpoint itself
returned fresh data. Added an identity-only worker security group to permit HTTP
80 to the private ALB only from that worker group and the existing relay; no CIDR
or public ingress. Combined network/LP-execution plan SHA
`e89c18c2942322b4e9b22f2681ad66e574ad757bb8509a6ef8f4bee9ffb43356`
was reviewed and applied. Deep comparison verified workers revision 4 changed
only LP execution mode; it was promoted with the private API client group.
All four trading/API services passed the ECS stable waiter. An
`oracle_update_mined` event at 09:46 UTC confirms the first live mark update.

The five-minute oracle cadence left the UI stale (its freshness threshold is
60 seconds). Reviewed plan SHA
`80800f11964c6e5970aa1d88f1f477583588395b6050dcfe48036afb23365db4`
changed only the oracle poll interval to 30 seconds. Applied and promoted workers
revision 5 after deep comparison; its rollout is being checked. The faster cadence
uses the existing bounded signer balance and does not authorize more owner funds.

Protection's 0.0005 allocation was below its hardcoded 0.001 alert threshold.
Moved 0.0006 existing setup ETH to that signer (resulting balance 0.0011), transaction
`0x77b6d784d67aa1d299522787a6ff033d006aa8611b22178ee735236c43ed0b7b`,
fee `6096621280000` wei. No additional owner funding. One-off preflight exited
successfully and its exact temporary definition was deregistered (INACTIVE).

CloudWatch test-metric-filter proved the heartbeat filter `caughtUp = true`
matches zero real-boolean fixtures, while `caughtUp IS TRUE` matches one.
Reviewed Frankfurt-only plan SHA
`74b16a5cf55494ed470823bd04056c51598543ac96b865544edd2605a133c72b`
changed only that filter and applied successfully. The heartbeat alarm is now OK;
the failure alarm still needs a clean observation window. No Singapore alarm was
changed. Workers revision 5 subsequently reached COMPLETED with one running task.

29 mocked Terraform tests and 11 local profile/secret tests passed. Worktree changes
are local, not a protected GitHub deployment or pushed release.

## Normal liquidity request

Verified exact runtime hashes for token, pool and junior vault. Pool is active,
unpaused and seeded, but held only 0.02 mock USDC. Requested a normal asynchronous
junior LP deposit of 100,000 mock USDC, with the approved owner as controller.
No governance bypass or owner-only accounting operation was used.

- Mint: `0x59125b6d4d28c2a5aa6ea7edaad14b0fc273da7c99ce17d23a0e2342ad46fe25`.
- Exact allowance: `0xcd095abd9be21fd4b6eb96619266c73f31d28d598ce4983b19c6cb07836a904b`.
- Request: `0x98cfc9a06b8fa383f591bfa39f09af0623c381da77764206b978b7ab7e24eb22`.
- Request fee: `163818218862000` wei. The initial 0.0001 fee cap stopped before
  sending; after measuring approximately 0.00024 maximum cost, a 0.0003 cap was
  used for the LP request only. Mint/approval were not repeated.
- Epoch `496978`; cutoff `1789120500`; settlement eligible at `1789120800`
  (2026-09-11 12:00 Europe/Warsaw). Settlement not yet verified.

At 10:00 UTC, the worker selected matured epoch 496978 but did not broadcast:
575943 gas at max fee 358525000 wei produced a 206489964075000-wei maximum
cost, above the initial 100000000000000-wei cap. Raised the proposed cap to
250000000000000 wei, preserving the eight-times-cap startup reserve.
Approved a further 0.0008 test-ETH transfer from owner to the setup wallet
in MetaMask (Arbitrum Sepolia, nonce 113223, max fee below 0.0001); total recorded
owner funding remains under 0.1 test ETH. Exact receipt hash is still to be added.
Transferred 0.0012 setup ETH to the existing LP signer, reaching 0.0022 ETH:
`0x802f99f048ba994f0f2caa78d74dc0ee8d1fcbab26783fb58126ce7cb6f57d25`,
fee `6078983724000` wei. Reviewed cap plan SHA
`c1b7c07ca90e4cef728ea90d13b833a9ab68d9f6d2fac8d1335a914903f1af69`
applied; workers revision 6 was deep-compared and promoted. It passed startup
audit, broadcast and confirmed settlement
`0xd64033b2238fea0a1a690628c8c60894997e356ee4ec8e4f7f249f518d8e9943`
at approximately 10:09 UTC. Junior assets read `100000009998` micro-USDC,
pending escrow zero; browser pool liquidity 100K. No LP request was repeated.

## Extended runtime funding (explicit approval)

User explicitly raised the total owner funding ceiling from 0.1 to **0.15 test
ETH** during this turn. Oracle had 0.000255579310794 remaining and could not
afford the estimated next update; normal 30-second cadence consumes more gas.
MetaMask approved 0.025 Arbitrum Sepolia ETH to setup (nonce 113227, max fee below
0.0001). Total recorded owner funding is approximately 0.12435 plus small
additional fees, below the new ceiling; retrieve exact funding receipts for the
final ledger. No mainnet value was moved.

- Oracle additional 0.02 ETH:
  `0xbf0daa3f258af37c4f0eba261f26412f410547cb9e3b1132c54ec16bb2234eb5`,
  fee `6143177106000` wei.
- Keeper additional 0.003 ETH (resulting allocation approximately 0.005):
  `0x76c7286830aa5db3f0ef04503657c637985165908886ea3c2310ca3e25632e51`,
  fee `6084388720000` wei.
- Other worker allocations unchanged. Funding helper refuses automatic retries
  after target balances change. Oracle freshness returned in Firefox.

## Browser checks in progress

Restarted localhost Vite with faucet origin token only in the server process.
Firefox's Get mock USDC button now submitted faucet transaction
`0x1289cb66de93444fd6327a31c6f771bb05a965cfe8782c6fbc631b2538742ef1`
for Trading Account `0x9314586D4068C73B23a64d7406Ca8FfEeCc2cBFc`; browser confirmation
succeeded with 100,000 mock USDC minted. A sponsored 1,000 mock-USDC deposit
(UserOp nonce 101) succeeded, with the browser showing 1,002 USDC margin and no
position. This included the user's independent extra 1-USDC deposit. Do not
repeat any owner transfer or the faucet/deposit operation.

Nonce-101 receipt: UserOp
`0x96c1f05a6220954c97becd70e11021d59265d1e6e5aa564fb5884ef1fa9506eb`,
transaction `0x4a82806601755287f255a437de3c9bf02c766c15b1130400095c847962d08c1a`,
block `307722479`, success true, actual sponsored cost `49560788164320` wei.

Installed the existing local licensed TradingView v32.0.0 runtime using the
repository installer, from worktree `253e`. Assets are git-ignored and were not
uploaded. Standalone JS SHA-256 is
`61b43e76ced4746506cdd10bae305103168c89545b2fa9df49d8d417b32576d6`.
The chart now loads, but its candle API returns NOT_FOUND because strict rollup
reads are disabled. It requires a separate gated candle rollout; no fake prices,
legacy fallback, or incomplete coverage was substituted.

First long test: 1100 plDXY, about 1109 USDC exposure, 5x reviewed leverage,
approximately 220 USDC margin, 0.1% slippage. Sponsorship/EntryPoint inclusion
worked, but the inner call failed with `OrderRouter__InvalidValidUntil`
(`0xe37e62c6`): signed expiry `1789121478`, inclusion timestamp `1789121494`
(16 seconds late during the operator's slow signing sequence).
No order or position opened; margin stayed 1002 USDC.

- UserOp `0x04dc4d3407d8a504f586f22bff74b6f9e347191ac802c2174b5f602b0141bb25`.
- Transaction `0xff0b9d8d48b0eff3e1fed3ab35ad94a5cc2e14e1d5d6fc53bc5388eae311e08f`.
- Block `307730057`, nonce 102, success false, sponsored cost `69893052215656` wei.
- Client order ID `0xd2fcd416f54261ebe4d8d6699686c7e85a56f7feaa08233674b562d40546e235`.
- Latest/safe RPC timestamps differed by 674 seconds when checked; retry stays
  locked until canonical-safe confirmation. Do not bypass or clear the journal.

The failed nonce-102 receipt became safe before retry. One fresh review stopped
before signing because the live bounty exceeded its exact reviewed cap; nothing
was submitted for that review. A subsequent prompt signing sequence succeeded:

- Open **order 1**, 1100 plDXY long, 5x reviewed leverage, execution about 1.0089
  USDC, approximately 219.7 margin, displayed settlement balance 1001.4 USDC.
- Commit transaction
  `0x15d4386f1e9def4480bc3c669669576fac1d395a72462a0348e22768a05f51b5`,
  block 307733646, UserOp nonce 103, success true, sponsored cost
  `397261864393784` wei.
- UserOp `0x6244799d04ba90a4559561dd5d3d61d7b0006f25f494aa9d56fba681c1e09118`.
- Automatic keeper execution
  `0x76a1f7d7532c5c912b6647effca77cb0aa13aa15bec2fae6f4e0523c0f00c869`.
- Firefox visibly reported "Trade executed", order 1, and the live position.

The first full reduce-only close attempt also expired during native wallet UI
automation. **Position remains open until the close retry is confirmed.**

- Close UserOp `0xbf9407e5b1beea435637b750fff3e8b6f796aa78c9a81408b6cc456cb2244954`.
- Transaction `0xf6b656e16ca9b4bb5b9f0f719d92071c9885e69e118bdd07cc466d955fac56fd`,
  block 307734116, nonce 104, success false, revert `0xe37e62c6`, sponsored cost
  `72148682729980` wei.
- Contract `maxOrderAge()` is 60 seconds. A proposed temporary increase was
  investigated but **not performed**: exact v1.2.3 OrderRouterAdmin source has a
  48-hour configuration timelock, so it cannot fix today's smoke test.
- Preserve the operation journal and safe-head retry lock. No governance proposal
  or Core configuration change was sent.

Nonce 104 subsequently reached safe confirmation (safe block 307734122).
The bounded automatic close retry stopped at its initial UI precheck because
the active browser session and account balance had changed; it did not request
a signature or submit another transaction. The last UI inspection still showed
a 1100 plDXY long position. This is historical smoke-test evidence, not a claim
about the account's current position after subsequent manual testing.

The user explicitly chose to retain the 60-second order window. No change to
that limit is planned here. A successful sponsored open-and-close round trip,
chart candle rollout, and end-to-end protection/liquidation tests remain
unverified. The protected Frankfurt candle rollout still needs approval.
Frontend RPC configuration and request-volume optimization were deferred by the
user to another branch; no such changes are included in this work.
