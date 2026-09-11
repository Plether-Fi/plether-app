# Frankfurt paymaster deployment — 2026-09-11

The user authorized continuing until a test transaction can be sent, including
wallet approvals, with a total ceiling of 0.1 Arbitrum Sepolia test ETH. No mainnet,
Core deployment, Singapore mutation, hosted frontend or DNS is authorized.

## Provenance and identity

- Exact upstream AA commit: `f9e29c1b3ac5937e0519108cebffb4d09048de36`.
- Isolated source archive; Solidity 0.8.35, optimizer 200, via-IR.
- 30 unit tests and both live Arbitrum Sepolia fork tests passed before deployment.
- Artifact SHA-256: `653bb0a8dee34613949cf5c727f86598158e6099b4bf11354cb703cd22718870`.
- KMS fixed-digest attestation task `a5ee2663a7fe4101a5d7b0e41aba7bc5`: all containers exit 0.
- Signer: `0x015736E1F47E37938236e481F7a3B7c57F922b80`.
- Dedicated deployer: `0x4dB6E8d6f9cd43D3e3e1cdC2eB9C9A4C41D1795c`.
  Its private key is an operator-only SecureString outside the ECS runtime namespace;
  no key was printed, exported to the browser, or written to local files.
- Owner: `0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B`.
- Paymaster: `0x9761091045616A388f5fE1433721B272c78fe31b`.
- Deployment block: `307684600`, hash
  `0x64210ad75de20ddd2ccf494e7c4d042447f4ea48119419945637f17c271904fe`.
- Runtime code hash: `0xb8ae276b01850fdbb8d9d7fd32ec7b9b1c7ab7af20f5d62179a76f6b4912c528`.

The bounded deployment helper independently checked chain ID, all three upstream
profile runtime hashes, the artifact hash, zero deployer nonce, gas ceiling and
funding reserve. Simulation succeeded before broadcast. Readback verified the
owner, signer, policy, account profile, 0.01 ETH maximum cost and paused/empty state.

## Confirmed transactions

| Purpose | Value (test ETH) | Transaction |
| --- | ---: | --- |
| Owner → dedicated deployer | 0.078 | `0x54462f31490b538b623ecc854aba510f6716e383fec1dd52c939ebac8dad34c2` |
| Deploy paused paymaster | 0 | `0x8e74127a63de69896907f20212dad8e475e9710f570d665290223a8d5c488c06` |
| Deployer → paymaster deposit | 0.05 | `0xac99340a37c6aef067586de76f6e5a1d8500869dd83bba5e7792078924081e63` |
| Deployer → Alto utility | 0.0265 | `0x92145e67ce7ecda793447cffcc880d66532addb92d2ab09e9bf847b6370f3bb0` |
| Owner → paymaster stake, 86400s delay | 0.001 | `0xaaa2e303af84ee374864c4feb509c914520c6f7374da943a9ce5145097d93648` |
| Owner → deposit buffer | 0.003 | `0x5f280a805f06c9e7443d0dad56f57bf6294eccb47987167ad922ec283bdefb19` |
| Owner → Alto utility reserve after simulation deployment | 0.008 | `0x916b31c008a464d90f57a88a1027f4004fff3398d66158159ab391d4f3e800e6` |

Owner outflow is **0.090040598567112 test ETH**, including the four owner
transaction fees. Deployer transactions spend from that allocation, not an
additional owner allocation. After deposit/utility transfers, deployer balance
is 0.000960929257614 ETH. EntryPoint readback: deposit 0.053 ETH, stake 0.001 ETH,
`staked=true`, delay 86400, withdrawal time 0. Paymaster remains paused.

## Pending qualification

Alto simulation bootstrap task `66934a8b64bd4babb611902fbee08e0b` used pinned
revision 2, manual bundling, no executor refills/utility monitoring, and no
load-balancer registration. It reached server-ready and was explicitly stopped;
STOPPED state was verified. Utility-wallet deployment costs were 0.005278654332196
test ETH; the subsequent reserve transfer restored its balance to
0.029221345667804 test ETH before executor refills.

Verified simulation contracts:

- v0.8: `0x9c3c25a084AE8B1df3B2e82bb07Dac4E115C9Ae1`, runtime hash
  `0x6b70d0e97a70a3d5a1c2011e438ac2aa05932687259379723b3827cb199e831a`.
- Pimlico simulation helper: `0x95CC02A7B69dD46c6DD6Bd56132A24a235D58948`, runtime hash
  `0xbf6f6629d08dd42ef5af9e7ba6ff5e1e7b53083f035d2f457fd8d4561182fcbd`.

22 mocked Terraform guard tests passed. Definitions-only plan SHA-256
`82d86c57f22eb9d09b722fa85c095cd0a61bda32c960540b100166a921f4b599`
was structurally reviewed and applied: three new task revisions, one guard update,
no count/IAM/network/database changes. Old revisions were deregistered, not data
deleted; ECS keeps their definitions available for inspection.

Qualification plan `f8ca4a3ca877cd367914912e9fadb6912b82b85d9dfeb1a4ba9b50e2d7c666bb`
was subsequently reviewed and applied. API, Alto and reconciler revision 3 were
promoted; all other seven services remain at zero. API health is 200. Origin
authentication rejects missing credentials (403); authenticated EntryPoint and
Alto gas-price discovery succeed; JSON-RPC batches are rejected (400). API signing
permissions, sponsorship and submission remain disabled. Paymaster remains paused.

The reconciler caught up, then failed closed when Alchemy's safe timestamp became
more than 600 seconds old. The user explicitly approved a temporary 1800-second
allowance for this testnet canary. Backend fix commit
`ab272612c29b4febd502bc2735e7e0d4d560fb21` shares the existing reconciler setting
with every gateway snapshot/revalidation check. Defaults stay 600; larger windows
require an allowlisted Arbitrum Sepolia cohort with global rollout off. Terraform
further restricts the exception to the exact Frankfurt deployment and owner.
Safe-block agreement, future-time checks, canonicality and reservation accounting
are unchanged. Validation: 1041 backend unit examples, 8 mandatory PostgreSQL AA
integration examples and 24 mocked Terraform guard tests passed. ARM64 image
`sha256:3078940d5bc82cadadf884e9ec1057059920c24ac99f5ec1bd8ef6561c4173c8`
was published and read back from the isolated API ECR repository; source tree is
`8c13f2d4f062e75bd6722d606f0e577f8aecd42e`. The existing build recipe and Core
v1.2.3 manifest are unchanged. A rebuilt log-router image was published by the
preparation helper but is not used by this rollout.

Plan SHA-256 `3b425f1906e46799b740169fd06d5256f35729cd74dfcfe88e6778fbb5b60021`
passed the strict two-definition review and was applied. Only API and reconciler
definitions changed: the image and shared safe-lag setting. Registered revisions
4 were independently deep-compared with revisions 3 before promotion. Service
counts, all IAM policies, Alto, inactive workers, log router, networking, funding
and the disabled sponsorship/submission flags were unchanged. Superseded task
definitions were deregistered; no application data was deleted. The ECS
`services-stable` waiter completed successfully for both services. Reconciler task
`1cfa4de32d6d4d549369a973fa5d925f` logged the explicit 1800-second setting and a
caught-up safe-boundary heartbeat. `/api/health`, authenticated EntryPoint discovery
and Alto gas-price discovery returned 200 through the private tunnel; missing
origin authentication returned 403. This is startup verification, not proof of
long-duration reliability or a successful sponsored transaction.

Reconciler readiness, alarm subscription confirmation, durable breaker resume,
owner unpause and the first sponsored transaction remain unfinished.

## Alarm email routing — 2026-09-11

User selected `stanislaw@plether.com`. Created the operator-managed SNS topic
`arn:aws:sns:eu-central-1:932542905614:plether-sepolia-aa-temp-operations` and
requested its email subscription. Subscription ID
`5b0befd6-a1a5-4775-bc12-6b4c9ee0dc03` remains pending user confirmation.
The topic/subscription were created with the named AWS CLI profile, outside
Terraform ownership; the qualification overlay records the destination ARN.

Plan `95ae0b1827b02eeefb1ad0e471ce328307d2ba9edea4d5518d23c3bbeabb3859`
was reviewed and applied: 49 existing Frankfurt alarm destinations and two guard
metadata inputs only. Readback verified all 49 alarm actions point to this topic.
No services, signing permissions, funding or sponsorship settings changed.
Actual email delivery is not verified until the user confirms the subscription.

## User-requested unpause and one-wallet activation

AWS subsequently verified `PendingConfirmation=false` for the requested email.
The user explicitly requested unpausing to test. Owner transaction
`0x3dd4b5da0b9080bb04b060bd47e41659e8ce20d9f860fd120ba87e8520d1a50a`
succeeded at block `307701845`, with zero value and a fee of
`0.000007204105448` test ETH. Readback confirms `paused=false`. Total recorded owner
outflow is now `0.09004780267256` test ETH, below the authorized 0.1 limit.
The temporary owner page was removed after submission and receipt verification.

Added an explicit `aa-canary` Terraform stage, pinned to the same owner, contracts,
region, alarm topic and global-rollout prohibition. All 26 mocked guard tests pass.
Plan `2c649c3e9875119bdb3da213ecb64816fa9f4ccd4acc77d2273e9d012839653f`
was reviewed and applied: new API definition with sponsorship/submission enabled,
two API task-role policies limited to the exact Frankfurt signing key, and guard
metadata. No other resources or limits changed. API service promotion is deferred
until the safe block includes the unpause; latest-block state alone is insufficient.

Audited issuance-resume task `e94dc68eb32a46018d9d7f6dc0480726` was dispatched
with the exact stale-timestamp pause reason and an operator note recording the
user authorization. Its containers never started because the initial launch
disabled public-IP egress on the deployment's public subnets. It was stopped
and replaced with task `21e6159bc82e4dd987122fce4a05e30c`, using the working
reconciler's subnet/egress setup and the dedicated admin security group with no
inbound rules. The exact command and roles were unchanged. The replacement and
both sidecars exited 0; the original task is STOPPED without container execution.
CloudWatch did not return the short-lived task's application log at the initial
readback; command exit status is verified, not log delivery. Localhost activation
and safe-block recognition of the unpause remained pending at that checkpoint.

Safe block `307703154` subsequently returned `paused=false` (age 498 seconds).
API revision 5 was deep-compared against revision 4 (only the two native flags
changed) and promoted. Task `a72af514d20a4d309358a59ccd6b6800` logged verified Core
bindings, configured AA mode and `api_started`, without an AA startup-attestation
failure in the inspected startup log. Local Vite was restarted with
`scripts/start-frankfurt-aa.mjs`; its origin token remains server-side and in
memory. Local manifest readback enables native sponsorship with the exact
Frankfurt paymaster; authenticated EntryPoint discovery succeeds through Vite.

For a usable deposit test, the already funded setup wallet minted 10 valueless
v1.2.3 mock USDC to the approved owner in transaction
`0x76fc6277a8ceccf9d4982ceb09680fee0affc436ae180ba69d20e28ac41e02f4`.
The exact token runtime hash and decimals were checked and mint was simulated.
Receipt succeeded, owner balance is 10 USDC, fee `0.000014765375228` test ETH from
the existing setup allocation; no additional owner funding. Firefox now shows
10 USDC available and an enabled **Transfer & Deposit** button for 1 USDC. It was
left unsubmitted for the user: owner-to-Trading-Account funding is a regular
gas-paid token transfer; the subsequent margin deposit uses AA sponsorship.
No sponsored UserOperation receipt has yet been verified. Oracle freshness and
pool liquidity remain separate prerequisites for trading, not margin deposits.

## Browser-origin failure and correction

The user's owner-to-Trading-Account transfer succeeded: Firefox shows owner
balance 9 USDC and Trading Account balance 1 USDC. The following margin deposit
failed before sponsorship. A deposit-only retry reproduced HTTP 400 with HTML
body `Unsupported origin: http://localhost:5173`; no second transfer occurred.
The preparation overlay incorrectly used comma-separated `CORS_ORIGINS`, while
`Plether.Config` parses space-separated values. Previous originless discovery
checks could not detect this mistake.

Corrected only the separator in the Frankfurt overlay and corresponding guard.
Added an origin-format contract regression and rejection of the comma-separated
configuration; all 27 mocked Terraform guard tests and 7 local-profile tests pass.
`scripts/smoke-frankfurt-aa-origin.mjs` now exercises discovery and gas-price POSTs
with both permitted browser Origin headers, plus rejection of an unrelated origin.
Plan `b79c1095ac18d634d46fff9f16821c6a2653218778bf510aaefdd7e8e9aa3295`
was reviewed and applied; only the API task definition's CORS value changed.
Registered revision 6 was verified against revision 5 before promotion. Live
browser-shaped smoke checks now pass: both permitted localhost origins return
JSON results for EntryPoint discovery and Alto gas prices; an unrelated origin
returns 403. ECS API revision 6 reached stable status.
The temporary local owner helper and its dev-server route were removed after
setup. They exposed no signing keys. The user's wallet keys never left MetaMask.

## Alto zero-postOp estimation correction

The next deposit-only retry reached the native paymaster stub, but Alto returned
AA33 during estimation. The exact deployed ABI decodes its revert as
`InvalidPaymasterPostOpGasLimit(2000000)`. No operation was submitted and the
owner-to-Trading-Account transfer was not repeated.

The pinned Alto v1.2.7 image overrides postOp gas to 2,000,000 during simulation
and normalizes a zero execution estimate to 1 before applying its multiplier.
Its shipped schema accepts zero for both relevant options. Added Frankfurt-only
`ALTO_SIMULATION_PAYMASTER_POST_OP_GAS_LIMIT=0` and
`ALTO_V7_PAYMASTER_POST_OP_GAS_LIMIT_MULTIPLIER=0`; neither changes the paymaster
contract or the legacy Singapore bundler. All 27 mocked Terraform tests and
8 local profile/configuration tests pass, including legacy exclusion.

Reviewed plan SHA-256
`1a998be5bb8104673ea7321241bdf3658fcd0a32dd3cca2603c0b69f7d15b79a`
registered only Alto task definition revision 4. Deep comparison against
revision 3 confirmed exactly those two environment additions, with the same
image, secrets, roles, networking, and gas/funding caps. Promoted the isolated
Alto service to revision 4; the ECS stable waiter completed successfully.

## Successful browser-sponsored margin deposit

Retried only the existing 1 mock-USDC Trading Account balance in Firefox. The
native stub, Alto estimate, and final sponsorship succeeded. MetaMask displayed
the expected localhost / Arbitrum Sepolia PackedUserOperation for nonce 99,
the approved Trading Account and EntryPoint, and the dedicated paymaster with
100,000 verification gas and zero postOp gas. Approved its signature under the
user's explicit testnet authorization; no second owner token transfer occurred.

- UserOperation: `0xc3a822adb688a0cec4c79090bf86b10f2b86f06d42b91709dd4f8e9ea7c16954`.
- Transaction: `0x5dd9662aa33c6b5579276f91ff6ded6ea7b37881b36c3b058b8d0add76d408cf`.
- Block: `307712759`, transaction status `0x1`, UserOperation `success: true`.
- Gas used: `206729`; sponsored actual cost: `59904141000692` wei
  (`0.000059904141000692` test ETH), paid by the dedicated paymaster.
- Firefox shows settlement balance, available-to-trade, and withdrawable balance
  of **1 USDC**, and the action marked included onchain.
- API revision 6, Alto revision 4, and reconciler revision 4 each report one
  running task, no pending tasks, and completed rollouts.

Receipt success is verified; safe-boundary reservation settlement is asynchronous
and was not yet independently verified. Oracle freshness and pool liquidity are
still separate prerequisites for opening trades.
