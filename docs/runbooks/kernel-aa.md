# Self-hosted Kernel account abstraction on Arbitrum Sepolia

Use these bindings for a fresh Kernel v4 deployment. The API accepts owner-signed operations using the index-zero account, EntryPoint 0.9 and a single Plether paymaster. The same account can pay its own gas in ETH. Configure the deployment independently of the SimpleAccount stack.

## Qualification and deployment evidence

Run the pinned [local qualification harness](../../scripts/kernel-qualification/run.mjs) before choosing a release configuration. It uses upstream Kernel release bytecode, a pinned ECDSA validator, a ZeroDev Staker and a local Anvil chain. See the [source and artifact pins](../../scripts/kernel-qualification/pins.json). Confirm the actual Arbitrum Sepolia deployment of the EntryPoint, simulator, Kernel factory, account implementation, validator, Staker and paymaster on independent RPC providers at a common safe block. Record their runtime hashes, the Staker approval and stake, the paymaster stake, and the paymaster deployment block and hash. Local address derivation and bytecode parity alone are insufficient evidence of a public deployment.

Provision the private Alto v0.9 profile and select the same simulator in its deployment smoke checks. Keep safe mode enabled. Use separate executor and utility keys from the SimpleAccount deployment. Set the following public pins in `AA_KERNEL_PROFILE_JSON` on the API and reconciler.

Build the canary Alto and API images with the protected [Alto](../../.github/workflows/build-kernel-alto.yml) and [API](../../.github/workflows/build-kernel-api.yml) workflows from the exact reviewed PR head. Set `KERNEL_CANARY_BUILD_ROLE_ARN` in their protected environments to `arn:aws:iam::932542905614:role/plether-sepolia-kernel-canary-image-build`; both workflows verify that dedicated ECR-only role before assuming it.

Keep both admission flags off until the patched Alto image, simulator and a debug-enabled Nitro RPC pass funded deployment and execution in both modes. Unmodified Alto v1.2.7 cannot decode EntryPoint 0.9’s `DelegateAndRevert` result in SafeMode. [The pinned source patch](../../scripts/alto-kernel-patch/README.md), a staked ZeroDev Staker and a staked Plether paymaster passed the full local SafeMode suite on Anvil on 2026-11-20. That suite does not exercise Arbitrum's L1 data fee or real wallets. [Qualification results](../../scripts/kernel-qualification/README.md)

### Point 5 evidence, 2026-11-20

| Gate | Observed result |
| --- | --- |
| Local Alto SafeMode | Patched local image ID `sha256:8397e4091316abf5857f63560576804144ffc09212235df2e8e1e23a983b0a6c` completed 11 signed Kernel operations on Anvil with SafeMode enabled and normal UserOperation validation. Both first-deployment gas modes, payer changes, withdrawals and cancellations passed. Direct first deployment through an unstaked KernelFactory was correctly rejected, so the Staker wrapper and its one-day stake are required. The Plether paymaster needs its own stake for sponsorship. Seven targeted decoder and RPC authorization tests passed in the image. A separate malformed-token startup check exited before any RPC call. [ERC-7562 storage rules](https://eips.ethereum.org/EIPS/eip-7562) |
| Disposable Nitro SafeMode, 2026-11-20 | Official Nitro `v3.12.2+5f02f3f` on local chain `412346` and patched Alto image ID `sha256:8397e4091316abf5857f63560576804144ffc09212235df2e8e1e23a983b0a6c` passed a signed ETH-paid first Kernel deployment (op `0x58a74a05141dd114537fd6641028e17e94480cdfd155f57646a8b2fbf28197ad`, tx `0x8182363a4240391c94845cf5d8aab800ce378f29fe8721357f789ceb8dc65520`) and a sponsored subsequent call on the same account (op `0x5fef6c255f06943d9778164c83d915dde3c96ace305fb5192d4b3e983f39caae`, tx `0x1387f07dcfb6174db5f23f18482a38e134b8cf3ede2a64c75ce62649ec36965b`). Both `UserOperationEvent` receipts succeeded with the expected payer debit. An RPC recorder captured five successful custom `debug_traceCall` requests. A zero owner signature failed `AA24` with the nonce unchanged. The local EntryPoint, Kernel implementation, factory and simulators used disposable addresses; a temporary Alto runtime override identified that local EntryPoint as v0.9, and the local paymaster changed only its chain-ID constant. This proves the Nitro SafeMode execution path on a disposable chain, not the public Sepolia deployment or wallet flows. [Sanitized evidence](./evidence/kernel-nitro-safemode-2026-11-20.json) |
| Private Sepolia Nitro tracer | Plether's private Nitro v3.12.2 node on chain `421614` executed the exact custom JavaScript `debug_traceCall` capability probe against the pinned EntryPoint 0.9 `getNonce(0,0)` call. The public Arbitrum and PublicNode RPC endpoints did not offer this method. A disposable ECS task made a direct authenticated fetch to Plether’s private RPC gateway and passed that probe, then stopped. The existing live Alto v1.2.7 image does not use its SSM-injected gateway token. The new patched image adds the bearer header for the primary RPC, but no signed operation has run through Alto on live Sepolia. |
| Public Sepolia bindings | At safe block `331161366`, hash `0x510a02af0fc993a57b1432dd518d6f13f148e737821881e1ab0e403278999496`, three independent RPCs agreed on the pinned EntryPoint, SenderCreator and CREATE2 runtime. Pinned KernelFactory, KernelUUPS, KernelImmutableECDSA and ECDSA validator addresses had no code. |
| Live gas | `eth_estimateGas` and `NodeInterface.gasEstimateComponents` agreed for three read-only EntryPoint calldata controls. Actual Kernel `eth_estimateUserOperationGas`, sponsored quotes, L1 data cost and receipt deltas remain to be measured on the fresh canary. [Arbitrum gas guide](https://docs.arbitrum.io/arbitrum-essentials/how-to-estimate-gas) |
| Wallets and admission | Local `personal_sign` simulations passed. The actual external and Privy wallet flows require a current-master preview, fresh deployed account addresses and approved wallet prompts. Both production admission flags remain off. |

On 2026-11-21, the private Arbitrum Sepolia Alto canary included a signed ETH-paid operation through Nitro SafeMode. It estimated 422,896 total gas, including 52,102 preverification gas. EntryPoint reported 320,767 actual gas and the expected ETH debit. The 5.86M preverification estimate came from the disposable Nitro chain. After review, repeat live gas and wallet checks with the two allowlisted owners against the revised candidate's exact images and task configuration before broader rollout. [Sepolia bundle receipt](https://sepolia.arbiscan.io/tx/0xc6c01aa32bc58de8ec96dd1dafa0c154a006376f9b9a6b6695e1733b17d49fb9)

The normal Alto workflow accepts only EntryPoint v0.8. Use the protected isolated canary build and private launch configuration for v0.9. A core v0.9 cutover requires private VPC execution for authenticated Nitro checks, a narrowly scoped grant to read the approved canary image from ECR, and task definitions using the patched image's native launcher.

The [shared deployment test fixture](../../apps/backend/test/fixtures/kernel-deployment.json) records the reviewed canary profile, gas limits, and safe-head lag bounds. Update its expectations with any reviewed policy change and run the backend, frontend, mocked Terraform, and qualification checks together. Each runtime validates its own inputs.

The [isolated Pages canary procedure](../../scripts/kernel-qualification/pages-canary.md) uses `https://plether-kernel-canary.pages.dev`. Check the intended Privy app's allowed-origin policy first: test an empty development-app list as-is, or add the exact preview origin without removing existing entries if a list is already enforced. Deploy `stage_preview` with the Kernel API route closed to connect the external and Privy wallets and copy each public owner EOA address from Trading Account activity. Set the canary owner allowlist and finish contract and API attestations before `active_canary`. After opening the route, wait for a healthy API target group and verify 403 unauthenticated and 400 authenticated invalid-JSON controls; a new attachment may briefly return 503. Then dispatch `active_canary` and invite wallet approvals. Real wallet approvals and live gas evidence are still required after the active preview is live.

| Field | Value |
| --- | --- |
| `deployment` | Unique lowercase ID for this deployment and its database scope |
| `chainId`, `entryPointVersion` | `421614`, `"0.9"` |
| `entryPoint`, `entryPointCodeHash` | Canonical v0.9 EntryPoint address and attested runtime hash |
| `factory`, `factoryCodeHash` | Verified KernelFactory address and runtime hash |
| `staker`, `stakerCodeHash` | Plether-owned ZeroDev Staker runtime, approved KernelFactory and at least one ETH staked at the EntryPoint with a one-day unstake delay. First UserOperations use the Staker as their `factory` |
| `implementation`, `implementationCodeHash` | Verified KernelUUPS address and runtime hash |
| `validator`, `validatorCodeHash` | Verified ECDSA validator address and runtime hash |
| `accountCodeHash` | Runtime hash of a real index-zero account from the reviewed factory |
| `accountVersion`, `accountIndex` | `"kernel-v4-uups"`, `0` |

The API needs `DATABASE_URL`, `PERPS_RPC_URL`, `AA_SECURITY_RPC_URL` from an independent provider on a different host, `AA_KERNEL_ALTO_RPC_URL` for private Alto and the trusted Worker-to-origin `AA_PROXY_ORIGIN_TOKEN`. The reconciler needs the database and two RPC providers. Optional `PERPS_RPC_AUTH_TOKEN` authenticates the API's primary RPC. The patched Alto canary reads its own `PLETHER_ALTO_RPC_AUTH_TOKEN` secret for the private Nitro gateway and sends it only to the configured primary `ALTO_RPC_URL`. The API holds the KMS signing permission. API startup requires a deployed Arbitrum Sepolia release. The gateway checks its compiled release pins and reads the target bytecode from both providers before admitting an operation. [Terraform configuration](../../infra/terraform/README.md)

Both Kernel services use `AA_RECONCILER_SECONDARY_RPC_URL` as a fallback only when `AA_SECURITY_RPC_URL` is unset. An empty or invalid value for the preferred setting fails startup. Confirm that each hostname belongs to a separately operated provider.

An isolated Kernel API sharing the live Sepolia database sets `KERNEL_CANARY_API_MODE=true` and `API_BACKGROUND_WORKERS_ENABLED=false`. Startup rejects a canary with generic background workers enabled. The dedicated Kernel reconciler runs as its own service. The canary API and reconciler attach the live AA reconciler database client security group by exact ID. Preserve that group identity while the canary is running; a live core replacement could sever canary database access. Recheck the ID on every canary Terraform plan and retire the canary before replacing it.

## Sponsorship configuration

Deploy the verified `PletherKernelVerifyingPaymaster` from `plether-core` in the paused state. The Staker-aware source is pinned to Core commit `469c40dd5757041dd57cafc35d0feb01d77a1aa2`. Publishing that exact commit in a reviewed Plether Core ref is a release prerequisite. The script attests the Staker stake before deployment and the paymaster stake before unpausing. Sponsored readiness and reconciliation require at least one ETH staked at the EntryPoint with a one-day unstake delay, separately from the paymaster deposit. Supply `AA_KERNEL_SPONSORSHIP_JSON` to the API and the reconciler with `paymaster`, `paymasterCodeHash`, `policyId`, `signerAddress`, `deploymentBlock`, `deploymentBlockHash`, `minDepositWei`, `verificationGasLimit`, `validitySeconds`, `canaryOwners` and `budget`. The budget object has integer Wei fields `maxCostWei`, `accountOutstandingWei`, `clientOutstandingWei`, `globalOutstandingWei`, `accountHourlyWei`, `globalHourlyWei` and `globalDailyWei`. Set a bounded canary allowlist and confirm the signer address matches the dedicated `AA_KERNEL_PAYMASTER_KMS_KEY_ID` on the API.

Start with both admission flags `AA_KERNEL_ETH_ENABLED` and `AA_KERNEL_SPONSORSHIP_ENABLED` false. Set `AA_KERNEL_CANARY_OWNERS_JSON` to the external and Privy test EOA addresses before enabling either mode. Both modes reject other owners at preparation and fresh submission. The eventual public ETH rollout requires the separate `AA_KERNEL_PUBLIC_ADMISSION=true` switch and removal of the canary list. Test ETH admission first. Enable issuance only after confirming the funded, attested paymaster, fresh reconciliation state, and the sponsor and ETH acceptance cases. Disabling new sponsorship issuance preserves access to already signed authorizations and status. Keep the same deployment scope, paymaster binding and chain evidence until signed authorizations settle or safely expire. Stop issuance and investigate a reconciliation pause before taking administrative action.

## Gateway and verification

The browser posts JSON-RPC to `/api/perps/v1/aa/kernel/rpc`. The Worker forwards it to `/api/aa/kernel/rpc` with its origin token. Responses use `Cache-Control: no-store`.

| Method | Result |
| --- | --- |
| `plether_getKernelManifest` | Version, approved account pins, Perps release, trading addresses, paymaster and independent readiness for each payment mode |
| `plether_prepareUserOperation` | A versioned, immutable operation including the selected payer, bounds, expiry and recovery token |
| `plether_sendUserOperation` | The exact accepted and relayed UserOperation hash |
| `plether_getUserOperationStatus` | Pending or terminal status and canonical inclusion evidence when available |
| `plether_prepareUserOperationCancellation` | A fee quote for an owner-approved ETH-paid replacement of a pending ETH operation on the same nonce |
| `plether_sendUserOperationCancellation` | The exact accepted and relayed cancellation hash |

Prepare with `version: 1`, a random 32-byte `preparationId`, `mode` of `"eth"` or `"sponsored"`, `sender`, `callData` and the paired factory fields for an undeployed account. Submit the same preparation ID, version and mode with the full `operation` including the owner signature. Look up status by version, preparation ID and UserOperation hash. Treat the optional recovery token as a credential. The account adapter uses a zero nonce key and the same account for both modes. [Viem adapter](../../apps/frontend/src/perps-aa/kernelAccount.ts) · [Gateway](../../apps/backend/src/Plether/AA/KernelGateway.hs)

Verify initial deployment with each payment mode, then switch the payer for a subsequent operation on the same account. Confirm onchain payer and actual gas, exact hash parity, and recovery from an uncertain relay response. The ETH path must remain available during a sponsor-only pause. Check an ETH-paid operation creates no sponsorship authorization and uses no KMS call. Capture readiness and canonical evidence before enabling either mode on a fresh deployment.

## Frontend review and operation recovery

Build the fresh site with `VITE_PERPS_KERNEL_AA_DEPLOYMENT` set to the exact `deployment` value from `AA_KERNEL_PROFILE_JSON`. The frontend requires the selected gateway profile and the compiled Perps release to match. The wallet header opens Trading Account activity even with no history. Its **Gas sponsorship** switch saves the mode per deployment, chain and account. Reviews show the selected payer and the maximum ETH charge for a user-paid operation. A preference change invalidates an unsigned review. The browser signs and submits the reviewed bytes under the shared account lane and keeps the recovery token local.

After connecting a wallet, users can set their preference during account setup or a gateway outage. The pinned Kernel profile derives one account per owner and deployment, so the choice carries over when the account is verified. Both submission modes still require live readiness.

Use **Check status** to recover an uncertain send. An exact resend uses the saved signature and payer. An ETH-paid attempt also offers **Review cancellation**. The owner reviews the maximum ETH cost before signing a fixed, same-nonce replacement. Counterfactual cancellation includes deployment gas and may create the account. The lane stays locked until the gateway has canonical evidence. An expired attempt that the browser can prove it never relayed offers **Discard unsent operation**. An operation receipt reports the actual gas charge, including for a revert. Order activity reports keeper execution separately from operation inclusion.

When the original relay failed to reach the gateway, cancellation review can reserve the same nonce using the exact signed original and its recovery token. The gateway can relay only the reviewed cancellation from that reservation. If an independent lookup lacks canonical evidence, the operation remains pending.

## Onboarding and gas funding

Before showing the ETH funding address, the app derives Kernel index zero from the connected owner and checks the factory, implementation, validator, EntryPoint and approved staked Staker at a safe block. For a deployed account it also checks the proxy implementation, root validator and owner. The gateway, testnet margin funding and Insights registration use the same owner derivation. A first UserOperation uses `Staker.deployWithFactory` with the exact KernelFactory initializer and is checked again at its canonical inclusion block.

Use a supported embedded or external EOA with empty code for the initial ECDSA profile. Contract wallets and delegated owners require different onboarding support.

Trading Account activity shows the account's native ETH and any EntryPoint deposit separately from Perps margin. **Add ETH** sends a wallet-paid transfer to the verified Arbitrum Sepolia address or lets the user copy it. Once the transfer is confirmed and the funds are visible at a safe block, return to review for a fresh gas quote. An estimate made to calculate an unfunded account's shortfall is advisory. Preparation and relay use actual chain balances.

For ETH recovery, select the source balance and review the transfer to the verified owner and the maximum network fee. An ETH-paid withdrawal must leave enough prefund for itself. This can leave a small balance. Follow the gateway's current sponsorship policy for owner withdrawals. The local qualification used direct Alto RPC and a local paymaster signature to sweep a remaining EntryPoint deposit after sponsorship resumed. That sweep exercised the contracts and bundler.

The welcome allocation credits testnet USDG margin directly, once per owner and program. It does not supply ETH for gas. Insights registration keeps its existing owner approval and records the verified Kernel trading address.
