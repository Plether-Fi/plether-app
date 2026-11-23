# Self-hosted Kernel account abstraction on Arbitrum Sepolia

Use these bindings for a fresh Kernel v4 deployment. The API accepts owner-signed operations using the index-zero account, EntryPoint 0.9 and a single Plether paymaster. The same account can pay its own gas in ETH. Configure the deployment independently of the SimpleAccount stack.

## Qualification and deployment evidence

Run the pinned [local qualification harness](../../scripts/kernel-qualification/run.mjs) with the exact upstream Alto image and direct-factory Core paymaster artifact. It installs the pinned EntryPoint, Kernel factory, implementation and ECDSA validator on an isolated Anvil chain and exercises sponsored and ETH-paid operations. Independently attest the deployed runtime hashes, simulator, paymaster deployment block and hash on two RPC providers at a common safe block. [Source and artifact pins](../../scripts/kernel-qualification/pins.json)

The private Alto v0.9 service uses a managed Arbitrum Sepolia HTTPS RPC from SSM, with separate executor and utility keys from the SimpleAccount deployment. Set `ALTO_SAFE_MODE=false` and `ALTO_DANGEROUS_SKIP_USER_OPERATION_VALIDATION=false`. Alto performs normal account and paymaster simulation. The private gateway applies the exact action, owner, fee and runtime policy, and the API and reconciler retain independent RPC checks. Trace-based ERC-7562 opcode, storage, role and reputation checks are outside this Sepolia configuration. [Sepolia AA decision](../adr/0002-self-hosted-account-abstraction.md#kernel-v09-validation-on-sepolia)

Mirror the unmodified upstream ARM64 Alto image and build the API with the protected [Alto](../../.github/workflows/build-kernel-alto.yml) and [API](../../.github/workflows/build-kernel-api.yml) workflows for the exact reviewed PR head. Set `KERNEL_CANARY_BUILD_ROLE_ARN` in those environments to `arn:aws:iam::932542905614:role/plether-sepolia-kernel-canary-image-build`. Record the exact ECR digests and require a complete scan with zero critical findings for the Alto digest. The normal Alto workflow continues to administer EntryPoint v0.8.

1. Start the services privately with both payment flags and the public Kernel route off. Keep the new paymaster paused. Probe the API and Alto from the VPC.
2. On the exact reviewed Alto image and task, obtain live funded and unfunded estimates, reject an invalid owner signature, and include a funded ETH-paid first deployment directly through KernelFactory. Record provider, chain, EntryPoint, simulator, configuration, operation hash, canonical receipt and payer debit. Confirm both independent security readers agree. Set `kernel_canary_managed_alto_qualified=true` and `kernel_canary_managed_alto_qualified_image` to the exact qualified ECR image URI before opening the owner-allowlisted route.
3. Verify the external and Privy EOA flows using reviewed wallet prompts in both gas modes on the same candidate. Verify a subsequent payer change, correct actual gas and payer debits, independent canonical recovery, and ETH readiness during a sponsor pause. Enable sponsored admission only for the bounded test after the direct-factory paymaster and new reconciler are attested. Wider Sepolia rollout requires these results and separate approval.

### Prior Nitro SafeMode evidence

The previous Staker-aware candidate passed 11 signed operations on local Anvil and two on disposable Nitro. It also included one private Sepolia ETH-paid operation, with 422,896 estimated total gas, 52,102 preverification gas and 320,767 actual gas. Preserve this evidence for its original profile. Qualify the managed RPC, direct factory, new paymaster and real wallets for `kernel-sepolia-direct-v1`. [Disposable Nitro evidence](./evidence/kernel-nitro-safemode-2026-11-20.json) · [Previous Sepolia receipt](https://sepolia.arbiscan.io/tx/0xc6c01aa32bc58de8ec96dd1dafa0c154a006376f9b9a6b6695e1733b17d49fb9)

The [shared deployment test fixture](../../apps/backend/test/fixtures/kernel-deployment.json) records the reviewed canary profile, gas limits, and safe-head lag bounds. Update its expectations with any reviewed policy change and run the backend, frontend, mocked Terraform, and qualification checks together. Each runtime validates its own inputs.

The [isolated Pages canary procedure](../../scripts/kernel-qualification/pages-canary.md) uses `https://plether-kernel-canary.pages.dev`. Check the intended Privy app's allowed-origin policy first: test an empty development-app list as-is, or add the exact preview origin without removing existing entries if a list is already enforced. Deploy `stage_preview` with the Kernel API route closed to connect the external and Privy wallets and copy each public owner EOA address from Trading Account activity. Set the canary owner allowlist and finish contract and API attestations before `active_canary`. After opening the route, wait for a healthy API target group and verify 403 unauthenticated and 400 authenticated invalid-JSON controls; a new attachment may briefly return 503. Then dispatch `active_canary` and invite wallet approvals. Real wallet approvals and live gas evidence are still required after the active preview is live.

| Field | Value |
| --- | --- |
| `deployment` | `kernel-sepolia-direct-v1` for this deployment and its database scope |
| `chainId`, `entryPointVersion` | `421614`, `"0.9"` |
| `entryPoint`, `entryPointCodeHash` | Canonical v0.9 EntryPoint address and attested runtime hash |
| `factory`, `factoryCodeHash` | Verified KernelFactory address and runtime hash. First UserOperations call its canonical `deploy` initializer directly |
| `implementation`, `implementationCodeHash` | Verified KernelUUPS address and runtime hash |
| `validator`, `validatorCodeHash` | Verified ECDSA validator address and runtime hash |
| `accountCodeHash` | Runtime hash of a real index-zero account from the reviewed factory |
| `accountVersion`, `accountIndex` | `"kernel-v4-uups"`, `0` |

The API needs `DATABASE_URL`, `PERPS_RPC_URL`, `AA_SECURITY_RPC_URL` from an independent provider on a different host, `AA_KERNEL_ALTO_RPC_URL` for private Alto and the trusted Worker-to-origin `AA_PROXY_ORIGIN_TOKEN`. The reconciler needs the database and two RPC providers. Optional `PERPS_RPC_AUTH_TOKEN` authenticates the API's primary RPC. The canary Alto task gets `ALTO_RPC_URL` as the managed HTTPS endpoint from SSM. The API holds the KMS signing permission. API startup requires a deployed Arbitrum Sepolia release. The gateway checks its compiled release pins and reads the target bytecode from both providers before admitting an operation. [Terraform configuration](../../infra/terraform/README.md)

Both Kernel services use `AA_RECONCILER_SECONDARY_RPC_URL` as a fallback only when `AA_SECURITY_RPC_URL` is unset. An empty or invalid value for the preferred setting fails startup. Confirm that each hostname belongs to a separately operated provider.

An isolated Kernel API sharing the live Sepolia database sets `KERNEL_CANARY_API_MODE=true` and `API_BACKGROUND_WORKERS_ENABLED=false`. Startup rejects a canary with generic background workers enabled. The dedicated Kernel reconciler runs as its own service. The canary API and reconciler attach the live AA reconciler database client security group by exact ID. Preserve that group identity while the canary is running; a live core replacement could sever canary database access. Recheck the ID on every canary Terraform plan and retire the canary before replacing it.

## Sponsorship configuration

Deploy a newly reviewed `PletherKernelVerifyingPaymaster` from `plether-core` in the paused state. The direct factory constructor and initCode policy require a new contract address and runtime hash. Fund its EntryPoint gas deposit and attest its code, owner, signer, policy and canonical deployment transaction. The direct factory and paymaster have no required EntryPoint stake for this Sepolia profile. Record the new attested paymaster address and runtime hash in the frontend pins before building the sponsored wallet preview.

Before replacing the old task definitions on the same stack, stop admitting new operations on the Staker-aware deployment and drain its signed ETH-paid and sponsored operations with the old API and reconciler. Check canonical outcomes and confirm that no reservations or accepted account lanes are outstanding. Both deployments share the same EntryPoint nonce lane for a given account even though the database and browser recovery keys are deployment-scoped. Keep the old profile, paymaster binding and cursor for recovery until the drain completes. An unsigned review can be prepared again under the new profile.

Supply `AA_KERNEL_SPONSORSHIP_JSON` to the API and the reconciler with `paymaster`, `paymasterCodeHash`, `policyId`, `signerAddress`, `deploymentBlock`, `deploymentBlockHash`, `minDepositWei`, `verificationGasLimit`, `validitySeconds`, `canaryOwners` and `budget`. The budget object has integer Wei fields `maxCostWei`, `accountOutstandingWei`, `clientOutstandingWei`, `globalOutstandingWei`, `accountHourlyWei`, `globalHourlyWei` and `globalDailyWei`. Set a bounded canary allowlist and confirm the signer address matches the dedicated `AA_KERNEL_PAYMASTER_KMS_KEY_ID` on the API.

Start with both admission flags `AA_KERNEL_ETH_ENABLED` and `AA_KERNEL_SPONSORSHIP_ENABLED` false. Set `AA_KERNEL_CANARY_OWNERS_JSON` to the external and Privy test EOA addresses before enabling either mode. Both modes reject other owners at preparation and fresh submission. The eventual public ETH rollout requires the separate `AA_KERNEL_PUBLIC_ADMISSION=true` switch and removal of the canary list. Test ETH admission first. Enable issuance only after confirming the funded, attested paymaster, fresh reconciliation state, and the sponsor and ETH acceptance cases. Disabling new sponsorship issuance preserves access to already signed authorizations and status. Keep the same deployment scope, paymaster binding and chain evidence until signed authorizations settle or safely expire. Stop issuance and investigate a reconciliation pause before taking administrative action.

## Gateway and verification

The browser posts JSON-RPC to `/api/perps/v1/aa/kernel/rpc`. The Worker forwards it to `/api/aa/kernel/rpc` with its origin token. Responses use `Cache-Control: no-store`.

| Method | Result |
| --- | --- |
| `plether_getKernelManifest` | Version, approved account pins, Perps release, trading addresses, paymaster and independent readiness for each payment mode |
| `plether_prepareUserOperation` | A versioned, immutable operation including the selected payer, bounds, expiry and recovery token. Funding and account state are read at a recent block agreed by both providers |
| `plether_sendUserOperation` | The exact accepted and relayed UserOperation hash |
| `plether_getUserOperationStatus` | Pending status, observed inclusion from both RPCs, or terminal status with safe-chain evidence |
| `plether_prepareUserOperationCancellation` | A fee quote for an owner-approved ETH-paid replacement of a pending ETH operation on the same nonce |
| `plether_sendUserOperationCancellation` | The exact accepted and relayed cancellation hash |

Prepare with `version: 1`, a random 32-byte `preparationId`, `mode` of `"eth"` or `"sponsored"`, `sender`, `callData` and the paired factory fields for an undeployed account. Submit the same preparation ID, version and mode with the full `operation` including the owner signature. Look up status by version, preparation ID and UserOperation hash. Treat the optional recovery token as a credential. The account adapter uses a zero nonce key and the same account for both modes. [Viem adapter](../../apps/frontend/src/perps-aa/kernelAccount.ts) · [Gateway](../../apps/backend/src/Plether/AA/KernelGateway.hs)

Verify initial deployment with each payment mode, then switch the payer for a subsequent operation on the same account. Confirm onchain payer and actual gas, exact hash parity, and recovery from an uncertain relay response. The ETH path must remain available during a sponsor-only pause. Check an ETH-paid operation creates no sponsorship authorization and uses no KMS call. Capture readiness and canonical evidence before enabling either mode on a fresh deployment.

## Frontend review and operation recovery

Build the fresh site with `VITE_PERPS_KERNEL_AA_DEPLOYMENT` set to the exact `deployment` value from `AA_KERNEL_PROFILE_JSON`. The frontend requires the selected gateway profile and the compiled Perps release to match. The wallet header opens Trading Account activity even with no history. Its **Gas sponsorship** switch saves the mode per deployment, chain and account. Reviews show the selected payer and the maximum ETH charge for a user-paid operation. A preference change invalidates an unsigned review. The browser signs and submits the reviewed bytes under the shared account lane and keeps the recovery token local.

After connecting a wallet, users can set their preference during account setup or a gateway outage. The pinned Kernel profile derives one account per owner and deployment, so the choice carries over when the account is verified. Both submission modes still require live readiness.

Use **Check status** to recover an uncertain send. An exact resend uses the saved signature and payer. An ETH-paid attempt also offers **Review cancellation**. The owner reviews the maximum ETH cost before signing a fixed, same-nonce replacement. Counterfactual cancellation includes deployment gas and may create the account. Activity shows the transaction as soon as both readers agree on its inclusion. The journal retains the pending attempt until safe-chain recovery finishes. An expired attempt that the browser can prove it never relayed offers **Discard unsent operation**. An operation receipt reports the actual gas charge, including for a revert. Order activity reports keeper execution separately from operation inclusion.

For ETH-paid operations sent through Alto, a verified successful inclusion on the opt-in Sepolia homepage allows the owner to review the next ETH-paid action while the earlier receipt settles. The browser requires a newer live nonce. The allowlisted gateway verifies that nonce, funding and current account state against both providers before accepting it. The database holds one pending operation per nonce and keeps the account-wide exclusion for sponsored admissions. Deploy the canary API before the homepage. API startup replaces the former account-wide pending index in one transaction. Drain pending ETH operations before rolling back to a version that restores the former index.

The two-wallet homepage opt-in uses **Pay from wallet** when Gas sponsorship is off. The owner signs the zero-fee Kernel UserOperation and then sends a zero-value transaction to the pinned EntryPoint from the same wallet. The Arbitrum Sepolia wallet pays the outer gas fee. The gateway's ETH readiness still gates new wallet-paid actions. Wallet-paid attempts share the account nonce and browser journal with earlier Alto attempts. Activity verifies the outer transaction and the inner UserOperation event, and shows the wallet's actual gas charge. Use the owner wallet to speed up or cancel a pending outer transaction.

ETH-paid preparation records a historical block agreed by both providers as a starting point for recovering aged-out receipts. This can be an older safe or finalized block. The quote uses current account state and can proceed before a recent funding transfer is safe.

When the original relay failed to reach the gateway, cancellation review can reserve the same nonce using the exact signed original and its recovery token. The gateway can relay only the reviewed cancellation from that reservation. If an independent lookup lacks canonical evidence, the operation remains pending.

## Onboarding and gas funding

Before showing the ETH funding address, the app derives Kernel index zero from the connected owner and checks the factory, implementation, validator and EntryPoint at a recent block. The gateway agrees on an explicit recent height and hash with two independent RPC providers before preparing or relaying a transaction. For a deployed account it also checks the proxy implementation, root validator and owner. The gateway, testnet margin funding and Insights registration use the same owner derivation. A first UserOperation uses the canonical `KernelFactory.deploy` initializer and is checked again at its canonical inclusion block. The same owner and pinned index-zero derivation retain the same funding address across the factory transport change. Reprepare any unsigned reviews under the new deployment scope.

Use a supported embedded or external EOA with empty code for the initial ECDSA profile. Contract wallets and delegated owners require different onboarding support.

Trading Account activity shows the account's native ETH and any EntryPoint deposit separately from Perps margin. **Add ETH** sends a wallet-paid transfer to the verified Arbitrum Sepolia address or lets the user copy it. Once the transfer is included and both providers report the funds on the same recent chain, return to review for a fresh gas quote. An estimate made to calculate an unfunded account's shortfall is advisory. Preparation and relay use actual chain balances.

For ETH recovery, select the source balance and review the transfer to the verified owner and the maximum network fee. An ETH-paid withdrawal must leave enough prefund for itself. This can leave a small balance. Follow the gateway's current sponsorship policy for owner withdrawals. The local qualification used direct Alto RPC and a local paymaster signature to sweep a remaining EntryPoint deposit after sponsorship resumed. That sweep exercised the contracts and bundler.

The welcome allocation credits testnet USDG margin directly, once per owner and program. It does not supply ETH for gas. Insights registration keeps its existing owner approval and records the verified Kernel trading address.
