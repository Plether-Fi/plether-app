# Sepolia AA canary preparation — 2026-09-10

Status: partial preparation only. No Terraform apply, service deployment,
contract transaction, funding, or native sponsorship activation was performed.

The user subsequently requested a temporary full-stack deployment in a different
AWS region, leaving Singapore untouched. This Singapore preparation overlay is
not the target for that deployment. See [temporary-stack isolation requirements](temporary-aa-test-stack.md)
before planning any infrastructure changes.

## Dedicated Alto keys

Verified AWS identity: `arn:aws:iam::932542905614:user/plether-admin`, profile
`plether`, region `ap-southeast-1`.

Created fresh secp256k1 keys directly into the following SSM `SecureString`
parameters, encrypted with `alias/aws/ssm`. Both are version 1 and were verified
by decrypted readback in process memory. Private keys were not printed, placed
in operating-system command arguments, or written to local files. No existing
parameter was overwritten. These addresses must never be reused for another role.

| Role | Public address |
| --- | --- |
| Executor 1 | `0x6E57819F6C3A4357B22D2d757E7B58AB54A26128` |
| Executor 2 | `0xD0356B7684e76600d918c823216596cC83946930` |
| Executor 3 | `0xaC052beD185c7bacba2B42E8AEe1d295b86EaeAD` |
| Executor 4 | `0xD5819E59f53A0A19Cf90518dAd76dD8374B75C87` |
| Utility/refill | `0x4C4fe88Fe4814a83Ab1AE20d0AE9Cfb06e8fb3b9` |

Parameters:

- `/plether/sepolia/alto-executor-private-keys` — four comma-separated keys
- `/plether/sepolia/alto-utility-private-key` — one key

No Alto or AA reconciler service existed in the `plether-sepolia` ECS service
inventory during this preparation. Neither service was created or started.
The keys are not the paymaster's KMS signing key or its governance owner.

## Prepared limits (not applied)

`infra/terraform/sepolia-aa-canary-preparation.tfvars` is a non-secret overlay,
not a complete environment file and not an activation profile. It explicitly
keeps provisioning/configuration/issuance/submission/global rollout disabled,
service counts zero, and only the user-approved test owner in the allowlist.
The populated allowlist does not activate sponsorship or submission.

| Control | ETH |
| --- | --- |
| Maximum sponsored cost per operation | 0.01 |
| Account/client/global outstanding liability | 0.02 each |
| Account/global hourly allowance | 0.05 each |
| Global daily allowance | 0.1 |
| Pause threshold for EntryPoint deposit | 0.05 |
| Sponsored-gas hourly alert | 0.02 |
| Existing executor refill floor, each | 0.005 |

The user approved a 10x increase to the prepared sponsorship budgets and final
request limit. The deposit pause and spend-alert thresholds scale by 10x as
well; the executor refill floor is unchanged. Final sponsorship requests are
capped at 20 per client per minute. These ceilings remain unapplied and require
live gas-estimation qualification; a rejected
operation is not permission to raise the caps. Deposit/stake, bootstrap gas,
utility refills, and executor balances are separate from sponsorship accounting.
No total funding budget or transfer has been authorized by this configuration.

## Approved owner and test wallet

The user supplied `0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B` as the
test-wallet owner and explicitly requested the same paymaster owner as Core.

Read-only checks against the configured primary RPC on chain 421614 at safe
block `0x12533043`, hash
`0x3acaea13ceec05554b5b2c1c4ade66439bbe5d36f3e4cbc9dae4275becd2b9ba`,
confirmed that both Core v1.2.3 `CfdEngine`
(`0xafece93321be41aa73474457e2f47cf7b2fb738f`) and `MarginClearinghouse`
(`0xfa6e677ec1062757c1194d411a5e61e1e9644499`) return this address from
`owner()`. It also matches the release manifest's owner. These checks used
one provider only, not the required independent-provider activation proof.

The selected owner has no contract code at that block: it is an EOA, not a
Safe. The user's explicit choice is recorded as a Sepolia-only exception to
the general rollout runbook's Safe-owner requirement. There is no multisig
approval threshold, and the same key controls Core ownership, paymaster
administration, and the test account. Do not extend this exception to mainnet.
For this canary, owner-only actions must be authorized by this EOA instead of
a Safe; do not label its approvals or evidence as multisig approvals.

The future deployment input is:

```sh
export PAYMASTER_OWNER=0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B
```

This records the requested deployment owner, not an on-chain ownership change.
No paymaster was deployed. The existing deployment script consumes
`PAYMASTER_OWNER`; it is not a Terraform variable. Reverify Core ownership
before deployment and stop for reconfirmation if it has changed.

The Terraform preparation allowlist contains this owner EOA, not its derived
smart-account address. Before activation, verify the derived index-zero
account, ownership, proxy runtime and implementation slots at the common safe
block and latest head per the rollout runbook. That qualification is still
pending, as are wallet signing tests; supplying an address alone is not an
on-chain proof of account control.

## Inputs still required

- Explicit overall funding ceiling and allocations before any transfer, plus
  a staffed alarm destination and qualification window before activation.

## Approved Alto RPC

The user requested the same RPC as the backend. The preparation overlay now
references `/plether/sepolia/perps-rpc-url` directly for Alto, rather than
creating a duplicate `/plether/sepolia/alto-rpc-url` credential. This is a
user-approved departure from the runbook's dedicated Alto endpoint: Alto and
the backend will share provider availability, quota, and credential rotation.
No SSM values or IAM policies were changed by this configuration update.

Verified the existing parameter is `SecureString`, version 1, encrypted with
`alias/aws/ssm`. Read-only preflight confirmed HTTPS/443, chain 421614, a safe
block read at `0x12533043`, and the Core deployment block `0x1252824c`.
No URL or credentials were logged. Write permissions, simulation support and
available provider quota still require qualification; no transactions were
submitted by this check.

The user subsequently approved Alchemy-only verification for the initial
Sepolia canary, with a self-hosted node planned later. The overlay now sets
`aa_rpc_mode="single-provider-sepolia"` and references the existing primary
SSM parameter in both RPC environment slots. This does not create a second
provider or duplicate credential. The independent secondary parameter is no
longer a prerequisite for this explicitly scoped mode.

Repeated safe-state, profile and log checks still run, but they cannot detect
correlated false provider evidence. Alchemy failure stops progress rather than
triggering a fallback. Both services and deployment checks visibly identify
the exception. Global/mainnet use remains blocked and sponsorship/submission
remain disabled in this unapplied profile. See the main rollout runbook's RPC
verification section for returning to independent verification with our node.

Supply credentials through a non-logging secret-injection path, not chat,
committed tfvars, or workflow inputs. Follow `self-hosted-aa-rollout.md` for
the remaining prerequisites and gates; this record does not waive them.
