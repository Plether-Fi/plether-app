# ADR 0002: Self-hosted account abstraction on Arbitrum Sepolia

- Status: Accepted
- Date: 2026-09-02
- Scope: Plether Perps managed gas sponsorship on Arbitrum Sepolia

## Context

The Perps frontend currently uses Pimlico for ERC-4337 v0.8 bundling and
verifying-paymaster sponsorship. The backend already acts as a strict
same-origin JSON-RPC gateway: it pins the EntryPoint and SimpleAccount stack,
derives the expected account, decodes every `execute`/`executeBatch` call, and
allows only reviewed Plether action sequences.

The external bundler is straightforward to replace with Alto. Sponsorship is
the security boundary: a leaked signing key, a racy budget counter, or an
incorrect expiry decision can drain the paymaster deposit even when protocol
and trader funds remain safe.

## Decision

Plether will self-host the Sepolia AA stack as follows:

1. A single-active Alto ECS service provides the bundler RPC. It has no public
   listener; the Plether API is its only caller.
2. The existing Haskell API remains the only public AA gateway and becomes the
   ERC-7677 paymaster service. The existing account and action validation is
   shared by estimation, sponsorship, and submission paths. Every
   authorization-critical account, runtime, and fee read is evaluated at one
   explicit `safe` block agreed by two independent RPC providers; both
   providers must return the same result and the block header is re-read before
   reserving, signing, or forwarding an operation.
3. Sponsorship approvals use the non-upgradeable
   `PletherVerifyingPaymaster` and fixed v0.8 envelope owned by
   `plether-core`. The API signs its EIP-712 digest with an AWS KMS
   `ECC_SECG_P256K1` key.
4. PostgreSQL is authoritative for authorization idempotency, maximum-liability
   reservations, actual charges, recovery-read authorization, and rate limits.
   In-memory state may reject traffic early but may not authorize spending.
5. A separate reconciler scans the configured EntryPoint through two
   administratively independent RPC providers and advances only through their
   common `safe` boundary. It is the only component allowed to release a
   signed authorization or settle its actual gas cost. Its initial cursor is
   pinned to the dual-attested paymaster deployment block and hash, and cursor
   bootstrap is refused when AA state already exists.
6. The manifest version suffix identifies a reviewed deployment generation,
   not its transport. The current public v2 manifest deliberately retains the
   exact same-origin Pimlico shape: `pimlicoRpcUrl` and no native fields. The
   parser selects transport only from an exclusive, exact field set. A v1
   suffix accepts only that Pimlico shape; a v2 suffix accepts either the
   Pimlico shape or the native shape containing all of `bundlerRpcUrl`,
   `paymasterRpcUrl`, `paymasterAddress`, and `paymasterVersion` and no
   `pimlicoRpcUrl`. Partial and hybrid shapes fail closed. The Pimlico validity
   decoder remains supported for every Pimlico-shaped browser journal, and
   provider fallback after preparation or submission is forbidden.

The public gateway retains the nine RPC methods used by the application. The
two `pm_*` methods terminate in the API. Bundler methods go to Alto only after
gateway authorization. The `pimlico_*` method names remain wire-compatible
Alto extensions and do not imply a request to Pimlico.

## Sponsorship lifecycle

`pm_getPaymasterStubData` returns fixed-size, deliberately invalid signature
bytes and does not reserve funds. The final `pm_getPaymasterData` request must
contain all estimated gas fields; the API calculates the exact EntryPoint v0.8
maximum prefund, reserves that liability transactionally, obtains a KMS
signature, persists the signature, and only then returns it.

The durable state machine is monotonic:

```text
reserved -> signed -> submitted -> settled
                \---------------> settled
reserved -> cancelled
signed/submitted -> expired
```

`cancelled` is legal only before a signature could have escaped. `settled` and
`expired` require a continuous safe-chain scan. A signature is a bearer
authorization and may be submitted outside Plether's Alto instance, so an Alto
rejection or an API timeout is never sufficient proof to release liability.
Unknown events from Plether's paymaster, a discontinuous reconciler cursor, or
actual gas cost above the reservation pause new issuance and alert operators.

Before any settlement, expiry, cursor advance, or healthy heartbeat, both
reconciliation providers must agree on the chain id, cursor and target block
headers, every event block header, and the complete canonical
`UserOperationEvent` set. Missing, altered, or duplicate logs and a target
header that changes during the scan durably pause issuance. Transient provider
unavailability stops progress and ages the heartbeat; it pauses issuance after
the configured failure threshold. Using two URLs backed by the same provider,
account, or administrative control is not considered independent.

## Unsigned paymaster request exception

ERC-7677 `pm_getPaymasterData` is requested before the account signature is
available, so the API cannot prove that its caller controls the declared
account owner. An attacker can therefore consume rate-limit and reservation
capacity by replaying a public canary owner, although they cannot spend the
paymaster deposit without a valid account signature.

This availability risk is accepted only for the bounded Sepolia canary. The
compensating controls are an explicit owner allowlist, a global-rollout switch
that the current configuration rejects, a dedicated low final-issuance rate,
atomic per-client outstanding-liability limits,
per-operation/account/global/daily caps, short validity, a small dedicated
deposit, and the fail-closed reconciler. Global or mainnet rollout requires a
wallet/session proof protocol for final sponsorship requests and a new
security review.

## Alto safe-mode exception

The pinned Alto v1.2.7 canary runs with `safeMode=false` because the selected
Arbitrum Sepolia RPC has not yet been qualified for Alto's exact custom
`debug_traceCall` tracer and state-override contract. Dangerous validation
skipping remains disabled, so Alto still simulates and validates the account
and paymaster; however, ERC-7562 tracer, entity-role, and reputation checks are
not part of this profile.

This exception is accepted only for the private, allowlisted, low-cap Sepolia
canary. The public frontend remains on its v2 Pimlico transport shape; the
native-v2 field set is exercised through a controlled client harness. Global
Sepolia or mainnet rollout is blocked until the pinned safe-mode tracer is
proven end to end—including a deliberately invalid operation—and the deployed
configuration uses it.

## UUPS exception

The current Sepolia SimpleAccounts are ERC-1967/UUPS proxies. Their runtime
code hash does not bind their implementation, and one contract cannot read
another contract's implementation storage slot. The API therefore checks the
implementation and beacon slots before stub issuance, final signing, and
submission, while the paymaster can bind only the proxy runtime code hash.

An account owner can still upgrade between authorization and inclusion or
submit through another bundler. This residual risk is accepted only for
Arbitrum Sepolia with short validity, a small onchain per-operation ceiling,
database spend limits, and a deliberately small deposit. Deployment tooling
must reject enabling this profile on mainnet. Mainnet sponsorship requires an
immutable or irreversibly upgrade-locked account implementation and a separate
security review.

## Operational consequences

- Alto executor and utility keys are dedicated, low-value SSM secrets. They
  are never shared with keepers, faucets, deployers, treasury, or the KMS
  sponsor signer.
- Alto runs one task without Redis or autoscaling. Deployments stop the old
  task before starting the new one so executor keys are never active twice.
- The paymaster authorizes one signer at a time. KMS rotation therefore pauses
  issuance, waits until every old-signer authorization is settled or expired
  behind the safe reconciler cursor, updates the signer through the Safe, and
  only then resumes issuance with the replacement key.
- New issuance, submission, and read/reconciliation availability have separate
  controls. Disabling issuance must not strand already signed operations.
- Startup attestation pins the exact runtime hashes and configuration of the
  EntryPoint, paymaster, account factory, account implementation, and account
  proxy before the API can issue a sponsorship. The paymaster runtime hash is
  deployment-specific and must be captured after deployment rather than
  guessed in source control.
- The proxy-origin token also derives durable pseudonymous client/account
  keys. It must be generated as 32 random bytes and represented as exactly 64
  lowercase hexadecimal characters; known placeholders are rejected. Rotating
  it is a continuity-breaking emergency operation: pause
  issuance and drain all outstanding authorizations before rotating the
  Worker and API configuration in the documented order.
- No Pimlico credential is required after the Pimlico transport drain
  completes, including both legacy-v1 and current-v2 Pimlico-shaped journals.
