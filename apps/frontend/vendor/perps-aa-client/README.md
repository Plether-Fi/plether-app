# Plether perps account-abstraction client

This package supplies vendor-neutral frontend primitives for sponsored Plether perps actions. It deliberately does not choose a wallet SDK, smart-account implementation, bundler, or paymaster provider.

The connected MetaMask, Rabby, Trust Wallet, or other EOA remains the owner and signature UI. The smart account is the canonical trader address seen by the existing perps contracts. All positions, balances, orders, and claims must therefore be queried by `account.accountAddress`, not by the owner EOA.

## Integration sequence

1. Connect the owner's ordinary wallet.
2. Resolve its deterministic smart-account address through your chosen account implementation.
3. Build a `PerpsActionPlan` with the exported action builders.
4. Pass adapters for the account, sponsor service, and bundler to `sendSponsoredAction`.
5. Render `onStatus` values as one transaction flow: preparing, confirm in wallet, submitting, confirmed.

The orchestrator enforces the v0.8 signing order:

1. Build the operation; the account adapter may retain a dummy signature only for bundler estimation.
2. Request `pm_getPaymasterStubData` and estimate gas.
3. Apply the gas estimate.
4. Request and apply final `pm_getPaymasterData`.
5. Ask the owner wallet to sign the final UserOperation.
6. Submit it to the bundler.

Never replace paymaster data after the owner signs; that changes the EntryPoint UserOperation hash and invalidates the signature.
The adapter may retain a dummy account signature in its internal operation for bundler estimation, but both ERC-7677
paymaster RPC payloads must omit `signature`; the owner supplies the real account signature only after final sponsorship.

## Adapter contract

`SmartAccountAdapter` receives inner calls. For the official EntryPoint v0.8 `SimpleAccount`, the adapter should encode them with `execute(address,uint256,bytes)` for one call or `executeBatch((address target,uint256 value,bytes data)[])` for a batch. Other account types can encode their own execution ABI without changing the perps builders.

`SponsorAdapter` mirrors ERC-7677 but returns either split fields or packed `paymasterAndData`. Its request serializer
must strip any adapter-local dummy `signature`. The parser requires this fixed Plether envelope:

```text
paymaster(20) | verificationGasLimit(16) | postOpGasLimit(16) |
validUntil(6) | validAfter(6) | maxCost(16) | policyId(32) |
accountCodeHash(32) | signature(65)
```

`paymasterData` is 157 bytes and the full envelope is 209 bytes. Stub and final
payloads must have the same fixed length so estimation runs the same validation
path. `accountCodeHash` binds the sponsorship to the validated smart-account
runtime, including an EIP-7702 delegation target or immutable account runtime.
The backend must reject mutable proxy accounts: the proxy runtime code hash does
not bind the implementation and is therefore insufficient authorization.
The initial stub must supply both paymaster gas limits. A standards-compatible
final response may return only `paymaster` and `paymasterData`; the orchestrator
reuses the stub limits and rejects a final response that changes paymaster address.

## First USDC deposit

`buildReceiveWithAuthorizationTypedData` creates the owner signature payload. The domain name and version are required configuration because the SDK does not guess token metadata. After the owner signs it, `buildAuthorizedDepositAction` creates one atomic smart-account batch:

```text
USDC.receiveWithAuthorization(owner, smartAccount, amount, ...)
USDC.approve(clearinghouse, amount)
clearinghouse.depositMargin(amount)
```

The smart account is both the EIP-3009 recipient and the caller of `depositMargin`, preserving the contracts' `msg.sender` ownership invariant. Enable this route only after verifying that the configured USDC implements `receiveWithAuthorization` with the expected EIP-712 domain.

## Trader actions and cancellation

Builders are provided for deposit, commit order, add margin, withdraw, and settle claim. `addMargin(account, amount)` and `settleTraderClaim(account)` always encode the smart-account address as the account argument.

`buildWithdrawAction` calls `withdrawMargin`, so the clearinghouse sends USDC to
the smart account (`msg.sender`). It does not silently append a transfer to the
owner EOA. `buildWithdrawToOwnerAction` provides the explicit atomic alternative:
`withdrawMargin(amount)` followed by `USDC.transfer(owner, amount)`. Both amounts
are identical and both calls carry zero native value. The backend must prove the
recipient is the registered smart-account owner; it must reject arbitrary
recipients, token addresses, extra calls, and mismatched amounts. EIP-7702
same-address accounts should use `buildWithdrawAction` because the withdrawal is
already paid to the owner's address.

Committed delayed orders are binding in the current perps protocol; there is no trader cancellation function. `cancelOrder.supported` is therefore false, and `buildCancelOrderAction` raises `ACTION_UNSUPPORTED`. The UI should show pending/finalizing state and must not display an active Cancel button.

## UI errors and fallback

Use `mapPerpsExecutionError` to turn nested wallet, bundler, paymaster, and contract failures into stable codes and user-safe messages. Do not silently fall back to an EOA transaction: it would create protocol state under a different `msg.sender` and split the user's account. If sponsorship is unavailable, show a retry/support state unless the product has explicitly implemented and disclosed user-paid smart-account gas.
# Position protection (perps v1.2.1)

`buildProtectedOpenAction`, `buildCreateProtectionAction`,
`buildReplaceProtectionAction`, and `buildCancelProtectionAction` encode
account-owned calls to the deployed PositionProtectionBook. Prices are raw
basket prices with 8 decimals; zero disables one leg. Bounties are reserved
from USDC collateral, so all four actions send zero ETH. Protected opens must
use fresh bounded V2 requests and cannot be replayed after an ambiguous result;
reconcile the exact UserOperation and Book events before another submission.
`positionProtectionBookAbi` is generated from source commit
`c3f60f58bcd5dc1b85a28739a5de7ec4a2ee114c` with Solidity 0.8.35.
