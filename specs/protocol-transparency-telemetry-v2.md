# Protocol transparency telemetry — next non-upgradeable release

This is the implementation contract for observability events in the next
non-upgradeable Plether deployment. It is intentionally separate from the
current Arbitrum Sepolia release: the current explorer may only infer facts
from receipts, existing logs, inputs and state snapshots, and must label those
facts with their evidence level.

The events in this document are accounting-neutral at the protocol accounting
and state-machine layer. They must not write telemetry-only storage, move
assets, change authorisation, select a different branch or feed emitted values
back into protocol calculations. Log gas is nevertheless a real transaction
cost and must be included in keeper gas-cost reporting and gas-budget tests.

Each required event must persist for every terminal action whose outer
transaction succeeds, including caught failure cleanup, expiry/cleanup and
liquidation. EVM logs are reverted with the rest of the transaction: an
uncaught reverting attempt cannot leave receipt telemetry, even if an `emit`
was executed before the revert. Reverted attempts may only appear in the
explorer when a configured provider offers complete transaction/call traces;
they must be labelled as trace-sourced with the provider coverage policy.
Otherwise they are `unavailable`, and the absence of a log must never be
presented as proof that no reverted attempt occurred.

## Versioning and common conventions

- Every event below includes `uint16 telemetryVersion`, initially `2`.
- All amounts are unrounded native contract integers. Events must not emit
  formatted decimals; the ABI or release manifest defines each amount's unit
  and scale.
- Identifiers must be stable across a reorg replay: use the stored order ID,
  request ID, epoch ID, proposal ID or account address, never a log index as an
  identifier.
- `before` and `after` values bracket the complete named protocol action, not
  an arbitrary individual `SSTORE`. `before` is captured before its first
  accounting mutation and `after` is read after its final accounting mutation
  and successful asset transfer.
- `reasonCode` is a documented enum. An unknown code must be rendered as
  `unknown(<code>)`, not silently mapped to a success state.
- Every event has an `applicabilityFlags` bitset defined in the release
  manifest. A cleared bit means its associated field is not applicable; a set
  bit means the emitted value is applicable, including when that value is
  legitimately zero. Indexers must not infer applicability from zero.
- `bytes32 contextHash` may commit to a canonical expanded evidence payload
  whose encoding is defined in the release manifest. The hash is supplementary:
  it must never be the sole carrier of a queryable protocol fact, and a hash
  without its payload is evidence of a commitment, not evidence of the
  committed contents.

Events for an action are emitted as its final non-reverting protocol step,
after all fallible external interactions and storage writes have succeeded.
No external call or other operation that can intentionally fail may follow the
telemetry sequence. If an uncaught failure still reverts the outer transaction,
all of the sequence's logs disappear as required by EVM semantics.

The canonical event schema is part of the release manifest, alongside contract
addresses, deployment block, calculation version and decimal scales. A release
must not change an event's indexed-field ordering without incrementing the
telemetry version.

## Explorer evidence mapping

The immutable ledger stores the receipt log's address, topics and data before
decoding it. A decoded telemetry field is `exact` evidence that the successful
transaction emitted that integer or enum. Contract tests below make those
fields trustworthy representations of the resulting accounting state; the
indexer must retain the raw-log reference so that claim remains auditable.

Any value calculated by the explorer from one or more exact fields remains
`derived`, with a formula identifier, calculation version, source block and
evidence references. It does not become `exact` merely because all of its
inputs are exact. A comparison of snapshots at `block - 1` and `block` remains
a `block-level delta` whenever more than one relevant protocol transaction
shares the block. Missing archive reads, payloads or trace coverage are
`unavailable` with a machine-readable reason; they must not be zero-filled.

## Order intent and terminal settlement

The router emits the complete committed intent exactly once after reserving its
margin and execution reward:

```solidity
event OrderCommittedTelemetry(
    uint16 telemetryVersion,
    bytes32 indexed orderId,
    address indexed account,
    address indexed committer,
    uint8 action,
    uint8 side,
    uint256 sizeDelta,
    int256 marginDelta,
    uint256 acceptablePrice,
    uint256 committedMargin,
    uint256 executionReward,
    uint64 commitTimestamp,
    uint64 earliestRevealTimestamp,
    uint64 expiryTimestamp,
    uint64 fifoSequence,
    uint256 applicabilityFlags,
    bytes32 contextHash
);

event OrderTerminalTelemetry(
    uint16 telemetryVersion,
    bytes32 indexed orderId,
    address indexed account,
    address indexed executor,
    uint8 outcome,
    uint8 reasonCode,
    uint8 marketMode,
    uint64 firstEligibleRevealTimestamp,
    uint64 minOraclePublishTime,
    uint64 maxOraclePublishTime,
    uint64 terminalTimestamp,
    uint256 executionPrice,
    uint256 acceptedPriceBoundary,
    uint256 grossExecutionReward,
    uint256 forfeitedExecutionReward,
    uint256 immediatePayout,
    uint256 claimCreated,
    uint256 claimConsumed,
    uint256 applicabilityFlags,
    bytes32 contextHash
);
```

`outcome` covers execution, expiry, invalid-oracle failure, rejected price,
cancelled and liquidation-cleared. `marketMode` includes normal, close-only,
frozen and FAD modes. `minOraclePublishTime` and `maxOraclePublishTime` cover
all six Pyth components used for the terminal price. The terminal event must
also be emitted when no reward is paid.

## Position, fee and settlement components

The engine emits a position transition event for every terminal action which
can alter a position, including a no-op terminal failure when it clears a
reservation. This makes position history and liquidation eligibility auditable
without relying on a historical archive node.

```solidity
event PositionSettlementTelemetry(
    uint16 telemetryVersion,
    bytes32 indexed orderId,
    address indexed account,
    uint8 transition,
    int256 sizeBefore,
    int256 sizeAfter,
    uint256 marginBefore,
    uint256 marginAfter,
    uint256 entryPriceBefore,
    uint256 entryPriceAfter,
    int256 equityBefore,
    int256 equityAfter,
    int256 realizedPnl,
    int256 realizedCarry,
    int256 realizedVpi,
    uint256 protocolFee,
    uint256 frozenSpread,
    uint256 executionReward,
    uint256 seizedCollateral,
    uint256 traderResidual,
    uint256 badDebt,
    uint256 claimCreated,
    uint256 claimConsumed,
    uint256 applicabilityFlags,
    bytes32 contextHash
);
```

`transition` distinguishes open, increase, reduce, close, liquidation,
margin-add, margin-remove and reservation-clear. Signs use the protocol's
canonical signed representation; the emitted value must be exactly the value
written to storage or used in the transfer/accounting operation.

For liquidations, capture the following record's eligibility and collateral
inputs before any pool allocation, then emit it in the final telemetry sequence
after the allocation succeeds. It establishes the observed margin regime
rather than claiming an inferred liquidation price as fact.

```solidity
event LiquidationTelemetry(
    uint16 telemetryVersion,
    address indexed account,
    address indexed liquidator,
    bytes32 indexed liquidationId,
    uint256 maintenanceMarginRequirement,
    int256 observedEquity,
    uint256 adverseConfidencePrice,
    uint256 reachableCollateral,
    uint256 liquidationBounty,
    uint256 pendingOrdersCleared,
    uint256 forfeitedRewards,
    uint256 traderResidual,
    uint256 traderClaimCreated,
    uint256 badDebt,
    uint256 applicabilityFlags,
    bytes32 contextHash
);
```

## HousePool and waterfall accounting

Every storage-changing HousePool action emits one complete accounting delta.
Its `after` fields are read after the action's storage changes and successful
asset transfers, and it is emitted in the final telemetry sequence. Contracts
must retain their normal reentrancy protection while doing so.

```solidity
event HousePoolAccountingTelemetry(
    uint16 telemetryVersion,
    bytes32 indexed actionId,
    uint8 indexed actionType,
    address indexed account,
    int256 rawAssetsDelta,
    int256 accountedAssetsDelta,
    int256 freeUsdcDelta,
    int256 withdrawalReserveDelta,
    int256 traderClaimsDelta,
    int256 badDebtDelta,
    int256 longBoundedLiabilityDelta,
    int256 shortBoundedLiabilityDelta,
    int256 pendingRevenueDelta,
    int256 pendingRecapitalizationDelta,
    uint256 rawAssetsAfter,
    uint256 accountedAssetsAfter,
    uint256 freeUsdcAfter,
    uint256 traderClaimsAfter,
    uint256 badDebtAfter,
    uint256 applicabilityFlags,
    bytes32 contextHash
);

event WaterfallAllocationTelemetry(
    uint16 telemetryVersion,
    bytes32 indexed allocationId,
    uint8 indexed cause,
    int256 seniorAllocation,
    int256 juniorAllocation,
    uint256 seniorPrincipalBefore,
    uint256 seniorPrincipalAfter,
    uint256 seniorHighWaterMarkBefore,
    uint256 seniorHighWaterMarkAfter,
    uint256 juniorPrincipalBefore,
    uint256 juniorPrincipalAfter,
    bool degradedBefore,
    bool degradedAfter,
    uint256 applicabilityFlags,
    bytes32 contextHash
);
```

`actionType` includes trader settlement, claim creation/consumption, bad-debt
recognition, withdrawal reserve movement, recapitalization and LP settlement.
`cause` includes trading revenue/loss, liquidation loss, coupon transfer and
recapitalization. Together, both events reconcile a transaction's pool and
tranche effects without over-attributing a block-level aggregate.

## Permissionless maintenance and operational-wallet liveness

The current release's standalone `updateMarkPrice(bytes[])` path can change the
cached mark without emitting a dedicated event. It is permissionless, so there
is no authoritative onchain "oracle updater" role, while the deployment may
still rely on a publicly identified automation wallet for routine liveness.
Release metadata therefore carries an optional, explicitly public operational
wallet registry. It contains addresses, role descriptions, and representative
public transaction selectors/hashes/blocks so the classification is auditable;
private keys, signing payloads, provider details and internal worker state are
never release metadata.

The next release emits one terminal event for every successful permissionless
maintenance call, including standalone mark updates and carry checkpoints:

```solidity
event PermissionlessMaintenanceTelemetry(
    uint16 telemetryVersion,
    bytes32 indexed maintenanceId,
    address indexed executor,
    uint8 indexed actionType,
    uint256 markPrice,
    uint64 minOraclePublishTime,
    uint64 maxOraclePublishTime,
    uint256 nativeOracleFee,
    uint256 applicabilityFlags,
    bytes32 contextHash
);
```

`actionType` distinguishes standalone mark update, carry checkpoint, queue
prune and LP maintenance. `nativeOracleFee` is the exact native amount passed
to the oracle update path, not the transaction's gas cost. Explorer gas cost
continues to come from the canonical receipt. Operational-wallet "estimated
transactions at observed gross native spend" is a derived diagnostic:

`confirmed native balance / median observed successful operational-transaction gross native spend`,

where one sample is one distinct successful operational transaction and gross
native spend is receipt gas cost plus the transaction's full native value when
both are available. A batched transaction is therefore one capacity unit, even
if it produces multiple protocol actions. The current-release calculation does
not net native-value refunds because canonical traces or refund telemetry are
not available; it is a conservative historical gross-spend diagnostic, not an
exact net-cost calculation. It exposes the sample window, sample count, formula
version, incomplete-receipt count and refund-coverage limitation. It is never
described as profit, a guarantee, an action count, or a time-based runway. A
missing public wallet identity or gross-spend sample is `unavailable`, not
healthy.

## Tranche request, epoch, claim and withdrawal lifecycle

Each vault emits a lifecycle event for every externally observable stage.
`tranche` is an enum (Senior or Junior) so cross-vault reporting remains
possible even if vault addresses change in a future release.

```solidity
event TrancheRequestTelemetry(
    uint16 telemetryVersion,
    uint8 indexed tranche,
    bytes32 indexed requestId,
    address indexed owner,
    uint8 requestType,
    uint256 assets,
    uint256 shares,
    uint256 feeAssets,
    uint64 cooldownEndsAt,
    uint64 epochId,
    uint256 applicabilityFlags,
    bytes32 contextHash
);

event TrancheEpochTelemetry(
    uint16 telemetryVersion,
    uint8 indexed tranche,
    uint64 indexed epochId,
    uint8 stage,
    uint256 assetsRequested,
    uint256 sharesRequested,
    uint256 assetsFinalized,
    uint256 sharesFinalized,
    uint256 assetsClaimable,
    uint256 sharesClaimable,
    uint256 sharePrice,
    uint256 withdrawalCapacity,
    uint256 applicabilityFlags,
    bytes32 contextHash
);

event TrancheClaimTelemetry(
    uint16 telemetryVersion,
    uint8 indexed tranche,
    bytes32 indexed requestId,
    address indexed owner,
    uint8 claimType,
    uint256 assets,
    uint256 shares,
    uint256 feeAssets,
    uint256 vaultAssetsBefore,
    uint256 vaultAssetsAfter,
    uint256 totalSupplyBefore,
    uint256 totalSupplyAfter,
    uint256 applicabilityFlags,
    bytes32 contextHash
);
```

`requestType` covers deposit, mint, withdraw, redeem and cancellation.
`stage` covers opened, finalized, cancelled, expired and paused. `claimType`
covers share mint, asset withdrawal and cancellation return. Coupon checkpoints,
waterfall allocations, recapitalizations, pauses and parameter changes must
also emit either a `WaterfallAllocationTelemetry`, `HousePoolAccountingTelemetry`
or `ParameterChangeTelemetry`. Queryable scope is carried by the explicit
`tranche`, `target`, `parameterKey`, `cause` and `actionType` fields; it must
not exist only inside `contextHash`.

## Governance, timelock and dependency changes

All mutable configuration uses one generic, stable parameter identifier. The
event includes canonical old and new values and the timelock lifecycle so an
indexer can rebuild governance history after a projection outage. Pending
structs in governance/admin contract storage remain the authoritative live
source and must be read directly at one confirmed block; telemetry does not
make a stale or unavailable indexer current.

```solidity
event ParameterChangeTelemetry(
    uint16 telemetryVersion,
    bytes32 indexed proposalId,
    bytes32 indexed parameterKey,
    address indexed target,
    uint8 lifecycle,
    address proposer,
    address executor,
    uint8 valueEncoding,
    bytes32 oldValueHash,
    bytes32 newValueHash,
    bytes oldValue,
    bytes newValue,
    uint64 proposedAt,
    uint64 eta,
    uint64 effectiveAt,
    uint256 applicabilityFlags,
    bytes32 contextHash
);
```

`lifecycle` is proposed, cancelled, superseded or executed. A parameter key is
stable across releases and maps to the checked-in catalog's getter, scale,
description, risk interpretation, mutability and timelock policy. Ownership,
pending ownership, pauser, treasury, dependency address and emergency-pause
changes are all parameter changes, even when their raw values are addresses or
booleans rather than numeric values.

`valueEncoding` selects a catalog-defined ABI type. `oldValue` and `newValue`
are the canonical `abi.encode` results for that exact type; this supports
dynamic values such as calendar arrays as well as scalars. Their hashes must be
`keccak256(oldValue)` and `keccak256(newValue)`. If a value is not applicable
to a lifecycle stage, its applicability bit is cleared and its bytes are empty;
the hash is then `keccak256(bytes(""))`. Implementations may replace the inline
bytes with a separate value-payload event only when that event is keyed by
`proposalId`, emitted in the same successful transaction, and carries the
canonical bytes and hashes. A hash alone or an off-chain payload is
insufficient for governance replay.

## Required tests and reconciliation gates

Each release must include contract and indexer tests proving the following:

1. Every terminal branch that commits state, including a caught failure that
   performs cleanup, persists its required telemetry before the successful
   outer transaction completes. Tests for uncaught reverting branches assert
   that no receipt log survives and that no state commits. Reverted-attempt
   coverage is tested separately through traces when the configured provider
   supports complete call indexing; it is never simulated with a supposedly
   persistent pre-revert event.
2. For every order action fixture, decode the receipt and reconstruct the
   committed intent, terminal outcome, oracle publish range, fees, carry, VPI,
   reward, claims, position before/after and pool/tranche allocation. The
   reconstructed post-state must equal contract storage exactly.
3. For each liquidation fixture, reconcile reachable collateral, bounty,
   cleared orders, forfeited rewards, residual/claim, bad debt and HousePool
   solvency deltas exactly. Include adverse-confidence and frozen-oracle paths.
4. For every tranche lifecycle fixture, replay deposits, requests,
   cancellations, finalizations, claims and withdrawals from events and match
   vault assets, total supply, request state and withdrawal capacity exactly.
5. Replay every scalar and dynamic governance fixture from
   `ParameterChangeTelemetry`, verify each canonical value hash, and match the
   current value, pending value, ETA, proposal status and dependency/role
   address in storage exactly. Include at least one dynamic calendar-array
   parameter.
6. For every permissionless maintenance path, reconcile the executor, mark
   price, six-component publish-time range and native oracle fee with calldata,
   receipt value and resulting engine state. Assert that standalone mark
   updates are attributable without transaction tracing.
7. Property-test signed arithmetic and scale bounds. No event value may be
   derived from a rounded display value, and no raw amount may overflow its
   declared ABI type.
8. Run the event-count parity check from deployment block to confirmed head:
   each monitored receipt log must round-trip byte-for-byte through
   `eth_getLogs` and exactly one immutable ledger row. Derived projections must
   reference their complete source-log set; their row counts need not equal log
   counts because one action can consume multiple events. Reorg tests must
   delete and reproject only orphaned rows.

Release acceptance is blocked if any reconciliation requires a block-level
delta or `unavailable` provenance for a value covered by this event contract
on a successful/caught terminal path. Reverted attempts remain governed by the
trace-coverage rule above and must not be mislabelled as receipt-log facts.
