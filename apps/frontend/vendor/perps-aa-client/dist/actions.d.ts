import { type Address, type Hex } from "viem";
import { type ReceiveWithAuthorization } from "./eip3009.js";
import type { PerpsActionPlan } from "./types.js";
export type PerpsSide = "BULL" | "BEAR";
export interface BuildAuthorizedDepositInput {
    readonly account: Address;
    readonly usdc: Address;
    readonly clearinghouse: Address;
    readonly authorization: ReceiveWithAuthorization;
    readonly authorizationSignature: Hex;
}
/**
 * Creates one atomic smart-account batch:
 * USDC.receiveWithAuthorization(owner -> account), approve, depositMargin.
 */
export declare function buildAuthorizedDepositAction(input: BuildAuthorizedDepositInput): PerpsActionPlan;
/** Deposit USDC already held by the smart account. */
export declare function buildSmartAccountBalanceDepositAction(input: {
    readonly account: Address;
    readonly usdc: Address;
    readonly clearinghouse: Address;
    readonly amount: bigint;
}): PerpsActionPlan;
export declare function buildPlaceOrderAction(input: {
    readonly account: Address;
    readonly orderRouter: Address;
    readonly side: PerpsSide;
    readonly sizeDelta: bigint;
    readonly marginDelta: bigint;
    readonly targetPrice: bigint;
    readonly isClose: boolean;
}): PerpsActionPlan;
/**
 * The current delayed-order protocol intentionally exposes no trader cancellation.
 * Keep this function so product code cannot accidentally invent a direct-call path.
 */
export declare function buildCancelOrderAction(_input: {
    readonly account: Address;
    readonly orderRouter: Address;
    readonly orderId: bigint;
}): never;
export declare function buildAddMarginAction(input: {
    readonly account: Address;
    readonly cfdEngine: Address;
    readonly amount: bigint;
}): PerpsActionPlan;
export declare function buildWithdrawAction(input: {
    readonly account: Address;
    readonly clearinghouse: Address;
    readonly amount: bigint;
}): PerpsActionPlan;
/**
 * Atomically withdraws from the clearinghouse to the smart account, then sends
 * that exact USDC amount to the account's externally owned wallet.
 *
 * The sponsor backend must independently prove `owner` is the registered owner
 * of `account`; accepting an arbitrary recipient would create a sponsored token
 * transfer primitive.
 */
export declare function buildWithdrawToOwnerAction(input: {
    readonly account: Address;
    readonly owner: Address;
    readonly usdc: Address;
    readonly clearinghouse: Address;
    readonly amount: bigint;
}): PerpsActionPlan;
export declare function buildSettleTraderClaimAction(input: {
    readonly account: Address;
    readonly cfdEngine: Address;
}): PerpsActionPlan;
//# sourceMappingURL=actions.d.ts.map