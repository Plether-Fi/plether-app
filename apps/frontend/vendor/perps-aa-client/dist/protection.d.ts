import { type Address, type ContractFunctionArgs } from "viem";
import { positionProtectionBookAbi } from "./protectionAbi.js";
import type { PerpsActionPlan } from "./types.js";
export type ProtectedOpenRequest = ContractFunctionArgs<typeof positionProtectionBookAbi, "nonpayable", "commitOpenOrderWithProtection">[0];
export interface PositionProtectionParams {
    takeProfitTriggerPrice: bigint;
    stopLossTriggerPrice: bigint;
}
export declare function buildCreateProtectionAction(input: {
    account: Address;
    book: Address;
    params: PositionProtectionParams;
}): PerpsActionPlan;
export declare function buildReplaceProtectionAction(input: {
    account: Address;
    book: Address;
    protectionId: bigint;
    params: PositionProtectionParams;
}): PerpsActionPlan;
export declare function buildCancelProtectionAction(input: {
    account: Address;
    book: Address;
    protectionId: bigint;
}): PerpsActionPlan;
export declare function buildProtectedOpenAction(input: {
    account: Address;
    book: Address;
    request: ProtectedOpenRequest;
    params: PositionProtectionParams;
}): PerpsActionPlan;
//# sourceMappingURL=protection.d.ts.map