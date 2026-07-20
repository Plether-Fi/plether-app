import type { BundlerAdapter, PerpsActionPlan, SmartAccountAdapter, SponsorAdapter, SponsoredExecutionResult, SponsoredExecutionStatus } from "./types.js";
export interface SendSponsoredActionInput<TOperation, TGasEstimate, TReceipt> {
    readonly chainId: number;
    readonly action: PerpsActionPlan;
    readonly account: SmartAccountAdapter<TOperation, TGasEstimate>;
    readonly sponsor: SponsorAdapter<TOperation>;
    readonly bundler: BundlerAdapter<TOperation, TGasEstimate, TReceipt>;
    readonly waitForReceipt?: boolean;
    readonly onStatus?: (status: SponsoredExecutionStatus) => void;
}
/**
 * Executes the ERC-7677/ERC-4337 sequence in signature-safe order. The final
 * paymaster data is installed before the owner signs the UserOperation.
 */
export declare function sendSponsoredAction<TOperation, TGasEstimate, TReceipt>(input: SendSponsoredActionInput<TOperation, TGasEstimate, TReceipt>): Promise<SponsoredExecutionResult<TReceipt>>;
//# sourceMappingURL=orchestrator.d.ts.map