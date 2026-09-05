import { type Address, type Hex } from "viem";
import type { BundlerAdapter, PerpsActionPlan, PletherPaymasterProfile, SmartAccountAdapter, SponsorAdapter, SponsoredExecutionResult, SponsoredExecutionStatus } from "./types.js";
export interface SendSponsoredActionInput<TOperation, TGasEstimate, TReceipt> {
    readonly chainId: number;
    readonly action: PerpsActionPlan;
    readonly account: SmartAccountAdapter<TOperation, TGasEstimate>;
    readonly sponsor: SponsorAdapter<TOperation>;
    readonly bundler: BundlerAdapter<TOperation, TGasEstimate, TReceipt>;
    readonly paymasterProfile: PletherPaymasterProfile;
    /**
     * Must durably persist the exact signed operation before resolving. It returns
     * the locally computed EntryPoint v0.8 UserOperation hash.
     */
    readonly journalSignedUserOperation: (input: {
        readonly operation: TOperation;
        readonly entryPoint: Address;
    }) => Promise<Hex>;
    readonly waitForReceipt?: boolean;
    readonly onStatus?: (status: SponsoredExecutionStatus) => void;
}
/**
 * Executes the ERC-7677/ERC-4337 sequence in signature-safe order. The final
 * paymaster data is installed before the owner signs the UserOperation.
 */
export declare function sendSponsoredAction<TOperation, TGasEstimate, TReceipt>(input: SendSponsoredActionInput<TOperation, TGasEstimate, TReceipt>): Promise<SponsoredExecutionResult<TReceipt>>;
//# sourceMappingURL=orchestrator.d.ts.map