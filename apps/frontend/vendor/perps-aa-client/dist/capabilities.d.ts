import type { Address } from "viem";
import type { PerpsActionKind } from "./types.js";
export interface WalletCapabilities {
    /** EIP-712 signing is needed only when USDC starts in the owner EOA. */
    readonly canSignTypedData: boolean;
}
export interface SmartAccountCapabilities {
    readonly accountAddress: Address;
    readonly canExecuteBatch: boolean;
    readonly entryPointVersion: "0.8";
}
export interface SponsorCapabilities {
    readonly available: boolean;
    readonly sponsoredActions: ReadonlySet<PerpsActionKind>;
}
export interface TokenCapabilities {
    readonly supportsReceiveWithAuthorization: boolean;
}
export interface ActionCapability {
    readonly supported: boolean;
    readonly sponsored: boolean;
    readonly reason?: string;
}
export interface PerpsCapabilities {
    /** MetaMask/Rabby/Trust remains the owner/signature UI. */
    readonly ownerWalletRemainsConnected: true;
    /** Protocol state is keyed by the smart account because contracts use msg.sender. */
    readonly smartAccountIsCanonicalTrader: true;
    readonly firstDeposit: ActionCapability;
    readonly placeOrder: ActionCapability;
    readonly cancelOrder: ActionCapability;
    readonly addMargin: ActionCapability;
    readonly withdraw: ActionCapability;
    readonly withdrawToOwner: ActionCapability;
    readonly settleClaim: ActionCapability;
}
export declare function derivePerpsCapabilities(input: {
    readonly wallet: WalletCapabilities;
    readonly smartAccount: SmartAccountCapabilities;
    readonly sponsor: SponsorCapabilities;
    readonly token: TokenCapabilities;
}): PerpsCapabilities;
//# sourceMappingURL=capabilities.d.ts.map