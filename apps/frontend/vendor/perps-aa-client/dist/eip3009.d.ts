import { type Address, type Hex } from "viem";
export declare const receiveWithAuthorizationTypes: {
    readonly ReceiveWithAuthorization: readonly [{
        readonly name: "from";
        readonly type: "address";
    }, {
        readonly name: "to";
        readonly type: "address";
    }, {
        readonly name: "value";
        readonly type: "uint256";
    }, {
        readonly name: "validAfter";
        readonly type: "uint256";
    }, {
        readonly name: "validBefore";
        readonly type: "uint256";
    }, {
        readonly name: "nonce";
        readonly type: "bytes32";
    }];
};
export interface ReceiveWithAuthorization {
    readonly from: Address;
    readonly to: Address;
    readonly value: bigint;
    readonly validAfter: bigint;
    readonly validBefore: bigint;
    readonly nonce: Hex;
}
export interface Eip3009Domain {
    readonly name: string;
    readonly version: string;
    readonly chainId: number;
    readonly verifyingContract: Address;
}
/** Builds the exact EIP-712 payload the owner wallet must sign. */
export declare function buildReceiveWithAuthorizationTypedData(domain: Eip3009Domain, authorization: ReceiveWithAuthorization): {
    readonly domain: {
        readonly name: string;
        readonly version: string;
        readonly chainId: number;
        readonly verifyingContract: `0x${string}`;
    };
    readonly types: {
        readonly ReceiveWithAuthorization: readonly [{
            readonly name: "from";
            readonly type: "address";
        }, {
            readonly name: "to";
            readonly type: "address";
        }, {
            readonly name: "value";
            readonly type: "uint256";
        }, {
            readonly name: "validAfter";
            readonly type: "uint256";
        }, {
            readonly name: "validBefore";
            readonly type: "uint256";
        }, {
            readonly name: "nonce";
            readonly type: "bytes32";
        }];
    };
    readonly primaryType: "ReceiveWithAuthorization";
    readonly message: {
        readonly from: `0x${string}`;
        readonly to: `0x${string}`;
        readonly value: bigint;
        readonly validAfter: bigint;
        readonly validBefore: bigint;
        readonly nonce: Hex;
    };
};
/** Browser-safe random nonce helper. Callers may persist and supply their own nonce instead. */
export declare function createAuthorizationNonce(): Hex;
export declare function splitEip3009Signature(signature: Hex): {
    readonly v: number;
    readonly r: Hex;
    readonly s: Hex;
};
export declare function validateAuthorization(authorization: ReceiveWithAuthorization): void;
//# sourceMappingURL=eip3009.d.ts.map