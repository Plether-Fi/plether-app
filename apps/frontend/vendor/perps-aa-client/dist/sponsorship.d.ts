import { type Address, type Hex } from "viem";
import type { SponsorshipUserOperation } from "./types.js";
export declare const PLETHER_SPONSORSHIP_DOMAIN_NAME = "PletherVerifyingPaymaster";
export declare const PLETHER_SPONSORSHIP_DOMAIN_VERSION = "1";
export declare const PLETHER_SPONSORSHIP_TYPEHASH: Hex;
export declare const PLETHER_SPONSORSHIP_TYPES: {
    readonly Sponsorship: readonly [{
        readonly name: "sender";
        readonly type: "address";
    }, {
        readonly name: "nonce";
        readonly type: "uint256";
    }, {
        readonly name: "initCodeHash";
        readonly type: "bytes32";
    }, {
        readonly name: "callDataHash";
        readonly type: "bytes32";
    }, {
        readonly name: "accountGasLimits";
        readonly type: "bytes32";
    }, {
        readonly name: "preVerificationGas";
        readonly type: "uint256";
    }, {
        readonly name: "gasFees";
        readonly type: "bytes32";
    }, {
        readonly name: "paymasterVerificationGasLimit";
        readonly type: "uint128";
    }, {
        readonly name: "paymasterPostOpGasLimit";
        readonly type: "uint128";
    }, {
        readonly name: "validUntil";
        readonly type: "uint48";
    }, {
        readonly name: "validAfter";
        readonly type: "uint48";
    }, {
        readonly name: "maxCost";
        readonly type: "uint128";
    }, {
        readonly name: "policyId";
        readonly type: "bytes32";
    }, {
        readonly name: "accountCodeHash";
        readonly type: "bytes32";
    }, {
        readonly name: "entryPoint";
        readonly type: "address";
    }];
};
export declare function getPletherSponsorshipTypedData(input: {
    readonly chainId: number;
    readonly entryPoint: Address;
    readonly userOperation: SponsorshipUserOperation;
}): {
    readonly domain: {
        readonly name: "PletherVerifyingPaymaster";
        readonly version: "1";
        readonly chainId: number;
        readonly verifyingContract: `0x${string}`;
    };
    readonly types: {
        readonly Sponsorship: readonly [{
            readonly name: "sender";
            readonly type: "address";
        }, {
            readonly name: "nonce";
            readonly type: "uint256";
        }, {
            readonly name: "initCodeHash";
            readonly type: "bytes32";
        }, {
            readonly name: "callDataHash";
            readonly type: "bytes32";
        }, {
            readonly name: "accountGasLimits";
            readonly type: "bytes32";
        }, {
            readonly name: "preVerificationGas";
            readonly type: "uint256";
        }, {
            readonly name: "gasFees";
            readonly type: "bytes32";
        }, {
            readonly name: "paymasterVerificationGasLimit";
            readonly type: "uint128";
        }, {
            readonly name: "paymasterPostOpGasLimit";
            readonly type: "uint128";
        }, {
            readonly name: "validUntil";
            readonly type: "uint48";
        }, {
            readonly name: "validAfter";
            readonly type: "uint48";
        }, {
            readonly name: "maxCost";
            readonly type: "uint128";
        }, {
            readonly name: "policyId";
            readonly type: "bytes32";
        }, {
            readonly name: "accountCodeHash";
            readonly type: "bytes32";
        }, {
            readonly name: "entryPoint";
            readonly type: "address";
        }];
    };
    readonly primaryType: "Sponsorship";
    readonly message: {
        readonly sender: `0x${string}`;
        readonly nonce: bigint;
        readonly initCodeHash: `0x${string}`;
        readonly callDataHash: `0x${string}`;
        readonly accountGasLimits: `0x${string}`;
        readonly preVerificationGas: bigint;
        readonly gasFees: `0x${string}`;
        readonly paymasterVerificationGasLimit: bigint;
        readonly paymasterPostOpGasLimit: bigint;
        readonly validUntil: number;
        readonly validAfter: number;
        readonly maxCost: bigint;
        readonly policyId: `0x${string}`;
        readonly accountCodeHash: `0x${string}`;
        readonly entryPoint: `0x${string}`;
    };
};
export declare function hashPletherSponsorship(input: {
    readonly chainId: number;
    readonly entryPoint: Address;
    readonly userOperation: SponsorshipUserOperation;
}): Hex;
//# sourceMappingURL=sponsorship.d.ts.map