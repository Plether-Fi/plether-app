/** Minimal ABIs for the trader-facing, sponsored perps action surface. */
export declare const erc20ApproveAbi: readonly [{
    readonly type: "function";
    readonly name: "approve";
    readonly stateMutability: "nonpayable";
    readonly inputs: readonly [{
        readonly name: "spender";
        readonly type: "address";
    }, {
        readonly name: "amount";
        readonly type: "uint256";
    }];
    readonly outputs: readonly [{
        readonly name: "";
        readonly type: "bool";
    }];
}];
export declare const erc20TransferAbi: readonly [{
    readonly type: "function";
    readonly name: "transfer";
    readonly stateMutability: "nonpayable";
    readonly inputs: readonly [{
        readonly name: "to";
        readonly type: "address";
    }, {
        readonly name: "amount";
        readonly type: "uint256";
    }];
    readonly outputs: readonly [{
        readonly name: "";
        readonly type: "bool";
    }];
}];
export declare const eip3009ReceiveWithAuthorizationAbi: readonly [{
    readonly type: "function";
    readonly name: "receiveWithAuthorization";
    readonly stateMutability: "nonpayable";
    readonly inputs: readonly [{
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
    }, {
        readonly name: "v";
        readonly type: "uint8";
    }, {
        readonly name: "r";
        readonly type: "bytes32";
    }, {
        readonly name: "s";
        readonly type: "bytes32";
    }];
    readonly outputs: readonly [];
}];
export declare const marginClearinghouseTraderAbi: readonly [{
    readonly type: "function";
    readonly name: "depositMargin";
    readonly stateMutability: "nonpayable";
    readonly inputs: readonly [{
        readonly name: "amount";
        readonly type: "uint256";
    }];
    readonly outputs: readonly [];
}, {
    readonly type: "function";
    readonly name: "withdrawMargin";
    readonly stateMutability: "nonpayable";
    readonly inputs: readonly [{
        readonly name: "amount";
        readonly type: "uint256";
    }];
    readonly outputs: readonly [];
}];
export declare const orderRouterTraderAbi: readonly [{
    readonly type: "function";
    readonly name: "commitOrder";
    readonly stateMutability: "nonpayable";
    readonly inputs: readonly [{
        readonly name: "side";
        readonly type: "uint8";
    }, {
        readonly name: "sizeDelta";
        readonly type: "uint256";
    }, {
        readonly name: "marginDelta";
        readonly type: "uint256";
    }, {
        readonly name: "targetPrice";
        readonly type: "uint256";
    }, {
        readonly name: "isClose";
        readonly type: "bool";
    }];
    readonly outputs: readonly [];
}];
export declare const cfdEngineTraderAbi: readonly [{
    readonly type: "function";
    readonly name: "addMargin";
    readonly stateMutability: "nonpayable";
    readonly inputs: readonly [{
        readonly name: "account";
        readonly type: "address";
    }, {
        readonly name: "amount";
        readonly type: "uint256";
    }];
    readonly outputs: readonly [];
}, {
    readonly type: "function";
    readonly name: "settleTraderClaim";
    readonly stateMutability: "nonpayable";
    readonly inputs: readonly [{
        readonly name: "account";
        readonly type: "address";
    }];
    readonly outputs: readonly [];
}];
//# sourceMappingURL=abis.d.ts.map