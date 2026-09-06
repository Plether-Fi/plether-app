export declare const positionProtectionBookAbi: readonly [{
    readonly type: "constructor";
    readonly inputs: readonly [{
        readonly name: "router";
        readonly type: "address";
        readonly internalType: "address";
    }, {
        readonly name: "engine";
        readonly type: "address";
        readonly internalType: "address";
    }];
    readonly stateMutability: "nonpayable";
}, {
    readonly type: "function";
    readonly name: "ENGINE";
    readonly inputs: readonly [];
    readonly outputs: readonly [{
        readonly name: "";
        readonly type: "address";
        readonly internalType: "contract IPositionProtectionEngine";
    }];
    readonly stateMutability: "view";
}, {
    readonly type: "function";
    readonly name: "ROUTER";
    readonly inputs: readonly [];
    readonly outputs: readonly [{
        readonly name: "";
        readonly type: "address";
        readonly internalType: "address";
    }];
    readonly stateMutability: "view";
}, {
    readonly type: "function";
    readonly name: "activate";
    readonly inputs: readonly [{
        readonly name: "protectionId";
        readonly type: "uint64";
        readonly internalType: "uint64";
    }, {
        readonly name: "markPrice";
        readonly type: "uint256";
        readonly internalType: "uint256";
    }, {
        readonly name: "publishTime";
        readonly type: "uint64";
        readonly internalType: "uint64";
    }, {
        readonly name: "linkedOrderId";
        readonly type: "uint64";
        readonly internalType: "uint64";
    }];
    readonly outputs: readonly [{
        readonly name: "plan";
        readonly type: "tuple";
        readonly internalType: "struct IPositionProtectionBook.TriggerPlan";
        readonly components: readonly [{
            readonly name: "account";
            readonly type: "address";
            readonly internalType: "address";
        }, {
            readonly name: "side";
            readonly type: "uint8";
            readonly internalType: "enum CfdTypes.Side";
        }, {
            readonly name: "size";
            readonly type: "uint256";
            readonly internalType: "uint256";
        }, {
            readonly name: "triggerBountyUsdc";
            readonly type: "uint256";
            readonly internalType: "uint256";
        }, {
            readonly name: "executionBountyUsdc";
            readonly type: "uint256";
            readonly internalType: "uint256";
        }];
    }];
    readonly stateMutability: "nonpayable";
}, {
    readonly type: "function";
    readonly name: "activePositionProtectionId";
    readonly inputs: readonly [{
        readonly name: "account";
        readonly type: "address";
        readonly internalType: "address";
    }];
    readonly outputs: readonly [{
        readonly name: "protectionId";
        readonly type: "uint64";
        readonly internalType: "uint64";
    }];
    readonly stateMutability: "view";
}, {
    readonly type: "function";
    readonly name: "afterOrderTerminal";
    readonly inputs: readonly [{
        readonly name: "orderId";
        readonly type: "uint64";
        readonly internalType: "uint64";
    }, {
        readonly name: "account";
        readonly type: "address";
        readonly internalType: "address";
    }, {
        readonly name: "terminalStatus";
        readonly type: "uint8";
        readonly internalType: "enum IOrderRouterAccounting.OrderStatus";
    }];
    readonly outputs: readonly [];
    readonly stateMutability: "nonpayable";
}, {
    readonly type: "function";
    readonly name: "cancelPositionProtection";
    readonly inputs: readonly [{
        readonly name: "protectionId";
        readonly type: "uint64";
        readonly internalType: "uint64";
    }];
    readonly outputs: readonly [];
    readonly stateMutability: "nonpayable";
}, {
    readonly type: "function";
    readonly name: "commitOpenOrderWithProtection";
    readonly inputs: readonly [{
        readonly name: "request";
        readonly type: "tuple";
        readonly internalType: "struct OrderV2Types.OrderRequest";
        readonly components: readonly [{
            readonly name: "clientOrderId";
            readonly type: "bytes32";
            readonly internalType: "bytes32";
        }, {
            readonly name: "side";
            readonly type: "uint8";
            readonly internalType: "enum CfdTypes.Side";
        }, {
            readonly name: "sizeDelta";
            readonly type: "uint256";
            readonly internalType: "uint256";
        }, {
            readonly name: "marginDelta";
            readonly type: "uint256";
            readonly internalType: "uint256";
        }, {
            readonly name: "targetPrice";
            readonly type: "uint256";
            readonly internalType: "uint256";
        }, {
            readonly name: "isClose";
            readonly type: "bool";
            readonly internalType: "bool";
        }, {
            readonly name: "bounds";
            readonly type: "tuple";
            readonly internalType: "struct OrderV2Types.ExecutionBounds";
            readonly components: readonly [{
                readonly name: "validUntil";
                readonly type: "uint64";
                readonly internalType: "uint64";
            }, {
                readonly name: "allowedExecutionModes";
                readonly type: "uint8";
                readonly internalType: "uint8";
            }, {
                readonly name: "expectedConfigHash";
                readonly type: "bytes32";
                readonly internalType: "bytes32";
            }, {
                readonly name: "maxExecutionBountyUsdc";
                readonly type: "uint256";
                readonly internalType: "uint256";
            }, {
                readonly name: "maxExecutionNotionalUsdc";
                readonly type: "uint256";
                readonly internalType: "uint256";
            }, {
                readonly name: "maxGrossAccountDebitUsdc";
                readonly type: "uint256";
                readonly internalType: "uint256";
            }, {
                readonly name: "maxActionChargeUsdc";
                readonly type: "uint256";
                readonly internalType: "uint256";
            }, {
                readonly name: "maxExplicitFeesUsdc";
                readonly type: "uint256";
                readonly internalType: "uint256";
            }, {
                readonly name: "maxPostPositionSize";
                readonly type: "uint256";
                readonly internalType: "uint256";
            }, {
                readonly name: "minPostSettlementBalanceUsdc";
                readonly type: "uint256";
                readonly internalType: "uint256";
            }, {
                readonly name: "minPostPositionEquityUsdc";
                readonly type: "uint256";
                readonly internalType: "uint256";
            }, {
                readonly name: "maxPostLeverageBps";
                readonly type: "uint32";
                readonly internalType: "uint32";
            }];
        }];
    }, {
        readonly name: "params";
        readonly type: "tuple";
        readonly internalType: "struct PositionProtectionTypes.PositionProtectionParams";
        readonly components: readonly [{
            readonly name: "takeProfitTriggerPrice";
            readonly type: "uint256";
            readonly internalType: "uint256";
        }, {
            readonly name: "stopLossTriggerPrice";
            readonly type: "uint256";
            readonly internalType: "uint256";
        }];
    }];
    readonly outputs: readonly [{
        readonly name: "parentOrderId";
        readonly type: "uint64";
        readonly internalType: "uint64";
    }, {
        readonly name: "protectionId";
        readonly type: "uint64";
        readonly internalType: "uint64";
    }];
    readonly stateMutability: "nonpayable";
}, {
    readonly type: "function";
    readonly name: "createPositionProtection";
    readonly inputs: readonly [{
        readonly name: "params";
        readonly type: "tuple";
        readonly internalType: "struct PositionProtectionTypes.PositionProtectionParams";
        readonly components: readonly [{
            readonly name: "takeProfitTriggerPrice";
            readonly type: "uint256";
            readonly internalType: "uint256";
        }, {
            readonly name: "stopLossTriggerPrice";
            readonly type: "uint256";
            readonly internalType: "uint256";
        }];
    }];
    readonly outputs: readonly [{
        readonly name: "protectionId";
        readonly type: "uint64";
        readonly internalType: "uint64";
    }];
    readonly stateMutability: "nonpayable";
}, {
    readonly type: "function";
    readonly name: "failPendingOpenForRiskOff";
    readonly inputs: readonly [{
        readonly name: "parentOrderId";
        readonly type: "uint64";
        readonly internalType: "uint64";
    }, {
        readonly name: "account";
        readonly type: "address";
        readonly internalType: "address";
    }];
    readonly outputs: readonly [{
        readonly name: "refundableProtectionBountyUsdc";
        readonly type: "uint256";
        readonly internalType: "uint256";
    }];
    readonly stateMutability: "nonpayable";
}, {
    readonly type: "function";
    readonly name: "forfeitOnLiquidation";
    readonly inputs: readonly [{
        readonly name: "account";
        readonly type: "address";
        readonly internalType: "address";
    }];
    readonly outputs: readonly [{
        readonly name: "forfeitedUsdc";
        readonly type: "uint256";
        readonly internalType: "uint256";
    }];
    readonly stateMutability: "nonpayable";
}, {
    readonly type: "function";
    readonly name: "getPositionProtection";
    readonly inputs: readonly [{
        readonly name: "protectionId";
        readonly type: "uint64";
        readonly internalType: "uint64";
    }];
    readonly outputs: readonly [{
        readonly name: "protection";
        readonly type: "tuple";
        readonly internalType: "struct PositionProtectionTypes.PositionProtectionView";
        readonly components: readonly [{
            readonly name: "protectionId";
            readonly type: "uint64";
            readonly internalType: "uint64";
        }, {
            readonly name: "parentOrderId";
            readonly type: "uint64";
            readonly internalType: "uint64";
        }, {
            readonly name: "linkedOrderId";
            readonly type: "uint64";
            readonly internalType: "uint64";
        }, {
            readonly name: "account";
            readonly type: "address";
            readonly internalType: "address";
        }, {
            readonly name: "side";
            readonly type: "uint8";
            readonly internalType: "enum CfdTypes.Side";
        }, {
            readonly name: "size";
            readonly type: "uint256";
            readonly internalType: "uint256";
        }, {
            readonly name: "takeProfitTriggerPrice";
            readonly type: "uint256";
            readonly internalType: "uint256";
        }, {
            readonly name: "stopLossTriggerPrice";
            readonly type: "uint256";
            readonly internalType: "uint256";
        }, {
            readonly name: "triggerBountyUsdc";
            readonly type: "uint256";
            readonly internalType: "uint256";
        }, {
            readonly name: "executionBountyUsdc";
            readonly type: "uint256";
            readonly internalType: "uint256";
        }, {
            readonly name: "armedAt";
            readonly type: "uint64";
            readonly internalType: "uint64";
        }, {
            readonly name: "armedBlock";
            readonly type: "uint64";
            readonly internalType: "uint64";
        }, {
            readonly name: "triggerMarkPrice";
            readonly type: "uint256";
            readonly internalType: "uint256";
        }, {
            readonly name: "triggerPublishTime";
            readonly type: "uint64";
            readonly internalType: "uint64";
        }, {
            readonly name: "triggeredLeg";
            readonly type: "uint8";
            readonly internalType: "enum PositionProtectionTypes.PositionProtectionTriggerLeg";
        }, {
            readonly name: "status";
            readonly type: "uint8";
            readonly internalType: "enum PositionProtectionTypes.PositionProtectionStatus";
        }];
    }];
    readonly stateMutability: "view";
}, {
    readonly type: "function";
    readonly name: "handleFailedProtectionAttempt";
    readonly inputs: readonly [{
        readonly name: "orderId";
        readonly type: "uint64";
        readonly internalType: "uint64";
    }, {
        readonly name: "account";
        readonly type: "address";
        readonly internalType: "address";
    }, {
        readonly name: "reason";
        readonly type: "uint8";
        readonly internalType: "enum OrderV2Types.TerminalReason";
    }, {
        readonly name: "executionBountyUsdc";
        readonly type: "uint256";
        readonly internalType: "uint256";
    }];
    readonly outputs: readonly [{
        readonly name: "retained";
        readonly type: "bool";
        readonly internalType: "bool";
    }];
    readonly stateMutability: "nonpayable";
}, {
    readonly type: "function";
    readonly name: "nextPositionProtectionId";
    readonly inputs: readonly [];
    readonly outputs: readonly [{
        readonly name: "";
        readonly type: "uint64";
        readonly internalType: "uint64";
    }];
    readonly stateMutability: "view";
}, {
    readonly type: "function";
    readonly name: "replacePositionProtection";
    readonly inputs: readonly [{
        readonly name: "protectionId";
        readonly type: "uint64";
        readonly internalType: "uint64";
    }, {
        readonly name: "params";
        readonly type: "tuple";
        readonly internalType: "struct PositionProtectionTypes.PositionProtectionParams";
        readonly components: readonly [{
            readonly name: "takeProfitTriggerPrice";
            readonly type: "uint256";
            readonly internalType: "uint256";
        }, {
            readonly name: "stopLossTriggerPrice";
            readonly type: "uint256";
            readonly internalType: "uint256";
        }];
    }];
    readonly outputs: readonly [];
    readonly stateMutability: "nonpayable";
}, {
    readonly type: "function";
    readonly name: "retryPositionProtectionClose";
    readonly inputs: readonly [{
        readonly name: "protectionId";
        readonly type: "uint64";
        readonly internalType: "uint64";
    }];
    readonly outputs: readonly [{
        readonly name: "linkedOrderId";
        readonly type: "uint64";
        readonly internalType: "uint64";
    }];
    readonly stateMutability: "nonpayable";
}, {
    readonly type: "function";
    readonly name: "triggerPositionProtection";
    readonly inputs: readonly [{
        readonly name: "protectionId";
        readonly type: "uint64";
        readonly internalType: "uint64";
    }, {
        readonly name: "pythUpdateData";
        readonly type: "bytes[]";
        readonly internalType: "bytes[]";
    }];
    readonly outputs: readonly [{
        readonly name: "linkedOrderId";
        readonly type: "uint64";
        readonly internalType: "uint64";
    }];
    readonly stateMutability: "payable";
}, {
    readonly type: "function";
    readonly name: "unpaidBounties";
    readonly inputs: readonly [{
        readonly name: "account";
        readonly type: "address";
        readonly internalType: "address";
    }];
    readonly outputs: readonly [{
        readonly name: "unpaidBountyUsdc";
        readonly type: "uint256";
        readonly internalType: "uint256";
    }];
    readonly stateMutability: "view";
}, {
    readonly type: "event";
    readonly name: "LiquidationBatchItem";
    readonly inputs: readonly [{
        readonly name: "index";
        readonly type: "uint256";
        readonly indexed: true;
        readonly internalType: "uint256";
    }, {
        readonly name: "account";
        readonly type: "address";
        readonly indexed: true;
        readonly internalType: "address";
    }, {
        readonly name: "result";
        readonly type: "uint8";
        readonly indexed: false;
        readonly internalType: "enum IOrderRouterErrors.LiquidationBatchResult";
    }, {
        readonly name: "keeperBountyUsdc";
        readonly type: "uint256";
        readonly indexed: false;
        readonly internalType: "uint256";
    }, {
        readonly name: "errorSelector";
        readonly type: "bytes4";
        readonly indexed: false;
        readonly internalType: "bytes4";
    }];
    readonly anonymous: false;
}, {
    readonly type: "event";
    readonly name: "LiquidationBatchStopped";
    readonly inputs: readonly [{
        readonly name: "nextIndex";
        readonly type: "uint256";
        readonly indexed: true;
        readonly internalType: "uint256";
    }];
    readonly anonymous: false;
}, {
    readonly type: "event";
    readonly name: "OrderCommitted";
    readonly inputs: readonly [{
        readonly name: "orderId";
        readonly type: "uint64";
        readonly indexed: true;
        readonly internalType: "uint64";
    }, {
        readonly name: "account";
        readonly type: "address";
        readonly indexed: true;
        readonly internalType: "address";
    }, {
        readonly name: "side";
        readonly type: "uint8";
        readonly indexed: false;
        readonly internalType: "enum CfdTypes.Side";
    }];
    readonly anonymous: false;
}, {
    readonly type: "event";
    readonly name: "PositionProtectionArmed";
    readonly inputs: readonly [{
        readonly name: "protectionId";
        readonly type: "uint64";
        readonly indexed: true;
        readonly internalType: "uint64";
    }, {
        readonly name: "account";
        readonly type: "address";
        readonly indexed: true;
        readonly internalType: "address";
    }, {
        readonly name: "side";
        readonly type: "uint8";
        readonly indexed: false;
        readonly internalType: "enum CfdTypes.Side";
    }, {
        readonly name: "size";
        readonly type: "uint256";
        readonly indexed: false;
        readonly internalType: "uint256";
    }, {
        readonly name: "armedAt";
        readonly type: "uint64";
        readonly indexed: false;
        readonly internalType: "uint64";
    }, {
        readonly name: "armedBlock";
        readonly type: "uint64";
        readonly indexed: false;
        readonly internalType: "uint64";
    }];
    readonly anonymous: false;
}, {
    readonly type: "event";
    readonly name: "PositionProtectionCancelled";
    readonly inputs: readonly [{
        readonly name: "protectionId";
        readonly type: "uint64";
        readonly indexed: true;
        readonly internalType: "uint64";
    }, {
        readonly name: "account";
        readonly type: "address";
        readonly indexed: true;
        readonly internalType: "address";
    }];
    readonly anonymous: false;
}, {
    readonly type: "event";
    readonly name: "PositionProtectionCloseAttemptFailed";
    readonly inputs: readonly [{
        readonly name: "protectionId";
        readonly type: "uint64";
        readonly indexed: true;
        readonly internalType: "uint64";
    }, {
        readonly name: "account";
        readonly type: "address";
        readonly indexed: true;
        readonly internalType: "address";
    }, {
        readonly name: "linkedOrderId";
        readonly type: "uint64";
        readonly indexed: true;
        readonly internalType: "uint64";
    }, {
        readonly name: "reason";
        readonly type: "uint8";
        readonly indexed: false;
        readonly internalType: "enum OrderV2Types.TerminalReason";
    }, {
        readonly name: "relatched";
        readonly type: "bool";
        readonly indexed: false;
        readonly internalType: "bool";
    }];
    readonly anonymous: false;
}, {
    readonly type: "event";
    readonly name: "PositionProtectionCloseAttemptQueued";
    readonly inputs: readonly [{
        readonly name: "protectionId";
        readonly type: "uint64";
        readonly indexed: true;
        readonly internalType: "uint64";
    }, {
        readonly name: "account";
        readonly type: "address";
        readonly indexed: true;
        readonly internalType: "address";
    }, {
        readonly name: "linkedOrderId";
        readonly type: "uint64";
        readonly indexed: true;
        readonly internalType: "uint64";
    }, {
        readonly name: "previousLinkedOrderId";
        readonly type: "uint64";
        readonly indexed: false;
        readonly internalType: "uint64";
    }];
    readonly anonymous: false;
}, {
    readonly type: "event";
    readonly name: "PositionProtectionCreated";
    readonly inputs: readonly [{
        readonly name: "protectionId";
        readonly type: "uint64";
        readonly indexed: true;
        readonly internalType: "uint64";
    }, {
        readonly name: "account";
        readonly type: "address";
        readonly indexed: true;
        readonly internalType: "address";
    }, {
        readonly name: "parentOrderId";
        readonly type: "uint64";
        readonly indexed: true;
        readonly internalType: "uint64";
    }, {
        readonly name: "takeProfitTriggerPrice";
        readonly type: "uint256";
        readonly indexed: false;
        readonly internalType: "uint256";
    }, {
        readonly name: "stopLossTriggerPrice";
        readonly type: "uint256";
        readonly indexed: false;
        readonly internalType: "uint256";
    }, {
        readonly name: "triggerBountyUsdc";
        readonly type: "uint256";
        readonly indexed: false;
        readonly internalType: "uint256";
    }, {
        readonly name: "executionBountyUsdc";
        readonly type: "uint256";
        readonly indexed: false;
        readonly internalType: "uint256";
    }];
    readonly anonymous: false;
}, {
    readonly type: "event";
    readonly name: "PositionProtectionReplaced";
    readonly inputs: readonly [{
        readonly name: "protectionId";
        readonly type: "uint64";
        readonly indexed: true;
        readonly internalType: "uint64";
    }, {
        readonly name: "account";
        readonly type: "address";
        readonly indexed: true;
        readonly internalType: "address";
    }, {
        readonly name: "takeProfitTriggerPrice";
        readonly type: "uint256";
        readonly indexed: false;
        readonly internalType: "uint256";
    }, {
        readonly name: "stopLossTriggerPrice";
        readonly type: "uint256";
        readonly indexed: false;
        readonly internalType: "uint256";
    }];
    readonly anonymous: false;
}, {
    readonly type: "event";
    readonly name: "PositionProtectionTerminal";
    readonly inputs: readonly [{
        readonly name: "protectionId";
        readonly type: "uint64";
        readonly indexed: true;
        readonly internalType: "uint64";
    }, {
        readonly name: "account";
        readonly type: "address";
        readonly indexed: true;
        readonly internalType: "address";
    }, {
        readonly name: "linkedOrderId";
        readonly type: "uint64";
        readonly indexed: true;
        readonly internalType: "uint64";
    }, {
        readonly name: "status";
        readonly type: "uint8";
        readonly indexed: false;
        readonly internalType: "enum PositionProtectionTypes.PositionProtectionStatus";
    }];
    readonly anonymous: false;
}, {
    readonly type: "event";
    readonly name: "PositionProtectionTriggered";
    readonly inputs: readonly [{
        readonly name: "protectionId";
        readonly type: "uint64";
        readonly indexed: true;
        readonly internalType: "uint64";
    }, {
        readonly name: "account";
        readonly type: "address";
        readonly indexed: true;
        readonly internalType: "address";
    }, {
        readonly name: "linkedOrderId";
        readonly type: "uint64";
        readonly indexed: true;
        readonly internalType: "uint64";
    }, {
        readonly name: "leg";
        readonly type: "uint8";
        readonly indexed: false;
        readonly internalType: "enum PositionProtectionTypes.PositionProtectionTriggerLeg";
    }, {
        readonly name: "triggerMarkPrice";
        readonly type: "uint256";
        readonly indexed: false;
        readonly internalType: "uint256";
    }, {
        readonly name: "triggerPublishTime";
        readonly type: "uint64";
        readonly indexed: false;
        readonly internalType: "uint64";
    }];
    readonly anonymous: false;
}, {
    readonly type: "error";
    readonly name: "EnforcedPause";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__AccountQueueCorrupt";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__BatchBeforeQueueHead";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__BatchOrderNotCommitted";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__CloseOnlyWindow";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__CloseWithPositiveMargin";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__CommitValidation";
    readonly inputs: readonly [{
        readonly name: "code";
        readonly type: "uint8";
        readonly internalType: "uint8";
    }];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__ConditionalTriggerFrozen";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__DegradedMode";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__EmptyFeeds";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__EmptyPythUpdateData";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__ExecutionBountyAboveGrossDebit";
    readonly inputs: readonly [{
        readonly name: "executionBountyUsdc";
        readonly type: "uint256";
        readonly internalType: "uint256";
    }, {
        readonly name: "maximumGrossDebitUsdc";
        readonly type: "uint256";
        readonly internalType: "uint256";
    }];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__ExecutionConfigMismatch";
    readonly inputs: readonly [{
        readonly name: "expectedConfigHash";
        readonly type: "bytes32";
        readonly internalType: "bytes32";
    }, {
        readonly name: "currentConfigHash";
        readonly type: "bytes32";
        readonly internalType: "bytes32";
    }];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__GlobalQueueCorrupt";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__InsufficientFreeEquity";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__InsufficientGas";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__InsufficientPythFee";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__InvalidBasePrice";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__InvalidEngineLens";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__InvalidExecutionModeMask";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__InvalidExecutionSidecar";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__InvalidKeeperSidecar";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__InvalidLifecycleBook";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__InvalidLiquidationBatchSize";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__InvalidOraclePrice";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__InvalidPletherOracle";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__InvalidPolicyEvaluator";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__InvalidProtectionPrices";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__InvalidSizeQuantum";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__InvalidValidUntil";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__InvalidWeights";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__LengthMismatch";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__LiquidationOraclePriceTooStale";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__MarginQueueCorrupt";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__MarkPriceOutOfOrder";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__MevDetected";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__MockOracleUnavailable";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__NoOpenPosition";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__NoOrdersToExecute";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__NoQueuedPosition";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__NotInSeedLifecycle";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__OracleConfidenceTooWide";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__OraclePriceTooStale";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__OraclePublishTimesDiverged";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__OrderNotPending";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__OrderNotQueueHead";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__OrderNotRiskOff";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__PendingOrdersExist";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__PositionChanged";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__PredictableOpenInvalid";
    readonly inputs: readonly [{
        readonly name: "code";
        readonly type: "uint8";
        readonly internalType: "uint8";
    }];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__ProtectionActive";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__ProtectionAlreadyActive";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__ProtectionDisabled";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__ProtectionMarkTooStale";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__ProtectionNotArmed";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__ProtectionNotFound";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__ProtectionNotLatched";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__ProtectionTriggerAlreadyMet";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__SameBlockTrigger";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__SideMismatch";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__SizeExceedsQueued";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__TooManyPendingOrders";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__TriggerNotMet";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__Unauthorized";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__VaultRiskBlocked";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__ZeroClientOrderId";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__ZeroPostLeverageBound";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__ZeroSize";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "OrderRouter__ZeroTargetPrice";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "PositionProtectionBook__BountyMismatch";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "PositionProtectionBook__InvalidHostResponse";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "PositionProtectionBook__InvalidLinkedOrder";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "PositionProtectionBook__InvalidTerminalReason";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "PositionProtectionBook__ZeroAddress";
    readonly inputs: readonly [];
}, {
    readonly type: "error";
    readonly name: "ReentrancyGuardReentrantCall";
    readonly inputs: readonly [];
}];
//# sourceMappingURL=protectionAbi.d.ts.map