// Generated application subsets from the checksum-verified v1.3.0 archive.
import { orderRouterV3TraderAbi, positionProtectionBookAbi } from '@plether-fi/perps-aa-client'
export { PERPS_CFD_CLOSE_PREVIEW_ABI } from './CfdClosePreview'
export const PERPS_ORDER_REQUEST_V3_COMPONENTS = orderRouterV3TraderAbi[0].inputs[0].components
export const PERPS_EXECUTION_BOUNDS_COMPONENTS = PERPS_ORDER_REQUEST_V3_COMPONENTS[6].components
export const PERPS_POSITION_PROTECTION_BOOK_ABI = positionProtectionBookAbi

export const PERPS_PUBLIC_LENS_ABI = [
  {
    "type": "function",
    "name": "ENGINE",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "ORDER_ROUTER",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "HOUSE_POOL",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getProtocolStatus",
    "inputs": [],
    "outputs": [
      {
        "name": "viewData",
        "type": "tuple",
        "components": [
          {
            "name": "phase",
            "type": "uint8"
          },
          {
            "name": "lastMarkPrice",
            "type": "uint256"
          },
          {
            "name": "lastMarkTime",
            "type": "uint64"
          },
          {
            "name": "oracleFrozen",
            "type": "bool"
          },
          {
            "name": "fadWindow",
            "type": "bool"
          },
          {
            "name": "tradingActive",
            "type": "bool"
          },
          {
            "name": "withdrawalLive",
            "type": "bool"
          },
          {
            "name": "lpEpochSettlementPaused",
            "type": "bool"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getLpStatus",
    "inputs": [],
    "outputs": [
      {
        "name": "viewData",
        "type": "tuple",
        "components": [
          {
            "name": "tradingActive",
            "type": "bool"
          },
          {
            "name": "withdrawalLive",
            "type": "bool"
          },
          {
            "name": "lastMarkTime",
            "type": "uint64"
          },
          {
            "name": "oracleFresh",
            "type": "bool"
          },
          {
            "name": "oracleFrozen",
            "type": "bool"
          },
          {
            "name": "lpEpochSettlementPaused",
            "type": "bool"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getTraderAccount",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "viewData",
        "type": "tuple",
        "components": [
          {
            "name": "equityUsdc",
            "type": "uint256"
          },
          {
            "name": "withdrawableUsdc",
            "type": "uint256"
          },
          {
            "name": "pendingOrderMarginUsdc",
            "type": "uint256"
          },
          {
            "name": "pendingExecutionBountyUsdc",
            "type": "uint256"
          },
          {
            "name": "hasOpenPosition",
            "type": "bool"
          },
          {
            "name": "liquidatable",
            "type": "bool"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getPosition",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "viewData",
        "type": "tuple",
        "components": [
          {
            "name": "exists",
            "type": "bool"
          },
          {
            "name": "side",
            "type": "uint8"
          },
          {
            "name": "size",
            "type": "uint256"
          },
          {
            "name": "entryPrice",
            "type": "uint256"
          },
          {
            "name": "marginUsdc",
            "type": "uint256"
          },
          {
            "name": "unrealizedPnlUsdc",
            "type": "int256"
          },
          {
            "name": "maintenanceMarginUsdc",
            "type": "uint256"
          },
          {
            "name": "liquidatable",
            "type": "bool"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getPendingOrders",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "pending",
        "type": "tuple[]",
        "components": [
          {
            "name": "orderId",
            "type": "uint64"
          },
          {
            "name": "side",
            "type": "uint8"
          },
          {
            "name": "sizeDelta",
            "type": "uint256"
          },
          {
            "name": "marginDeltaUsdc",
            "type": "int256"
          },
          {
            "name": "acceptablePrice",
            "type": "uint256"
          },
          {
            "name": "isReduceOnly",
            "type": "bool"
          },
          {
            "name": "status",
            "type": "uint8"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getActivePositionProtection",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "protection",
        "type": "tuple",
        "components": [
          {
            "name": "protectionId",
            "type": "uint64"
          },
          {
            "name": "parentOrderId",
            "type": "uint64"
          },
          {
            "name": "linkedOrderId",
            "type": "uint64"
          },
          {
            "name": "account",
            "type": "address"
          },
          {
            "name": "side",
            "type": "uint8"
          },
          {
            "name": "size",
            "type": "uint256"
          },
          {
            "name": "takeProfitTriggerPrice",
            "type": "uint256"
          },
          {
            "name": "stopLossTriggerPrice",
            "type": "uint256"
          },
          {
            "name": "triggerBountyUsdc",
            "type": "uint256"
          },
          {
            "name": "executionBountyUsdc",
            "type": "uint256"
          },
          {
            "name": "armedAt",
            "type": "uint64"
          },
          {
            "name": "armedBlock",
            "type": "uint64"
          },
          {
            "name": "triggerMarkPrice",
            "type": "uint256"
          },
          {
            "name": "triggerPublishTime",
            "type": "uint64"
          },
          {
            "name": "triggeredLeg",
            "type": "uint8"
          },
          {
            "name": "status",
            "type": "uint8"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "isLiquidatable",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getSeniorTranche",
    "inputs": [],
    "outputs": [
      {
        "name": "viewData",
        "type": "tuple",
        "components": [
          {
            "name": "totalAssetsUsdc",
            "type": "uint256"
          },
          {
            "name": "totalShares",
            "type": "uint256"
          },
          {
            "name": "effectiveTotalShares",
            "type": "uint256"
          },
          {
            "name": "pendingMaintenanceFeeShares",
            "type": "uint256"
          },
          {
            "name": "maintenanceFeeAprBps",
            "type": "uint256"
          },
          {
            "name": "maintenanceFeeRecipient",
            "type": "address"
          },
          {
            "name": "sharePrice",
            "type": "uint256"
          },
          {
            "name": "maxWithdrawUsdc",
            "type": "uint256"
          },
          {
            "name": "frozenLpFeeBps",
            "type": "uint256"
          },
          {
            "name": "depositEnabled",
            "type": "bool"
          },
          {
            "name": "withdrawEnabled",
            "type": "bool"
          },
          {
            "name": "oracleFrozen",
            "type": "bool"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getJuniorTranche",
    "inputs": [],
    "outputs": [
      {
        "name": "viewData",
        "type": "tuple",
        "components": [
          {
            "name": "totalAssetsUsdc",
            "type": "uint256"
          },
          {
            "name": "totalShares",
            "type": "uint256"
          },
          {
            "name": "effectiveTotalShares",
            "type": "uint256"
          },
          {
            "name": "pendingMaintenanceFeeShares",
            "type": "uint256"
          },
          {
            "name": "maintenanceFeeAprBps",
            "type": "uint256"
          },
          {
            "name": "maintenanceFeeRecipient",
            "type": "address"
          },
          {
            "name": "sharePrice",
            "type": "uint256"
          },
          {
            "name": "maxWithdrawUsdc",
            "type": "uint256"
          },
          {
            "name": "frozenLpFeeBps",
            "type": "uint256"
          },
          {
            "name": "depositEnabled",
            "type": "bool"
          },
          {
            "name": "withdrawEnabled",
            "type": "bool"
          },
          {
            "name": "oracleFrozen",
            "type": "bool"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getTrancheQueues",
    "inputs": [
      {
        "name": "isSenior",
        "type": "bool"
      }
    ],
    "outputs": [
      {
        "name": "viewData",
        "type": "tuple",
        "components": [
          {
            "name": "vault",
            "type": "address"
          },
          {
            "name": "currentEpoch",
            "type": "uint256"
          },
          {
            "name": "cutoffEpoch",
            "type": "uint256"
          },
          {
            "name": "nextRequestEpoch",
            "type": "uint256"
          },
          {
            "name": "nextRequestCutoffTime",
            "type": "uint256"
          },
          {
            "name": "depositHeadEpoch",
            "type": "uint256"
          },
          {
            "name": "depositHeadAssets",
            "type": "uint256"
          },
          {
            "name": "redeemHeadEpoch",
            "type": "uint256"
          },
          {
            "name": "redeemHeadShares",
            "type": "uint256"
          },
          {
            "name": "depositBacklog",
            "type": "bool"
          },
          {
            "name": "redeemBacklog",
            "type": "bool"
          },
          {
            "name": "settlementLive",
            "type": "bool"
          },
          {
            "name": "poolPaused",
            "type": "bool"
          },
          {
            "name": "lpEpochSettlementPaused",
            "type": "bool"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getLpRequestState",
    "inputs": [
      {
        "name": "isSenior",
        "type": "bool"
      },
      {
        "name": "requestId",
        "type": "uint256"
      },
      {
        "name": "controller",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "viewData",
        "type": "tuple",
        "components": [
          {
            "name": "vault",
            "type": "address"
          },
          {
            "name": "requestId",
            "type": "uint256"
          },
          {
            "name": "controller",
            "type": "address"
          },
          {
            "name": "pendingDepositAssets",
            "type": "uint256"
          },
          {
            "name": "pendingDepositSharesEstimate",
            "type": "uint256"
          },
          {
            "name": "claimableDepositAssets",
            "type": "uint256"
          },
          {
            "name": "claimableDepositShares",
            "type": "uint256"
          },
          {
            "name": "pendingRedeemShares",
            "type": "uint256"
          },
          {
            "name": "pendingRedeemAssetsEstimate",
            "type": "uint256"
          },
          {
            "name": "claimableRedeemShares",
            "type": "uint256"
          },
          {
            "name": "claimableRedeemAssets",
            "type": "uint256"
          },
          {
            "name": "refundableDepositAssets",
            "type": "uint256"
          },
          {
            "name": "refundableRedeemShares",
            "type": "uint256"
          },
          {
            "name": "redeemRefundPending",
            "type": "bool"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getLpDepositCooldownState",
    "inputs": [
      {
        "name": "isSenior",
        "type": "bool"
      },
      {
        "name": "requestId",
        "type": "uint256"
      },
      {
        "name": "controller",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "viewData",
        "type": "tuple",
        "components": [
          {
            "name": "vault",
            "type": "address"
          },
          {
            "name": "requestId",
            "type": "uint256"
          },
          {
            "name": "controller",
            "type": "address"
          },
          {
            "name": "activationTime",
            "type": "uint256"
          },
          {
            "name": "cooldownEnd",
            "type": "uint256"
          },
          {
            "name": "remainingClaimableShares",
            "type": "uint256"
          },
          {
            "name": "directRedeemableShares",
            "type": "uint256"
          }
        ]
      }
    ],
    "stateMutability": "view"
  }
] as const

export const PERPS_MARGIN_CLEARINGHOUSE_ABI = [
  {
    "type": "function",
    "name": "getOrderReservation",
    "inputs": [
      {
        "name": "orderId",
        "type": "uint64"
      }
    ],
    "outputs": [
      {
        "name": "reservation",
        "type": "tuple",
        "components": [
          {
            "name": "account",
            "type": "address"
          },
          {
            "name": "originalAmountUsdc",
            "type": "uint96"
          },
          {
            "name": "remainingAmountUsdc",
            "type": "uint96"
          },
          {
            "name": "previousOrderId",
            "type": "uint64"
          },
          {
            "name": "nextOrderId",
            "type": "uint64"
          },
          {
            "name": "bucket",
            "type": "uint8"
          },
          {
            "name": "status",
            "type": "uint8"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "vpiRebateReserveUsdc",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "depositMargin",
    "inputs": [
      {
        "name": "amount",
        "type": "uint256"
      }
    ],
    "outputs": [],
    "stateMutability": "nonpayable"
  },
  {
    "type": "function",
    "name": "withdrawMargin",
    "inputs": [
      {
        "name": "amount",
        "type": "uint256"
      }
    ],
    "outputs": [],
    "stateMutability": "nonpayable"
  },
  {
    "type": "function",
    "name": "getAccountEquityUsdc",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "totalEquityUsdc",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getFreeBuyingPowerUsdc",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "event",
    "name": "Deposit",
    "inputs": [
      {
        "name": "account",
        "type": "address",
        "indexed": true
      },
      {
        "name": "asset",
        "type": "address",
        "indexed": true
      },
      {
        "name": "amount",
        "type": "uint256",
        "indexed": false
      }
    ],
    "anonymous": false
  },
  {
    "type": "event",
    "name": "Withdraw",
    "inputs": [
      {
        "name": "account",
        "type": "address",
        "indexed": true
      },
      {
        "name": "asset",
        "type": "address",
        "indexed": true
      },
      {
        "name": "amount",
        "type": "uint256",
        "indexed": false
      }
    ],
    "anonymous": false
  }
] as const

export const PERPS_ORDER_ROUTER_ABI = [
  {
    "type": "function",
    "name": "commitOrder",
    "inputs": [
      {
        "name": "request",
        "type": "tuple",
        "components": [
          {
            "name": "clientOrderId",
            "type": "bytes32"
          },
          {
            "name": "side",
            "type": "uint8"
          },
          {
            "name": "sizeDelta",
            "type": "uint256"
          },
          {
            "name": "marginDelta",
            "type": "uint256"
          },
          {
            "name": "targetPrice",
            "type": "uint256"
          },
          {
            "name": "isClose",
            "type": "bool"
          },
          {
            "name": "bounds",
            "type": "tuple",
            "components": [
              {
                "name": "submitBy",
                "type": "uint64"
              },
              {
                "name": "executionWindowSeconds",
                "type": "uint32"
              },
              {
                "name": "allowedExecutionModes",
                "type": "uint8"
              },
              {
                "name": "expectedConfigHash",
                "type": "bytes32"
              },
              {
                "name": "maxExecutionBountyUsdc",
                "type": "uint256"
              },
              {
                "name": "maxExecutionNotionalUsdc",
                "type": "uint256"
              },
              {
                "name": "maxGrossAccountDebitUsdc",
                "type": "uint256"
              },
              {
                "name": "maxActionChargeUsdc",
                "type": "uint256"
              },
              {
                "name": "maxExplicitFeesUsdc",
                "type": "uint256"
              },
              {
                "name": "maxPostPositionSize",
                "type": "uint256"
              },
              {
                "name": "minPostSettlementBalanceUsdc",
                "type": "uint256"
              },
              {
                "name": "minPostPositionEquityUsdc",
                "type": "uint256"
              },
              {
                "name": "maxPostLeverageBps",
                "type": "uint32"
              }
            ]
          }
        ]
      }
    ],
    "outputs": [
      {
        "name": "orderId",
        "type": "uint64"
      }
    ],
    "stateMutability": "nonpayable"
  },
  {
    "type": "function",
    "name": "executeOrder",
    "inputs": [
      {
        "name": "orderId",
        "type": "uint64"
      },
      {
        "name": "pythUpdateData",
        "type": "bytes[]"
      }
    ],
    "outputs": [
      {
        "name": "result",
        "type": "tuple",
        "components": [
          {
            "name": "orderId",
            "type": "uint64"
          },
          {
            "name": "status",
            "type": "uint8"
          },
          {
            "name": "terminalReason",
            "type": "uint8"
          },
          {
            "name": "pendingReason",
            "type": "uint8"
          },
          {
            "name": "receiptHash",
            "type": "bytes32"
          }
        ]
      }
    ],
    "stateMutability": "payable"
  },
  {
    "type": "function",
    "name": "expireOrder",
    "inputs": [
      {
        "name": "orderId",
        "type": "uint64"
      }
    ],
    "outputs": [
      {
        "name": "",
        "type": "tuple",
        "components": [
          {
            "name": "orderId",
            "type": "uint64"
          },
          {
            "name": "status",
            "type": "uint8"
          },
          {
            "name": "terminalReason",
            "type": "uint8"
          },
          {
            "name": "pendingReason",
            "type": "uint8"
          },
          {
            "name": "receiptHash",
            "type": "bytes32"
          }
        ]
      }
    ],
    "stateMutability": "nonpayable"
  },
  {
    "type": "error",
    "name": "CfdEngine__NoPositionToLiquidate",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__AccountQueueCorrupt",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__BatchBeforeQueueHead",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__BatchOrderNotCommitted",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__CloseOnlyWindow",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__CloseWithPositiveMargin",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__CommitValidation",
    "inputs": [
      {
        "name": "code",
        "type": "uint8"
      }
    ]
  },
  {
    "type": "error",
    "name": "OrderRouter__ConditionalTriggerFrozen",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__DegradedMode",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__EmptyFeeds",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__EmptyPythUpdateData",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__ExecutionBountyAboveGrossDebit",
    "inputs": [
      {
        "name": "executionBountyUsdc",
        "type": "uint256"
      },
      {
        "name": "maximumGrossDebitUsdc",
        "type": "uint256"
      }
    ]
  },
  {
    "type": "error",
    "name": "OrderRouter__ExecutionConfigMismatch",
    "inputs": [
      {
        "name": "expectedConfigHash",
        "type": "bytes32"
      },
      {
        "name": "currentConfigHash",
        "type": "bytes32"
      }
    ]
  },
  {
    "type": "error",
    "name": "OrderRouter__GlobalQueueCorrupt",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InsufficientFreeEquity",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InsufficientGas",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InsufficientPythFee",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InvalidBasePrice",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InvalidEngineLens",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InvalidExecutionModeMask",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InvalidExecutionSidecar",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InvalidExecutionWindow",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InvalidKeeperSidecar",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InvalidLifecycleBook",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InvalidLiquidationBatchSize",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InvalidOraclePrice",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InvalidPletherOracle",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InvalidPolicyEvaluator",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InvalidProtectionPrices",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InvalidSizeQuantum",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InvalidSubmitBy",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InvalidWeights",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__LengthMismatch",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__LiquidationOraclePriceTooStale",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__MarginQueueCorrupt",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__MarkPriceOutOfOrder",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__MevDetected",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__MockOracleUnavailable",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__NoOpenPosition",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__NoOrdersToExecute",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__NoQueuedPosition",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__NotCloseOrder",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__NotInSeedLifecycle",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__OracleConfidenceTooWide",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__OraclePriceTooStale",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__OraclePublishTimesDiverged",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__OrderNotExpired",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__OrderNotPending",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__OrderNotQueueHead",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__OrderNotRiskOff",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__PendingOrdersExist",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__PositionChanged",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__PredictableOpenInvalid",
    "inputs": [
      {
        "name": "code",
        "type": "uint8"
      }
    ]
  },
  {
    "type": "error",
    "name": "OrderRouter__ProtectionActive",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__ProtectionAlreadyActive",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__ProtectionMarkTooStale",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__ProtectionNotArmed",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__ProtectionNotFound",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__ProtectionNotLatched",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__ProtectionTriggerAlreadyMet",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__SameBlockTrigger",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__SideMismatch",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__SizeExceedsQueued",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__TooManyPendingOrders",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__TriggerNotMet",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__Unauthorized",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__VaultRiskBlocked",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__ZeroClientOrderId",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__ZeroPostLeverageBound",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__ZeroSize",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__ZeroTargetPrice",
    "inputs": []
  },
  {
    "type": "error",
    "name": "ReentrancyGuardReentrantCall",
    "inputs": []
  },
  {
    "type": "function",
    "name": "getPendingOrderView",
    "inputs": [
      {
        "name": "orderId",
        "type": "uint64"
      }
    ],
    "outputs": [
      {
        "name": "pending",
        "type": "tuple",
        "components": [
          {
            "name": "orderId",
            "type": "uint64"
          },
          {
            "name": "isClose",
            "type": "bool"
          },
          {
            "name": "side",
            "type": "uint8"
          },
          {
            "name": "sizeDelta",
            "type": "uint256"
          },
          {
            "name": "marginDelta",
            "type": "uint256"
          },
          {
            "name": "targetPrice",
            "type": "uint256"
          },
          {
            "name": "commitTime",
            "type": "uint64"
          },
          {
            "name": "commitBlock",
            "type": "uint64"
          },
          {
            "name": "committedMarginUsdc",
            "type": "uint256"
          },
          {
            "name": "executionBountyUsdc",
            "type": "uint256"
          }
        ]
      },
      {
        "name": "nextAccountOrderId",
        "type": "uint64"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "maxExecutionWindowSeconds",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "maxPendingOrders",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "minOpenNotionalUsdc",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "openOrderExecutionBountyBps",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "minOpenOrderExecutionBountyUsdc",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "maxOpenOrderExecutionBountyUsdc",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "closeOrderExecutionBountyUsdc",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "engine",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "lifecycleBook",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "policyEvaluator",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "positionProtectionBook",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "executeOrder",
    "inputs": [
      {
        "name": "orderId",
        "type": "uint64"
      },
      {
        "name": "pythUpdateData",
        "type": "bytes[]"
      }
    ],
    "outputs": [
      {
        "name": "result",
        "type": "tuple",
        "components": [
          {
            "name": "orderId",
            "type": "uint64"
          },
          {
            "name": "status",
            "type": "uint8"
          },
          {
            "name": "terminalReason",
            "type": "uint8"
          },
          {
            "name": "pendingReason",
            "type": "uint8"
          },
          {
            "name": "receiptHash",
            "type": "bytes32"
          }
        ]
      }
    ],
    "stateMutability": "payable"
  },
  {
    "type": "function",
    "name": "executeOrderBatch",
    "inputs": [
      {
        "name": "maxOrderId",
        "type": "uint64"
      },
      {
        "name": "pythUpdateData",
        "type": "bytes[]"
      }
    ],
    "outputs": [
      {
        "name": "result",
        "type": "tuple",
        "components": [
          {
            "name": "nextOrderId",
            "type": "uint64"
          },
          {
            "name": "terminalCount",
            "type": "uint32"
          },
          {
            "name": "stopReason",
            "type": "uint8"
          }
        ]
      }
    ],
    "stateMutability": "payable"
  },
  {
    "type": "function",
    "name": "executeLiquidation",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      },
      {
        "name": "pythUpdateData",
        "type": "bytes[]"
      }
    ],
    "outputs": [],
    "stateMutability": "payable"
  },
  {
    "type": "function",
    "name": "updateMarkPrice",
    "inputs": [
      {
        "name": "pythUpdateData",
        "type": "bytes[]"
      }
    ],
    "outputs": [],
    "stateMutability": "payable"
  },
  {
    "type": "event",
    "name": "OrderCommitted",
    "inputs": [
      {
        "name": "orderId",
        "type": "uint64",
        "indexed": true
      },
      {
        "name": "account",
        "type": "address",
        "indexed": true
      },
      {
        "name": "side",
        "type": "uint8",
        "indexed": false
      }
    ],
    "anonymous": false
  },
  {
    "type": "error",
    "name": "OrderRouter__ZeroClientOrderId",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InvalidSubmitBy",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InvalidExecutionWindow",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__InvalidExecutionModeMask",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__ExecutionConfigMismatch",
    "inputs": [
      {
        "name": "expectedConfigHash",
        "type": "bytes32"
      },
      {
        "name": "currentConfigHash",
        "type": "bytes32"
      }
    ]
  },
  {
    "type": "error",
    "name": "OrderRouter__ProtectionActive",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__ZeroPostLeverageBound",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderRouter__ExecutionBountyAboveGrossDebit",
    "inputs": [
      {
        "name": "executionBountyUsdc",
        "type": "uint256"
      },
      {
        "name": "maximumGrossDebitUsdc",
        "type": "uint256"
      }
    ]
  }
] as const

export const PERPS_ORDER_LIFECYCLE_BOOK_ABI = [
  {
    "type": "function",
    "name": "INTENT_TYPEHASH",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "bytes32"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "ROUTER",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "ENGINE",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "CLEARINGHOUSE",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "HOUSE_POOL",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "currentExecutionConfigHash",
    "inputs": [],
    "outputs": [
      {
        "name": "configHash",
        "type": "bytes32"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "resolveClientIntent",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      },
      {
        "name": "request",
        "type": "tuple",
        "components": [
          {
            "name": "clientOrderId",
            "type": "bytes32"
          },
          {
            "name": "side",
            "type": "uint8"
          },
          {
            "name": "sizeDelta",
            "type": "uint256"
          },
          {
            "name": "marginDelta",
            "type": "uint256"
          },
          {
            "name": "targetPrice",
            "type": "uint256"
          },
          {
            "name": "isClose",
            "type": "bool"
          },
          {
            "name": "bounds",
            "type": "tuple",
            "components": [
              {
                "name": "submitBy",
                "type": "uint64"
              },
              {
                "name": "executionWindowSeconds",
                "type": "uint32"
              },
              {
                "name": "allowedExecutionModes",
                "type": "uint8"
              },
              {
                "name": "expectedConfigHash",
                "type": "bytes32"
              },
              {
                "name": "maxExecutionBountyUsdc",
                "type": "uint256"
              },
              {
                "name": "maxExecutionNotionalUsdc",
                "type": "uint256"
              },
              {
                "name": "maxGrossAccountDebitUsdc",
                "type": "uint256"
              },
              {
                "name": "maxActionChargeUsdc",
                "type": "uint256"
              },
              {
                "name": "maxExplicitFeesUsdc",
                "type": "uint256"
              },
              {
                "name": "maxPostPositionSize",
                "type": "uint256"
              },
              {
                "name": "minPostSettlementBalanceUsdc",
                "type": "uint256"
              },
              {
                "name": "minPostPositionEquityUsdc",
                "type": "uint256"
              },
              {
                "name": "maxPostLeverageBps",
                "type": "uint32"
              }
            ]
          }
        ]
      }
    ],
    "outputs": [
      {
        "name": "resolution",
        "type": "uint8"
      },
      {
        "name": "orderId",
        "type": "uint64"
      },
      {
        "name": "intentHash",
        "type": "bytes32"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "clientIntent",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      },
      {
        "name": "clientOrderId",
        "type": "bytes32"
      }
    ],
    "outputs": [
      {
        "name": "intent",
        "type": "tuple",
        "components": [
          {
            "name": "orderId",
            "type": "uint64"
          },
          {
            "name": "intentHash",
            "type": "bytes32"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "orderTiming",
    "inputs": [
      {
        "name": "orderId",
        "type": "uint64"
      }
    ],
    "outputs": [
      {
        "name": "",
        "type": "tuple",
        "components": [
          {
            "name": "submitBy",
            "type": "uint64"
          },
          {
            "name": "executionWindowSeconds",
            "type": "uint32"
          },
          {
            "name": "commitTimestamp",
            "type": "uint64"
          },
          {
            "name": "executionDeadline",
            "type": "uint64"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "pendingPolicy",
    "inputs": [
      {
        "name": "orderId",
        "type": "uint64"
      }
    ],
    "outputs": [
      {
        "name": "bounds",
        "type": "tuple",
        "components": [
          {
            "name": "submitBy",
            "type": "uint64"
          },
          {
            "name": "executionWindowSeconds",
            "type": "uint32"
          },
          {
            "name": "allowedExecutionModes",
            "type": "uint8"
          },
          {
            "name": "expectedConfigHash",
            "type": "bytes32"
          },
          {
            "name": "maxExecutionBountyUsdc",
            "type": "uint256"
          },
          {
            "name": "maxExecutionNotionalUsdc",
            "type": "uint256"
          },
          {
            "name": "maxGrossAccountDebitUsdc",
            "type": "uint256"
          },
          {
            "name": "maxActionChargeUsdc",
            "type": "uint256"
          },
          {
            "name": "maxExplicitFeesUsdc",
            "type": "uint256"
          },
          {
            "name": "maxPostPositionSize",
            "type": "uint256"
          },
          {
            "name": "minPostSettlementBalanceUsdc",
            "type": "uint256"
          },
          {
            "name": "minPostPositionEquityUsdc",
            "type": "uint256"
          },
          {
            "name": "maxPostLeverageBps",
            "type": "uint32"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "lifecycleStatus",
    "inputs": [
      {
        "name": "orderId",
        "type": "uint64"
      }
    ],
    "outputs": [
      {
        "name": "status",
        "type": "uint8"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "event",
    "name": "IntentRegistered",
    "inputs": [
      {
        "name": "orderId",
        "type": "uint64",
        "indexed": true
      },
      {
        "name": "account",
        "type": "address",
        "indexed": true
      },
      {
        "name": "clientOrderId",
        "type": "bytes32",
        "indexed": true
      },
      {
        "name": "intentHash",
        "type": "bytes32",
        "indexed": false
      },
      {
        "name": "executionBountyUsdc",
        "type": "uint256",
        "indexed": false
      },
      {
        "name": "request",
        "type": "tuple",
        "indexed": false,
        "components": [
          {
            "name": "clientOrderId",
            "type": "bytes32"
          },
          {
            "name": "side",
            "type": "uint8"
          },
          {
            "name": "sizeDelta",
            "type": "uint256"
          },
          {
            "name": "marginDelta",
            "type": "uint256"
          },
          {
            "name": "targetPrice",
            "type": "uint256"
          },
          {
            "name": "isClose",
            "type": "bool"
          },
          {
            "name": "bounds",
            "type": "tuple",
            "components": [
              {
                "name": "submitBy",
                "type": "uint64"
              },
              {
                "name": "executionWindowSeconds",
                "type": "uint32"
              },
              {
                "name": "allowedExecutionModes",
                "type": "uint8"
              },
              {
                "name": "expectedConfigHash",
                "type": "bytes32"
              },
              {
                "name": "maxExecutionBountyUsdc",
                "type": "uint256"
              },
              {
                "name": "maxExecutionNotionalUsdc",
                "type": "uint256"
              },
              {
                "name": "maxGrossAccountDebitUsdc",
                "type": "uint256"
              },
              {
                "name": "maxActionChargeUsdc",
                "type": "uint256"
              },
              {
                "name": "maxExplicitFeesUsdc",
                "type": "uint256"
              },
              {
                "name": "maxPostPositionSize",
                "type": "uint256"
              },
              {
                "name": "minPostSettlementBalanceUsdc",
                "type": "uint256"
              },
              {
                "name": "minPostPositionEquityUsdc",
                "type": "uint256"
              },
              {
                "name": "maxPostLeverageBps",
                "type": "uint32"
              }
            ]
          }
        ]
      },
      {
        "name": "timing",
        "type": "tuple",
        "indexed": false,
        "components": [
          {
            "name": "submitBy",
            "type": "uint64"
          },
          {
            "name": "executionWindowSeconds",
            "type": "uint32"
          },
          {
            "name": "commitTimestamp",
            "type": "uint64"
          },
          {
            "name": "executionDeadline",
            "type": "uint64"
          }
        ]
      }
    ],
    "anonymous": false
  },
  {
    "type": "error",
    "name": "OrderLifecycleBook__ZeroClientOrderId",
    "inputs": []
  },
  {
    "type": "error",
    "name": "OrderLifecycleBook__ClientIdConflict",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      },
      {
        "name": "clientOrderId",
        "type": "bytes32"
      },
      {
        "name": "existingIntentHash",
        "type": "bytes32"
      },
      {
        "name": "suppliedIntentHash",
        "type": "bytes32"
      }
    ]
  },
  {
    "type": "error",
    "name": "OrderLifecycleBook__ClientIdDomainMismatch",
    "inputs": [
      {
        "name": "clientOrderId",
        "type": "bytes32"
      },
      {
        "name": "protocolIntent",
        "type": "bool"
      }
    ]
  },
  {
    "type": "error",
    "name": "OrderLifecycleBook__ExecutionBountyAboveBound",
    "inputs": [
      {
        "name": "actualBountyUsdc",
        "type": "uint256"
      },
      {
        "name": "maximumBountyUsdc",
        "type": "uint256"
      }
    ]
  },
  {
    "type": "function",
    "name": "terminalOutcome",
    "inputs": [
      {
        "name": "orderId",
        "type": "uint64"
      }
    ],
    "outputs": [
      {
        "name": "",
        "type": "tuple",
        "components": [
          {
            "name": "account",
            "type": "address"
          },
          {
            "name": "terminalBlock",
            "type": "uint64"
          },
          {
            "name": "status",
            "type": "uint8"
          },
          {
            "name": "reason",
            "type": "uint8"
          },
          {
            "name": "receiptHash",
            "type": "bytes32"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "event",
    "name": "OrderFinalized",
    "inputs": [
      {
        "name": "orderId",
        "type": "uint64",
        "indexed": true
      },
      {
        "name": "account",
        "type": "address",
        "indexed": true
      },
      {
        "name": "clientOrderId",
        "type": "bytes32",
        "indexed": true
      },
      {
        "name": "receiptHash",
        "type": "bytes32",
        "indexed": false
      },
      {
        "name": "terminalBlock",
        "type": "uint64",
        "indexed": false
      },
      {
        "name": "terminalTime",
        "type": "uint64",
        "indexed": false
      },
      {
        "name": "receipt",
        "type": "tuple",
        "indexed": false,
        "components": [
          {
            "name": "orderId",
            "type": "uint64"
          },
          {
            "name": "account",
            "type": "address"
          },
          {
            "name": "clientOrderId",
            "type": "bytes32"
          },
          {
            "name": "intentHash",
            "type": "bytes32"
          },
          {
            "name": "expectedConfigHash",
            "type": "bytes32"
          },
          {
            "name": "observedConfigHash",
            "type": "bytes32"
          },
          {
            "name": "status",
            "type": "uint8"
          },
          {
            "name": "reason",
            "type": "uint8"
          },
          {
            "name": "executionMode",
            "type": "uint8"
          },
          {
            "name": "executor",
            "type": "address"
          },
          {
            "name": "priceSource",
            "type": "uint8"
          },
          {
            "name": "executionPrice",
            "type": "uint256"
          },
          {
            "name": "neutralMarkPrice",
            "type": "uint256"
          },
          {
            "name": "poolDepthUsdc",
            "type": "uint256"
          },
          {
            "name": "oraclePublishTime",
            "type": "uint64"
          },
          {
            "name": "priceReachedEngine",
            "type": "bool"
          },
          {
            "name": "bountyUsdc",
            "type": "uint256"
          },
          {
            "name": "bountyRecipient",
            "type": "address"
          },
          {
            "name": "bountyDisposition",
            "type": "uint8"
          },
          {
            "name": "failure",
            "type": "tuple",
            "components": [
              {
                "name": "selector",
                "type": "bytes4"
              },
              {
                "name": "category",
                "type": "uint8"
              },
              {
                "name": "code",
                "type": "uint8"
              },
              {
                "name": "constraint",
                "type": "uint8"
              },
              {
                "name": "actual",
                "type": "uint256"
              },
              {
                "name": "limit",
                "type": "uint256"
              },
              {
                "name": "revertDataHash",
                "type": "bytes32"
              }
            ]
          },
          {
            "name": "economics",
            "type": "tuple",
            "components": [
              {
                "name": "executionNotionalUsdc",
                "type": "uint256"
              },
              {
                "name": "realizedPnlUsdc",
                "type": "int256"
              },
              {
                "name": "vpiUsdc",
                "type": "int256"
              },
              {
                "name": "carryUsdc",
                "type": "int256"
              },
              {
                "name": "executionFeeUsdc",
                "type": "uint256"
              },
              {
                "name": "frozenSpreadUsdc",
                "type": "uint256"
              },
              {
                "name": "actionChargeAssessedUsdc",
                "type": "uint256"
              },
              {
                "name": "actionChargeCollectedUsdc",
                "type": "uint256"
              },
              {
                "name": "grossAccountDebitUsdc",
                "type": "uint256"
              },
              {
                "name": "preSettlementBalanceUsdc",
                "type": "uint256"
              },
              {
                "name": "postSettlementBalanceUsdc",
                "type": "uint256"
              },
              {
                "name": "preTraderClaimBalanceUsdc",
                "type": "uint256"
              },
              {
                "name": "postTraderClaimBalanceUsdc",
                "type": "uint256"
              },
              {
                "name": "postPositionSize",
                "type": "uint256"
              },
              {
                "name": "postPositionMarginUsdc",
                "type": "uint256"
              },
              {
                "name": "postPositionEquityUsdc",
                "type": "int256"
              },
              {
                "name": "postLeverageBps",
                "type": "uint256"
              },
              {
                "name": "close",
                "type": "tuple",
                "components": [
                  {
                    "name": "postFreeSettlementUsdc",
                    "type": "uint256"
                  },
                  {
                    "name": "safeMarginReleaseUsdc",
                    "type": "uint256"
                  },
                  {
                    "name": "actionChargeFromReleasedMarginUsdc",
                    "type": "uint256"
                  },
                  {
                    "name": "netReleasedMarginUsdc",
                    "type": "uint256"
                  },
                  {
                    "name": "priceLossUsdc",
                    "type": "uint256"
                  },
                  {
                    "name": "pricePnlClaimConsumedUsdc",
                    "type": "uint256"
                  },
                  {
                    "name": "pricePnlPledgeConsumedUsdc",
                    "type": "uint256"
                  },
                  {
                    "name": "priceLossWrittenOffUsdc",
                    "type": "uint256"
                  },
                  {
                    "name": "actionChargeWithheldUsdc",
                    "type": "uint256"
                  },
                  {
                    "name": "actionChargeWaivedUsdc",
                    "type": "uint256"
                  }
                ]
              }
            ]
          },
          {
            "name": "commitment",
            "type": "tuple",
            "components": [
              {
                "name": "carryCollectedUsdc",
                "type": "uint256"
              },
              {
                "name": "carryOutstandingUsdc",
                "type": "uint256"
              },
              {
                "name": "bountyFromFreeUsdc",
                "type": "uint256"
              },
              {
                "name": "bountyFromPledgeUsdc",
                "type": "uint256"
              },
              {
                "name": "settlementBeforeUsdc",
                "type": "uint256"
              },
              {
                "name": "settlementAfterUsdc",
                "type": "uint256"
              },
              {
                "name": "freeSettlementAfterUsdc",
                "type": "uint256"
              }
            ]
          },
          {
            "name": "bounty",
            "type": "tuple",
            "components": [
              {
                "name": "bountyEntitlementUsdc",
                "type": "uint256"
              },
              {
                "name": "bountyPaidUsdc",
                "type": "uint256"
              },
              {
                "name": "bountyRefundedUsdc",
                "type": "uint256"
              },
              {
                "name": "bountyRetainedUsdc",
                "type": "uint256"
              },
              {
                "name": "bountyForfeitedUsdc",
                "type": "uint256"
              },
              {
                "name": "discrepancy",
                "type": "uint8"
              }
            ]
          },
          {
            "name": "timing",
            "type": "tuple",
            "components": [
              {
                "name": "submitBy",
                "type": "uint64"
              },
              {
                "name": "executionWindowSeconds",
                "type": "uint32"
              },
              {
                "name": "commitTimestamp",
                "type": "uint64"
              },
              {
                "name": "executionDeadline",
                "type": "uint64"
              }
            ]
          }
        ]
      }
    ],
    "anonymous": false
  }
] as const

export const PERPS_ORDER_POLICY_EVALUATOR_ABI = [
  {
    "type": "function",
    "name": "assessOrder",
    "inputs": [
      {
        "name": "engineAddress",
        "type": "address"
      },
      {
        "name": "order",
        "type": "tuple",
        "components": [
          {
            "name": "account",
            "type": "address"
          },
          {
            "name": "sizeDelta",
            "type": "uint256"
          },
          {
            "name": "marginDelta",
            "type": "uint256"
          },
          {
            "name": "targetPrice",
            "type": "uint256"
          },
          {
            "name": "commitTime",
            "type": "uint64"
          },
          {
            "name": "commitBlock",
            "type": "uint64"
          },
          {
            "name": "orderId",
            "type": "uint64"
          },
          {
            "name": "side",
            "type": "uint8"
          },
          {
            "name": "isClose",
            "type": "bool"
          }
        ]
      },
      {
        "name": "executor",
        "type": "address"
      },
      {
        "name": "currentOraclePrice",
        "type": "uint256"
      },
      {
        "name": "poolDepthUsdc",
        "type": "uint256"
      },
      {
        "name": "publishTime",
        "type": "uint64"
      },
      {
        "name": "bounds",
        "type": "tuple",
        "components": [
          {
            "name": "submitBy",
            "type": "uint64"
          },
          {
            "name": "executionWindowSeconds",
            "type": "uint32"
          },
          {
            "name": "allowedExecutionModes",
            "type": "uint8"
          },
          {
            "name": "expectedConfigHash",
            "type": "bytes32"
          },
          {
            "name": "maxExecutionBountyUsdc",
            "type": "uint256"
          },
          {
            "name": "maxExecutionNotionalUsdc",
            "type": "uint256"
          },
          {
            "name": "maxGrossAccountDebitUsdc",
            "type": "uint256"
          },
          {
            "name": "maxActionChargeUsdc",
            "type": "uint256"
          },
          {
            "name": "maxExplicitFeesUsdc",
            "type": "uint256"
          },
          {
            "name": "maxPostPositionSize",
            "type": "uint256"
          },
          {
            "name": "minPostSettlementBalanceUsdc",
            "type": "uint256"
          },
          {
            "name": "minPostPositionEquityUsdc",
            "type": "uint256"
          },
          {
            "name": "maxPostLeverageBps",
            "type": "uint32"
          }
        ]
      },
      {
        "name": "executionBountyUsdc",
        "type": "uint256"
      }
    ],
    "outputs": [
      {
        "name": "assessment",
        "type": "tuple",
        "components": [
          {
            "name": "mode",
            "type": "uint8"
          },
          {
            "name": "executionNotionalUsdc",
            "type": "uint256"
          },
          {
            "name": "grossAccountDebitUsdc",
            "type": "uint256"
          },
          {
            "name": "actionChargeAssessedUsdc",
            "type": "uint256"
          },
          {
            "name": "actionChargeCollectedUsdc",
            "type": "uint256"
          },
          {
            "name": "explicitFeesUsdc",
            "type": "uint256"
          },
          {
            "name": "preSettlementBalanceUsdc",
            "type": "uint256"
          },
          {
            "name": "postSettlementBalanceUsdc",
            "type": "uint256"
          },
          {
            "name": "realizedPnlUsdc",
            "type": "int256"
          },
          {
            "name": "vpiUsdc",
            "type": "int256"
          },
          {
            "name": "carryUsdc",
            "type": "uint256"
          },
          {
            "name": "executionFeeUsdc",
            "type": "uint256"
          },
          {
            "name": "frozenSpreadUsdc",
            "type": "uint256"
          },
          {
            "name": "preTraderClaimUsdc",
            "type": "uint256"
          },
          {
            "name": "postTraderClaimUsdc",
            "type": "uint256"
          },
          {
            "name": "postPositionSize",
            "type": "uint256"
          },
          {
            "name": "postPositionMarginUsdc",
            "type": "uint256"
          },
          {
            "name": "postPositionEquityUsdc",
            "type": "int256"
          },
          {
            "name": "postLeverageBps",
            "type": "uint256"
          },
          {
            "name": "close",
            "type": "tuple",
            "components": [
              {
                "name": "postFreeSettlementUsdc",
                "type": "uint256"
              },
              {
                "name": "safeMarginReleaseUsdc",
                "type": "uint256"
              },
              {
                "name": "actionChargeFromReleasedMarginUsdc",
                "type": "uint256"
              },
              {
                "name": "netReleasedMarginUsdc",
                "type": "uint256"
              },
              {
                "name": "priceLossUsdc",
                "type": "uint256"
              },
              {
                "name": "pricePnlClaimConsumedUsdc",
                "type": "uint256"
              },
              {
                "name": "pricePnlPledgeConsumedUsdc",
                "type": "uint256"
              },
              {
                "name": "priceLossWrittenOffUsdc",
                "type": "uint256"
              },
              {
                "name": "actionChargeWithheldUsdc",
                "type": "uint256"
              },
              {
                "name": "actionChargeWaivedUsdc",
                "type": "uint256"
              }
            ]
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "error",
    "name": "CfdOrderPolicyEvaluator__ExecutionModeDisallowed",
    "inputs": [
      {
        "name": "mode",
        "type": "uint8"
      },
      {
        "name": "allowedExecutionModes",
        "type": "uint8"
      }
    ]
  },
  {
    "type": "error",
    "name": "CfdOrderPolicyEvaluator__ConstraintViolation",
    "inputs": [
      {
        "name": "constraint",
        "type": "uint8"
      },
      {
        "name": "actual",
        "type": "uint256"
      },
      {
        "name": "limit",
        "type": "uint256"
      }
    ]
  }
] as const

export const PERPS_CFD_ENGINE_ABI = [
  {
    "type": "function",
    "name": "positionEntryCostUsdcAtoms",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "positionCarryState",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "borrowBaseUsdc",
        "type": "uint256"
      },
      {
        "name": "lastCarryIndex",
        "type": "uint256"
      },
      {
        "name": "lastCarryTimestamp",
        "type": "uint64"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "unsettledCarryUsdc",
    "inputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "sideCarryIndex",
    "inputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "sideCarryTimestamp",
    "inputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "outputs": [
      {
        "name": "",
        "type": "uint64"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "sideBorrowBaseUsdc",
    "inputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "event",
    "name": "PositionOpened",
    "inputs": [
      {
        "name": "account",
        "type": "address",
        "indexed": true
      },
      {
        "name": "side",
        "type": "uint8",
        "indexed": false
      },
      {
        "name": "sizeDelta",
        "type": "uint256",
        "indexed": false
      },
      {
        "name": "price",
        "type": "uint256",
        "indexed": false
      },
      {
        "name": "marginDelta",
        "type": "uint256",
        "indexed": false
      }
    ],
    "anonymous": false
  },
  {
    "type": "event",
    "name": "PositionClosed",
    "inputs": [
      {
        "name": "account",
        "type": "address",
        "indexed": true
      },
      {
        "name": "side",
        "type": "uint8",
        "indexed": false
      },
      {
        "name": "sizeDelta",
        "type": "uint256",
        "indexed": false
      },
      {
        "name": "price",
        "type": "uint256",
        "indexed": false
      },
      {
        "name": "pnl",
        "type": "int256",
        "indexed": false
      }
    ],
    "anonymous": false
  },
  {
    "type": "event",
    "name": "PositionLiquidated",
    "inputs": [
      {
        "name": "account",
        "type": "address",
        "indexed": true
      },
      {
        "name": "side",
        "type": "uint8",
        "indexed": false
      },
      {
        "name": "size",
        "type": "uint256",
        "indexed": false
      },
      {
        "name": "price",
        "type": "uint256",
        "indexed": false
      },
      {
        "name": "keeperBounty",
        "type": "uint256",
        "indexed": false
      }
    ],
    "anonymous": false
  },
  {
    "type": "event",
    "name": "MarginAdded",
    "inputs": [
      {
        "name": "account",
        "type": "address",
        "indexed": true
      },
      {
        "name": "amount",
        "type": "uint256",
        "indexed": false
      }
    ],
    "anonymous": false
  },
  {
    "type": "function",
    "name": "sides",
    "inputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "outputs": [
      {
        "name": "maxProfitUsdc",
        "type": "uint256"
      },
      {
        "name": "openInterest",
        "type": "uint256"
      },
      {
        "name": "entryNotional",
        "type": "uint256"
      },
      {
        "name": "totalMargin",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "riskParams",
    "inputs": [],
    "outputs": [
      {
        "name": "vpiFactor",
        "type": "uint256"
      },
      {
        "name": "maxSkewRatio",
        "type": "uint256"
      },
      {
        "name": "maintMarginBps",
        "type": "uint256"
      },
      {
        "name": "initMarginBps",
        "type": "uint256"
      },
      {
        "name": "fadMarginBps",
        "type": "uint256"
      },
      {
        "name": "baseCarryBps",
        "type": "uint256"
      },
      {
        "name": "minBountyUsdc",
        "type": "uint256"
      },
      {
        "name": "bountyBps",
        "type": "uint256"
      },
      {
        "name": "keeperShareBps",
        "type": "uint256"
      },
      {
        "name": "protocolShareBps",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "CAP_PRICE",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "lastMarkPrice",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "clearinghouse",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "pool",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "isFadWindow",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "positions",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "size",
        "type": "uint256"
      },
      {
        "name": "margin",
        "type": "uint256"
      },
      {
        "name": "entryPrice",
        "type": "uint256"
      },
      {
        "name": "maxProfitUsdc",
        "type": "uint256"
      },
      {
        "name": "side",
        "type": "uint8"
      },
      {
        "name": "lastUpdateTime",
        "type": "uint64"
      },
      {
        "name": "vpiAccrued",
        "type": "int256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "executionFeeBps",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "settleTraderClaim",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      }
    ],
    "outputs": [],
    "stateMutability": "nonpayable"
  },
  {
    "type": "function",
    "name": "addMargin",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      },
      {
        "name": "amount",
        "type": "uint256"
      }
    ],
    "outputs": [],
    "stateMutability": "nonpayable"
  }
] as const

export const PERPS_HOUSE_POOL_ABI = [
  {
    "type": "function",
    "name": "totalAssets",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getPoolLiquidityView",
    "inputs": [],
    "outputs": [
      {
        "name": "viewData",
        "type": "tuple",
        "components": [
          {
            "name": "totalAssetsUsdc",
            "type": "uint256"
          },
          {
            "name": "freeUsdc",
            "type": "uint256"
          },
          {
            "name": "withdrawalReservedUsdc",
            "type": "uint256"
          },
          {
            "name": "pendingRecapitalizationUsdc",
            "type": "uint256"
          },
          {
            "name": "pendingTradingRevenueUsdc",
            "type": "uint256"
          },
          {
            "name": "seniorPrincipalUsdc",
            "type": "uint256"
          },
          {
            "name": "juniorPrincipalUsdc",
            "type": "uint256"
          },
          {
            "name": "seniorHighWaterMarkUsdc",
            "type": "uint256"
          },
          {
            "name": "currentTerminalDeficitUsdc",
            "type": "uint256"
          },
          {
            "name": "markFresh",
            "type": "bool"
          },
          {
            "name": "oracleFrozen",
            "type": "bool"
          },
          {
            "name": "degradedMode",
            "type": "bool"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getPendingTrancheState",
    "inputs": [],
    "outputs": [
      {
        "name": "seniorPrincipalUsdc",
        "type": "uint256"
      },
      {
        "name": "juniorPrincipalUsdc",
        "type": "uint256"
      },
      {
        "name": "maxSeniorWithdrawUsdc",
        "type": "uint256"
      },
      {
        "name": "maxJuniorWithdrawUsdc",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "maxSeniorExposureUsdc",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "maxSeniorShareBps",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "seniorRateBps",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getSeniorDepositCapacity",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "reservedSeniorDepositAssetsUsdc",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "areSeniorDepositReservationsWithinLimits",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "bool"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "minTrancheDepositUsdc",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "pure"
  }
] as const

export const PERPS_PLETHER_ORACLE_ABI = [
  {
    "type": "error",
    "name": "PletherOracle__PriceOutOfOrder",
    "inputs": [
      {
        "name": "publishTime",
        "type": "uint64"
      },
      {
        "name": "lastMarkTime",
        "type": "uint64"
      }
    ]
  },
  {
    "type": "function",
    "name": "getUpdateFee",
    "inputs": [
      {
        "name": "pythUpdateData",
        "type": "bytes[]"
      }
    ],
    "outputs": [
      {
        "name": "pythFee",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "orderSettlementWindow",
    "inputs": [],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getLatestPrice",
    "inputs": [],
    "outputs": [
      {
        "name": "latestPrice",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getLatestPrice",
    "inputs": [
      {
        "name": "mode",
        "type": "uint8"
      }
    ],
    "outputs": [
      {
        "name": "snapshot",
        "type": "tuple",
        "components": [
          {
            "name": "price",
            "type": "uint256"
          },
          {
            "name": "markPrice",
            "type": "uint256"
          },
          {
            "name": "publishTime",
            "type": "uint64"
          },
          {
            "name": "updateFee",
            "type": "uint256"
          },
          {
            "name": "maxStaleness",
            "type": "uint256"
          },
          {
            "name": "closeOnly",
            "type": "bool"
          },
          {
            "name": "oracleFrozen",
            "type": "bool"
          },
          {
            "name": "isFadWindow",
            "type": "bool"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "claimableEth",
    "inputs": [
      {
        "name": "",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "claimEthRefund",
    "inputs": [],
    "outputs": [],
    "stateMutability": "nonpayable"
  },
  {
    "type": "function",
    "name": "getOrderExecutionPolicy",
    "inputs": [
      {
        "name": "isClose",
        "type": "bool"
      }
    ],
    "outputs": [
      {
        "name": "policy",
        "type": "tuple",
        "components": [
          {
            "name": "closeOnly",
            "type": "bool"
          },
          {
            "name": "requireStoredMark",
            "type": "bool"
          },
          {
            "name": "allowAnyStoredMark",
            "type": "bool"
          },
          {
            "name": "maxStaleness",
            "type": "uint256"
          },
          {
            "name": "oracleFrozen",
            "type": "bool"
          },
          {
            "name": "isFadWindow",
            "type": "bool"
          }
        ]
      }
    ],
    "stateMutability": "view"
  }
] as const

export const PERPS_CFD_ENGINE_LENS_ABI = [
  {
    "type": "function",
    "name": "quoteMaxOpen",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      },
      {
        "name": "side",
        "type": "uint8"
      },
      {
        "name": "marginDelta",
        "type": "uint256"
      },
      {
        "name": "oraclePrice",
        "type": "uint256"
      },
      {
        "name": "publishTime",
        "type": "uint64"
      }
    ],
    "outputs": [
      {
        "name": "quote",
        "type": "tuple",
        "components": [
          {
            "name": "maxSizeDelta",
            "type": "uint256"
          },
          {
            "name": "preview",
            "type": "tuple",
            "components": [
              {
                "name": "valid",
                "type": "bool"
              },
              {
                "name": "invalidReason",
                "type": "uint8"
              },
              {
                "name": "failureCategory",
                "type": "uint8"
              },
              {
                "name": "executionPrice",
                "type": "uint256"
              },
              {
                "name": "sizeDelta",
                "type": "uint256"
              },
              {
                "name": "notionalUsdc",
                "type": "uint256"
              },
              {
                "name": "marginDeltaUsdc",
                "type": "uint256"
              },
              {
                "name": "vpiUsdc",
                "type": "int256"
              },
              {
                "name": "executionFeeUsdc",
                "type": "uint256"
              },
              {
                "name": "tradeCostUsdc",
                "type": "int256"
              },
              {
                "name": "poolRebatePayoutUsdc",
                "type": "uint256"
              },
              {
                "name": "pendingCarryUsdc",
                "type": "uint256"
              },
              {
                "name": "initialMarginRequirementUsdc",
                "type": "uint256"
              },
              {
                "name": "maintenanceMarginUsdc",
                "type": "uint256"
              },
              {
                "name": "postSize",
                "type": "uint256"
              },
              {
                "name": "postMarginUsdc",
                "type": "uint256"
              },
              {
                "name": "postEntryPrice",
                "type": "uint256"
              },
              {
                "name": "postVpiAccrued",
                "type": "int256"
              },
              {
                "name": "postUnrealizedPnlUsdc",
                "type": "int256"
              },
              {
                "name": "postEquityUsdc",
                "type": "int256"
              },
              {
                "name": "postHealthBps",
                "type": "uint256"
              },
              {
                "name": "postLiquidatable",
                "type": "bool"
              },
              {
                "name": "hasLiquidationPrice",
                "type": "bool"
              },
              {
                "name": "liquidationPrice",
                "type": "uint256"
              }
            ]
          },
          {
            "name": "limitingReason",
            "type": "uint8"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "previewOpen",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      },
      {
        "name": "side",
        "type": "uint8"
      },
      {
        "name": "sizeDelta",
        "type": "uint256"
      },
      {
        "name": "marginDelta",
        "type": "uint256"
      },
      {
        "name": "oraclePrice",
        "type": "uint256"
      },
      {
        "name": "publishTime",
        "type": "uint64"
      }
    ],
    "outputs": [
      {
        "name": "preview",
        "type": "tuple",
        "components": [
          {
            "name": "valid",
            "type": "bool"
          },
          {
            "name": "invalidReason",
            "type": "uint8"
          },
          {
            "name": "failureCategory",
            "type": "uint8"
          },
          {
            "name": "executionPrice",
            "type": "uint256"
          },
          {
            "name": "sizeDelta",
            "type": "uint256"
          },
          {
            "name": "notionalUsdc",
            "type": "uint256"
          },
          {
            "name": "marginDeltaUsdc",
            "type": "uint256"
          },
          {
            "name": "vpiUsdc",
            "type": "int256"
          },
          {
            "name": "executionFeeUsdc",
            "type": "uint256"
          },
          {
            "name": "tradeCostUsdc",
            "type": "int256"
          },
          {
            "name": "poolRebatePayoutUsdc",
            "type": "uint256"
          },
          {
            "name": "pendingCarryUsdc",
            "type": "uint256"
          },
          {
            "name": "initialMarginRequirementUsdc",
            "type": "uint256"
          },
          {
            "name": "maintenanceMarginUsdc",
            "type": "uint256"
          },
          {
            "name": "postSize",
            "type": "uint256"
          },
          {
            "name": "postMarginUsdc",
            "type": "uint256"
          },
          {
            "name": "postEntryPrice",
            "type": "uint256"
          },
          {
            "name": "postVpiAccrued",
            "type": "int256"
          },
          {
            "name": "postUnrealizedPnlUsdc",
            "type": "int256"
          },
          {
            "name": "postEquityUsdc",
            "type": "int256"
          },
          {
            "name": "postHealthBps",
            "type": "uint256"
          },
          {
            "name": "postLiquidatable",
            "type": "bool"
          },
          {
            "name": "hasLiquidationPrice",
            "type": "bool"
          },
          {
            "name": "liquidationPrice",
            "type": "uint256"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "previewOpenRevertCode",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      },
      {
        "name": "side",
        "type": "uint8"
      },
      {
        "name": "sizeDelta",
        "type": "uint256"
      },
      {
        "name": "marginDelta",
        "type": "uint256"
      },
      {
        "name": "oraclePrice",
        "type": "uint256"
      },
      {
        "name": "publishTime",
        "type": "uint64"
      }
    ],
    "outputs": [
      {
        "name": "code",
        "type": "uint8"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "previewOpenFailurePolicyCategory",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      },
      {
        "name": "side",
        "type": "uint8"
      },
      {
        "name": "sizeDelta",
        "type": "uint256"
      },
      {
        "name": "marginDelta",
        "type": "uint256"
      },
      {
        "name": "oraclePrice",
        "type": "uint256"
      },
      {
        "name": "publishTime",
        "type": "uint64"
      }
    ],
    "outputs": [
      {
        "name": "category",
        "type": "uint8"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "previewClose",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      },
      {
        "name": "sizeDelta",
        "type": "uint256"
      },
      {
        "name": "oraclePrice",
        "type": "uint256"
      }
    ],
    "outputs": [
      {
        "name": "preview",
        "type": "tuple",
        "components": [
          {
            "name": "valid",
            "type": "bool"
          },
          {
            "name": "invalidReason",
            "type": "uint8"
          },
          {
            "name": "executionPrice",
            "type": "uint256"
          },
          {
            "name": "sizeDelta",
            "type": "uint256"
          },
          {
            "name": "realizedPnlUsdc",
            "type": "int256"
          },
          {
            "name": "vpiDeltaUsdc",
            "type": "int256"
          },
          {
            "name": "vpiUsdc",
            "type": "uint256"
          },
          {
            "name": "executionFeeUsdc",
            "type": "uint256"
          },
          {
            "name": "freshTraderPayoutUsdc",
            "type": "uint256"
          },
          {
            "name": "existingTraderClaimConsumedUsdc",
            "type": "uint256"
          },
          {
            "name": "existingTraderClaimRemainingUsdc",
            "type": "uint256"
          },
          {
            "name": "immediatePayoutUsdc",
            "type": "uint256"
          },
          {
            "name": "traderClaimBalanceUsdc",
            "type": "uint256"
          },
          {
            "name": "seizedCollateralUsdc",
            "type": "uint256"
          },
          {
            "name": "badDebtUsdc",
            "type": "uint256"
          },
          {
            "name": "remainingSize",
            "type": "uint256"
          },
          {
            "name": "remainingMargin",
            "type": "uint256"
          },
          {
            "name": "triggersDegradedMode",
            "type": "bool"
          },
          {
            "name": "postOpDegradedMode",
            "type": "bool"
          },
          {
            "name": "effectiveAssetsAfterUsdc",
            "type": "uint256"
          },
          {
            "name": "maxLiabilityAfterUsdc",
            "type": "uint256"
          },
          {
            "name": "frozenSpreadUsdc",
            "type": "uint256"
          },
          {
            "name": "frozenSpreadPaidUsdc",
            "type": "uint256"
          },
          {
            "name": "frozenSpreadWaivedUsdc",
            "type": "uint256"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "previewLiquidation",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      },
      {
        "name": "oraclePrice",
        "type": "uint256"
      }
    ],
    "outputs": [
      {
        "name": "preview",
        "type": "tuple",
        "components": [
          {
            "name": "liquidatable",
            "type": "bool"
          },
          {
            "name": "oraclePrice",
            "type": "uint256"
          },
          {
            "name": "equityUsdc",
            "type": "int256"
          },
          {
            "name": "pnlUsdc",
            "type": "int256"
          },
          {
            "name": "reachableCollateralUsdc",
            "type": "uint256"
          },
          {
            "name": "liquidationChargeUsdc",
            "type": "uint256"
          },
          {
            "name": "keeperBountyUsdc",
            "type": "uint256"
          },
          {
            "name": "protocolLiquidationFeeUsdc",
            "type": "uint256"
          },
          {
            "name": "lpLiquidationFeeUsdc",
            "type": "uint256"
          },
          {
            "name": "seizedCollateralUsdc",
            "type": "uint256"
          },
          {
            "name": "settlementRetainedUsdc",
            "type": "uint256"
          },
          {
            "name": "freshTraderPayoutUsdc",
            "type": "uint256"
          },
          {
            "name": "existingTraderClaimConsumedUsdc",
            "type": "uint256"
          },
          {
            "name": "existingTraderClaimRemainingUsdc",
            "type": "uint256"
          },
          {
            "name": "immediatePayoutUsdc",
            "type": "uint256"
          },
          {
            "name": "traderClaimBalanceUsdc",
            "type": "uint256"
          },
          {
            "name": "badDebtUsdc",
            "type": "uint256"
          },
          {
            "name": "triggersDegradedMode",
            "type": "bool"
          },
          {
            "name": "postOpDegradedMode",
            "type": "bool"
          },
          {
            "name": "effectiveAssetsAfterUsdc",
            "type": "uint256"
          },
          {
            "name": "maxLiabilityAfterUsdc",
            "type": "uint256"
          }
        ]
      }
    ],
    "stateMutability": "view"
  }
] as const

export const PERPS_CFD_ENGINE_ACCOUNT_LENS_ABI = [
  {
    "type": "function",
    "name": "getAccountCollateralView",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "viewData",
        "type": "tuple",
        "components": [
          {
            "name": "settlementBalanceUsdc",
            "type": "uint256"
          },
          {
            "name": "lockedMarginUsdc",
            "type": "uint256"
          },
          {
            "name": "activePositionMarginUsdc",
            "type": "uint256"
          },
          {
            "name": "otherLockedMarginUsdc",
            "type": "uint256"
          },
          {
            "name": "freeSettlementUsdc",
            "type": "uint256"
          },
          {
            "name": "closeReachableUsdc",
            "type": "uint256"
          },
          {
            "name": "liquidationReachableSettlementUsdc",
            "type": "uint256"
          },
          {
            "name": "terminalPriceCollectibleCapUsdc",
            "type": "uint256"
          },
          {
            "name": "accountEquityUsdc",
            "type": "uint256"
          },
          {
            "name": "freeBuyingPowerUsdc",
            "type": "uint256"
          },
          {
            "name": "traderClaimBalanceUsdc",
            "type": "uint256"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getWithdrawableUsdc",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "withdrawableUsdc",
        "type": "uint256"
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getAccountLedgerView",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "viewData",
        "type": "tuple",
        "components": [
          {
            "name": "settlementBalanceUsdc",
            "type": "uint256"
          },
          {
            "name": "freeSettlementUsdc",
            "type": "uint256"
          },
          {
            "name": "activePositionMarginUsdc",
            "type": "uint256"
          },
          {
            "name": "otherLockedMarginUsdc",
            "type": "uint256"
          },
          {
            "name": "executionBountyReserveUsdc",
            "type": "uint256"
          },
          {
            "name": "committedMarginUsdc",
            "type": "uint256"
          },
          {
            "name": "traderClaimBalanceUsdc",
            "type": "uint256"
          },
          {
            "name": "pendingOrderCount",
            "type": "uint256"
          }
        ]
      }
    ],
    "stateMutability": "view"
  },
  {
    "type": "function",
    "name": "getAccountLedgerSnapshot",
    "inputs": [
      {
        "name": "account",
        "type": "address"
      }
    ],
    "outputs": [
      {
        "name": "snapshot",
        "type": "tuple",
        "components": [
          {
            "name": "settlementBalanceUsdc",
            "type": "uint256"
          },
          {
            "name": "freeSettlementUsdc",
            "type": "uint256"
          },
          {
            "name": "activePositionMarginUsdc",
            "type": "uint256"
          },
          {
            "name": "otherLockedMarginUsdc",
            "type": "uint256"
          },
          {
            "name": "positionMarginBucketUsdc",
            "type": "uint256"
          },
          {
            "name": "committedOrderMarginBucketUsdc",
            "type": "uint256"
          },
          {
            "name": "reservedSettlementBucketUsdc",
            "type": "uint256"
          },
          {
            "name": "executionBountyReserveUsdc",
            "type": "uint256"
          },
          {
            "name": "committedMarginUsdc",
            "type": "uint256"
          },
          {
            "name": "traderClaimBalanceUsdc",
            "type": "uint256"
          },
          {
            "name": "pendingOrderCount",
            "type": "uint256"
          },
          {
            "name": "closeReachableUsdc",
            "type": "uint256"
          },
          {
            "name": "liquidationReachableSettlementUsdc",
            "type": "uint256"
          },
          {
            "name": "terminalPriceCollectibleCapUsdc",
            "type": "uint256"
          },
          {
            "name": "accountEquityUsdc",
            "type": "uint256"
          },
          {
            "name": "freeBuyingPowerUsdc",
            "type": "uint256"
          },
          {
            "name": "hasPosition",
            "type": "bool"
          },
          {
            "name": "side",
            "type": "uint8"
          },
          {
            "name": "size",
            "type": "uint256"
          },
          {
            "name": "margin",
            "type": "uint256"
          },
          {
            "name": "entryPrice",
            "type": "uint256"
          },
          {
            "name": "unrealizedPnlUsdc",
            "type": "int256"
          },
          {
            "name": "netEquityUsdc",
            "type": "int256"
          },
          {
            "name": "liquidatable",
            "type": "bool"
          }
        ]
      }
    ],
    "stateMutability": "view"
  }
] as const
