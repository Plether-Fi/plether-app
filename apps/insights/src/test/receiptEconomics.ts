// Test-only neutral API fixtures derived from scripts/fixtures/insights-close-{waiver,rebate}.json.
// Historical receipt bytes and their source fixtures remain unchanged. Amounts are copied exactly.
export const waiver = {
  "receipt": {
    "transactionHash": "0xa583ee512122717defccf27f420f17ac10e1e887777132b67f31d5edbaac8bbd"
  },
  "execution": {
    "orderId": "1551",
    "protocolVersion": "v1.2.3",
    "status": "complete",
    "receipt": {
      "executionBountyAssets": "200000",
      "executionNotionalAssets": "2762728584000",
      "realizedPnlAssets": "-269360000",
      "vpiAssets": "6708298582",
      "carryAssets": "37341",
      "executionFeeAssets": "1105091433",
      "frozenSpreadAssets": "0",
      "actionChargeAssessedAssets": "7813427356",
      "actionChargeCollectedAssets": "6930647038",
      "grossAccountDebitAssets": "7200207038",
      "preSettlementBalanceAssets": "108436671883",
      "postSettlementBalanceAssets": "101236464845",
      "preTraderClaimBalanceAssets": "0",
      "postTraderClaimBalanceAssets": "0",
      "postPositionSize": "0",
      "postPositionMarginAssets": "0",
      "postPositionEquityAssets": "0",
      "postLeverageBps": "0"
    },
    "settlements": [
      {
        "kind": "ActionChargeSettled",
        "assessedAssets": "7813390015",
        "recoveredAssets": "6930609697",
        "waivedAssets": "882780318",
        "protocolFeeCollectedAssets": "222311115"
      }
    ]
  }
}

export const rebate = {
  "receipt": {
    "transactionHash": "0x4d3a8cf4d63c62f84b2a1065dc8cb9bc1210da44bc7096a30be77acaeb95f99c"
  },
  "execution": {
    "orderId": "846",
    "protocolVersion": "v1.2.3",
    "status": "complete",
    "receipt": {
      "executionBountyAssets": "200000",
      "executionNotionalAssets": "2166132764800",
      "realizedPnlAssets": "4586170150",
      "vpiAssets": "-1755484993",
      "carryAssets": "25733",
      "executionFeeAssets": "866453105",
      "frozenSpreadAssets": "0",
      "actionChargeAssessedAssets": "0",
      "actionChargeCollectedAssets": "25733",
      "grossAccountDebitAssets": "225733",
      "preSettlementBalanceAssets": "97395491583",
      "postSettlementBalanceAssets": "102870467888",
      "preTraderClaimBalanceAssets": "0",
      "postTraderClaimBalanceAssets": "0",
      "postPositionSize": "0",
      "postPositionMarginAssets": "0",
      "postPositionEquityAssets": "0",
      "postLeverageBps": "0"
    },
    "settlements": [
      {
        "kind": "ActionRebateSettled",
        "assessedAssets": "889031888",
        "recoveredAssets": "889031888",
        "waivedAssets": "0"
      }
    ]
  }
}
