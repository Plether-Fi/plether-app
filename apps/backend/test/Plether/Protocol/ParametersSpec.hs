module Plether.Protocol.ParametersSpec (spec) where

import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Protocol.Parameters
import Test.Hspec

spec :: Spec
spec = do
  describe "parameterCatalog" $ do
    it "keeps the public parameter keys stable" $ do
      map pdKey parameterCatalog `shouldBe` expectedKeys

    it "uses a unique key for every parameter" $ do
      let keys = map pdKey parameterCatalog

      length (nub keys) `shouldBe` length keys

    it "provides complete display and provenance metadata" $ do
      parameterCatalog `shouldSatisfy` all hasCompleteMetadata

    it "marks immutable constants as non-timelocked and mutable values as governed" $ do
      parameterCatalog `shouldSatisfy` all hasConsistentGovernanceMetadata

    it "uses the deployed eight-decimal price scale" $ do
      let capPrice = filter ((== "market.cap_price") . pdKey) parameterCatalog

      map pdRawScale capPrice `shouldBe` ["1e8"]

    it "marks tranche timing constants immutable even within their protocol group" $ do
      let trancheConstants =
            filter
              (T.isPrefixOf "tranches." . pdKey)
              parameterCatalog

      map pdMutability trancheConstants `shouldSatisfy` all (== "immutable")
      map pdTimelockPolicy trancheConstants `shouldSatisfy` all (== "not_applicable")

expectedKeys :: [Text]
expectedKeys =
  [ "market.vpi_factor"
  , "market.max_skew_ratio"
  , "liquidation.maintenance_margin_bps"
  , "market.initial_margin_bps"
  , "liquidation.fad_margin_bps"
  , "fees.base_carry_bps"
  , "liquidation.minimum_bounty_usdc"
  , "liquidation.bounty_bps"
  , "fees.execution_fee_bps"
  , "orders.max_order_age"
  , "orders.max_pending_orders"
  , "orders.minimum_engine_gas"
  , "orders.max_prune_per_call"
  , "keepers.open_order_execution_bounty_bps"
  , "keepers.minimum_open_order_execution_bounty"
  , "keepers.maximum_open_order_execution_bounty"
  , "keepers.close_order_execution_bounty"
  , "orders.minimum_open_notional"
  , "oracle.settlement_window"
  , "oracle.execution_staleness_limit"
  , "oracle.liquidation_staleness_limit"
  , "oracle.adverse_confidence_multiplier_bps"
  , "oracle.max_confidence_ratio_bps"
  , "oracle.max_component_publish_time_divergence"
  , "oracle.frozen_mark_staleness_limit"
  , "oracle.engine_mark_staleness_limit"
  , "market.fad_runway_seconds"
  , "market.frozen_close_vpi_factor"
  , "house_pool.senior_rate_bps"
  , "house_pool.mark_staleness_limit"
  , "house_pool.senior_frozen_lp_fee_bps"
  , "house_pool.junior_frozen_lp_fee_bps"
  , "tranches.senior.deposit_cooldown"
  , "tranches.junior.deposit_cooldown"
  , "tranches.senior.deposit_epoch_duration"
  , "tranches.junior.deposit_epoch_duration"
  , "tranches.senior.deposit_activation_epoch_delay"
  , "tranches.junior.deposit_activation_epoch_delay"
  , "market.cap_price"
  , "house_pool.maximum_frozen_lp_fee_bps"
  , "house_pool.minimum_tranche_deposit"
  ]

hasCompleteMetadata :: ParameterDefinition -> Bool
hasCompleteMetadata definition =
  all
    isPresent
    [ pdKey definition
    , pdGroup definition
    , pdContract definition
    , pdGetter definition
    , pdRawScale definition
    , pdDisplayUnit definition
    , pdDescription definition
    , pdRiskInterpretation definition
    , pdMutability definition
    , pdTimelockPolicy definition
    , pdDocumentationLink definition
    ]
    && pdWordIndex definition >= 0
    && pdDocumentationLink definition == "/methodology#protocol-parameters"

hasConsistentGovernanceMetadata :: ParameterDefinition -> Bool
hasConsistentGovernanceMetadata definition
  | pdTimelockPolicy definition == "not_applicable" =
      pdMutability definition == "immutable"
  | otherwise =
      pdMutability definition == "governance"
        && pdTimelockPolicy definition == "admin_timelock"

isPresent :: Text -> Bool
isPresent = not . T.null . T.strip
