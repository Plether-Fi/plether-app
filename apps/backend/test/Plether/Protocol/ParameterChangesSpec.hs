{-# LANGUAGE LambdaCase #-}

module Plether.Protocol.ParameterChangesSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import Plether.Protocol.ParameterChanges
  ( ParameterProjection (..)
  , parameterProjectionActionTypes
  , parameterProjectionsForAction
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "parameterProjectionsForAction" $ do
    it "covers every indexed governance lifecycle action and no keeper action" $ do
      parameterProjectionActionTypes
        `shouldMatchList`
          [ "governance_proposal"
          , "governance_execution"
          , "governance_cancellation"
          , "ownership_transfer_started"
          , "ownership_transfer"
          , "pauser_update"
          , "pause"
          , "unpause"
          , "protocol_treasury_update"
          , "governance_role_change"
          ]
      parameterProjectionActionTypes
        `shouldNotContain` ["order_execution", "liquidation", "keeper_maintenance"]

    it "projects every proposed config field with exact scale, unit, and ETA" $ do
      let projections =
            parameterProjectionsForAction
              "governance_proposal"
              adminAddress
              ( object
                  [ "category" .= ("router_config" :: Text)
                  , "lifecycle" .= ("proposed" :: Text)
                  , "fields" .=
                      [ governanceField
                          "orders.max_order_age"
                          (String "3600")
                          "1"
                          "seconds"
                          "uint256"
                      , governanceField
                          "fees.execution_fee_bps"
                          (String "25")
                          "10000"
                          "bps"
                          "uint256"
                      , governanceField
                          "governance.activation_time"
                          (String "1730000000")
                          "1"
                          "unix_seconds"
                          "uint256"
                      ]
                  ]
              )

      map ppParameterKey projections
        `shouldBe` ["orders.max_order_age", "fees.execution_fee_bps"]
      map ppEta projections `shouldBe` [Just 1730000000, Just 1730000000]
      map ppRawScale projections `shouldBe` [Just "1", Just "10000"]
      map ppDisplayUnit projections `shouldBe` [Just "seconds", Just "bps"]
      map ppNewValue projections
        `shouldBe` [Just $ String "3600", Just $ String "25"]
      projections `shouldSatisfy` all (not . null . ppAvailability)

    it "keeps an unsupported dynamic event as a nullable category projection" $ do
      let projections =
            parameterProjectionsForAction
              "governance_execution"
              adminAddress
              ( object
                  [ "category" .= ("engine_calendar_config" :: Text)
                  , "lifecycle" .= ("finalized" :: Text)
                  , "classification" .= ("unavailable" :: Text)
                  , "reason" .= ("calendar_dynamic_tuple_not_supported" :: Text)
                  ]
              )

      projections
        `shouldSatisfy` \case
          [projection] ->
            ppParameterKey projection == "engine_calendar_config.*"
              && ppNewValue projection == Nothing
              && not (null $ ppAvailability projection)
          _ -> False

    it "projects ownership, role, dependency, and emergency pause changes" $ do
      let ownership =
            parameterProjectionsForAction
              "ownership_transfer_started"
              adminAddress
              ( object
                  [ "contractRole" .= ("order_router_admin" :: Text)
                  , "previousOwner" .= previousAddress
                  , "newOwner" .= nextAddress
                  ]
              )
          paused =
            parameterProjectionsForAction
              "pause"
              adminAddress
              ( object
                  [ "contractRole" .= ("order_router_admin" :: Text)
                  , "account" .= nextAddress
                  ]
              )
          treasury =
            parameterProjectionsForAction
              "protocol_treasury_update"
              engineAddress
              ( object
                  [ "contractRole" .= ("cfd_engine" :: Text)
                  , "protocolTreasury" .= nextAddress
                  ]
              )

      ownership
        `shouldSatisfy` \case
          [projection] ->
            ppCategory projection == "role:" <> adminAddress
              && ppLifecycle projection == "proposed"
              && ppParameterKey projection == "governance.order_router_admin.owner"
              && ppOldValue projection == Just (String previousAddress)
              && ppNewValue projection == Just (String nextAddress)
              && ppEta projection == Nothing
              && ppRawScale projection == Just "1"
              && ppDisplayUnit projection == Just "address"
              && ppValueType projection == Just "address"
              && not (null $ ppAvailability projection)
          _ -> False
      map (\projection -> (ppParameterKey projection, ppOldValue projection, ppNewValue projection)) paused
        `shouldBe`
          [ ("governance.order_router_admin.paused", Just $ Bool False, Just $ Bool True)
          ]
      map ppParameterKey treasury
        `shouldBe` ["dependencies.cfd_engine.protocol_treasury"]

    it "does not collapse identical role events emitted by different contracts" $ do
      let project role address =
            parameterProjectionsForAction
              "ownership_transfer"
              address
              ( object
                  [ "contractRole" .= (role :: Text)
                  , "previousOwner" .= previousAddress
                  , "newOwner" .= nextAddress
                  ]
              )

      map ppParameterKey
        (project "order_router_admin" adminAddress <> project "cfd_engine" engineAddress)
        `shouldBe`
          [ "governance.order_router_admin.owner"
          , "governance.cfd_engine.owner"
          ]

    it "does not turn unrelated protocol actions into governance history" $
      parameterProjectionsForAction
        "order_execution"
        adminAddress
        (object ["orderId" .= ("42" :: Text)])
        `shouldBe` []

governanceField :: Text -> Value -> Text -> Text -> Text -> Value
governanceField key rawValue scale unitName valueType =
  object
    [ "key" .= key
    , "rawValue" .= rawValue
    , "scale" .= scale
    , "unit" .= unitName
    , "valueType" .= valueType
    ]

adminAddress :: Text
adminAddress = "0x1111111111111111111111111111111111111111"

engineAddress :: Text
engineAddress = "0x2222222222222222222222222222222222222222"

previousAddress :: Text
previousAddress = "0x3333333333333333333333333333333333333333"

nextAddress :: Text
nextAddress = "0x4444444444444444444444444444444444444444"
