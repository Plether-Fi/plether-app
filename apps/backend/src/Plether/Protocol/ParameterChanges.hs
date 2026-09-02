{-# LANGUAGE LambdaCase #-}

module Plether.Protocol.ParameterChanges
  ( ParameterProjection (..)
  , parameterProjectionActionTypes
  , parameterProjectionsForAction
  ) where

import Data.Aeson (Array, Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Protocol.Governance (qualifyGovernanceKey)

-- | One release-scoped parameter value affected by a governance action.
--
-- This is deliberately a projection of the immutable action payload rather
-- than a second decoder. If the log decoder could not recover a value, the
-- projection retains a null and an explicit availability reason.
data ParameterProjection = ParameterProjection
  { ppCategory :: Text
  , ppLifecycle :: Text
  , ppParameterKey :: Text
  , ppOldValue :: Maybe Value
  , ppNewValue :: Maybe Value
  , ppEta :: Maybe Integer
  , ppRawScale :: Maybe Text
  , ppDisplayUnit :: Maybe Text
  , ppValueType :: Maybe Text
  , ppAvailability :: [Value]
  }
  deriving stock (Show, Eq)

parameterProjectionsForAction ::
  Text ->
  Text ->
  Value ->
  [ParameterProjection]
parameterProjectionsForAction actionType contractAddress payload
  | actionType `elem` configActionTypes =
      configProjections actionType payload
  | actionType `elem` roleActionTypes =
      roleProjections actionType contractAddress payload
  | otherwise = []

parameterProjectionActionTypes :: [Text]
parameterProjectionActionTypes =
  configActionTypes <> roleActionTypes

configActionTypes :: [Text]
configActionTypes =
  [ "governance_proposal"
  , "governance_execution"
  , "governance_cancellation"
  ]

roleActionTypes :: [Text]
roleActionTypes =
  [ "ownership_transfer_started"
  , "ownership_transfer"
  , "pauser_update"
  , "pause"
  , "unpause"
  , "protocol_treasury_update"
  , "governance_role_change"
  ]

configProjections :: Text -> Value -> [ParameterProjection]
configProjections actionType payload =
  case objectText "category" payload of
    Nothing -> []
    Just category ->
      let lifecycle =
            fromMaybe (actionLifecycle actionType) $
              objectText "lifecycle" payload
          decodedFields =
            maybe [] (mapMaybe projectionField . toList) $
              objectArray "fields" payload
          eta =
            ( projectionRawValue
                =<< findProjection "governance.activation_time" decodedFields
            )
              >>= integerValue
          parameterFields =
            filter
              ((/= "governance.activation_time") . ppParameterKey)
              decodedFields
          fallbackReason =
            fromMaybe
              (case lifecycle of
                "cancelled" -> "cancellation_event_has_no_parameter_values"
                "finalized" -> "finalization_event_has_no_parameter_values"
                _ -> "governance_event_values_unavailable")
              (objectText "reason" payload)
          fallback =
            ParameterProjection
              { ppCategory = category
              , ppLifecycle = lifecycle
              , ppParameterKey = category <> ".*"
              , ppOldValue = Nothing
              , ppNewValue = Nothing
              , ppEta = eta
              , ppRawScale = Nothing
              , ppDisplayUnit = Nothing
              , ppValueType = Nothing
              , ppAvailability =
                  [ unavailable "newValue" fallbackReason
                  , unavailable "oldValue" "prior_applied_value_not_reconstructed"
                  ]
                    <> [ unavailable "eta" "activation_time_not_decoded"
                       | lifecycle == "proposed"
                       ]
              }
       in case parameterFields of
            [] -> [fallback]
            fields ->
              map
                ( \projection ->
                    projection
                      { ppCategory = category
                      , ppLifecycle = lifecycle
                      , ppEta = eta
                      , ppAvailability =
                          ppAvailability projection
                            <> [ unavailable
                                  "oldValue"
                                  "prior_applied_value_not_reconstructed"
                               ]
                      }
                )
                fields

projectionField :: Value -> Maybe ParameterProjection
projectionField value = do
  parameterKey <- objectText "key" value
  let rawValue = objectValue "rawValue" value
      missingValue =
        [unavailable "newValue" "decoded_governance_value_unavailable" | rawValue == Nothing]
  pure
    ParameterProjection
      { ppCategory = ""
      , ppLifecycle = ""
      , ppParameterKey = parameterKey
      , ppOldValue = Nothing
      , ppNewValue = rawValue
      , ppEta = Nothing
      , ppRawScale = objectText "scale" value
      , ppDisplayUnit = objectText "unit" value
      , ppValueType = objectText "valueType" value
      , ppAvailability = missingValue
      }

projectionRawValue :: ParameterProjection -> Maybe Value
projectionRawValue = ppNewValue

findProjection :: Text -> [ParameterProjection] -> Maybe ParameterProjection
findProjection key =
  findFirst ((== key) . ppParameterKey)

roleProjections :: Text -> Text -> Value -> [ParameterProjection]
roleProjections actionType contractAddress payload =
  maybe [] pure $ case actionType of
    "ownership_transfer_started" ->
      roleChange
        "proposed"
        "governance.owner"
        (objectValue "previousOwner" payload)
        (objectValue "newOwner" payload)
    "ownership_transfer" ->
      roleChange
        "finalized"
        "governance.owner"
        (objectValue "previousOwner" payload)
        (objectValue "newOwner" payload)
    "pauser_update" ->
      roleChange
        "finalized"
        "governance.pauser"
        (objectValue "previousPauser" payload)
        (objectValue "newPauser" payload)
    "pause" ->
      roleChange
        "finalized"
        "governance.paused"
        (Just $ Bool False)
        (Just $ Bool True)
    "unpause" ->
      roleChange
        "finalized"
        "governance.paused"
        (Just $ Bool True)
        (Just $ Bool False)
    "protocol_treasury_update" ->
      roleChange
        "finalized"
        "dependencies.protocol_treasury"
        Nothing
        (objectValue "protocolTreasury" payload)
    _ -> do
      governanceKey <- objectText "governanceKey" payload
      roleChange
        "finalized"
        governanceKey
        Nothing
        Nothing
  where
    category = "role:" <> T.toLower contractAddress
    contractRole =
      objectText "contractRole" payload
    roleKey =
      fromMaybe
        ("contract_" <> T.toLower contractAddress)
        contractRole
    roleQualificationAvailability =
      [ unavailable
          "parameterKey"
          "governance_contract_role_unavailable_address_qualified_fallback"
      | contractRole == Nothing
      ]
    roleChange lifecycle parameterKey oldValue newValue =
      Just
        ParameterProjection
          { ppCategory = category
          , ppLifecycle = lifecycle
          , ppParameterKey = qualifyGovernanceKey roleKey parameterKey
          , ppOldValue = oldValue
          , ppNewValue = newValue
          , ppEta = Nothing
          , ppRawScale = Just "1"
          , ppDisplayUnit =
              Just $
                if parameterKey == "governance.paused"
                  then "boolean"
                  else "address"
          , ppValueType =
              Just $
                if parameterKey == "governance.paused"
                  then "bool"
                  else "address"
          , ppAvailability =
              roleQualificationAvailability
                <> [ unavailable "oldValue" "prior_value_not_emitted"
              | oldValue == Nothing
              ]
                <> [ unavailable "newValue" "new_value_not_emitted"
                   | newValue == Nothing
                   ]
                <> [ unavailable "eta" "ownership_transfer_eta_not_emitted"
                   | lifecycle == "proposed"
                   ]
          }

actionLifecycle :: Text -> Text
actionLifecycle = \case
  "governance_proposal" -> "proposed"
  "governance_execution" -> "finalized"
  "governance_cancellation" -> "cancelled"
  _ -> "unknown"

objectValue :: Text -> Value -> Maybe Value
objectValue key (Object fields) =
  case KM.lookup (Key.fromText key) fields of
    Just Null -> Nothing
    value -> value
objectValue _ _ = Nothing

objectText :: Text -> Value -> Maybe Text
objectText key value =
  case objectValue key value of
    Just (String textValue) -> Just textValue
    _ -> Nothing

objectArray :: Text -> Value -> Maybe Array
objectArray key value =
  case objectValue key value of
    Just (Array values) -> Just values
    _ -> Nothing

integerValue :: Value -> Maybe Integer
integerValue (String textValue) =
  case reads $ T.unpack textValue of
    [(value, "")] -> Just value
    _ -> Nothing
integerValue _ = Nothing

findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst _ [] = Nothing
findFirst predicate (value : rest)
  | predicate value = Just value
  | otherwise = findFirst predicate rest

unavailable :: Text -> Text -> Value
unavailable fieldName reason =
  object
    [ "field" .= fieldName
    , "reason" .= reason
    ]
