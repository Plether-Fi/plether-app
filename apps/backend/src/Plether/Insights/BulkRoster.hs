module Plether.Insights.BulkRoster
  ( BulkParticipantEntry (..)
  , parseBulkParticipantEntries
  ) where

import Data.Char (isAsciiLower, isAsciiUpper, isControl, isDigit, isHexDigit)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Plether.AA.SimpleAccount (deriveTradingAccountAddress)
import Plether.Utils.Address (isValidAddress)

data BulkParticipantEntry = BulkParticipantEntry
  { bpeAlias :: Text
  , bpeTraderReference :: Text
  , bpeTradingAccount :: Text
  }
  deriving stock (Show, Eq)

parseBulkParticipantEntries
  :: Integer
  -> Text
  -> Either Text [BulkParticipantEntry]
parseBulkParticipantEntries expectedCount input
  | expectedCount <= 0 = Left "Bulk participant EXPECTED_INPUT_COUNT must be positive"
  | otherwise = do
      entries <- traverse parseLine $ filter (not . T.null . T.strip) $ T.lines input
      if fromIntegral (length entries) /= expectedCount
        then Left "Bulk participant entry count does not match EXPECTED_INPUT_COUNT"
        else do
          requireUnique "alias" $ map (T.toCaseFold . bpeAlias) entries
          requireUnique "opaque trader reference" $ map bpeTraderReference entries
          requireUnique "Trading Account destination" $ map bpeTradingAccount entries
          pure entries
  where
    parseLine line =
      case T.splitOn "\t" line of
        [rawAlias, rawTraderReference, rawOwnerWallet] -> do
          let alias = T.strip rawAlias
              traderReference = T.toLower $ T.strip rawTraderReference
              ownerWallet = canonicalAddress rawOwnerWallet
          if not $ validAlias alias
            then Left "Bulk participant roster contains an invalid @alias"
            else if not $ validUuidV4 traderReference
              then Left "Bulk participant roster references must be opaque UUIDv4 values"
              else if not $ isValidAddress ownerWallet
                then Left "Bulk participant roster contains an invalid OWNER_WALLET"
                else case deriveTradingAccountAddress ownerWallet of
                  Left err -> Left err
                  Right tradingAccount ->
                    Right
                      BulkParticipantEntry
                        { bpeAlias = alias
                        , bpeTraderReference = traderReference
                        , bpeTradingAccount = canonicalAddress tradingAccount
                        }
        _ ->
          Left
            "Every bulk participant line must be ALIAS, TRADER_REFERENCE, OWNER_WALLET TSV"

validAlias :: Text -> Bool
validAlias alias =
  case T.uncons alias of
    Just ('@', handle) ->
      let lengthInRange = not (T.null handle) && T.length handle <= 15
          validCharacter character =
            isAsciiLower character
              || isAsciiUpper character
              || isDigit character
              || character == '_'
       in lengthInRange && T.all validCharacter handle && not (T.any isControl alias)
    _ -> False

validUuidV4 :: Text -> Bool
validUuidV4 value =
  T.length value == 36
    && and [T.index value index == '-' | index <- [8, 13, 18, 23]]
    && T.index value 14 == '4'
    && T.index value 19 `elem` ("89ab" :: String)
    && T.all (\character -> character == '-' || isHexDigit character) value

requireUnique :: Text -> [Text] -> Either Text ()
requireUnique label values
  | Set.size (Set.fromList values) == length values = Right ()
  | otherwise = Left $ "Bulk participant roster contains a duplicate " <> label

canonicalAddress :: Text -> Text
canonicalAddress value =
  let normalized = T.toLower $ T.strip value
   in if "0x" `T.isPrefixOf` normalized then normalized else "0x" <> normalized
