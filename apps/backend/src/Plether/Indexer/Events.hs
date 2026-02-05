module Plether.Indexer.Events
  ( ParsedEvent (..)
  , parseEventLog
  , EventLog (..)
  ) where

import Data.Aeson (Value, object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.Indexer.Contracts

data EventLog = EventLog
  { elTxHash :: Text
  , elBlockNumber :: Integer
  , elAddress :: Text
  , elTopics :: [ByteString]
  , elData :: ByteString
  }
  deriving stock (Show)

data ParsedEvent = ParsedEvent
  { peUserAddress :: Text
  , peTxType :: Text
  , peSide :: Maybe Text
  , peData :: Value
  }
  deriving stock (Show)

parseEventLog :: EventLog -> [Text] -> [Text] -> Maybe ParsedEvent
parseEventLog log bearContracts bullContracts
  | null (elTopics log) = Nothing
  | otherwise = parseByTopic (head $ elTopics log) log bearContracts bullContracts

parseByTopic :: ByteString -> EventLog -> [Text] -> [Text] -> Maybe ParsedEvent
parseByTopic topic log bearContracts bullContracts
  | topic == esTopic mintEvent = parseMintBurnEvent log "mint" Nothing
  | topic == esTopic burnEvent = parseMintBurnEvent log "burn" Nothing
  | topic == esTopic tokenExchangeEvent = parseTokenExchangeEvent log
  | topic == esTopic zapMintEvent = parseZapEvent log "zap_buy" (Just "bull")
  | topic == esTopic zapBurnEvent = parseZapEvent log "zap_sell" (Just "bull")
  | topic == esTopic stakingDepositEvent = parseStakingEvent log "stake" bearContracts bullContracts
  | topic == esTopic stakingWithdrawEvent = parseUnstakeEvent log bearContracts bullContracts
  | topic == esTopic positionOpenedEvent = parseLeverageOpenEvent log bearContracts bullContracts
  | topic == esTopic positionClosedEvent = parseLeverageCloseEvent log bearContracts bullContracts
  | otherwise = Nothing

parseMintBurnEvent :: EventLog -> Text -> Maybe Text -> Maybe ParsedEvent
parseMintBurnEvent log txType side = do
  userAddr <- getIndexedAddress (elTopics log) 1
  let (amount1, amount2) = decodeUint256Pair (elData log)
  Just $ ParsedEvent
    { peUserAddress = userAddr
    , peTxType = txType
    , peSide = side
    , peData = object
        [ "usdcAmount" .= amount1
        , "pairAmount" .= amount2
        ]
    }

parseTokenExchangeEvent :: EventLog -> Maybe ParsedEvent
parseTokenExchangeEvent log = do
  buyer <- getIndexedAddress (elTopics log) 1
  let (soldId, tokensSold, boughtId, tokensBought) = decodeUint256Quad (elData log)
  Just $ ParsedEvent
    { peUserAddress = buyer
    , peTxType = "swap"
    , peSide = Nothing
    , peData = object
        [ "soldId" .= soldId
        , "tokensSold" .= tokensSold
        , "boughtId" .= boughtId
        , "tokensBought" .= tokensBought
        ]
    }

parseZapEvent :: EventLog -> Text -> Maybe Text -> Maybe ParsedEvent
parseZapEvent log txType side = do
  userAddr <- getIndexedAddress (elTopics log) 1
  let (amountIn, amountOut) = decodeUint256Pair (elData log)
  Just $ ParsedEvent
    { peUserAddress = userAddr
    , peTxType = txType
    , peSide = side
    , peData = object
        [ "amountIn" .= amountIn
        , "amountOut" .= amountOut
        ]
    }

parseStakingEvent :: EventLog -> Text -> [Text] -> [Text] -> Maybe ParsedEvent
parseStakingEvent log txType bearContracts bullContracts = do
  ownerAddr <- getIndexedAddress (elTopics log) 2
  let (assets, shares) = decodeUint256Pair (elData log)
      side = determineSide (elAddress log) bearContracts bullContracts
  Just $ ParsedEvent
    { peUserAddress = ownerAddr
    , peTxType = txType
    , peSide = side
    , peData = object
        [ "assets" .= assets
        , "shares" .= shares
        ]
    }

parseUnstakeEvent :: EventLog -> [Text] -> [Text] -> Maybe ParsedEvent
parseUnstakeEvent log bearContracts bullContracts = do
  ownerAddr <- getIndexedAddress (elTopics log) 3
  let (assets, shares) = decodeUint256Pair (elData log)
      side = determineSide (elAddress log) bearContracts bullContracts
  Just $ ParsedEvent
    { peUserAddress = ownerAddr
    , peTxType = "unstake"
    , peSide = side
    , peData = object
        [ "assets" .= assets
        , "shares" .= shares
        ]
    }

parseLeverageOpenEvent :: EventLog -> [Text] -> [Text] -> Maybe ParsedEvent
parseLeverageOpenEvent log bearContracts bullContracts = do
  userAddr <- getIndexedAddress (elTopics log) 1
  let (principal, leverage, positionSize, debt) = decodeUint256Quad (elData log)
      side = determineSide (elAddress log) bearContracts bullContracts
  Just $ ParsedEvent
    { peUserAddress = userAddr
    , peTxType = "leverage_open"
    , peSide = side
    , peData = object
        [ "principal" .= principal
        , "leverage" .= leverage
        , "positionSize" .= positionSize
        , "debt" .= debt
        ]
    }

parseLeverageCloseEvent :: EventLog -> [Text] -> [Text] -> Maybe ParsedEvent
parseLeverageCloseEvent log bearContracts bullContracts = do
  userAddr <- getIndexedAddress (elTopics log) 1
  let (collateralReturned, debtRepaid) = decodeUint256Pair (elData log)
      side = determineSide (elAddress log) bearContracts bullContracts
  Just $ ParsedEvent
    { peUserAddress = userAddr
    , peTxType = "leverage_close"
    , peSide = side
    , peData = object
        [ "collateralReturned" .= collateralReturned
        , "debtRepaid" .= debtRepaid
        ]
    }

determineSide :: Text -> [Text] -> [Text] -> Maybe Text
determineSide addr bearContracts bullContracts
  | normalizeAddress addr `elem` map normalizeAddress bearContracts = Just "bear"
  | normalizeAddress addr `elem` map normalizeAddress bullContracts = Just "bull"
  | otherwise = Nothing

normalizeAddress :: Text -> Text
normalizeAddress = T.toLower

getIndexedAddress :: [ByteString] -> Int -> Maybe Text
getIndexedAddress topics idx
  | idx < length topics = Just $ "0x" <> T.drop 24 (bytesToHex (topics !! idx))
  | otherwise = Nothing

decodeUint256Pair :: ByteString -> (Integer, Integer)
decodeUint256Pair bs =
  ( bytesToInteger (BS.take 32 bs)
  , bytesToInteger (BS.take 32 (BS.drop 32 bs))
  )

decodeUint256Quad :: ByteString -> (Integer, Integer, Integer, Integer)
decodeUint256Quad bs =
  ( bytesToInteger (BS.take 32 bs)
  , bytesToInteger (BS.take 32 (BS.drop 32 bs))
  , bytesToInteger (BS.take 32 (BS.drop 64 bs))
  , bytesToInteger (BS.take 32 (BS.drop 96 bs))
  )

bytesToInteger :: ByteString -> Integer
bytesToInteger = BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) 0

bytesToHex :: ByteString -> Text
bytesToHex = TE.decodeUtf8 . B16.encode
