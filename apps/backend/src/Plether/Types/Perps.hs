module Plether.Types.Perps
  ( BasketHistory (..)
  , BasketHistoryParams (..)
  , BasketHistoryPoint (..)
  , BasketLatest (..)
  , PythUpdateResponse (..)
  , RevealPayloadResponse (..)
  , basketRangeSeconds
  , defaultBasketHistoryParams
  ) where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

data BasketHistoryParams = BasketHistoryParams
  { bhpRange :: Text
  , bhpIntervalSeconds :: Integer
  }
  deriving stock (Show)

defaultBasketHistoryParams :: BasketHistoryParams
defaultBasketHistoryParams =
  BasketHistoryParams
    { bhpRange = "7d"
    , bhpIntervalSeconds = 60
    }

basketRangeSeconds :: Text -> Integer
basketRangeSeconds range =
  case T.toLower range of
    "24h" -> 24 * 60 * 60
    "30d" -> 30 * 24 * 60 * 60
    "1y" -> 365 * 24 * 60 * 60
    _ -> 7 * 24 * 60 * 60

data BasketHistoryPoint = BasketHistoryPoint
  { bhpTimestamp :: Integer
  , bhpBasketPrice :: Integer
  , bhpComponents :: Value
  }
  deriving stock (Show, Generic)

instance ToJSON BasketHistoryPoint where
  toJSON BasketHistoryPoint {..} =
    object
      [ "timestamp" .= bhpTimestamp
      , "basketPrice" .= show bhpBasketPrice
      , "components" .= bhpComponents
      ]

data BasketHistory = BasketHistory
  { bhRange :: Text
  , bhIntervalSeconds :: Integer
  , bhSource :: Text
  , bhGeneratedAt :: POSIXTime
  , bhLatestPrice :: Maybe Integer
  , bhChangePct :: Maybe Double
  , bhPoints :: [BasketHistoryPoint]
  }
  deriving stock (Show, Generic)

instance ToJSON BasketHistory where
  toJSON BasketHistory {..} =
    object
      [ "range" .= bhRange
      , "intervalSeconds" .= bhIntervalSeconds
      , "source" .= bhSource
      , "generatedAt" .= (round bhGeneratedAt :: Integer)
      , "latestPrice" .= fmap show bhLatestPrice
      , "changePct" .= bhChangePct
      , "points" .= bhPoints
      ]

data BasketLatest = BasketLatest
  { blTimestamp :: Integer
  , blBasketPrice :: Integer
  , blComponents :: Value
  , blGeneratedAt :: POSIXTime
  , blSource :: Text
  }
  deriving stock (Show, Generic)

instance ToJSON BasketLatest where
  toJSON BasketLatest {..} =
    object
      [ "timestamp" .= blTimestamp
      , "basketPrice" .= show blBasketPrice
      , "components" .= blComponents
      , "generatedAt" .= (round blGeneratedAt :: Integer)
      , "source" .= blSource
      ]

data PythUpdateResponse = PythUpdateResponse
  { purUpdateData :: [Text]
  , purFetchedAt :: Integer
  , purPublishTimes :: [Integer]
  , purSource :: Text
  }
  deriving stock (Show, Generic)

instance ToJSON PythUpdateResponse where
  toJSON PythUpdateResponse {..} =
    object
      [ "updateData" .= purUpdateData
      , "fetchedAt" .= purFetchedAt
      , "publishTimes" .= purPublishTimes
      , "source" .= purSource
      ]

data RevealPayloadResponse = RevealPayloadResponse
  { rprOrderId :: Integer
  , rprUpdateData :: [Text]
  , rprFetchedAt :: Integer
  , rprPublishTimes :: [Integer]
  , rprMinPublishTime :: Integer
  , rprMaxPublishTime :: Integer
  , rprSource :: Text
  }
  deriving stock (Show, Generic)

instance ToJSON RevealPayloadResponse where
  toJSON RevealPayloadResponse {..} =
    object
      [ "orderId" .= show rprOrderId
      , "updateData" .= rprUpdateData
      , "fetchedAt" .= rprFetchedAt
      , "publishTimes" .= rprPublishTimes
      , "minPublishTime" .= rprMinPublishTime
      , "maxPublishTime" .= rprMaxPublishTime
      , "source" .= rprSource
      ]
