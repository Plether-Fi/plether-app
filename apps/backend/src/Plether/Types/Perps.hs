module Plether.Types.Perps
  ( BasketHistory (..)
  , BasketHistoryParams (..)
  , BasketHistoryPoint (..)
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
