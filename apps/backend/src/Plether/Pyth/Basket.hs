module Plether.Pyth.Basket
  ( BasketComponent (..)
  , BasketComponentPrice (..)
  , PythPricePoint (..)
  , basketComponents
  , basketDisplayPriceCap
  , computeBasketSnapshot
  , invertPythPrice
  , normalizeFeedId
  , normalizePythPrice
  ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data BasketComponent = BasketComponent
  { bcSymbol :: Text
  , bcFeedSymbol :: Text
  , bcFeedId :: Text
  , bcWeight :: Integer
  , bcBasePrice :: Integer
  , bcInverted :: Bool
  }
  deriving stock (Show, Eq, Generic)

data PythPricePoint = PythPricePoint
  { pppFeedId :: Text
  , pppPrice :: Integer
  , pppConfidence :: Integer
  , pppExponent :: Int
  , pppPublishTime :: Integer
  }
  deriving stock (Show, Eq, Generic)

data BasketComponentPrice = BasketComponentPrice
  { bcpSymbol :: Text
  , bcpFeedSymbol :: Text
  , bcpFeedId :: Text
  , bcpPrice :: Integer
  , bcpRawPrice :: Integer
  , bcpConfidence :: Integer
  , bcpExponent :: Int
  , bcpPublishTime :: Integer
  , bcpInverted :: Bool
  , bcpWeightBps :: Integer
  , bcpBasePrice :: Integer
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON BasketComponentPrice where
  toJSON BasketComponentPrice {..} =
    object
      [ "symbol" .= bcpSymbol
      , "feedSymbol" .= bcpFeedSymbol
      , "feedId" .= bcpFeedId
      , "price" .= show bcpPrice
      , "rawPrice" .= show bcpRawPrice
      , "confidence" .= show bcpConfidence
      , "exponent" .= bcpExponent
      , "publishTime" .= bcpPublishTime
      , "inverted" .= bcpInverted
      , "weightBps" .= bcpWeightBps
      , "basePrice" .= show bcpBasePrice
      ]

basketComponents :: [BasketComponent]
basketComponents =
  [ BasketComponent
      { bcSymbol = "EUR/USD"
      , bcFeedSymbol = "EUR/USD"
      , bcFeedId = "0xa995d00bb36a63cef7fd2c287dc105fc8f3d93779f062f09551b0af3e81ec30b"
      , bcWeight = 576 * 10 ^ (15 :: Int)
      , bcBasePrice = 117_500_000
      , bcInverted = False
      }
  , BasketComponent
      { bcSymbol = "JPY/USD"
      , bcFeedSymbol = "USD/JPY"
      , bcFeedId = "0xef2c98c804ba503c6a707e38be4dfbb16683775f195b091252bf24693042fd52"
      , bcWeight = 136 * 10 ^ (15 :: Int)
      , bcBasePrice = 638_000
      , bcInverted = True
      }
  , BasketComponent
      { bcSymbol = "GBP/USD"
      , bcFeedSymbol = "GBP/USD"
      , bcFeedId = "0x84c2dde9633d93d1bcad84e7dc41c9d56578b7ec52fabedc1f335d673df0a7c1"
      , bcWeight = 119 * 10 ^ (15 :: Int)
      , bcBasePrice = 134_480_000
      , bcInverted = False
      }
  , BasketComponent
      { bcSymbol = "CAD/USD"
      , bcFeedSymbol = "USD/CAD"
      , bcFeedId = "0x3112b03a41c910ed446852aacf67118cb1bec67b2cd0b9a214c58cc0eaa2ecca"
      , bcWeight = 91 * 10 ^ (15 :: Int)
      , bcBasePrice = 72_880_000
      , bcInverted = True
      }
  , BasketComponent
      { bcSymbol = "SEK/USD"
      , bcFeedSymbol = "USD/SEK"
      , bcFeedId = "0x8ccb376aa871517e807358d4e3cf0bc7fe4950474dbe6c9ffc21ef64e43fc676"
      , bcWeight = 42 * 10 ^ (15 :: Int)
      , bcBasePrice = 10_860_000
      , bcInverted = True
      }
  , BasketComponent
      { bcSymbol = "CHF/USD"
      , bcFeedSymbol = "USD/CHF"
      , bcFeedId = "0x0b1e3297e69f162877b577b0d6a47a0d63b2392bc8499e6540da4187a63e28f8"
      , bcWeight = 36 * 10 ^ (15 :: Int)
      , bcBasePrice = 126_100_000
      , bcInverted = True
      }
  ]

-- The immutable v1 display transform is K - raw_price. Raw values at or above
-- K have no valid positive display-domain representation and must never enter
-- either the legacy snapshot store or the candle observation ledger.
basketDisplayPriceCap :: Integer
basketDisplayPriceCap = 200_000_000

normalizeFeedId :: Text -> Text
normalizeFeedId feedId =
  T.toLower $
    case T.stripPrefix "0x" feedId of
      Just stripped -> stripped
      Nothing -> feedId

computeBasketSnapshot :: [PythPricePoint] -> Either Text (Integer, [BasketComponentPrice])
computeBasketSnapshot points = do
  priced <- traverse computeComponent basketComponents
  let basketPrice = sum (map fst priced)
  if basketPrice <= 0 || basketPrice >= basketDisplayPriceCap
    then Left "Pyth basket price is outside the immutable v1 display domain"
    else pure (basketPrice, map snd priced)
  where
    pointsById :: Map Text PythPricePoint
    pointsById = Map.fromList [(normalizeFeedId (pppFeedId point), point) | point <- points]

    computeComponent :: BasketComponent -> Either Text (Integer, BasketComponentPrice)
    computeComponent component =
      case Map.lookup (normalizeFeedId (bcFeedId component)) pointsById of
        Nothing -> Left $ "missing Pyth feed " <> bcFeedId component
        Just point -> do
          normalized <-
            if bcInverted component
              then invertPythPrice (pppPrice point) (pppExponent point)
              else normalizePythPrice (pppPrice point) (pppExponent point)
          let contribution =
                (normalized * bcWeight component)
                  `div` (bcBasePrice component * 10_000_000_000)
              componentPrice =
                BasketComponentPrice
                  { bcpSymbol = bcSymbol component
                  , bcpFeedSymbol = bcFeedSymbol component
                  , bcpFeedId = bcFeedId component
                  , bcpPrice = normalized
                  , bcpRawPrice = pppPrice point
                  , bcpConfidence = pppConfidence point
                  , bcpExponent = pppExponent point
                  , bcpPublishTime = pppPublishTime point
                  , bcpInverted = bcInverted component
                  , bcpWeightBps = bcWeight component `div` 10 ^ (14 :: Int)
                  , bcpBasePrice = bcBasePrice component
                  }
          pure (contribution, componentPrice)

normalizePythPrice :: Integer -> Int -> Either Text Integer
normalizePythPrice price expo
  | price <= 0 = Left "Pyth price must be positive"
  | expo == -8 = Right price
  | expo > -8 = Right $ price * pow10 (expo + 8)
  | otherwise = Right $ price `div` pow10 (-8 - expo)

invertPythPrice :: Integer -> Int -> Either Text Integer
invertPythPrice price expo
  | price <= 0 = Left "Pyth price must be positive"
  | scaleExponent < 0 = Left "Pyth exponent is too large to invert"
  | otherwise =
      let scaledPrecision = pow10 scaleExponent
          scaledInverse = (scaledPrecision + price `div` 2) `div` price
       in Right $ scaledInverse `div` pow10 18
  where
    scaleExponent = 26 - expo

pow10 :: Int -> Integer
pow10 scaleExponent = 10 ^ scaleExponent
