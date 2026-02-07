module Plether.Types.Quote
  ( MintQuote (..)
  , BurnQuote (..)
  , ZapQuote (..)
  , ZapInput (..)
  , ZapOutput (..)
  , ZapDirection (..)
  , TradeQuote (..)
  , TradeFrom (..)
  , LeverageQuote (..)
  , Side (..)
  ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)

data Side = Bear | Bull
  deriving stock (Show, Eq, Generic)

instance ToJSON Side where
  toJSON = \case
    Bear -> "bear"
    Bull -> "bull"

data ZapDirection = Buy | Sell
  deriving stock (Show, Eq, Generic)

instance ToJSON ZapDirection where
  toJSON = \case
    Buy -> "buy"
    Sell -> "sell"

data TradeFrom = FromUsdc | FromBear
  deriving stock (Show, Eq, Generic)

instance ToJSON TradeFrom where
  toJSON = \case
    FromUsdc -> "usdc"
    FromBear -> "bear"

data MintQuote = MintQuote
  { mintUsdcIn :: Integer
  , mintBearOut :: Integer
  , mintBullOut :: Integer
  , mintPricePerToken :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON MintQuote where
  toJSON MintQuote {..} =
    object
      [ "usdcIn" .= show mintUsdcIn
      , "bearOut" .= show mintBearOut
      , "bullOut" .= show mintBullOut
      , "pricePerToken" .= show mintPricePerToken
      ]

data BurnQuote = BurnQuote
  { burnPairIn :: Integer
  , burnUsdcOut :: Integer
  , burnBearIn :: Integer
  , burnBullIn :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON BurnQuote where
  toJSON BurnQuote {..} =
    object
      [ "pairIn" .= show burnPairIn
      , "usdcOut" .= show burnUsdcOut
      , "bearIn" .= show burnBearIn
      , "bullIn" .= show burnBullIn
      ]

data ZapInput = ZapInput
  { zapInToken :: Text
  , zapInAmount :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON ZapInput where
  toJSON ZapInput {..} =
    object
      [ "token" .= zapInToken
      , "amount" .= show zapInAmount
      ]

data ZapOutput = ZapOutput
  { zapOutToken :: Text
  , zapOutAmount :: Integer
  , zapOutMinAmount :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON ZapOutput where
  toJSON ZapOutput {..} =
    object
      [ "token" .= zapOutToken
      , "amount" .= show zapOutAmount
      , "minAmount" .= show zapOutMinAmount
      ]

data ZapQuote = ZapQuote
  { zapDirection :: ZapDirection
  , zapInput :: ZapInput
  , zapOutput :: ZapOutput
  , zapPriceImpact :: Integer
  , zapRoute :: [Text]
  }
  deriving stock (Show, Generic)

instance ToJSON ZapQuote where
  toJSON ZapQuote {..} =
    object
      [ "direction" .= zapDirection
      , "input" .= zapInput
      , "output" .= zapOutput
      , "priceImpact" .= show zapPriceImpact
      , "route" .= zapRoute
      ]

data TradeQuote = TradeQuote
  { tradeFrom :: TradeFrom
  , tradeTo :: TradeFrom
  , tradeAmountIn :: Integer
  , tradeAmountOut :: Integer
  , tradeMinAmountOut :: Integer
  , tradeSpotPrice :: Integer
  , tradePriceImpact :: Integer
  , tradeFee :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON TradeQuote where
  toJSON TradeQuote {..} =
    object
      [ "from" .= tradeFrom
      , "to" .= tradeTo
      , "amountIn" .= show tradeAmountIn
      , "amountOut" .= show tradeAmountOut
      , "minAmountOut" .= show tradeMinAmountOut
      , "spotPrice" .= show tradeSpotPrice
      , "priceImpact" .= show tradePriceImpact
      , "fee" .= show tradeFee
      ]

data LeverageQuote = LeverageQuote
  { levqSide :: Side
  , levqPrincipal :: Integer
  , levqLeverage :: Integer
  , levqPositionSize :: Integer
  , levqPositionSizeUsd :: Integer
  , levqDebt :: Integer
  , levqHealthFactor :: Integer
  , levqLiquidationPrice :: Integer
  , levqPriceImpact :: Integer
  , levqBorrowRate :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON LeverageQuote where
  toJSON LeverageQuote {..} =
    object
      [ "side" .= levqSide
      , "principal" .= show levqPrincipal
      , "leverage" .= show levqLeverage
      , "positionSize" .= show levqPositionSize
      , "positionSizeUsd" .= show levqPositionSizeUsd
      , "debt" .= show levqDebt
      , "healthFactor" .= show levqHealthFactor
      , "liquidationPrice" .= show levqLiquidationPrice
      , "priceImpact" .= show levqPriceImpact
      , "borrowRate" .= show levqBorrowRate
      ]
