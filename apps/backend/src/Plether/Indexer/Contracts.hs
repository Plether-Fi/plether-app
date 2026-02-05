module Plether.Indexer.Contracts
  ( EventSignature (..)
  , allEventSignatures
  , mintEvent
  , burnEvent
  , tokenExchangeEvent
  , zapMintEvent
  , zapBurnEvent
  , stakingDepositEvent
  , stakingWithdrawEvent
  , positionOpenedEvent
  , positionClosedEvent
  , keccak256Text
  ) where

import Crypto.Hash (Digest, Keccak_256, hash)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

data EventSignature = EventSignature
  { esName :: Text
  , esTopic :: ByteString
  , esTxType :: Text
  , esSide :: Maybe Text
  }
  deriving stock (Show)

keccak256 :: ByteString -> ByteString
keccak256 input = convert (hash input :: Digest Keccak_256)

keccak256Text :: Text -> ByteString
keccak256Text = keccak256 . TE.encodeUtf8

eventTopic :: Text -> ByteString
eventTopic = keccak256Text

mintEvent :: EventSignature
mintEvent = EventSignature
  { esName = "Mint"
  , esTopic = eventTopic "Mint(address,uint256,uint256)"
  , esTxType = "mint"
  , esSide = Nothing
  }

burnEvent :: EventSignature
burnEvent = EventSignature
  { esName = "Burn"
  , esTopic = eventTopic "Burn(address,uint256,uint256)"
  , esTxType = "burn"
  , esSide = Nothing
  }

tokenExchangeEvent :: EventSignature
tokenExchangeEvent = EventSignature
  { esName = "TokenExchange"
  , esTopic = eventTopic "TokenExchange(address,uint256,uint256,uint256,uint256)"
  , esTxType = "swap"
  , esSide = Nothing
  }

zapMintEvent :: EventSignature
zapMintEvent = EventSignature
  { esName = "ZapMint"
  , esTopic = eventTopic "ZapMint(address,uint256,uint256)"
  , esTxType = "zap_buy"
  , esSide = Just "bull"
  }

zapBurnEvent :: EventSignature
zapBurnEvent = EventSignature
  { esName = "ZapBurn"
  , esTopic = eventTopic "ZapBurn(address,uint256,uint256)"
  , esTxType = "zap_sell"
  , esSide = Just "bull"
  }

stakingDepositEvent :: EventSignature
stakingDepositEvent = EventSignature
  { esName = "Deposit"
  , esTopic = eventTopic "Deposit(address,address,uint256,uint256)"
  , esTxType = "stake"
  , esSide = Nothing
  }

stakingWithdrawEvent :: EventSignature
stakingWithdrawEvent = EventSignature
  { esName = "Withdraw"
  , esTopic = eventTopic "Withdraw(address,address,address,uint256,uint256)"
  , esTxType = "unstake"
  , esSide = Nothing
  }

positionOpenedEvent :: EventSignature
positionOpenedEvent = EventSignature
  { esName = "PositionOpened"
  , esTopic = eventTopic "PositionOpened(address,uint256,uint256,uint256,uint256)"
  , esTxType = "leverage_open"
  , esSide = Nothing
  }

positionClosedEvent :: EventSignature
positionClosedEvent = EventSignature
  { esName = "PositionClosed"
  , esTopic = eventTopic "PositionClosed(address,uint256,uint256)"
  , esTxType = "leverage_close"
  , esSide = Nothing
  }

allEventSignatures :: [EventSignature]
allEventSignatures =
  [ mintEvent
  , burnEvent
  , tokenExchangeEvent
  , zapMintEvent
  , zapBurnEvent
  , stakingDepositEvent
  , stakingWithdrawEvent
  , positionOpenedEvent
  , positionClosedEvent
  ]
