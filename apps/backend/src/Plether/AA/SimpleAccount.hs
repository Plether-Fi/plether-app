module Plether.AA.SimpleAccount
  ( simpleAccountFactory
  , simpleAccountImplementation
  , deriveTradingAccountAddress
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.Ethereum.Abi
  ( encodeAddress
  , encodeCall
  , encodeUint256
  , keccak256
  )
import Plether.Utils.Address (isValidAddress)

simpleAccountFactory :: Text
simpleAccountFactory = "0x13e9ed32155810fdbd067d4522c492d6f68e5944"

simpleAccountImplementation :: Text
simpleAccountImplementation = "0x28426d752372d68d34340bd94390950dce3c9ec3"

-- ERC1967Proxy creation code embedded in the canonical v0.8.0
-- SimpleAccountFactory deployment. It was compiled with Solidity 0.8.28,
-- Cancun, via-IR, 1,000,000 optimizer runs, and OpenZeppelin 5.1.0.
proxyCreationCodeHex :: Text
proxyCreationCodeHex =
  T.concat
  [ "60806040526102a88038038061001481610168565b92833981016040828203126101645781516001600160a01b0381169290"
  , "9190838303610164576020810151906001600160401b03821161016457019281601f8501121561016457835161006e610069"
  , "826101a1565b610168565b9481865260208601936020838301011161016457815f926020809301865e86010152823b156101"
  , "52577f360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc80546001600160a01b031916821790"
  , "557fbc7cd75a20ee27fd9adebab32041f755214dbc6bffa90cc0225b39da2e5c2d3b5f80a282511561013a575f8091610122"
  , "945190845af43d15610132573d91610113610069846101a1565b9283523d5f602085013e6101bc565b505b604051608d9081"
  , "61021b8239f35b6060916101bc565b50505034156101245763b398979f60e01b5f5260045ffd5b634c9c8ce360e01b5f5260"
  , "045260245ffd5b5f80fd5b6040519190601f01601f191682016001600160401b0381118382101761018d57604052565b634e"
  , "487b7160e01b5f52604160045260245ffd5b6001600160401b03811161018d57601f01601f191660200190565b906101e057"
  , "508051156101d157805190602001fd5b63d6bda27560e01b5f5260045ffd5b81511580610211575b6101f1575090565b6399"
  , "96b31560e01b5f9081526001600160a01b0391909116600452602490fd5b50803b156101e956fe60806040525f8073ffffff"
  , "ffffffffffffffffffffffffffffffffff7f360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc"
  , "5416368280378136915af43d5f803e156053573d5ff35b3d5ffdfea264697066735822122012ef914fc5c0fe0eff95047a7f"
  , "10780a737a1ca4f30269b985bcf38a18e4d23464736f6c634300081c0033"
  ]

deriveTradingAccountAddress :: Text -> Either Text Text
deriveTradingAccountAddress owner
  | not $ isValidAddress owner = Left "OWNER_WALLET must be a valid Ethereum address"
  | otherwise = do
      factory <- decodeAddressBytes "SimpleAccount factory" simpleAccountFactory
      implementation <- decodeAddressBytes "SimpleAccount implementation" simpleAccountImplementation
      proxyCreationCode <- decodeHex "pinned ERC1967Proxy creation code" proxyCreationCodeHex
      if BS.length proxyCreationCode /= 680
        then Left "pinned ERC1967Proxy creation code must be exactly 680 bytes"
        else do
          let initializer =
                encodeCall "initialize(address)" [encodeAddress owner]
              constructorArguments =
                BS.replicate 12 0
                  <> implementation
                  <> encodeUint256 64
                  <> encodeUint256 (fromIntegral $ BS.length initializer)
                  <> padRightToWord initializer
              initCodeHash = keccak256 $ proxyCreationCode <> constructorArguments
              create2Digest =
                keccak256 $
                  BS.singleton 0xff
                    <> factory
                    <> encodeUint256 0
                    <> initCodeHash
          pure $ "0x" <> TE.decodeUtf8 (B16.encode $ BS.drop 12 create2Digest)

decodeAddressBytes :: Text -> Text -> Either Text ByteString
decodeAddressBytes label value
  | not $ isValidAddress value = Left $ label <> " is not a valid Ethereum address"
  | otherwise = decodeHex label $ T.drop 2 value

decodeHex :: Text -> Text -> Either Text ByteString
decodeHex label value =
  case B16.decode $ TE.encodeUtf8 $ T.toLower value of
    Left _ -> Left $ label <> " is not valid hex"
    Right bytes -> Right bytes

padRightToWord :: ByteString -> ByteString
padRightToWord value =
  let remainder = BS.length value `mod` 32
      padding = if remainder == 0 then 0 else 32 - remainder
   in value <> BS.replicate padding 0
