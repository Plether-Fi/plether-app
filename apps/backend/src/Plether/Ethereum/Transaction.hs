module Plether.Ethereum.Transaction
  ( Tx1559 (..)
  , SignedTransaction (..)
  , deriveAddress
  , signTransaction
  , rawTransactionHash
  , applyBpsBuffer
  , sameNonceReplacementFees
  , normalizePrivateKey
  ) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Foreign
  ( FunPtr
  , Ptr
  , alloca
  , allocaBytes
  , castPtr
  , nullFunPtr
  , nullPtr
  , peek
  , poke
  )
import Foreign.C (CInt (..), CSize (..), CUInt (..))
import Plether.Ethereum.Abi (keccak256)
import Plether.Ethereum.Rlp (Rlp (..), encodeRlp, rlpBytes, rlpInteger, rlpList)

data SecpCtx
data SecpPubKey
data SecpRecoverableSig

type NonceFun a =
  Ptr Word8 ->
  Ptr Word8 ->
  Ptr Word8 ->
  Ptr Word8 ->
  Ptr a ->
  CInt ->
  IO CInt

data Tx1559 = Tx1559
  { txChainId :: Integer
  , txNonce :: Integer
  , txMaxPriorityFeePerGas :: Integer
  , txMaxFeePerGas :: Integer
  , txGasLimit :: Integer
  , txTo :: Text
  , txValue :: Integer
  , txData :: ByteString
  }
  deriving stock (Show, Eq)

data SignedTransaction = SignedTransaction
  { signedRawTransaction :: ByteString
  , signedTransactionHash :: Text
  , signedFrom :: Text
  }
  deriving stock (Show, Eq)

deriveAddress :: Text -> IO (Either Text Text)
deriveAddress privateKeyText = do
  case parsePrivateKey privateKeyText of
    Left err -> pure $ Left err
    Right privateKey -> do
      pubResult <- derivePublicKey privateKey
      pure $
        fmap
          ( \pub ->
              let pubBody = if BS.length pub == 65 && BS.head pub == 0x04 then BS.drop 1 pub else pub
                  digest = keccak256 pubBody
               in "0x" <> TE.decodeUtf8 (B16.encode $ BS.drop (BS.length digest - 20) digest)
          )
          pubResult

signTransaction :: Text -> Tx1559 -> IO (Either Text SignedTransaction)
signTransaction privateKeyText tx = do
  case parsePrivateKey privateKeyText of
    Left err -> pure $ Left err
    Right privateKey -> do
      fromResult <- deriveAddress privateKeyText
      signatureResult <- signRecoverable privateKey $ keccak256 $ BS.cons 0x02 $ unsignedPayload tx
      pure $ do
        from <- fromResult
        (compactSig, recId) <- signatureResult
        let r = bytesToInteger $ BS.take 32 compactSig
            s = bytesToInteger $ BS.drop 32 compactSig
            yParity = fromIntegral (recId `mod` 2) :: Integer
            raw = BS.cons 0x02 $ signedPayload tx yParity r s
            txHash = rawTransactionHash raw
        Right
          SignedTransaction
            { signedRawTransaction = raw
            , signedTransactionHash = txHash
            , signedFrom = from
          }

-- | Deterministic EIP-2718 transaction identifier for already-signed bytes.
-- This is available before broadcast so callers can persist intent first and
-- verify that the node returns the locally derived hash.
rawTransactionHash :: ByteString -> Text
rawTransactionHash raw = "0x" <> TE.decodeUtf8 (B16.encode $ keccak256 raw)

-- | Apply a basis-point buffer with ceiling division.
applyBpsBuffer :: Integer -> Integer -> Integer
applyBpsBuffer value bufferBps =
  ((value * (10_000 + bufferBps)) + 9_999) `div` 10_000

-- | Price a same-nonce EIP-1559 replacement. Both fee fields beat the prior
-- transaction by at least 12.5%, while current buffered network quotes may
-- require a larger bump. The max fee is always at least the priority fee.
sameNonceReplacementFees
  :: Integer -- ^ Current fee buffer in basis points.
  -> Integer -- ^ Current gas-price quote.
  -> Integer -- ^ Current priority-fee quote.
  -> Integer -- ^ Previous max-priority fee.
  -> Integer -- ^ Previous max fee.
  -> (Integer, Integer)
sameNonceReplacementFees feeBufferBps gasPrice priorityBase oldPriorityFee oldMaxFee =
  (replacementPriorityFee, replacementMaxFee)
 where
  currentPriorityFee = applyBpsBuffer priorityBase feeBufferBps
  currentMaxFee = max currentPriorityFee $ applyBpsBuffer (max gasPrice priorityBase) feeBufferBps
  replacementPriorityFee = max currentPriorityFee $ applyBpsBuffer oldPriorityFee 1_250
  replacementMaxFee =
    max replacementPriorityFee $
      max currentMaxFee (applyBpsBuffer oldMaxFee 1_250)

normalizePrivateKey :: Text -> Text
normalizePrivateKey value =
  T.toLower $
    case T.stripPrefix "0x" (T.strip value) of
      Just stripped -> stripped
      Nothing -> T.strip value

parsePrivateKey :: Text -> Either Text ByteString
parsePrivateKey privateKeyText = do
  bytes <- decodeHexText $ normalizePrivateKey privateKeyText
  if BS.length bytes == 32
    then Right bytes
    else Left "Invalid keeper private key"

derivePublicKey :: ByteString -> IO (Either Text ByteString)
derivePublicKey privateKey =
  withSecpContext $ \ctx ->
    allocaBytes 64 $ \pubKeyPtr ->
      BS.useAsCString privateKey $ \privateKeyPtr -> do
        created <- c_secp256k1_ec_pubkey_create ctx pubKeyPtr (castPtr privateKeyPtr)
        if not (isSuccess created)
          then pure $ Left "Invalid keeper private key"
          else
            alloca $ \lenPtr ->
              allocaBytes 65 $ \outPtr -> do
                poke lenPtr 65
                serialized <-
                  c_secp256k1_ec_pubkey_serialize
                    ctx
                    (castPtr outPtr)
                    lenPtr
                    pubKeyPtr
                    secp256k1EcUncompressed
                if not (isSuccess serialized)
                  then pure $ Left "Could not serialize secp256k1 public key"
                  else do
                    len <- peek lenPtr
                    Right <$> BS.packCStringLen (outPtr, fromIntegral len)

signRecoverable :: ByteString -> ByteString -> IO (Either Text (ByteString, Int))
signRecoverable privateKey digest
  | BS.length digest /= 32 = pure $ Left "Transaction signing digest must be 32 bytes"
  | otherwise =
      withSecpContext $ \ctx ->
        allocaBytes 65 $ \sigPtr ->
          BS.useAsCString digest $ \msgPtr ->
            BS.useAsCString privateKey $ \privateKeyPtr -> do
              signed <-
                c_secp256k1_ecdsa_sign_recoverable
                  ctx
                  sigPtr
                  (castPtr msgPtr)
                  (castPtr privateKeyPtr)
                  nullFunPtr
                  nullPtr
              if not (isSuccess signed)
                then pure $ Left "Could not sign transaction"
                else
                  allocaBytes 64 $ \compactPtr ->
                    alloca $ \recIdPtr -> do
                      serialized <-
                        c_secp256k1_ecdsa_recoverable_signature_serialize_compact
                          ctx
                          (castPtr compactPtr)
                          recIdPtr
                          sigPtr
                      if not (isSuccess serialized)
                        then pure $ Left "Could not serialize secp256k1 signature"
                        else do
                          compactSig <- BS.packCStringLen (compactPtr, 64)
                          recId <- peek recIdPtr
                          pure $ Right (compactSig, fromIntegral recId)

withSecpContext :: (Ptr SecpCtx -> IO a) -> IO a
withSecpContext =
  bracket
    (c_secp256k1_context_create secp256k1ContextSignVerify)
    c_secp256k1_context_destroy

isSuccess :: CInt -> Bool
isSuccess 1 = True
isSuccess _ = False

secp256k1ContextSignVerify :: CUInt
secp256k1ContextSignVerify = 0x0301

secp256k1EcUncompressed :: CUInt
secp256k1EcUncompressed = 0x0002

unsignedPayload :: Tx1559 -> ByteString
unsignedPayload tx =
  encodeRlp $ rlpList $
    txFields tx <> [RlpList []]

signedPayload :: Tx1559 -> Integer -> Integer -> Integer -> ByteString
signedPayload tx yParity r s =
  encodeRlp $ rlpList $
    txFields tx <> [RlpList [], rlpInteger yParity, rlpInteger r, rlpInteger s]

txFields :: Tx1559 -> [Rlp]
txFields Tx1559 {..} =
  [ rlpInteger txChainId
  , rlpInteger txNonce
  , rlpInteger txMaxPriorityFeePerGas
  , rlpInteger txMaxFeePerGas
  , rlpInteger txGasLimit
  , rlpBytes $ decodeAddressBytes txTo
  , rlpInteger txValue
  , rlpBytes txData
  ]

decodeAddressBytes :: Text -> ByteString
decodeAddressBytes address =
  either (const BS.empty) id $
    decodeHexText $
      case T.stripPrefix "0x" (T.strip address) of
        Just stripped -> stripped
        Nothing -> T.strip address

decodeHexText :: Text -> Either Text ByteString
decodeHexText txt =
  case B16.decode (TE.encodeUtf8 $ T.toLower txt) of
    Right bytes -> Right bytes
    Left err -> Left $ "Invalid hex: " <> T.pack err

bytesToInteger :: ByteString -> Integer
bytesToInteger = BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) 0

foreign import ccall safe "secp256k1.h secp256k1_context_create"
  c_secp256k1_context_create :: CUInt -> IO (Ptr SecpCtx)

foreign import ccall safe "secp256k1.h secp256k1_context_destroy"
  c_secp256k1_context_destroy :: Ptr SecpCtx -> IO ()

foreign import ccall safe "secp256k1.h secp256k1_ec_pubkey_create"
  c_secp256k1_ec_pubkey_create ::
    Ptr SecpCtx ->
    Ptr SecpPubKey ->
    Ptr Word8 ->
    IO CInt

foreign import ccall safe "secp256k1.h secp256k1_ec_pubkey_serialize"
  c_secp256k1_ec_pubkey_serialize ::
    Ptr SecpCtx ->
    Ptr Word8 ->
    Ptr CSize ->
    Ptr SecpPubKey ->
    CUInt ->
    IO CInt

foreign import ccall safe "secp256k1_recovery.h secp256k1_ecdsa_sign_recoverable"
  c_secp256k1_ecdsa_sign_recoverable ::
    Ptr SecpCtx ->
    Ptr SecpRecoverableSig ->
    Ptr Word8 ->
    Ptr Word8 ->
    FunPtr (NonceFun ()) ->
    Ptr () ->
    IO CInt

foreign import ccall safe "secp256k1_recovery.h secp256k1_ecdsa_recoverable_signature_serialize_compact"
  c_secp256k1_ecdsa_recoverable_signature_serialize_compact ::
    Ptr SecpCtx ->
    Ptr Word8 ->
    Ptr CInt ->
    Ptr SecpRecoverableSig ->
    IO CInt
