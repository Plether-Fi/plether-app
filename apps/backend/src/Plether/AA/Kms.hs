module Plether.AA.Kms
  ( PaymasterSigner (..)
  , newKmsPaymasterSigner
  , parseKmsPublicKey
  , parseKmsDerSignature
  , normalizeLowS
  , assembleRecoverableSignature
  , canonicalRequest
  ) where

import Control.Exception (try)
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.Aeson (Value (..), eitherDecode, encode, object, (.=))
import qualified Data.Aeson.Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Word (Word8)
import Network.HTTP.Client
  ( BodyReader
  , HttpException
  , Manager
  , Request (..)
  , RequestBody (..)
  , brRead
  , parseRequest
  , responseBody
  , responseStatus
  , responseTimeoutMicro
  , withResponse
  )
import Network.HTTP.Types.Status (statusCode)
import Plether.Ethereum.Transaction
  ( ethereumAddressFromPublicKey
  , recoverSignerAddress
  )
import System.Environment (lookupEnv)

data PaymasterSigner = PaymasterSigner
  { psAddress :: Text
  , psSignDigest :: ByteString -> IO (Either Text ByteString)
  }

data AwsCredentials = AwsCredentials
  { acAccessKeyId :: ByteString
  , acSecretAccessKey :: ByteString
  , acSessionToken :: ByteString
  }

newKmsPaymasterSigner
  :: Manager
  -> Text
  -> Text
  -> IO (Either Text PaymasterSigner)
newKmsPaymasterSigner manager keyId configuredAddress = do
  publicKeyResult <- callKms manager "TrentService.GetPublicKey" $
    object ["KeyId" .= keyId]
  case publicKeyResult >>= responseBase64 "PublicKey" >>= parseKmsPublicKey of
    Left err -> pure $ Left err
    Right publicKey ->
      case ethereumAddressFromPublicKey publicKey of
        Left err -> pure $ Left err
        Right derivedAddress
          | T.toLower derivedAddress /= T.toLower configuredAddress ->
              pure $ Left "KMS public key does not match AA_PAYMASTER_SIGNER_ADDRESS"
          | otherwise ->
              pure $
                Right $
                  PaymasterSigner
                    { psAddress = T.toLower derivedAddress
                    , psSignDigest = signWithKms manager keyId derivedAddress
                    }

signWithKms :: Manager -> Text -> Text -> ByteString -> IO (Either Text ByteString)
signWithKms manager keyId expectedAddress digest
  | BS.length digest /= 32 = pure $ Left "KMS signing digest must be exactly 32 bytes"
  | otherwise = do
      response <- callKms manager "TrentService.Sign" $
        object
          [ "KeyId" .= keyId
          , "Message" .= TE.decodeUtf8 (B64.encode digest)
          , "MessageType" .= ("DIGEST" :: Text)
          , "SigningAlgorithm" .= ("ECDSA_SHA_256" :: Text)
          ]
      case response >>= responseBase64 "Signature" >>= parseKmsDerSignature of
        Left err -> pure $ Left err
        Right (rawR, rawS) -> do
          let r = integerToFixed 32 rawR
              s = integerToFixed 32 $ normalizeLowS rawS
              compact = r <> s
          parityResults <- traverse (recoverSignerAddress digest compact) [0, 1]
          let matches =
                [ parity
                | (parity, Right recovered) <- zip [0, 1] parityResults
                , T.toLower recovered == T.toLower expectedAddress
                ]
          pure $ case matches of
            [parity] -> assembleRecoverableSignature compact parity
            _ -> Left "KMS signature could not be uniquely recovered to the attested signer"

assembleRecoverableSignature :: ByteString -> Int -> Either Text ByteString
assembleRecoverableSignature compact parity
  | BS.length compact /= 64 = Left "recoverable signature must contain exactly 64 compact bytes"
  | parity `notElem` [0, 1] = Left "recoverable signature parity must be zero or one"
  | otherwise = Right $ compact <> BS.singleton (fromIntegral $ 27 + parity)

-- | Strictly parse the fixed SubjectPublicKeyInfo encoding returned by KMS for
-- an ECC_SECG_P256K1 key.  Rejecting any alternate algorithm/curve keeps key
-- substitution from being silently accepted.
parseKmsPublicKey :: ByteString -> Either Text ByteString
parseKmsPublicKey der
  | BS.length der /= BS.length secp256k1SpkiPrefix + 65 =
      Left "KMS public key has an unexpected DER length"
  | BS.take (BS.length secp256k1SpkiPrefix) der /= secp256k1SpkiPrefix =
      Left "KMS public key is not a canonical secp256k1 SubjectPublicKeyInfo"
  | BS.index der (BS.length secp256k1SpkiPrefix) /= 0x04 =
      Left "KMS public key is not uncompressed"
  | otherwise = Right $ BS.drop (BS.length secp256k1SpkiPrefix) der

-- | Parse a canonical ASN.1 DER ECDSA signature and return positive r/s.
parseKmsDerSignature :: ByteString -> Either Text (Integer, Integer)
parseKmsDerSignature der = do
  (sequenceBytes, rest) <- derElement 0x30 der
  unlessEmpty rest "ECDSA DER signature has trailing bytes"
  (rBytes, afterR) <- derElement 0x02 sequenceBytes
  (sBytes, afterS) <- derElement 0x02 afterR
  unlessEmpty afterS "ECDSA DER sequence has trailing fields"
  r <- positiveInteger "r" rBytes
  s <- positiveInteger "s" sBytes
  if r >= curveOrder || s >= curveOrder
    then Left "ECDSA DER scalar is outside the secp256k1 group"
    else Right (r, s)

normalizeLowS :: Integer -> Integer
normalizeLowS value
  | value > curveOrder `div` 2 = curveOrder - value
  | otherwise = value

callKms :: Manager -> Text -> Value -> IO (Either Text Value)
callKms manager target payload = do
  region <- fmap (fmap T.pack) $ lookupEnv "AWS_REGION"
  fallbackRegion <- fmap (fmap T.pack) $ lookupEnv "AWS_DEFAULT_REGION"
  case region <|> fallbackRegion of
    Nothing -> pure $ Left "AWS_REGION is required for KMS signing"
    Just selectedRegion
      | not (validRegion selectedRegion) -> pure $ Left "AWS_REGION contains unsupported characters"
      | otherwise -> do
          credentials <- loadEcsCredentials manager
          case credentials of
            Left err -> pure $ Left err
            Right creds -> do
              now <- getCurrentTime
              let body = LBS.toStrict $ encode payload
                  host = "kms." <> TE.encodeUtf8 selectedRegion <> ".amazonaws.com"
                  (authorization, amzDate) =
                    authorizationHeader now selectedRegion host target body creds
              requestResult <- try @HttpException $ do
                base <- parseRequest $ "https://" <> BSC.unpack host <> "/"
                let request =
                      base
                        { method = "POST"
                        , requestHeaders =
                            [ ("Content-Type", "application/x-amz-json-1.1")
                            , ("X-Amz-Date", amzDate)
                            , ("X-Amz-Security-Token", acSessionToken creds)
                            , ("X-Amz-Target", TE.encodeUtf8 target)
                            , ("Authorization", authorization)
                            ]
                        , requestBody = RequestBodyBS body
                        , responseTimeout = responseTimeoutMicro 10_000_000
                        , redirectCount = 0
                        , checkResponse = \_ _ -> pure ()
                        }
                withResponse request manager $ \response -> do
                  bounded <- readBoundedBody maxKmsResponseBytes $ responseBody response
                  pure (statusCode $ responseStatus response, bounded)
              pure $ case requestResult of
                Left _ -> Left "AWS KMS request failed"
                Right (_, Left err) -> Left err
                Right (code, Right body)
                  | code < 200 || code >= 300 -> Left $ kmsError body
                  | otherwise ->
                      case eitherDecode $ LBS.fromStrict body of
                        Left _ -> Left "AWS KMS returned invalid JSON"
                        Right value -> Right value

loadEcsCredentials :: Manager -> IO (Either Text AwsCredentials)
loadEcsCredentials manager = do
  relativeUri <- lookupEnv "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"
  case relativeUri of
    Nothing -> pure $ Left "ECS task credentials are unavailable"
    Just uri
      | not (validRelativeCredentialsUri uri) ->
          pure $ Left "ECS credentials relative URI is invalid"
      | otherwise -> do
          result <- try @HttpException $ do
            base <- parseRequest $ "http://169.254.170.2" <> uri
            let request =
                  base
                    { method = "GET"
                    , responseTimeout = responseTimeoutMicro 2_000_000
                    , redirectCount = 0
                    , checkResponse = \_ _ -> pure ()
                    }
            withResponse request manager $ \response -> do
              bounded <- readBoundedBody maxMetadataResponseBytes $ responseBody response
              pure (statusCode $ responseStatus response, bounded)
          pure $ case result of
            Left _ -> Left "ECS task credential request failed"
            Right (_, Left err) -> Left err
            Right (code, Right body)
              | code /= 200 -> Left "ECS task credential endpoint rejected the request"
              | otherwise -> parseCredentials body

parseCredentials :: ByteString -> Either Text AwsCredentials
parseCredentials body = do
  value <-
    case eitherDecode $ LBS.fromStrict body of
      Left _ -> Left "ECS task credential response is invalid JSON"
      Right parsed -> Right parsed
  case value of
    Object objectValue -> do
      access <- requiredText objectValue "AccessKeyId"
      secret <- requiredText objectValue "SecretAccessKey"
      token <- requiredText objectValue "Token"
      if any T.null [access, secret, token]
        then Left "ECS task credential response contains an empty credential"
        else
          Right $
            AwsCredentials
              (TE.encodeUtf8 access)
              (TE.encodeUtf8 secret)
              (TE.encodeUtf8 token)
    _ -> Left "ECS task credential response must be an object"

authorizationHeader
  :: UTCTime
  -> Text
  -> ByteString
  -> Text
  -> ByteString
  -> AwsCredentials
  -> (ByteString, ByteString)
authorizationHeader now region host target body credentials =
  ( "AWS4-HMAC-SHA256 Credential="
      <> acAccessKeyId credentials
      <> "/"
      <> scope
      <> ", SignedHeaders="
      <> signedHeaders
      <> ", Signature="
      <> hex signature
  , amzDate
  )
 where
  amzDate = BSC.pack $ formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" now
  dateStamp = BSC.pack $ formatTime defaultTimeLocale "%Y%m%d" now
  regionBytes = TE.encodeUtf8 region
  scope = dateStamp <> "/" <> regionBytes <> "/kms/aws4_request"
  request =
    canonicalRequest
      host
      amzDate
      (acSessionToken credentials)
      (TE.encodeUtf8 target)
      body
  stringToSign =
    "AWS4-HMAC-SHA256\n"
      <> amzDate
      <> "\n"
      <> scope
      <> "\n"
      <> hex (sha256 request)
  dateKey = hmacSha256 ("AWS4" <> acSecretAccessKey credentials) dateStamp
  regionKey = hmacSha256 dateKey regionBytes
  serviceKey = hmacSha256 regionKey "kms"
  signingKey = hmacSha256 serviceKey "aws4_request"
  signature = hmacSha256 signingKey stringToSign

canonicalRequest
  :: ByteString
  -> ByteString
  -> ByteString
  -> ByteString
  -> ByteString
  -> ByteString
canonicalRequest host amzDate sessionToken target body =
  "POST\n/\n\n"
    <> "content-type:application/x-amz-json-1.1\n"
    <> "host:" <> host <> "\n"
    <> "x-amz-date:" <> amzDate <> "\n"
    <> "x-amz-security-token:" <> sessionToken <> "\n"
    <> "x-amz-target:" <> target <> "\n\n"
    <> signedHeaders
    <> "\n"
    <> hex (sha256 body)

signedHeaders :: ByteString
signedHeaders = "content-type;host;x-amz-date;x-amz-security-token;x-amz-target"

readBoundedBody :: Int -> BodyReader -> IO (Either Text ByteString)
readBoundedBody limit = go 0 []
 where
  go total chunks reader = do
    chunk <- brRead reader
    if BS.null chunk
      then pure $ Right $ BS.concat $ reverse chunks
      else
        let next = total + BS.length chunk
         in if next > limit
              then pure $ Left "AWS response exceeded the configured size limit"
              else go next (chunk : chunks) reader

responseBase64 :: Text -> Value -> Either Text ByteString
responseBase64 fieldName = \case
  Object objectValue -> do
    encoded <- requiredText objectValue fieldName
    case B64.decode $ TE.encodeUtf8 encoded of
      Left _ -> Left $ "AWS KMS " <> fieldName <> " is not canonical base64"
      Right bytes -> Right bytes
  _ -> Left "AWS KMS response must be an object"

requiredText :: KM.KeyMap Value -> Text -> Either Text Text
requiredText objectValue fieldName =
  case KM.lookup (fromStringKey fieldName) objectValue of
    Just (String value) -> Right value
    _ -> Left $ "AWS response is missing " <> fieldName

fromStringKey :: Text -> Data.Aeson.Key.Key
fromStringKey = Data.Aeson.Key.fromText

kmsError :: ByteString -> Text
kmsError body =
  case eitherDecode (LBS.fromStrict body) of
    Right (Object objectValue) ->
      case KM.lookup "message" objectValue <|> KM.lookup "Message" objectValue of
        Just (String message) | not (T.null message) -> "AWS KMS rejected the request: " <> T.take 256 message
        _ -> "AWS KMS rejected the request"
    _ -> "AWS KMS rejected the request"

derElement :: Word8 -> ByteString -> Either Text (ByteString, ByteString)
derElement expectedTag input = do
  if BS.length input < 2 || BS.head input /= expectedTag
    then Left "ECDSA DER element has an unexpected tag"
    else do
      (lengthValue, headerBytes) <- derLength $ BS.drop 1 input
      let contentStart = 1 + headerBytes
          contentEnd = contentStart + lengthValue
      if contentEnd > BS.length input
        then Left "ECDSA DER element is truncated"
        else Right (BS.take lengthValue $ BS.drop contentStart input, BS.drop contentEnd input)

derLength :: ByteString -> Either Text (Int, Int)
derLength input
  | BS.null input = Left "ECDSA DER length is missing"
  | first < 0x80 = Right (fromIntegral first, 1)
  | first == 0x81 =
      if BS.length input >= 2 && BS.index input 1 >= 0x80
        then Right (fromIntegral $ BS.index input 1, 2)
        else Left "ECDSA DER length is not canonical"
  | otherwise = Left "ECDSA DER length encoding is unsupported"
 where
  first = BS.head input

positiveInteger :: Text -> ByteString -> Either Text Integer
positiveInteger name bytes
  | BS.null bytes = Left $ "ECDSA DER " <> name <> " is empty"
  | BS.head bytes >= 0x80 = Left $ "ECDSA DER " <> name <> " is negative"
  | BS.length bytes > 33 = Left $ "ECDSA DER " <> name <> " is too large"
  | BS.head bytes == 0 && (BS.length bytes == 1 || BS.index bytes 1 < 0x80) =
      Left $ "ECDSA DER " <> name <> " has non-canonical padding"
  | value == 0 = Left $ "ECDSA DER " <> name <> " is zero"
  | otherwise = Right value
 where
  value = bytesToInteger $ if BS.head bytes == 0 then BS.tail bytes else bytes

unlessEmpty :: ByteString -> Text -> Either Text ()
unlessEmpty bytes message = if BS.null bytes then Right () else Left message

validRelativeCredentialsUri :: String -> Bool
validRelativeCredentialsUri uri =
  not (null uri)
    && length uri <= 2048
    && head uri == '/'
    && not (".." `T.isInfixOf` T.pack uri)
    && all validUriChar uri
 where
  validUriChar char =
    (char >= 'a' && char <= 'z')
      || (char >= 'A' && char <= 'Z')
      || (char >= '0' && char <= '9')
      || char `elem` ("/-_" :: String)

validRegion :: Text -> Bool
validRegion region =
  not (T.null region)
    && T.length region <= 64
    && T.all (\char -> isAsciiAlphaNumeric char || char == '-') region
 where
  isAsciiAlphaNumeric char =
    (char >= 'a' && char <= 'z')
      || (char >= 'A' && char <= 'Z')
      || (char >= '0' && char <= '9')

sha256 :: ByteString -> ByteString
sha256 bytes = convert (hash bytes :: Digest SHA256)

hmacSha256 :: ByteString -> ByteString -> ByteString
hmacSha256 key message = convert (hmac key message :: HMAC SHA256)

hex :: ByteString -> ByteString
hex = B16.encode

integerToFixed :: Int -> Integer -> ByteString
integerToFixed width value = BS.replicate (width - BS.length raw) 0 <> raw
 where
  raw
    | value == 0 = BS.empty
    | otherwise = BS.reverse $ BS.unfoldr step value
  step 0 = Nothing
  step remaining = Just (fromIntegral $ remaining `mod` 256, remaining `div` 256)

bytesToInteger :: ByteString -> Integer
bytesToInteger = BS.foldl' (\total byte -> total * 256 + fromIntegral byte) 0

secp256k1SpkiPrefix :: ByteString
secp256k1SpkiPrefix =
  decodeHex "3056301006072a8648ce3d020106052b8104000a034200"

decodeHex :: ByteString -> ByteString
decodeHex value = either (const BS.empty) id $ B16.decode value

curveOrder :: Integer
curveOrder = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141

maxMetadataResponseBytes :: Int
maxMetadataResponseBytes = 16 * 1024

maxKmsResponseBytes :: Int
maxKmsResponseBytes = 64 * 1024

infixr 3 <|>
(<|>) :: Maybe a -> Maybe a -> Maybe a
Just value <|> _ = Just value
Nothing <|> other = other
