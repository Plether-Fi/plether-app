module Plether.Infra.AwsHealth
  ( AwsControlPlaneSnapshot (..)
  , AwsEcsServiceStatus (..)
  , AwsRdsStatus (..)
  , AwsEc2Status (..)
  , fetchAwsControlPlaneSnapshot
  , parseEcsServicesJson
  , parseRdsStatusXml
  , parseEc2StatusXml
  , normalizeServiceKey
  , chunkList
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (Concurrently (..), mapConcurrently, runConcurrently)
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.Aeson (Value (..), eitherDecode, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import qualified Data.Vector as V
import Network.HTTP.Client
  ( BodyReader
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
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (statusCode)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data AwsEcsServiceStatus = AwsEcsServiceStatus
  { aessServiceKey :: Text
  , aessStatus :: Text
  , aessDesiredCount :: Int
  , aessRunningCount :: Int
  , aessPendingCount :: Int
  , aessRolloutState :: Maybe Text
  }
  deriving stock (Eq, Show)

data AwsRdsStatus = AwsRdsStatus
  { arsStatus :: Text
  , arsEngineVersion :: Maybe Text
  , arsMultiAz :: Maybe Bool
  , arsStorageEncrypted :: Maybe Bool
  , arsAllocatedStorageGb :: Maybe Int
  }
  deriving stock (Eq, Show)

data AwsEc2Status = AwsEc2Status
  { aecInstanceState :: Text
  , aecSystemStatus :: Maybe Text
  , aecInstanceStatus :: Maybe Text
  }
  deriving stock (Eq, Show)

data AwsControlPlaneSnapshot = AwsControlPlaneSnapshot
  { acpsAvailable :: Bool
  , acpsEcsServices :: Map.Map Text AwsEcsServiceStatus
  , acpsRds :: Maybe AwsRdsStatus
  , acpsRpcEc2 :: Maybe AwsEc2Status
  }
  deriving stock (Eq, Show)

emptyAwsSnapshot :: AwsControlPlaneSnapshot
emptyAwsSnapshot = AwsControlPlaneSnapshot False Map.empty Nothing Nothing

data AwsTempCredentials = AwsTempCredentials
  { atcAccessKeyId :: ByteString
  , atcSecretAccessKey :: ByteString
  , atcSessionToken :: Maybe ByteString
  }

fetchAwsControlPlaneSnapshot :: Manager -> IO AwsControlPlaneSnapshot
fetchAwsControlPlaneSnapshot manager = do
  mRegion <- (<|>) <$> lookupEnvText "AWS_REGION" <*> lookupEnvText "AWS_DEFAULT_REGION"
  mCluster <- lookupEnvText "INSIGHTS_AWS_ECS_CLUSTER"
  mServices <- lookupEnvText "INSIGHTS_AWS_ECS_SERVICES"
  mRdsId <- lookupEnvText "INSIGHTS_AWS_RDS_IDENTIFIER"
  mRpcInstId <- lookupEnvText "INSIGHTS_AWS_RPC_INSTANCE_ID"
  let serviceNames =
        maybe [] (filter (not . T.null) . map T.strip . T.splitOn ",") mServices
  case mRegion of
    Nothing -> pure emptyAwsSnapshot
    Just region
      | not (validRegion region) -> pure emptyAwsSnapshot
      | otherwise -> do
          mCreds <- timeout 2000000 $ loadTaskCredentials manager
          case mCreds of
            Just (Just creds) -> do
              (ecsList, mRds, mEc2) <-
                runConcurrently $
                  (,,)
                    <$> Concurrently (fetchEcsServices manager creds region mCluster serviceNames)
                    <*> Concurrently (fetchRdsStatus manager creds region mRdsId)
                    <*> Concurrently (fetchEc2Status manager creds region mRpcInstId)
              let ecsMap = Map.fromList [(aessServiceKey s, s) | s <- ecsList]
                  anyData = not (Map.null ecsMap) || maybe False (const True) mRds || maybe False (const True) mEc2
              pure $ AwsControlPlaneSnapshot anyData ecsMap mRds mEc2
            _ -> pure emptyAwsSnapshot

fetchEcsServices
  :: Manager
  -> AwsTempCredentials
  -> Text
  -> Maybe Text
  -> [Text]
  -> IO [AwsEcsServiceStatus]
fetchEcsServices _ _ _ Nothing _ = pure []
fetchEcsServices _ _ _ _ [] = pure []
fetchEcsServices manager creds region (Just cluster) services = do
  let chunks = chunkList 10 services
  batches <- mapConcurrently (fetchChunk cluster) chunks
  pure (concat batches)
  where
    fetchChunk cl chunk = do
      let payload = LBS.toStrict $ encode $ object ["cluster" .= cl, "services" .= chunk]
          host = "ecs." <> TE.encodeUtf8 region <> ".amazonaws.com"
          extraHeaders =
            [ ("Content-Type", "content-type", "application/x-amz-json-1.1")
            , ("X-Amz-Target", "x-amz-target", "AmazonEC2ContainerServiceV20141113.DescribeServices")
            ]
      mBody <- signedAwsPost manager creds region "ecs" host extraHeaders payload
      pure $ case mBody >>= either (const Nothing) Just . eitherDecode . LBS.fromStrict of
        Just val -> parseEcsServicesJson val
        Nothing -> []

fetchRdsStatus
  :: Manager
  -> AwsTempCredentials
  -> Text
  -> Maybe Text
  -> IO (Maybe AwsRdsStatus)
fetchRdsStatus _ _ _ Nothing = pure Nothing
fetchRdsStatus manager creds region (Just dbId) = do
  let body =
        TE.encodeUtf8 $
          "Action=DescribeDBInstances&DBInstanceIdentifier="
            <> urlEncodeSimple dbId
            <> "&Version=2014-10-31"
      host = "rds." <> TE.encodeUtf8 region <> ".amazonaws.com"
      extraHeaders = [("Content-Type", "content-type", "application/x-www-form-urlencoded; charset=utf-8")]
  mBody <- signedAwsPost manager creds region "rds" host extraHeaders body
  pure $ mBody >>= parseRdsStatusXml . TE.decodeUtf8Lenient

fetchEc2Status
  :: Manager
  -> AwsTempCredentials
  -> Text
  -> Maybe Text
  -> IO (Maybe AwsEc2Status)
fetchEc2Status _ _ _ Nothing = pure Nothing
fetchEc2Status manager creds region (Just instId) = do
  let body =
        TE.encodeUtf8 $
          "Action=DescribeInstanceStatus&IncludeAllInstances=true&InstanceId.1="
            <> urlEncodeSimple instId
            <> "&Version=2016-11-15"
      host = "ec2." <> TE.encodeUtf8 region <> ".amazonaws.com"
      extraHeaders = [("Content-Type", "content-type", "application/x-www-form-urlencoded; charset=utf-8")]
  mBody <- signedAwsPost manager creds region "ec2" host extraHeaders body
  pure $ mBody >>= parseEc2StatusXml . TE.decodeUtf8Lenient

signedAwsPost
  :: Manager
  -> AwsTempCredentials
  -> Text
  -> ByteString
  -> ByteString
  -> [(HeaderName, ByteString, ByteString)]
  -> ByteString
  -> IO (Maybe ByteString)
signedAwsPost manager creds region service host extraHeaders body = do
  now <- getCurrentTime
  let amzDate = BSC.pack $ formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" now
      dateStamp = BSC.pack $ formatTime defaultTimeLocale "%Y%m%d" now
      payloadHash = hexSha256 body
      baseHeaders =
        ("host", host)
          : ("x-amz-date", amzDate)
          : maybe [] (\tok -> [("x-amz-security-token", tok)]) (atcSessionToken creds)
          ++ [(lowerName, v) | (_, lowerName, v) <- extraHeaders]
      sortedHeaders = sortOn fst baseHeaders
      canonicalHeaders =
        BS.concat [k <> ":" <> trimAscii v <> "\n" | (k, v) <- sortedHeaders]
      signedHeaders = BS.intercalate ";" (map fst sortedHeaders)
      canonicalReq =
        BS.intercalate
          "\n"
          ["POST", "/", "", canonicalHeaders, signedHeaders, payloadHash]
      scope = dateStamp <> "/" <> TE.encodeUtf8 region <> "/" <> service <> "/aws4_request"
      stringToSign =
        BS.intercalate "\n" ["AWS4-HMAC-SHA256", amzDate, scope, hexSha256 canonicalReq]
      kDate = hmacSha256 ("AWS4" <> atcSecretAccessKey creds) dateStamp
      kRegion = hmacSha256 kDate (TE.encodeUtf8 region)
      kService = hmacSha256 kRegion service
      kSigning = hmacSha256 kService "aws4_request"
      sig = B16.encode (hmacSha256 kSigning stringToSign)
      authHeader =
        "AWS4-HMAC-SHA256 Credential="
          <> atcAccessKeyId creds
          <> "/"
          <> scope
          <> ", SignedHeaders="
          <> signedHeaders
          <> ", Signature="
          <> sig
      reqHeaders =
        ("X-Amz-Date", amzDate)
          : ("Authorization", authHeader)
          : maybe [] (\tok -> [("X-Amz-Security-Token", tok)]) (atcSessionToken creds)
          ++ [(hName, v) | (hName, _, v) <- extraHeaders]
  res <- timeout 3000000 $ observeAws $ do
    base <- parseRequest ("https://" <> BSC.unpack host <> "/")
    let req =
          base
            { method = "POST"
            , requestHeaders = reqHeaders
            , requestBody = RequestBodyBS body
            , responseTimeout = responseTimeoutMicro 3000000
            , redirectCount = 0
            , checkResponse = \_ _ -> pure ()
            }
    withResponse req manager $ \resp -> do
      raw <- readBounded 262144 (responseBody resp)
      let code = statusCode (responseStatus resp)
      pure $ if code >= 200 && code < 300 then raw else Nothing
  pure $ case res of
    Just (Just mBytes) -> mBytes
    _ -> Nothing

loadTaskCredentials :: Manager -> IO (Maybe AwsTempCredentials)
loadTaskCredentials manager = do
  mRel <- lookupEnv "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"
  case mRel of
    Just uri | validRelUri uri -> do
      res <- observeAws $ do
        base <- parseRequest ("http://169.254.170.2" <> uri)
        let req =
              base
                { method = "GET"
                , responseTimeout = responseTimeoutMicro 1500000
                , redirectCount = 0
                , checkResponse = \_ _ -> pure ()
                }
        withResponse req manager $ \resp -> do
          raw <- readBounded 65536 (responseBody resp)
          let code = statusCode (responseStatus resp)
          pure $ if code == 200 then raw >>= parseCredsJson else Nothing
      pure (res >>= id)
    _ -> do
      mAk <- lookupEnvText "AWS_ACCESS_KEY_ID"
      mSk <- lookupEnvText "AWS_SECRET_ACCESS_KEY"
      mTok <- lookupEnvText "AWS_SESSION_TOKEN"
      pure $ case (mAk, mSk) of
        (Just ak, Just sk) ->
          Just $
            AwsTempCredentials
              (TE.encodeUtf8 ak)
              (TE.encodeUtf8 sk)
              (TE.encodeUtf8 <$> mTok)
        _ -> Nothing

parseCredsJson :: ByteString -> Maybe AwsTempCredentials
parseCredsJson raw =
  case eitherDecode (LBS.fromStrict raw) of
    Right (Object obj) -> do
      ak <- lookupText "AccessKeyId" obj
      sk <- lookupText "SecretAccessKey" obj
      let tok = lookupText "Token" obj
      pure $ AwsTempCredentials (TE.encodeUtf8 ak) (TE.encodeUtf8 sk) (TE.encodeUtf8 <$> tok)
    _ -> Nothing

-- Pure response parsers (unit-tested in AwsHealthSpec)

parseEcsServicesJson :: Value -> [AwsEcsServiceStatus]
parseEcsServicesJson (Object root) =
  case KM.lookup (Key.fromText "services") root of
    Just (Array arr) -> catMaybes [parseOne v | v <- V.toList arr]
    _ -> []
  where
    parseOne (Object obj) = do
      svcName <- lookupText "serviceName" obj
      svcStatus <- lookupText "status" obj
      desired <- lookupInt "desiredCount" obj
      running <- lookupInt "runningCount" obj
      pending <- lookupInt "pendingCount" obj
      let rollout =
            case KM.lookup (Key.fromText "deployments") obj of
              Just (Array deps) ->
                case V.toList deps of
                  (Object firstDep : _) -> lookupText "rolloutState" firstDep
                  _ -> Nothing
              _ -> Nothing
      pure $
        AwsEcsServiceStatus
          { aessServiceKey = normalizeServiceKey svcName
          , aessStatus = svcStatus
          , aessDesiredCount = desired
          , aessRunningCount = running
          , aessPendingCount = pending
          , aessRolloutState = rollout
          }
    parseOne _ = Nothing
parseEcsServicesJson _ = []

parseRdsStatusXml :: Text -> Maybe AwsRdsStatus
parseRdsStatusXml xml = do
  st <- extractXmlTag "DBInstanceStatus" xml
  let ver = extractXmlTag "EngineVersion" xml
      multiAz = parseXmlBool <$> extractXmlTag "MultiAZ" xml
      encrypted = parseXmlBool <$> extractXmlTag "StorageEncrypted" xml
      storageGb = extractXmlTag "AllocatedStorage" xml >>= readMaybe . T.unpack
  pure $ AwsRdsStatus st ver multiAz encrypted storageGb

parseEc2StatusXml :: Text -> Maybe AwsEc2Status
parseEc2StatusXml xml = do
  stateSection <- extractXmlTag "instanceState" xml
  stateName <- extractXmlTag "name" stateSection
  let sysStatus = extractXmlTag "systemStatus" xml >>= extractXmlTag "status"
      instStatus = extractXmlTag "instanceStatus" xml >>= extractXmlTag "status"
  pure $ AwsEc2Status stateName sysStatus instStatus

normalizeServiceKey :: Text -> Text
normalizeServiceKey raw =
  let s = T.toLower (T.strip raw)
   in if s == "plether-api" then "api"
      else if s == "plether-keeper" then "keeper"
      else if s == "plether-liquidation-worker" then "liquidation_worker"
      else if s == "plether-position-protection-worker" then "protection_worker"
      else if s == "plether-basket-worker" then "basket_worker"
      else if s == "plether-perps-indexer" then "perps_indexer"
      else if s == "plether-insights-worker" then "insights_worker"
      else if s == "plether-workers" then "workers"
      else if s == "plether-aa-reconciler" then "aa_reconciler"
      else if s == "plether-alto" then "alto"
      else if "rpc-gateway" `T.isSuffixOf` s then "rpc_gateway"
      else s

chunkList :: Int -> [a] -> [[a]]
chunkList n xs
  | n <= 0 || null xs = []
  | otherwise = let (h, t) = splitAt n xs in h : chunkList n t

extractXmlTag :: Text -> Text -> Maybe Text
extractXmlTag tag xml =
  let openTag = "<" <> tag <> ">"
      closeTag = "</" <> tag <> ">"
   in case T.breakOn openTag xml of
        (_, rest)
          | T.null rest -> Nothing
          | otherwise ->
              let afterOpen = T.drop (T.length openTag) rest
                  (inside, tailPart) = T.breakOn closeTag afterOpen
               in if T.null tailPart then Nothing else Just (T.strip inside)

parseXmlBool :: Text -> Bool
parseXmlBool t = T.toLower (T.strip t) == "true"

lookupText :: Text -> KM.KeyMap Value -> Maybe Text
lookupText k obj = case KM.lookup (Key.fromText k) obj of
  Just (String t) -> Just t
  _ -> Nothing

lookupInt :: Text -> KM.KeyMap Value -> Maybe Int
lookupInt k obj = case KM.lookup (Key.fromText k) obj of
  Just (Number n) -> scientificToInt n
  _ -> Nothing

scientificToInt :: Scientific -> Maybe Int
scientificToInt n = case floatingOrInteger n :: Either Double Integer of
  Right i | i >= fromIntegral (minBound :: Int) && i <= fromIntegral (maxBound :: Int) -> Just (fromIntegral i)
  _ -> Nothing

lookupEnvText :: String -> IO (Maybe Text)
lookupEnvText name = do
  mVal <- lookupEnv name
  pure $ case mVal of
    Just s | not (null s) -> Just (T.pack s)
    _ -> Nothing

validRegion :: Text -> Bool
validRegion r =
  not (T.null r) && T.length r <= 32 && T.all (\c -> isAlphaNum c || c == '-') r

validRelUri :: String -> Bool
validRelUri ('/' : rest) = not (".." `T.isInfixOf` T.pack rest)
validRelUri _ = False

urlEncodeSimple :: Text -> Text
urlEncodeSimple = T.filter (\c -> isAlphaNum c || c `elem` ("-_." :: String))

hexSha256 :: ByteString -> ByteString
hexSha256 bs = B16.encode (convert (hash bs :: Digest SHA256))

hmacSha256 :: ByteString -> ByteString -> ByteString
hmacSha256 key msg = convert (hmac key msg :: HMAC SHA256)

trimAscii :: ByteString -> ByteString
trimAscii = BSC.dropWhile (== ' ') . fst . BSC.spanEnd (== ' ')

readBounded :: Int -> BodyReader -> IO (Maybe ByteString)
readBounded limit reader = go 0 []
  where
    go total chunks = do
      chunk <- brRead reader
      if BS.null chunk
        then pure $ Just $ BS.concat (reverse chunks)
        else
          let next = total + BS.length chunk
           in if next > limit then pure Nothing else go next (chunk : chunks)

observeAws :: IO a -> IO (Maybe a)
observeAws action = do
  res <- try action
  case res of
    Right val -> pure (Just val)
    Left (err :: SomeException) ->
      case fromException err :: Maybe SomeAsyncException of
        Just _ -> throwIO err
        Nothing -> pure Nothing
