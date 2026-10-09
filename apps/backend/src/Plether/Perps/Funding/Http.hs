module Plether.Perps.Funding.Http (FundingHttpState, newFundingHttpState, registerFundingRoutes) where

import Control.Concurrent.MVar
import Control.Monad (unless, when)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client (Manager)
import Network.HTTP.Types.Status
import System.Environment (lookupEnv)
import Web.Scotty (ActionM, ScottyM, liftIO, get, post, json, finish, setHeader, status, pathParam, request)
import Plether.AA.Pimlico (readBoundedRequestBody)
import Plether.Config (Config (..))
import Plether.Database
import Plether.Ethereum.Client (EthClient,ethBlockNumber,newClientWithOptions,RpcClientOptions (..))
import Plether.Ethereum.Rpc (ethChainId)
import Plether.Ethereum.Abi (keccak256)
import Plether.Perps.Funding.Across
import Plether.Perps.Funding.Chain
import Plether.Perps.Funding.Source
import Plether.Perps.Funding.Store
import Plether.Perps.Funding.Types

data FundingHttpState = FundingHttpState
  { fhConfigured :: Either Text (Maybe FundingDeployment)
  , fhEnabled :: Bool
  , fhProvider :: FundingProvider
  , fhBudget :: MVar [Integer]
  , fhSourceClient :: Maybe EthClient
  , fhManager :: Manager
  }

newFundingHttpState :: Manager -> Config -> IO FundingHttpState
newFundingHttpState manager cfg = do
  loaded <- loadFundingDeployment
  let configured = loaded >>= \case
        Just deployment -> Just <$> validateFundingReleaseBinding (cfgPerpsChainId cfg) (cfgPerpsMarginClearinghouse cfg) (cfgPerpsUsdc cfg) deployment
        Nothing -> Right Nothing
  FundingHttpState configured
    <$> ((== Just "true") <$> lookupEnv "PERPS_FUNDING_ENABLED")
    <*> acrossProvider manager <*> newMVar [] <*> sourceClient <*> pure manager
  where
    sourceClient = lookupEnv "PERPS_FUNDING_SOURCE_RPC_URL" >>= \case
      Just url | "https://" `T.isPrefixOf` T.pack url -> do
        auth <- fmap T.pack <$> lookupEnv "PERPS_FUNDING_SOURCE_RPC_AUTH_TOKEN"
        Just <$> newClientWithOptions (RpcClientOptions (T.pack url) auth "funding-source")
      _ -> pure Nothing

-- Endpoints remain discoverable while disabled; no arbitrary targets/calldata.
registerFundingRoutes :: FundingHttpState -> EthClient -> Maybe DbPool -> ScottyM ()
registerFundingRoutes FundingHttpState {fhConfigured = configured, fhEnabled = enabled, fhProvider = provider, fhBudget = budget, fhManager = manager, fhSourceClient = sourceClient} client maybePool = do
  let deployment = either (const Nothing) id configured
      unavailable = case configured of
        Left _ -> Just "FUNDING_CONFIGURATION_INVALID"
        Right Nothing -> Just "FUNDING_NOT_CONFIGURED"
        _ | not enabled -> Just "FUNDING_DISABLED"
          | not (isJust maybePool) -> Just "FUNDING_DATABASE_UNAVAILABLE"
          | not (isJust sourceClient) -> Just "SOURCE_RPC_NOT_CONFIGURED"
          | otherwise -> fpUnavailableReason provider
      requireLive = do
        maybe (pure ()) (failRequest status503) unavailable
        case (deployment,maybePool) of
          (Just release,Just pool) -> do
            executor <- liftIO $ withDb pool $ \conn -> isWorkerReady conn (deploymentReadinessKey release) (fdChainId release)
            unless executor $ failRequest status503 "DESTINATION_OBSERVER_UNAVAILABLE"
            source <- maybe (failRequest status503 "SOURCE_RPC_NOT_CONFIGURED") pure sourceClient
            sourceChain <- liftIO (ethChainId source) >>= either (const $ failRequest status503 "SOURCE_RPC_UNAVAILABLE") pure
            unless (sourceChain == 1) $ failRequest status503 "SOURCE_CHAIN_MISMATCH"
            proof <- liftIO $ verifyFundingDeployment client release
            either (failRequest status503) pure proof
            pure (release,pool)
          _ -> failRequest status503 "FUNDING_NOT_CONFIGURED"
      findStored = do
        identifier <- pathParam "id"
        either (const $ failRequest status400 "INVALID_INTENT_ID") (const $ pure ()) $ validateHash identifier
        pool <- maybe (failRequest status503 "FUNDING_DATABASE_UNAVAILABLE") pure maybePool
        pure (pool,identifier)
  get "/api/perps/funding/config" $ do
    setHeader "Cache-Control" "no-store"
    -- Configuration is not attestation. A quote re-verifies live bindings before
    -- allowing any source wallet to send funds.
    executor <- case (deployment,maybePool) of
      (Just release,Just pool) -> liftIO $ withDb pool $ \conn -> isWorkerReady conn (deploymentReadinessKey release) (fdChainId release)
      _ -> pure False
    sourceReady <- case sourceClient of
      Nothing -> pure False
      Just source -> liftIO $ (== Right 1) <$> ethChainId source
    let reason = case unavailable of
          Just value -> Just value
          Nothing | not executor -> Just "DESTINATION_OBSERVER_UNAVAILABLE"
          Nothing | not sourceReady -> Just "SOURCE_RPC_UNAVAILABLE"
          _ -> Nothing
        base = maybe (object []) toJSON deployment
    json $ envelope $ setFields
      [("enabled",Bool $ reason == Nothing),("reason",toJSON reason)
      ,("provider",String $ fpName provider),("sourceAssets",acrossSourceAssets)] base
  post "/api/perps/funding/quotes" $ do
    (release,pool) <- requireLive
    now <- liftIO $ floor <$> getPOSIXTime
    permitted <- liftIO $ modifyMVar budget $ \times -> do
      let recent = filter (> now - 60) times
      pure (if length recent < 30 then now:recent else recent,length recent < 30)
    unless permitted $ failRequest status429 "FUNDING_QUOTE_RATE_LIMITED"
    input <- boundedJson
    identifier <- liftIO newIdentifier
    quoted <- liftIO (fpQuote provider release input identifier) >>= either (failRequest status503) pure
    when (pqExpiresAt quoted <= now || pqExpiresAt quoted > now + 600 || null (pqTransactions quoted)) $
      failRequest status503 "INVALID_PROVIDER_QUOTE"
    minimumAmount <- either (const $ failRequest status503 "INVALID_PROVIDER_AMOUNT") pure $ validateAmount $ pqMinimumAmount quoted
    estimate <- either (const $ failRequest status503 "INVALID_PROVIDER_AMOUNT") pure $ validateAmount $ pqEstimatedAmount quoted
    unless (estimate >= minimumAmount && minimumAmount >= 1_000_000) $ failRequest status400 "FUNDING_AMOUNT_BELOW_MINIMUM"
    block <- liftIO (ethBlockNumber client) >>= either (const $ failRequest status503 "DESTINATION_RPC_UNAVAILABLE") pure
    destinationMessage <- either (const $ failRequest status503 "INVALID_PROVIDER_MESSAGE") pure $ decodeHex $ pqDestinationMessage quoted
    let quote = setFields
          [("quoteId",String identifier)
          ,("beneficiary",String $ qrBeneficiary input),("ownerAddress",String $ qrSourceOwner input)
          ,("sourceChainId",toJSON $ qrSourceChainId input),("sourceToken",String $ qrSourceToken input)
          ,("sourceAmount",String $ qrSourceAmount input)
          ,("destinationMessage",String $ pqDestinationMessage quoted)
          ,("destinationMessageHash",String $ encodeHex $ keccak256 destinationMessage)
          ,("expiresAt",toJSON $ pqExpiresAt quoted),("estimatedAmount",String $ pqEstimatedAmount quoted)
          ,("minimumAmount",String $ pqMinimumAmount quoted),("provider",String $ fpName provider)
          ,("providerReference",String $ pqProviderReference quoted),("sourceTransactions",toJSON $ pqTransactions quoted)
          ,("observationFromBlock",toJSON $ max (fdStartBlock release) (block - fdConfirmations release))]
          (toJSON release)
    liftIO $ withDb pool $ \conn -> insertQuote conn identifier quote (pqExpiresAt quoted)
    json $ envelope $ publicIntent quote
  post "/api/perps/funding/intents" $ do
    pool <- maybe (failRequest status503 "FUNDING_DATABASE_UNAVAILABLE") pure maybePool
    input <- boundedJson
    quoteId <- requiredText "quoteId" input
    key <- requiredText "idempotencyKey" input
    either (const $ failRequest status400 "INVALID_QUOTE_ID") (const $ pure ()) $ validateHash quoteId
    unless (T.length key >= 16 && T.length key <= 128 && T.all (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c `elem` ['-', '_', ':']) key) $
      failRequest status400 "INVALID_IDEMPOTENCY_KEY"
    previous <- liftIO $ withDb pool $ \conn -> findIntentByIdempotencyKey conn key
    case previous of
      Just intent | fieldText "quoteId" intent == Just quoteId -> respondStored pool intent >> finish
      Just _ -> failRequest status409 "IDEMPOTENCY_CONFLICT"
      Nothing -> pure ()
    _ <- requireLive
    quote <- liftIO (withDb pool $ \conn -> findQuote conn quoteId) >>= maybe (failRequest status404 "QUOTE_NOT_FOUND") pure
    identifier <- liftIO newIdentifier
    result <- liftIO $ withDb pool $ \conn -> createIntent conn key identifier quoteId $ intentFromQuote identifier quote
    either (failRequest status409) (respondStored pool) result
  get "/api/perps/funding/intents/:id" $ do
    (pool,identifier) <- findStored
    setHeader "Cache-Control" "no-store"
    intent <- liftIO (withDb pool $ \conn -> getIntent conn identifier) >>= maybe (failRequest status404 "INTENT_NOT_FOUND") pure
    refreshed <- liftIO $ refreshProviderStatus manager sourceClient pool identifier intent
    respondStored pool refreshed
  post "/api/perps/funding/intents/:id/source" $ do
    (pool,identifier) <- findStored
    input <- boundedJson
    hash <- requiredText "sourceTxHash" input >>= either (const $ failRequest status400 "INVALID_SOURCE_TRANSACTION") pure . validateHash
    original <- liftIO (withDb pool $ \conn -> getIntent conn identifier) >>= maybe (failRequest status404 "INTENT_NOT_FOUND") pure
    source <- maybe (failRequest status503 "SOURCE_RPC_NOT_CONFIGURED") pure sourceClient
    liftIO (validateSourceTransaction source original hash) >>= either (failRequest status409) pure
    result <- liftIO $ withDb pool $ \conn -> withFundingStateLock conn $ getIntent conn identifier >>= \case
      Nothing -> pure $ Left "INTENT_NOT_FOUND"
      Just old -> do
        relay <- getSourceRelay conn identifier
        if isJust relay then pure $ if fieldText "sourceTxHash" old == Just hash then Right old else Left "SOURCE_RELAY_ALREADY_BOUND" else do
         let next = setFields [("sourceTxHash",String hash),("sourceStatus",String "pending"),("sourceTerminal",Bool False),("sourceBlockNumber",Null),("sourceBlockHash",Null),("sourceConfirmations",toJSON (0 :: Integer)),("bridgeStatus",Null),("providerCheckedAt",toJSON (0 :: Integer)),("status",String $ if fieldText "status" old == Just "awaiting-source" then "bridging" else fromMaybe "bridging" $ fieldText "status" old)] old
         updateIntent conn identifier next
         pure $ Right next
    either (failRequest status409) (respondStored pool) result
  post "/api/perps/funding/intents/:id/retry" $ do
    (pool,identifier) <- findStored
    result <- liftIO $ withDb pool $ \conn -> withFundingStateLock conn $ getIntent conn identifier >>= \case
      Nothing -> pure Nothing
      Just old -> do
        let next = if fieldText "status" old == Just "retryable" then setFields [("lastError",Null),("status",String "bridging")] old else old
        updateIntent conn identifier next
        pure $ Just next
    maybe (failRequest status404 "INTENT_NOT_FOUND") (respondStored pool) result

-- Use the stored release identity so historical intents cannot borrow the
-- current release's heartbeat, while a retained old observer can serve them.
respondStored :: DbPool -> Value -> ActionM ()
respondStored pool intent = do
  now <- liftIO $ floor <$> getPOSIXTime
  ready <- case fromJSON intent :: Result FundingDeployment of
    Success release -> liftIO $ withDb pool $ \conn -> isWorkerReady conn (deploymentReadinessKey release) (fdChainId release)
    Error _ -> pure False
  json $ envelope $ publicObservedIntent now ready intent

envelope :: Value -> Value
envelope value = object ["data" .= value]
failRequest :: Status -> Text -> ActionM a
failRequest code reason = do
  status code
  setHeader "Cache-Control" "no-store"
  json $ object ["error" .= object ["code" .= reason,"message" .= reason]]
  finish
requiredText :: Text -> Value -> ActionM Text
requiredText key value = maybe (failRequest status400 "INVALID_FUNDING_REQUEST") pure $ fieldText key value
boundedJson :: FromJSON a => ActionM a
boundedJson = do
  req <- request
  body <- liftIO $ readBoundedRequestBody 8192 req
  either (const $ failRequest status400 "INVALID_FUNDING_REQUEST")
    (either (const $ failRequest status400 "INVALID_FUNDING_REQUEST") pure . eitherDecode) body

-- Provider state is advisory and never changes canonical deposit confirmation.
-- Reserve a per-intent poll before network IO so concurrent browsers cannot
-- exhaust the provider quota; release the database lock during that IO.
refreshProviderStatus :: Manager -> Maybe EthClient -> DbPool -> Text -> Value -> IO Value
refreshProviderStatus manager sourceClient pool identifier original = do
  now <- floor <$> getPOSIXTime
  poll <- withDb pool $ \conn -> withFundingStateLock conn $ do
    current <- getIntent conn identifier
    case current of
      Just value | Just hash <- fieldText "sourceTxHash" value
        , now - fromMaybe 0 (fieldInteger "providerCheckedAt" value) >= 10 -> do
          updateIntent conn identifier $ setFields [("providerCheckedAt",toJSON now)] value
          pure $ Just hash
      _ -> pure Nothing
  case poll of
    Nothing -> pure original
    Just hash -> do
      sourceStatus <- case sourceClient of
        Nothing -> pure $ Left "SOURCE_RPC_NOT_CONFIGURED"
        Just source -> validateSourceTransaction source original hash >>= \case
          Left reason -> pure $ Left reason
          Right () -> sourceReceiptEvidence source hash
      observation <- case sourceStatus of
        Right evidence | fieldText "sourceStatus" evidence == Just "reverted" -> pure $ Left "SOURCE_TRANSACTION_REVERTED"
        Left reason -> pure $ Left reason
        _ -> acrossTransferStatus manager hash
      withDb pool $ \conn -> withFundingStateLock conn $ do
        current <- fromMaybe original <$> getIntent conn identifier
        if fieldText "sourceTxHash" current /= Just hash then pure current else do
          claim <- getSourceRelay conn identifier
          let sourceFields = either (const [("sourceTerminal",Bool False),("sourceBlockHash",Null),("sourceBlockNumber",Null),("sourceConfirmations",toJSON (0 :: Integer))])
                (\value -> case value of Object sourceData -> [(Key.toText key,item) | (key,item) <- KM.toList sourceData]; _ -> []) sourceStatus
              fields = (if isJust claim then filter (\(key,_) -> key `elem` ["sourceStatus","sourceTerminal","sourceConfirmations"]) sourceFields else sourceFields)
                <> either (const []) (\value -> [("bridgeStatus",String value)]) observation
              next = setFields fields current
          updateIntent conn identifier next
          pure next
