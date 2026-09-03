module Plether.Api
  ( app
  , handleBasketCurrentCandleAt
  ) where

import Control.Exception (evaluate)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON, withObject, (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as LT
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Types.Header (hCacheControl, hPragma)
import Network.HTTP.Types.Status (status200, status400, status403, status404, status426, status429, status500, status503)
import Network.HTTP.Client (Manager)
import Network.Wai (Middleware, pathInfo, queryString, requestHeaders)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (..)
  , cors
  , simpleCorsResourcePolicy
  )
import Plether.Cache (AppCache)
import Plether.AA.Pimlico (PimlicoProxyState, handlePimlicoProxy)
import Plether.Config (AaConfig (..), Config (..), perpsCandleRollupReadEnabled)
import Plether.Insights.Registration.Config (RegistrationConfig (..))
import Plether.Ethereum.Client (EthClient)
import Plether.Handlers.Protocol (getProtocolConfig, getProtocolStatus)
import Plether.Perps.Release
  ( perpsV2CalldataPolicy
  , perpsV2DeploymentBlock
  , perpsV2ManifestVersion
  , perpsV2PolicyEvaluator
  , perpsV2PositionProtectionBook
  , perpsV2PublicLens
  , validatePerpsV2ReleaseConfig
  )
import Plether.Handlers.Perps
  ( BasketHistoryFetch (..)
  , BasketHistoryTimings (..)
  , BasketCandleFetch (..)
  , BasketCandleTimings (..)
  , basketCandleServerTiming
  , basketCandleTimingMetrics
  , coverageLagSeconds
  , basketHistoryServerTiming
  , basketHistoryTimingMetrics
  , durationMilliseconds
  , getBasketHistoryTimed
  , getBasketCandlePageTimed
  , getBasketCurrentCandleTimedAt
  , isBoundedComponentHistoryRequest
  , getBasketLatest
  , getCachedLatestPythUpdate
  , getPythUpdate
  , getRevealPayload
  )
import Plether.Logging (field, logInfo, logInfoEvery, logWarn)
import Plether.Handlers.PerpsHistory
  ( getPerpsAccountActivity
  , getPerpsAccountOrders
  , getPerpsIndexerStatusResponse
  , getPerpsMarketStatsResponse
  , waitForPerpsOrderTerminal
  )
import Plether.Handlers.VaultPerformance (getVaultPerformanceHistory)
import Plether.Handlers.Quote
  ( getBurnQuote
  , getLeverageQuote
  , getMintQuote
  , getTradeQuote
  , getZapQuote
  )
import Plether.Handlers.User
  ( getUserAllowances
  , getUserBalances
  , getUserDashboard
  , getUserPositions
  )
import Plether.Handlers.History
  ( getHistory
  , getLeverageHistory
  , getLendingHistory
  )
import Plether.Handlers.Insights
  ( getCompetitionLeaderboardResponse
  , getCompetitionWalletResponse
  , getCurrentCompetitionResponse
  , getInsightsDataStatusResponse
  )
import Plether.Handlers.InsightsRegistration (registerInsightsRegistrationRoutes)
import Plether.Database (DbPool)
import Plether.Database.VaultActivity (VaultActivityDeployment (..))
import Plether.Handlers.VaultActivity
  ( getVaultAccountRequestIds
  , getVaultActivity
  )
import Plether.Handlers.TestnetFaucet
  ( claimTestnetFaucet
  )
import Plether.Handlers.TestnetFaucetGuard
  ( FaucetClientId (..)
  , FaucetGuardFailure (..)
  , FaucetGuardFailureReason (..)
  , FaucetGuardState
  , authenticateFaucetRequest
  , checkFaucetRequest
  , faucetGuardFailureReasonText
  , faucetQuotaScopeText
  )
import Plether.Types.History (HistoryParams (..))
import Plether.Types.Perps
  ( BasketHistoryParams (..)
  , isAlignedBasketCandleCursor
  , hasExactBasketCandleQueryKeys
  , isBasketCandleCursorWithinFutureBound
  , isCanonicalBasketCandleInterval
  , parseCanonicalPositiveInteger
  , parseBasketHistoryQueryParams
  )
import Plether.Types.VaultPerformance
  ( VaultPerformanceDeployment (..)
  , isCanonicalVaultPerformanceRequest
  )
import Plether.Types (ApiError)
import qualified Plether.Types.Error as E
import Plether.Utils.Address (isValidAddress)
import Web.Scotty
  ( ActionM
  , ScottyM
  , get
  , header
  , jsonData
  , json
  , middleware
  , pathParam
  , post
  , queryParamMaybe
  , raw
  , request
  , setHeader
  , status
  )

data TestnetFaucetRequest = TestnetFaucetRequest (Maybe Text) Bool

instance FromJSON TestnetFaucetRequest where
  parseJSON = withObject "TestnetFaucetRequest" $ \v -> do
    addressValue <- v .:? "address"
    confirmationMode <- v .:? "confirmationMode"
    let address = case addressValue of
          Just (Aeson.String value) -> Just value
          _ -> Nothing
        acceptsAsync = confirmationMode == Just (Aeson.String "async")
    pure $ TestnetFaucetRequest address acceptsAsync

app :: AppCache -> EthClient -> EthClient -> Config -> Maybe DbPool -> Manager -> PimlicoProxyState -> FaucetGuardState -> ScottyM ()
app cache client perpsClient cfg mPool manager pimlicoProxyState faucetGuardState = do
  middleware $ corsMiddleware cfg

  case mPool of
    Just pool -> registerInsightsRegistrationRoutes pool perpsClient cfg manager
    Nothing -> pure ()

  get "/api/health" $ do
    status status200
    json ("{\"status\":\"ok\"}" :: Text)

  get "/api/aa/status" $ do
    let releaseConfigured =
          isRight $
            validatePerpsV2ReleaseConfig
              (cfgPerpsChainId cfg)
              (cfgPerpsOrderRouter cfg)
              (cfgPerpsOrderLifecycleBook cfg)
              (cfgPerpsCfdEngine cfg)
              (cfgPerpsMarginClearinghouse cfg)
              (cfgPerpsHousePool cfg)
              (cfgPerpsIndexerStartBlock cfg)
        sponsorshipEnabled = maybe False aaSponsorshipEnabled $ cfgAaConfig cfg
    status status200
    json $
      Aeson.object
        [ "manifestVersion" .= perpsV2ManifestVersion
        , "chainId" .= cfgPerpsChainId cfg
        , "deploymentBlock" .= perpsV2DeploymentBlock
        , "usdc" .= cfgPerpsUsdc cfg
        , "orderRouter" .= cfgPerpsOrderRouter cfg
        , "orderLifecycleBook" .= cfgPerpsOrderLifecycleBook cfg
        , "cfdEngine" .= cfgPerpsCfdEngine cfg
        , "marginClearinghouse" .= cfgPerpsMarginClearinghouse cfg
        , "housePool" .= cfgPerpsHousePool cfg
        , "policyEvaluator" .= perpsV2PolicyEvaluator
        , "positionProtectionBook" .= perpsV2PositionProtectionBook
        , "perpsPublicLens" .= perpsV2PublicLens
        , "calldataPolicy" .= perpsV2CalldataPolicy
        -- A configured lifecycle book makes startup perform the coherent-block
        -- graph and runtime-code verification before the server can listen.
        , "bindingsVerified" .= releaseConfigured
        , "sponsorshipEnabled" .= sponsorshipEnabled
        ]

  post "/api/testnet/faucet" $ do
    suppliedToken <- fmap LT.toStrict <$> header "X-Plether-Faucet-Proxy-Token"
    suppliedClientIp <- fmap LT.toStrict <$> header "CF-Connecting-IP"
    case
        authenticateFaucetRequest
          (cfgFaucetGuardConfig cfg)
          suppliedToken
          suppliedClientIp
      of
      Left failure -> handleFaucetGuardFailure failure
      Right (guardConfig, clientId) -> do
        TestnetFaucetRequest maybeAddress acceptsAsync <- jsonData
        guardResult <-
          liftIO $
            checkFaucetRequest
              faucetGuardState
              guardConfig
              clientId
              acceptsAsync
        case guardResult of
          Left failure -> handleFaucetGuardFailure failure
          Right () -> do
            liftIO $ logFaucetGuardAccepted clientId
            case maybeAddress of
              Just addr
                | isValidAddress addr ->
                    case mPool of
                      Just pool ->
                        liftIO (claimTestnetFaucet pool perpsClient cfg addr) >>= handleResult
                      Nothing ->
                        handleServiceUnavailable $
                          E.internalError "DATABASE_URL is not configured; testnet faucet is unavailable"
              Just addr -> handleError $ E.invalidAddress addr
              Nothing -> handleError $ E.invalidAddress "address is required"

  post "/api/aa/pimlico" $
    handlePimlicoProxy pimlicoProxyState cfg perpsClient manager

  get "/api/protocol/status" $ do
    result <- liftIO $ getProtocolStatus cache client cfg mPool
    handleResult result

  get "/api/protocol/config" $ do
    result <- liftIO $ getProtocolConfig client cfg
    handleResult result

  get "/api/user/:address/dashboard" $ do
    addr <- pathParam "address"
    if isValidAddress addr
      then do
        result <- liftIO $ getUserDashboard cache client cfg addr
        handleResult result
      else handleError $ E.invalidAddress addr

  get "/api/user/:address/balances" $ do
    addr <- pathParam "address"
    if isValidAddress addr
      then do
        result <- liftIO $ getUserBalances client cfg addr
        handleResult result
      else handleError $ E.invalidAddress addr

  get "/api/user/:address/positions" $ do
    addr <- pathParam "address"
    if isValidAddress addr
      then do
        result <- liftIO $ getUserPositions client cfg addr
        handleResult result
      else handleError $ E.invalidAddress addr

  get "/api/user/:address/allowances" $ do
    addr <- pathParam "address"
    if isValidAddress addr
      then do
        result <- liftIO $ getUserAllowances cache client cfg addr
        handleResult result
      else handleError $ E.invalidAddress addr

  get "/api/quotes/mint" $ do
    mAmount <- queryParamMaybe "amount"
    case mAmount >>= parseAmount of
      Just amount | amount > 0 -> do
        result <- liftIO $ getMintQuote client cfg amount
        handleResult result
      _ -> handleError $ E.invalidAmount "amount must be a positive integer"

  get "/api/quotes/burn" $ do
    mAmount <- queryParamMaybe "amount"
    case mAmount >>= parseAmount of
      Just amount | amount > 0 -> do
        result <- liftIO $ getBurnQuote client cfg amount
        handleResult result
      _ -> handleError $ E.invalidAmount "amount must be a positive integer"

  get "/api/quotes/zap" $ do
    mDirection <- queryParamMaybe "direction"
    mAmount <- queryParamMaybe "amount"
    case (mDirection, mAmount >>= parseAmount) of
      (Just dir, Just amount) | amount > 0 && (dir == "buy" || dir == "sell") -> do
        result <- liftIO $ getZapQuote client cfg dir amount
        handleResult result
      (Nothing, _) -> handleError $ E.invalidAmount "direction parameter required (buy or sell)"
      (_, Nothing) -> handleError $ E.invalidAmount "amount must be a positive integer"
      _ -> handleError $ E.invalidAmount "invalid parameters"

  get "/api/quotes/trade" $ do
    mFrom <- queryParamMaybe "from"
    mAmount <- queryParamMaybe "amount"
    case (mFrom, mAmount >>= parseAmount) of
      (Just from, Just amount) | amount > 0 && (from == "usdc" || from == "bear") -> do
        result <- liftIO $ getTradeQuote client cfg from amount
        handleResult result
      (Nothing, _) -> handleError $ E.invalidAmount "from parameter required (usdc or bear)"
      (_, Nothing) -> handleError $ E.invalidAmount "amount must be a positive integer"
      _ -> handleError $ E.invalidAmount "invalid parameters"

  get "/api/quotes/leverage" $ do
    mSide <- queryParamMaybe "side"
    mPrincipal <- queryParamMaybe "principal"
    mLeverage <- queryParamMaybe "leverage"
    case (mSide, mPrincipal >>= parseAmount, mLeverage >>= parseAmount) of
      (Just side, Just principal, Just leverage)
        | principal > 0 && leverage > 0 && (side == "bear" || side == "bull") -> do
            result <- liftIO $ getLeverageQuote client cfg side principal leverage
            handleResult result
      (Nothing, _, _) -> handleError $ E.invalidSide "side parameter required"
      (_, Nothing, _) -> handleError $ E.invalidAmount "principal must be a positive integer"
      (_, _, Nothing) -> handleError $ E.invalidAmount "leverage must be a positive integer"
      _ -> handleError $ E.invalidAmount "invalid parameters"

  get "/api/insights/v1/competitions/current" $
    case mPool of
      Just pool -> liftIO (getCurrentCompetitionResponse pool cfg) >>= handleResult
      Nothing -> insightsUnavailable

  get "/api/insights/v1/competitions/:slug/leaderboard" $ do
    slug <- pathParam "slug"
    mLimit <- queryParamMaybe "limit"
    mCursor <- queryParamMaybe "cursor"
    mSearch <- queryParamMaybe "search"
    case (traverse parseNonNegativeInt mLimit, traverse parseNonNegativeInt mCursor) of
      (Just parsedLimit, Just parsedCursor) ->
        case mPool of
          Just pool -> do
            result <-
              liftIO $
                getCompetitionLeaderboardResponse
                  pool
                  cfg
                  slug
                  mSearch
                  (maybe 50 id parsedLimit)
                  (maybe 0 id parsedCursor)
            handleResult result
          Nothing -> insightsUnavailable
      (Nothing, _) -> handleError $ E.invalidAmount "limit must be a non-negative integer"
      (_, Nothing) -> handleError $ E.invalidAmount "cursor must be a non-negative integer"

  get "/api/insights/v1/competitions/:slug/wallets/:address" $ do
    slug <- pathParam "slug"
    addr <- pathParam "address"
    mActivityLimit <- queryParamMaybe "activityLimit"
    case traverse parseNonNegativeInt mActivityLimit of
      Nothing -> handleError $ E.invalidAmount "activityLimit must be a non-negative integer"
      Just parsedLimit ->
        case mPool of
          Just pool -> do
            result <-
              liftIO $
                getCompetitionWalletResponse
                  pool
                  cfg
                  slug
                  addr
                  (maybe 100 id parsedLimit)
            handleResult result
          Nothing -> insightsUnavailable

  get "/api/insights/v1/status" $
    case mPool of
      Just pool -> liftIO (getInsightsDataStatusResponse pool cfg) >>= handleResult
      Nothing -> insightsUnavailable

  case mPool of
    Just pool -> do
      get "/api/user/:address/history" $ do
        addr <- pathParam "address"
        if isValidAddress addr
          then do
            params <- historyParams
            result <- liftIO $ getHistory pool client cfg addr params
            handleResult result
          else handleError $ E.invalidAddress addr

      get "/api/user/:address/history/leverage" $ do
        addr <- pathParam "address"
        if isValidAddress addr
          then do
            params <- historyParams
            result <- liftIO $ getLeverageHistory pool client cfg addr params
            handleResult result
          else handleError $ E.invalidAddress addr

      get "/api/user/:address/history/lending" $ do
        addr <- pathParam "address"
        if isValidAddress addr
          then do
            params <- historyParams
            result <- liftIO $ getLendingHistory pool client cfg addr params
            handleResult result
          else handleError $ E.invalidAddress addr

      get "/api/perps/accounts/:address/orders" $ do
        addr <- pathParam "address"
        if isValidAddress addr
          then do
            limit <- perpsHistoryLimit
            mCursor <- queryParamMaybe "cursor"
            mRouter <- queryParamMaybe "router"
            case (traverse parseHistoryCursor mCursor, validateRouterParam mRouter) of
              (Just cursor, Just router) -> do
                result <- liftIO $ getPerpsAccountOrders pool cfg router addr limit cursor
                handleResult result
              (Nothing, _) ->
                handleError $ E.invalidAmount "cursor must be blockNumber:tieBreaker"
              (_, Nothing) ->
                handleError $ E.invalidAddress $ maybe "" id mRouter
          else handleError $ E.invalidAddress addr

      get "/api/perps/accounts/:address/activity" $ do
        addr <- pathParam "address"
        if isValidAddress addr
          then do
            limit <- perpsHistoryLimit
            mCursor <- queryParamMaybe "cursor"
            mRouter <- queryParamMaybe "router"
            case (traverse parseHistoryCursor mCursor, validateRouterParam mRouter) of
              (Just cursor, Just router) -> do
                result <- liftIO $ getPerpsAccountActivity pool cfg router addr limit cursor
                handleResult result
              (Nothing, _) ->
                handleError $ E.invalidAmount "cursor must be blockNumber:tieBreaker"
              (_, Nothing) ->
                handleError $ E.invalidAddress $ maybe "" id mRouter
          else handleError $ E.invalidAddress addr

      get "/api/perps/indexer/status" $ do
        result <- liftIO $ getPerpsIndexerStatusResponse pool cfg
        handleResult result

      get "/api/perps/orders/:orderId/wait" $ do
        rawOrderId <- pathParam "orderId"
        mAccount <- queryParamMaybe "account"
        mRouter <- queryParamMaybe "router"
        mTimeoutSeconds <- queryParamMaybe "timeoutSeconds"
        case (parsePositiveInteger rawOrderId, traverse parsePositiveInt mTimeoutSeconds, validateRouterParam mRouter) of
          (Just orderId, Just timeoutSeconds, Just router)
            | maybe True isValidAddress mAccount -> do
                result <- liftIO $
                  waitForPerpsOrderTerminal pool cfg router orderId mAccount (maybe 60 id timeoutSeconds)
                handleResult result
            | otherwise ->
                handleError $ E.invalidAddress $ maybe "" id mAccount
          (Nothing, _, _) ->
            handleError $ E.invalidAmount "orderId must be a positive integer"
          (_, Nothing, _) ->
            handleError $ E.invalidAmount "timeoutSeconds must be a positive integer"
          (_, _, Nothing) ->
            handleError $ E.invalidAddress $ maybe "" id mRouter
    Nothing -> pure ()

  get "/api/perps/market/stats" $ do
    case mPool of
      Just pool -> do
        result <- liftIO $ getPerpsMarketStatsResponse pool cfg
        handleResult result
      Nothing ->
        handleServiceUnavailable $
          E.internalError "DATABASE_URL is not configured; perps market stats are unavailable"

  get "/api/perps/vaults/history" $ do
    queryKeys <- currentQueryKeys
    mRange <- queryParamMaybe "range"
    mInterval <- queryParamMaybe "interval"
    if not $ isCanonicalVaultPerformanceRequest queryKeys mRange mInterval
      then
        handleError $
          E.invalidAmount "vault history is restricted to range=7d and interval=3600"
      else case mPool of
        Just pool -> do
          let deployment =
                VaultPerformanceDeployment
                  { vpdChainId = cfgPerpsChainId cfg
                  , vpdHousePool = cfgVaultHistoryHousePoolAddress cfg
                  , vpdSeniorVault = cfgVaultHistorySeniorVaultAddress cfg
                  , vpdJuniorVault = cfgVaultHistoryJuniorVaultAddress cfg
                  }
          result <- liftIO $ getVaultPerformanceHistory pool deployment
          handleResult result
        Nothing ->
          handleServiceUnavailable $
            E.internalError "DATABASE_URL is not configured; vault performance history is unavailable"

  get "/api/perps/vaults/activity" $ do
    queryKeys <- currentQueryKeys
    if not $ null queryKeys
      then handleError $ E.invalidAmount "vault activity does not accept query parameters"
      else case mPool of
        Nothing ->
          handleServiceUnavailable $
            E.internalError "DATABASE_URL is not configured; vault activity is unavailable"
        Just pool -> do
          response <- liftIO $ getVaultActivity pool $ vaultActivityDeployment cfg
          case response of
            Just value -> handleResult $ Right value
            Nothing ->
              handleServiceUnavailable $
                E.internalError "Vault activity is backfilling confirmed Alchemy logs"

  get "/api/perps/vaults/:tranche/accounts/:address/request-ids" $ do
    tranche <- T.toLower <$> pathParam "tranche"
    account <- pathParam "address"
    queryKeys <- currentQueryKeys
    mLimit <- queryParamMaybe "limit"
    mCursor <- queryParamMaybe "cursor"
    let knownQuery = all (`elem` ["limit", "cursor"]) queryKeys
        uniqueQuery = countKey "limit" queryKeys <= 1 && countKey "cursor" queryKeys <= 1
        parsedLimit = maybe (Just 100) parseStrictPositiveInt mLimit
        parsedCursor = traverse parseStrictUnsignedInteger mCursor
    case (tranche `elem` ["senior", "junior"], isStrictVaultAccountAddress account, knownQuery && uniqueQuery, parsedLimit, parsedCursor, mPool) of
      (False, _, _, _, _, _) ->
        handleError $ E.invalidAmount "tranche must be senior or junior"
      (_, False, _, _, _, _) -> handleError $ E.invalidAddress account
      (_, _, False, _, _, _) ->
        handleError $ E.invalidAmount "only one limit and one cursor parameter are accepted"
      (_, _, _, Just limit, Just cursor, Just pool)
        | limit <= 250 -> do
            response <-
              liftIO $
                getVaultAccountRequestIds
                  pool
                  (vaultActivityDeployment cfg)
                  tranche
                  account
                  limit
                  cursor
            case response of
              Just value -> handleResult $ Right value
              Nothing ->
                handleServiceUnavailable $
                  E.internalError "Vault request discovery is backfilling confirmed Alchemy logs"
        | otherwise -> handleError $ E.invalidAmount "limit must be between 1 and 250"
      (_, _, _, Nothing, _, _) ->
        handleError $ E.invalidAmount "limit must be an unsigned integer between 1 and 250"
      (_, _, _, _, Nothing, _) ->
        handleError $ E.invalidAmount "cursor must be an unsigned request ID"
      (_, _, _, _, _, Nothing) ->
        handleServiceUnavailable $
          E.internalError "DATABASE_URL is not configured; vault request discovery is unavailable"

  get "/api/perps/basket/history" $ do
    handlerStartedAt <- liftIO getMonotonicTimeNSec
    parsedParams <- basketHistoryParams
    case parsedParams of
      Left reason -> handleError $ E.invalidAmount reason
      Right params
        | not $ isBoundedComponentHistoryRequest params ->
            handleError $
              E.invalidAmount "component history is restricted to range=24h and interval=3600"
        | otherwise -> case mPool of
            Just pool -> do
              result <- liftIO $ getBasketHistoryTimed pool cfg params
              case result of
                Left err -> handleError err
                Right fetch -> handleBasketHistoryResult handlerStartedAt params fetch
            Nothing ->
              handleServiceUnavailable $
                E.internalError "DATABASE_URL is not configured; perps basket history is unavailable"

  get "/api/perps/basket/candles" $ do
    handlerStartedAt <- liftIO getMonotonicTimeNSec
    queryKeys <- currentQueryKeys
    if not $ hasExactBasketCandleQueryKeys ["interval", "cursor"] queryKeys
      then
        handleError $
          E.invalidAmount "exactly one interval and one cursor query parameter are required"
      else do
        now <- floor <$> liftIO getPOSIXTime
        mInterval <- queryParamMaybe "interval"
        mCursor <- queryParamMaybe "cursor"
        case (mInterval >>= parseCanonicalPositiveInteger, mCursor >>= parseCanonicalPositiveInteger, mPool) of
          (Just interval, Just cursor, Just pool)
            | not (isCanonicalBasketCandleInterval interval) ->
                handleError $
                  E.invalidAmount
                    "interval must be one of 60, 180, 300, 900, 1800, 3600, or 86400"
            | not $
                perpsCandleRollupReadEnabled
                  (cfgPerpsCandleReadMode cfg)
                  (cfgPerpsCandleStrictCoverage cfg)
                  (cfgPerpsCandleReadIntervals cfg)
                  interval ->
                handleError $
                  E.notFound "Strict candle rollup reads are not enabled for this interval"
            | not (isAlignedBasketCandleCursor interval cursor) ->
                handleError $
                  E.invalidAmount
                    "cursor must be a positive Unix timestamp aligned to interval * 500"
            | not (isBasketCandleCursorWithinFutureBound now interval cursor) ->
                handleError $
                  E.invalidAmount "cursor is too far ahead of the backend clock"
            | otherwise -> do
                requireFresh <- requestForcesCandleRefresh
                result <-
                  liftIO $
                    getBasketCandlePageTimed cache pool cfg interval cursor requireFresh
                case result of
                  Left err -> handleError err
                  Right fetch ->
                    handleBasketCandleResult handlerStartedAt "historical" interval fetch
          (Nothing, _, _) ->
            handleError $ E.invalidAmount "interval must be a canonical positive integer"
          (_, Nothing, _) ->
            handleError $ E.invalidAmount "cursor must be a canonical positive integer"
          (_, _, Nothing) ->
            handleServiceUnavailable $
              E.internalError "DATABASE_URL is not configured; perps basket candles are unavailable"

  get "/api/perps/basket/candles/current" $ do
    validatedAt <- floor <$> liftIO getPOSIXTime
    handleBasketCurrentCandleAt cache cfg mPool validatedAt

  get "/api/perps/basket/latest" $ do
    case mPool of
      Just pool -> do
        result <- liftIO $ getBasketLatest pool cfg
        handleResult result
      Nothing ->
        handleServiceUnavailable $
          E.internalError "DATABASE_URL is not configured; perps basket latest is unavailable"

  get "/api/perps/orders/:orderId/reveal-payload" $ do
    rawOrderId <- pathParam "orderId"
    mMinPublishTime <- queryParamMaybe "minPublishTime"
    mMaxPublishTime <- queryParamMaybe "maxPublishTime"
    case (parsePositiveInteger rawOrderId, mMinPublishTime >>= parsePositiveInteger, mMaxPublishTime >>= parsePositiveInteger, mPool) of
      (Just orderId, Just minPublishTime, Just maxPublishTime, Just pool)
        | minPublishTime <= maxPublishTime -> do
            result <- liftIO $ getRevealPayload pool perpsClient cfg orderId minPublishTime maxPublishTime
            handleResult result
      (Nothing, _, _, _) ->
        handleError $ E.invalidAmount "orderId must be a positive integer"
      (_, Nothing, _, _) ->
        handleError $ E.invalidAmount "minPublishTime must be a positive integer"
      (_, _, Nothing, _) ->
        handleError $ E.invalidAmount "maxPublishTime must be a positive integer"
      (_, _, _, Nothing) ->
        handleServiceUnavailable $
          E.internalError "DATABASE_URL is not configured; reveal payload cache is unavailable"
      _ ->
        handleError $ E.invalidAmount "minPublishTime must be less than or equal to maxPublishTime"

  get "/api/perps/pyth/update" $ do
    mPublishTime <- queryParamMaybe "publishTime"
    case traverse parsePositiveInteger mPublishTime of
      Just mTs -> do
        result <- liftIO $ getPythUpdate cache manager perpsClient cfg mTs
        handleResult result
      Nothing ->
        handleError $ E.invalidAmount "publishTime must be a positive integer"

  get "/api/perps/pyth/cached-latest" $ do
    case mPool of
      Just pool -> do
        result <- liftIO $ getCachedLatestPythUpdate cache pool perpsClient cfg
        handleResult result
      Nothing ->
        handleServiceUnavailable $
          E.internalError "DATABASE_URL is not configured; cached Pyth updates are unavailable"

historyParams :: ActionM HistoryParams
historyParams = do
  mPage <- queryParamMaybe "page"
  mLimit <- queryParamMaybe "limit"
  mType <- queryParamMaybe "type"
  mSide <- queryParamMaybe "side"
  pure $ HistoryParams
    { hpPage = maybe 1 (max 1 . parseIntOr 1) mPage
    , hpLimit = maybe 20 (min 100 . max 1 . parseIntOr 20) mLimit
    , hpTxType = mType
    , hpSide = mSide
    , hpTxTypes = []
    }
  where
    parseIntOr :: Int -> Text -> Int
    parseIntOr def txt = maybe def id (readMaybeInt txt)

    readMaybeInt :: Text -> Maybe Int
    readMaybeInt txt =
      let stripped = T.strip txt
      in if T.all (\c -> c >= '0' && c <= '9') stripped && not (T.null stripped)
           then Just $ read $ T.unpack stripped
           else Nothing

perpsHistoryLimit :: ActionM Int
perpsHistoryLimit = do
  mLimit <- queryParamMaybe "limit"
  pure $ maybe 30 (min 100 . max 1 . parseIntOr 30) mLimit
  where
    parseIntOr :: Int -> Text -> Int
    parseIntOr def txt = maybe def id (readMaybeInt txt)

    readMaybeInt :: Text -> Maybe Int
    readMaybeInt txt =
      let stripped = T.strip txt
      in if T.all (\c -> c >= '0' && c <= '9') stripped && not (T.null stripped)
           then Just $ read $ T.unpack stripped
           else Nothing

basketHistoryParams :: ActionM (Either Text BasketHistoryParams)
basketHistoryParams = do
  queryKeys <- currentQueryKeys
  mRange <- queryParamMaybe "range"
  mInterval <- queryParamMaybe "interval"
  mIncludeComponents <- queryParamMaybe "includeComponents"
  pure $
    parseBasketHistoryQueryParams
      queryKeys
      mRange
      mInterval
      mIncludeComponents

handleResult :: (ToJSON a) => Either ApiError a -> ActionM ()
handleResult = \case
  Right response -> do
    setHeader "Content-Type" "application/json"
    status status200
    json response
  Left err -> handleError err

handleBasketHistoryResult :: Word64 -> BasketHistoryParams -> BasketHistoryFetch -> ActionM ()
handleBasketHistoryResult handlerStartedAt params fetch = do
  encodeStartedAt <- liftIO getMonotonicTimeNSec
  let body = Aeson.encode $ bhfResponse fetch
  -- Aeson and the history merge are lazy. Force this exact body before ending
  -- the stage so response encoding is not deferred into Warp's body streaming.
  bodyBytes <- liftIO $ evaluate $ LBS.length body
  encodeFinishedAt <- liftIO getMonotonicTimeNSec
  -- plether_app is core application time through a fully materialized response.
  -- The structured log flush and socket write happen after this measurement.
  let timings =
        BasketHistoryTimings
          { bhtBackendTotalNs = encodeFinishedAt - handlerStartedAt
          , bhtDbPoolWaitNs = bhfPoolWaitNs fetch
          , bhtSnapshotQueryNs = bhfSnapshotQueryNs fetch
          , bhtVolumeQueryNs = bhfVolumeQueryNs fetch
          , bhtResponseEncodeNs = encodeFinishedAt - encodeStartedAt
          }
  liftIO $ logBasketHistoryTimings params fetch bodyBytes timings
  setHeader "Content-Type" "application/json"
  setHeader "Server-Timing" $ LT.fromStrict $ basketHistoryServerTiming timings
  status status200
  raw body

-- | Serve the strict current-candle route with a caller-supplied backend clock
-- second. Production samples it once at route entry; the integration suite
-- fixes it at the publication-grace boundary so the response header and
-- validation outcome remain one testable invariant.
handleBasketCurrentCandleAt :: AppCache -> Config -> Maybe DbPool -> Integer -> ActionM ()
handleBasketCurrentCandleAt cache cfg mPool validatedAt = do
  handlerStartedAt <- liftIO getMonotonicTimeNSec
  queryKeys <- currentQueryKeys
  if not $ hasExactBasketCandleQueryKeys ["interval"] queryKeys
    then
      handleError $
        E.invalidAmount "exactly one interval query parameter is required"
    else do
      mInterval <- queryParamMaybe "interval"
      case (mInterval >>= parseCanonicalPositiveInteger, mPool) of
        (Just interval, Just pool)
          | not (isCanonicalBasketCandleInterval interval) ->
              handleError $
                E.invalidAmount
                  "interval must be one of 60, 180, 300, 900, 1800, 3600, or 86400"
          | not $
              perpsCandleRollupReadEnabled
                (cfgPerpsCandleReadMode cfg)
                (cfgPerpsCandleStrictCoverage cfg)
                (cfgPerpsCandleReadIntervals cfg)
                interval ->
              handleError $
                E.notFound "Strict candle rollup reads are not enabled for this interval"
          | otherwise -> do
              -- This is the exact integer wall-clock sample used by strict
              -- current-candle freshness validation. Set it before running
              -- the handler so both its 200 and 503 paths retain the same
              -- deterministic origin-clock evidence. The public Date header
              -- may be generated or replaced by an intermediary.
              setHeader "X-Plether-Candle-Validated-At" $ LT.pack $ show validatedAt
              requireFresh <- requestForcesCandleRefresh
              result <-
                liftIO $
                  getBasketCurrentCandleTimedAt
                    cache pool cfg validatedAt interval requireFresh
              case result of
                Left err -> handleError err
                Right fetch ->
                  handleBasketCandleResult handlerStartedAt "current" interval fetch
        (Nothing, _) ->
          handleError $ E.invalidAmount "interval must be a canonical positive integer"
        (_, Nothing) ->
          handleServiceUnavailable $
            E.internalError "DATABASE_URL is not configured; current perps basket candle is unavailable"

handleBasketCandleResult
  :: (ToJSON a)
  => Word64
  -> Text
  -> Integer
  -> BasketCandleFetch a
  -> ActionM ()
handleBasketCandleResult handlerStartedAt requestKind interval fetch = do
  encodeStartedAt <- liftIO getMonotonicTimeNSec
  let body = Aeson.encode $ bcfResponse fetch
  bodyBytes <- liftIO $ evaluate $ LBS.length body
  encodeFinishedAt <- liftIO getMonotonicTimeNSec
  let timings =
        BasketCandleTimings
          { bctBackendTotalNs = encodeFinishedAt - handlerStartedAt
          , bctDbPoolWaitNs = bcfPoolWaitNs fetch
          , bctQueryNs = bcfQueryNs fetch
          , bctSingleFlightWaitNs = bcfSingleFlightWaitNs fetch
          , bctResponseEncodeNs = encodeFinishedAt - encodeStartedAt
          }
  liftIO $ logBasketCandleTimings requestKind interval fetch bodyBytes timings
  -- A bounded stale backend value is useful to the current caller, but must
  -- not be promoted into a fresh shared edge-cache entry.
  when (bcfReadSource fetch == "rollup_stale_memory_cache") $
    setHeader "Cache-Control" "no-store"
  setHeader "Content-Type" "application/json"
  setHeader "Server-Timing" $ LT.fromStrict $ basketCandleServerTiming timings
  status status200
  raw body

logBasketCandleTimings
  :: Text
  -> Integer
  -> BasketCandleFetch a
  -> Int64
  -> BasketCandleTimings
  -> IO ()
logBasketCandleTimings requestKind interval fetch bodyBytes timings = do
  now <- floor <$> getPOSIXTime
  let finalizedThrough = bcfFinalizedThrough fetch
      -- A coarse interval's finalized watermark is intentionally bucket
      -- aligned, so its absolute age can approach the full interval without
      -- indicating ingestion lag. Alarm on delay beyond that expected bucket
      -- age instead of making daily candles permanently breach a five-minute
      -- threshold.
      lagSeconds = max 0 $ coverageLagSeconds now finalizedThrough - interval
  logInfoEvery
    10
    "perps_candle_request_timing"
    "Perps basket candle request completed"
    $ [ field "request_kind" requestKind
      , field "interval_seconds" interval
      , field "read_source" $ bcfReadSource fetch
      , field "rows" $ bcfRowCount fetch
      , field "dataset_generation" $ bcfDatasetGeneration fetch
      , field "response_bytes" bodyBytes
      , field "query_ms" $ durationMilliseconds $ bcfQueryNs fetch
      ]
        <> map
          (\(metric, duration) -> field (Key.fromText $ metric <> "_ms") $ durationMilliseconds duration)
          (basketCandleTimingMetrics timings)
  logInfoEvery
    60
    "perps_candle_coverage"
    "Perps candle rollup coverage observed"
    [ field "interval_seconds" interval
    , field "read_source" $ bcfReadSource fetch
    , field "coverage_available" $ maybe False (const True) finalizedThrough
    , field "lag_seconds" lagSeconds
    , field "dataset_generation" $ bcfDatasetGeneration fetch
    , field "complete" True
    ]

logBasketHistoryTimings
  :: BasketHistoryParams
  -> BasketHistoryFetch
  -> Int64
  -> BasketHistoryTimings
  -> IO ()
logBasketHistoryTimings params fetch bodyBytes timings =
  logInfoEvery
    10
    "perps_basket_history_timing"
    "Perps basket history request completed"
    $ [ field "range" $ bhpRange params
      , field "interval_seconds" $ bhpIntervalSeconds params
      , field "include_components" $ bhpIncludeComponents params
      , field "read_source" $ bhfReadSource fetch
      , field "snapshot_rows" $ bhfSnapshotRows fetch
      , field "volume_rows" $ bhfVolumeRows fetch
      , field "response_bytes" bodyBytes
      ]
        <> map
          (\(metric, duration) -> field (Key.fromText $ metric <> "_ms") $ durationMilliseconds duration)
          (basketHistoryTimingMetrics timings)

handleError :: ApiError -> ActionM ()
handleError err = do
  setHeader "Content-Type" "application/json"
  status $
    case E.errCode err of
      E.RateLimited -> status429
      E.Forbidden -> status403
      E.UpgradeRequired -> status426
      E.RpcError -> status503
      E.NetworkError -> status503
      E.NotFound -> status404
      E.InternalError -> status500
      _ -> status400
  json err

handleFaucetGuardFailure :: FaucetGuardFailure -> ActionM ()
handleFaucetGuardFailure failure = do
  liftIO $ logFaucetGuardRejected failure
  case fgfReason failure of
    FaucetConfirmationModeUnsupported ->
      handleError $
        E.upgradeRequired
          "Plether was updated. Refresh this page, then try the faucet again."
    FaucetClientQuotaExceeded -> handleQuotaFailure
    FaucetGlobalQuotaExceeded -> handleQuotaFailure
    _ ->
      handleError $
        E.forbidden "Faucet requests must use the official Plether app."
  where
    handleQuotaFailure = do
      case fgfRetryAfterSeconds failure of
        Just retryAfter -> setHeader "Retry-After" $ LT.pack $ show retryAfter
        Nothing -> pure ()
      handleError $ E.rateLimited

logFaucetGuardAccepted :: FaucetClientId -> IO ()
logFaucetGuardAccepted clientId =
  logInfo
    "testnet_faucet_guard"
    "Testnet faucet request passed the request guard"
    [ field "outcome" ("accepted" :: Text)
    , field "rejection_reason" ("none" :: Text)
    , field "client_id" $ unFaucetClientId clientId
    , field "quota_scope" ("both" :: Text)
    , field "retry_after_seconds" (Nothing :: Maybe Int)
    ]

logFaucetGuardRejected :: FaucetGuardFailure -> IO ()
logFaucetGuardRejected failure =
  logWarn
    "testnet_faucet_guard"
    "Testnet faucet request was rejected by the request guard"
    [ field "outcome" ("rejected" :: Text)
    , field "rejection_reason" $ faucetGuardFailureReasonText $ fgfReason failure
    , field "client_id" $ fmap unFaucetClientId $ fgfClientId failure
    , field "quota_scope" $ faucetQuotaScopeText $ fgfQuotaScope failure
    , field "retry_after_seconds" $ fgfRetryAfterSeconds failure
    ]

handleServiceUnavailable :: ApiError -> ActionM ()
handleServiceUnavailable err = do
  setHeader "Content-Type" "application/json"
  status status503
  json err

currentQueryKeys :: ActionM [Text]
currentQueryKeys = do
  req <- request
  pure $
    map
      (Data.Text.Encoding.decodeUtf8With lenientDecode . fst)
      (queryString req)

requestForcesCandleRefresh :: ActionM Bool
requestForcesCandleRefresh = do
  req <- request
  let headers = requestHeaders req
      contains directive name =
        maybe False
          (BS8.isInfixOf directive . BS8.map toLowerAscii)
          (lookup name headers)
  pure $
    contains "no-cache" hCacheControl
      || contains "no-store" hCacheControl
      || contains "max-age=0" hCacheControl
      || contains "no-cache" hPragma
  where
    toLowerAscii byte
      | byte >= 'A' && byte <= 'Z' = toEnum $ fromEnum byte + 32
      | otherwise = byte

parseAmount :: Text -> Maybe Integer
parseAmount txt =
  let stripped = T.strip txt
   in if T.all (\c -> c >= '0' && c <= '9') stripped && not (T.null stripped)
        then Just $ read $ T.unpack stripped
        else Nothing

parsePositiveInteger :: Text -> Maybe Integer
parsePositiveInteger txt = do
  value <- parseAmount txt
  if value > 0 then Just value else Nothing

parsePositiveInt :: Text -> Maybe Int
parsePositiveInt txt = do
  value <- parsePositiveInteger txt
  if value <= fromIntegral (maxBound :: Int)
    then Just $ fromInteger value
    else Nothing

parseNonNegativeInt :: Text -> Maybe Int
parseNonNegativeInt txt = do
  value <- parseAmount txt
  if value <= fromIntegral (maxBound :: Int)
    then Just $ fromInteger value
    else Nothing

insightsUnavailable :: ActionM ()
insightsUnavailable =
  handleServiceUnavailable $
    E.internalError "DATABASE_URL is not configured; Plether Insights is unavailable"

parseHistoryCursor :: Text -> Maybe (Integer, Integer)
parseHistoryCursor txt =
  case T.splitOn ":" (T.strip txt) of
    [rawBlock, rawTieBreaker] -> do
      blockNumber <- parseAmount rawBlock
      tieBreaker <- parseAmount rawTieBreaker
      Just (blockNumber, tieBreaker)
    _ -> Nothing

validateRouterParam :: Maybe Text -> Maybe (Maybe Text)
validateRouterParam Nothing = Just Nothing
validateRouterParam (Just router)
  | isValidAddress router = Just $ Just router
  | otherwise = Nothing

countKey :: Text -> [Text] -> Int
countKey needle = length . filter (== needle)

isStrictVaultAccountAddress :: Text -> Bool
isStrictVaultAccountAddress value =
  T.length value == 42 && "0x" `T.isPrefixOf` value && isValidAddress value

parseStrictUnsignedInteger :: Text -> Maybe Integer
parseStrictUnsignedInteger value
  | not (T.null value) && T.all isAsciiDigit value = Just $ read $ T.unpack value
  | otherwise = Nothing
 where
  isAsciiDigit character = character >= '0' && character <= '9'

parseStrictPositiveInt :: Text -> Maybe Int
parseStrictPositiveInt value = do
  parsed <- parseStrictUnsignedInteger value
  if parsed > 0 && parsed <= fromIntegral (maxBound :: Int)
    then Just $ fromIntegral parsed
    else Nothing

vaultActivityDeployment :: Config -> VaultActivityDeployment
vaultActivityDeployment cfg =
  VaultActivityDeployment
    { vadChainId = cfgPerpsChainId cfg
    , vadHousePool = cfgVaultHistoryHousePoolAddress cfg
    , vadSeniorVault = cfgVaultHistorySeniorVaultAddress cfg
    , vadJuniorVault = cfgVaultHistoryJuniorVaultAddress cfg
    , vadDeploymentBlock = cfgVaultHistoryDeploymentBlock cfg
    }

corsMiddleware :: Config -> Middleware
corsMiddleware cfg = cors $ \waiRequest -> Just $
  if isRegistrationPath $ pathInfo waiRequest
    then registrationPolicy
    else policy
  where
    origins = cfgCorsOrigins cfg

    policy =
      simpleCorsResourcePolicy
        { corsOrigins = Just (map encodeUtf8 origins, True)
        , corsMethods = ["GET", "POST", "OPTIONS"]
        , corsRequestHeaders = ["Content-Type", "Authorization", "X-Registration-CSRF"]
        , corsExposedHeaders = Just ["Server-Timing", "Retry-After"]
        }

    registrationPolicy =
      policy
        { corsOrigins =
            Just
              ( maybe [] (pure . encodeUtf8 . rcPublicOrigin) (cfgRegistrationConfig cfg)
              , True
              )
        }

    isRegistrationPath = \case
      "api" : "insights" : "v1" : "competitions" : _ : "registrations" : _ -> True
      _ -> False

    encodeUtf8 :: Text -> Data.ByteString.ByteString
    encodeUtf8 = Data.Text.Encoding.encodeUtf8
