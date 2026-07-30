module Plether.Api
  ( app
  , noStoreErrorResponses
  , parseDatabaseBigInt
  , parseProtocolCursor
  , parseTrancheHistoryCursor
  , parseProtocolOrderId
  , protocolExplorerGate
  , protocolRpcChainGate
  , protocolRpcChainGateWith
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON, encode, object, withObject, (.:), (.=))
import qualified Data.ByteString
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding
import Network.HTTP.Types.Header (hCacheControl, hContentType)
import Network.HTTP.Types.Method (methodGet)
import Network.HTTP.Types.Status (status200, status400, status404, status429, status500, status503, statusCode)
import Network.HTTP.Client (Manager)
import Network.Wai
  ( Middleware
  , mapResponseHeaders
  , pathInfo
  , requestMethod
  , responseLBS
  , responseStatus
  )
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (..)
  , cors
  , simpleCorsResourcePolicy
  )
import Plether.Cache (AppCache)
import Plether.AA.Pimlico (PimlicoProxyState, handlePimlicoProxy)
import Plether.Config (Config (..))
import Plether.Ethereum.Client
  ( EthClient
  , RpcChainBindingError (..)
  , RpcError
  , ethChainId
  , validateRpcChainId
  )
import Plether.Handlers.Protocol (getProtocolConfig, getProtocolStatus)
import Plether.Handlers.Perps
  ( getBasketHistory
  , getBasketLatest
  , getCachedLatestPythUpdate
  , getPythUpdate
  , getRevealPayload
  )
import Plether.Handlers.PerpsHistory
  ( getPerpsAccountActivity
  , getPerpsAccountOrders
  , getPerpsIndexerStatusResponse
  , getPerpsMarketStatsResponse
  , waitForPerpsOrderTerminal
  )
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
import Plether.Handlers.ProtocolInsights
  ( ProtocolCursor
  , ProtocolTransactionFilters (..)
  , TrancheHistoryCursor
  , decodeProtocolCursor
  , decodeTrancheHistoryCursor
  , getCurrentProtocolReleaseResponse
  , getHousePoolResponse
  , getKeeperResponse
  , getKeepersResponse
  , getOperationalWalletResponse
  , getOperationalWalletsResponse
  , getParameterChangesResponse
  , getParametersResponse
  , getProtocolOrderResponse
  , getProtocolOverviewResponse
  , getProtocolTransactionResponse
  , getProtocolTransactionsResponse
  , getTrancheHistoryResponse
  , getTrancheResponse
  )
import Plether.Database (DbPool)
import Plether.Handlers.TestnetFaucet (claimTestnetFaucet)
import Plether.Protocol.Release (ProtocolRelease (..), currentProtocolRelease)
import Plether.Types.History (HistoryParams (..))
import Plether.Types.Perps (BasketHistoryParams (..), defaultBasketHistoryParams)
import Plether.Types (ApiError)
import qualified Plether.Types.Error as E
import Plether.Utils.Address (isValidAddress)
import Web.Scotty
  ( ActionM
  , ScottyM
  , get
  , jsonData
  , json
  , middleware
  , pathParam
  , post
  , queryParamMaybe
  , setHeader
  , status
  )

newtype TestnetFaucetRequest = TestnetFaucetRequest Text

instance FromJSON TestnetFaucetRequest where
  parseJSON = withObject "TestnetFaucetRequest" $ \v ->
    TestnetFaucetRequest <$> v .: "address"

app :: AppCache -> EthClient -> EthClient -> Config -> Maybe DbPool -> Manager -> PimlicoProxyState -> ScottyM ()
app cache client perpsClient cfg mPool manager pimlicoProxyState = do
  middleware noStoreErrorResponses
  middleware $ corsMiddleware cfg
  middleware $ protocolExplorerGate (cfgProtocolExplorerEnabled cfg)
  middleware $
    if cfgProtocolExplorerEnabled cfg
      then
        protocolRpcChainGate
          perpsClient
          (prChainId $ currentProtocolRelease cfg)
      else id

  get "/api/health" $ do
    status status200
    json ("{\"status\":\"ok\"}" :: Text)

  post "/api/testnet/faucet" $ do
    TestnetFaucetRequest addr <- jsonData
    if isValidAddress addr
      then case mPool of
        Just pool -> do
          result <- liftIO $ claimTestnetFaucet pool perpsClient cfg addr
          handleResult result
        Nothing ->
          handleServiceUnavailable $
            E.internalError "DATABASE_URL is not configured; testnet faucet is unavailable"
      else handleError $ E.invalidAddress addr

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

  get "/api/insights/v1/protocol/releases/current" $
    liftIO (getCurrentProtocolReleaseResponse mPool perpsClient cfg) >>= handleResult

  get "/api/insights/v1/protocol/releases/:releaseId/overview" $ do
    releaseId <- pathParam "releaseId"
    case mPool of
      Just pool -> liftIO (getProtocolOverviewResponse pool perpsClient cfg releaseId) >>= handleResult
      Nothing -> protocolInsightsUnavailable

  get "/api/insights/v1/protocol/releases/:releaseId/transactions" $ do
    releaseId <- pathParam "releaseId"
    mActionType <- queryParamMaybe "actionType"
    mOutcome <- queryParamMaybe "outcome"
    mAddress <- queryParamMaybe "address"
    mAccount <- queryParamMaybe "account"
    mKeeper <- queryParamMaybe "keeper"
    mContract <- queryParamMaybe "contract"
    mTxHash <- queryParamMaybe "transactionHash"
    mFrom <- queryParamMaybe "from"
    mTo <- queryParamMaybe "to"
    mLimit <- queryParamMaybe "limit"
    mCursor <- queryParamMaybe "cursor"
    case
        ( traverse validateOptionalAddress [mAddress, mAccount, mKeeper, mContract]
        , traverse validateOptionalTxHash mTxHash
        , traverse parseDatabaseBigInt mFrom
        , traverse parseDatabaseBigInt mTo
        , traverse parseNonNegativeInt mLimit
        , traverse parseProtocolCursor mCursor
        )
      of
      (Just _, Just _, Just fromTimestamp, Just toTimestamp, Just parsedLimit, Just cursor) ->
        case mPool of
          Just pool -> do
            result <- liftIO $
              getProtocolTransactionsResponse
                pool
                perpsClient
                cfg
                releaseId
                ProtocolTransactionFilters
                  { ptfActionType = mActionType
                  , ptfOutcome = mOutcome
                  , ptfAddress = mAddress
                  , ptfAccount = mAccount
                  , ptfKeeper = mKeeper
                  , ptfContract = mContract
                  , ptfTransactionHash = mTxHash
                  , ptfFromTimestamp = fromTimestamp
                  , ptfToTimestamp = toTimestamp
                  }
                (maybe 50 id parsedLimit)
                cursor
            handleResult result
          Nothing -> protocolInsightsUnavailable
      (Nothing, _, _, _, _, _) -> handleError $ E.invalidAddress "address, account, keeper, or contract filter"
      (_, Nothing, _, _, _, _) -> handleError $ E.invalidAmount "transactionHash must be a 32-byte hex hash"
      (_, _, Nothing, _, _, _) -> handleError $ E.invalidAmount "from must be a unix timestamp"
      (_, _, _, Nothing, _, _) -> handleError $ E.invalidAmount "to must be a unix timestamp"
      (_, _, _, _, Nothing, _) -> handleError $ E.invalidAmount "limit must be a non-negative integer"
      (_, _, _, _, _, Nothing) -> handleError $ E.invalidAmount "cursor is invalid"

  get "/api/insights/v1/protocol/releases/:releaseId/transactions/:txHash" $ do
    releaseId <- pathParam "releaseId"
    txHash <- pathParam "txHash"
    if isValidTransactionHash txHash
      then case mPool of
        Just pool -> liftIO (getProtocolTransactionResponse pool perpsClient cfg releaseId txHash) >>= handleResult
        Nothing -> protocolInsightsUnavailable
      else handleError $ E.invalidAmount "txHash must be a 32-byte hex hash"

  get "/api/insights/v1/protocol/releases/:releaseId/orders/:orderId" $ do
    releaseId <- pathParam "releaseId"
    rawOrderId <- pathParam "orderId"
    case parseProtocolOrderId rawOrderId of
      Just orderId -> case mPool of
        Just pool -> liftIO (getProtocolOrderResponse pool perpsClient cfg releaseId orderId) >>= handleResult
        Nothing -> protocolInsightsUnavailable
      Nothing -> handleError $ E.invalidAmount "orderId must be a non-negative indexed uint64 value"

  get "/api/insights/v1/protocol/releases/:releaseId/house-pool" $ do
    releaseId <- pathParam "releaseId"
    case mPool of
      Just pool -> liftIO (getHousePoolResponse pool perpsClient cfg releaseId) >>= handleResult
      Nothing -> protocolInsightsUnavailable

  get "/api/insights/v1/protocol/releases/:releaseId/tranches/:tranche/history" $ do
    releaseId <- pathParam "releaseId"
    tranche <- pathParam "tranche"
    mLimit <- queryParamMaybe "limit"
    mCursor <- queryParamMaybe "cursor"
    case (traverse parseNonNegativeInt mLimit, traverse parseTrancheHistoryCursor mCursor) of
      (Just parsedLimit, Just cursor) -> case mPool of
        Just pool -> liftIO (getTrancheHistoryResponse pool perpsClient cfg releaseId tranche (maybe 200 id parsedLimit) cursor) >>= handleResult
        Nothing -> protocolInsightsUnavailable
      (Nothing, _) -> handleError $ E.invalidAmount "limit must be a non-negative integer"
      (_, Nothing) -> handleError $ E.invalidAmount "cursor is invalid"

  get "/api/insights/v1/protocol/releases/:releaseId/tranches/:tranche" $ do
    releaseId <- pathParam "releaseId"
    tranche <- pathParam "tranche"
    case mPool of
      Just pool -> liftIO (getTrancheResponse pool perpsClient cfg releaseId tranche) >>= handleResult
      Nothing -> protocolInsightsUnavailable

  get "/api/insights/v1/protocol/releases/:releaseId/keepers" $ do
    releaseId <- pathParam "releaseId"
    window <- maybe "7d" id <$> queryParamMaybe "window"
    mLimit <- queryParamMaybe "limit"
    mCursor <- queryParamMaybe "cursor"
    case
        ( isValidKeeperWindow window
        , traverse parseNonNegativeInt mLimit
        , traverse parseProtocolCursor mCursor
        )
      of
      (True, Just parsedLimit, Just cursor) ->
        case mPool of
          Just pool ->
            liftIO
              (getKeepersResponse pool perpsClient cfg releaseId window (maybe 100 id parsedLimit) cursor)
              >>= handleResult
          Nothing -> protocolInsightsUnavailable
      (False, _, _) -> handleError $ E.invalidAmount "window must be 24h, 7d, or 30d"
      (_, Nothing, _) -> handleError $ E.invalidAmount "limit must be a non-negative integer"
      (_, _, Nothing) -> handleError $ E.invalidAmount "cursor is invalid"

  get "/api/insights/v1/protocol/releases/:releaseId/keepers/:address" $ do
    releaseId <- pathParam "releaseId"
    address <- pathParam "address"
    window <- maybe "7d" id <$> queryParamMaybe "window"
    mLimit <- queryParamMaybe "limit"
    mCursor <- queryParamMaybe "cursor"
    case
        ( isValidAddress address
        , isValidKeeperWindow window
        , traverse parseNonNegativeInt mLimit
        , traverse parseProtocolCursor mCursor
        )
      of
      (True, True, Just parsedLimit, Just cursor) ->
        case mPool of
          Just pool ->
            liftIO
              (getKeeperResponse pool perpsClient cfg releaseId address window (maybe 100 id parsedLimit) cursor)
              >>= handleResult
          Nothing -> protocolInsightsUnavailable
      (False, _, _, _) -> handleError $ E.invalidAddress address
      (_, False, _, _) -> handleError $ E.invalidAmount "window must be 24h, 7d, or 30d"
      (_, _, Nothing, _) -> handleError $ E.invalidAmount "limit must be a non-negative integer"
      (_, _, _, Nothing) -> handleError $ E.invalidAmount "cursor is invalid"

  get "/api/insights/v1/protocol/releases/:releaseId/wallets" $ do
    releaseId <- pathParam "releaseId"
    window <- maybe "7d" id <$> queryParamMaybe "window"
    mLimit <- queryParamMaybe "limit"
    mCursor <- queryParamMaybe "cursor"
    case
        ( isValidKeeperWindow window
        , traverse parseNonNegativeInt mLimit
        , traverse parseProtocolCursor mCursor
        )
      of
      (True, Just parsedLimit, Just cursor) ->
        case mPool of
          Just pool ->
            liftIO
              (getOperationalWalletsResponse pool perpsClient cfg releaseId window (maybe 50 id parsedLimit) cursor)
              >>= handleResult
          Nothing -> protocolInsightsUnavailable
      (False, _, _) -> handleError $ E.invalidAmount "window must be 24h, 7d, or 30d"
      (_, Nothing, _) -> handleError $ E.invalidAmount "limit must be a non-negative integer"
      (_, _, Nothing) -> handleError $ E.invalidAmount "cursor is invalid"

  get "/api/insights/v1/protocol/releases/:releaseId/wallets/:address" $ do
    releaseId <- pathParam "releaseId"
    address <- pathParam "address"
    window <- maybe "7d" id <$> queryParamMaybe "window"
    mLimit <- queryParamMaybe "limit"
    mCursor <- queryParamMaybe "cursor"
    case
        ( isValidAddress address
        , isValidKeeperWindow window
        , traverse parseNonNegativeInt mLimit
        , traverse parseProtocolCursor mCursor
        )
      of
      (True, True, Just parsedLimit, Just cursor) ->
        case mPool of
          Just pool ->
            liftIO
              (getOperationalWalletResponse pool perpsClient cfg releaseId address window (maybe 100 id parsedLimit) cursor)
              >>= handleResult
          Nothing -> protocolInsightsUnavailable
      (False, _, _, _) -> handleError $ E.invalidAddress address
      (_, False, _, _) -> handleError $ E.invalidAmount "window must be 24h, 7d, or 30d"
      (_, _, Nothing, _) -> handleError $ E.invalidAmount "limit must be a non-negative integer"
      (_, _, _, Nothing) -> handleError $ E.invalidAmount "cursor is invalid"

  get "/api/insights/v1/protocol/releases/:releaseId/parameters" $ do
    releaseId <- pathParam "releaseId"
    case mPool of
      Just pool -> liftIO (getParametersResponse pool perpsClient cfg releaseId) >>= handleResult
      Nothing -> protocolInsightsUnavailable

  get "/api/insights/v1/protocol/releases/:releaseId/parameter-changes" $ do
    releaseId <- pathParam "releaseId"
    mLimit <- queryParamMaybe "limit"
    mCursor <- queryParamMaybe "cursor"
    case (traverse parseNonNegativeInt mLimit, traverse parseProtocolCursor mCursor) of
      (Just parsedLimit, Just cursor) -> case mPool of
        Just pool -> liftIO (getParameterChangesResponse pool perpsClient cfg releaseId (maybe 200 id parsedLimit) cursor) >>= handleResult
        Nothing -> protocolInsightsUnavailable
      (Nothing, _) -> handleError $ E.invalidAmount "limit must be a non-negative integer"
      (_, Nothing) -> handleError $ E.invalidAmount "cursor is invalid"

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

  get "/api/perps/basket/history" $ do
    params <- basketHistoryParams
    case mPool of
      Just pool -> do
        result <- liftIO $ getBasketHistory pool cfg params
        handleResult result
      Nothing ->
        handleServiceUnavailable $
          E.internalError "DATABASE_URL is not configured; perps basket history is unavailable"

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
        result <- liftIO $ getCachedLatestPythUpdate pool perpsClient cfg
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

basketHistoryParams :: ActionM BasketHistoryParams
basketHistoryParams = do
  mRange <- queryParamMaybe "range"
  mInterval <- queryParamMaybe "interval"
  mIncludeComponents <- queryParamMaybe "includeComponents"
  pure
    defaultBasketHistoryParams
      { bhpRange = maybe (bhpRange defaultBasketHistoryParams) normalizeRange mRange
      , bhpIntervalSeconds = maybe (bhpIntervalSeconds defaultBasketHistoryParams) (max 60 . parseIntegerOr 60) mInterval
      , bhpIncludeComponents = maybe (bhpIncludeComponents defaultBasketHistoryParams) parseBool mIncludeComponents
      }
  where
    normalizeRange :: Text -> Text
    normalizeRange range =
      case T.toLower (T.strip range) of
        "24h" -> "24h"
        "30d" -> "30d"
        "1y" -> "1y"
        _ -> "7d"

    parseIntegerOr :: Integer -> Text -> Integer
    parseIntegerOr def txt = maybe def id (readMaybeInteger txt)

    parseBool :: Text -> Bool
    parseBool value =
      case T.toLower (T.strip value) of
        "1" -> True
        "true" -> True
        "yes" -> True
        _ -> False

    readMaybeInteger :: Text -> Maybe Integer
    readMaybeInteger txt =
      let stripped = T.strip txt
       in if T.all (\c -> c >= '0' && c <= '9') stripped && not (T.null stripped)
            then Just $ read $ T.unpack stripped
            else Nothing

handleResult :: (ToJSON a) => Either ApiError a -> ActionM ()
handleResult = \case
  Right response -> do
    setHeader "Content-Type" "application/json"
    status status200
    json response
  Left err -> handleError err

handleError :: ApiError -> ActionM ()
handleError err = do
  setHeader "Content-Type" "application/json"
  status $
    case E.errCode err of
      E.RateLimited -> status429
      E.RpcError -> status503
      E.NetworkError -> status503
      E.NotFound -> status404
      E.InternalError -> status500
      _ -> status400
  json err

handleServiceUnavailable :: ApiError -> ActionM ()
handleServiceUnavailable err = do
  setHeader "Content-Type" "application/json"
  status status503
  json err

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

parseDatabaseBigInt :: Text -> Maybe Integer
parseDatabaseBigInt txt = do
  value <- parseAmount txt
  if value <= 2 ^ (63 :: Int) - 1
    then Just value
    else Nothing

insightsUnavailable :: ActionM ()
insightsUnavailable =
  handleServiceUnavailable $
    E.internalError "DATABASE_URL is not configured; Plether Insights is unavailable"

protocolInsightsUnavailable :: ActionM ()
protocolInsightsUnavailable =
  handleServiceUnavailable $
    E.internalError "Protocol Insights is temporarily unavailable"

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

parseProtocolCursor :: Text -> Maybe ProtocolCursor
parseProtocolCursor = decodeProtocolCursor

parseTrancheHistoryCursor :: Text -> Maybe TrancheHistoryCursor
parseTrancheHistoryCursor = decodeTrancheHistoryCursor

parseProtocolOrderId :: Text -> Maybe Integer
parseProtocolOrderId txt = do
  value <- parseAmount txt
  -- The contract identity is uint64, while the current projection stores it
  -- in PostgreSQL BIGINT. Enforce the narrower storage boundary before either
  -- querying the database or encoding calldata.
  if value <= min maxUint64Value maxDatabaseBigInt
    then Just value
    else Nothing

maxUint64Value :: Integer
maxUint64Value = 2 ^ (64 :: Int) - 1

maxDatabaseBigInt :: Integer
maxDatabaseBigInt = 2 ^ (63 :: Int) - 1

validateOptionalAddress :: Maybe Text -> Maybe (Maybe Text)
validateOptionalAddress Nothing = Just Nothing
validateOptionalAddress (Just address)
  | isValidAddress address = Just $ Just address
  | otherwise = Nothing

validateOptionalTxHash :: Text -> Maybe Text
validateOptionalTxHash txHash
  | isValidTransactionHash txHash = Just txHash
  | otherwise = Nothing

isValidTransactionHash :: Text -> Bool
isValidTransactionHash value =
  T.length value == 66
    && ("0x" `T.isPrefixOf` value || "0X" `T.isPrefixOf` value)
    && T.all isHexDigitText (T.drop 2 value)
  where
    isHexDigitText char =
      (char >= '0' && char <= '9')
        || (char >= 'a' && char <= 'f')
        || (char >= 'A' && char <= 'F')

isValidKeeperWindow :: Text -> Bool
isValidKeeperWindow value = T.toLower value `elem` ["24h", "7d", "30d"]

protocolExplorerGate :: Bool -> Middleware
protocolExplorerGate explorerEnabled downstream request respond
  | explorerEnabled
      || not (isProtocolExplorerReadPath $ pathInfo request)
      || pathInfo request == currentReleasePath =
      downstream request respond
  | otherwise =
      respond $
        responseLBS
          status404
          [ (hContentType, "application/json")
          , (hCacheControl, "no-store")
          ]
          (encode $ E.notFound "Protocol explorer is disabled")
  where
    currentReleasePath = ["api", "insights", "v1", "protocol", "releases", "current"]

-- | Prevent clients and intermediary caches from retaining any unsuccessful
-- response, including framework-generated errors and redirects. This
-- middleware must remain the first Scotty middleware so it wraps every other
-- middleware and route.
noStoreErrorResponses :: Middleware
noStoreErrorResponses downstream request respond =
  downstream request $ \response ->
    let code = statusCode $ responseStatus response
     in respond $
          if code >= 200 && code < 300
            then response
            else mapResponseHeaders replaceCacheControl response
 where
  replaceCacheControl headers =
    (hCacheControl, "no-store")
      : filter ((/= hCacheControl) . fst) headers

-- | Refuse every protocol explorer read unless the perps provider is
-- positively bound to the chain declared by the selected current release.
-- This runs before route handlers, so a mismatch cannot trigger an eth_call
-- that is subsequently labeled as exact release state.
protocolRpcChainGate :: EthClient -> Integer -> Middleware
protocolRpcChainGate client =
  protocolRpcChainGateWith (ethChainId client)

protocolRpcChainGateWith
  :: IO (Either RpcError Integer)
  -> Integer
  -> Middleware
protocolRpcChainGateWith readChainId expectedChainId downstream request respond
  | requestMethod request /= methodGet
      || not (isProtocolExplorerReadPath $ pathInfo request) =
      downstream request respond
  | otherwise = do
      binding <- validateRpcChainId expectedChainId <$> readChainId
      case binding of
        Right () -> downstream request respond
        Left failure ->
          respond $
            responseLBS
              status503
              [ (hContentType, "application/json")
              , (hCacheControl, "no-store")
              ]
              (encode $ rpcChainBindingError failure)

rpcChainBindingError :: RpcChainBindingError -> ApiError
rpcChainBindingError failure =
  (E.networkError "Protocol release RPC chain could not be verified")
    { E.errDetails =
        Just $
          object
            [ "availability"
                .= [ object
                       [ "field" .= ("rpcChainId" :: Text)
                       , "reason" .= rpcChainBindingReason failure
                       ]
                   ]
            ]
    }

rpcChainBindingReason :: RpcChainBindingError -> Text
rpcChainBindingReason = \case
  RpcChainIdUnavailable -> "rpc_chain_id_unavailable"
  RpcChainIdMismatch -> "rpc_chain_id_mismatch"

isProtocolExplorerReadPath :: [Text] -> Bool
isProtocolExplorerReadPath path =
  ["api", "insights", "v1", "protocol", "releases"] `isPrefixOfPath` path
  where
    isPrefixOfPath [] _ = True
    isPrefixOfPath _ [] = False
    isPrefixOfPath (expected : restExpected) (actual : restActual) =
      expected == actual && isPrefixOfPath restExpected restActual

corsMiddleware :: Config -> Middleware
corsMiddleware cfg = cors $ const $ Just policy
  where
    origins = cfgCorsOrigins cfg

    policy =
      simpleCorsResourcePolicy
        { corsOrigins = Just (map encodeUtf8 origins, True)
        , corsMethods = ["GET", "POST", "OPTIONS"]
        , corsRequestHeaders = ["Content-Type", "Authorization"]
        }

    encodeUtf8 :: Text -> Data.ByteString.ByteString
    encodeUtf8 = Data.Text.Encoding.encodeUtf8
