module Plether.Api
  ( app
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON, withObject, (.:))
import qualified Data.ByteString
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding
import Network.HTTP.Types.Status (status200, status400, status429, status503)
import Network.HTTP.Client (Manager)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (..)
  , cors
  , simpleCorsResourcePolicy
  )
import Plether.Cache (AppCache)
import Plether.Config (Config (..))
import Plether.Ethereum.Client (EthClient)
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
import Plether.Database (DbPool)
import Plether.Handlers.TestnetFaucet (claimTestnetFaucet)
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

app :: AppCache -> EthClient -> EthClient -> Config -> Maybe DbPool -> Manager -> ScottyM ()
app cache client perpsClient cfg mPool manager = do
  middleware $ corsMiddleware cfg

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
            result <- liftIO $ getRevealPayload pool cfg orderId minPublishTime maxPublishTime
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
        result <- liftIO $ getPythUpdate cache manager cfg mTs
        handleResult result
      Nothing ->
        handleError $ E.invalidAmount "publishTime must be a positive integer"

  get "/api/perps/pyth/cached-latest" $ do
    case mPool of
      Just pool -> do
        result <- liftIO $ getCachedLatestPythUpdate pool cfg
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
  pure
    defaultBasketHistoryParams
      { bhpRange = maybe (bhpRange defaultBasketHistoryParams) normalizeRange mRange
      , bhpIntervalSeconds = maybe (bhpIntervalSeconds defaultBasketHistoryParams) (max 60 . parseIntegerOr 60) mInterval
      }
  where
    normalizeRange :: Text -> Text
    normalizeRange range =
      case T.toLower (T.strip range) of
        "24h" -> "24h"
        "30d" -> "30d"
        _ -> "7d"

    parseIntegerOr :: Integer -> Text -> Integer
    parseIntegerOr def txt = maybe def id (readMaybeInteger txt)

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
