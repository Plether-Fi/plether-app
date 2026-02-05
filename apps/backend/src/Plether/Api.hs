module Plether.Api
  ( app
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import qualified Data.ByteString
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding
import Network.HTTP.Types.Status (status200, status400)
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
import Plether.Types.History (HistoryParams (..), defaultHistoryParams)
import Plether.Types (ApiError)
import qualified Plether.Types.Error as E
import Plether.Utils.Address (isValidAddress)
import Web.Scotty
  ( ActionM
  , ScottyM
  , get
  , json
  , middleware
  , pathParam
  , queryParamMaybe
  , setHeader
  , status
  )

app :: AppCache -> EthClient -> Config -> Maybe DbPool -> ScottyM ()
app cache client cfg mPool = do
  middleware $ corsMiddleware cfg

  get "/api/protocol/status" $ do
    result <- liftIO $ getProtocolStatus cache client cfg
    handleResult result

  get "/api/protocol/config" $ do
    result <- liftIO $ getProtocolConfig cfg
    setHeader "Content-Type" "application/json"
    status status200
    json result

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
    Nothing -> pure ()

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
  status status400
  json err

parseAmount :: Text -> Maybe Integer
parseAmount txt =
  let stripped = T.strip txt
   in if T.all (\c -> c >= '0' && c <= '9') stripped && not (T.null stripped)
        then Just $ read $ T.unpack stripped
        else Nothing

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
