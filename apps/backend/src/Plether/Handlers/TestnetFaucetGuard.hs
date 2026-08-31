module Plether.Handlers.TestnetFaucetGuard
  ( FaucetClientId (..)
  , FaucetGuardFailure (..)
  , FaucetGuardFailureReason (..)
  , FaucetGuardState
  , FaucetQuotaScope (..)
  , authenticateFaucetRequest
  , checkFaucetRequest
  , faucetGuardFailureReasonText
  , faucetQuotaScopeText
  , newFaucetGuardState
  , newFaucetGuardStateWithClock
  ) where

import Control.Concurrent.STM
  ( TVar
  , atomically
  , newTVarIO
  , readTVar
  , writeTVar
  )
import qualified Data.ByteString.Base16 as B16
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Plether.Config (FaucetGuardConfig (..))
import Plether.Insights.Registration.Crypto (constantTimeEqual, secretDigest)

newtype FaucetClientId = FaucetClientId
  { unFaucetClientId :: Text
  }
  deriving stock (Eq, Ord, Show)

data FaucetQuotaScope
  = FaucetQuotaNone
  | FaucetQuotaOrigin
  | FaucetQuotaClient
  | FaucetQuotaGlobal
  deriving stock (Eq, Show)

data FaucetGuardFailureReason
  = FaucetGuardNotConfigured
  | FaucetOriginTokenMissing
  | FaucetOriginTokenInvalid
  | FaucetClientIpMissing
  | FaucetClientIpInvalid
  | FaucetConfirmationModeUnsupported
  | FaucetClientQuotaExceeded
  | FaucetGlobalQuotaExceeded
  deriving stock (Eq, Show)

data FaucetGuardFailure = FaucetGuardFailure
  { fgfReason :: FaucetGuardFailureReason
  , fgfClientId :: Maybe FaucetClientId
  , fgfQuotaScope :: FaucetQuotaScope
  , fgfRetryAfterSeconds :: Maybe Int
  }
  deriving stock (Eq, Show)

data FaucetQuotaState = FaucetQuotaState
  { fqsGlobalAcceptedAt :: !(Seq POSIXTime)
  , fqsClientAcceptedAt :: !(Map FaucetClientId (Seq POSIXTime))
  }

data FaucetGuardState = FaucetGuardState
  { fgsClock :: !(IO POSIXTime)
  , fgsQuotaState :: !(TVar FaucetQuotaState)
  }

faucetWindowSeconds :: POSIXTime
faucetWindowSeconds = 60 * 60

newFaucetGuardState :: IO FaucetGuardState
newFaucetGuardState = newFaucetGuardStateWithClock getPOSIXTime

newFaucetGuardStateWithClock :: IO POSIXTime -> IO FaucetGuardState
newFaucetGuardStateWithClock clock =
  FaucetGuardState clock
    <$> newTVarIO
      FaucetQuotaState
        { fqsGlobalAcceptedAt = Seq.empty
        , fqsClientAcceptedAt = Map.empty
        }

-- The edge credential is verified before the Cloudflare-provided client IP is
-- parsed or used. A caller that reaches the public ALB directly therefore
-- cannot select a trusted client identity or consume a quota entry.
authenticateFaucetRequest
  :: Maybe FaucetGuardConfig
  -> Maybe Text
  -> Maybe Text
  -> Either FaucetGuardFailure (FaucetGuardConfig, FaucetClientId)
authenticateFaucetRequest maybeConfig suppliedToken suppliedClientIp = do
  config <-
    maybe
      (Left $ forbiddenFailure FaucetGuardNotConfigured)
      Right
      maybeConfig
  token <-
    maybe
      (Left $ forbiddenFailure FaucetOriginTokenMissing)
      Right
      suppliedToken
  if not $ constantTimeTextEqual (fgcProxyOriginToken config) token
    then Left $ forbiddenFailure FaucetOriginTokenInvalid
    else do
      clientIp <-
        maybe
          (Left $ forbiddenFailure FaucetClientIpMissing)
          (maybe (Left $ forbiddenFailure FaucetClientIpInvalid) Right . validateClientIp)
          suppliedClientIp
      Right (config, pseudonymousClientId config clientIp)

-- Every authenticated async request consumes both quotas, including a request
-- that only checks a durable submitted claim. Unsupported clients are rejected
-- before the atomic quota check and before the caller can begin faucet work.
checkFaucetRequest
  :: FaucetGuardState
  -> FaucetGuardConfig
  -> FaucetClientId
  -> Bool
  -> IO (Either FaucetGuardFailure ())
checkFaucetRequest state config clientId acceptsAsync
  | not acceptsAsync =
      pure $
        Left
          FaucetGuardFailure
            { fgfReason = FaucetConfirmationModeUnsupported
            , fgfClientId = Just clientId
            , fgfQuotaScope = FaucetQuotaNone
            , fgfRetryAfterSeconds = Nothing
            }
  | otherwise = do
      now <- fgsClock state
      atomically $ do
        current <- readTVar $ fgsQuotaState state
        let active = expireOldEntries now current
            clientAccepted =
              Map.findWithDefault Seq.empty clientId $ fqsClientAcceptedAt active
        writeTVar (fgsQuotaState state) active
        if Seq.length clientAccepted >= fgcClientRequestsPerHour config
          then
            pure $
              Left $
                quotaFailure
                  FaucetClientQuotaExceeded
                  FaucetQuotaClient
                  clientId
                  (retryAfterSeconds now clientAccepted)
          else if Seq.length (fqsGlobalAcceptedAt active) >= fgcGlobalRequestsPerHour config
            then
              pure $
                Left $
                  quotaFailure
                    FaucetGlobalQuotaExceeded
                    FaucetQuotaGlobal
                    clientId
                    (retryAfterSeconds now $ fqsGlobalAcceptedAt active)
            else do
              let accepted =
                    active
                      { fqsGlobalAcceptedAt = fqsGlobalAcceptedAt active |> now
                      , fqsClientAcceptedAt =
                          Map.insert clientId (clientAccepted |> now) $
                            fqsClientAcceptedAt active
                      }
              writeTVar (fgsQuotaState state) accepted
              pure $ Right ()

expireOldEntries :: POSIXTime -> FaucetQuotaState -> FaucetQuotaState
expireOldEntries now state =
  FaucetQuotaState
    { fqsGlobalAcceptedAt = activeEntries $ fqsGlobalAcceptedAt state
    , fqsClientAcceptedAt = Map.mapMaybe nonEmptyActive $ fqsClientAcceptedAt state
    }
  where
    cutoff = now - faucetWindowSeconds
    activeEntries = Seq.dropWhileL (<= cutoff)
    nonEmptyActive entries =
      case activeEntries entries of
        active | Seq.null active -> Nothing
        active -> Just active

retryAfterSeconds :: POSIXTime -> Seq POSIXTime -> Int
retryAfterSeconds now entries =
  case Seq.lookup 0 entries of
    Nothing -> 1
    Just oldest -> max 1 $ ceiling $ oldest + faucetWindowSeconds - now

forbiddenFailure :: FaucetGuardFailureReason -> FaucetGuardFailure
forbiddenFailure reason =
  FaucetGuardFailure
    { fgfReason = reason
    , fgfClientId = Nothing
    , fgfQuotaScope = FaucetQuotaOrigin
    , fgfRetryAfterSeconds = Nothing
    }

quotaFailure
  :: FaucetGuardFailureReason
  -> FaucetQuotaScope
  -> FaucetClientId
  -> Int
  -> FaucetGuardFailure
quotaFailure reason scope clientId retryAfter =
  FaucetGuardFailure
    { fgfReason = reason
    , fgfClientId = Just clientId
    , fgfQuotaScope = scope
    , fgfRetryAfterSeconds = Just retryAfter
    }

pseudonymousClientId :: FaucetGuardConfig -> Text -> FaucetClientId
pseudonymousClientId config clientIp =
  FaucetClientId $
    TE.decodeUtf8 $
      B16.encode $
        secretDigest
          (TE.encodeUtf8 $ fgcProxyOriginToken config)
          "faucet-client-ip-v1"
          (TE.encodeUtf8 clientIp)

constantTimeTextEqual :: Text -> Text -> Bool
constantTimeTextEqual expected supplied =
  constantTimeEqual (TE.encodeUtf8 expected) (TE.encodeUtf8 supplied)

validateClientIp :: Text -> Maybe Text
validateClientIp raw =
  let value = T.toLower $ T.strip raw
      validChar char =
        (char >= '0' && char <= '9')
          || (char >= 'a' && char <= 'f')
          || char == '.'
          || char == ':'
      hasIpSeparator = T.any (\char -> char == '.' || char == ':') value
   in if
        T.null value
          || T.length value > 45
          || not hasIpSeparator
          || T.any (not . validChar) value
        then Nothing
        else Just value

faucetGuardFailureReasonText :: FaucetGuardFailureReason -> Text
faucetGuardFailureReasonText = \case
  FaucetGuardNotConfigured -> "guard_not_configured"
  FaucetOriginTokenMissing -> "origin_token_missing"
  FaucetOriginTokenInvalid -> "origin_token_invalid"
  FaucetClientIpMissing -> "client_ip_missing"
  FaucetClientIpInvalid -> "client_ip_invalid"
  FaucetConfirmationModeUnsupported -> "confirmation_mode_unsupported"
  FaucetClientQuotaExceeded -> "client_quota_exceeded"
  FaucetGlobalQuotaExceeded -> "global_quota_exceeded"

faucetQuotaScopeText :: FaucetQuotaScope -> Text
faucetQuotaScopeText = \case
  FaucetQuotaNone -> "none"
  FaucetQuotaOrigin -> "origin"
  FaucetQuotaClient -> "client"
  FaucetQuotaGlobal -> "global"
