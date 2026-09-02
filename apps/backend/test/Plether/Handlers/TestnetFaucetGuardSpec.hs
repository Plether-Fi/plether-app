module Plether.Handlers.TestnetFaucetGuardSpec (spec) where

import Control.Monad (replicateM, replicateM_)
import Data.Either (isRight)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime)
import Plether.Config (FaucetGuardConfig (..))
import Plether.Handlers.TestnetFaucetGuard
  ( FaucetClientId (..)
  , FaucetGuardFailure (..)
  , FaucetGuardFailureReason (..)
  , FaucetGuardState
  , FaucetQuotaScope (..)
  , authenticateFaucetRequest
  , checkFaucetRequest
  , newFaucetGuardStateWithClock
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "faucet origin authentication" $ do
    it "accepts a valid edge token and trusted client IP" $
      authenticate testConfig (Just testToken) (Just "203.0.113.8")
        `shouldSatisfy` isRight

    it "fails closed when the guard, token, or trusted IP is missing" $ do
      failureReason (authenticate Nothing (Just testToken) (Just "203.0.113.8"))
        `shouldBe` Just FaucetGuardNotConfigured
      failureReason (authenticate testConfig Nothing (Just "203.0.113.8"))
        `shouldBe` Just FaucetOriginTokenMissing
      failureReason (authenticate testConfig (Just testToken) Nothing)
        `shouldBe` Just FaucetClientIpMissing

    it "rejects an incorrect token before accepting a client identity" $
      failureReason (authenticate testConfig (Just "wrong-origin-token-that-is-long-enough") (Just "203.0.113.8"))
        `shouldBe` Just FaucetOriginTokenInvalid

    it "rejects malformed trusted client IP values" $
      failureReason (authenticate testConfig (Just testToken) (Just "attacker-controlled"))
        `shouldBe` Just FaucetClientIpInvalid

  describe "faucet client identifiers" $ do
    it "is stable, distinct, and never contains the raw IP" $ do
      first <- authenticatedClient "203.0.113.8"
      repeated <- authenticatedClient "203.0.113.8"
      second <- authenticatedClient "203.0.113.9"
      first `shouldBe` repeated
      first `shouldNotBe` second
      let firstDigest = unFaucetClientId first
      T.length firstDigest `shouldBe` 64
      firstDigest `shouldSatisfy` T.all (\char -> char `elem` (['0' .. '9'] <> ['a' .. 'f']))
      firstDigest `shouldNotSatisfy` T.isInfixOf "203.0.113.8"

  describe "faucet hourly quotas" $ do
    it "allows twenty client requests and rejects the twenty-first" $ do
      (state, _) <- stateAt 0
      client <- authenticatedClient "203.0.113.8"
      results <- replicateM 20 $ checkFaucetRequest state defaultConfig client True
      results `shouldSatisfy` all isRight
      checkFaucetRequest state defaultConfig client True
        `shouldReturn` Left
          FaucetGuardFailure
            { fgfReason = FaucetClientQuotaExceeded
            , fgfClientId = Just client
            , fgfQuotaScope = FaucetQuotaClient
            , fgfRetryAfterSeconds = Just 3600
            }

    it "allows two hundred global requests and rejects the next client" $ do
      (state, _) <- stateAt 0
      clients <- mapM (authenticatedClient . testIpv6) [1 .. 201]
      results <- mapM (\client -> checkFaucetRequest state defaultConfig client True) $ take 200 clients
      results `shouldSatisfy` all isRight
      checkFaucetRequest state defaultConfig (clients !! 200) True
        `shouldReturn` Left
          FaucetGuardFailure
            { fgfReason = FaucetGlobalQuotaExceeded
            , fgfClientId = Just (clients !! 200)
            , fgfQuotaScope = FaucetQuotaGlobal
            , fgfRetryAfterSeconds = Just 3600
            }

    it "returns an accurate retry delay and resets after one hour" $ do
      (state, clock) <- stateAt 0
      client <- authenticatedClient "203.0.113.8"
      replicateM_ 20 $ checkFaucetRequest state defaultConfig client True
      writeIORef clock 30
      result <- checkFaucetRequest state defaultConfig client True
      fmap fgfRetryAfterSeconds (either Just (const Nothing) result)
        `shouldBe` Just (Just 3570)
      writeIORef clock 3600
      checkFaucetRequest state defaultConfig client True `shouldReturn` Right ()

    it "does not consume quota for an unsupported confirmation mode" $ do
      (state, _) <- stateAt 0
      client <- authenticatedClient "203.0.113.8"
      checkFaucetRequest state defaultConfig client False
        `shouldReturn` Left
          FaucetGuardFailure
            { fgfReason = FaucetConfirmationModeUnsupported
            , fgfClientId = Just client
            , fgfQuotaScope = FaucetQuotaNone
            , fgfRetryAfterSeconds = Nothing
            }
      results <- replicateM 20 $ checkFaucetRequest state defaultConfig client True
      results `shouldSatisfy` all isRight

    it "does not increment counters for a quota rejection" $ do
      (state, _) <- stateAt 0
      firstClient <- authenticatedClient "203.0.113.8"
      secondClient <- authenticatedClient "203.0.113.9"
      let oneGlobalRequest = defaultConfig {fgcGlobalRequestsPerHour = 1}
          twoGlobalRequests = defaultConfig {fgcGlobalRequestsPerHour = 2}
      checkFaucetRequest state oneGlobalRequest firstClient True `shouldReturn` Right ()
      rejection <- checkFaucetRequest state oneGlobalRequest secondClient True
      case rejection of
        Left failure -> fgfReason failure `shouldBe` FaucetGlobalQuotaExceeded
        Right () -> expectationFailure "the full global quota accepted another request"
      checkFaucetRequest state twoGlobalRequests secondClient True `shouldReturn` Right ()

authenticate
  :: Maybe FaucetGuardConfig
  -> Maybe T.Text
  -> Maybe T.Text
  -> Either FaucetGuardFailure (FaucetGuardConfig, FaucetClientId)
authenticate = authenticateFaucetRequest

failureReason
  :: Either FaucetGuardFailure (FaucetGuardConfig, FaucetClientId)
  -> Maybe FaucetGuardFailureReason
failureReason = either (Just . fgfReason) (const Nothing)

authenticatedClient :: T.Text -> IO FaucetClientId
authenticatedClient ip =
  case authenticate testConfig (Just testToken) (Just ip) of
    Right (_, clientId) -> pure clientId
    Left failure -> expectationFailure ("authentication failed: " <> show failure) >> fail "unreachable"

stateAt :: POSIXTime -> IO (FaucetGuardState, IORef POSIXTime)
stateAt initial = do
  clock <- newIORef initial
  state <- newFaucetGuardStateWithClock $ readIORef clock
  pure (state, clock)

testIpv6 :: Int -> T.Text
testIpv6 value = T.pack $ "2001:db8::" <> show value

testConfig :: Maybe FaucetGuardConfig
testConfig = Just defaultConfig

defaultConfig :: FaucetGuardConfig
defaultConfig =
  FaucetGuardConfig
    { fgcProxyOriginToken = testToken
    , fgcClientRequestsPerHour = 20
    , fgcGlobalRequestsPerHour = 200
    }

testToken :: T.Text
testToken = "0123456789abcdef0123456789abcdef0123456789abcdef"
