{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PackageImports #-}

-- Isolated process harness: the production reconciler and database operations
-- run unchanged. Only loopback fixture endpoints and a dedicated database are
-- accepted. This executable has no wallet, AWS client, signing key or live RPC.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM_, unless, void)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString as BS
import qualified "base16-bytestring" Data.ByteString.Base16 as B16
import qualified Data.Text as T
import Data.Text (Text)
import Data.String (fromString)
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple
import Plether.AA.Reconciler
import Plether.Config (NativeAaConfig(..), AaRpcMode(..))
import Plether.Database (newDbPool, destroyDbPool)
import Plether.Database.AaSponsorship
import Plether.Ethereum.Abi (encodeCall, encodeAddress, keccak256)
import Plether.Ethereum.Client (newClient, withoutRpcFallback)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [mode, db] | validDb (T.pack db) -> case mode of
      "setup" -> withConn db setup
      "resume-checks" -> withConn db resumeChecks
      "barrier" -> withConn db $ \conn -> do
        void (query_ conn "SELECT pg_advisory_lock(875129401)" :: IO [Only ()])
        putStrLn "barrier-held"
        threadDelay 600_000_000
      _ -> die "unsupported isolated drill mode"
    ["worker", db, first, second] | validDb (T.pack db) && validRpc first && validRpc second ->
      bracket (newDbPool $ T.pack db) destroyDbPool $ \pool -> do
        a <- withoutRpcFallback <$> newClient (T.pack first)
        b <- withoutRpcFallback <$> newClient (T.pack second)
        runAaReconciler pool a b cfg
    _ -> die "Requires a dedicated loopback plether_recovery_drill database and loopback RPC fixture ports"

validDb :: Text -> Bool
validDb raw = case T.stripPrefix "postgresql://127.0.0.1:" raw of
  Just rest -> let (port, name) = T.breakOn "/" rest
    in not (T.null port) && T.all (`elem` ['0'..'9']) port && name == "/plether_recovery_drill"
  _ -> False

validRpc :: String -> Bool
validRpc raw = case T.stripPrefix "http://127.0.0.1:" $ T.pack raw of
  Just port -> not (T.null port) && T.all (`elem` ['0'..'9']) port
  _ -> False

withConn :: String -> (Connection -> IO a) -> IO a
withConn db = bracket (connectPostgreSQL $ TE.encodeUtf8 $ T.pack db) close

hex :: BS.ByteString -> Text
hex = ("0x" <>) . TE.decodeUtf8 . B16.encode

hashOf :: Char -> Text
hashOf c = "0x" <> T.replicate 64 (T.singleton c)

addressOf :: Char -> Text
addressOf c = "0x" <> T.replicate 40 (T.singleton c)

paymaster :: Text
paymaster = addressOf '3'

cfg :: AaReconcilerConfig
cfg = AaReconcilerConfig 421614 paymaster (hex $ keccak256 "\x60\x00") 1 ("0x" <> T.replicate 63 "0" <> "1") 1 1000 1 5 600

setup :: Connection -> IO ()
setup conn = do
  ensureAaSponsorshipSchema conn
  forM_ ["aa-preparation-v1.sql", "aa-preparation-recovery-v1.sql"] $ \migration -> do
    statement <- fromString <$> readFile ("config/migrations/" <> migration)
    void $ execute_ conn statement
  _ <- initializeAaReconcilerCursor conn 421614 paymaster 0 ("0x" <> T.replicate 64 "0")
  recordAaReconcilerHeartbeat conn 421614 paymaster 0 ("0x" <> T.replicate 64 "0")
  request <- resumeRequest conn controlBootstrapReason
  expect "fixture bootstrap" (Right ()) =<< resumeAaIssuance conn request
  now <- floor <$> getPOSIXTime
  forM_ [('1','4','a',7),('2','5','b',8)] $ \(requestChar,digestChar,operationChar,nonce) -> do
    let draft = SponsorshipDraft (hashOf requestChar) (hashOf digestChar) (addressOf '1') (addressOf '2') nonce (now-30) (now+300) 1000 (hashOf '6') (object ["nonce" .= nonce])
    reserved <- reserveSponsorship conn nativeCfg draft
    authorization <- either (die . T.unpack) pure reserved
    stored <- storeSponsorshipSignature conn nativeCfg (saDigest authorization) ("0x" <> T.replicate 130 "a") (hashOf operationChar)
    unless stored $ die "fixture signature storage refused"
    submitted <- markSponsorshipSubmitted conn (saDigest authorization) (hashOf operationChar) (hashOf '6')
    unless submitted $ die "fixture submission marking refused"
  void $ execute_ conn "DELETE FROM aa_reconciler_health"
  LBS.putStrLn $ encode $ object
    [ "paymaster" .= paymaster
    , "sender" .= addressOf '1'
    , "depositCall" .= hex (encodeCall "getDepositInfo(address)" [encodeAddress paymaster])
    , "eventTopic" .= hex (keccak256 "UserOperationEvent(bytes32,address,address,uint256,bool,uint256,uint256)")
    , "entryPoint" .= ("0x4337084d9e255ff0702461cf8895ce9e3b5ff108" :: Text)
    ]

expect :: (Eq a, Show a) => String -> a -> a -> IO ()
expect label wanted actual = unless (actual == wanted) $ die $ label <> ": " <> show actual

resumeRequest :: Connection -> Text -> IO AaIssuanceResumeRequest
resumeRequest conn reason = do
  [Only generation] <- query_ conn "SELECT id FROM aa_sponsorship_control_events WHERE action='pause' ORDER BY id DESC LIMIT 1"
  pure $ AaIssuanceResumeRequest generation reason 421614 paymaster "isolated recovery drill; synthetic data only"

resumeChecks :: Connection -> IO ()
resumeChecks conn = do
  Just reason <- getAaIssuancePause conn
  approved <- resumeRequest conn reason
  void $ execute_ conn "CREATE TEMP TABLE drill_health_backup AS SELECT * FROM aa_reconciler_health"
  void $ execute_ conn "CREATE TEMP TABLE drill_cursor_backup AS SELECT * FROM aa_reconciler_cursor"
  forM_ ["missing", "stale", "future", "cursor mismatch", "different paymaster"] $ \failure -> do
    case failure of
      "missing" -> void $ execute_ conn "DELETE FROM aa_reconciler_health"
      "stale" -> void $ execute_ conn "UPDATE aa_reconciler_health SET last_success_at=clock_timestamp()-interval '121 seconds'"
      "future" -> void $ execute_ conn "UPDATE aa_reconciler_health SET last_success_at=clock_timestamp()+interval '60 seconds'"
      "cursor mismatch" -> void $ execute conn "UPDATE aa_reconciler_cursor SET safe_block_hash=?" (Only $ hashOf 'f')
      _ -> pure ()
    let request = if failure == "different paymaster" then approved {airPaymaster=addressOf 'f'} else approved
    result <- resumeAaIssuance conn request
    expect failure (Left "The configured paymaster reconciliation is not fresh and caught up") result
    expect (failure <> " preserves pause") (Just reason) =<< getAaIssuancePause conn
    void $ execute_ conn "DELETE FROM aa_reconciler_health; INSERT INTO aa_reconciler_health SELECT * FROM drill_health_backup; DELETE FROM aa_reconciler_cursor; INSERT INTO aa_reconciler_cursor SELECT * FROM drill_cursor_backup"
  expect "fresh exact generation resumes" (Right ()) =<< resumeAaIssuance conn approved
  pauseAaIssuance conn reason
  newer <- resumeRequest conn reason
  unless (airExpectedPauseEventId newer > airExpectedPauseEventId approved) $ die "pause generation did not advance"
  expect "superseded same-reason approval refuses" (Left "The expected pause generation does not match the current circuit breaker") =<< resumeAaIssuance conn approved
  expect "new exact generation resumes" (Right ()) =<< resumeAaIssuance conn newer
  LBS.putStrLn $ encode $ object ["resumeGuards" .= (["missing", "stale", "future", "cursor mismatch", "different paymaster", "superseded same-reason generation", "fresh exact generation"] :: [Text]), "passed" .= True]

nativeCfg :: NativeAaConfig
nativeCfg = NativeAaConfig
  { naaProxyOriginToken=T.replicate 64 "1", naaAltoRpcUrl="http://alto.invalid"
  , naaSecurityRpcUrl="https://secondary-rpc.invalid", naaRpcMode=DualIndependent
  , naaMaxSafeLagSeconds=600, naaPaymasterAddress=paymaster, naaPaymasterCodeHash=hashOf '4'
  , naaPolicyId=hashOf '5', naaSignerAddress=addressOf '4', naaKmsKeyId="alias/isolated-drill-unused"
  , naaAccountCodeHash=hashOf '6', naaSponsorshipEnabled=True, naaPreparationEnabled=False
  , naaSubmissionEnabled=True, naaIpRateLimitPerMinute=120, naaFinalRateLimitPerMinute=6
  , naaAccountRateLimitPerMinute=30, naaMaxRequestBytes=262144, naaValiditySeconds=300
  , naaVerificationGasLimit=100000, naaPostOpGasLimit=40000, naaMaxCostWei=10000
  , naaAccountOutstandingWei=20000, naaClientOutstandingWei=20000, naaGlobalOutstandingWei=100000
  , naaAccountHourlyWei=30000, naaGlobalHourlyWei=100000, naaGlobalDailyWei=250000
  , naaCanaryOwners=[addressOf '2'], naaGlobalRolloutEnabled=False
  }
