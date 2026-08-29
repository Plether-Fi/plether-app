module Plether.Handlers.InsightsRegistrationSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types.Header (RequestHeaders)
import qualified Network.Wai as Wai
import Plether.AA.Pimlico (UndeployedTradingAccountFailure (..))
import Plether.Config
  ( Config (..)
  , PerpsCandleReadMode (..)
  , PerpsCandleWriteMode (..)
  )
import qualified Plether.Database.Insights.Registration as Db
import Plether.Ethereum.Abi (keccak256)
import Plether.Handlers.InsightsRegistration
  ( canonicalBlockLookupParams
  , completionResultDecision
  , csrfTokenFromRequest
  , fundingActivityFilters
  , maximumRegistrationBodyBytes
  , maximumRegistrationProofScanBlocks
  , parseCanonicalRpcQuantity
  , readBoundedRequestBody
  , registrationIndexerCursorRangeValid
  , registrationUiRedirect
  , releaseActivityFilters
  , sessionTokenFromRequest
  , undeployedAccountDecision
  , validateJsonRequest
  , validateOrigin
  , xAccountAgeEligible
  )
import Plether.Insights.Competition
  ( CompetitionReleaseManifest (..)
  , september2026Competition
  )
import Plether.Insights.Registration.Config (RegistrationConfig (..))
import Plether.Insights.Registration.Types
  ( RegistrationError (..)
  , RegistrationErrorCode (..)
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "registration JSON request boundaries" $ do
    it "accepts only JSON media types without any Content-Encoding" $ do
      validateJsonRequest (jsonRequest "application/json" Nothing $ Wai.KnownLength 2)
        `shouldBe` Right ()
      validateJsonRequest (jsonRequest "Application/JSON;charset=UTF-8" Nothing $ Wai.KnownLength 2)
        `shouldBe` Right ()
      mapM_
        (\contentType ->
          leftCode (validateJsonRequest $ jsonRequest contentType Nothing $ Wai.KnownLength 2)
            `shouldBe` Just InvalidRequest
        )
        [ "text/json"
        , "application/jsonp"
        , "application/json ; charset=utf-8"
        , ""
        ]
      leftCode (validateJsonRequest $ requestWith [] $ Wai.KnownLength 2)
        `shouldBe` Just InvalidRequest
      mapM_
        (\encoding ->
          leftCode
            ( validateJsonRequest $
                jsonRequest "application/json" (Just encoding) $ Wai.KnownLength 2
            )
            `shouldBe` Just InvalidRequest
        )
        ["gzip", "identity", ""]

    it "enforces the advertised body-length boundary before reading" $ do
      maximumRegistrationBodyBytes `shouldBe` 16 * 1024
      validateJsonRequest
        ( jsonRequest
            "application/json"
            Nothing
            (Wai.KnownLength $ fromIntegral maximumRegistrationBodyBytes)
        )
        `shouldBe` Right ()
      leftCode
        ( validateJsonRequest $
            jsonRequest
              "application/json"
              Nothing
              (Wai.KnownLength $ fromIntegral maximumRegistrationBodyBytes + 1)
        )
        `shouldBe` Just InvalidRequest
      validateJsonRequest (jsonRequest "application/json" Nothing Wai.ChunkedBody)
        `shouldBe` Right ()

    it "aggregates streamed chunks up to the exact cap" $ do
      let first = BS.replicate 8192 0x61
          second = BS.replicate 8192 0x62
      waiRequest <- requestWithBodyChunks [first, second]
      readBoundedRequestBody maximumRegistrationBodyBytes waiRequest
        `shouldReturn` Right (LBS.fromChunks [first, second])

    it "rejects a chunked body as soon as the aggregate exceeds the cap" $ do
      waiRequest <-
        requestWithBodyChunks
          [ BS.replicate maximumRegistrationBodyBytes 0x61
          , "x"
          , "unread-tail"
          ]
      readBoundedRequestBody maximumRegistrationBodyBytes waiRequest
        `shouldReturn` Left ()

  describe "registration origin, CSRF, and cookie parsing" $ do
    it "requires the exact configured Origin byte-for-byte" $ do
      validateOrigin registrationConfig (requestWith [("Origin", publicOriginBytes)] Wai.ChunkedBody)
        `shouldBe` Right ()
      mapM_
        (\headers ->
          leftCode (validateOrigin registrationConfig $ requestWith headers Wai.ChunkedBody)
            `shouldBe` Just OriginRejected
        )
        [ []
        , [("Origin", "https://evil.invalid")]
        , [("Origin", "https://insights.plether.com/")]
        , [("Origin", "https://Insights.plether.com")]
        ]

    it "accepts only a mandatory canonical 43-character CSRF header" $ do
      csrfTokenFromRequest (requestWith [("X-Registration-CSRF", csrfBytes)] Wai.ChunkedBody)
        `shouldBe` Just csrfToken
      mapM_
        (\headers -> csrfTokenFromRequest (requestWith headers Wai.ChunkedBody) `shouldBe` Nothing)
        [ []
        , [("X-Registration-CSRF", BS.take 42 csrfBytes)]
        , [("X-Registration-CSRF", csrfBytes <> "x")]
        , [("X-Registration-CSRF", BS.take 42 csrfBytes <> "=")]
        , [("X-Registration-CSRF", BS.take 42 csrfBytes <> ".")]
        ]

    it "parses one canonical host-only session cookie and rejects ambiguity" $ do
      sessionTokenFromRequest
        ( requestWith
            [("Cookie", "theme=dark; __Host-plether_registration=" <> sessionBytes <> "; accepted=true")]
            Wai.ChunkedBody
        )
        `shouldBe` Just sessionToken
      sessionTokenFromRequest
        ( requestWith
            [ ( "Cookie"
              , "__Host-plether_registration="
                  <> sessionBytes
                  <> "; __Host-plether_registration="
                  <> sessionBytes
              )
            ]
            Wai.ChunkedBody
        )
        `shouldBe` Nothing
      sessionTokenFromRequest
        (requestWith [("Cookie", "__Host-plether_registration=too-short")] Wai.ChunkedBody)
        `shouldBe` Nothing

  describe "OAuth callback redirect" $ do
    it "always returns the fixed clean registration route" $ do
      let redirect = registrationUiRedirect registrationConfig competitionSlug
      redirect
        `shouldBe` "https://insights.plether.com/competitions/testnet-trading-2026-09/register"
      redirect `shouldNotSatisfy` T.any (`elem` ['?', '#'])

  describe "account-proof RPC construction" $ do
    it "keeps uncovered proof scans within the fixed bounded window" $ do
      maximumRegistrationProofScanBlocks `shouldBe` 2048

    it "constructs canonical non-hydrated block lookups" $ do
      canonicalBlockLookupParams 0
        `shouldBe` toJSON [String "0x0", Bool False]
      canonicalBlockLookupParams 421_614
        `shouldBe` toJSON [String "0x66eee", Bool False]

    it "parses only canonical lowercase JSON-RPC quantities" $ do
      parseCanonicalRpcQuantity (String "0x0") `shouldBe` Right 0
      parseCanonicalRpcQuantity (String "0x66eee") `shouldBe` Right 421_614
      mapM_
        (\value -> parseCanonicalRpcQuantity value `shouldSatisfy` isLeft)
        [ String "0x"
        , String "0x00"
        , String "0x01"
        , String "0X1"
        , String "0xA"
        , String "0xgg"
        , Number 1
        , Null
        ]

    it "constructs exact release emitters, account topics, and cursor range" $ do
      let (topicOneFilter, orderFilter) =
            releaseActivityFilters filterConfig ownerAddress 1000 1999
          (transferFromFilter, transferToFilter) =
            fundingActivityFilters filterConfig ownerAddress 1000 1999
          accountTopic = "0x" <> T.replicate 24 "0" <> T.drop 2 (T.toLower ownerAddress)
          accountTopicOneEvents =
            map
              eventTopic
              [ "PositionOpened(address,uint8,uint256,uint256,uint256)"
              , "PositionClosed(address,uint8,uint256,uint256,int256)"
              , "PositionLiquidated(address,uint8,uint256,uint256,uint256)"
              , "MarginAdded(address,uint256)"
              , "Deposit(address,address,uint256)"
              , "Withdraw(address,address,uint256)"
              ]
      topicOneFilter
        `shouldBe`
          object
            [ "address" .= ([orderRouter, cfdEngine, marginClearinghouse] :: [T.Text])
            , "topics" .= [toJSON accountTopicOneEvents, String accountTopic]
            , "fromBlock" .= ("0x3e8" :: T.Text)
            , "toBlock" .= ("0x7cf" :: T.Text)
            ]
      orderFilter
        `shouldBe`
          object
            [ "address" .= orderRouter
            , "topics"
                .= [ String $ eventTopic "OrderCommitted(uint64,address,uint8)"
                   , Null
                   , String accountTopic
                   ]
            , "fromBlock" .= ("0x3e8" :: T.Text)
            , "toBlock" .= ("0x7cf" :: T.Text)
            ]
      transferFromFilter
        `shouldBe`
          object
            [ "address" .= usdc
            , "topics"
                .= [ String $ eventTopic "Transfer(address,address,uint256)"
                   , String accountTopic
                   ]
            , "fromBlock" .= ("0x3e8" :: T.Text)
            , "toBlock" .= ("0x7cf" :: T.Text)
            ]
      transferToFilter
        `shouldBe`
          object
            [ "address" .= usdc
            , "topics"
                .= [ String $ eventTopic "Transfer(address,address,uint256)"
                   , Null
                   , String accountTopic
                   ]
            , "fromBlock" .= ("0x3e8" :: T.Text)
            , "toBlock" .= ("0x7cf" :: T.Text)
            ]

    it "requires the persisted indexer lower bound and a fully covered bounded gap" $ do
      registrationIndexerCursorRangeValid 100 100 200 199 `shouldBe` True
      registrationIndexerCursorRangeValid 100 99 200 199 `shouldBe` False
      registrationIndexerCursorRangeValid 100 101 200 199 `shouldBe` False
      registrationIndexerCursorRangeValid 100 100 99 99 `shouldBe` False
      registrationIndexerCursorRangeValid 100 100 2200 100 `shouldBe` False
      registrationIndexerCursorRangeValid 0 0 0 0 `shouldBe` False

    it "distinguishes deployed owners/accounts from provider proof outages" $ do
      leftCode (undeployedAccountDecision $ Left OwnerAddressAlreadyDeployed)
        `shouldBe` Just TradingAccountExists
      leftCode (undeployedAccountDecision $ Left TradingAccountAlreadyDeployed)
        `shouldBe` Just TradingAccountExists
      leftCode (undeployedAccountDecision $ Left UndeployedTradingAccountProofUnavailable)
        `shouldBe` Just ProviderUnavailable
      undeployedAccountDecision (Right $ T.toLower ownerAddress)
        `shouldBe` Right (T.toLower ownerAddress)

  describe "X account age boundary" $
    it "accepts the exact 90-day cutoff and rejects the next second" $ do
      let competition =
            Db.RegistrationCompetition
              { Db.rgcSlug = competitionSlug
              , Db.rgcChainId = 421_614
              , Db.rgcReleaseRouter = orderRouter
              , Db.rgcUsdcAddress = usdc
              , Db.rgcReleaseManifest = "manifest"
              , Db.rgcStartTimestamp = 1_789_329_600
              , Db.rgcRegistrationOpenTimestamp = Just 1_788_000_000
              , Db.rgcRegistrationCloseTimestamp = 1_789_934_400
              , Db.rgcMinimumXAccountAgeDays = 90
              , Db.rgcTargetXHandle = "plether_fi"
              , Db.rgcRulesVersion = "rules-v1"
              , Db.rgcPrivacyNoticeVersion = Just "privacy-v1"
              , Db.rgcFinalized = False
              }
          cutoff = Db.rgcStartTimestamp competition - 90 * 86_400
      xAccountAgeEligible competition cutoff `shouldBe` True
      xAccountAgeEligible competition (cutoff + 1) `shouldBe` False

  describe "registration completion outcomes" $ do
    it "treats both first completion and an already-completed retry as success" $ do
      completionResultDecision Db.CompletionSucceeded `shouldBe` Right ()
      completionResultDecision Db.CompletionAlreadySucceeded `shouldBe` Right ()

    it "maps each transactional refusal to its stable public error code" $ do
      mapM_
        (\(result, expectedCode) ->
          leftCode (completionResultDecision result) `shouldBe` Just expectedCode
        )
        [ (Db.CompletionClosed, ClosedRegistration)
        , (Db.CompletionIncomplete, RegistrationIncomplete)
        , (Db.CompletionDuplicate, DuplicateRegistration)
        , (Db.CompletionTradingAccountUsed, TradingAccountExists)
        ]

jsonRequest :: BS.ByteString -> Maybe BS.ByteString -> Wai.RequestBodyLength -> Wai.Request
jsonRequest contentType maybeEncoding lengthValue =
  requestWith
    ([("Content-Type", contentType)] <> maybe [] (\value -> [("Content-Encoding", value)]) maybeEncoding)
    lengthValue

requestWith :: RequestHeaders -> Wai.RequestBodyLength -> Wai.Request
requestWith headers lengthValue =
  Wai.defaultRequest
    { Wai.requestHeaders = headers
    , Wai.requestBodyLength = lengthValue
    }

requestWithBodyChunks :: [BS.ByteString] -> IO Wai.Request
requestWithBodyChunks chunks = do
  chunksRef <- newIORef chunks
  let nextChunk =
        atomicModifyIORef' chunksRef $ \remaining -> case remaining of
          [] -> ([], BS.empty)
          chunk : rest -> (rest, chunk)
  pure $ Wai.setRequestBodyChunks nextChunk Wai.defaultRequest

leftCode :: Either RegistrationError a -> Maybe RegistrationErrorCode
leftCode result = case result of
  Left err -> Just $ reCode err
  Right _ -> Nothing

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

eventTopic :: T.Text -> T.Text
eventTopic signature =
  "0x" <> TE.decodeUtf8 (B16.encode $ keccak256 $ TE.encodeUtf8 signature)

competitionSlug :: T.Text
competitionSlug = "testnet-trading-2026-09"

ownerAddress :: T.Text
ownerAddress = "0x7E5F4552091A69125D5DFCB7B8C2659029395BDF"

orderRouter :: T.Text
orderRouter = "0xaa00000000000000000000000000000000000001"

usdc :: T.Text
usdc = "0x1100000000000000000000000000000000000007"

cfdEngine :: T.Text
cfdEngine = "0xbb00000000000000000000000000000000000002"

marginClearinghouse :: T.Text
marginClearinghouse = "0xcc00000000000000000000000000000000000003"

publicOriginBytes :: BS.ByteString
publicOriginBytes = "https://insights.plether.com"

csrfToken :: T.Text
csrfToken = T.replicate 43 "c"

csrfBytes :: BS.ByteString
csrfBytes = TE.encodeUtf8 csrfToken

sessionToken :: T.Text
sessionToken = T.replicate 43 "s"

sessionBytes :: BS.ByteString
sessionBytes = TE.encodeUtf8 sessionToken

registrationConfig :: RegistrationConfig
registrationConfig =
  RegistrationConfig
    { rcActivationEnabled = True
    , rcPublicOrigin = "https://insights.plether.com"
    , rcOriginToken = BS.pack [0 .. 31]
    , rcOriginTokenNext = Nothing
    , rcTurnstileSecretKey = "0x4F7q9Nz3Lp8Rc2Vm6Tk1Ws5Y"
    , rcTurnstileExpectedHostname = "insights.plether.com"
    , rcTurnstileExpectedAction = "competition_registration"
    , rcXClientId = "TjA4bGh6QnV8z2Mf"
    , rcXClientSecret = "Qp7Vn2Ls9Kx4Rm8Tz6Bc"
    , rcXCallbackUrl = "https://insights.plether.com/api/insights/v1/competitions/testnet-trading-2026-09/registrations/x/callback"
    , rcXCallbackCompetitionSlug = competitionSlug
    , rcXTargetUserId = "1234567890123456789"
    , rcXTargetHandle = "plether_fi"
    , rcEmailKeys = Map.fromList [("v1", BS.pack [0 .. 31]), ("v2", BS.pack [64 .. 95])]
    , rcActiveEmailKeyVersion = "v2"
    , rcLookupHmacKey = BS.pack [32 .. 63]
    , rcSessionTtlSeconds = 1800
    , rcIpRateLimitPerMinute = 10
    , rcSessionRateLimitPerMinute = 30
    , rcRulesVersion = "2026-09-v1"
    , rcPrivacyVersion = "2026-09-v1"
    , rcMinimumXAccountAgeDays = 90
    }

filterConfig :: Config
filterConfig =
  Config
    { cfgRpcUrl = ""
    , cfgChainId = 1
    , cfgPort = 3001
    , cfgCorsOrigins = []
    , cfgDeployments = []
    , cfgDatabaseUrl = Nothing
    , cfgIndexerStartBlock = 0
    , cfgPythBenchmarksUrl = ""
    , cfgPythHermesUrl = ""
    , cfgPythApiKey = Nothing
    , cfgPythBackfillDays = 7
    , cfgPythSampleIntervalSeconds = 60
    , cfgPythLatestMaxAgeSeconds = 10
    , cfgPythIngestionEnabled = False
    , cfgPerpsCandleWriteMode = PerpsCandleWritesOff
    , cfgPerpsCandleReadMode = PerpsCandleReadsLegacy
    , cfgPerpsCandleReadIntervals = []
    , cfgPerpsCandleShadowSampleBps = 0
    , cfgPerpsCandleStrictCoverage = True
    , cfgPerpsCandleLatenessSeconds = 120
    , cfgPerpsCandleFinalizationGraceSeconds = 15
    , cfgPerpsRpcUrl = ""
    , cfgPerpsChainId = 421_614
    , cfgPerpsUsdc = T.toUpper usdc
    , cfgPerpsOrderRouter = T.toUpper orderRouter
    , cfgPerpsCfdEngine = T.toUpper cfdEngine
    , cfgPerpsCfdEngineLens = "0xdd00000000000000000000000000000000000004"
    , cfgPerpsCfdEngineSettlementSidecar = "0xee00000000000000000000000000000000000005"
    , cfgPerpsMarginClearinghouse = T.toUpper marginClearinghouse
    , cfgPerpsPletherOracle = ""
    , cfgPerpsAccountLens = ""
    , cfgPerpsIndexerStartBlock = 100
    , cfgInsightsCompetitionRules = september2026Competition
    , cfgInsightsCompetitionReleaseManifest =
        CompetitionReleaseManifest
          { crmReleaseId = "registration-handler-spec"
          , crmChainId = 421_614
          , crmUsdc = usdc
          , crmOrderRouter = orderRouter
          , crmMarginClearinghouse = marginClearinghouse
          , crmAccountLens = "0xcc00000000000000000000000000000000000003"
          , crmCfdEngine = cfdEngine
          , crmCfdEngineLens = "0xdd00000000000000000000000000000000000004"
          , crmSettlementSidecar = "0xee00000000000000000000000000000000000005"
          , crmPletherOracle = "0xff00000000000000000000000000000000000006"
          , crmIndexerStartBlock = 100
          }
    , cfgRegistrationConfig = Nothing
    , cfgAaConfig = Nothing
    , cfgFaucetPrivateKey = Nothing
    , cfgKeeperPrivateKey = Nothing
    , cfgKeeperPollSeconds = 1
    , cfgKeeperMaxBatchSize = 20
    , cfgKeeperConfirmations = 1
    , cfgKeeperGasBufferBps = 2000
    , cfgKeeperFeeBufferBps = 2500
    }
