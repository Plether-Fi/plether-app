module Plether.Handlers.InsightsRegistrationSpec (spec) where

import Data.Aeson (Value (..), toJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types.Header (RequestHeaders)
import qualified Network.Wai as Wai
import Plether.AA.Pimlico (OwnedTradingAccountFailure (..))
import qualified Plether.Database.Insights.Registration as Db
import Plether.Handlers.InsightsRegistration
  ( XFollowFailureDisposition (..)
  , canonicalBlockLookupParams
  , completionResultDecision
  , csrfTokenFromRequest
  , maximumRegistrationBodyBytes
  , ownedAccountDecision
  , parseCanonicalRpcQuantity
  , readBoundedRequestBody
  , registrationUiRedirect
  , sessionTokenFromRequest
  , validateJsonRequest
  , validateOrigin
  , xAccountAgeEligible
  , xFollowFailureDisposition
  )
import Plether.Insights.Registration.Config (RegistrationConfig (..))
import Plether.Insights.Registration.Provider (XFollowVerificationFailure (..))
import Plether.Insights.Registration.Types
  ( RegistrationError (..)
  , RegistrationErrorCode (..)
  , registrationError
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "X follow failure state preservation" $ do
    it "releases retryable attempts but resets definitively invalid credentials" $ do
      xFollowFailureDisposition
        (XFollowVerificationFailure (registrationError ProviderUnavailable "retryable") False)
        `shouldBe` ReleaseXFollowAttempt
      xFollowFailureDisposition
        (XFollowVerificationFailure (registrationError ProviderUnavailable "invalid credential") True)
        `shouldBe` ResetXIdentity

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

    it "requires an EOA owner but does not reject a deployed Trading Account" $ do
      leftCode (ownedAccountDecision $ Left OwnerWalletIsContract)
        `shouldBe` Just InvalidRequest
      leftCode (ownedAccountDecision $ Left OwnedTradingAccountProofUnavailable)
        `shouldBe` Just ProviderUnavailable
      ownedAccountDecision (Right $ T.toLower ownerAddress)
        `shouldBe` Right (T.toLower ownerAddress)

  describe "X account age boundary" $
    it "accepts the exact 30-day cutoff and rejects the next second" $ do
      let competition =
            Db.RegistrationCompetition
              { Db.rgcSlug = competitionSlug
              , Db.rgcChainId = 421_614
              , Db.rgcStartTimestamp = 1_789_329_600
              , Db.rgcRegistrationOpenTimestamp = Just 1_788_000_000
              , Db.rgcRegistrationCloseTimestamp = 1_789_934_400
              , Db.rgcMinimumXAccountAgeDays = 30
              , Db.rgcTargetXHandle = "plether_fi"
              , Db.rgcRulesVersion = "rules-v1"
              , Db.rgcPrivacyNoticeVersion = Just "privacy-v1"
              , Db.rgcFinalized = False
              }
          cutoff = Db.rgcStartTimestamp competition - 30 * 86_400
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
        , (Db.CompletionWalletProofChanged, RegistrationIncomplete)
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

competitionSlug :: T.Text
competitionSlug = "testnet-trading-2026-09"

ownerAddress :: T.Text
ownerAddress = "0x7E5F4552091A69125D5DFCB7B8C2659029395BDF"

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
    , rcMinimumXAccountAgeDays = 30
    }
