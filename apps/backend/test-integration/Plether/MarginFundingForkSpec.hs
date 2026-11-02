module Plether.MarginFundingForkSpec (marginFundingForkSpec) where

import Control.Concurrent.Async (replicateConcurrently)
import Control.Exception (bracket, finally)
import Control.Monad (void, forM_, when)
import Data.Either (isLeft)
import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Client as HTTP
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString
import qualified Data.ByteString.Base16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Unique (hashUnique, newUnique)
import Database.PostgreSQL.Simple (Only (..), close, connectPostgreSQL, execute_, query_)
import Database.PostgreSQL.Simple.Types (Query (..))
import Plether.Config (Config (..), FaucetGuardConfig (..))
import Plether.Database (DbPool, newDbPool, destroyDbPool, withDb)
import Plether.Database.Schema (ensureTestnetFaucetSchema)
import Plether.Database.TestnetFunding
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeCall)
import Plether.Ethereum.Client (CallParams (..), ethCall, newClient, rpcCall)
import Plether.Ethereum.Rpc (ethGetBalance, ethGetCode)
import Plether.Ethereum.Transaction (deriveAddress)
import qualified Plether.Handlers.TestnetFaucet as Legacy
import Plether.Handlers.TestnetMarginFunding
import Plether.Perps.CriticalPathSpec (testConfig, makeApiApplication)
import qualified Network.Wai.Test as WT
import Network.HTTP.Types (status200, status400, status403, status404, status422, status429)
import qualified Plether.Perps.Manifest as Manifest
import Plether.Types (ApiResponse (..))
import System.Environment (lookupEnv)
import Test.Hspec

marginFundingForkSpec :: Text -> Spec
marginFundingForkSpec databaseUrl = do
  describe "direct margin funding API" $ do
    it "authenticates both routes, validates request versions and hides no-store status from caches" $
      withFundingDatabase databaseUrl $ \pool -> do
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        let config = apiConfig
        app <- makeApiApplication True manager pool config "http://127.0.0.1:1"
        let headers = [("X-Plether-Faucet-Proxy-Token",origin),("CF-Connecting-IP","203.0.113.4")]
            path = "/api/testnet/margin-funding"
            send method url supplied body = WT.runSession (WT.srequest $ WT.SRequest
              ((WT.setPath Wai.defaultRequest url) {Wai.requestMethod=method,Wai.requestHeaders=supplied}) body) app
        rejected <- send "POST" path [] "{}"
        WT.simpleStatus rejected `shouldBe` status403
        rejectedRead <- send "GET" (path <> "/a6415ba2-5668-4ba5-a058-f627bd1b938d") [] ""
        WT.simpleStatus rejectedRead `shouldBe` status403
        mapM_ (\body -> do
          invalid <- send "POST" path headers body
          WT.simpleStatus invalid `shouldBe` status422)
          ["{\"version\":2,\"owner\":\"0x1\",\"address\":\"0x2\"}",
           "{\"version\":1,\"owner\":\"0x1\",\"address\":\"0x2\",\"unknown\":true}"]
        malformed <- send "GET" (path <> "/not-a-uuid") headers ""
        WT.simpleStatus malformed `shouldBe` status400
        missing <- send "GET" (path <> "/a6415ba2-5668-4ba5-a058-f627bd1b938d") headers ""
        WT.simpleStatus missing `shouldBe` status404
        lookup "Cache-Control" (WT.simpleHeaders missing) `shouldBe` Just "no-store"
    it "returns a saved allocation and independently bounds status polling" $
      withFundingDatabase databaseUrl $ \pool -> do
        let config = apiConfig
            address = "0x" <> T.replicate 40 "2"
            signer = "0x" <> T.replicate 40 "5"
        Just claim <- withDb pool $ \conn -> reserveMarginFunding conn $
          FundingClaim "" address address Manifest.settlementTokenAddress 421614 Manifest.marginClearinghouseAddress signer
            Manifest.releaseVersion 100_000_000_000 "mint" "pending" Nothing Nothing Nothing
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        app <- makeApiApplication True manager pool config "http://127.0.0.1:1"
        let readStatus = WT.runSession (WT.request $ (WT.setPath Wai.defaultRequest $
              TE.encodeUtf8 $ "/api/testnet/margin-funding/" <> fcId claim)
              {Wai.requestHeaders=[("X-Plether-Faucet-Proxy-Token",origin),("CF-Connecting-IP","203.0.113.4")]}) app
        responses <- sequence $ replicate 60 readStatus
        map WT.simpleStatus responses `shouldBe` replicate 60 status200
        limited <- readStatus
        WT.simpleStatus limited `shouldBe` status429
        lookup "Retry-After" (WT.simpleHeaders limited) `shouldSatisfy` (/= Nothing)
  configured <- runIO $ lookupEnv "MARGIN_FUNDING_FORK_RPC_URL"
  case configured of
    Nothing -> pure ()
    Just endpoint -> describe "direct margin funding fork" $
      it "credits a counterfactual account once without owner ETH, surviving dropped transactions and processor restarts" $
        withFundingDatabase databaseUrl $ \pool -> withAnvilEndpoint endpoint $ \rpcUrl -> do
          if not ("http://127.0.0.1:" `T.isPrefixOf` rpcUrl) then fail "Fork tests require localhost" else pure ()
          client <- newClient rpcUrl
          version <- rpcCall client "web3_clientVersion" (toJSON ([] :: [Value]))
          case version of
            Right (String name) | "anvil" `T.isInfixOf` T.toLower name -> pure ()
            _ -> fail "Refusing to send test transactions to a non-Anvil chain"
          let key = "0x" <> T.replicate 63 "0" <> "1"
              ownerKey = "0x" <> T.replicate 63 "0" <> "2"
          Right signer <- deriveAddress key
          Right owner <- deriveAddress ownerKey
          void $ rpcCall client "anvil_setBalance" $ toJSON [signer,"0x56bc75e2d63100000"]
          void $ rpcCall client "anvil_setBalance" $ toJSON [owner,"0x0"]
          let factory = "0x13e9ed32155810fdbd067d4522c492d6f68e5944"
          Right derived <- ethCall client $ CallParams factory $ encodeCall "getAddress(address,uint256)" [encodeAddress owner, memptyWord]
          let beneficiary = "0x" <> T.takeEnd 40 (hex derived)
              config = (testConfig databaseUrl rpcUrl)
                { cfgPerpsChainId=421614, cfgPerpsSettlementToken=Manifest.settlementTokenAddress
                , cfgPerpsMarginClearinghouse=Manifest.marginClearinghouseAddress
                , cfgFaucetPrivateKey=Just key, cfgTestnetMarginFundingEnabled=True, cfgKeeperConfirmations=1
                }
          ethGetBalance client owner `shouldReturn` Right 0
          ethGetCode client beneficiary `shouldReturn` Right mempty
          requests <- replicateConcurrently 4 $ claimMarginFunding pool client config owner beneficiary
          identifiers <- mapM identifier requests
          identifiers `shouldBe` replicate 4 (head identifiers)
          let claimId = head identifiers
          void $ rpcCall client "evm_setAutomine" $ toJSON [False]
          forM_ ["mint","approval","deposit"] $ \phase -> do
            processMarginFundingOnce pool client config
            Just original <- withDb pool submittedFundingTransaction
            ftPhase original `shouldBe` phase
            when (phase == "mint") $ do
              legacy <- Legacy.claimTestnetFaucet pool client config $ "0x" <> T.replicate 40 "9"
              legacy `shouldSatisfy` isLeft
              hashes <- withDb pool $ \conn -> query_ conn
                "SELECT tx_hash FROM testnet_faucet_claims WHERE delivery_mode='wallet'" :: IO [Only (Maybe Text)]
              hashes `shouldBe` [Only Nothing]
            void $ rpcCall client "anvil_dropTransaction" $ toJSON [ftHash original]
            restarted <- newClient rpcUrl
            processMarginFundingOnce pool restarted config
            Just recovered <- withDb pool submittedFundingTransaction
            ftHash recovered `shouldBe` ftHash original
            -- Mine the receipt and its confirmation, then restart the client
            -- before reconciling. Every phase resumes entirely from the DB.
            forM_ [1 :: Int,2] $ \_ -> void $ rpcCall client "evm_mine" $ toJSON ([] :: [Value])
            fresh <- newClient rpcUrl
            processMarginFundingOnce pool fresh config
          Just completed <- withDb pool $ \conn -> getMarginFunding conn claimId
          fcStatus completed `shouldBe` "funded"
          Right balance <- ethCall client $ CallParams Manifest.marginClearinghouseAddress
            $ encodeCall "balanceAssets(address)" [encodeAddress beneficiary]
          decodeUint256 balance `shouldBe` 100_000_000_000
          ethGetBalance client owner `shouldReturn` Right 0
          ethGetCode client beneficiary `shouldReturn` Right mempty
          repeated <- claimMarginFunding pool client config owner beneficiary
          identifier repeated `shouldReturn` claimId
          processMarginFundingOnce pool client config
          counts <- withDb pool $ \conn -> query_ conn
            "SELECT COUNT(*) FROM testnet_margin_funding_transactions" :: IO [Only Int]
          counts `shouldBe` [Only 3]
  where
    origin = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
    apiConfig = (testConfig databaseUrl "http://127.0.0.1:1")
      { cfgPerpsChainId=421614, cfgPerpsSettlementToken=Manifest.settlementTokenAddress
      , cfgPerpsMarginClearinghouse=Manifest.marginClearinghouseAddress
      , cfgFaucetGuardConfig=Just $ FaucetGuardConfig (TE.decodeUtf8 origin) 20 200
      }
    memptyWord = encodeAddress "0x0000000000000000000000000000000000000000"
    identifier (Right response) = case respData response of
      Object values -> case KM.lookup "claimId" values of
        Just (String value) -> pure value
        _ -> fail "No claim reference"
      _ -> fail "No funding response"
    identifier (Left _) = fail "Funding request failed"

hex :: Data.ByteString.ByteString -> Text
hex = TE.decodeUtf8 . Data.ByteString.Base16.encode

withFundingDatabase :: Text -> (DbPool -> IO a) -> IO a
withFundingDatabase databaseUrl action = bracket (connectPostgreSQL $ TE.encodeUtf8 databaseUrl) close $ \admin -> do
  unique <- hashUnique <$> newUnique
  let schema = "margin_funding_fork_" <> T.pack (show $ abs unique)
      sql = Query . TE.encodeUtf8
      joiner = if "?" `T.isInfixOf` databaseUrl then "&" else "?"
  void $ execute_ admin $ sql $ "CREATE SCHEMA " <> schema
  finally
    (bracket (newDbPool $ databaseUrl <> joiner <> "options=-csearch_path%3D" <> schema <> "%2Cpublic") destroyDbPool $ \pool -> do
      withDb pool $ \conn -> ensureTestnetFaucetSchema conn >> ensureMarginFundingSchema conn
      action pool)
    (void $ execute_ admin $ sql $ "DROP SCHEMA " <> schema <> " CASCADE")

-- Anvil emits numeric l1BlockNumber on fork-mined blocks; Arbitrum RPC uses
-- the canonical hex quantity. Drop this unused optional test field, preserving
-- the production parser's strict rejection of malformed RPC quantities.
withAnvilEndpoint :: String -> (Text -> IO a) -> IO a
withAnvilEndpoint endpoint action = do
  if not ("http://127.0.0.1:" `T.isPrefixOf` T.pack endpoint)
    then fail "Fork tests require a localhost upstream" else pure ()
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  upstream <- HTTP.parseRequest endpoint
  let app request respond = do
        body <- Wai.strictRequestBody request
        response <- HTTP.httpLbs (upstream {HTTP.method="POST", HTTP.requestBody=HTTP.RequestBodyLBS body,
          HTTP.requestHeaders=[("content-type","application/json")]}) manager
        let normalize (Object fields) = Object $ KM.mapWithKey (\key value ->
              if key == "l1BlockNumber" then Null else normalize value) fields
            normalize value = value
            normalized = maybe (HTTP.responseBody response) (Aeson.encode . normalize)
              (Aeson.decode (HTTP.responseBody response) :: Maybe Value)
        respond $ Wai.responseLBS (HTTP.responseStatus response) [("content-type","application/json")] normalized
  testWithApplication (pure app) $ \port -> do
    let rpcUrl = "http://127.0.0.1:" <> T.pack (show port)
    client <- newClient rpcUrl
    version <- rpcCall client "web3_clientVersion" $ toJSON ([] :: [Value])
    case version of
      Right (String name) | "anvil" `T.isInfixOf` T.toLower name -> pure ()
      _ -> fail "Fork tests require Anvil"
    snapshot <- rpcCall client "evm_snapshot" $ toJSON ([] :: [Value])
    case snapshot of
      Right (String _) -> pure ()
      _ -> fail "Unable to snapshot test chain"
    finally (action rpcUrl) $ do
      void $ rpcCall client "evm_setAutomine" $ toJSON [True]
      case snapshot of
        Right identifier -> void $ rpcCall client "evm_revert" $ toJSON [identifier]
        Left _ -> pure ()
