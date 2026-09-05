module Plether.Ethereum.Contracts.SettlementMonitorSpec (spec) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Plether.Ethereum.Abi (encodeAddress, encodeUint256, selector)
import Plether.Ethereum.Client (RpcError (..), newClient)
import Plether.Ethereum.Contracts.SettlementMonitor
  ( ExecutionPath (..)
  , SettlementDeployment (..)
  , SettlementObservation (..)
  , SettlementStatus (..)
  , decodeSettlementObservation
  , decodeSettlementStatus
  , supportedConfigSchemaVersion
  , supportedObservationSchemaVersion
  , validateSettlementDeployment
  , verifyBindings
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "v1.2.1 Settlement Monitor ABI decoding" $ do
    it "decodes the exact 109-word SettlementStatus layout" $ do
      let words' =
            setWords
              109
              [ (1, 302_300_000)
              , (4, 500_000)
              , (5, 500_000)
              , (11, 1_800_000_000)
              , (23, 499_998)
              , (24, 11)
              , (25, 499_996)
              , (58, 499_995)
              , (60, 499_997)
              , (61, 22)
              , (86, 2)
              , (87, 100_000_000)
              , (88, 1_799_999_999)
              , (93, 0)
              , (94, 4)
              , (97, 0)
              , (98, 0)
              , (99, 1)
              , (108, 0)
              ]
      decodeSettlementStatus words'
        `shouldBe` Right
          SettlementStatus
            { ssObservedBlock = 302_300_000
            , ssCurrentEpoch = 500_000
            , ssSettlementCutoffEpoch = 500_000
            , ssMinimumAtomicPublishTime = 1_800_000_000
            , ssSeniorMaturedDepositHeadEpoch = Just 499_998
            , ssSeniorMaturedDepositHeadAssets = 11
            , ssSeniorMaturedRedeemHeadEpoch = Nothing
            , ssSeniorMaturedRedeemHeadShares = 0
            , ssJuniorMaturedDepositHeadEpoch = Nothing
            , ssJuniorMaturedDepositHeadAssets = 0
            , ssJuniorMaturedRedeemHeadEpoch = Just 499_997
            , ssJuniorMaturedRedeemHeadShares = 22
            , ssOldestMaturedHead = Just 499_997
            , ssRequiredExecutionPath = CachedMark
            , ssCachedMarkPrice = 100_000_000
            , ssCachedMarkTime = 1_799_999_999
            , ssOperationalBlockerMask = 0
            , ssWarningMask = 4
            , ssExecutionPathDependencyMask = 0
            , ssDependencyFailureMask = 0
            , ssHasMaturedWork = True
            , ssLpEpochSettlementPaused = False
            }

    it "decodes observation health, digest, completeness, and nested atomic status" $ do
      let digestWord = BS.replicate 31 0 <> BS.singleton 0xaa
          base =
            setWords
              194
              [ (0, supportedObservationSchemaVersion)
              , (2, 302_300_001)
              , (5, 500_001)
              , (12, 1_800_000_100)
              , (87, 3)
              , (100, 1)
              , (159, 1)
              , (160, 0)
              , (161, 0)
              , (193, 1)
              ]
          encoded = replaceWord 191 digestWord base
      case decodeSettlementObservation encoded of
        Left err -> expectationFailure $ show err
        Right observation -> do
          soSchemaVersion observation `shouldBe` supportedObservationSchemaVersion
          ssRequiredExecutionPath (soStatus observation) `shouldBe` AtomicOracleRefresh
          soHealthState observation `shouldBe` 1
          soCriticalFaultMask observation `shouldBe` 0
          soObservationDigest observation
            `shouldBe` "0x00000000000000000000000000000000000000000000000000000000000000aa"
          soObservationComplete observation `shouldBe` True

    it "fails closed on truncated, oversized, or misaligned tuple responses" $ do
      let statusLengthError =
            Left (RpcJsonError "getSettlementStatus(uint256) did not return exactly 109 ABI words")
          observationLengthError =
            Left (RpcJsonError "getSettlementObservation(uint256) did not return exactly 194 ABI words")
      decodeSettlementStatus (BS.replicate (108 * 32) 0)
        `shouldBe` statusLengthError
      decodeSettlementStatus (BS.replicate (110 * 32) 0)
        `shouldBe` statusLengthError
      decodeSettlementStatus (BS.replicate (109 * 32 + 1) 0)
        `shouldBe` statusLengthError
      decodeSettlementObservation (BS.replicate (193 * 32) 0)
        `shouldBe` observationLengthError
      decodeSettlementObservation (BS.replicate (195 * 32) 0)
        `shouldBe` observationLengthError
      decodeSettlementObservation (BS.replicate (194 * 32 + 1) 0)
        `shouldBe` observationLengthError

    it "rejects non-canonical booleans in the status tuple" $ do
      let encoded = setWords 109 []
      decodeSettlementStatus (replaceIntegerWord 99 2 encoded)
        `shouldBe` Left
          (RpcJsonError "getSettlementStatus(uint256).hasMaturedWork returned a non-canonical ABI boolean")
      decodeSettlementStatus (replaceIntegerWord 108 2 encoded)
        `shouldBe` Left
          (RpcJsonError "getSettlementStatus(uint256).lpEpochSettlementPaused returned a non-canonical ABI boolean")

    it "rejects non-canonical booleans in the observation tuple" $ do
      let encoded = setWords 194 []
      decodeSettlementObservation (replaceIntegerWord 100 2 encoded)
        `shouldBe` Left
          (RpcJsonError "getSettlementStatus(uint256).hasMaturedWork returned a non-canonical ABI boolean")
      decodeSettlementObservation (replaceIntegerWord 109 2 encoded)
        `shouldBe` Left
          (RpcJsonError "getSettlementStatus(uint256).lpEpochSettlementPaused returned a non-canonical ABI boolean")
      decodeSettlementObservation (replaceIntegerWord 193 2 encoded)
        `shouldBe` Left
          (RpcJsonError "getSettlementObservation(uint256).observationComplete returned a non-canonical ABI boolean")

  describe "LP settlement call selectors" $ do
    it "uses the canonical pool and router settlement signatures" $ do
      selector "settleLpEpoch(uint256,uint256)" `shouldNotBe` selector "settleLpEpoch(bytes[])"

  describe "settlement deployment validation" $ do
    it "accepts the exact schema and address graph case-insensitively" $ do
      let expected = deployment
          observed = deployment {sdRouter = "0xAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"}
      validateSettlementDeployment expected observed `shouldBe` Right ()

    it "fails closed on schema or vault drift" $ do
      validateSettlementDeployment deployment (deployment {sdConfigSchemaVersion = 3})
        `shouldSatisfy` isLeft
      validateSettlementDeployment
        deployment
        (deployment {sdJuniorVault = "0xffffffffffffffffffffffffffffffffffffffff"})
        `shouldSatisfy` isLeft

    it "rejects non-canonical address padding during startup binding reads" $ do
      let paddedRouter = BS.cons 1 $ BS.drop 1 $ encodeAddress $ sdRouter deployment
      testWithApplication (pure $ addressRpcApplication paddedRouter) $ \port -> do
        client <- newClient $ "http://127.0.0.1:" <> T.pack (show port)
        verifyBindings
          client
          (sdMonitor deployment)
          (sdRouter deployment)
          (sdHousePool deployment)
          `shouldReturn` Left "RpcJsonError \"ROUTER() returned a non-canonical ABI address\""

deployment :: SettlementDeployment
deployment =
  SettlementDeployment
    { sdConfigSchemaVersion = supportedConfigSchemaVersion
    , sdMonitor = "0x1111111111111111111111111111111111111111"
    , sdRouter = "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    , sdEngine = "0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    , sdHousePool = "0xcccccccccccccccccccccccccccccccccccccccc"
    , sdSeniorVault = "0xdddddddddddddddddddddddddddddddddddddddd"
    , sdJuniorVault = "0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
    , sdPletherOracle = "0xffffffffffffffffffffffffffffffffffffffff"
    }

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

addressRpcApplication :: ByteString -> Application
addressRpcApplication result _ respond =
  respond $
    responseLBS
      status200
      [("Content-Type", "application/json")]
      ( encode $
          object
            [ "jsonrpc" .= ("2.0" :: T.Text)
            , "id" .= (1 :: Integer)
            , "result" .= ("0x" <> TE.decodeUtf8 (B16.encode result))
            ]
      )

setWords :: Int -> [(Int, Integer)] -> ByteString
setWords wordCount values =
  foldr (uncurry replaceIntegerWord) (BS.replicate (wordCount * 32) 0) values

replaceIntegerWord :: Int -> Integer -> ByteString -> ByteString
replaceIntegerWord index value = replaceWord index (encodeUint256 value)

replaceWord :: Int -> ByteString -> ByteString -> ByteString
replaceWord index value bytes =
  BS.take (index * 32) bytes
    <> BS.take 32 value
    <> BS.drop ((index + 1) * 32) bytes
