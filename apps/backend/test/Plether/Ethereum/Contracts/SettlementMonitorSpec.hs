module Plether.Ethereum.Contracts.SettlementMonitorSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Plether.Ethereum.Abi (encodeUint256, selector)
import Plether.Ethereum.Client (RpcError (..))
import Plether.Ethereum.Contracts.SettlementMonitor
  ( ExecutionPath (..)
  , SettlementObservation (..)
  , SettlementStatus (..)
  , decodeSettlementObservation
  , decodeSettlementStatus
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "v1.2.0 Settlement Monitor ABI decoding" $ do
    it "decodes the exact 109-word SettlementStatus layout" $ do
      let words' =
            setWords
              109
              [ (1, 302_300_000)
              , (4, 500_000)
              , (11, 1_800_000_000)
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
            , ssMinimumAtomicPublishTime = 1_800_000_000
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
              [ (0, 1)
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
          soSchemaVersion observation `shouldBe` 1
          ssRequiredExecutionPath (soStatus observation) `shouldBe` AtomicOracleRefresh
          soHealthState observation `shouldBe` 1
          soCriticalFaultMask observation `shouldBe` 0
          soObservationDigest observation
            `shouldBe` "0x00000000000000000000000000000000000000000000000000000000000000aa"
          soObservationComplete observation `shouldBe` True

    it "fails closed on truncated status and observation responses" $ do
      decodeSettlementStatus (BS.replicate (108 * 32) 0)
        `shouldBe` Left (RpcJsonError "getSettlementStatus(uint256) returned fewer than 109 ABI words")
      decodeSettlementObservation (BS.replicate (193 * 32) 0)
        `shouldBe` Left (RpcJsonError "getSettlementObservation(uint256) returned fewer than 194 ABI words")

  describe "LP settlement call selectors" $ do
    it "uses the canonical pool and router settlement signatures" $ do
      selector "settleLpEpoch(uint256,uint256)" `shouldNotBe` selector "settleLpEpoch(bytes[])"

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
