module Plether.Perps.ReleaseSpec (spec) where

import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base16 as B16
import Plether.Ethereum.Abi (keccak256)
import Plether.Perps.Release
  ( orderV3IntentDomain
  , perpsV2DeploymentBlock
  , perpsV2OrderLifecycleBook
  , perpsV2OrderRouter
  , perpsV2VolumeHistoryStartTimestamp
  , validatePerpsV2ReleaseConfig
  )
import Plether.Perps.HistoryIndexer
  ( PerpsAddresses (..)
  , defaultPerpsAddresses
  , perpsIndexerName
  , perpsIndexerNameForLifecycleBook
  , perpsV2IndexerName
  , validatePerpsIndexerReleaseConfig
  )
import Test.Hspec

engine, clearinghouse, housePool :: Text
engine = "0xca18a6783d7542f9d2f28f4f259772b74c4d6baa"
clearinghouse = "0xb5b851f627d6df3cf3f096e23e20a58884d7b89a"
housePool = "0xeb5604c393843b2f3c347ec24fa9ddad6ab2131e"

validReleaseConfig :: Either Text ()
validReleaseConfig =
  validatePerpsV2ReleaseConfig
    421614
    perpsV2OrderRouter
    (Just perpsV2OrderLifecycleBook)
    engine
    clearinghouse
    housePool
    perpsV2DeploymentBlock

spec :: Spec
spec =
  describe "bounded V2 release configuration" $ do
    it "pins the USDG V3 intent type hash" $
      B16.encode (keccak256 $ TE.encodeUtf8 orderV3IntentDomain)
        `shouldBe` "b88aa2b16bc10f54a7acd702c11d43dfe7246057278a7fac91335877d86e2a2d"

    it "accepts the exact deployed v2.0.0 graph" $
      validReleaseConfig `shouldBe` Right ()

    it "starts volume at the first whole minute after deployment" $
      perpsV2VolumeHistoryStartTimestamp `shouldBe` 1793185200

    it "rejects a missing LifecycleBook" $
      validatePerpsV2ReleaseConfig
        421614
        perpsV2OrderRouter
        Nothing
        engine
        clearinghouse
        housePool
        perpsV2DeploymentBlock
        `shouldSatisfy` isLeft

    it "rejects a cursor that could suppress the V2 backfill" $
      validatePerpsV2ReleaseConfig
        421614
        perpsV2OrderRouter
        (Just perpsV2OrderLifecycleBook)
        engine
        clearinghouse
        housePool
        (perpsV2DeploymentBlock - 1)
        `shouldSatisfy` isLeft

    it "rejects every mismatched startup binding input" $ do
      let wrong = "0x0000000000000000000000000000000000000001"
          mismatchCases =
            [ validatePerpsV2ReleaseConfig 1 perpsV2OrderRouter (Just perpsV2OrderLifecycleBook) engine clearinghouse housePool perpsV2DeploymentBlock
            , validatePerpsV2ReleaseConfig 421614 wrong (Just perpsV2OrderLifecycleBook) engine clearinghouse housePool perpsV2DeploymentBlock
            , validatePerpsV2ReleaseConfig 421614 perpsV2OrderRouter (Just wrong) engine clearinghouse housePool perpsV2DeploymentBlock
            , validatePerpsV2ReleaseConfig 421614 perpsV2OrderRouter (Just perpsV2OrderLifecycleBook) wrong clearinghouse housePool perpsV2DeploymentBlock
            , validatePerpsV2ReleaseConfig 421614 perpsV2OrderRouter (Just perpsV2OrderLifecycleBook) engine wrong housePool perpsV2DeploymentBlock
            , validatePerpsV2ReleaseConfig 421614 perpsV2OrderRouter (Just perpsV2OrderLifecycleBook) engine clearinghouse wrong perpsV2DeploymentBlock
            ]
      mismatchCases `shouldSatisfy` all isLeft

    it "accepts an indexer bound to the new deployment boundary" $
      validatePerpsIndexerReleaseConfig
        421614
        defaultPerpsAddresses
        housePool
        perpsV2DeploymentBlock
        `shouldBe` Right ()

    it "fails indexer startup when the effective LifecycleBook is absent or wrong" $ do
      let wrong = "0x0000000000000000000000000000000000000001"
          missingLifecycle = defaultPerpsAddresses {paOrderLifecycleBook = Nothing}
          wrongLifecycle = defaultPerpsAddresses {paOrderLifecycleBook = Just wrong}
      validatePerpsIndexerReleaseConfig 421614 missingLifecycle housePool perpsV2DeploymentBlock
        `shouldSatisfy` isLeft
      validatePerpsIndexerReleaseConfig 421614 wrongLifecycle housePool perpsV2DeploymentBlock
        `shouldSatisfy` isLeft

    it "selects the indexer format independently of deployment addresses" $ do
      perpsV2IndexerName `shouldBe` "perps-history-costs-v2:finalized-abi3"
      perpsIndexerNameForLifecycleBook (Just perpsV2OrderLifecycleBook)
        `shouldBe` perpsV2IndexerName
      perpsIndexerNameForLifecycleBook (Just "0x1111111111111111111111111111111111111111")
        `shouldBe` perpsV2IndexerName
      perpsIndexerNameForLifecycleBook Nothing `shouldBe` perpsIndexerName
