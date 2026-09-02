module Plether.Perps.ReleaseSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Plether.Perps.Release
  ( perpsV2DeploymentBlock
  , perpsV2OrderLifecycleBook
  , perpsV2OrderRouter
  , perpsV2VolumeHistoryStartTimestamp
  , validatePerpsV2ReleaseConfig
  )
import Plether.Perps.HistoryIndexer
  ( PerpsAddresses (..)
  , defaultPerpsAddresses
  , perpsIndexerName
  , perpsIndexerNameForRelease
  , perpsV2IndexerName
  , validatePerpsIndexerReleaseConfig
  )
import Test.Hspec

engine, clearinghouse, housePool :: Text
engine = "0x3dc9C0A1f9C745A4B08BD5C2E6c7aE613561c20D"
clearinghouse = "0x2f98787F6dCC3b1f2E4a2AFa5acf410159b9F211"
housePool = "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"

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
    it "accepts only the pinned Sepolia graph and deployment boundary" $
      validReleaseConfig `shouldSatisfy` isRight

    it "pins the first whole minute after the deployment block for volume history" $
      perpsV2VolumeHistoryStartTimestamp `shouldBe` 1787759880

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

    it "validates the effective indexer graph before startup" $
      validatePerpsIndexerReleaseConfig
        421614
        defaultPerpsAddresses
        housePool
        perpsV2DeploymentBlock
        `shouldSatisfy` isRight

    it "fails indexer startup when the effective LifecycleBook is absent or wrong" $ do
      let wrong = "0x0000000000000000000000000000000000000001"
          missingLifecycle = defaultPerpsAddresses {paOrderLifecycleBook = Nothing}
          wrongLifecycle = defaultPerpsAddresses {paOrderLifecycleBook = Just wrong}
      validatePerpsIndexerReleaseConfig 421614 missingLifecycle housePool perpsV2DeploymentBlock
        `shouldSatisfy` isLeft
      validatePerpsIndexerReleaseConfig 421614 wrongLifecycle housePool perpsV2DeploymentBlock
        `shouldSatisfy` isLeft

    it "isolates only the pinned Sepolia release in the V2 cursor namespace" $ do
      perpsV2IndexerName `shouldBe` "perps-history-costs-v2:finalized-abi3"
      perpsIndexerNameForRelease
        421614
        perpsV2OrderRouter
        (Just perpsV2OrderLifecycleBook)
        `shouldBe` perpsV2IndexerName
      perpsIndexerNameForRelease
        1
        perpsV2OrderRouter
        (Just perpsV2OrderLifecycleBook)
        `shouldBe` perpsIndexerName
