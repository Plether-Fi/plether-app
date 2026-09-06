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
  , perpsIndexerNameForLifecycleBook
  , perpsV2IndexerName
  , validatePerpsIndexerReleaseConfig
  )
import Test.Hspec

engine, clearinghouse, housePool :: Text
engine = "0x9611E643aC4691E8fDeD8a0c2C22c56438B6f352"
clearinghouse = "0xA863F985EedA8BF5BE2320693BB93d109EBB2dBd"
housePool = "0x21D52509Bb9b9857DaBc8c7FD36dD7fed9118918"

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
      perpsV2VolumeHistoryStartTimestamp `shouldBe` 1788719520

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

    it "selects the indexer format independently of deployment addresses" $ do
      perpsV2IndexerName `shouldBe` "perps-history-costs-v2:finalized-abi3"
      perpsIndexerNameForLifecycleBook (Just perpsV2OrderLifecycleBook)
        `shouldBe` perpsV2IndexerName
      perpsIndexerNameForLifecycleBook (Just "0x1111111111111111111111111111111111111111")
        `shouldBe` perpsV2IndexerName
      perpsIndexerNameForLifecycleBook Nothing `shouldBe` perpsIndexerName
