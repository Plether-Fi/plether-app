module Plether.Perps.ReleaseSpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base16 as B16
import Plether.Ethereum.Abi (keccak256)
import Plether.Perps.Release
  ( PerpsReleaseConfigPins (..)
  , orderV3IntentDomain
  , perpsV2DeploymentBlock
  , perpsV2OrderLifecycleBook
  , perpsV2OrderRouter
  , perpsV2VolumeHistoryStartTimestamp
  , validatePerpsV2ReleaseConfig
  , validatePerpsV2ReleaseConfigWithPins
  )
import Plether.Perps.HistoryIndexer
  ( PerpsAddresses (..)
  , defaultPerpsAddresses
  , perpsIndexerName
  , perpsIndexerNameForLifecycleBook
  , perpsV2IndexerName
  , validatePerpsIndexerReleaseConfig
  , validatePerpsIndexerReleaseConfigWithPins
  )
import Test.Hspec

spec :: Spec
spec = describe "bounded V2 release configuration" $ do
  it "pins the USDG V3 intent type hash" $
    B16.encode (keccak256 $ TE.encodeUtf8 orderV3IntentDomain)
      `shouldBe` "b88aa2b16bc10f54a7acd702c11d43dfe7246057278a7fac91335877d86e2a2d"

  it "rejects perps activation while the canonical release is prepared" $
    validatePerpsV2ReleaseConfig 421614 perpsV2OrderRouter
      (Just perpsV2OrderLifecycleBook) (address "3") (address "4") (address "5") perpsV2DeploymentBlock
      `shouldBe` Left "Perps release is prepared; deployment is required"

  it "does not invent a volume history boundary for an undeployed release" $
    perpsV2VolumeHistoryStartTimestamp `shouldBe` 0

  it "accepts a complete deployed release and case-insensitive address spellings" $ do
    validateInput deployedPins (Just $ prcpLifecycleBook deployedPins) `shouldBe` Right ()
    validateInput (deployedPins {prcpRouter = T.toUpper $ prcpRouter deployedPins})
      (Just $ T.toUpper $ prcpLifecycleBook deployedPins) `shouldBe` Right ()

  it "rejects a missing LifecycleBook after the deployed positive control" $ do
    validateInput deployedPins (Just $ prcpLifecycleBook deployedPins) `shouldBe` Right ()
    validateInput deployedPins Nothing
      `shouldBe` Left "PERPS_ORDER_LIFECYCLE_BOOK is required for bounded V3 sponsorship"

  forM_
    [ ("PERPS_CHAIN_ID", deployedPins {prcpChainId = 1}, "421614")
    , ("PERPS_ORDER_ROUTER", deployedPins {prcpRouter = wrong}, prcpRouter deployedPins)
    , ("PERPS_CFD_ENGINE", deployedPins {prcpEngine = wrong}, prcpEngine deployedPins)
    , ("PERPS_MARGIN_CLEARINGHOUSE", deployedPins {prcpClearinghouse = wrong}, prcpClearinghouse deployedPins)
    , ("PERPS_HOUSE_POOL", deployedPins {prcpHousePool = wrong}, prcpHousePool deployedPins)
    , ("PERPS_INDEXER_START_BLOCK", deployedPins {prcpDeploymentBlock = 99}, "100")
    ] $ \(label, invalid, expected) ->
      it ("rejects only the changed deployed binding " <> T.unpack label) $ do
        validateInput deployedPins (Just $ prcpLifecycleBook deployedPins) `shouldBe` Right ()
        validateInput invalid (Just $ prcpLifecycleBook deployedPins)
          `shouldBe` Left (label <> " must match the pinned bounded V3 release " <> expected)

  it "rejects a different deployed LifecycleBook with the specific binding error" $ do
    validateInput deployedPins (Just $ prcpLifecycleBook deployedPins) `shouldBe` Right ()
    validateInput deployedPins (Just wrong)
      `shouldBe` Left ("PERPS_ORDER_LIFECYCLE_BOOK must match the pinned bounded V3 release " <> prcpLifecycleBook deployedPins)

  it "rejects an indexer bound to prepared internal pins before startup" $
    validatePerpsIndexerReleaseConfig 421614 defaultPerpsAddresses (address "5") perpsV2DeploymentBlock
      `shouldBe` Left "Perps release is prepared; deployment is required"

  it "validates the effective deployed indexer LifecycleBook independently" $ do
    let validate = validatePerpsIndexerReleaseConfigWithPins deployedPins 421614
    validate deployedAddresses (prcpHousePool deployedPins) 100 `shouldBe` Right ()
    validate (deployedAddresses {paOrderLifecycleBook = Nothing}) (prcpHousePool deployedPins) 100
      `shouldBe` Left "PERPS_ORDER_LIFECYCLE_BOOK is required for bounded V3 sponsorship"
    validate (deployedAddresses {paOrderLifecycleBook = Just wrong}) (prcpHousePool deployedPins) 100
      `shouldBe` Left ("PERPS_ORDER_LIFECYCLE_BOOK must match the pinned bounded V3 release " <> prcpLifecycleBook deployedPins)

  it "selects the indexer format independently of deployment addresses" $ do
    perpsV2IndexerName `shouldBe` "perps-history-costs-v2:finalized-abi3"
    perpsIndexerNameForLifecycleBook (Just perpsV2OrderLifecycleBook) `shouldBe` perpsV2IndexerName
    perpsIndexerNameForLifecycleBook (Just wrong) `shouldBe` perpsV2IndexerName
    perpsIndexerNameForLifecycleBook Nothing `shouldBe` perpsIndexerName

validateInput :: PerpsReleaseConfigPins -> Maybe Text -> Either Text ()
validateInput actual = \book -> validatePerpsV2ReleaseConfigWithPins deployedPins
  (prcpChainId actual) (prcpRouter actual) book (prcpEngine actual)
  (prcpClearinghouse actual) (prcpHousePool actual) (prcpDeploymentBlock actual)

deployedPins :: PerpsReleaseConfigPins
deployedPins = PerpsReleaseConfigPins True 421614 (address "1") (address "2")
  (address "3") (address "4") (address "5") 100

deployedAddresses :: PerpsAddresses
deployedAddresses = defaultPerpsAddresses
  { paOrderRouter = prcpRouter deployedPins
  , paOrderLifecycleBook = Just $ prcpLifecycleBook deployedPins
  , paCfdEngine = prcpEngine deployedPins
  , paMarginClearinghouse = prcpClearinghouse deployedPins
  }

address :: Text -> Text
address digit = "0x" <> T.replicate 40 digit

wrong :: Text
wrong = address "f"
