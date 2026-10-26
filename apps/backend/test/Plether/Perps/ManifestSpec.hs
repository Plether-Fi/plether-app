module Plether.Perps.ManifestSpec (spec) where

import Data.Aeson (Value (..), eitherDecodeFileStrict', object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft)
import Plether.Perps.Manifest.Embed (settlementTokenField, releaseIsDeployed)
import qualified Plether.Perps.Manifest as Manifest
import Test.Hspec

spec :: Spec
spec = describe "settlement token release binding" $ do
  prepared <- runIO $ eitherDecodeFileStrict' "../../config/perps/mock-usdg-prepared.json"
  let root = either error id prepared
      set key value (Object fields) = Object $ KM.insert key value fields
      set _ _ _ = error "fixture must be an object"
      release value = set "release" value root
      activated = release $ object ["phase" .= ("deployed" :: String), "version" .= ("test-only" :: String), "sourceCommit" .= ("test-only" :: String), "deploymentBlock" .= (1 :: Int)]
  it "embeds a prepared release without fabricating deployed pins" $ do
    releaseIsDeployed root `shouldBe` Right False
    settlementTokenField root "address" `shouldBe` Right Null
    Manifest.releaseIsDeployed `shouldBe` False
    Manifest.releaseDeploymentBlock `shouldBe` 0
    Manifest.releaseVersion `shouldBe` ""
  it "does not activate merely by changing the release phase" $
    releaseIsDeployed activated `shouldSatisfy` isLeft
  it "rejects historical schema adapters" $ do
    releaseIsDeployed (set "schemaVersion" (Number 3) root) `shouldSatisfy` isLeft
    let Object contracts = case root of Object fields -> maybe (error "contracts") id $ KM.lookup "contracts" fields; _ -> error "fixture"
    releaseIsDeployed (set "contracts" (Object $ KM.insert "mockUsdc" Null contracts) root) `shouldSatisfy` isLeft
  it "rejects missing contract pins and unsupported metadata" $ do
    releaseIsDeployed (set "contracts" (object []) root) `shouldSatisfy` isLeft
    releaseIsDeployed (set "settlementToken" (object ["symbol" .= ("USDC" :: String)]) root) `shouldSatisfy` isLeft
  it "stops worker startup before clients or database connections are made" $
    Manifest.requireDeployedRelease `shouldThrow` anyIOException
