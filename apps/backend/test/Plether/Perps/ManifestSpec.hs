module Plether.Perps.ManifestSpec (spec) where

import Data.Aeson (Value (..), eitherDecodeFileStrict', object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft)
import Plether.Perps.Manifest.Embed (settlementTokenField, releaseIsDeployed)
import qualified Plether.Perps.Manifest as Manifest
import Test.Hspec

spec :: Spec
spec = describe "USDG release binding" $ do
  prepared <- runIO $ eitherDecodeFileStrict' "../../config/perps/mock-usdg-prepared.json"
  let root = either error id prepared
      set key value (Object fields) = Object $ KM.insert key value fields
      set _ _ _ = error "fixture must be an object"
      release value = set "release" value root
      activated = release $ object ["phase" .= ("deployed" :: String), "version" .= ("test-only" :: String), "sourceCommit" .= ("test-only" :: String), "deploymentBlock" .= (1 :: Int)]
  it "embeds a prepared release without fabricating deployed pins" $ do
    releaseIsDeployed root `shouldBe` Right False
    settlementTokenField root "address" `shouldBe` Right Null

  it "does not activate merely by changing the release phase" $
    releaseIsDeployed activated `shouldSatisfy` isLeft
  it "rejects historical schema adapters" $ do
    releaseIsDeployed (set "schemaVersion" (Number 3) root) `shouldSatisfy` isLeft
    let Object contracts = case root of Object fields -> maybe (error "contracts") id $ KM.lookup "contracts" fields; _ -> error "fixture"
    releaseIsDeployed (set "contracts" (Object $ KM.insert "mockUsdc" Null contracts) root) `shouldSatisfy` isLeft
  it "rejects missing contract pins and unsupported metadata" $ do
    releaseIsDeployed (set "contracts" (object []) root) `shouldSatisfy` isLeft
    releaseIsDeployed (set "settlementToken" (object ["symbol" .= ("USDC" :: String)]) root) `shouldSatisfy` isLeft
  it "embeds the deployed v2.1.0 identity and permits worker startup" $ do
    Manifest.releaseIsDeployed `shouldBe` True
    Manifest.releaseDeploymentBlock `shouldBe` 333192048
    Manifest.releaseVersion `shouldBe` "v2.1.0"
    Manifest.settlementTokenAddress `shouldBe` "0x66705F56f2c88C9474d021D9C6AFE134205a63CE"
    Manifest.cfdOrderAdmissionValidatorAddress `shouldBe` "0x6F6eBb24C49a449e0Ec14208D883d0e4938b4a58"
    Manifest.requireDeployedRelease
