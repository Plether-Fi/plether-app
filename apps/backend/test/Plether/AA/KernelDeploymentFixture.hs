module Plether.AA.KernelDeploymentFixture
  ( KernelDeploymentFixture (..)
  , KernelGasFixture (..)
  , loadKernelDeploymentFixture
  ) where

import Data.Aeson (FromJSON (..), Value, eitherDecodeFileStrict', withObject, (.:))
import Text.Read (readMaybe)

data KernelDeploymentFixture = KernelDeploymentFixture
  { fixtureProfile :: Value
  , fixtureMinimumSafeLag :: Integer
  , fixtureMaximumSafeLag :: Integer
  , fixtureGas :: KernelGasFixture
  }

data KernelGasFixture = KernelGasFixture
  { fixtureCallGas :: Integer
  , fixtureVerificationGas :: Integer
  , fixturePreVerificationGas :: Integer
  , fixturePaymasterVerificationGas :: Integer
  , fixturePaymasterPostOpGas :: Integer
  , fixtureAggregateGas :: Integer
  , fixtureMaxFeePerGas :: Integer
  , fixtureMaxPriorityFeePerGas :: Integer
  , fixtureEthMaxPrefundWei :: Integer
  , fixtureNitroPreVerificationGas :: Integer
  }

instance FromJSON KernelDeploymentFixture where
  parseJSON = withObject "Kernel deployment fixture" $ \fields -> do
    lag <- fields .: "safeHeadLagSeconds"
    KernelDeploymentFixture
      <$> fields .: "profile"
      <*> lag .: "minimum"
      <*> lag .: "maximum"
      <*> fields .: "gas"

instance FromJSON KernelGasFixture where
  parseJSON = withObject "Kernel gas fixture" $ \fields -> do
    rawPrefund <- fields .: "ethMaxPrefundWei"
    prefund <- case readMaybe rawPrefund of
      Just value | value > 0 && show value == rawPrefund -> pure value
      _ -> fail "ethMaxPrefundWei must be a canonical positive decimal string"
    KernelGasFixture
      <$> fields .: "callGasLimit"
      <*> fields .: "verificationGasLimit"
      <*> fields .: "preVerificationGas"
      <*> fields .: "paymasterVerificationGasLimit"
      <*> fields .: "paymasterPostOpGasLimit"
      <*> fields .: "aggregateGas"
      <*> fields .: "maxFeePerGas"
      <*> fields .: "maxPriorityFeePerGas"
      <*> pure prefund
      <*> fields .: "nitroFixturePreVerificationGas"

loadKernelDeploymentFixture :: IO KernelDeploymentFixture
loadKernelDeploymentFixture =
  eitherDecodeFileStrict' "test/fixtures/kernel-deployment.json" >>= either fail pure
