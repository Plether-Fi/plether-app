{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- The production preparation gas policy is independent of RPC and storage.
module Plether.AA.PreparationGas
  ( gasPolicyVersion, sepoliaExecutionGasCap, executionGasWithHeadroom ) where

import Control.Monad (unless)
import Data.Text (Text)

-- Applied ONLY to a fresh Alto estimate, before immutable preparation storage.
-- Changing this identifier requires a new reviewed preparation, never a rewrite
-- of a persisted/signed operation or its reservation.
gasPolicyVersion :: Text
gasPolicyVersion = "execution-headroom-v3-sepolia-cap3000000-150pct-min100000"

-- Native issuance is restricted to Arbitrum Sepolia. All action classes share
-- this ceiling; the separate wei liability and verification-gas caps do not move.
sepoliaExecutionGasCap :: Integer
sepoliaExecutionGasCap = 3_000_000

executionGasWithHeadroom :: Integer -> Either Text Integer
executionGasWithHeadroom estimated = do
  unless (estimated > 0 && estimated <= sepoliaExecutionGasCap) $
    Left "Invalid execution gas estimate"
  let padded = max ((estimated * 3 + 1) `div` 2) (estimated + 100_000)
  unless (padded <= sepoliaExecutionGasCap) $
    Left "Execution gas including headroom exceeds the reviewed sponsorship bounds"
  pure padded
