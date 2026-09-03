module Plether.Ethereum.Contracts.TrancheVault
  ( TrancheVaultSnapshot (..)
  , vaultSharePriceProbe
  , trancheVaultSnapshotCalls
  , decodeTrancheVaultSnapshotResults
  , vaultDecimalsCalls
  , decodeVaultDecimalsResults
  , sharePriceWadFromConvertedAssets
  ) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Ethereum.Abi (decodeUint256, encodeCall, encodeUint256)
import Plether.Ethereum.Multicall (Call (..), CallResult (..))

data TrancheVaultSnapshot = TrancheVaultSnapshot
  { tvsTotalAssets :: Integer
  , tvsTotalSupply :: Integer
  , tvsSharePriceWad :: Integer
  }
  deriving stock (Eq, Show)

-- A large notional avoids losing economically relevant precision to the
-- ERC-4626 integer conversion. The deployed tranche tokens use 9 decimals and
-- their USDC asset uses 6 decimals.
vaultSharePriceProbe :: Integer
vaultSharePriceProbe = 10 ^ (27 :: Integer)

trancheVaultSnapshotCalls :: Text -> Text -> Text -> [Call]
trancheVaultSnapshotCalls housePool seniorVault juniorVault =
  Call housePool True (encodeCall "getPoolLiquidityView()" [])
    : concatMap callsFor [seniorVault, juniorVault]
 where
  callsFor vault =
    [ Call vault True $ encodeCall "totalAssets()" []
    , Call vault True $ encodeCall "totalSupply()" []
    , Call vault True $ encodeCall "convertToAssets(uint256)" [encodeUint256 vaultSharePriceProbe]
    ]

vaultDecimalsCalls :: Text -> Text -> Text -> [Call]
vaultDecimalsCalls asset seniorVault juniorVault =
  [ Call asset True $ encodeCall "decimals()" []
  , Call seniorVault True $ encodeCall "decimals()" []
  , Call juniorVault True $ encodeCall "decimals()" []
  ]

-- | The WAD normalization below is valid only for the reviewed deployment's
-- six-decimal USDC asset and nine-decimal tranche shares. Fail closed if a
-- configured redeployment changes either invariant.
decodeVaultDecimalsResults :: [CallResult] -> Either Text ()
decodeVaultDecimalsResults results =
  case results of
    [assetResult, seniorResult, juniorResult] -> do
      assetDecimals <- decodeResult "Asset" "decimals" assetResult
      seniorDecimals <- decodeResult "Senior" "decimals" seniorResult
      juniorDecimals <- decodeResult "Junior" "decimals" juniorResult
      if (assetDecimals, seniorDecimals, juniorDecimals) == (6, 9, 9)
        then Right ()
        else
          Left $
            "Vault performance requires asset/Senior/Junior decimals 6/9/9, received "
              <> T.intercalate "/" (map (T.pack . show) [assetDecimals, seniorDecimals, juniorDecimals])
    _ ->
      Left $
        "Vault decimals Multicall returned "
          <> T.pack (show $ length results)
          <> " results; expected 3"

decodeTrancheVaultSnapshotResults
  :: [CallResult]
  -> Either Text (Bool, TrancheVaultSnapshot, TrancheVaultSnapshot)
decodeTrancheVaultSnapshotResults results =
  case results of
    [poolLiquidity, seniorAssets, seniorSupply, seniorConverted, juniorAssets, juniorSupply, juniorConverted] ->
      (,,)
        <$> decodePoolMarkFresh poolLiquidity
        <*> decodeTranche "Senior" seniorAssets seniorSupply seniorConverted
        <*> decodeTranche "Junior" juniorAssets juniorSupply juniorConverted
    _ ->
      Left $
        "Vault snapshot Multicall returned "
          <> T.pack (show $ length results)
          <> " results; expected 7"

-- getPoolLiquidityView() is twelve static ABI words. `markFresh` is word 9
-- (zero-based), after the nine monetary fields. Validate the canonical bool
-- encoding rather than accepting arbitrary nonzero values.
decodePoolMarkFresh :: CallResult -> Either Text Bool
decodePoolMarkFresh CallResult {..}
  | not resultSuccess = Left "HousePool getPoolLiquidityView subcall failed"
  | BS.length resultData /= poolLiquidityViewLength =
      Left $
        "HousePool getPoolLiquidityView returned "
          <> T.pack (show $ BS.length resultData)
          <> " bytes; expected "
          <> T.pack (show poolLiquidityViewLength)
  | encoded == 0 = Right False
  | encoded == 1 = Right True
  | otherwise = Left "HousePool getPoolLiquidityView returned a non-canonical markFresh bool"
 where
  encoded = decodeUint256 $ BS.take abiWordLength $ BS.drop (9 * abiWordLength) resultData

decodeTranche
  :: Text
  -> CallResult
  -> CallResult
  -> CallResult
  -> Either Text TrancheVaultSnapshot
decodeTranche label assetsResult supplyResult convertedResult = do
  assets <- decodeResult label "totalAssets" assetsResult
  supply <- decodeResult label "totalSupply" supplyResult
  converted <- decodeResult label "convertToAssets" convertedResult
  pure $
    TrancheVaultSnapshot
      { tvsTotalAssets = assets
      , tvsTotalSupply = supply
      , tvsSharePriceWad = sharePriceWadFromConvertedAssets converted
      }

decodeResult :: Text -> Text -> CallResult -> Either Text Integer
decodeResult tranche method CallResult {..}
  | not resultSuccess = Left $ tranche <> " vault " <> method <> " subcall failed"
  | BS.length resultData /= abiWordLength =
      Left $
        tranche
          <> " vault "
          <> method
          <> " returned "
          <> T.pack (show $ BS.length resultData)
          <> " bytes; expected 32"
  | otherwise = Right $ decodeUint256 resultData

-- convertToAssets(1e27) returns raw six-decimal USDC for 1e27 raw nine-decimal
-- shares. Normalizing that ratio to 18-decimal USDC-per-share WAD simplifies
-- to division by 1e6. Integer division deliberately rounds down, matching the
-- vault conversion's conservative ERC-4626 rounding direction.
sharePriceWadFromConvertedAssets :: Integer -> Integer
sharePriceWadFromConvertedAssets convertedAssets = convertedAssets `div` usdcScale

usdcScale :: Integer
usdcScale = 10 ^ (6 :: Integer)

abiWordLength :: Int
abiWordLength = 32

poolLiquidityViewLength :: Int
poolLiquidityViewLength = 12 * abiWordLength
