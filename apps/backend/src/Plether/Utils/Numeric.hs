module Plether.Utils.Numeric
  ( parseDecimal
  , formatWei
  , weiToEther
  , etherToWei
  , mulDiv
  , wad
  , ray
  , WAD
  , RAY
  , wadMul
  , wadDiv
  , rayMul
  , rayDiv
  ) where

import Data.Text (Text)
import qualified Data.Text as T

type WAD = Integer
type RAY = Integer

wad :: Integer
wad = 10 ^ (18 :: Integer)

ray :: Integer
ray = 10 ^ (27 :: Integer)

parseDecimal :: Text -> Maybe Integer
parseDecimal txt
  | T.null txt = Nothing
  | otherwise =
      let stripped = T.strip txt
       in if T.all (\c -> c >= '0' && c <= '9') stripped
            then Just $ read $ T.unpack stripped
            else Nothing

formatWei :: Integer -> Int -> Text
formatWei amount decimals =
  let divisor = 10 ^ decimals
      whole = amount `div` divisor
      frac = abs (amount `mod` divisor)
      fracStr = T.pack $ reverse $ take decimals $ reverse (replicate decimals '0' ++ show frac)
      trimmed = T.dropWhileEnd (== '0') fracStr
   in T.pack (show whole) <> "." <> (if T.null trimmed then "0" else trimmed)

weiToEther :: Integer -> Double
weiToEther amount = fromIntegral amount / fromIntegral wad

etherToWei :: Double -> Integer
etherToWei amount = round $ amount * fromIntegral wad

mulDiv :: Integer -> Integer -> Integer -> Integer
mulDiv x y z = (x * y) `div` z

wadMul :: WAD -> WAD -> WAD
wadMul a b = (a * b) `div` wad

wadDiv :: WAD -> WAD -> WAD
wadDiv a b = (a * wad) `div` b

rayMul :: RAY -> RAY -> RAY
rayMul a b = (a * b) `div` ray

rayDiv :: RAY -> RAY -> RAY
rayDiv a b = (a * ray) `div` b
