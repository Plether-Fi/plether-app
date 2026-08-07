module Plether.Handlers.PerpsSpec (spec) where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Database.Schema
  ( BasketHistorySnapshotRow (..)
  , PerpsMarketVolumeBucketRow (..)
  )
import Plether.Handlers.Perps
  ( PythUpdateAdmission (..)
  , basketHistoryPointsWithVolume
  , decodePythUpdateForAdmission
  )
import Plether.Pyth.Basket (BasketComponent (..), basketComponents)
import Plether.Types
  ( ApiError (..)
  , BasketHistoryPoint (..)
  , PythUpdateResponse (..)
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "basketHistoryPointsWithVolume" $ do
    it "matches activity volume by the requested interval bucket" $ do
      let points = basketHistoryPointsWithVolume 60 basketRows volumeRows
      map bhpVolumeUsdc points `shouldBe` [123_456, 789]

    it "zero-fills candles without activity in their interval bucket" $ do
      let points = basketHistoryPointsWithVolume 60 basketRows (take 1 volumeRows)
      map bhpVolumeUsdc points `shouldBe` [123_456, 0]

  describe "decodePythUpdateForAdmission" $ do
    it "prepares strict six-feed latest payload admission inputs" $ do
      admission <-
        expectRight $
          decodePythUpdateForAdmission
            Nothing
            105
            10
            (hermesResponse ["0102"] configuredFeedIds [100 .. 105])
      puaUpdateData admission `shouldBe` [BS.pack [0x01, 0x02]]
      length (puaFeedIds admission) `shouldBe` 6
      puaMinPublishTime admission `shouldBe` 100
      puaMaxPublishTime admission `shouldBe` 105
      purUpdateData (puaPayload admission) `shouldBe` ["0x0102"]
      purPublishTimes (puaPayload admission) `shouldBe` [100 .. 105]

    it "uses the requested historical timestamp as the on-chain lower bound" $ do
      admission <-
        expectRight $
          decodePythUpdateForAdmission
            (Just 100)
            200
            10
            (hermesResponse ["0x01"] configuredFeedIds [100 .. 105])
      puaMinPublishTime admission `shouldBe` 100
      puaMaxPublishTime admission `shouldBe` 105

    it "rejects historical metadata that predates the requested timestamp" $ do
      decodePythUpdateForAdmission
        (Just 101)
        200
        10
        (hermesResponse ["0x01"] configuredFeedIds [100 .. 105])
        `shouldFailWith` "predates the requested publish time"

    it "rejects stale latest metadata before any RPC admission call" $ do
      decodePythUpdateForAdmission
        Nothing
        200
        10
        (hermesResponse ["0x01"] configuredFeedIds [100 .. 105])
        `shouldFailWith` "latest payload is 100s old"

    it "rejects missing, duplicate, or unexpected requested feed IDs" $ do
      let wrongFeedIds = "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" : drop 1 configuredFeedIds
      decodePythUpdateForAdmission
        Nothing
        105
        10
        (hermesResponse ["0x01"] wrongFeedIds [100 .. 105])
        `shouldFailWith` "six requested basket feed IDs"

    it "rejects malformed raw Hermes bytes instead of caching an empty payload" $ do
      decodePythUpdateForAdmission
        Nothing
        105
        10
        (hermesResponse ["0xnot-hex"] configuredFeedIds [100 .. 105])
        `shouldFailWith` "update data item 0 is invalid"

basketRows :: [BasketHistorySnapshotRow]
basketRows =
  [ BasketHistorySnapshotRow
      { bhsrTimestamp = 125
      , bhsrIntervalSeconds = 5
      , bhsrBasketPrice = 101_660_000
      , bhsrComponents = Nothing
      }
  , BasketHistorySnapshotRow
      { bhsrTimestamp = 181
      , bhsrIntervalSeconds = 5
      , bhsrBasketPrice = 101_670_000
      , bhsrComponents = Nothing
      }
  ]

volumeRows :: [PerpsMarketVolumeBucketRow]
volumeRows =
  [ PerpsMarketVolumeBucketRow
      { pmvbrBucket = 2
      , pmvbrVolumeUsdc = 123_456
      }
  , PerpsMarketVolumeBucketRow
      { pmvbrBucket = 3
      , pmvbrVolumeUsdc = 789
      }
  ]

configuredFeedIds :: [Text]
configuredFeedIds = bcFeedId <$> basketComponents

hermesResponse :: [Text] -> [Text] -> [Integer] -> LBS.ByteString
hermesResponse updateData feedIds publishTimes =
  encode $
    object
      [ "binary" .= object ["data" .= updateData]
      , "parsed"
          .= zipWith
            (\feedId publishTime ->
              object
                [ "id" .= feedId
                , "price" .= object ["publish_time" .= publishTime]
                ]
            )
            feedIds
            publishTimes
      ]

shouldFailWith :: Either ApiError value -> Text -> Expectation
shouldFailWith result expected =
  case result of
    Left err -> errMessage err `shouldSatisfy` T.isInfixOf expected
    Right _ -> expectationFailure "expected Pyth admission preparation to fail"

expectRight :: (Show err) => Either err value -> IO value
expectRight result =
  case result of
    Right value -> pure value
    Left err -> expectationFailure ("expected Right, got " <> show err) >> fail "unreachable"
