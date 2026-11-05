module Plether.AA.TimingSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Monad.Trans.Except (runExceptT, throwE)
import qualified Data.Text as T
import Plether.AA.Timing
import Plether.Logging (LogTiming (..))
import Test.Hspec

spec :: Spec
spec = describe "AA request timings" $ do
  it "records elapsed stages, repeated releases and typed rejection results" $ do
    timing <- newTiming
    timed timing "body_read" (threadDelay 2_000)
    timed timing "lease_release" (pure ())
    timed timing "lease_release" (pure ())
    result <- timed timing "prepare" $ runExceptT $ throwE ("rejected" :: String)
    (result :: Either String ()) `shouldBe` Left "rejected"
    (_, header) <- timingHeaders timing
    T.isInfixOf "body_read;dur=" header `shouldBe` True
    T.count "lease_release;dur=" header `shouldBe` 2
    T.isInfixOf "prepare;dur=" header `shouldBe` True

  it "aggregates logging wait and write time without adding them to nested stages" $ do
    timing <- newTiming
    observeLogTiming timing $ LogTiming 12 30
    observeLogTiming timing $ LogTiming 3 4
    (_, header) <- timingHeaders timing
    T.isInfixOf "log_lock_wait;dur=15" header `shouldBe` True
    T.isInfixOf "log_write;dur=34" header `shouldBe` True

  it "keeps concurrent requests' measurements and identifiers separate" $ do
    first <- newTiming
    second <- newTiming
    _ <- concurrently (observeLogTiming first $ LogTiming 10 20)
      (observeLogTiming second $ LogTiming 30 40)
    (firstId, firstHeader) <- timingHeaders first
    (secondId, secondHeader) <- timingHeaders second
    firstId `shouldNotBe` secondId
    T.isInfixOf "log_write;dur=20" firstHeader `shouldBe` True
    T.isInfixOf "log_write;dur=40" secondHeader `shouldBe` True
