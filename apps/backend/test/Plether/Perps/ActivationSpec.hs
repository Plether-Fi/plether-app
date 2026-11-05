module Plether.Perps.ActivationSpec (spec) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Network.HTTP.Types.Status (status200, status503)
import Network.Wai (defaultRequest, pathInfo, responseLBS, responseStatus)
import Network.Wai.Internal (ResponseReceived (..))
import Plether.Api (perpsReleaseMiddleware)
import Test.Hspec

spec :: Spec
spec = describe "prepared perps API boundary" $ do
  let requestStatus deployed path = do
        called <- newIORef False
        observed <- newIORef status200
        let handler _ respond = writeIORef called True >> respond (responseLBS status200 [] "spot")
            respond response = writeIORef observed (responseStatus response) >> pure ResponseReceived
        _ <- perpsReleaseMiddleware deployed handler (defaultRequest {pathInfo = path}) respond
        (,) <$> readIORef observed <*> readIORef called
  it "blocks every perps surface before invoking its handler" $
    mapM_ (\surface -> requestStatus False ["api", surface, "status"] `shouldReturn` (status503, False))
      ["perps", "aa", "testnet", "insights", "divisions", "readiness"]
  it "keeps spot and liveness handlers available" $
    mapM_ (\surface -> requestStatus False ["api", surface] `shouldReturn` (status200, True))
      ["protocol", "user", "quotes", "health"]
  it "allows configured deployed perps routes through" $
    requestStatus True ["api", "perps", "market"] `shouldReturn` (status200, True)
