module Plether.ServerSpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Network.HTTP.Client as HTTP
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket
import Network.HTTP.Types (status200, status503)
import Network.Wai (pathInfo, responseLBS)
import Network.Wai.Handler.Warp (getPort, setTimeout, testWithApplicationSettings)
import Network.Wai.Handler.Warp.Internal (Settings (settingsTimeout))
import Plether.Server
import Test.Hspec
import Web.Scotty (Options (..))
import System.Timeout (timeout)

spec :: Spec
spec = describe "API server connection lifecycle" $ do
  it "keeps backend idle timeout above the 75-second ALB timeout" $ do
    let configured = settings $ apiServerOptions 3001
    getPort configured `shouldBe` 3001
    settingsTimeout configured `shouldBe` 120
    apiConnectionTimeoutSeconds `shouldSatisfy` (> 75)

  it "keeps the same socket reusable beyond the ALB's 75-second idle timeout" $ do
    let configured = settings $ apiServerOptions 0
        app _ respond = respond $ responseLBS status200 [("Content-Length", "2")] "ok"
    testWithApplicationSettings configured (pure app) $ \port -> do
      -- A raw socket cannot silently reconnect/retry, unlike http-client's
      -- manager (which also expires its own idle connections after 30s).
      bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \connection -> do
        Socket.connect connection $ Socket.SockAddrInet (fromIntegral port) $ Socket.tupleToHostAddress (127,0,0,1)
        let exchange = do
              Socket.sendAll connection "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
              readResponse connection BS.empty
        timeout 5_000_000 exchange `shouldReturn` Just True
        threadDelay 80_000_000
        timeout 5_000_000 exchange `shouldReturn` Just True

  it "does not turn the connection timeout into a long-poll request deadline" $ do
    -- Use a scaled timeout so this exercises Warp's application pause without
    -- adding a two-minute test; the production value is asserted separately.
    let configured = setTimeout 1 $ settings $ apiServerOptions 0
        app _ respond = threadDelay 3_000_000 >> respond (responseLBS status200 [] "complete")
    testWithApplicationSettings configured (pure app) $ \port -> do
      manager <- HTTP.newManager HTTP.defaultManagerSettings
      request <- HTTP.parseRequest $ "http://127.0.0.1:" <> show port
      response <- HTTP.httpLbs request manager
      HTTP.responseBody response `shouldBe` "complete"

  it "sheds excess concurrent foreground requests while keeping health checks and long polls isolated" $ do
    entered <- newEmptyMVar
    release <- newEmptyMVar
    limitMiddleware <- newConcurrencyLimitMiddleware 1 1
    let app req respond = case pathInfo req of
          ["api", "health"] -> respond $ responseLBS status200 [] "healthy"
          ["api", "perps", "orders", "1", "wait"] -> respond $ responseLBS status200 [] "longpoll"
          _ -> do
            putMVar entered ()
            takeMVar release
            respond $ responseLBS status200 [] "ok"
    testWithApplicationSettings (settings $ apiServerOptions 0) (pure $ limitMiddleware app) $ \port -> do
      manager <- HTTP.newManager HTTP.defaultManagerSettings
      let baseUrl = "http://127.0.0.1:" <> show port
      fgReq <- HTTP.parseRequest $ baseUrl <> "/api/perps/basket/candles/current?interval=900"
      healthReq <- HTTP.parseRequest $ baseUrl <> "/api/health"
      longPollReq <- HTTP.parseRequest $ baseUrl <> "/api/perps/orders/1/wait"

      inFlight <- async $ HTTP.httpLbs fgReq manager
      takeMVar entered

      shedResp <- HTTP.httpLbs fgReq manager
      HTTP.responseStatus shedResp `shouldBe` status503
      lookup "Retry-After" (HTTP.responseHeaders shedResp) `shouldBe` Just "1"
      lookup "Cache-Control" (HTTP.responseHeaders shedResp) `shouldBe` Just "no-store"

      healthResp <- HTTP.httpLbs healthReq manager
      HTTP.responseStatus healthResp `shouldBe` status200

      longPollResp <- HTTP.httpLbs longPollReq manager
      HTTP.responseStatus longPollResp `shouldBe` status200

      putMVar release ()
      firstResp <- wait inFlight
      HTTP.responseStatus firstResp `shouldBe` status200

      recoveredResp <- async $ HTTP.httpLbs fgReq manager
      takeMVar entered
      putMVar release ()
      finalResp <- wait recoveredResp
      HTTP.responseStatus finalResp `shouldBe` status200

readResponse :: Socket.Socket -> BS.ByteString -> IO Bool
readResponse connection accumulated
  | "\r\n\r\nok" `BS.isSuffixOf` accumulated = pure $ "HTTP/1.1 200" `BS.isPrefixOf` accumulated
  | BS.length accumulated > 8192 = pure False
  | otherwise = do
      bytes <- Socket.recv connection 4096
      if BS.null bytes then pure False else readResponse connection $ accumulated <> bytes
