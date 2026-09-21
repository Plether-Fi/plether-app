module Plether.Server (apiServerOptions, apiConnectionTimeoutSeconds) where

import Network.Wai.Handler.Warp (setPort, setTimeout)
import Web.Scotty (Options (..), defaultOptions)

-- Must exceed the API ALB's 75-second idle timeout. This is a connection
-- timeout, NOT a request deadline; Warp pauses it while the application runs.
-- Keep the 60-second long-poll and faucet route deadlines independent.
apiConnectionTimeoutSeconds :: Int
apiConnectionTimeoutSeconds = 120

apiServerOptions :: Int -> Options
apiServerOptions port = defaultOptions
  { settings = setPort port $ setTimeout apiConnectionTimeoutSeconds $ settings defaultOptions }
