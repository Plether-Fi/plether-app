module Plether.Api.BackgroundWorkers
  ( StartupPolicy (..)
  , resolveStartupPolicy
  ) where

-- The isolated Kernel API shares the Sepolia database with the live API. Its
-- generic indexers and processors must remain off so that only the existing
-- service owns their shared cursors and queues.
data StartupPolicy = StartupPolicy
  { spCanaryApiMode :: Bool
  , spGenericSchemaEnabled :: Bool
  , spBackgroundWorkersEnabled :: Bool
  } deriving stock (Eq, Show)

resolveStartupPolicy :: Bool -> Maybe String -> Maybe String -> Either String StartupPolicy
resolveStartupPolicy kernelConfigured canaryMode workersFlag = do
  canary <- parseFlag "KERNEL_CANARY_API_MODE" False canaryMode
  workers <- parseFlag "API_BACKGROUND_WORKERS_ENABLED" True workersFlag
  if canary && not kernelConfigured
    then Left "KERNEL_CANARY_API_MODE requires an enabled Kernel gateway"
    else if canary && workersFlag /= Just "false"
      then Left "KERNEL_CANARY_API_MODE requires API_BACKGROUND_WORKERS_ENABLED=false"
      else Right $ StartupPolicy canary (not canary) workers
 where
  parseFlag _ fallback Nothing = Right fallback
  parseFlag _ _ (Just "true") = Right True
  parseFlag _ _ (Just "false") = Right False
  parseFlag name _ _ = Left $ name <> " must be exactly true or false"
