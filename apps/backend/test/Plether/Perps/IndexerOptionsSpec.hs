module Plether.Perps.IndexerOptionsSpec (spec) where

import Data.Either (isLeft)
import Plether.Perps.IndexerOptions
  ( PerpsIndexerInvocation (..)
  , ReplayOptions (..)
  , maximumReplayBlockSpan
  , parsePerpsIndexerInvocation
  , validateReplayOptions
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "parsePerpsIndexerInvocation" $ do
    it "preserves ordinary loop and once invocations" $ do
      parsePerpsIndexerInvocation [] `shouldBe` Right PerpsIndexerLoop
      parsePerpsIndexerInvocation ["--once"] `shouldBe` Right PerpsIndexerLoop
      parsePerpsIndexerInvocation ["--confirmations", "2", "--batch-size", "5000"]
        `shouldBe` Right PerpsIndexerLoop

    it "parses an exact, fully bounded replay command" $
      parsePerpsIndexerInvocation validReplayArgs
        `shouldBe` Right (PerpsIndexerReplay validReplayOptions)

    it "accepts a one-block replay and options in any order" $
      parsePerpsIndexerInvocation
        [ "--replay"
        , "--max-runtime-seconds"
        , "60"
        , "--to-block"
        , "42"
        , "--lock-timeout-ms"
        , "100"
        , "--from-block"
        , "42"
        , "--statement-timeout-ms"
        , "1000"
        ]
        `shouldBe` Right
          ( PerpsIndexerReplay
              ReplayOptions
                { roFromBlock = 42
                , roToBlock = 42
                , roStatementTimeoutMs = 1_000
                , roLockTimeoutMs = 100
                , roMaxRuntimeSeconds = 60
                }
          )

    it "rejects every missing required replay option" $
      mapM_
        ( \option ->
            parsePerpsIndexerInvocation (removeOption validReplayArgs option)
              `shouldSatisfy` isLeft
        )
        replayOptionNames

    it "rejects an option token with no following value" $
      mapM_
        ( \option ->
            parsePerpsIndexerInvocation ["--replay", option]
              `shouldSatisfy` isLeft
        )
        replayOptionNames

    it "rejects every duplicated replay option and command" $ do
      parsePerpsIndexerInvocation ("--replay" : validReplayArgs) `shouldSatisfy` isLeft
      mapM_
        ( \option ->
            parsePerpsIndexerInvocation
              (validReplayArgs <> [option, valueFor option])
              `shouldSatisfy` isLeft
        )
        replayOptionNames

    it "rejects unknown and ordinary-loop options in replay mode" $ do
      parsePerpsIndexerInvocation (validReplayArgs <> ["--unknown", "1"])
        `shouldSatisfy` isLeft
      parsePerpsIndexerInvocation (validReplayArgs <> ["--once"])
        `shouldSatisfy` isLeft

    it "requires --replay to be the exact first command token" $ do
      parsePerpsIndexerInvocation (["--once"] <> validReplayArgs)
        `shouldSatisfy` isLeft
      parsePerpsIndexerInvocation ["--replay=true"] `shouldSatisfy` isLeft

    it "rejects legacy backfill invocations instead of falling through to loop mode" $ do
      parsePerpsIndexerInvocation ["--backfill", "--from", "1", "--to", "2"]
        `shouldSatisfy` isLeft
      parsePerpsIndexerInvocation ["--backfill=1"] `shouldSatisfy` isLeft

    it "rejects malformed, signed, whitespace-padded, and overflowing values for every field" $
      mapM_
        ( \option ->
            mapM_
              ( \invalidValue ->
                  parsePerpsIndexerInvocation
                    (replaceOption option invalidValue validReplayArgs)
                    `shouldSatisfy` isLeft
              )
              ["", "1x", "+1", "-1", " 1", "1 ", replicate 100 '9']
        )
        replayOptionNames

    it "rejects reversed and over-limit inclusive block ranges" $ do
      parsePerpsIndexerInvocation
        (replaceOption "--from-block" "5100" validReplayArgs)
        `shouldSatisfy` isLeft
      parsePerpsIndexerInvocation
        (replaceOption "--to-block" (show $ 100 + maximumReplayBlockSpan) validReplayArgs)
        `shouldSatisfy` isLeft

  describe "validateReplayOptions" $ do
    it "revalidates parser invariants for programmatically constructed values" $ do
      validateReplayOptions validReplayOptions `shouldBe` Right validReplayOptions
      validateReplayOptions validReplayOptions {roFromBlock = -1} `shouldSatisfy` isLeft
      validateReplayOptions validReplayOptions {roToBlock = roFromBlock validReplayOptions + maximumReplayBlockSpan}
        `shouldSatisfy` isLeft
      validateReplayOptions validReplayOptions {roStatementTimeoutMs = 0} `shouldSatisfy` isLeft
      validateReplayOptions validReplayOptions {roLockTimeoutMs = 0} `shouldSatisfy` isLeft
      validateReplayOptions validReplayOptions {roMaxRuntimeSeconds = 0} `shouldSatisfy` isLeft

validReplayArgs :: [String]
validReplayArgs =
  [ "--replay"
  , "--from-block"
  , "100"
  , "--to-block"
  , "5099"
  , "--statement-timeout-ms"
  , "1800000"
  , "--lock-timeout-ms"
  , "5000"
  , "--max-runtime-seconds"
  , "21600"
  ]

validReplayOptions :: ReplayOptions
validReplayOptions =
  ReplayOptions
    { roFromBlock = 100
    , roToBlock = 5_099
    , roStatementTimeoutMs = 1_800_000
    , roLockTimeoutMs = 5_000
    , roMaxRuntimeSeconds = 21_600
    }

replayOptionNames :: [String]
replayOptionNames =
  [ "--from-block"
  , "--to-block"
  , "--statement-timeout-ms"
  , "--lock-timeout-ms"
  , "--max-runtime-seconds"
  ]

valueFor :: String -> String
valueFor option =
  case dropWhile (/= option) validReplayArgs of
    _ : value : _ -> value
    _ -> error $ "Missing test option: " <> option

removeOption :: [String] -> String -> [String]
removeOption [] _ = []
removeOption (option : _ : rest) target
  | option == target = rest
removeOption (value : rest) target = value : removeOption rest target

replaceOption :: String -> String -> [String] -> [String]
replaceOption _ _ [] = []
replaceOption target replacement (option : _ : rest)
  | option == target = option : replacement : rest
replaceOption target replacement (value : rest) =
  value : replaceOption target replacement rest
