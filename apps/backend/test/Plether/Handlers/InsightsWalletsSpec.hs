module Plether.Handlers.InsightsWalletsSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Plether.Ethereum.Contracts.CfdEngineAccountLens (AccountLedgerSnapshot (..))
import Plether.Handlers.InsightsWallets
  ( BalanceEventRow (..)
  , BalanceHistoryPoint (..)
  , DivisionsEquityBridge (..)
  , DivisionsWeekRecord (..)
  , buildBalanceHistory
  , buildDivisionsEquityBridge
  , classifyVaultTranche
  , extractDivisionsHistory
  , ledgerSnapshotToJson
  )
import Test.Hspec

spec :: Spec
spec = describe "Plether.Handlers.InsightsWallets" $ do
  it "classifies Senior and Junior vault addresses case-insensitively" $ do
    let senior = "0x1111111111111111111111111111111111111111"
        junior = "0x2222222222222222222222222222222222222222"
    classifyVaultTranche senior junior "0x1111111111111111111111111111111111111111" `shouldBe` "senior"
    classifyVaultTranche senior junior "0x2222222222222222222222222222222222222222" `shouldBe` "junior"
    classifyVaultTranche senior junior "0x3333333333333333333333333333333333333333" `shouldBe` "unknown"

  it "extracts account-scoped weekly Divisions history from publication JSON" $ do
    let target = "0xAbCdEf0123456789012345678901234567890123"
        pub1 =
          object
            [ "publishedAt" .= ("2026-11-23T00:05:00Z" :: String)
            , "week"
                .= object
                  [ "startsAt" .= ("2026-11-16T00:00:00Z" :: String)
                  , "closesAt" .= ("2026-11-23T00:00:00Z" :: String)
                  ]
            , "standings"
                .= [ object
                      [ "address" .= ("0xabcdef0123456789012345678901234567890123" :: String)
                      , "displayName" .= ("alice" :: String)
                      , "rank" .= (2 :: Int)
                      , "tier" .= ("gold" :: String)
                      , "previousTier" .= ("silver" :: String)
                      , "movement" .= ("promoted" :: String)
                      , "weeklyPnlAssets" .= ("420000000" :: String)
                      , "adjustedEquityAssets" .= ("10420000000" :: String)
                      , "activeDays" .= (4 :: Int)
                      , "qualifying" .= True
                      ]
                   ]
            ]
        pub2 =
          object
            [ "publishedAt" .= ("2026-11-16T00:05:00Z" :: String)
            , "standings"
                .= [ object
                      [ "address" .= ("0x9999999999999999999999999999999999999999" :: String)
                      , "rank" .= (1 :: Int)
                      ]
                   ]
            ]
        history = extractDivisionsHistory target [(1794787200, 330000000, pub1), (1794182400, 329000000, pub2)]
    length history `shouldBe` 1
    case history of
      [week] -> do
        dwrRank week `shouldBe` Just 2
        dwrTier week `shouldBe` Just "gold"
        dwrMovement week `shouldBe` Just "promoted"
        dwrWeeklyPnlAssets week `shouldBe` Just "420000000"
        dwrActiveDays week `shouldBe` Just 4
        dwrQualifying week `shouldBe` Just True
      _ -> expectationFailure "Expected exactly one matching Divisions week"

  it "serializes AccountLedgerSnapshot with open position details and omits position when flat" $ do
    let baseSnap =
          AccountLedgerSnapshot
            { alsSettlementBalanceAssets = 1000000000
            , alsFreeSettlementAssets = 800000000
            , alsActivePositionMarginAssets = 200000000
            , alsOtherLockedMarginAssets = 0
            , alsPositionMarginBucketAssets = 200000000
            , alsCommittedOrderMarginBucketAssets = 0
            , alsReservedSettlementBucketAssets = 0
            , alsExecutionBountyReserveAssets = 500000
            , alsCommittedMarginAssets = 0
            , alsTraderClaimBalanceAssets = 0
            , alsPendingOrderCount = 0
            , alsCloseReachableAssets = 1000000000
            , alsLiquidationReachableSettlementAssets = 1000000000
            , alsTerminalPriceCollectibleCapAssets = 1000000000
            , alsAccountEquityAssets = 1050000000
            , alsFreeBuyingPowerAssets = 800000000
            , alsHasPosition = True
            , alsSide = 0
            , alsSize = 500000000000000000000
            , alsMargin = 200000000
            , alsEntryPrice = 104500000000000000000
            , alsUnrealizedPnlAssets = 50000000
            , alsNetEquityAssets = 1050000000
            , alsLiquidatable = False
            }
    case ledgerSnapshotToJson baseSnap of
      Object obj -> do
        KM.lookup (Key.fromText "hasPosition") obj `shouldBe` Just (Bool True)
        case KM.lookup (Key.fromText "position") obj of
          Just (Object pos) -> KM.lookup (Key.fromText "side") pos `shouldBe` Just (String "long")
          _ -> expectationFailure "Expected position object when hasPosition is True"
      _ -> expectationFailure "Expected JSON object"

    case ledgerSnapshotToJson (baseSnap {alsHasPosition = False}) of
      Object obj -> KM.lookup (Key.fromText "position") obj `shouldBe` Just Null
      _ -> expectationFailure "Expected JSON object"

  it "reconstructs chronological 90d balance history from deposits, receipts, withdrawals, and live ledger" $ do
    let nowUnix = 1800000000
        oldTs = nowUnix - 100 * 86400
        midTs = nowUnix - 20 * 86400
        recentTs = nowUnix - 2 * 86400
        events =
          [ BalanceEventRow "Deposit" (Just 1000000000) Nothing Nothing Nothing Nothing Nothing 100 0 oldTs
          , BalanceEventRow "Close" Nothing (Just 150000000) (Just 1000000000) (Just 1140000000) (Just 0) (Just 10000000) 200 0 midTs
          , BalanceEventRow "Withdraw" (Just 200000000) Nothing Nothing Nothing Nothing Nothing 300 0 recentTs
          ]
        liveSnap =
          AccountLedgerSnapshot
            { alsSettlementBalanceAssets = 950000000
            , alsFreeSettlementAssets = 950000000
            , alsActivePositionMarginAssets = 0
            , alsOtherLockedMarginAssets = 0
            , alsPositionMarginBucketAssets = 0
            , alsCommittedOrderMarginBucketAssets = 0
            , alsReservedSettlementBucketAssets = 0
            , alsExecutionBountyReserveAssets = 0
            , alsCommittedMarginAssets = 0
            , alsTraderClaimBalanceAssets = 0
            , alsPendingOrderCount = 0
            , alsCloseReachableAssets = 950000000
            , alsLiquidationReachableSettlementAssets = 950000000
            , alsTerminalPriceCollectibleCapAssets = 950000000
            , alsAccountEquityAssets = 950000000
            , alsFreeBuyingPowerAssets = 950000000
            , alsHasPosition = False
            , alsSide = 0
            , alsSize = 0
            , alsMargin = 0
            , alsEntryPrice = 0
            , alsUnrealizedPnlAssets = 0
            , alsNetEquityAssets = 950000000
            , alsLiquidatable = False
            }
        pts = buildBalanceHistory nowUnix 400 (Just liveSnap) events
    -- Preserves pre-90d baseline anchor + 2 in-window points + live anchor
    map bhpBalanceAssets pts `shouldBe` [1000000000, 1150000000, 950000000, 950000000]
    map bhpSource pts `shouldBe` ["transfer", "receipt", "transfer", "ledger"]
    bhpNetEquityAssets (last pts) `shouldBe` Just 950000000

  it "returns empty balance history when account has no activity and zero live balance" $ do
    buildBalanceHistory 1800000000 400 Nothing [] `shouldBe` []

  it "computes Divisions weekly equity bridge accounting for deposits, withdrawals, and pending carry" $ do
    let cutoffSnap =
          AccountLedgerSnapshot
            { alsSettlementBalanceAssets = 1500000000
            , alsFreeSettlementAssets = 1100000000
            , alsActivePositionMarginAssets = 400000000
            , alsOtherLockedMarginAssets = 0
            , alsPositionMarginBucketAssets = 400000000
            , alsCommittedOrderMarginBucketAssets = 0
            , alsReservedSettlementBucketAssets = 0
            , alsExecutionBountyReserveAssets = 0
            , alsCommittedMarginAssets = 0
            , alsTraderClaimBalanceAssets = 50000000
            , alsPendingOrderCount = 0
            , alsCloseReachableAssets = 1500000000
            , alsLiquidationReachableSettlementAssets = 1500000000
            , alsTerminalPriceCollectibleCapAssets = 1500000000
            , alsAccountEquityAssets = 1650000000
            , alsFreeBuyingPowerAssets = 1100000000
            , alsHasPosition = True
            , alsSide = 0
            , alsSize = 500000000000000000000
            , alsMargin = 400000000
            , alsEntryPrice = 95000000
            , alsUnrealizedPnlAssets = 100000000
            , alsNetEquityAssets = 1650000000
            , alsLiquidatable = False
            }
        mBridge = buildDivisionsEquityBridge (Just 1000000000) 500000000 200000000 (Just (cutoffSnap, 10000000))
    case mBridge of
      Just bridge -> do
        debStartingEquityAssets bridge `shouldBe` 1000000000
        debDepositsAssets bridge `shouldBe` 500000000
        debWithdrawalsAssets bridge `shouldBe` 200000000
        debAdjustedEquityAssets bridge `shouldBe` 1300000000
        -- cutoff economic equity = 1500M settlement + 100M upnl - 10M pending carry + 50M claims = 1640M
        debCutoffEquityAssets bridge `shouldBe` 1640000000
        debWeeklyPnlAssets bridge `shouldBe` 340000000
        debPendingCarryAssets bridge `shouldBe` 10000000
      Nothing -> expectationFailure "Expected Divisions equity bridge"
