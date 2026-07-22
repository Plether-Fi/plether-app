module Main (main) where

import Control.Monad (forM)
import qualified Data.Text as T
import Plether.AA.SimpleAccount (deriveTradingAccountAddress)
import Plether.Config (Config (..), loadConfig)
import Plether.Database (DbPool, newDbPool, withDb)
import Plether.Database.Insights
  ( ParticipantRow (..)
  , applyCompetitionParticipantWalletRemaps
  , ensureInsightsSchema
  , finalizeCompetition
  , getCompetitionParticipantTraderReferenceByAlias
  , listCompetitionParticipants
  , setParticipantEligibility
  , stageCompetitionParticipantWalletRemap
  , upsertCompetitionParticipant
  )
import Plether.Insights.Competition
  ( july2026CompetitionSlug
  , participantEligibilityFromText
  )
import Plether.Utils.Address (isValidAddress)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  eConfig <- loadConfig
  case eConfig of
    Left err -> failWith $ "Configuration error: " <> err
    Right cfg ->
      case cfgDatabaseUrl cfg of
        Nothing -> failWith "DATABASE_URL is required for plether-insights-admin"
        Just databaseUrl -> do
          pool <- newDbPool databaseUrl
          withDb pool $ \conn ->
            ensureInsightsSchema
              conn
              (cfgPerpsChainId cfg)
              (cfgPerpsOrderRouter cfg)
              (cfgPerpsUsdc cfg)
              (cfgPerpsMarginClearinghouse cfg)
              (cfgPerpsAccountLens cfg)
          runCommand pool args

runCommand :: DbPool -> [String] -> IO ()
runCommand pool = \case
  ["register", rawTraderReference, rawWallet] ->
    register pool rawTraderReference rawWallet Nothing
  ["register", rawTraderReference, rawWallet, rawAlias] ->
    register pool rawTraderReference rawWallet $ Just rawAlias
  ["stage-wallet-remap", rawTraderReference, rawOldWallet, rawNewWallet] ->
    stageWalletRemap pool rawTraderReference rawOldWallet rawNewWallet
  ["stage-trading-account-remap", rawTraderReference, rawOldWallet] ->
    stageTradingAccountRemap pool rawTraderReference rawOldWallet
  "stage-alias-owner-remaps" : rawMappings ->
    stageAliasOwnerRemaps pool rawMappings
  ["apply-wallet-remaps", rawExpectedCount, rawAppliedBy] ->
    applyWalletRemaps pool rawExpectedCount rawAppliedBy
  ["review", rawWallet, rawStatus, rawReviewer] ->
    review pool rawWallet rawStatus rawReviewer Nothing
  ["review", rawWallet, rawStatus, rawReviewer, rawReason] ->
    review pool rawWallet rawStatus rawReviewer $ Just rawReason
  ["list"] -> do
    rows <- withDb pool $ \conn ->
      listCompetitionParticipants conn july2026CompetitionSlug
    mapM_ printParticipant rows
  ["finalize", rawReviewer] -> finalize pool rawReviewer
  _ -> failWith usage

register :: DbPool -> String -> String -> Maybe String -> IO ()
register pool rawTraderReference rawWallet rawAlias = do
  let wallet = T.pack rawWallet
      traderReference = T.strip $ T.pack rawTraderReference
      alias = T.pack <$> rawAlias
  if T.null traderReference
    then failWith "TRADER_REFERENCE must be a non-empty opaque registration identifier"
    else if not $ isValidAddress wallet
    then failWith $ "Invalid Ethereum address: " <> rawWallet
    else do
      result <- withDb pool $ \conn ->
        upsertCompetitionParticipant
          conn
          july2026CompetitionSlug
          traderReference
          wallet
          alias
      case result of
        Left err -> failWith $ T.unpack err
        Right () -> putStrLn $ "Registered " <> T.unpack (canonicalAddress wallet)

stageWalletRemap :: DbPool -> String -> String -> String -> IO ()
stageWalletRemap pool rawTraderReference rawOldWallet rawNewWallet = do
  let traderReference = T.strip $ T.pack rawTraderReference
      oldWallet = T.pack rawOldWallet
      newWallet = T.pack rawNewWallet
  if T.null traderReference
    then failWith "TRADER_REFERENCE must be a non-empty opaque registration identifier"
    else if not $ isValidAddress oldWallet
      then failWith "OLD_WALLET must be a valid Ethereum address"
      else if not $ isValidAddress newWallet
        then failWith "NEW_WALLET must be a valid Ethereum address"
        else do
          result <- withDb pool $ \conn ->
            stageCompetitionParticipantWalletRemap
              conn
              july2026CompetitionSlug
              traderReference
              oldWallet
              newWallet
          case result of
            Left err -> failWith $ T.unpack err
            Right () -> putStrLn "Staged participant wallet remap"

stageTradingAccountRemap :: DbPool -> String -> String -> IO ()
stageTradingAccountRemap pool rawTraderReference rawOldWallet = do
  let traderReference = T.strip $ T.pack rawTraderReference
      oldWallet = T.pack rawOldWallet
  if T.null traderReference
    then failWith "TRADER_REFERENCE must be a non-empty opaque registration identifier"
    else if not $ isValidAddress oldWallet
      then failWith "OLD_WALLET must be a valid Ethereum address"
      else case deriveTradingAccountAddress oldWallet of
        Left err -> failWith $ "Trading Account derivation failed: " <> T.unpack err
        Right newWallet -> do
          result <- withDb pool $ \conn ->
            stageCompetitionParticipantWalletRemap
              conn
              july2026CompetitionSlug
              traderReference
              oldWallet
              newWallet
          case result of
            Left err -> failWith $ T.unpack err
            Right () ->
              putStrLn $
                "Staged Trading Account remap "
                  <> T.unpack (canonicalAddress oldWallet)
                  <> " -> "
                  <> T.unpack (canonicalAddress newWallet)

stageAliasOwnerRemaps :: DbPool -> [String] -> IO ()
stageAliasOwnerRemaps pool rawMappings =
  case mappingTriples rawMappings of
    Nothing ->
      failWith "Alias remaps must be provided as repeated ALIAS OLD_WALLET OWNER_WALLET triples"
    Just [] -> failWith "At least one alias remap is required"
    Just mappings | length mappings > 20 -> failWith "At most 20 alias remaps can be staged per batch"
    Just mappings -> do
      resolved <- forM mappings $ \(rawAlias, rawOldWallet, rawOwnerWallet) -> do
        let alias = T.strip $ T.pack rawAlias
            oldWallet = T.pack rawOldWallet
            ownerWallet = T.pack rawOwnerWallet
        if T.null alias
          then failWith "ALIAS must not be empty"
          else if not $ isValidAddress oldWallet
            then failWith "OLD_WALLET must be a valid Ethereum address"
            else if not $ isValidAddress ownerWallet
              then failWith "OWNER_WALLET must be a valid Ethereum address"
              else case deriveTradingAccountAddress ownerWallet of
                Left err -> failWith $ T.unpack err
                Right newWallet -> do
                  reference <- withDb pool $ \conn ->
                    getCompetitionParticipantTraderReferenceByAlias
                      conn
                      july2026CompetitionSlug
                      alias
                  case reference of
                    Left err -> failWith $ T.unpack err
                    Right traderReference ->
                      pure (traderReference, oldWallet, newWallet)
      results <- forM resolved $ \(traderReference, oldWallet, newWallet) ->
        withDb pool $ \conn ->
          stageCompetitionParticipantWalletRemap
            conn
            july2026CompetitionSlug
            traderReference
            oldWallet
            newWallet
      case [err | Left err <- results] of
        [] -> putStrLn $ "Staged " <> show (length results) <> " participant wallet remaps"
        err : _ -> failWith $ T.unpack err

mappingTriples :: [String] -> Maybe [(String, String, String)]
mappingTriples = \case
  [] -> Just []
  alias : oldWallet : newWallet : rest ->
    ((alias, oldWallet, newWallet) :) <$> mappingTriples rest
  _ -> Nothing

applyWalletRemaps :: DbPool -> String -> String -> IO ()
applyWalletRemaps pool rawExpectedCount rawAppliedBy = do
  let appliedBy = T.strip $ T.pack rawAppliedBy
  case readMaybe rawExpectedCount of
    Nothing -> failWith "EXPECTED_COUNT must be a positive integer"
    Just expectedCount
      | expectedCount <= 0 -> failWith "EXPECTED_COUNT must be a positive integer"
      | T.null appliedBy -> failWith "APPLIED_BY must not be empty"
      | otherwise -> do
          result <- withDb pool $ \conn ->
            applyCompetitionParticipantWalletRemaps
              conn
              july2026CompetitionSlug
              expectedCount
              appliedBy
          case result of
            Left err -> failWith $ T.unpack err
            Right () -> putStrLn $ "Applied " <> show expectedCount <> " participant wallet remaps"

review
  :: DbPool
  -> String
  -> String
  -> String
  -> Maybe String
  -> IO ()
review pool rawWallet rawStatus rawReviewer rawReason = do
  let wallet = T.pack rawWallet
      status = participantEligibilityFromText $ T.pack rawStatus
      reviewer = T.strip $ T.pack rawReviewer
      publicReason = normalizeOptionalText $ T.pack <$> rawReason
  if not $ isValidAddress wallet
    then failWith $ "Invalid Ethereum address: " <> rawWallet
    else if T.null reviewer
      then failWith "REVIEWER must not be empty"
    else case status of
      Nothing -> failWith "Status must be pending, eligible, under_review, or ineligible"
      Just parsedStatus -> do
        changed <-
          withDb pool $ \conn ->
            setParticipantEligibility
              conn
              july2026CompetitionSlug
              wallet
              parsedStatus
              publicReason
              reviewer
        if changed
          then putStrLn $ "Updated review status for " <> T.unpack (T.toLower wallet)
          else failWith "Participant was not found or the competition is already finalized"

finalize :: DbPool -> String -> IO ()
finalize pool rawReviewer = do
  let reviewer = T.strip $ T.pack rawReviewer
  if T.null reviewer
    then failWith "REVIEWER must not be empty"
    else do
      result <- withDb pool $ \conn ->
        finalizeCompetition conn july2026CompetitionSlug reviewer
      case result of
        Left err -> failWith $ "Competition is not ready to finalize: " <> T.unpack err
        Right () -> putStrLn "Competition standings finalized"

normalizeOptionalText :: Maybe T.Text -> Maybe T.Text
normalizeOptionalText value =
  case T.strip <$> value of
    Just normalized | not (T.null normalized) -> Just normalized
    _ -> Nothing

canonicalAddress :: T.Text -> T.Text
canonicalAddress value =
  let normalized = T.toLower $ T.strip value
   in if "0x" `T.isPrefixOf` normalized then normalized else "0x" <> normalized

printParticipant :: ParticipantRow -> IO ()
printParticipant ParticipantRow {..} =
  putStrLn $
    T.unpack iprWallet
      <> maybe "" (("\t" <>) . T.unpack) iprAlias
      <> "\t"
      <> T.unpack iprEligibilityStatus
      <> maybe "" (("\t" <>) . T.unpack) iprEligibilityReason

failWith :: String -> IO a
failWith message = ioError $ userError message

usage :: String
usage =
  unlines
    [ "Usage:"
    , "  plether-insights-admin register TRADER_REFERENCE WALLET [ALIAS]"
    , "  plether-insights-admin stage-wallet-remap TRADER_REFERENCE OLD_WALLET NEW_WALLET"
    , "  plether-insights-admin stage-trading-account-remap TRADER_REFERENCE OLD_WALLET"
    , "  plether-insights-admin stage-alias-owner-remaps ALIAS OLD_WALLET OWNER_WALLET [...]"
    , "  plether-insights-admin apply-wallet-remaps EXPECTED_COUNT APPLIED_BY"
    , "  plether-insights-admin review WALLET STATUS REVIEWER [PUBLIC_REASON]"
    , "  plether-insights-admin finalize REVIEWER"
    , "  plether-insights-admin list"
    , ""
    , "TRADER_REFERENCE is private and is never printed by list. PUBLIC_REASON is exposed by the public API."
    ]
