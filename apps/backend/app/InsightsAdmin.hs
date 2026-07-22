module Main (main) where

import Codec.Compression.GZip qualified as GZip
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isControl, isSpace)
import Data.Set qualified as Set
import qualified Data.Text as T
import Data.Text.Encoding qualified as TE
import Plether.AA.SimpleAccount (deriveTradingAccountAddress)
import Plether.Config (Config (..), loadConfig)
import Plether.Database (DbPool, newDbPool, withDb)
import Plether.Database.Insights
  ( BulkParticipantAppendResult (..)
  , ParticipantRow (..)
  , RosterSnapshotVerification (..)
  , applyCompetitionParticipantWalletRemaps
  , bulkAppendCompetitionParticipants
  , bulkApplyCompetitionParticipantWalletRemaps
  , ensureInsightsSchema
  , finalizeCompetition
  , getCompetitionParticipantTraderReferenceByAlias
  , listCompetitionParticipants
  , setParticipantEligibility
  , stageCompetitionParticipantWalletRemap
  , upsertCompetitionParticipant
  , verifyCompetitionRosterSnapshots
  )
import Plether.Insights.Competition
  ( july2026CompetitionSlug
  , participantEligibilityFromText
  )
import Plether.Insights.BulkRoster
  ( BulkParticipantEntry (..)
  , parseBulkParticipantEntries
  )
import Plether.Utils.Address (isValidAddress)
import System.Environment (getArgs, lookupEnv, unsetEnv)
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
  ["bulk-apply-alias-owner-roster", rawExpectedCount, rawAppliedBy] ->
    bulkApplyAliasOwnerRoster pool rawExpectedCount rawAppliedBy
  ["register", "bulk-append-alias-owner-roster", rawExpectedExistingCount, rawExpectedInputCount, rawRequestId] ->
    bulkAppendAliasOwnerRoster
      pool
      rawExpectedExistingCount
      rawExpectedInputCount
      rawRequestId
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
  ["list", "verify-roster-correction", rawExpectedCount] ->
    verifyRosterCorrection pool rawExpectedCount
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

bulkApplyAliasOwnerRoster :: DbPool -> String -> String -> IO ()
bulkApplyAliasOwnerRoster pool rawExpectedCount rawAppliedBy = do
  let appliedBy = T.strip $ T.pack rawAppliedBy
  expectedCount <- case readMaybe rawExpectedCount of
    Just count | count > 0 -> pure count
    _ -> failWith "EXPECTED_COUNT must be a positive integer"
  if T.null appliedBy
    then failWith "APPLIED_BY must not be empty"
    else do
      encoded <- loadBulkRosterSecret
      rosterText <- decodeBulkRosterSecret encoded
      mappings <- case parseBulkRosterMappings expectedCount rosterText of
        Left err -> failWith $ T.unpack err
        Right values -> pure values
      result <- withDb pool $ \conn ->
        bulkApplyCompetitionParticipantWalletRemaps
          conn
          july2026CompetitionSlug
          expectedCount
          appliedBy
          mappings
      case result of
        Left err -> failWith $ T.unpack err
        Right changedCount ->
          putStrLn $
            "Validated and atomically applied "
              <> show expectedCount
              <> " participant wallet remaps; changed="
              <> show changedCount
              <> ", identity="
              <> show (expectedCount - changedCount)

bulkAppendAliasOwnerRoster :: DbPool -> String -> String -> String -> IO ()
bulkAppendAliasOwnerRoster pool rawExpectedExistingCount rawExpectedInputCount rawRequestId = do
  expectedExistingCount <- positiveCount "EXPECTED_EXISTING_COUNT" rawExpectedExistingCount
  expectedInputCount <- positiveCount "EXPECTED_INPUT_COUNT" rawExpectedInputCount
  let requestId = T.strip $ T.pack rawRequestId
  if T.null requestId
    then failWith "REQUEST_ID must not be empty"
    else do
      encoded <- loadBulkRosterSecret
      rosterText <- decodeBulkRosterSecret encoded
      entries <- case parseBulkParticipantEntries expectedInputCount rosterText of
        Left err -> failWith $ T.unpack err
        Right values -> pure values
      result <- withDb pool $ \conn ->
        bulkAppendCompetitionParticipants
          conn
          july2026CompetitionSlug
          expectedExistingCount
          requestId
          [ (bpeAlias entry, bpeTraderReference entry, bpeTradingAccount entry)
          | entry <- entries
          ]
      case result of
        Left err -> failWith $ T.unpack err
        Right BulkParticipantAppendResult {..} ->
          putStrLn $
            "Validated and atomically corrected participant roster; previous="
              <> show bparPreviousCount
              <> ", input="
              <> show bparInputCount
              <> ", existing_aliases="
              <> show bparExistingAliasCount
              <> ", inserted="
              <> show bparInsertedCount
              <> ", remapped="
              <> show bparRemappedCount
              <> ", final="
              <> show bparFinalCount

positiveCount :: String -> String -> IO Integer
positiveCount label rawValue =
  case readMaybe rawValue of
    Just count | count > 0 -> pure count
    _ -> failWith $ label <> " must be a positive integer"

verifyRosterCorrection :: DbPool -> String -> IO ()
verifyRosterCorrection pool rawExpectedCount = do
  expectedCount <- positiveCount "EXPECTED_COUNT" rawExpectedCount
  result <- withDb pool $ \conn ->
    verifyCompetitionRosterSnapshots conn july2026CompetitionSlug expectedCount
  case result of
    Left err -> failWith $ T.unpack err
    Right RosterSnapshotVerification {..} ->
      putStrLn $
        "Verified roster snapshots; participantCount="
          <> show rsvParticipantCount
          <> ", snapshottedWalletCount="
          <> show rsvSnapshottedWalletCount
          <> ", startSnapshotCount="
          <> show rsvStartSnapshotCount
          <> ", missingStartSnapshotCount="
          <> show rsvMissingStartSnapshotCount
          <> ", openPositionCount="
          <> show rsvOpenPositionCount
          <> ", pendingOrderCount="
          <> show rsvPendingOrderCount
          <> ", bankrollMismatchCount="
          <> show rsvBankrollMismatchCount

loadBulkRosterSecret :: IO T.Text
loadBulkRosterSecret = do
  let secretName = "INSIGHTS_BULK_ROSTER_GZIP_BASE64"
      chunkCountName = secretName <> "_CHUNK_COUNT"
  encodedSecret <- lookupEnv secretName
  unsetEnv secretName
  case encodedSecret of
    Just value | not $ null value -> pure $ T.pack value
    _ -> do
      rawChunkCount <- lookupEnv chunkCountName
      unsetEnv chunkCountName
      chunkCount <- case rawChunkCount >>= (readMaybe :: String -> Maybe Int) of
        Just count | count > 0 && count <= 64 -> pure count
        _ -> failWith $ secretName <> " or a valid " <> chunkCountName <> " is required"
      chunks <- forM [1 .. chunkCount] $ \index -> do
        let suffix = if index < 10 then "0" <> show index else show index
            chunkName = secretName <> "_" <> suffix
        chunk <- lookupEnv chunkName
        unsetEnv chunkName
        case chunk of
          Just value | not $ null value -> pure $ T.pack value
          _ -> failWith $ "Bulk roster secret chunk " <> show index <> " is missing"
      pure $ T.concat chunks

decodeBulkRosterSecret :: T.Text -> IO T.Text
decodeBulkRosterSecret encoded =
  case Base64.decode $ TE.encodeUtf8 $ T.filter (not . isSpace) encoded of
    Left _ -> failWith "The bulk roster secret is not valid base64"
    Right compressed -> do
      decompressedResult <-
        try (evaluate $ LBS.toStrict $ GZip.decompress $ LBS.fromStrict compressed)
          :: IO (Either SomeException BS.ByteString)
      case decompressedResult of
        Left _ -> failWith "The bulk roster secret is not valid gzip data"
        Right bytes -> case TE.decodeUtf8' bytes of
          Left _ -> failWith "The bulk roster secret is not valid UTF-8"
          Right value -> pure value

parseBulkRosterMappings :: Integer -> T.Text -> Either T.Text [(T.Text, T.Text, T.Text)]
parseBulkRosterMappings expectedCount input = do
  mappings <- traverse parseLine $ filter (not . T.null . T.strip) $ T.lines input
  if fromIntegral (length mappings) /= expectedCount
    then Left "Bulk roster entry count does not match EXPECTED_COUNT"
    else do
      requireUnique "alias" [alias | (alias, _, _) <- mappings]
      requireUnique "OLD_WALLET" [oldWallet | (_, oldWallet, _) <- mappings]
      requireUnique "Trading Account destination" [newWallet | (_, _, newWallet) <- mappings]
      pure mappings
  where
    parseLine line =
      case T.splitOn "\t" line of
        [rawAlias, rawOldWallet, rawOwnerWallet] -> do
          let alias = T.strip rawAlias
              aliasKey = T.toCaseFold alias
              oldWallet = canonicalAddress rawOldWallet
              ownerWallet = canonicalAddress rawOwnerWallet
          if T.null alias || T.length alias > 80 || T.any isControl alias
            then Left "Bulk roster contains an invalid alias"
            else if not $ "@" `T.isPrefixOf` alias
              then Left "Bulk roster aliases must use the public @handle form"
              else if not $ isValidAddress oldWallet
                then Left "Bulk roster contains an invalid OLD_WALLET"
                else if not $ isValidAddress ownerWallet
                  then Left "Bulk roster contains an invalid OWNER_WALLET"
                  else case deriveTradingAccountAddress ownerWallet of
                    Left err -> Left err
                    Right newWallet -> Right (aliasKey, oldWallet, canonicalAddress newWallet)
        _ -> Left "Every bulk roster line must be ALIAS, OLD_WALLET, OWNER_WALLET TSV"

    requireUnique label values
      | Set.size (Set.fromList values) == length values = Right ()
      | otherwise = Left $ "Bulk roster contains a duplicate " <> label

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
    , "  plether-insights-admin bulk-apply-alias-owner-roster EXPECTED_COUNT APPLIED_BY"
    , "  plether-insights-admin register bulk-append-alias-owner-roster EXPECTED_EXISTING_COUNT EXPECTED_INPUT_COUNT REQUEST_ID"
    , "  plether-insights-admin apply-wallet-remaps EXPECTED_COUNT APPLIED_BY"
    , "  plether-insights-admin review WALLET STATUS REVIEWER [PUBLIC_REASON]"
    , "  plether-insights-admin finalize REVIEWER"
    , "  plether-insights-admin list"
    , "  plether-insights-admin list verify-roster-correction EXPECTED_COUNT"
    , ""
    , "TRADER_REFERENCE is private and is never printed by list. PUBLIC_REASON is exposed by the public API."
    ]
