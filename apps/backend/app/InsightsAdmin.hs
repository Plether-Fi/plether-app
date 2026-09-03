module Main (main) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text as T
import Plether.AA.Pimlico (resolveTradingAccountAddress)
import Plether.Config (Config (..), loadConfig)
import Plether.Database (DbPool, newDbPool, withDb)
import Plether.Database.Insights
  ( FinalizationCanonicalityTarget (..)
  , ParticipantRow (..)
  , applyCompetitionParticipantWalletRemaps
  , ensureInsightsSchema
  , finalizeCompetition
  , listCompetitionParticipants
  , setParticipantEligibility
  , stageCompetitionParticipantWalletRemap
  , upsertCompetitionParticipant
  )
import Plether.Database.Schema (ensureTestnetFaucetSchema)
import qualified Plether.Database.Insights.Registration as RegistrationDb
import Plether.Insights.Competition
  ( CompetitionRules (..)
  , competitionRulesForSlug
  , participantEligibilityFromText
  )
import Plether.Insights.Registration.Config (RegistrationConfig)
import Plether.Insights.Registration.Rotation
  ( RegistrationEmailRotationResult (..)
  , registrationKeyReferencePreflight
  , registrationKeyReferenceTotal
  , rotateRegistrationEmails
  )
import Plether.Ethereum.Client (EthClient, RpcClientOptions (..), newClientWithOptions)
import Plether.Ethereum.Rpc (RpcBlock (..), ethGetBlockByNumber)
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
          perpsClient <-
            newClientWithOptions $
              RpcClientOptions (cfgPerpsRpcUrl cfg) (cfgPerpsRpcAuthToken cfg) "insights-admin"
          withDb pool ensureTestnetFaucetSchema
          withDb pool $ \conn ->
            ensureInsightsSchema
              conn
              (cfgInsightsCompetitionRules cfg)
              (cfgPerpsChainId cfg)
              (cfgPerpsOrderRouter cfg)
              (cfgPerpsUsdc cfg)
              (cfgPerpsMarginClearinghouse cfg)
              (cfgPerpsAccountLens cfg)
              (cfgInsightsCompetitionReleaseManifest cfg)
          case cfgRegistrationConfig cfg of
            Just _ -> withDb pool RegistrationDb.ensureRegistrationSchema
            Nothing -> pure ()
          runCommand pool perpsClient (cfgRegistrationConfig cfg) args

runCommand :: DbPool -> EthClient -> Maybe RegistrationConfig -> [String] -> IO ()
runCommand pool perpsClient registrationConfig = \case
  ["register", rawSlug, rawTraderReference, rawWallet] ->
    withCompetitionSlug rawSlug $ \slug -> register pool slug rawTraderReference rawWallet Nothing
  ["register", rawSlug, rawTraderReference, rawWallet, rawAlias] ->
    withCompetitionSlug rawSlug $ \slug -> register pool slug rawTraderReference rawWallet $ Just rawAlias
  ["stage-wallet-remap", rawSlug, rawTraderReference, rawOldWallet, rawNewWallet] ->
    withCompetitionSlug rawSlug $ \slug -> stageWalletRemap pool slug rawTraderReference rawOldWallet rawNewWallet
  ["stage-trading-account-remap", rawSlug, rawTraderReference, rawOldWallet] ->
    withCompetitionSlug rawSlug $ \slug -> stageTradingAccountRemap pool perpsClient slug rawTraderReference rawOldWallet
  ["apply-wallet-remaps", rawSlug, rawExpectedCount, rawAppliedBy] ->
    withCompetitionSlug rawSlug $ \slug -> applyWalletRemaps pool slug rawExpectedCount rawAppliedBy
  ["review", rawSlug, rawWallet, rawStatus, rawReviewer] ->
    withCompetitionSlug rawSlug $ \slug -> review pool slug rawWallet rawStatus rawReviewer Nothing
  ["review", rawSlug, rawWallet, rawStatus, rawReviewer, rawReason] ->
    withCompetitionSlug rawSlug $ \slug -> review pool slug rawWallet rawStatus rawReviewer $ Just rawReason
  ["list", rawSlug] ->
    withCompetitionSlug rawSlug $ \slug -> do
      rows <- withDb pool $ \conn -> listCompetitionParticipants conn slug
      mapM_ printParticipant rows
  ["finalize", rawSlug, rawReviewer] ->
    withCompetitionSlug rawSlug $ \slug -> finalize pool perpsClient slug rawReviewer
  ["registration-key-preflight", rawKeyVersion] ->
    registrationKeyPreflight pool registrationConfig rawKeyVersion
  ["rotate-registration-email-key", rawOldVersion, rawExpectedCount] ->
    rotateRegistrationEmailKey pool registrationConfig rawOldVersion rawExpectedCount
  _ -> failWith usage

registrationKeyPreflight :: DbPool -> Maybe RegistrationConfig -> String -> IO ()
registrationKeyPreflight pool maybeConfig rawKeyVersion = do
  _ <- requireRegistrationConfig maybeConfig
  let keyVersion = T.strip $ T.pack rawKeyVersion
  if T.null keyVersion || T.length keyVersion > 64
    then failWith "KEY_VERSION must be a non-empty identifier of at most 64 characters"
    else do
      counts <- registrationKeyReferencePreflight pool keyVersion
      printKeyReferenceCounts counts

rotateRegistrationEmailKey
  :: DbPool
  -> Maybe RegistrationConfig
  -> String
  -> String
  -> IO ()
rotateRegistrationEmailKey pool maybeConfig rawOldVersion rawExpectedCount = do
  registrationConfig <- requireRegistrationConfig maybeConfig
  expectedCount <-
    case readMaybe rawExpectedCount of
      Just count | count > 0 -> pure count
      _ -> failWith "EXPECTED_COUNT must be a positive exact preflight count"
  result <-
    rotateRegistrationEmails
      pool
      registrationConfig
      (T.strip $ T.pack rawOldVersion)
      expectedCount
  case result of
    Left err -> failWith $ T.unpack err
    Right rotationResult -> do
      putStrLn $ "rotated_emails=" <> show (rerrRotatedEmails rotationResult)
      printKeyReferenceCounts $ rerrOldKeyReferences rotationResult

requireRegistrationConfig :: Maybe RegistrationConfig -> IO RegistrationConfig
requireRegistrationConfig =
  maybe
    (failWith "INSIGHTS_REGISTRATION_PROVISIONED=true and its complete private configuration are required")
    pure

printKeyReferenceCounts :: RegistrationDb.RegistrationKeyReferenceCounts -> IO ()
printKeyReferenceCounts counts = do
  putStrLn $ "email_references=" <> show (RegistrationDb.rkrcEmail counts)
  putStrLn $ "x_user_id_references=" <> show (RegistrationDb.rkrcXUserId counts)
  putStrLn $ "x_access_references=" <> show (RegistrationDb.rkrcXAccess counts)
  putStrLn $ "csrf_references=" <> show (RegistrationDb.rkrcCsrf counts)
  putStrLn $ "pkce_references=" <> show (RegistrationDb.rkrcPkce counts)
  putStrLn $ "wallet_message_references=" <> show (RegistrationDb.rkrcWalletMessage counts)
  putStrLn $ "total_references=" <> show (registrationKeyReferenceTotal counts)

register :: DbPool -> T.Text -> String -> String -> Maybe String -> IO ()
register pool slug rawTraderReference rawWallet rawAlias = do
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
          slug
          traderReference
          wallet
          alias
      case result of
        Left err -> failWith $ T.unpack err
        Right () -> putStrLn $ "Registered " <> T.unpack (canonicalAddress wallet)

stageWalletRemap :: DbPool -> T.Text -> String -> String -> String -> IO ()
stageWalletRemap pool slug rawTraderReference rawOldWallet rawNewWallet = do
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
              slug
              traderReference
              oldWallet
              newWallet
          case result of
            Left err -> failWith $ T.unpack err
            Right () -> putStrLn "Staged participant wallet remap"

stageTradingAccountRemap :: DbPool -> EthClient -> T.Text -> String -> String -> IO ()
stageTradingAccountRemap pool perpsClient slug rawTraderReference rawOldWallet = do
  let traderReference = T.strip $ T.pack rawTraderReference
      oldWallet = T.pack rawOldWallet
  if T.null traderReference
    then failWith "TRADER_REFERENCE must be a non-empty opaque registration identifier"
    else if not $ isValidAddress oldWallet
      then failWith "OLD_WALLET must be a valid Ethereum address"
      else do
        resolved <- resolveTradingAccountAddress perpsClient oldWallet
        case resolved of
          Left err -> failWith $ "Trading Account resolution failed: " <> T.unpack err
          Right newWallet -> do
            result <- withDb pool $ \conn ->
              stageCompetitionParticipantWalletRemap
                conn
                slug
                traderReference
                oldWallet
                newWallet
            case result of
              Left err -> failWith $ T.unpack err
              Right () -> putStrLn "Staged Trading Account remap"

applyWalletRemaps :: DbPool -> T.Text -> String -> String -> IO ()
applyWalletRemaps pool slug rawExpectedCount rawAppliedBy = do
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
              slug
              expectedCount
              appliedBy
          case result of
            Left err -> failWith $ T.unpack err
            Right () -> putStrLn $ "Applied " <> show expectedCount <> " participant wallet remaps"

review
  :: DbPool
  -> T.Text
  -> String
  -> String
  -> String
  -> Maybe String
  -> IO ()
review pool slug rawWallet rawStatus rawReviewer rawReason = do
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
              slug
              wallet
              parsedStatus
              publicReason
              reviewer
        if changed
          then putStrLn $ "Updated review status for " <> T.unpack (T.toLower wallet)
          else failWith "Participant was not found, the competition is finalized, or an eligible review is blocked by integrity flags; run list for the private flag summary"

finalize :: DbPool -> EthClient -> T.Text -> String -> IO ()
finalize pool perpsClient slug rawReviewer = do
  let reviewer = T.strip $ T.pack rawReviewer
  if T.null reviewer
    then failWith "REVIEWER must not be empty"
    else do
      result <- withDb pool $ \conn ->
        finalizeCompetition conn slug reviewer $ verifyFinalizationCanonicality perpsClient
      case result of
        Left err -> failWith $ "Competition is not ready to finalize: " <> T.unpack err
        Right () -> putStrLn "Competition standings finalized"

verifyFinalizationCanonicality
  :: EthClient
  -> FinalizationCanonicalityTarget
  -> IO (Either T.Text ())
verifyFinalizationCanonicality client target =
  verifyBlocks
    [ ("start boundary", fctStartBlock target, fctStartBlockHash target)
    , ("baseline", fctBaselineBlock target, fctBaselineBlockHash target)
    , ("score cutoff", fctScoreCutoffBlock target, fctScoreCutoffBlockHash target)
    , ("indexer cursor", fctIndexerBlock target, fctIndexerBlockHash target)
    ]
  where
    verifyBlocks [] = verifyBlock "indexer cursor (stability recheck)" (fctIndexerBlock target) (fctIndexerBlockHash target)
    verifyBlocks ((label, blockNumber, expectedHash) : rest) =
      verifyBlock label blockNumber expectedHash >>= \case
        Left err -> pure $ Left err
        Right () -> verifyBlocks rest

    verifyBlock label blockNumber expectedHash = do
      result <- ethGetBlockByNumber client blockNumber
      pure $ case result of
        Left err -> Left $ label <> " RPC lookup failed: " <> T.pack (show err)
        Right block
          | T.toLower (rpcBlockHash block) /= T.toLower expectedHash ->
              Left $ label <> " block hash no longer matches the canonical chain"
          | otherwise -> Right ()

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
      <> "\tintegrity_flags="
      <> LBS8.unpack (encode iprIntegrityFlags)

withCompetitionSlug :: String -> (T.Text -> IO a) -> IO a
withCompetitionSlug rawSlug action =
  case competitionRulesForSlug $ T.pack rawSlug of
    Nothing -> failWith $ "Unknown versioned competition slug: " <> rawSlug
    Just rules -> action $ crSlug rules

failWith :: String -> IO a
failWith message = ioError $ userError message

usage :: String
usage =
  unlines
    [ "Usage:"
    , "  plether-insights-admin register COMPETITION_SLUG TRADER_REFERENCE WALLET [ALIAS]"
    , "  plether-insights-admin stage-wallet-remap COMPETITION_SLUG TRADER_REFERENCE OLD_WALLET NEW_WALLET"
    , "  plether-insights-admin stage-trading-account-remap COMPETITION_SLUG TRADER_REFERENCE OLD_WALLET"
    , "  plether-insights-admin apply-wallet-remaps COMPETITION_SLUG EXPECTED_COUNT APPLIED_BY"
    , "  plether-insights-admin review COMPETITION_SLUG WALLET STATUS REVIEWER [PUBLIC_REASON]"
    , "  plether-insights-admin finalize COMPETITION_SLUG REVIEWER"
    , "  plether-insights-admin list COMPETITION_SLUG"
    , "  plether-insights-admin registration-key-preflight KEY_VERSION"
    , "  plether-insights-admin rotate-registration-email-key OLD_VERSION EXPECTED_COUNT"
    , ""
    , "TRADER_REFERENCE is private and is never printed by list. PUBLIC_REASON is exposed by the public API."
    ]
