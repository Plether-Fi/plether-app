module Plether.Database.AaPreparation
  ( PreparationClaim (..), claimPreparation, savePreparedOperation, releasePreparation, linkPreparation ) where

import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple

data PreparationClaim = PreparationBusy | PreparationConflict | PreparationExpired | PreparationDisabled | PreparationClaimed (Maybe Value)
  deriving stock (Eq, Show)

-- A short transaction owns only the lease, never an upstream RPC or KMS call.
-- The chosen operation is immutable once written, including across lease loss.
claimPreparation :: Connection -> Bool -> Text -> Text -> Text -> Text -> Text -> IO PreparationClaim
claimPreparation conn allowNew client sender identifier intent lease = withTransaction conn $ do
  if allowNew then do
    _ <- execute conn
      "INSERT INTO aa_preparations(client_key,sender,preparation_id,intent_hash) VALUES (?,?,?,?) ON CONFLICT DO NOTHING"
      (client, sender, identifier, intent)
    pure ()
  else pure ()
  rows <- query conn
    "SELECT intent_hash,operation,lease_until IS NULL OR lease_until < clock_timestamp(),expires_at>clock_timestamp() FROM aa_preparations WHERE client_key=? AND sender=? AND preparation_id=? FOR UPDATE"
    (client,sender,identifier) :: IO [(Text, Maybe Value, Bool, Bool)]
  case rows of
    [(stored, operation, available, fresh)]
      | stored /= intent -> pure PreparationConflict
      | not fresh -> pure PreparationExpired
      | not available -> pure PreparationBusy
      | otherwise -> do
          _ <- execute conn
            "UPDATE aa_preparations SET lease_token=?,lease_until=clock_timestamp()+interval '60 seconds',updated_at=clock_timestamp() WHERE client_key=? AND sender=? AND preparation_id=?"
            (lease,client,sender,identifier)
          pure $ PreparationClaimed operation
    [] | not allowNew -> pure PreparationDisabled
    _ -> pure PreparationConflict

savePreparedOperation :: Connection -> Text -> Text -> Text -> Text -> Value -> IO Bool
savePreparedOperation conn client sender identifier lease operation = do
  affected <- execute conn
    "UPDATE aa_preparations SET operation=?::jsonb,updated_at=clock_timestamp() WHERE client_key=? AND sender=? AND preparation_id=? AND lease_token=? AND lease_until>clock_timestamp() AND operation IS NULL"
    (TE.decodeUtf8 $ LBS.toStrict $ encode operation,client,sender,identifier,lease)
  pure $ affected == 1

linkPreparation :: Connection -> Text -> Text -> Text -> Text -> Text -> IO Bool
linkPreparation conn client sender identifier lease digest = do
  affected <- execute conn
    "UPDATE aa_preparations SET authorization_digest=? WHERE client_key=? AND sender=? AND preparation_id=? AND lease_token=? AND lease_until>clock_timestamp() AND (authorization_digest IS NULL OR authorization_digest=?)"
    (digest,client,sender,identifier,lease,digest)
  pure $ affected == 1

releasePreparation :: Connection -> Text -> Text -> Text -> Text -> IO ()
releasePreparation conn client sender identifier lease = do
  _ <- execute conn
    "UPDATE aa_preparations SET lease_token=NULL,lease_until=NULL,updated_at=clock_timestamp() WHERE client_key=? AND sender=? AND preparation_id=? AND lease_token=?"
    (client,sender,identifier,lease)
  pure ()
