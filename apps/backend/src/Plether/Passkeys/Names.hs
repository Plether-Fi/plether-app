module Plether.Passkeys.Names
  ( ensureSchema, listNames, saveName, validName, validCredential ) where

import Control.Monad (void)
import Data.Char (isControl)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, Only (..), execute, execute_, query, withTransaction)

ensureSchema :: Connection -> IO ()
ensureSchema connection = withTransaction connection $ do
  -- Multiple API tasks can start together on the first rollout. Serialize
  -- catalog creation; IF NOT EXISTS alone does not prevent that PostgreSQL race.
  _ <- query connection "SELECT 1::int FROM pg_advisory_xact_lock(7431, 1)" () :: IO [Only Int]
  void $ execute_ connection
    "CREATE TABLE IF NOT EXISTS passkey_names (privy_user_id TEXT NOT NULL, credential_id TEXT NOT NULL CHECK (length(credential_id) BETWEEN 1 AND 2048), name TEXT NOT NULL CHECK (length(name) BETWEEN 1 AND 64), updated_at TIMESTAMPTZ NOT NULL DEFAULT now(), PRIMARY KEY(privy_user_id, credential_id))"

listNames :: Connection -> Text -> IO [(Text, Text)]
listNames connection user = query connection
  "SELECT credential_id, name FROM passkey_names WHERE privy_user_id = ? ORDER BY credential_id LIMIT 200" (Only user)

saveName :: Connection -> Text -> Text -> Text -> IO Bool
saveName connection user credential name = withTransaction connection $ do
  -- Serialize edits only within this account. Distinct keys are independent
  -- upserts, so editing on another device cannot overwrite unrelated labels.
  _ <- query connection "SELECT 1::int FROM pg_advisory_xact_lock(hashtextextended(?, 7431))" (Only user) :: IO [Only Int]
  if T.null name then do
    void $ execute connection "DELETE FROM passkey_names WHERE privy_user_id = ? AND credential_id = ?" (user, credential)
    pure True
  else do
    changed <- execute connection
      "INSERT INTO passkey_names (privy_user_id, credential_id, name) SELECT ?, ?, ? WHERE (SELECT count(*) FROM passkey_names WHERE privy_user_id = ?) < 200 OR EXISTS (SELECT 1 FROM passkey_names WHERE privy_user_id = ? AND credential_id = ?) ON CONFLICT (privy_user_id, credential_id) DO UPDATE SET name = EXCLUDED.name, updated_at = now()"
      (user, credential, name, user, user, credential)
    pure $ changed == 1

validCredential :: Text -> Bool
validCredential value = not (T.null value) && T.length value <= 2048
  && T.all (\c -> c `elem` (['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "_-")) value

validName :: Text -> Bool
validName value = T.length value <= 64 && not (T.any isControl value)
