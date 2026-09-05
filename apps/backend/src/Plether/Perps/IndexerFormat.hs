module Plether.Perps.IndexerFormat
  ( PerpsIndexerFormat (..)
  , indexerName
  , indexerFormatForLifecycleBook
  , competitionIndexerFormat
  , competitionIndexerNamespace
  , competitionIndexerNamespaceSql
  ) where

import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Query)
import Plether.Insights.Competition (july2026CompetitionSlug)

-- | Data format is a protocol property, independent of contract addresses.
data PerpsIndexerFormat = LegacyV1 | BoundedV2
  deriving stock (Eq, Show)

indexerName :: PerpsIndexerFormat -> Text
indexerName LegacyV1 = "perps-history-costs-v1"
indexerName BoundedV2 = "perps-history-costs-v2:finalized-abi3"

-- Release validation separately checks the configured lifecycle binding.
indexerFormatForLifecycleBook :: Maybe Text -> PerpsIndexerFormat
indexerFormatForLifecycleBook Nothing = LegacyV1
indexerFormatForLifecycleBook (Just _) = BoundedV2

-- The archived July competition used V1. Current competitions use bounded V2.
-- Each competition's immutable router still isolates all its data and locks.
competitionIndexerFormat :: Text -> PerpsIndexerFormat
competitionIndexerFormat slug
  | slug == july2026CompetitionSlug = LegacyV1
  | otherwise = BoundedV2

competitionIndexerNamespace :: Text -> Text
competitionIndexerNamespace slug = indexerName (competitionIndexerFormat slug) <> ":"

-- | The argument is an internal SQL column expression, never user input.
-- Keep SQL readers and Haskell cursor writers on the same format names.
competitionIndexerNamespaceSql :: Query -> Query
competitionIndexerNamespaceSql slugColumn =
  "(CASE WHEN " <> slugColumn <> " = " <> literal july2026CompetitionSlug
    <> " THEN " <> literal (indexerName LegacyV1 <> ":")
    <> " ELSE " <> literal (indexerName BoundedV2 <> ":") <> " END)"
 where
  literal value = fromString $ T.unpack $ "'" <> T.replace "'" "''" value <> "'"
