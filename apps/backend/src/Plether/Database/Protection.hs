{-# LANGUAGE TemplateHaskell #-}
module Plether.Database.Protection (ensureProtectionSchema) where

import Control.Monad (void)
import Data.String (fromString)
import Database.PostgreSQL.Simple (Connection, Query, execute_)
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (addDependentFile, lift)

-- Compile the same migration consumed by the Node indexer, avoiding two DDL copies.
protectionSchema :: Query
protectionSchema = fromString $(do
  addDependentFile "schema.sql"
  contents <- runIO $ readFile "schema.sql"
  let isStart = (== "-- v1.2.1 Book event history and independent trigger/retry worker checkpoints.")
      isEnd = (== "-- Cached six-feed Pyth update payloads used by reveal payload APIs and keeper execution")
      statements = takeWhile (not . isEnd) $ dropWhile (not . isStart) $ lines contents
  if null statements then fail "Protection schema marker missing" else lift $ unlines statements)

ensureProtectionSchema :: Connection -> IO ()
ensureProtectionSchema conn = void $ execute_ conn protectionSchema
