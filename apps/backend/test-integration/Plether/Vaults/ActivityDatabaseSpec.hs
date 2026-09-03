module Plether.Vaults.ActivityDatabaseSpec
  ( vaultActivityDatabaseSpec
  ) where

import Control.Exception (SomeException, finally, try)
import Data.Either (isLeft)
import Data.Maybe (isNothing)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, withTransaction)
import Plether.Database (DbPool, newDbPool, withDb)
import Plether.Database.VaultActivity
  ( VaultActivityDeployment (..)
  , VaultHolderRow (..)
  , VaultRequestRow (..)
  , ensureVaultActivitySchema
  , getVaultActivityIndexerState
  , getVaultHolders
  , getVaultRequestIds
  , getVaultRequests
  , insertVaultLogIdentityExact
  , insertVaultRequestExact
  , insertVaultShareTransferExact
  , recomputeVaultHolderBalance
  , resetVaultActivityDeployment
  , setVaultActivityIndexerState
  , tryLockVaultActivityIndexer
  , unlockVaultActivityIndexer
  )
import Plether.Handlers.VaultActivity (getVaultActivity)
import Plether.Types
  ( ApiResponse (..)
  , VaultActivityCoverage (..)
  , VaultActivityResponse (..)
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

vaultActivityDatabaseSpec :: Text -> Spec
vaultActivityDatabaseSpec databaseUrl =
  describe "vault activity PostgreSQL index" $ do
    it "derives holders atomically and rejects duplicate conflicts or negative balances" $
      withVaultDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        insertTransfer conn deploymentA seniorVault zeroAddress holderA 100 1 0
        insertTransfer conn deploymentA seniorVault holderA holderB 30 2 0
        insertTransfer conn deploymentA seniorVault holderB holderB 10 3 0
        recomputeVaultHolderBalance conn deploymentA seniorVault holderA
        recomputeVaultHolderBalance conn deploymentA seniorVault holderB

        holders <- getVaultHolders conn deploymentA seniorVault 10
        holders `shouldBe`
          [ VaultHolderRow holderA 70
          , VaultHolderRow holderB 30
          ]

        -- Exact retries are idempotent, but a different payload at the same
        -- canonical log identity is never silently accepted.
        insertTransfer conn deploymentA seniorVault holderA holderB 30 2 0
        insertVaultLogIdentityExact
          conn deploymentA seniorVault "Transfer" (txHash 2) 2 (blockHash 2) 0 0 1_700_000_002
        conflicting <- try $
          insertVaultShareTransferExact
            conn deploymentA seniorVault holderA holderB 31 (txHash 2) 2 (blockHash 2) 0 0 1_700_000_002
          :: IO (Either SomeException ())
        conflicting `shouldSatisfy` isLeft
        crossEventConflict <- try $
          insertVaultLogIdentityExact
            conn deploymentA seniorVault "DepositRequest" (txHash 2) 2 (blockHash 2) 0 0 1_700_000_002
          :: IO (Either SomeException ())
        crossEventConflict `shouldSatisfy` isLeft

        negative <- try $
          withTransaction conn $ do
            insertTransfer conn deploymentA seniorVault holderA zeroAddress 80 4 0
            recomputeVaultHolderBalance conn deploymentA seniorVault holderA
          :: IO (Either SomeException ())
        negative `shouldSatisfy` isLeft
        getVaultHolders conn deploymentA seniorVault 10 `shouldReturnValue` holders

    it "paginates distinct request IDs and keeps legacy discovery out of public activity" $
      withVaultDatabase databaseUrl $ \pool -> withDb pool $ \conn -> do
        insertVaultRequestExact conn deploymentA $ request "DepositRequest" 200 10 0
        insertVaultRequestExact conn deploymentA $ request "RedeemRequest" 300 11 0
        insertVaultRequestExact conn deploymentA $
          (request "DepositRequested" 400 12 0) {vrrController = holderB}
        insertVaultRequestExact conn deploymentA $ request "RedeemRequest" 300 13 0

        getVaultRequestIds conn deploymentA seniorVault holderA 3 Nothing
          `shouldReturnValue` [400, 300, 200]
        getVaultRequestIds conn deploymentA seniorVault holderA 2 (Just 400)
          `shouldReturnValue` [300, 200]
        visible <- getVaultRequests conn deploymentA seniorVault 10
        map vrrEventName visible `shouldBe` ["RedeemRequest", "RedeemRequest", "DepositRequest"]

    it "isolates state, data rebuilds, and advisory leadership by deployment" $
      withVaultDatabase databaseUrl $ \pool -> do
        initial <- getVaultActivity pool deploymentA
        initial `shouldSatisfy` isNothing
        withDb pool $ \first -> withDb pool $ \second -> do
          tryLockVaultActivityIndexer first `shouldReturnValue` True
          tryLockVaultActivityIndexer second `shouldReturnValue` False
          unlockVaultActivityIndexer first
        withDb pool $ \conn -> do
          setState conn deploymentA 20
          setState conn deploymentB 30
          insertTransfer conn deploymentA seniorVault zeroAddress holderA 100 21 0
          insertTransfer conn deploymentB seniorVault zeroAddress holderB 200 31 0
          recomputeVaultHolderBalance conn deploymentA seniorVault holderA
          recomputeVaultHolderBalance conn deploymentB seniorVault holderB

          stateA <- getVaultActivityIndexerState conn deploymentA
          stateB <- getVaultActivityIndexerState conn deploymentB
          fmap (const True) stateA `shouldBe` Just True
          fmap (const True) stateB `shouldBe` Just True
          resetVaultActivityDeployment conn deploymentA
          getVaultActivityIndexerState conn deploymentA `shouldReturnValue` Nothing
          getVaultHolders conn deploymentA seniorVault 10 `shouldReturnValue` []
          getVaultHolders conn deploymentB seniorVault 10
            `shouldReturnValue` [VaultHolderRow holderB 200]

    it "serves a completed snapshot as explicitly stale while the index catches up" $
      withVaultDatabase databaseUrl $ \pool -> do
        withDb pool $ \conn -> do
          setState conn deploymentA 20
          setVaultActivityIndexerState
            conn deploymentA 20 (Just $ blockHash 20) (1_700_000_020 :: Integer)
            30 (blockHash 30) (1_700_000_030 :: Integer) False
        published <- getVaultActivity pool deploymentA
        fmap (vacStale . varCoverage . respData) published `shouldBe` Just True

withVaultDatabase :: Text -> (DbPool -> IO a) -> IO a
withVaultDatabase databaseUrl action = do
  pool <- newDbPool databaseUrl
  let cleanup = do
        withDb pool $ \conn -> do
          resetVaultActivityDeployment conn deploymentA
          resetVaultActivityDeployment conn deploymentB
        destroyAllResources pool
  (do
      withDb pool ensureVaultActivitySchema
      withDb pool $ \conn -> do
        resetVaultActivityDeployment conn deploymentA
        resetVaultActivityDeployment conn deploymentB
      action pool
    ) `finally` cleanup

insertTransfer
  :: Connection
  -> VaultActivityDeployment
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Integer
  -> Integer
  -> IO ()
insertTransfer conn targetDeployment vault fromAddress toAddress amount blockNumber logIndex = do
    insertVaultLogIdentityExact
      conn targetDeployment vault "Transfer" (txHash blockNumber) blockNumber
      (blockHash blockNumber) 0 logIndex (1_700_000_000 + blockNumber)
    insertVaultShareTransferExact
      conn targetDeployment vault fromAddress toAddress amount
      (txHash blockNumber) blockNumber (blockHash blockNumber) 0 logIndex
      (1_700_000_000 + blockNumber)

request :: Text -> Integer -> Integer -> Integer -> VaultRequestRow
request eventName requestId blockNumber logIndex =
  VaultRequestRow
    { vrrEventName = eventName
    , vrrVaultAddress = seniorVault
    , vrrController = holderA
    , vrrOwner = holderA
    , vrrRequestId = requestId
    , vrrRawAmount = requestId * 10
    , vrrTxHash = txHash blockNumber
    , vrrBlockNumber = blockNumber
    , vrrBlockHash = blockHash blockNumber
    , vrrTxIndex = 0
    , vrrLogIndex = logIndex
    , vrrTimestamp = 1_700_000_000 + blockNumber
    }

setState :: Connection -> VaultActivityDeployment -> Integer -> IO ()
setState conn targetDeployment blockNumber =
  setVaultActivityIndexerState
    conn targetDeployment blockNumber (Just $ blockHash blockNumber)
    (1_700_000_000 + blockNumber) blockNumber (blockHash blockNumber)
    (1_700_000_000 + blockNumber) True

shouldReturnValue :: (Eq a, Show a) => IO a -> a -> IO ()
shouldReturnValue action expected = action >>= (`shouldBe` expected)

txHash, blockHash :: Integer -> Text
txHash value = fixedHash 'a' value
blockHash value = fixedHash 'b' value

fixedHash :: Char -> Integer -> Text
fixedHash prefix value =
  let suffix = show value
   in "0x" <> replicateText (64 - length suffix) prefix <> T.pack suffix

replicateText :: Int -> Char -> Text
replicateText count character = T.pack $ replicate count character

deploymentA, deploymentB :: VaultActivityDeployment
deploymentA = mkDeployment 10
deploymentB = mkDeployment 11

mkDeployment :: Integer -> VaultActivityDeployment
mkDeployment startBlock =
  VaultActivityDeployment
    { vadChainId = 9_900_001
    , vadHousePool = housePool
    , vadSeniorVault = seniorVault
    , vadJuniorVault = juniorVault
    , vadDeploymentBlock = startBlock
    }

housePool, seniorVault, juniorVault, holderA, holderB, zeroAddress :: Text
housePool = "0x0000000000000000000000000000000000000100"
seniorVault = "0x0000000000000000000000000000000000000200"
juniorVault = "0x0000000000000000000000000000000000000300"
holderA = "0x0000000000000000000000000000000000000400"
holderB = "0x0000000000000000000000000000000000000500"
zeroAddress = "0x0000000000000000000000000000000000000000"
