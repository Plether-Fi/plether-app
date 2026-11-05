module Plether.Handlers.TestnetMarginFunding
  ( claimMarginFunding, marginFundingStatus, startMarginFundingProcessor
  , processMarginFundingOnce, fundingCalldata, fundingReceiptMatches, validFundingId, verifySignedIntent, legacyTransactionMayUseSigner
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Control.Monad (forever, void, when)
import Data.Aeson (Value, object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isHexDigit)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base16 as B16
import Database.PostgreSQL.Simple (Connection, Only (..), query, query_)
import Plether.AA.Pimlico (resolveOwnedTradingAccountAtBlock)
import Plether.Config (Config (..))
import Plether.Database (DbPool)
import Plether.Database.Schema (TestnetFaucetClaimRow (..))
import Plether.Database.TestnetFunding
import Plether.Ethereum.Abi (encodeCall, encodeAddress, encodeUint256, keccak256)
import Plether.Ethereum.Client (EthClient, ethBlockNumber)
import Plether.Ethereum.Rpc
import Plether.Ethereum.Transaction
import qualified Plether.Handlers.TestnetFaucet as Legacy
import Plether.Logging (field, logInfo, logWarn)
import qualified Plether.Perps.Manifest as Manifest
import Plether.Types (ApiError, ApiResponse, mkResponse)
import qualified Plether.Types.Error as E
import Plether.Utils.Hex (hexToByteStringEither)

validFundingId :: Text -> Bool
validFundingId identifier =
  map T.length parts == [8,4,4,4,12] && all (T.all isHexDigit) parts
  where parts = T.splitOn "-" identifier

claimMarginFunding :: DbPool -> EthClient -> Config -> Text -> Text -> IO (Either ApiError (ApiResponse Value))
claimMarginFunding pool client cfg owner beneficiary
  | not (configurationMatches cfg) = pure $ Left $ E.forbidden "Direct margin funding is unavailable for this deployment."
  | otherwise = do
      block <- ethBlockNumber client
      canonical <- case block of
        Left _ -> pure $ Left ("Trading Account verification is unavailable." :: Text)
        Right number -> either (const $ Left ("Trading Account verification failed." :: Text)) Right
          <$> resolveOwnedTradingAccountAtBlock client owner number
      case canonical of
        Right address | T.toLower address == T.toLower beneficiary -> do
          signer <- maybe (pure $ Left "Faucet signer is unavailable.") deriveAddress $ cfgFaucetPrivateKey cfg
          case signer of
            Left _ -> pure $ Left $ E.networkError "The test funding service is unavailable."
            Right payer -> do
              result <- Legacy.withFaucetSignerLock pool $ \conn -> do
                -- Activation controls new reservations. Already reserved work
                -- and legacy allocations remain readable during rollback.
                existing <- query conn "SELECT status FROM testnet_faucet_claims WHERE address=? AND token_address=?"
                  (T.toLower beneficiary,T.toLower $ cfgPerpsSettlementToken cfg) :: IO [Only Text]
                if null existing && not (cfgTestnetMarginFundingEnabled cfg)
                  then pure $ Left $ E.networkError "Direct margin funding is temporarily unavailable."
                  else do
                    claim <- reserveMarginFunding conn $ FundingClaim "" (T.toLower owner) (T.toLower beneficiary)
                      (T.toLower $ cfgPerpsSettlementToken cfg) (cfgPerpsChainId cfg)
                      (T.toLower $ cfgPerpsMarginClearinghouse cfg) (T.toLower payer) Manifest.releaseVersion
                      Legacy.testnetFaucetAmount "mint" "pending" Nothing Nothing Nothing
                    pure $ Right $ case claim of
                      Just value -> fundingResponse value
                      Nothing -> mkResponse 0 (cfgPerpsChainId cfg) $ object
                        [ "version" .= (1 :: Int), "destination" .= ("wallet" :: Text)
                        , "status" .= ("legacy_wallet" :: Text), "address" .= T.toLower beneficiary
                        , "token" .= cfgPerpsSettlementToken cfg
                        ]
              pure $ either Left id result
        Left _ -> pure $ Left $ E.networkError "Trading Account verification is temporarily unavailable."
        Right _ -> pure $ Left $ E.forbidden "The beneficiary does not match the canonical Trading Account."

marginFundingStatus :: DbPool -> Config -> Text -> IO (Either ApiError (ApiResponse Value))
marginFundingStatus pool cfg identifier
  | not (validFundingId identifier) = pure $ Left $ E.invalidAddress "Invalid funding reference."
  | otherwise = do
      result <- Legacy.withFaucetDb pool $ \conn -> getMarginFunding conn identifier
      pure $ case result of
        Left failure -> Left failure
        Right (Just claim) | claimMatches cfg claim -> Right $ fundingResponse claim
        _ -> Left $ E.mkError E.NotFound "Funding reference was not found for this deployment."

fundingResponse :: FundingClaim -> ApiResponse Value
fundingResponse claim = mkResponse (maybe 0 id $ fcDepositBlock claim) (fcChain claim) $ object
  [ "version" .= (1 :: Int), "claimId" .= fcId claim, "destination" .= ("margin" :: Text)
  , "address" .= fcAddress claim, "token" .= fcToken claim, "clearinghouse" .= fcClearinghouse claim
  , "amount" .= show (fcAmount claim), "phase" .= fcPhase claim, "status" .= fcStatus claim
  , "reason" .= fcReason claim, "depositTxHash" .= fcDepositHash claim
  ]

configurationMatches :: Config -> Bool
configurationMatches cfg = Manifest.releaseIsDeployed && cfgPerpsChainId cfg == 421614
  && T.toLower (cfgPerpsSettlementToken cfg) == T.toLower Manifest.settlementTokenAddress
  && T.toLower (cfgPerpsMarginClearinghouse cfg) == T.toLower Manifest.marginClearinghouseAddress

claimMatches :: Config -> FundingClaim -> Bool
claimMatches cfg claim = configurationMatches cfg && fcChain claim == cfgPerpsChainId cfg
  && fcToken claim == T.toLower (cfgPerpsSettlementToken cfg)
  && fcClearinghouse claim == T.toLower (cfgPerpsMarginClearinghouse cfg)
  && fcRelease claim == Manifest.releaseVersion && fcAmount claim == Legacy.testnetFaucetAmount

-- The database is the queue. No request-owned thread or browser heartbeat is
-- needed to finish a reserved allocation. Turning issuance off does not stop it.
startMarginFundingProcessor :: DbPool -> EthClient -> Config -> IO ()
startMarginFundingProcessor pool client cfg = forever $ do
  result <- try @SomeException $ processMarginFundingOnce pool client cfg
  case result of
    Left err -> case fromException err :: Maybe SomeAsyncException of
      Just _ -> throwIO err
      Nothing -> logWarn "margin_funding_processor_failed" "Margin funding will resume from its durable journal." []
    Right () -> pure ()
  threadDelay 2_000_000

processMarginFundingOnce :: DbPool -> EthClient -> Config -> IO ()
processMarginFundingOnce pool client cfg = when (configurationMatches cfg && isJust (cfgFaucetPrivateKey cfg)) $ do
  result <- Legacy.withFaucetSignerLock pool $ \conn -> do
    -- Resume durable direct intent before considering another nonce. Historical
    -- legacy rows must not starve its receipt lookup or exact rebroadcast.
    submitted <- submittedFundingTransaction conn
    case submitted of
      Just tx -> do
        claim <- getMarginFunding conn $ ftClaim tx
        case claim of
          Just value | fcStatus value == "recovery" -> pure ()
          Just value | claimMatches cfg value -> reconcile conn value tx
          Just value -> failMarginFunding conn value "recovery" "DEPLOYMENT_CHANGED"
          Nothing -> fail "Funding journal has no allocation"
        pure Nothing
      Nothing -> do
        claim <- nextMarginFunding conn
        case claim of
          Nothing -> pure Nothing
          Just value | not (claimMatches cfg value) -> do
            failMarginFunding conn value "recovery" "DEPLOYMENT_CHANGED"
            pure Nothing
          Just value -> do
            payer <- maybe (pure $ Left "Missing faucet signer") deriveAddress $ cfgFaucetPrivateKey cfg
            case payer of
              Left _ -> failMarginFunding conn value "recovery" "SIGNER_CHANGED" >> pure Nothing
              Right signer -> do
                block <- ethBlockNumber client
                confirmed <- case block of
                  Left err -> pure $ Left err
                  Right number -> ethGetTransactionCountAtBlock client signer number
                case confirmed of
                  Left _ -> failMarginFunding conn value "pending" "RPC_UNAVAILABLE" >> pure Nothing
                  Right nonceFloor -> do
                    legacy <- legacySubmitted conn (cfgPerpsChainId cfg) signer nonceFloor
                    case legacy of
                      Just historical -> pure $ Just historical
                      Nothing -> prepare conn value nonceFloor >> pure Nothing
  case result of
    Left _ -> logWarn "margin_funding_database_unavailable" "Funding will resume from the durable journal." []
    Right Nothing -> pure ()
    -- The legacy resumer owns the same lock, so invoke it after releasing ours.
    Right (Just claim) -> void $ Legacy.resumeSubmittedClaim pool client cfg (tfcAddress claim) (tfcTokenAddress claim) claim
  where
    prepare conn claim nonceFloor = case cfgFaucetPrivateKey cfg of
      Nothing -> pure ()
      Just key -> do
        signer <- deriveAddress key
        if fmap T.toLower signer /= Right (fcSigner claim)
          then failMarginFunding conn claim "recovery" "SIGNER_CHANGED"
          else do
            prepared <- prepareTransaction conn client cfg key claim nonceFloor
            case prepared of
              Left reason -> failMarginFunding conn claim "failed" reason
              Right (unsigned,signed) -> do
                let (target,calldata) = fundingCalldata claim
                persistFundingTransaction conn claim (txNonce unsigned) target (encodeHex calldata)
                  (T.toLower $ signedTransactionHash signed) (encodeHex $ signedRawTransaction signed)
                logInfo "margin_funding_submitted" "A funding transaction was durably journaled."
                  [field "claim_id" $ fcId claim,field "stage" $ fcPhase claim]
                -- Commit precedes network submission. A timeout here leaves a
                -- unique submitted signer/nonce reservation for the next tick.
                broadcast conn claim (T.toLower $ signedTransactionHash signed) $ signedRawTransaction signed
    broadcast conn claim expected raw = do
      result <- ethSendRawTransaction client raw
      case result of
        Right actual | T.toLower actual /= expected ->
          failMarginFunding conn claim "recovery" "BROADCAST_HASH_MISMATCH"
        -- A transport error or already-known response leaves the same signed
        -- intent reserved. Receipt lookup and exact rebroadcast resume it.
        _ -> pure ()
    reconcile conn claim tx = do
      verified <- verifySignedIntent claim tx
      if not verified then failMarginFunding conn claim "recovery" "SIGNED_INTENT_MISMATCH"
      else do
        receipt <- ethGetTransactionReceipt client $ ftHash tx
        case receipt of
          Left _ -> failMarginFunding conn claim "pending" "RPC_UNAVAILABLE"
          Right Nothing -> case hexToByteStringEither $ ftRaw tx of
            Left _ -> failMarginFunding conn claim "recovery" "SIGNED_INTENT_MISMATCH"
            Right raw -> broadcast conn claim (ftHash tx) raw
          Right (Just value) -> do
            chain <- ethChainId client
            headBlock <- ethBlockNumber client
            canonical <- ethGetBlockByNumber client $ receiptBlockNumber value
            let enough = either (const False) (>= receiptBlockNumber value + fromIntegral (max 1 (cfgKeeperConfirmations cfg))) headBlock
                canonicalHash = fmap rpcBlockHash canonical
            case (chain,canonicalHash,headBlock) of
              (Right chainId,Right blockHash,Right _)
                | chainId /= fcChain claim || blockHash /= receiptBlockHash value ->
                    failMarginFunding conn claim "recovery" "RECEIPT_CHAIN_MISMATCH"
                | otherwise -> when enough $
                if T.toLower (receiptTxHash value) /= ftHash tx
                  || (receiptSucceeded value && not (fundingReceiptMatches claim (ftPhase tx) value))
                  then failMarginFunding conn claim "recovery" "RECEIPT_INTENT_MISMATCH"
                  else do
                    finishFundingTransaction conn claim tx (receiptSucceeded value)
                      (receiptBlockNumber value) (receiptBlockHash value)
                    logInfo "margin_funding_receipt" "A funding receipt was verified and recorded."
                      [field "claim_id" $ fcId claim,field "stage" $ ftPhase tx,field "succeeded" $ receiptSucceeded value]
              _ -> failMarginFunding conn claim "pending" "RPC_UNAVAILABLE"

-- Historical grants can have a different signer or chain. They retain their
-- original recovery path. Proven consumed nonces also cannot reserve the next
-- nonce; their claims are never reset or reissued. Unknown or
-- corrupt intent stays conservative rather than being silently discarded.
legacySubmitted :: Connection -> Integer -> Text -> Integer -> IO (Maybe TestnetFaucetClaimRow)
legacySubmitted conn chain signer nonceFloor = do
  rows <- query_ conn
    "SELECT address,amount,token_address,tx_hash,raw_tx,mint_block_number,status,error FROM testnet_faucet_claims WHERE status='submitted' ORDER BY updated_at,address"
  let findRelevant [] = pure Nothing
      findRelevant (value:rest) = do
        relevant <- legacyTransactionMayUseSigner chain signer nonceFloor value
        if relevant then pure $ Just value else findRelevant rest
  findRelevant rows

legacyTransactionMayUseSigner :: Integer -> Text -> Integer -> TestnetFaucetClaimRow -> IO Bool
legacyTransactionMayUseSigner chain signer nonceFloor claim = case (tfcRawTx claim,tfcTxHash claim) of
  (Just raw,Just expectedHash) -> case hexToByteStringEither raw of
    Right bytes | T.toLower (rawTransactionHash bytes) == T.toLower expectedHash -> do
      decoded <- decodeSignedTransaction bytes
      pure $ case decoded of
        Right (unsigned,payer) -> txChainId unsigned == chain && T.toLower payer == T.toLower signer && txNonce unsigned >= nonceFloor
        Left _ -> True
    _ -> pure True
  _ -> pure True

fundingCalldata :: FundingClaim -> (Text,ByteString)
fundingCalldata claim = case fcPhase claim of
  "mint" -> (fcToken claim, encodeCall "mint(address,uint256)" [encodeAddress $ fcSigner claim,encodeUint256 $ fcAmount claim])
  "approval" -> (fcToken claim, encodeCall "approve(address,uint256)" [encodeAddress $ fcClearinghouse claim,encodeUint256 $ fcAmount claim])
  "deposit" -> (fcClearinghouse claim, encodeCall "depositFor(address,uint256)" [encodeAddress $ fcAddress claim,encodeUint256 $ fcAmount claim])
  _ -> error "Invalid persisted funding phase"

prepareTransaction :: Connection -> EthClient -> Config -> Text -> FundingClaim -> Integer -> IO (Either Text (Tx1559,SignedTransaction))
prepareTransaction conn client cfg key claim nonceFloor = do
  let (target,calldata) = fundingCalldata claim
  chain <- ethChainId client
  tokenCode <- ethGetCode client $ fcToken claim
  clearinghouseCode <- ethGetCode client $ fcClearinghouse claim
  nonce <- ethGetTransactionCount client $ fcSigner claim
  floors <- query conn "SELECT COALESCE(MAX(nonce)+1,0)::bigint FROM testnet_margin_funding_transactions WHERE signer=?" (Only $ fcSigner claim) :: IO [Only Integer]
  gas <- ethEstimateGas client (fcSigner claim) target 0 calldata
  price <- ethGasPrice client
  priority <- ethMaxPriorityFeePerGas client
  case (chain,tokenCode,clearinghouseCode,nonce,gas,price) of
    (Right chainId,Right tokenBytes,Right clearingBytes,Right nextNonce,Right estimated,Right gasPrice)
      | chainId == fcChain claim
      , encodeHex (keccak256 tokenBytes) == Manifest.settlementTokenCodeHash
      , encodeHex (keccak256 clearingBytes) == Manifest.marginClearinghouseCodeHash
      , [Only floorNonce] <- floors
      , nextNonce >= max floorNonce nonceFloor -> do
          let tip = either (const gasPrice) id priority
              unsigned = Tx1559 chainId nextNonce (applyBpsBuffer tip $ cfgKeeperFeeBufferBps cfg)
                (applyBpsBuffer (max gasPrice tip) $ cfgKeeperFeeBufferBps cfg)
                (max 21_000 $ applyBpsBuffer estimated $ cfgKeeperGasBufferBps cfg) target 0 calldata
          signed <- signTransaction key unsigned
          pure $ either (const $ Left "SIGNING_FAILED") (Right . (unsigned,)) signed
    _ -> pure $ Left "PREPARATION_UNAVAILABLE"

verifySignedIntent :: FundingClaim -> FundingTransaction -> IO Bool
verifySignedIntent claim tx = case hexToByteStringEither $ ftRaw tx of
  Left _ -> pure False
  Right bytes -> do
    decoded <- decodeSignedTransaction bytes
    let (target,calldata) = fundingCalldata claim
    pure $ case decoded of
      Right (unsigned,signer) -> ftClaim tx == fcId claim && ftPhase tx == fcPhase claim
        && T.toLower signer == fcSigner claim && ftSigner tx == fcSigner claim
        && txChainId unsigned == fcChain claim && txNonce unsigned == ftNonce tx
        && T.toLower (txTo unsigned) == target && ftTarget tx == target
        && txValue unsigned == 0 && txData unsigned == calldata && ftCalldata tx == encodeHex calldata
        && T.toLower (rawTransactionHash bytes) == ftHash tx
      Left _ -> False

-- DepositFor is metadata for the same credit represented by Deposit. Require
-- both events, but record one allocation and one margin credit.
fundingReceiptMatches :: FundingClaim -> Text -> TxReceipt -> Bool
fundingReceiptMatches claim phase receipt = case phase of
  "mint" -> one (fcToken claim) "Transfer(address,address,uint256)"
    [BS.replicate 32 0,encodeAddress $ fcSigner claim]
  "approval" -> one (fcToken claim) "Approval(address,address,uint256)"
    [encodeAddress $ fcSigner claim,encodeAddress $ fcClearinghouse claim]
  "deposit" -> one (fcClearinghouse claim) "Deposit(address,address,uint256)"
      [encodeAddress $ fcAddress claim,encodeAddress $ fcToken claim]
    && one (fcClearinghouse claim) "DepositFor(address,address,uint256)"
      [encodeAddress $ fcSigner claim,encodeAddress $ fcAddress claim]
    && one (fcToken claim) "Transfer(address,address,uint256)"
      [encodeAddress $ fcSigner claim,encodeAddress $ fcClearinghouse claim]
  _ -> False
  where
    one target signature indexed = length (filter matches $ receiptLogs receipt) == 1
      where matches logEntry = T.toLower (rpcLogAddress logEntry) == target
              && rpcLogTopics logEntry == keccak256 (TE.encodeUtf8 signature) : indexed
              && rpcLogData logEntry == encodeUint256 (fcAmount claim)
              && T.toLower (rpcLogTxHash logEntry) == T.toLower (receiptTxHash receipt)
              && rpcLogBlockHash logEntry == receiptBlockHash receipt
              && rpcLogBlockNumber logEntry == receiptBlockNumber receipt
              && rpcLogTransactionIndex logEntry == receiptTransactionIndex receipt

encodeHex :: ByteString -> Text
encodeHex = ("0x" <>) . TE.decodeUtf8 . B16.encode
