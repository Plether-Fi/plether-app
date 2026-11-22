module KernelOperationSpec (kernelOperationSpec) where

import Control.Monad (void)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Bits (shiftL)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, execute_, withTransaction)
import Numeric (showHex)
import Plether.AA.KernelReconciler (ensureKernelReconcilerSchema)
import Plether.Database.KernelOperation
import Plether.Database.TestnetFunding (ensureMarginFundingSchema, requireKernelMarginFundingSchema)
import Test.Hspec

kernelOperationSpec :: ((Connection -> IO ()) -> IO ()) -> Spec
kernelOperationSpec withFixture = describe "Kernel PostgreSQL operation lifecycle" $ do
  it "recovers exactly the stored ETH-paid operation without sponsorship state" $
    withKernel $ \conn -> do
      prepared <- preparation scope PayWithEth "first" 0
      sponsored <- preparation scope Sponsored "first" 0
      let digest = hashOf 'a'
          signed = signWith "0x1111" $ kpUnsignedOperation prepared
          otherSignature = signWith "0x2222" $ kpUnsignedOperation prepared
      storeKernelPreparation conn sponsored {kpMode = PayWithEth} `shouldReturn` PreparationConflict
      storeKernelPreparation conn prepared {kpUnsignedOperation = object ["paymasterSignature" .= Null]}
        `shouldReturn` PreparationConflict
      storeKernelPreparation conn prepared `shouldReturn` PreparationStored
      storeKernelPreparation conn prepared `shouldReturn` PreparationAlreadyStored
      storeKernelPreparation conn prepared {kpSafeBlock = Just 110} `shouldReturn` PreparationAlreadyStored
      fmap (fmap kpSafeBlock) (getKernelPreparationForClient conn scope (kpId prepared) client)
        `shouldReturn` Just (Just 10)
      storeKernelPreparation conn prepared {kpOwner = addressOf '8'} `shouldReturn` PreparationConflict
      acceptKernelOperation conn scope (kpId prepared) "wrong-client" digest signed
        `shouldReturn` OperationPreparationMissing
      acceptKernelOperation conn scope (kpId prepared) client digest signed `shouldReturn` OperationAccepted
      acceptKernelOperation conn scope (kpId prepared) client digest otherSignature `shouldReturn` OperationConflict
      acceptKernelCancellationAnchor conn scope (kpId prepared) client digest signed `shouldReturn` OperationReplay
      fmap (fmap koCancellationOnly) (getKernelOperationForClient conn scope digest client)
        `shouldReturn` Just False
      acceptKernelOperation conn scope (kpId prepared) client digest signed `shouldReturn` OperationReplay
      fmap (fmap koSignedOperation) (getKernelOperationForClient conn scope digest client)
        `shouldReturn` Just signed
      fmap (fmap koState) (getKernelOperationForClient conn scope digest "wrong-client") `shouldReturn` Nothing
      fmap (fmap koState) (getKernelOperationForClient conn scope {ksEntryPoint = addressOf 'd'} digest client)
        `shouldReturn` Nothing
      kernelSponsorshipStateIsEmpty conn scope paymaster `shouldReturn` True

  it "preserves unanchored preparations across a schema upgrade and retries" $
    withKernel $ \conn -> do
      prepared <- preparation scope PayWithEth "pre-upgrade" 0
      let digest = hashOf 'a'
          signed = signWith "0x1111" $ kpUnsignedOperation prepared
      storeKernelPreparation conn prepared `shouldReturn` PreparationStored
      acceptKernelOperation conn scope (kpId prepared) client digest signed `shouldReturn` OperationAccepted
      void $ execute_ conn
        "ALTER TABLE aa_kernel_preparations DROP COLUMN nonce_scan_through_hash,\
        \ DROP COLUMN nonce_scan_through_block, DROP COLUMN prepared_safe_block"
      ensureKernelOperationSchema conn
      storeKernelPreparation conn prepared `shouldReturn` PreparationAlreadyStored
      fmap (fmap (kpSafeBlock . koPreparation)) (getKernelOperationForClient conn scope digest client)
        `shouldReturn` Just Nothing
      storeKernelNonceScanCursor conn prepared Nothing (12, hashOf 'd') `shouldReturn` False

  it "rejects stale recovery cursor updates and permits a canonical rewind" $
    withKernel $ \conn -> do
      prepared <- preparation scope PayWithEth "scan-progress" 0
      let first = (20, hashOf 'd')
          next = (30, hashOf 'e')
          rewind = (15, hashOf 'c')
          otherClient = prepared {kpClientKey = "different-client"}
      storeKernelPreparation conn prepared `shouldReturn` PreparationStored
      getKernelNonceScanCursor conn prepared `shouldReturn` Nothing
      storeKernelNonceScanCursor conn prepared Nothing (9, hashOf 'd') `shouldReturn` False
      storeKernelNonceScanCursor conn otherClient Nothing first `shouldReturn` False
      storeKernelNonceScanCursor conn prepared Nothing first `shouldReturn` True
      storeKernelNonceScanCursor conn prepared Nothing next `shouldReturn` False
      storeKernelNonceScanCursor conn prepared (Just (20, hashOf 'c')) next `shouldReturn` False
      getKernelNonceScanCursor conn otherClient `shouldReturn` Nothing
      getKernelNonceScanCursor conn prepared `shouldReturn` Just first
      storeKernelNonceScanCursor conn prepared (Just first) next `shouldReturn` True
      storeKernelNonceScanCursor conn prepared (Just next) rewind `shouldReturn` True
      getKernelNonceScanCursor conn prepared `shouldReturn` Just rewind

  it "fences accounts across modes and keeps accepted ETH operations recoverable after quote expiry" $
    withKernel $ \conn -> do
      now <- floor <$> getPOSIXTime
      seedSponsor conn scope now
      eth <- preparation scope PayWithEth "eth-1" 0
      sponsored <- preparation scope Sponsored "sponsored-1" 1
      let ethHash = hashOf 'a'
          sponsorHash = hashOf 'b'
          ethSigned = signWith "0x1111" $ kpUnsignedOperation eth
          sponsoredSigned = signWith "0x2222" $ kpUnsignedOperation sponsored
          nonceEvidence = KernelNonceConsumed sender 0 1 11 (hashOf 'e')
      storeKernelPreparation conn eth `shouldReturn` PreparationStored
      storeKernelPreparation conn sponsored `shouldReturn` PreparationStored
      authorize conn sponsored sponsorHash now
      acceptKernelOperation conn scope (kpId eth) client ethHash ethSigned `shouldReturn` OperationAccepted
      acceptKernelOperation conn scope (kpId sponsored) client sponsorHash sponsoredSigned `shouldReturn` OperationLaneBusy
      void $ execute conn
        "UPDATE aa_kernel_preparations SET expires_at=clock_timestamp()-interval '1 second' WHERE preparation_id=?"
        (Only (kpId eth))
      acceptKernelOperation conn scope (kpId eth) client ethHash ethSigned `shouldReturn` OperationReplay
      retireKernelOperation conn scope ethHash nonceEvidence {krObservedNonce = 1 `shiftL` 64}
        `shouldReturn` False
      retireKernelOperation conn scope ethHash nonceEvidence `shouldReturn` True
      acceptKernelOperation conn scope (kpId eth) client ethHash ethSigned `shouldReturn` OperationAlreadyFinal
      acceptKernelOperation conn scope (kpId sponsored) client sponsorHash sponsoredSigned `shouldReturn` OperationAccepted

  it "allows an attested ETH nonce while the prior ETH operation is pending settlement" $
    withKernel $ \conn -> do
      first <- preparation scope PayWithEth "eth-first" 0
      next <- preparation scope PayWithEth "eth-next" 1
      competing <- preparation scope PayWithEth "eth-competing" 1
      sponsored <- preparation scope Sponsored "sponsored-next" 2
      let signed = signWith "0x1111" . kpUnsignedOperation
          acceptAt nonce prepared digest =
            acceptKernelEthOperationAtNonce conn scope nonce (kpId prepared) client digest (signed prepared)
      void $ execute_ conn
        "CREATE UNIQUE INDEX aa_kernel_pending_account ON aa_kernel_operations\
        \ (deployment,chain_id,entry_point,sender) WHERE status='pending'"
      mapM_ (\prepared -> storeKernelPreparation conn prepared `shouldReturn` PreparationStored)
        [first, next, competing, sponsored]
      acceptKernelOperation conn scope (kpId first) client (hashOf 'a') (signed first) `shouldReturn` OperationAccepted
      ensureKernelOperationSchema conn
      acceptKernelOperation conn scope (kpId next) client (hashOf 'b') (signed next) `shouldReturn` OperationLaneBusy
      acceptAt 0 next (hashOf 'b') `shouldReturn` OperationPreparationMismatch
      acceptAt 1 next (hashOf 'b') `shouldReturn` OperationAccepted
      acceptAt 1 competing (hashOf 'c') `shouldReturn` OperationLaneBusy
      acceptAt 2 sponsored (hashOf 'd') `shouldReturn` OperationPreparationMismatch
      status conn scope (hashOf 'a') `shouldReturn` Just "pending"
      status conn scope (hashOf 'b') `shouldReturn` Just "pending"

  it "anchors an ambiguous signed ETH operation after expiry and never allows its ordinary relay" $
    withKernel $ \conn -> do
      now <- getCurrentTime
      eth <- preparation scope PayWithEth "eth-ambiguous" 5
      competing <- preparation scope PayWithEth "eth-competing" 5
      sponsored <- preparation scope Sponsored "sponsored-anchor" 5
      let digest = hashOf 'a'
          otherDigest = hashOf 'b'
          cancellationHash = hashOf 'c'
          signed = signWith "0x1111" $ kpUnsignedOperation eth
          unsignedChange = case signed of
            Object op -> Object (KM.insert "callData" (String "0xaaaa") op)
            _ -> signed
          sponsoredSigned = signWith "0x2222" $ kpUnsignedOperation sponsored
          included = KernelInclusion sender 5 Nothing (hashOf 'd') (hashOf 'e') 20 False 40
      storeKernelPreparation conn eth {kpExpiresAt = addUTCTime (-1) now} `shouldReturn` PreparationStored
      storeKernelPreparation conn competing `shouldReturn` PreparationStored
      storeKernelPreparation conn sponsored `shouldReturn` PreparationStored
      acceptKernelOperation conn scope (kpId eth) client digest signed `shouldReturn` OperationPreparationExpired
      acceptKernelCancellationAnchor conn scope (kpId eth) "wrong-client" digest signed
        `shouldReturn` OperationPreparationMissing
      acceptKernelCancellationAnchor conn scope (kpId eth) client digest unsignedChange
        `shouldReturn` OperationPreparationMismatch
      acceptKernelCancellationAnchor conn scope (kpId sponsored) client otherDigest sponsoredSigned
        `shouldReturn` OperationPreparationMismatch
      acceptKernelCancellationAnchor conn scope (kpId eth) client digest signed `shouldReturn` OperationAccepted
      acceptKernelCancellationAnchor conn scope (kpId eth) client digest signed `shouldReturn` OperationReplay
      Just anchor <- getKernelOperationForClient conn scope digest client
      koCancellationOnly anchor `shouldBe` True
      koSignedOperation anchor `shouldBe` signed
      acceptKernelOperation conn scope (kpId eth) client digest signed `shouldReturn` OperationConflict
      acceptKernelOperation conn scope (kpId competing) client otherDigest signed `shouldReturn` OperationLaneBusy
      acceptKernelCancellationAnchor conn scope (kpId competing) client otherDigest signed
        `shouldReturn` OperationLaneBusy
      let quote = cancellationQuote eth "cancel-anchor"
          signedCancellation = signWith "0x3333" $ kcUnsignedOperation quote
      storeKernelCancellation conn anchor quote `shouldReturn` PreparationStored
      acceptKernelCancellation conn anchor (kcPreparationId quote) cancellationHash signedCancellation
        `shouldReturn` OperationAccepted
      finalizeKernelCancellation conn anchor cancellationHash included `shouldReturn` True
      acceptKernelOperation conn scope (kpId eth) client digest signed `shouldReturn` OperationConflict

  it "releases an ETH cancellation anchor after verified same-lane nonce consumption" $
    withKernel $ \conn -> do
      eth <- preparation scope PayWithEth "cancellation-anchor" 5
      successor <- preparation scope PayWithEth "new-nonce" 6
      let digest = hashOf 'a'
          signed = signWith "0x1111" $ kpUnsignedOperation eth
          nextSigned = signWith "0x2222" $ kpUnsignedOperation successor
          evidence = KernelNonceConsumed sender 5 6 20 (hashOf 'e')
      storeKernelPreparation conn eth `shouldReturn` PreparationStored
      storeKernelPreparation conn successor `shouldReturn` PreparationStored
      acceptKernelCancellationAnchor conn scope (kpId eth) client digest signed `shouldReturn` OperationAccepted
      acceptKernelOperation conn scope (kpId successor) client (hashOf 'b') nextSigned `shouldReturn` OperationLaneBusy
      retireKernelOperation conn scope digest evidence {krObservedNonce = 1 `shiftL` 64} `shouldReturn` False
      retireKernelOperation conn scope digest evidence `shouldReturn` True
      status conn scope digest `shouldReturn` Just "nonce_consumed"
      acceptKernelOperation conn scope (kpId successor) client (hashOf 'b') nextSigned `shouldReturn` OperationAccepted

  it "journals one exact ETH cancellation and keeps the account fenced until canonical inclusion" $
    withKernel $ \conn -> do
      prepared <- preparation scope PayWithEth "eth-original" 5
      subsequent <- preparation scope PayWithEth "eth-subsequent" 6
      let originalHash = hashOf 'a'
          cancellationHash = hashOf 'b'
          nextHash = hashOf 'c'
          signed = signWith "0x1111" $ kpUnsignedOperation prepared
          nextSigned = signWith "0x2222" $ kpUnsignedOperation subsequent
          quote = cancellationQuote prepared "cancel-1"
          alternateQuote = quote {kcPreparationId = "cancel-2"}
          cancellation = signWith "0x3333" $ kcUnsignedOperation quote
          included = KernelInclusion sender 5 Nothing (hashOf 'd') (hashOf 'e') 19 False 40
      storeKernelPreparation conn prepared `shouldReturn` PreparationStored
      storeKernelPreparation conn subsequent `shouldReturn` PreparationStored
      acceptKernelOperation conn scope (kpId prepared) client originalHash signed `shouldReturn` OperationAccepted
      Just original <- getKernelOperationForClient conn scope originalHash client
      storeKernelCancellation conn original quote {kcPreparationId = kpId prepared} `shouldReturn` PreparationConflict
      storeKernelCancellation conn original quote {kcUnsignedOperation = object ["sender" .= sender, "nonce" .= ("0x6" :: Text)]}
        `shouldReturn` PreparationConflict
      storeKernelCancellation conn original quote {kcUnsignedOperation =
        object ["sender" .= sender, "nonce" .= ("0x5" :: Text), "paymasterSignature" .= Null]}
        `shouldReturn` PreparationConflict
      storeKernelCancellation conn original quote `shouldReturn` PreparationStored
      storeKernelCancellation conn original quote `shouldReturn` PreparationAlreadyStored
      storeKernelCancellation conn original alternateQuote `shouldReturn` PreparationStored
      let wrongClient = original {koPreparation = (koPreparation original) {kpClientKey = "wrong-client"}}
          wrongDeployment = original {koPreparation = (koPreparation original) {kpScope = scope {ksDeployment = "other"}}}
      fmap (fmap kcPreparationId) (getKernelCancellation conn wrongClient (kcPreparationId quote))
        `shouldReturn` Nothing
      fmap (fmap kcPreparationId) (getKernelCancellation conn wrongDeployment (kcPreparationId quote))
        `shouldReturn` Nothing
      acceptKernelCancellation conn wrongClient (kcPreparationId quote) cancellationHash cancellation
        `shouldReturn` OperationPreparationMissing
      acceptKernelCancellation conn original (kcPreparationId quote) cancellationHash signed
        `shouldReturn` OperationPreparationMismatch
      finalizeKernelCancellation conn original cancellationHash included `shouldReturn` False
      acceptKernelOperation conn scope (kpId prepared) client originalHash signed `shouldReturn` OperationReplay
      acceptKernelCancellation conn original (kcPreparationId quote) cancellationHash cancellation
        `shouldReturn` OperationAccepted
      acceptKernelCancellation conn original (kcPreparationId alternateQuote) (hashOf 'f') cancellation
        `shouldReturn` OperationConflict
      acceptKernelCancellation conn original (kcPreparationId quote) cancellationHash
        (signWith "0x4444" $ kcUnsignedOperation quote) `shouldReturn` OperationConflict
      expireCancellationQuote conn quote
      acceptKernelCancellation conn original (kcPreparationId quote) cancellationHash cancellation
        `shouldReturn` OperationReplay
      fmap (fmap kcSignedOperation) (getAcceptedKernelCancellation conn original)
        `shouldReturn` Just (Just cancellation)
      status conn scope originalHash `shouldReturn` Just "pending"
      acceptKernelOperation conn scope (kpId prepared) client originalHash signed `shouldReturn` OperationConflict
      acceptKernelOperation conn scope (kpId subsequent) client nextHash nextSigned `shouldReturn` OperationLaneBusy
      finalizeKernelCancellation conn original cancellationHash included {kiNonce = 6} `shouldReturn` False
      finalizeKernelCancellation conn original cancellationHash included {kiPaymaster = Just paymaster} `shouldReturn` False
      finalizeKernelCancellation conn original cancellationHash included `shouldReturn` True
      finalizeKernelCancellation conn original cancellationHash included `shouldReturn` True
      finalizeKernelCancellation conn original cancellationHash included {kiBlockHash = hashOf 'f'} `shouldReturn` False
      finalizeKernelOperation conn scope originalHash included `shouldReturn` False
      Just terminal <- getKernelOperationForClient conn scope originalHash client
      koState terminal `shouldBe` "nonce_consumed"
      (do Object fields <- koTerminalEvidence terminal
          KM.lookup "cancellationHash" fields) `shouldBe` Just (String cancellationHash)
      acceptKernelOperation conn scope (kpId subsequent) client nextHash nextSigned `shouldReturn` OperationAccepted

  it "lets the canonical original win and rejects an expired unaccepted cancellation" $
    withKernel $ \conn -> do
      prepared <- preparation scope PayWithEth "eth-original" 5
      let originalHash = hashOf 'a'
          cancellationHash = hashOf 'b'
          signed = signWith "0x1111" $ kpUnsignedOperation prepared
          quote = cancellationQuote prepared "cancel-1"
          expiredQuote = quote {kcPreparationId = "cancel-old"}
          cancellation = signWith "0x3333" $ kcUnsignedOperation quote
          included = KernelInclusion sender 5 Nothing (hashOf 'd') (hashOf 'e') 19 True 40
      storeKernelPreparation conn prepared `shouldReturn` PreparationStored
      acceptKernelOperation conn scope (kpId prepared) client originalHash signed `shouldReturn` OperationAccepted
      Just original <- getKernelOperationForClient conn scope originalHash client
      storeKernelCancellation conn original expiredQuote `shouldReturn` PreparationStored
      expireCancellationQuote conn expiredQuote
      acceptKernelCancellation conn original (kcPreparationId expiredQuote) cancellationHash cancellation
        `shouldReturn` OperationPreparationExpired
      storeKernelCancellation conn original quote `shouldReturn` PreparationStored
      acceptKernelCancellation conn original (kcPreparationId quote) cancellationHash cancellation
        `shouldReturn` OperationAccepted
      finalizeKernelOperation conn scope originalHash included `shouldReturn` True
      finalizeKernelCancellation conn original cancellationHash included `shouldReturn` False
      status conn scope originalHash `shouldReturn` Just "included"
      acceptKernelCancellation conn original (kcPreparationId quote) cancellationHash cancellation
        `shouldReturn` OperationAlreadyFinal

  it "requires the authorized sponsorship hash and settles only matching canonical cost" $
    withKernel $ \conn -> do
      now <- floor <$> getPOSIXTime
      seedSponsor conn scope now
      prepared <- preparation scope Sponsored "sponsored-2" 3
      let digest = hashOf 'a'
          signed = signWith "0x1111" $ kpUnsignedOperation prepared
          included = KernelInclusion sender 3 (Just paymaster) (hashOf 'c') (hashOf 'd') 13 False 40
      storeKernelPreparation conn prepared `shouldReturn` PreparationStored
      acceptKernelOperation conn scope (kpId prepared) client digest signed `shouldReturn` OperationPreparationMismatch
      authorize conn prepared digest now
      acceptKernelOperation conn scope (kpId prepared) client (hashOf 'b') signed `shouldReturn` OperationPreparationMismatch
      acceptKernelOperation conn scope (kpId prepared) client digest signed `shouldReturn` OperationAccepted
      retireKernelOperation conn scope digest (KernelNonceConsumed sender 3 4 13 (hashOf 'd'))
        `shouldReturn` False
      withTransaction conn (do
        lockKernelSponsorScope conn scope paymaster
        settleKernelSponsorship conn scope paymaster digest included {kiActualGasCost = 101} `shouldReturn` False
        settleKernelSponsorship conn scope paymaster digest included `shouldReturn` True
        settleKernelSponsorship conn scope paymaster digest included `shouldReturn` True)
      status conn scope digest `shouldReturn` Just "included"
      fmap (fmap kspStatus) (getKernelSponsorshipForClient conn scope paymaster (kpId prepared) client)
        `shouldReturn` Just "settled"

  it "preserves a signed authorization if the operation's terminal receipt conflicts" $
    withKernel $ \conn -> do
      now <- floor <$> getPOSIXTime
      seedSponsor conn scope now
      prepared <- preparation scope Sponsored "terminal-conflict" 3
      let digest = hashOf 'a'
          signed = signWith "0x1111" $ kpUnsignedOperation prepared
          included = KernelInclusion sender 3 (Just paymaster) (hashOf 'c') (hashOf 'd') 13 True 40
          sponsorshipStatus = fmap (fmap kspStatus) $ getKernelSponsorshipForClient conn scope paymaster (kpId prepared) client
      storeKernelPreparation conn prepared `shouldReturn` PreparationStored
      authorize conn prepared digest now
      acceptKernelOperation conn scope (kpId prepared) client digest signed `shouldReturn` OperationAccepted
      finalizeKernelOperation conn scope digest included `shouldReturn` True
      withTransaction conn $ do
        lockKernelSponsorScope conn scope paymaster
        settleKernelSponsorship conn scope paymaster digest included {kiBlockHash = hashOf 'e'} `shouldReturn` False
      sponsorshipStatus `shouldReturn` Just "signed"
      withTransaction conn $ do
        lockKernelSponsorScope conn scope paymaster
        settleKernelSponsorship conn scope paymaster digest included `shouldReturn` True
      sponsorshipStatus `shouldReturn` Just "settled"

  it "does not create a pending lane for a sponsorship already settled by another relay" $
    withKernel $ \conn -> do
      now <- floor <$> getPOSIXTime
      seedSponsor conn scope now
      prepared <- preparation scope Sponsored "elsewhere" 3
      let digest = hashOf 'a'
          signed = signWith "0x1111" $ kpUnsignedOperation prepared
          included = KernelInclusion sender 3 (Just paymaster) (hashOf 'c') (hashOf 'd') 13 True 40
      storeKernelPreparation conn prepared `shouldReturn` PreparationStored
      authorize conn prepared digest now
      withTransaction conn $ do
        lockKernelSponsorScope conn scope paymaster
        settleKernelSponsorship conn scope paymaster digest included `shouldReturn` True
      acceptKernelOperation conn scope (kpId prepared) client digest signed `shouldReturn` OperationPreparationMismatch
      status conn scope digest `shouldReturn` Nothing

  it "expires only the selected sponsor scope after its inclusive validity bound" $
    withKernel $ \conn -> do
      now <- floor <$> getPOSIXTime
      let other = scope {ksDeployment = "other-release"}
      seedSponsor conn scope now
      seedSponsor conn other now
      prepared <- preparation scope Sponsored "sponsored-3" 4
      otherPrepared <- preparation other Sponsored "sponsored-3" 4
      let digest = hashOf 'a'
          signed = signWith "0x1111" $ kpUnsignedOperation prepared
      storeKernelPreparation conn prepared `shouldReturn` PreparationStored
      storeKernelPreparation conn otherPrepared `shouldReturn` PreparationStored
      authorize conn prepared digest now
      authorize conn otherPrepared digest now
      acceptKernelOperation conn scope (kpId prepared) client digest signed `shouldReturn` OperationAccepted
      acceptKernelOperation conn other (kpId otherPrepared) client digest signed `shouldReturn` OperationAccepted
      expireAt conn (now + 60)
      status conn scope digest `shouldReturn` Just "pending"
      expireAt conn (now + 61)
      status conn scope digest `shouldReturn` Just "sponsorship_expired"
      status conn other digest `shouldReturn` Just "pending"

  it "requires the exact additive funding allocation schema before the canary starts" $
    withFixture $ \conn -> do
      requireKernelMarginFundingSchema conn `shouldThrow` anyIOException
      void $ execute_ conn "CREATE TABLE testnet_faucet_claims (address TEXT)"
      ensureMarginFundingSchema conn
      requireKernelMarginFundingSchema conn
      void $ execute_ conn "DROP INDEX testnet_margin_funding_kernel_owner_program"
      void $ execute_ conn
        "CREATE UNIQUE INDEX testnet_margin_funding_kernel_owner_program\
        \ ON testnet_margin_funding(chain_id, kernel_program, lower(owner_address))\
        \ WHERE kernel_program IS NULL"
      requireKernelMarginFundingSchema conn `shouldThrow` anyIOException
 where
  withKernel action = withFixture $ \conn -> do
    ensureKernelOperationSchema conn
    ensureKernelReconcilerSchema conn
    action conn
  expireAt conn safeTimestamp = withTransaction conn $ do
    lockKernelSponsorScope conn scope paymaster
    expireKernelSponsorshipsThrough conn scope paymaster safeTimestamp

seedSponsor :: Connection -> KernelScope -> Integer -> IO ()
seedSponsor conn profile now = void $ execute conn
  "INSERT INTO aa_kernel_reconciler_state\
  \ (deployment,chain_id,entry_point,paymaster,safe_block,safe_block_hash,safe_timestamp,\
  \ entry_point_code_hash,paymaster_code_hash,start_block,start_block_hash,last_success_at)\
  \ VALUES (?,?,?,?,10,?,?,?,?,10,?,clock_timestamp())"
  (ksDeployment profile, ksChainId profile, ksEntryPoint profile, paymaster,
    hashOf 'a', now, hashOf 'b', hashOf 'c', hashOf 'd')

authorize :: Connection -> KernelPreparation -> Text -> Integer -> IO ()
authorize conn prepared digest now = do
  let draft = KernelSponsorshipDraft (kpScope prepared) paymaster (kpId prepared) (kpId prepared)
        client sender (kpNonce prepared) 100 (now - 1) (now + 60) (kpUnsignedOperation prepared)
      budget = KernelSponsorBudget 100 1000 1000 1000 1000 1000 1000
  reserved <- reserveKernelSponsorship conn budget draft
  either (expectationFailure . T.unpack) (\row -> kspStatus row `shouldBe` "reserved") reserved
  storeKernelSponsorshipSignature conn (kpScope prepared) paymaster (ksdId draft) client "0x3333" digest
    `shouldReturn` True

preparation :: KernelScope -> PaymentMode -> Text -> Integer -> IO KernelPreparation
preparation profile mode identifier nonce = do
  expiresAt <- addUTCTime 60 <$> getCurrentTime
  let gasFields = case mode of
        PayWithEth -> []
        Sponsored -> ["paymaster" .= paymaster, "paymasterData" .= ("0x1234" :: Text),
          "paymasterVerificationGasLimit" .= ("0x1" :: Text), "paymasterPostOpGasLimit" .= ("0x0" :: Text)]
      unsigned = object $ ["sender" .= sender, "nonce" .= show nonce] ++ gasFields
  pure $ KernelPreparation profile sender (addressOf '1') nonce identifier client mode unsigned expiresAt (Just 10)

signWith :: Text -> Value -> Value
signWith signature (Object op) = Object $ KM.insert "signature" (String signature) op
signWith _ value = value

cancellationQuote :: KernelPreparation -> Text -> KernelCancellation
cancellationQuote original identifier =
  KernelCancellation identifier unsigned (kpExpiresAt original) Nothing Nothing
 where
  unsigned = object
    [ "sender" .= kpSender original
    , "nonce" .= ("0x" <> T.pack (showHex (kpNonce original) ""))
    , "callData" .= ("0xaaaa" :: Text)
    ]

expireCancellationQuote :: Connection -> KernelCancellation -> IO ()
expireCancellationQuote conn quote = void $ execute conn
  "UPDATE aa_kernel_cancellations SET expires_at=clock_timestamp()-interval '1 second' WHERE cancellation_preparation_id=?"
  (Only (kcPreparationId quote))

status :: Connection -> KernelScope -> Text -> IO (Maybe Text)
status conn profile digest = fmap koState <$> getKernelOperationForClient conn profile digest client

scope :: KernelScope
scope = KernelScope "kernel-release" 421614 (addressOf '9')

client, sender, paymaster :: Text
client = "test-client"
sender = addressOf '2'
paymaster = addressOf '3'

addressOf, hashOf :: Char -> Text
addressOf c = "0x" <> T.replicate 40 (T.singleton c)
hashOf c = "0x" <> T.replicate 64 (T.singleton c)
