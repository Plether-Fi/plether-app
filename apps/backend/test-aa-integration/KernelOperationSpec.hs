module KernelOperationSpec (kernelOperationSpec) where

import Control.Monad (void)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Bits (shiftL)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, withTransaction)
import Numeric (showHex)
import Plether.AA.KernelReconciler (ensureKernelReconcilerSchema)
import Plether.Database.KernelOperation
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
      retireKernelOperation conn scope digest (KernelNonceConsumed sender 5 6 20 (hashOf 'e'))
        `shouldReturn` False
      let quote = cancellationQuote eth "cancel-anchor"
          signedCancellation = signWith "0x3333" $ kcUnsignedOperation quote
      storeKernelCancellation conn anchor quote `shouldReturn` PreparationStored
      acceptKernelCancellation conn anchor (kcPreparationId quote) cancellationHash signedCancellation
        `shouldReturn` OperationAccepted
      finalizeKernelCancellation conn anchor cancellationHash included `shouldReturn` True
      acceptKernelOperation conn scope (kpId eth) client digest signed `shouldReturn` OperationConflict

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
      withTransaction conn (do
        lockKernelSponsorScope conn scope paymaster
        settleKernelSponsorship conn scope paymaster digest included {kiActualGasCost = 101} `shouldReturn` False
        settleKernelSponsorship conn scope paymaster digest included `shouldReturn` True
        settleKernelSponsorship conn scope paymaster digest included `shouldReturn` True)
      status conn scope digest `shouldReturn` Just "included"
      fmap (fmap kspStatus) (getKernelSponsorshipForClient conn scope paymaster (kpId prepared) client)
        `shouldReturn` Just "settled"

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
  pure $ KernelPreparation profile sender (addressOf '1') nonce identifier client mode unsigned expiresAt

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
