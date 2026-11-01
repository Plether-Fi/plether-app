module Plether.Database.AaSponsorshipSpec (spec) where

import Data.Char (isSpace, toLower)
import Data.List (isInfixOf)
import Test.Hspec

spec :: Spec
spec =
  describe "native AA schema source parity (startup behavior is exercised in BackendUnitRepairSpec)" $ do
    it "keeps the static bootstrap and runtime request-key migration aligned" $ do
      runtime <- normalized <$> readFile "src/Plether/Database/AaSponsorship.hs"
      static <- normalized <$> readFile "schema.sql"
      mapM_ (assertBoth runtime static)
        [ "addcolumnifnotexistsrequest_keyvarchar(66)"
        , "setrequest_key=digestwhererequest_keyisnull"
        , "altercolumnrequest_keysetnotnull"
        , "altercolumnexpected_user_operation_hashdropnotnull"
        , "dropconstraintifexistsaa_sponsorship_authorizations_request_key_key"
        , "dropindexifexistsidx_aa_sponsorship_request_key"
        , "createuniqueindexifnotexistsidx_aa_sponsorship_active_request_key"
        , "wherestatein('reserved','signed','submitted')"
        ]

    it "declares every named security invariant in both schema sources" $ do
      runtime <- normalized <$> readFile "src/Plether/Database/AaSponsorship.hs"
      static <- normalized <$> readFile "schema.sql"
      mapM_ (assertBoth runtime static)
        [ "aa_authorization_invariants_ck"
        , "aa_ledger_invariants_ck"
        , "aa_event_invariants_ck"
        , "aa_cursor_invariants_ck"
        , "aa_health_invariants_ck"
        , "aa_recovery_invariants_ck"
        , "aa_rate_invariants_ck"
        , "aa_control_singleton_ck"
        , "aa_sponsorship_control_reason_consistent"
        , "aa_control_event_invariants_ck"
        ]


 where
  normalized = map toLower . filter (\char -> not (isSpace char) && char /= '\\')
  assertBoth runtime static needle = do
    runtime `shouldSatisfy` isInfixOf needle
    static `shouldSatisfy` isInfixOf needle
