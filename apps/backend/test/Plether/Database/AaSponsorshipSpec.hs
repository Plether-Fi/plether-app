module Plether.Database.AaSponsorshipSpec (spec) where

import Data.Char (isSpace, toLower)
import Data.List (isInfixOf)
import Test.Hspec

spec :: Spec
spec =
  describe "native AA schema migration parity" $ do
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

    it "installs every named security invariant in both schema paths" $ do
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

    it "fails startup against drifted columns, keys, checks, indexes, or defaults" $ do
      runtime <- normalized <$> readFile "src/Plether/Database/AaSponsorship.hs"
      mapM_ (\needle -> runtime `shouldSatisfy` isInfixOf (normalized needle))
        [ "table durability/row-security fingerprint mismatch"
        , "schema column fingerprint mismatch"
        , "schema key/foreign-key fingerprint mismatch"
        , "schema check-constraint fingerprint mismatch"
        , "schema index fingerprint mismatch"
        , "schema default fingerprint mismatch"
        , "pg_get_constraintdef(con.oid,true)=pg_get_constraintdef(expected.oid,true)"
        , "pg_get_expr(actual.indpred,actual.indrelid,true)=pg_get_expr(expected.indpred,expected.indrelid,true)"
        , "key_index.indisuniqueandkey_index.indisvalidandkey_index.indisready"
        , "key_index.indnullsnotdistinct"
        , "attrelid='pg_catalog.pg_index'::regclass"
        , "attname='indnullsnotdistinct'andnotattisdropped"
        , "aaschemakeysquerysupportsnullsnotdistinct"
        , "aaschemakeysquerypg15"
        , "aaschemakeysquerypg14"
        , "\\false"
        , "con.confupdtype"
        , "con.confdeltype"
        , "con.confmatchtype"
        , "con.condeferrable,con.condeferred,con.convalidated"
        , "c.relpersistence"
        , "c.relrowsecurity,c.relforcerowsecurity"
        , "idx.indnkeyatts"
        , "idx.indnatts"
        , "idx.indexprsisnull"
        , "idx.indclass=expected.indclass"
        , "idx.indcollation=expected.indcollation"
        , "idx.indoption=expected.indoption"
        , "target.relnamespace=source.relnamespace"
        , "aa_sponsorship_ledger_id_seq"
        , "aa_sponsorship_control_events_id_seq"
        ]
 where
  normalized = map toLower . filter (\char -> not (isSpace char) && char /= '\\')
  assertBoth runtime static needle = do
    runtime `shouldSatisfy` isInfixOf needle
    static `shouldSatisfy` isInfixOf needle
