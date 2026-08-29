module Plether.Insights.Registration.RotationSpec (spec) where

import Plether.Database.Insights.Registration (RegistrationKeyReferenceCounts (..))
import Plether.Insights.Registration.Rotation (registrationKeyReferenceTotal)
import Test.Hspec

spec :: Spec
spec =
  describe "registration key-retirement preflight" $ do
    it "counts every encrypted envelope class without exposing row data" $ do
      registrationKeyReferenceTotal
        RegistrationKeyReferenceCounts
          { rkrcEmail = 1
          , rkrcXUserId = 2
          , rkrcXAccess = 3
          , rkrcCsrf = 4
          , rkrcPkce = 5
          , rkrcWalletMessage = 6
          }
        `shouldBe` 21

    it "reports zero only when all six envelope classes are clear" $ do
      registrationKeyReferenceTotal
        RegistrationKeyReferenceCounts
          { rkrcEmail = 0
          , rkrcXUserId = 0
          , rkrcXAccess = 0
          , rkrcCsrf = 0
          , rkrcPkce = 0
          , rkrcWalletMessage = 0
          }
        `shouldBe` 0
