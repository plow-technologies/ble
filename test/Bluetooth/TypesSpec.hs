{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bluetooth.TypesSpec (spec) where

import Bluetooth.Types
import DBus
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  uuidSpec

uuidSpec :: Spec
uuidSpec = describe "UUID" $ do

  context "fromString" $ do

    it "works for full UUIDs" $ do
      show ("636e8de5-c57b-4069-8c59-bb72e1ad805e" :: UUID)
        `shouldBe` "UnofficialUUID 636e8de5-c57b-4069-8c59-bb72e1ad805e"

    it "throws an error for invalid UUIDs" $ do
      print ("boo" :: UUID) `shouldThrow` anyException

  context "Representable" $ do

    it "fromRep is a left inverse of toRep" $ property $ \u -> do
      fromRep (toRep (u :: UUID)) `shouldBe` Just u


-- * Instances

instance Arbitrary UUID where
  arbitrary = choose ("00000000-0000-0000-0000-000000000000"
                     ,"FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF"
                     )
