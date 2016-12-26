{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bluetooth.TypesSpec (spec) where

import Bluetooth.Types
import qualified Data.Text as T
import Data.Proxy (Proxy(Proxy))
import DBus
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

spec :: Spec
spec = do
  uuidSpec
  parentPathSpec

uuidSpec :: Spec
uuidSpec = describe "UUID" $ do

  context "fromString" $ do

    it "works for full UUIDs" $ do
      show ("636e8de5-c57b-4069-8c59-bb72e1ad805e" :: UUID)
        `shouldBe` "UnofficialUUID 636e8de5-c57b-4069-8c59-bb72e1ad805e"

    it "throws an error for invalid UUIDs" $ do
      print ("boo" :: UUID) `shouldThrow` anyException

  context "Representable" $ do
    fromRepToRepInverse (Proxy :: Proxy UUID)

parentPathSpec :: Spec
parentPathSpec = describe "parentPath" $ do

  it "returns the path of the parent" $ do
    parentPath "/com/turingjump/service01/char10"
      `shouldBe` "/com/turingjump/service01"

-- * Utils

fromRepToRepInverse
  :: forall proxy a. (Eq a, Arbitrary a, Representable a, Show a)
  => proxy a -> Spec
fromRepToRepInverse _
  = it "has fromRep as a left inverse of toRep" $ property $ \val -> do
      fromRep (toRep (val :: a)) `shouldBe` Just val

-- * Instances

instance Arbitrary UUID where
  arbitrary = choose ("00000000-0000-0000-0000-000000000000"
                     ,"FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF"
                     )

instance Arbitrary Application where
  arbitrary = Application <$> arbitrary <*> arbitrary

instance Arbitrary ObjectPath where
  arbitrary = objectPath . T.pack <$> arbitrary

instance Arbitrary Service where
  arbitrary = Service <$> arbitrary <*> arbitrary

instance (CoArbitrary a, Arbitrary b, Arbitrary c)
  => Arbitrary (Characteristic a b c) where
  arbitrary = Characteristic
    <$> arbitrary
    <*> arbitrary
    <*> (fmap return <$> arbitrary)
    <*> (fmap (fmap return) <$> arbitrary)
    <*> arbitrary

instance Arbitrary CharacteristicProperty where
  arbitrary = elements [minBound..maxBound]
