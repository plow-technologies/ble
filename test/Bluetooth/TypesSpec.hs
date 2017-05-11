{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bluetooth.TypesSpec (spec) where

import Data.Maybe                (fromJust)
import Data.Proxy                (Proxy (Proxy))
import Data.Tuple                (swap)
import Data.Void                 (Void)
import DBus
import Lens.Micro
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import qualified Data.Text as T

import Bluetooth.Internal.Types
import Bluetooth.Internal.Lenses

spec :: Spec
spec = do
  uuidSpec
  parentPathSpec
  chrPropPairsSpec

uuidSpec :: Spec
uuidSpec = describe "UUID" $ do

  context "fromString" $ do

    it "works for full UUIDs" $ do
      show ("636e8de5-c57b-4069-8c59-bb72e1ad805e" :: UUID)
        `shouldBe` "UUID 636e8de5-c57b-4069-8c59-bb72e1ad805e"

    it "works for 32-bit UUIDs" $ do
      show ("636e8de5" :: UUID)
        `shouldBe` "UUID 636e8de5-0000-1000-8000-00805f9b34fb"

    it "throws an error for invalid UUIDs" $ do
      print ("boo" :: UUID) `shouldThrow` anyException

  context "Representable" $ do
    fromRepToRepInverse (Proxy :: Proxy UUID)

parentPathSpec :: Spec
parentPathSpec = describe "parentPath" $ do

  it "returns the path of the parent" $ do
    parentPath "/com/turingjump/service01/char10"
      `shouldBe` "/com/turingjump/service01"

chrPropPairsSpec :: Spec
chrPropPairsSpec = describe "chrPropPairs" $ do

  it "contains all constructors of CharacteristicProperty" $ do
    all (`elem` (fst <$> chrPropPairs)) [minBound..maxBound] `shouldBe` True

  it "is one-to-one" $ do
    let there = [ fromJust $ lookup d chrPropPairs  | d <- fst <$> chrPropPairs ]
    let back = [ fromJust $ lookup d (swap <$> chrPropPairs)   | d <- there ]
    back `shouldBe` fst <$> chrPropPairs


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

instance Arbitrary a => Arbitrary (WithObjectPath a) where
  arbitrary = WOP <$> arbitrary <*> arbitrary

instance Arbitrary Application where
  arbitrary = Application <$> arbitrary <*> arbitrary

instance Arbitrary ObjectPath where
  arbitrary = objectPath . T.pack <$> arbitrary

instance Arbitrary (Service 'Local) where
  arbitrary = Service <$> arbitrary <*> arbitrary

instance {-# OVERLAPPABLE #-} (CoArbitrary a, Arbitrary a)
  => Arbitrary (Characteristic 'Local a) where
  arbitrary = LocalChar
    <$> arbitrary
    <*> arbitrary
    <*> (fmap return <$> arbitrary)
    <*> (fmap (fmap return) <$> arbitrary)

instance Arbitrary CharacteristicProperty where
  arbitrary = elements [minBound..maxBound]

instance Eq (Characteristic m Void) where
  a == b
    = a ^. uuid       == b ^. uuid
   && a ^. properties == b ^. properties

instance Show (Characteristic m Void) where
  show a = "Characteristic { "
        ++ "characteristicUuid = " ++ show (a ^. uuid) ++ ", "
        ++ "characteristicProperties = " ++ show (a ^. properties) ++ " }"

instance {-# OVERLAPPING #-} Arbitrary (Characteristic 'Local Void) where
  arbitrary = LocalChar
    <$> arbitrary
    <*> arbitrary
    <*> pure Nothing
    <*> pure Nothing
