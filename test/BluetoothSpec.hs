{-# OPTIONS_GHC -fno-warn-orphans #-}
module BluetoothSpec (spec) where

import Bluetooth
import DBus
import Lens.Micro
import Test.Hspec

spec :: Spec
spec = do
  registerApplicationSpec

registerApplicationSpec :: Spec
registerApplicationSpec = describe "registerApplication" $ beforeAll connect $ do

  it "registers the service with bluez" $ \conn -> do
    v <- runBluetoothM (registerApplication testApp) conn
    v `shouldBe` Right ()


-- * Test service

testApp :: Application
testApp
  = "/com/turingjump/test"
      & services .~ [testService]

testService :: Service
testService
  = "351930f8-7d31-43c1-92f5-fd2f0eac272f"
      & characteristics .~ [testCharacteristic]

testCharacteristic :: Characteristic
testCharacteristic
  = "cdcb58aa-7e4c-4d22-b0bf-a90cd67ba60b"
      & readValue .~ encoded (return True)

-- * Orphans

instance Eq MethodError where
  a == b = show a == show b
