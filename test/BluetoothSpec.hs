{-# OPTIONS_GHC -fno-warn-orphans #-}
module BluetoothSpec (spec) where

import Bluetooth
import Test.Hspec
import DBus 

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
testApp = Application
  { applicationRoot = "/com/turingjump/test"
  , applicationServices = [testService]
  }

testService :: Service
testService = Service
  { serviceName = "test"
  , serviceUUID = "351930f8-7d31-43c1-92f5-fd2f0eac272f"
  , serviceCharacteristics = []
  }

-- * Orphans

instance Eq MethodError where
  a == b = show a == show b
