{-# OPTIONS_GHC -fno-warn-orphans #-}
module BluetoothSpec (spec) where

import Bluetooth
import Test.Hspec
import DBus 
import Control.Concurrent (threadDelay)

spec :: Spec
spec = do
  registerServiceSpec

registerServiceSpec :: Spec
registerServiceSpec = describe "registerService" $ beforeAll connect $ do
  
  it "registers the service with bluez" $ \conn -> do
    v <- runBluetoothM (registerService testService) conn
    threadDelay 10000000
    v `shouldBe` Right ()


-- * Test service

testService :: Service
testService = Service
  { serviceName = "test"
  , serviceUUID = "351930f8-7d31-43c1-92f5-fd2f0eac272f"
  , serviceCharacteristics = []
  }

-- * Orphans

instance Eq MethodError where
  a == b = show a == show b
