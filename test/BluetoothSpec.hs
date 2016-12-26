{-# OPTIONS_GHC -fno-warn-orphans #-}
module BluetoothSpec (spec) where

import Bluetooth
import DBus
import Lens.Micro
import Test.Hspec
import Control.Monad.IO.Class

import qualified Data.ByteString as BS
import Control.Concurrent

spec :: Spec
spec = do
  registerApplicationSpec
  advertiseSpec

registerApplicationSpec :: Spec
registerApplicationSpec = describe "registerApplication" $ beforeAll connect $ do

  it "registers the service with bluez" $ \conn -> do
    v <- runBluetoothM (registerApplication testApp) conn
    v `shouldBe` Right ()

advertiseSpec :: Spec
advertiseSpec = describe "advertise" $ beforeAll connect $ do

  it "adverstises a set of services" $ \conn -> do
    Right () <- runBluetoothM (registerApplication testApp) conn
    v <- runBluetoothM (advertise testAdv) conn
    threadDelay maxBound
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
      & readValue .~ Just (encoded go)
      & properties .~ [CPRead]
  where
    go :: MethodHandlerT IO BS.ByteString
    go = do
      liftIO $ putStrLn "Reading characteristic!"
      return "s"

testAdv :: WithObjectPath Advertisement
testAdv
  = advertisementFor testApp
     {-& value . serviceData .~ Map.fromList [( testCharacteristic ^. uuid-}
                                    {-, "b")]-}

-- * Orphans

instance Eq MethodError where
  a == b = show a == show b
