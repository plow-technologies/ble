{-# OPTIONS_GHC -fno-warn-orphans #-}
module BluetoothSpec (spec) where

import Bluetooth
import Control.Monad.IO.Class
import DBus
import Test.Hspec

import qualified Data.Text as T
import qualified Data.ByteString as BS

import Control.Concurrent

spec :: Spec
spec = do
  registerApplicationSpec
  advertiseSpec

registerApplicationSpec :: Spec
registerApplicationSpec = describe "registerApplication" $ before connect $ do

  it "registers the service with bluez" $ \conn -> do
    v <- runBluetoothM (registerApplication testApp) conn
    v `shouldBe` Right ()

advertiseSpec :: Spec
advertiseSpec = describe "advertise" $ before connect $ do

  it "adverstises a set of services" $ \conn -> do
    Right () <- runBluetoothM (registerApplication testApp) conn
    v <- runBluetoothM (advertise testAdv) conn
    v `shouldBe` Right ()

  it "works with service data" $ \conn -> do
    Right () <- runBluetoothM (registerApplication testApp) conn
    let adv = testAdv & value . serviceData . at "a" ?~ "hi"
    v <- runBluetoothM (advertise adv) conn
    {-threadDelay maxBound-}
    v `shouldBe` Right ()

  it "works with manufacturer data" $ \conn -> do
    Right () <- runBluetoothM (registerApplication testApp) conn
    let adv = testAdv & value . manufacturerData . at 1 ?~ "hi"
    v <- runBluetoothM (advertise adv) conn
    {-threadDelay maxBound-}
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

testCharacteristic :: CharacteristicBS
testCharacteristic
  = "cdcb58aa-7e4c-4d22-b0bf-a90cd67ba60b"
      & readValue ?~ encodeRead go
      & properties .~ [CPRead]
  where
    go :: ReadValue BS.ByteString
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
