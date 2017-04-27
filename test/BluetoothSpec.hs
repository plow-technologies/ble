{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#ifndef Bluez
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
#endif
module BluetoothSpec (spec) where

import Bluetooth
import Bluetooth.Internal.Types (Error)
import Control.Monad
import Control.Monad.IO.Class
import Data.Either              (isRight)
import Data.Maybe               (fromJust)
import DBus
import Test.Hspec

import qualified Data.ByteString as BS


spec :: Spec
spec = do
#ifndef Bluez
  return ()
#else
  registerApplicationSpec
  unregisterApplicationSpec
  advertiseSpec
  unadvertiseSpec
  {-notifySpec-}
  getServiceSpec
#endif

registerApplicationSpec :: Spec
registerApplicationSpec = describe "registerApplication" $ before connect $ do

  it "registers the service with bluez" $ \conn -> do
    v <- runBluetoothM (registerApplication testApp) conn
    v `shouldSatisfy` isRight
    -- We verify that the application is in fact registered by checking that
    -- attempting to register it again throws an AlreadyExists error.
    Left err <- runBluetoothM (registerApplication testApp) conn
    show err `shouldContain` "Already Exists"

unregisterApplicationSpec :: Spec
unregisterApplicationSpec = describe "unregisterApplication" $ before connect $ do

  it "unregisters the service" $ \conn -> do
    Right p <- runBluetoothM (registerApplication testApp) conn
    v1 <- runBluetoothM (unregisterApplication p) conn
    v1 `shouldSatisfy` isRight
    v2 <- runBluetoothM (registerApplication testApp) conn
    v2 `shouldSatisfy` isRight

advertiseSpec :: Spec
advertiseSpec = describe "advertise" $ before connect $ do

  let checkAdvert ad conn = do
        v <- runBluetoothM (advertise ad) conn
        v `shouldSatisfy` isRight
        -- We verify that the advertisement was registered by checking that
        -- attempting to register it again throws an AlreadyExists error.
        Left err <- runBluetoothM (advertise ad) conn
        _ <- runBluetoothM (unadvertise ad) conn
        show err `shouldContain` "Already Exists"

  it "adverstises a set of services" $ \conn -> checkAdvert testAdv conn

  {-it "works with service data" $ \conn -> do-}
    {-let adv = testAdv-}
         {-& value . serviceData . at "351930f8"-}
         {-?~ "hi"-}
    {-checkAdvert adv conn-}

  it "works with manufacturer data" $ \conn -> do
    let adv = testAdv & value . manufacturerData . at 1 ?~ "hi"
    checkAdvert adv conn

unadvertiseSpec :: Spec
unadvertiseSpec = describe "unadvertise" $ before connect $ do

  it "unregisters an advertisement" $ \conn -> do
    v1 <- runBluetoothM (advertise testAdv) conn
    v1 `shouldSatisfy` isRight
    Left err <- runBluetoothM (advertise testAdv) conn
    show err `shouldContain` "Already Exists"
    v2 <- runBluetoothM (unadvertise testAdv) conn
    v2 `shouldSatisfy` isRight
    v3 <- runBluetoothM (advertise testAdv) conn
    v3 `shouldSatisfy` isRight

{-notifySpec :: Spec-}
{-notifySpec = describe "notification" $ before connect $ do-}

  {-it "accepts StartNotify" $ \conn -> do-}
    {-readIORef isNotifying `shouldReturn` False-}
    {-v <- runBluetoothM (registerApplication testApp) conn-}
    {-_ <- runBluetoothM (testCharacteristic ^. startNotify) conn-}
    {-readIORef isNotifying `shouldReturn` True-}

  {-it "accepts StopNotify" $ \conn -> do-}
    {-readIORef isNotifying `shouldReturn` False-}
    {-v <- runBluetoothM (registerApplication testApp) conn-}
    {-_ <- runBluetoothM (testCharacteristic ^. startNotify) conn-}
    {-readIORef isNotifying `shouldReturn` True-}
    {-_ <- runBluetoothM (testCharacteristic ^. stopNotify) conn-}
    {-readIORef isNotifying `shouldReturn` False-}

getServiceSpec :: Spec
getServiceSpec = describe "getService" $ before connect $ do

  it "retrieves services by UUID" $ \conn -> do
    Right app <- runBluetoothM (registerApplication testApp) conn
    Right (Just service) <- runBluetoothM (getService (testService ^. uuid)) conn
    let [res] = [ c | c <- service ^. characteristics
                    , c ^. uuid == testCharacteristic ^. uuid
                ]
    h <- runHandler $ res ^. readValue . to fromJust
    void $ runBluetoothM (unregisterApplication app) conn
    h `shouldBe` Right ("response" :: BS.ByteString)

  {-it "fails if the application does not exist" $ \conn -> do-}
    {-let unknownUUID = "da92ce4a-2a0f-4c5d-ac68-9d3b01886976"-}
    {-h <- runBluetoothM (getService unknownUUID) conn-}
    {-h `shouldReturn` Nothing-}

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
      & properties .~ [CPRead, CPNotify]
  where
    go :: Handler BS.ByteString
    go = do
      liftIO $ putStrLn "Reading characteristic!"
      return "response"

testAdv :: WithObjectPath Advertisement
testAdv
  = advertisementFor testApp

-- * Orphans

instance Eq MethodError where
  a == b = show a == show b

instance Eq Error where
  a == b = show a == show b
