{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#ifndef Bluez
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
#else
#ifndef DBus
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
#endif
#endif
module BluetoothSpec (spec) where

import Bluetooth
import Bluetooth.Internal.Types (Error)
import Control.Monad
import Control.Monad.IO.Class
import Data.Either              (isRight)
import Data.Maybe               (isJust)
import DBus
import Test.Hspec

import qualified Data.ByteString as BS

import Mock


spec :: Spec
spec = do
#ifdef Bluez
  registerApplicationSpec
  unregisterApplicationSpec
  advertiseSpec
  unadvertiseSpec
#endif
#ifdef DBusMock
  getServiceSpec
  getAllServicesSpec
  getAllDevicesSpec
#endif
  return ()

registerApplicationSpec :: Spec
registerApplicationSpec = describe "registerApplication" $ before connect $ do

  it "registers the service with bluez" $ \conn -> do
    v <- runBluetoothM (registerApplication testApp) conn
    v `shouldSatisfy` isRight
    -- We verify that the application is in fact registered by checking that
    -- attempting to register it again throws an AlreadyExists error.
    Left err <- runBluetoothM (registerApplication testApp) conn
    _ <- runBluetoothM (unregisterApplication $ fromRight v) conn
    show err `shouldContain` "Already Exists"

unregisterApplicationSpec :: Spec
unregisterApplicationSpec = describe "unregisterApplication" $ before connect $ do

  it "unregisters the service" $ \conn -> do
    Right p <- runBluetoothM (registerApplication testApp) conn
    v1 <- runBluetoothM (unregisterApplication p) conn
    v1 `shouldSatisfy` isRight
    v2 <- runBluetoothM (registerApplication testApp) conn
    void $ runBluetoothM (unregisterApplication $ fromRight v2) conn
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

  it "advertises a set of services" $ \conn -> checkAdvert testAdv conn

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

getServiceSpec :: Spec
getServiceSpec = describe "getService" $ before connect $ do

  it "retrieves services by UUID" $ \conn -> withAService $ do
    Right ms <- runBluetoothM (getService mockServiceUUID) conn
    ms `shouldSatisfy` isJust

  it "returns Nothing if the application does not exist" $ \conn -> withAService $ do
    let unknownUUID = "da92ce4a-2a0f-4c5d-ac68-9d3b01886976"
    h <- runBluetoothM (getService unknownUUID) conn
    h `shouldBe` Right Nothing

  context "the returned service" $ do

    it "has the service's characteristics" $ \conn -> withAService $ do
      Right (Just serv) <- runBluetoothM (getService mockServiceUUID) conn
      let [char] = serv ^. characteristics
      char ^. uuid `shouldBe` mockCharUUID

    it "has characteristics that can be read" $ \conn -> withAService $ do
      Right (Just serv) <- runBluetoothM (getService mockServiceUUID) conn
      let [char] = serv ^. characteristics
      let Just handler = char ^. readValue
      res <- runBluetoothM handler conn
      res `shouldBe` (Right $ BS.singleton 3)

    it "has characteristics that can be written" $ \conn -> withAService $ do
      Right (Just serv) <- runBluetoothM (getService mockServiceUUID) conn
      let [char] = serv ^. characteristics
      let Just whandler = char ^. writeValue
          Just rhandler = char ^. readValue
      writeResult <- runBluetoothM (whandler $ BS.singleton 10)  conn
      writeResult `shouldBe` Right True
      readResult <- runBluetoothM rhandler conn
      readResult `shouldBe` (Right $ BS.singleton 10)

getAllServicesSpec :: Spec
getAllServicesSpec = describe "getAllServices" $ before connect $ do

  it "retrieves services" $ \conn -> withAService $ do
    Right [service] <- runBluetoothM getAllServices conn
    service ^. uuid `shouldBe` mockServiceUUID

getAllDevicesSpec :: Spec
getAllDevicesSpec = describe "getAllDevices" $ before connect $ do

  it "retrieves devices" $ \conn -> withAService $ do
    Right devices <- runBluetoothM getAllDevices conn
    devices `shouldContain` ["11:22:33:44:55:66"]

  {-context "the returned device" $-}
    {-pendingWith "dbusmock doesn't currently allow changing properties"-}

    {-it "allows connecting" $ \conn -> withAService $ do-}
      {-resp1 <- runBluetoothM (isConnected "11:22:33:44:55:66") conn-}
      {-resp1 `shouldBe` Right False-}
      {-Right () <- runBluetoothM (connectTo "11:22:33:44:55:66") conn-}
      {-resp2 <- runBluetoothM (isConnected "11:22:33:44:55:66") conn-}
      {-resp2 `shouldBe` Right True-}

    {-it "allows pairing" $ \conn -> withAService $ do-}
      {-resp1 <- runBluetoothM (isPaired "11:22:33:44:55:66") conn-}
      {-resp1 `shouldBe` Right False-}
      {-Right () <- runBluetoothM (pairWith "11:22:33:44:55:66") conn-}
      {-resp2 <- runBluetoothM (isPaired "11:22:33:44:55:66") conn-}
      {-resp2 `shouldBe` Right True-}

    {-it "allows trusting" $ \conn -> withAService $ do-}
      {-resp1 <- runBluetoothM (isTrusted "11:22:33:44:55:66") conn-}
      {-resp1 `shouldBe` Right False-}
      {-Right () <- runBluetoothM (trust "11:22:33:44:55:66") conn-}
      {-resp2 <- runBluetoothM (isTrusted "11:22:33:44:55:66") conn-}
      {-resp2 `shouldBe` Right True-}
      {-Right () <- runBluetoothM (distrust "11:22:33:44:55:66") conn-}
      {-resp3 <- runBluetoothM (isTrusted "11:22:33:44:55:66") conn-}
      {-resp3 `shouldBe` Right True-}

-- * Test service

testApp :: Application
testApp
  = "/com/turingjump/test"
      & services .~ [testService]

testService :: Service 'Local
testService
  = "351930f8-7d31-43c1-92f5-fd2f0eac272f"
      & characteristics .~ [testCharacteristic]

testCharacteristic :: CharacteristicBS 'Local
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

-- * Utils

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Expected Right, got Left"

-- * Orphans

instance Eq MethodError where
  a == b = show a == show b

instance Eq Error where
  a == b = show a == show b

instance Show (Service m) where
  show a = show $ a ^. uuid

instance Eq (Service m) where
  a == b = a ^. uuid == b ^. uuid
