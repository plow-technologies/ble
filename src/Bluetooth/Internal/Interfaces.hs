module Bluetooth.Internal.Interfaces where

import Data.IORef (IORef, newIORef)
import Data.Proxy
import DBus
import GHC.TypeLits
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text as T


type ObjectManager = "org.freedesktop.DBus.ObjectManager"

objectManagerIFaceP :: Proxy ObjectManager
objectManagerIFaceP = Proxy

objectManagerIFace :: String
objectManagerIFace = symbolVal objectManagerIFaceP


type Properties = "org.freedesktop.DBus.Properties"

propertiesIFaceP :: Proxy Properties
propertiesIFaceP = Proxy

propertiesIFace :: String
propertiesIFace = symbolVal propertiesIFaceP


type GattService = "org.bluez.GattService1"

gattServiceIFaceP :: Proxy GattService
gattServiceIFaceP = Proxy

gattServiceIFace :: String
gattServiceIFace = symbolVal gattServiceIFaceP

type GattCharacteristic = "org.bluez.GattCharacteristic1"

gattCharacteristicIFaceP :: Proxy GattCharacteristic
gattCharacteristicIFaceP = Proxy

gattCharacteristicIFace :: String
gattCharacteristicIFace = symbolVal gattCharacteristicIFaceP


type GattManager = "org.bluez.GattManager1"

gattManagerIFaceP :: Proxy GattManager
gattManagerIFaceP = Proxy

gattManagerIFace :: String
gattManagerIFace = symbolVal gattManagerIFaceP


type LEAdvertisement = "org.bluez.LEAdvertisement1"

leAdvertisementIFaceP :: Proxy LEAdvertisement
leAdvertisementIFaceP = Proxy

leAdvertisementIFace :: String
leAdvertisementIFace = symbolVal leAdvertisementIFaceP


type LEAdvertisingManager1 = "org.bluez.LEAdvertisingManager1"

leAdvertisingManagerIFaceP :: Proxy LEAdvertisingManager1
leAdvertisingManagerIFaceP = Proxy

leAdvertisingManagerIFace :: String
leAdvertisingManagerIFace = symbolVal leAdvertisingManagerIFaceP

type Device1 = "org.bluez.Device1"

deviceIFaceP :: Proxy Device1
deviceIFaceP = Proxy

deviceIFace :: String
deviceIFace = symbolVal deviceIFaceP

-- * Errors

invalidArgs :: MsgError
invalidArgs = MsgError
  { errorName = "org.freedesktop.DBus.Error.InvalidArgs"
  , errorText = Nothing
  , errorBody = []
  }

notSupported :: MsgError
notSupported = MsgError
  { errorName = "org.bluez.Error.NotSupported"
  , errorText = Nothing
  , errorBody = []
  }

-- * Constants (ish)
-- We use IORefs here rather than have it be a constant because when mocking
-- for tests we can't take up this name.
bluezName :: IORef T.Text
bluezName = unsafePerformIO $ newIORef "org.bluez"
{-# NOINLINE bluezName #-}

bluezPath :: IORef ObjectPath
bluezPath = unsafePerformIO $ newIORef "/org/bluez/hci0"
{-# NOINLINE bluezPath #-}
