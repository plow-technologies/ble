module Bluetooth.Internal.Interfaces where

import Data.Proxy
import DBus
import GHC.TypeLits


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
