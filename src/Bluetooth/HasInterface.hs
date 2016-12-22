module Bluetooth.HasInterface where

import Bluetooth.Types
import Bluetooth.Utils
import Data.Proxy
import DBus
import DBus.Types      (methodError, object)
import GHC.TypeLits
import Lens.Micro

import qualified Data.Map  as Map
import qualified Data.Text as T

-- The Bluez DBus API makes certain requirements about the interfaces
-- that objects must meet. These requirements are outlined in:
--
-- https://kernel.googlesource.com/pub/scm/bluetooth/bluez/+/5.43/doc/gatt-api.txt
--
-- In this file, we declare how our objects meet these interfaces.


-- A class that declares that @obj@ meets the interface @iface@.
class KnownSymbol iface => HasInterface obj (iface :: Symbol) where
  getInterface :: obj -> Proxy iface -> Interface

withInterface :: HasInterface obj iface => obj -> Proxy iface -> Object
withInterface o p = object (T.pack i) $ getInterface o p
  where
    i = symbolVal p


-- * ObjectManager

type ObjectManager = "org.freedesktop.DBus.ObjectManager"
objectManagerIFaceP :: Proxy ObjectManager
objectManagerIFaceP = Proxy
objectManagerIFace :: String
objectManagerIFace = symbolVal objectManagerIFaceP


instance HasInterface Application ObjectManager where
  getInterface app _ =
    Interface
      { interfaceMethods = [getManagedObjects]
      , interfaceSignals = []
      , interfaceAnnotations = []
      , interfaceProperties = []
      }
    where

    getManagedObjects :: Method
    getManagedObjects
      = Method (repMethod (return app :: IO Application))
               "GetManagedObjects"
               Done
               ("rep" :> Done)

-- * Properties

type Properties    = "org.freedesktop.DBus.Properties"
propertiesIFaceP :: Proxy Properties
propertiesIFaceP = Proxy
propertiesIFace :: String
propertiesIFace = symbolVal propertiesIFaceP

instance HasInterface (WithObjectPath Service) Properties where
  getInterface service _ =
    Interface
      { interfaceMethods = [getAll]
      , interfaceSignals = []
      , interfaceAnnotations = []
      , interfaceProperties = []
      }
    where
     getAll
       = Method (repMethod go)
                "GetAll"
                ("interface" :> Done)
                ("rep" :> Done)
       where
         go :: T.Text -> MethodHandlerT IO (WithObjectPath Service)
         go iface
           | iface == "org.bluez.GattService1" = return service
           | otherwise = methodError invalidArgs

instance HasInterface (WithObjectPath Characteristic) Properties where
  getInterface char _ =
    Interface
      { interfaceMethods = [getAll]
      , interfaceSignals = []
      , interfaceAnnotations = []
      , interfaceProperties = []
      }
    where
     getAll
       = Method (repMethod go)
                "GetAll"
                ("interface" :> Done)
                ("rep" :> Done)
       where
         go :: T.Text -> MethodHandlerT IO (WithObjectPath Characteristic)
         go iface
           | iface == "org.bluez.GattCharacteristic1" = return char
           | otherwise = methodError invalidArgs

instance HasInterface Advertisement Properties where
  getInterface adv _ =
    Interface
      { interfaceMethods = [getAll]
      , interfaceSignals = []
      , interfaceAnnotations = []
      , interfaceProperties = []
      }
    where
     getAll
       = Method (repMethod go)
                "GetAll"
                ("interface" :> Done)
                ("rep" :> Done)
       where
         go :: T.Text -> MethodHandlerT IO Advertisement
         go iface
           | iface == "org.bluez.LEAdvertisement1" = return adv
           | otherwise = methodError invalidArgs

-- * GattService

type GattService = "org.Bluez.GattService1"
gattServiceIFaceP :: Proxy GattService
gattServiceIFaceP = Proxy

gattServiceIFace :: String
gattServiceIFace = symbolVal gattServiceIFaceP

instance HasInterface (WithObjectPath Service) GattService where
  getInterface service _ =
    Interface
      { interfaceMethods = []
      , interfaceSignals = []
      , interfaceAnnotations = []
      , interfaceProperties = [SomeProperty uuid', SomeProperty primary]
      }
    where
      uuid' :: Property (RepType UUID)
      uuid' = Property
        { propertyPath = objectPath $ (service ^. path . toText) </> "UUID"
        , propertyInterface = T.pack gattServiceIFace
        , propertyName = "UUID"
        , propertyGet = Just . return $ toRep (service ^. value . uuid)
        , propertySet = Nothing
        , propertyEmitsChangedSignal = PECSFalse
        }

      primary :: Property (RepType Bool)
      primary = Property
        { propertyPath = objectPath $ (service ^. path . toText) </> "Primary"
        , propertyInterface = T.pack gattServiceIFace
        , propertyName = "Primary"
        , propertyGet = Just . return $ toRep True
        , propertySet = Nothing
        , propertyEmitsChangedSignal = PECSFalse
        }

-- * GattCharacteristic

type GattCharacteristic = "org.Bluez.GattCharacteristic1"
gattCharacteristicIFaceP :: Proxy GattCharacteristic
gattCharacteristicIFaceP = Proxy

gattCharacteristicIFace :: String
gattCharacteristicIFace = symbolVal gattServiceIFaceP

instance HasInterface (WithObjectPath Characteristic) GattCharacteristic where
  getInterface char _ =
    Interface
      { interfaceMethods = [readVal, writeVal, startNotify, stopNotify]
      , interfaceSignals = []
      , interfaceAnnotations = []
      , interfaceProperties = [ SomeProperty uuid'
                              , SomeProperty service
                              , SomeProperty flags
                              ]
      }
    where
      notSup :: MethodHandlerT IO ()
      notSup = methodError notSupported

      readVal = case char ^. value . readValue of
        Just v -> Method (repMethod v) "ReadValue" Done ("rep" :> Done)
        Nothing -> Method (repMethod notSup) "ReadValue" Done Done

      writeVal = case char ^. value . writeValue of
        Just v -> Method (repMethod v) "ReadValue" ("arg" :> Done) ("rep" :> Done)
        Nothing -> Method (repMethod notSup) "ReadValue" Done Done

      stopNotify = Method (repMethod notSup) "StopNotify" Done Done

      startNotify = Method (repMethod go) "StartNotify" Done Done
        where
          go :: MethodHandlerT IO ()
          go = return ()

      uuid' :: Property (RepType UUID)
      uuid' = Property
        { propertyPath = objectPath $ (char ^. path . toText) </> "UUID"
        , propertyInterface = T.pack gattCharacteristicIFace
        , propertyName = "UUID"
        , propertyGet = Just . return . toRep $ char ^. value . uuid
        , propertySet = Nothing
        , propertyEmitsChangedSignal = PECSFalse
        }

      service :: Property (RepType ObjectPath)
      service = Property
        { propertyPath = objectPath $ (char ^. path . toText) </> "Service"
        , propertyInterface = T.pack gattCharacteristicIFace
        , propertyName = "Service"
        , propertyGet = Just . return . toRep . objectPath . parentPath
            $ char ^. path . toText
        , propertySet = Nothing
        , propertyEmitsChangedSignal = PECSFalse
        }

      flags :: Property (RepType [CharacteristicProperty])
      flags = Property
        { propertyPath = objectPath $ (char ^. path . toText) </> "Flags"
        , propertyInterface = T.pack gattCharacteristicIFace
        , propertyName = "Flags"
        , propertyGet = Just . return . toRep $ char ^. value . properties
        , propertySet = Nothing
        , propertyEmitsChangedSignal = PECSFalse
        }

type LEAdvertisement = "org.Bluez.LLAdvertisement1"
leAdvertisementIFaceP :: Proxy LEAdvertisement
leAdvertisementIFaceP = Proxy

leAdvertisementIFace :: String
leAdvertisementIFace = symbolVal leAdvertisementIFaceP

instance HasInterface (WithObjectPath Advertisement) LEAdvertisement where
  getInterface adv _ =
    Interface
      { interfaceMethods = [release]
      , interfaceSignals = []
      , interfaceAnnotations = []
      , interfaceProperties = [ SomeProperty type'
                              , SomeProperty serviceUUIDs'
                              , SomeProperty manufacturerData'
                              , SomeProperty solicitUUIDs'
                              , SomeProperty serviceData'
                              , SomeProperty includeTxPower']
      }
    where
      release = Method (repMethod (return () :: IO ())) "Release" Done Done

      type' :: Property (RepType AdvertisementType)
      type' = Property
        { propertyPath = objectPath $ (adv ^. path . toText) </> "Type"
        , propertyInterface = T.pack leAdvertisementIFace
        , propertyName = "Type"
        , propertyGet = Just . return . toRep $ adv ^. value . type_
        , propertySet = Nothing
        , propertyEmitsChangedSignal = PECSFalse
        }

      serviceUUIDs' :: Property (RepType [UUID])
      serviceUUIDs' = Property
        { propertyPath = objectPath $ (adv ^. path . toText) </> "ServiceUUIDs"
        , propertyInterface = T.pack leAdvertisementIFace
        , propertyName = "ServiceUUIDs"
        , propertyGet = Just . return . toRep $ adv ^. value . serviceUUIDs
        , propertySet = Nothing
        , propertyEmitsChangedSignal = PECSFalse
        }

      solicitUUIDs' :: Property (RepType [UUID])
      solicitUUIDs' = Property
        { propertyPath = objectPath $ (adv ^. path . toText) </> "SolicitUUIDs"
        , propertyInterface = T.pack leAdvertisementIFace
        , propertyName = "SolicitUUIDs"
        , propertyGet = Just . return . toRep $ adv ^. value . solicitUUIDs
        , propertySet = Nothing
        , propertyEmitsChangedSignal = PECSFalse
        }

      manufacturerData' :: Property (RepType (Map.Map T.Text T.Text))
      manufacturerData' = Property
        { propertyPath = objectPath $ (adv ^. path . toText) </> "ManufacturerData"
        , propertyInterface = T.pack leAdvertisementIFace
        , propertyName = "ManufacturerData"
        , propertyGet = Just . return . toRep $ adv ^. value . manufacturerData
        , propertySet = Nothing
        , propertyEmitsChangedSignal = PECSFalse
        }

      serviceData' :: Property (RepType (Map.Map UUID T.Text))
      serviceData' = Property
        { propertyPath = objectPath $ (adv ^. path . toText) </> "ServiceData"
        , propertyInterface = T.pack leAdvertisementIFace
        , propertyName = "ServiceData"
        , propertyGet = Just . return . toRep $ adv ^. value . serviceData
        , propertySet = Nothing
        , propertyEmitsChangedSignal = PECSFalse
        }

      includeTxPower' :: Property (RepType Bool)
      includeTxPower' = Property
        { propertyPath = objectPath $ (adv ^. path . toText) </> "IncludeTxPower"
        , propertyInterface = T.pack leAdvertisementIFace
        , propertyName = "IncludeTxPower"
        , propertyGet = Just . return . toRep $ adv ^. value . includeTxPower
        , propertySet = Nothing
        , propertyEmitsChangedSignal = PECSFalse
        }




-- * Utils


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
