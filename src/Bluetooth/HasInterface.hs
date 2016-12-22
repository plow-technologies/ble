module Bluetooth.HasInterface where

import GHC.TypeLits
import Bluetooth.Types
import DBus
import DBus.Types (object, methodError)
import Data.Proxy
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
                ("inteface" :> Done)
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
                ("inteface" :> Done)
                ("rep" :> Done)
       where
         go :: T.Text -> MethodHandlerT IO (WithObjectPath Characteristic)
         go iface
           | iface == "org.bluez.GattService1" = return char
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
      , interfaceProperties = [SomeProperty uuid, SomeProperty primary]
      }
    where
      uuid :: Property (RepType UUID)
      uuid = Property
        { propertyPath = objectPath $ objectPathToText (wopOP service) </> "UUID"
        , propertyInterface = T.pack gattServiceIFace
        , propertyName = "UUID"
        , propertyGet = Just . return . toRep . serviceUUID $ wopV service
        , propertySet = Nothing
        , propertyEmitsChangedSignal = PECSFalse
        }
        
      primary :: Property (RepType Bool)
      primary = Property
        { propertyPath = objectPath $ objectPathToText (wopOP service) </> "Primary"
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
      { interfaceMethods = [readValue, writeValue, startNotify, stopNotify]
      , interfaceSignals = []
      , interfaceAnnotations = []
      , interfaceProperties = [SomeProperty uuid, SomeProperty service, SomeProperty flags]
      }
    where
      notSup :: MethodHandlerT IO ()
      notSup = methodError notSupported
      
      readValue = case characteristicRead $ wopV char of
        Just v -> Method (repMethod v) "ReadValue" Done ("rep" :> Done)
        Nothing -> Method (repMethod notSup) "ReadValue" Done Done
        
      writeValue = case characteristicWrite $ wopV char of
        Just v -> Method (repMethod v) "ReadValue" ("arg" :> Done) ("rep" :> Done)
        Nothing -> Method (repMethod notSup) "ReadValue" Done Done
        
      stopNotify = Method (repMethod notSup) "StopNotify" Done Done
          
      startNotify = Method (repMethod go) "StartNotify" Done Done
        where
          go :: MethodHandlerT IO ()
          go = return ()

      uuid :: Property (RepType UUID)
      uuid = Property
        { propertyPath = objectPath $ objectPathToText (wopOP char) </> "UUID"
        , propertyInterface = T.pack gattCharacteristicIFace
        , propertyName = "UUID"
        , propertyGet = Just . return . toRep . characteristicUUID $ wopV char
        , propertySet = Nothing
        , propertyEmitsChangedSignal = PECSFalse
        }
        
      service :: Property (RepType ObjectPath)
      service = Property
        { propertyPath = objectPath $ objectPathToText (wopOP char) </> "Service"
        , propertyInterface = T.pack gattCharacteristicIFace
        , propertyName = "Service"
        , propertyGet = Just . return . toRep . objectPath . parentPath . objectPathToText
            $ wopOP char 
        , propertySet = Nothing
        , propertyEmitsChangedSignal = PECSFalse
        }
        
      flags :: Property (RepType [CharacteristicProperty])
      flags = Property
        { propertyPath = objectPath $ objectPathToText (wopOP char) </> "Flags"
        , propertyInterface = T.pack gattCharacteristicIFace
        , propertyName = "Flags"
        , propertyGet = Just . return . toRep . characteristicProperties $ wopV char
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
           
