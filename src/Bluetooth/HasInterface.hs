module Bluetooth.HasInterface where


import Control.Monad.Except        (liftIO, mapExceptT)
import Control.Monad.Writer.Strict (WriterT)
import Data.Proxy
import Data.Word                   (Word16)
import DBus
import DBus.Types                  (SomeSignal, methodError, object)
import GHC.TypeLits
import Lens.Micro

import qualified Data.ByteString as BS
import qualified Data.Map        as Map
import qualified Data.Text       as T

import Bluetooth.Errors
import Bluetooth.Interfaces
import Bluetooth.Types
import Bluetooth.Utils

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

type ChangedProperties = 'TypeStruct
  [ 'DBusSimpleType 'TypeString               -- interface_name
  , AnyDBusDict                               -- changed_properties
  , 'TypeArray ('DBusSimpleType 'TypeString)  -- invalidated_properties
  ]

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
           | iface == T.pack gattServiceIFace = return service
           | otherwise = methodError invalidArgs

instance HasInterface (WithObjectPath CharacteristicBS) Properties where
  getInterface char _ =
    Interface
      { interfaceMethods = [getAll]
      , interfaceSignals = [SSD propertiesChanged]
      , interfaceAnnotations = []
      , interfaceProperties = []
      }
    where
     propertiesChanged :: SignalDescription '[ChangedProperties]
     propertiesChanged = SignalDescription
       { signalDPath = char ^. path
       , signalDInterface = T.pack propertiesIFace
       , signalDMember = "PropertiesChanged"
       , signalDArguments = "changes" :> Done
       }
     getAll
       = Method (repMethod go)
                "GetAll"
                ("interface" :> Done)
                ("rep" :> Done)
       where
         go :: T.Text -> MethodHandlerT IO (WithObjectPath CharacteristicBS)
         go iface
           | iface == T.pack gattCharacteristicIFace = return char
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
           | iface == T.pack leAdvertisementIFace = return adv
           | otherwise = methodError invalidArgs

-- * GattService


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

acceptingOptions :: MethodHandlerT IO BS.ByteString
 -> CharacteristicOptions
 -> MethodHandlerT IO BS.ByteString
acceptingOptions handler opts = case opts ^. offset of
  Nothing -> handler
  Just v -> BS.drop (fromInteger $ toInteger v) <$> handler

handlerToMethodHandler :: Handler errs a -> MethodHandlerT IO a
handlerToMethodHandler (Handler h) = MHT $ mapExceptT go h
  where
    go :: IO (Either T.Text a) -> WriterT [SomeSignal] IO (Either MsgError a)
    go x = liftIO $ x >>= \x' -> case x' of
      Left e -> return . Left $ MsgError e Nothing []
      Right v -> return $ Right v

instance HasInterface (WithObjectPath CharacteristicBS) GattCharacteristic where
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
        Just v -> Method (repMethod $ acceptingOptions $ handlerToMethodHandler v)
                         "ReadValue" ("options" :> Done) ("rep" :> Done)
        Nothing -> Method (repMethod notSup) "ReadValue" Done Done

      writeVal = case char ^. value . writeValue of
        Just v -> Method (repMethod $ handlerToMethodHandler <$> v)
                          "WriteValue" ("arg" :> Done) ("rep" :> Done)
        Nothing -> Method (repMethod notSup) "WriteValue" Done Done

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

      manufacturerData' :: Property (RepType (Map.Map Word16 BS.ByteString))
      manufacturerData' = Property
        { propertyPath = objectPath $ (adv ^. path . toText) </> "ManufacturerData"
        , propertyInterface = T.pack leAdvertisementIFace
        , propertyName = "ManufacturerData"
        , propertyGet = Just . return . toRep $ adv ^. value . manufacturerData
        , propertySet = Nothing
        , propertyEmitsChangedSignal = PECSFalse
        }

      serviceData' :: Property (RepType (Map.Map T.Text BS.ByteString))
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
