module Bluetooth.Internal.HasInterface where


import Control.Monad
import Control.Monad.Except        (liftIO, mapExceptT)
import Control.Monad.Writer.Strict (WriterT)
import Data.IORef
import Data.Proxy
import Data.Word                   (Word16)
import DBus
import DBus.Types                  (SomeSignal, methodError, object)
import GHC.TypeLits
import Lens.Micro

import qualified Data.ByteString as BS
import qualified Data.Map        as Map
import qualified Data.Text       as T

import Bluetooth.Internal.Errors
import Bluetooth.Internal.Interfaces
import Bluetooth.Internal.Types
import Bluetooth.Internal.Utils

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
-- | The @org.freedesktop.DBus.Properties@ interface.
--
-- See the <https://dbus.freedesktop.org/doc/dbus-specification.html#standard-interfaces-properties
-- relevant section of the DBus spec> for more information.

type ChangedProperties = 'TypeStruct
  [ 'DBusSimpleType 'TypeString               -- interface_name
  , AnyDBusDict                               -- changed_properties
  , 'TypeArray ('DBusSimpleType 'TypeString)  -- invalidated_properties
  ]

-- A helper function for constructing D-Bus Property interfaces. Pass a
-- non-Nothing if the object supports the PropertiesChanged signal.
--
-- The 'Get' and 'Set' methods don't seem to be used by the Bluez DBus API, but
-- are supplied for compliance with the D-Bus Property Interface.
defPropIFace :: forall a.
  ( Representable a
  , RepType a ~ AnyDBusDict
  )
  => Maybe ObjectPath -> T.Text -> a -> Interface
defPropIFace opath supportedIFaceName val =
    Interface
      { interfaceMethods = [getAll]
      -- The 'd-bus' library's implementation of @DBus.Property.property@ does
      -- not create an independent signal for PropertyChanged, which makes me
      -- wonder whether this is the right thing to do.
      , interfaceSignals = signals
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
         go :: T.Text -> MethodHandlerT IO a
         go iface
           | iface == supportedIFaceName = return val
           | otherwise = methodError invalidArgs
     {-get-}
       {-= Method (repMethod go)-}
                {-"Get"-}
                {-("interface" :> "propertyName" :> Done)-}
                {-("rep" :> Done)-}
       {-where-}
         {-go :: T.Text -> T.Text -> MethodHandlerT IO (D-}
         {-go iface propName-}
           {-| iface == supportedIFaceName = case Map.lookup propName dbusPropMap of-}
             {-Nothing -> methodError invalidArgs-}
             {-Just p  -> return p-}
           {-| otherwise = methodError invalidArgs-}

     {-dbusPropMap :: Map.Map T.Text (DBusValue TypeVariant)-}
     {-Just dbusPropMap = fromRep (toRep val)-}

     signals = case opath of
       Nothing -> []
       Just p  -> [SSD propertiesChanged]
         where
         propertiesChanged :: SignalDescription '[ChangedProperties]
         propertiesChanged = SignalDescription
           { signalDPath = p
           , signalDInterface = T.pack propertiesIFace
           , signalDMember = "PropertiesChanged"
           , signalDArguments = "changes" :> Done
           }

instance HasInterface (WithObjectPath Service) Properties where
  getInterface service _
    = defPropIFace (Just $ service ^. path) (T.pack gattServiceIFace) service

instance HasInterface (WithObjectPath CharacteristicBS) Properties where
  getInterface char _ = case char ^. value . notifying of
    Nothing -> baseIface
    Just _  -> baseIface { interfaceProperties = SomeProperty prop : interfaceProperties baseIface }
   where
     baseIface = defPropIFace (Just $ char ^. path) (T.pack gattCharacteristicIFace) char
     prop = mkProperty (char ^. path)
                       (T.pack gattCharacteristicIFace)
                       "Value"
                       (handlerToMethodHandler <$> char ^. value . readValue)
                       (fmap handlerToMethodHandler <$> char ^. value . writeValue)
                       PECSTrue

instance HasInterface Advertisement Properties where
  getInterface adv _
    = defPropIFace Nothing (T.pack leAdvertisementIFace) adv

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
                              , SomeProperty val
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
        Just w -> Method (repMethod $ go w)
                          "WriteValue" ("arg" :> Done) ("rep" :> Done)
        Nothing -> Method (repMethod notSup) "WriteValue" Done Done
        where
          go writeTheVal newVal = do
            res <- handlerToMethodHandler $ writeTheVal newVal
            nots <- liftIO $ sequence $ readIORef <$> char ^. value . notifying
            when (nots == Just True && res) $ propertyChanged val newVal
            return res

      stopNotify = Method (repMethod go) "StopNotify" Done Done
        where
          go :: MethodHandlerT IO ()
          go = case char ^. value . notifying of
            Nothing -> return ()
            Just r -> liftIO $ writeIORef r False

      startNotify = Method (repMethod go) "StartNotify" Done Done
        where
          go :: MethodHandlerT IO ()
          go = case char ^. value . notifying of
            Nothing -> return ()
            Just r -> liftIO $ writeIORef r True

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

      val :: Property (RepType BS.ByteString)
      val = mkProperty (char ^. path)
                       (T.pack gattCharacteristicIFace)
                       "Value"
                       (handlerToMethodHandler <$> char ^. value . readValue)
                       (fmap handlerToMethodHandler <$> char ^. value . writeValue)
                       PECSTrue


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

      serviceData' :: Property (RepType (Map.Map UUID BS.ByteString))
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
