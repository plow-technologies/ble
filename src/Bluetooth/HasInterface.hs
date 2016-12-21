module Bluetooth.HasInterface where

import GHC.TypeLits
import Bluetooth.Types
import DBus
import DBus.Types (object, methodError)
import Data.Proxy
import qualified Data.Text as T

class KnownSymbol iface => HasInterface obj (iface :: Symbol) where
  getInterface :: obj -> Proxy iface -> Interface

withInterface :: HasInterface obj iface => obj -> Proxy iface -> Object
withInterface o p = object (T.pack i) $ getInterface o p
  where
    i = symbolVal p

type ObjectManager = "org.freedesktop.DBus.ObjectManager"
objectManagerIFace :: Proxy ObjectManager
objectManagerIFace = Proxy

type Properties    = "org.freedesktop.DBus.Properties"
propertiesIFace :: Proxy Properties
propertiesIFace = Proxy

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


  
instance HasInterface (WithObjectPath Service) Properties where
  getInterface service _ =
    Interface
      { interfaceMethods = [get, set, getAll]
      , interfaceSignals = []
      , interfaceAnnotations = []
      , interfaceProperties = []
      }
    where  
     get = _
     set = _
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

invalidArgs :: MsgError
invalidArgs = MsgError
  { errorName = "org.freedesktop.DBus.Error.InvalidArgs"
  , errorText = Nothing
  , errorBody = []
  }
           
