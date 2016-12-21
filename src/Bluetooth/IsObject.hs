module Bluetooth.HasInterface where

import GHC.TypeLits
import Bluetooth.Types
import DBus
import DBus.Types (object)
import Data.Proxy
import qualified Data.Text as T

class KnownSymbol iface => HasInterface obj (iface :: Symbol) where
  getInterface :: obj -> Proxy iface -> Interface

withInterface :: HasInterface obj iface => obj -> Proxy iface -> Object
withInterface o p = object (T.pack i) $ getInterface o p
  where
    i = symbolVal p

instance HasInterface Application "org.freedesktop.DBus.ObjectManager" where
  getInterface = _
  

 
