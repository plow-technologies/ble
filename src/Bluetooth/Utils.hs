module Bluetooth.Utils where

import qualified Data.Text as T
import Lens.Micro
import DBus

-- Doing Iso's would require the full 'lens' dependency
class ToText a where
  toText :: Lens' a T.Text

instance ToText ObjectPath where
  toText = lens objectPathToText (const objectPath)

class FromText a where
  fromText :: Lens' T.Text a

instance FromText ObjectPath where
  fromText = lens objectPath (const objectPathToText)
