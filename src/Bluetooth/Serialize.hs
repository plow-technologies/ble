module Bluetooth.Serialize where

import DBus (MethodHandlerT, MsgError(..))
import DBus.Types (methodError)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Serialize as S

class Encoded original encoded | original -> encoded where
  encoded :: original -> encoded

instance S.Serialize a => Encoded (MethodHandlerT IO a) (MethodHandlerT IO BS.ByteString) where
  encoded o = S.encode <$> o

instance (S.Serialize a, S.Serialize b)
  => Encoded (a -> MethodHandlerT IO b) (BS.ByteString -> MethodHandlerT IO BS.ByteString) where
  encoded f = \b -> case S.decode b of
    Left err -> methodError $ MsgError
      { errorName = "org.bluez.Error.Failed"
      , errorText = Just $ T.pack err
      , errorBody = []
      }
    Right v -> S.encode <$> f v 

--- instance Encoded (a -> IO b) (BS.ByteString -> IO BS.ByteString) where
  

