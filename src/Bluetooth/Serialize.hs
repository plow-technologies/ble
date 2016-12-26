{-# LANGUAGE UndecidableInstances #-}
module Bluetooth.Serialize where

import qualified Data.ByteString as BS
import qualified Data.Serialize  as S

import Bluetooth.Errors

-- This is no good, since we need annotations for the *monad* of the original
-- type.
class Encoded original encoded | original -> encoded where
  encoded :: original -> encoded

instance S.Serialize a => Encoded (Handler err a)
                                  (Handler err BS.ByteString) where
  encoded o = S.encode <$> o

instance (S.Serialize a, S.Serialize b, ThrowsFailed `IsElem` err)
  => Encoded (a -> Handler err b)
             (BS.ByteString -> Handler err BS.ByteString) where
  encoded f = \b -> case S.decode b of
    Left err -> errFailed
    Right v -> S.encode <$> f v

--- instance Encoded (a -> IO b) (BS.ByteString -> IO BS.ByteString) where
