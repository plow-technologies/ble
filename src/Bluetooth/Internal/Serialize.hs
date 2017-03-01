{-# LANGUAGE UndecidableInstances #-}
module Bluetooth.Internal.Serialize where

import qualified Data.ByteString as BS
import qualified Data.Serialize  as S

import Bluetooth.Internal.Errors

encodeRead :: S.Serialize a => Handler a -> Handler BS.ByteString
encodeRead h = S.encode <$> h

encodeWrite :: (S.Serialize a)
  => (a -> Handler Bool) -> (BS.ByteString -> Handler Bool)
encodeWrite h v = case S.decode v of
  Left _   -> errorFailed
  Right v' -> h v'
