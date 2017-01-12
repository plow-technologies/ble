{-# LANGUAGE UndecidableInstances #-}
module Bluetooth.Internal.Serialize where

import qualified Data.ByteString as BS
import qualified Data.Serialize  as S

import Bluetooth.Internal.Errors

encodeRead :: S.Serialize a => ReadValueM a -> ReadValueM BS.ByteString
encodeRead h = S.encode <$> h

encodeWrite :: (S.Serialize a)
  => (a -> WriteValueM Bool) -> (BS.ByteString -> WriteValueM Bool)
encodeWrite h v = case S.decode v of
  Left _   -> errFailed
  Right v' -> h v'
