module Main (main) where

-- A simple example of a complete Bluetooth Low Energy application. The
-- application allows a counter to be read, and adds one to the value of the
-- counter, as well as allowing the counter to be set to any value.

import Bluetooth
import Control.Concurrent     (threadDelay)
import Control.Concurrent.STM
import Control.Monad.IO.Class

main :: IO ()
main = do
  ref <- newTVarIO 0
  conn <- connect
  x <- runBluetoothM (registerAndAdvertiseApplication $ app ref) conn
  case x of
    Right _ -> putStrLn "Started BLE counter application!"
    Left e -> error $ "Error starting application " ++ show e
  threadDelay maxBound

app :: TVar Int -> Application
app ref
  = "/com/turingjump/example/counter"
     & services .~ [counter ref]

counter :: TVar Int -> Service
counter ref
  = "4f1f704f-0a0b-49e4-bd27-6368f27697a7"
     & characteristics .~ [getCounter ref]

getCounter :: TVar Int -> CharacteristicBS
getCounter ref
  = "90874979-563e-4224-9da6-3b1a6c03e97d"
      & readValue  ?~ encodeRead readV
      & writeValue ?~ encodeWrite writeV
      & properties .~ [CPRead, CPWrite]
  where
    readV :: Handler Int
    readV = liftIO $ do
      v <- atomically $ modifyTVar' ref succ >> readTVar ref
      putStrLn $ "Value requested. New value: " ++ show v
      return v

    writeV :: Int -> Handler Bool
    writeV i = liftIO $ do
      v <- atomically $ swapTVar ref i
      putStrLn $ "Value changed to: " ++ show i
      putStrLn $ "Old value: " ++ show v
      return True
