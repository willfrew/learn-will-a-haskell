module Lib
  ( createEchoServer
  , HostPreference ( HostAny, HostIPv4, HostIPv6, Host )
  , Port
  ) where

import Control.Monad ( forever )
import Network.Simple.TCP
import qualified Data.ByteString as B

type Port = ServiceName

maxBytesToRead :: Int
maxBytesToRead = 1024

-- Echos new line delimited strings to a Socket
echo :: Socket -> IO ()
echo socket = do
  -- Read (up to) 1024 bytes from the socket
  maybeBytes <- recv socket maxBytesToRead
  case maybeBytes of
    Nothing -> return ()
    Just bytes -> do
      send socket bytes
      echo socket

handleConnection :: (Socket, SockAddr) -> IO ()
handleConnection (socket, remoteAddr) = do
  putStrLn $ "Connection established from " ++ show remoteAddr
  echo socket
  putStrLn $ "Connection to " ++ show remoteAddr ++ " closed"

createEchoServer :: HostPreference -> Port -> IO ()
createEchoServer listenAddr listenPort = serve listenAddr listenPort handleConnection
