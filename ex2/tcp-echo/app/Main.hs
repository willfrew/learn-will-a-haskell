module Main where

import Lib

listenAddr :: HostPreference
listenAddr = Host "127.0.0.1"

listenPort :: Port
listenPort = "8080"

main = createEchoServer listenAddr listenPort
