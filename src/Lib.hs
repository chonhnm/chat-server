{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import Control.Concurrent
import Control.Monad.Fix (fix)
import GHC.IO.Handle (BufferMode (NoBuffering))
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Network.Socket
import System.IO (hGetLine, hPutStrLn, hSetBuffering)

someFunc :: IO ()
someFunc = main

type Msg = String

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1 -- make socket immediately reusable - eases debugging.
  addr:_ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "4343")
  bind sock $ addrAddress addr -- listen on TCP port 4343.
  listen sock 2 -- set a max of 2 queued connections
  chan <- newChan -- chan means channel. single write multiple read channel.
  mainLoop sock chan

mainLoop :: Socket -> Chan Msg -> IO ()
mainLoop sock chan = do
  conn <- accept sock -- accept a connection and handle it
  tid <- forkIO $ runConn conn chan -- run our server's logic
  print tid
  mainLoop sock chan -- repeat

runConn :: (Socket, SockAddr) -> Chan Msg -> IO ()
runConn (sock, _) chan = do
  let broadcast = writeChan chan
  -- change socket to handler
  handler <- socketToHandle sock ReadWriteMode
  hSetBuffering handler NoBuffering
  -- duplicate Chan
  duppedChan <- dupChan chan

  -- fork off a thread for reading from the duplicated channel
  -- read from broadcast
  _ <- forkIO $ fix $ \loop -> do
    line <- readChan duppedChan
    hPutStrLn handler line
    loop

  -- read lines form the socket and echo them back to the user
  -- write to broadcast
  fix $ \loop -> do
    line <- fmap init (hGetLine handler)
    broadcast line
    loop
