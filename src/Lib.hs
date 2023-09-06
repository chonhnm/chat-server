{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import Control.Concurrent
import Control.Exception (handle)
import Control.Exception.Base (SomeException (..))
import Control.Monad (when)
import Control.Monad.Fix (fix)
import GHC.IO.Handle (BufferMode (NoBuffering), hClose)
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Network.Socket
import System.IO (hGetLine, hPutStrLn, hSetBuffering)

someFunc :: IO ()
someFunc = main

type Msg = (Int, String)

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1 -- make socket immediately reusable - eases debugging.
  addr : _ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "4343")
  bind sock $ addrAddress addr -- listen on TCP port 4343.
  listen sock 2 -- set a max of 2 queued connections
  chan <- newChan -- chan means channel. single write multiple read channel.

  -- read init chan ?why
  _ <- forkIO $ fix $ \loop -> do
    (_, _) <- readChan chan
    loop
  mainLoop sock chan 0

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  conn <- accept sock -- accept a connection and handle it
  tid <- forkIO $ runConn conn chan msgNum -- run our server's logic
  print $ "write threadId:" ++ show tid
  mainLoop sock chan $! msgNum + 1 -- repeat

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
  let broadcast msg = writeChan chan (msgNum, msg)
  -- change socket to handler
  handler <- socketToHandle sock ReadWriteMode
  hSetBuffering handler NoBuffering

  -- welcome
  hPutStrLn handler "Hi, what's your name?"
  name <- fmap init (hGetLine handler)
  broadcast ("-->" ++ name ++ " entered chat.")
  hPutStrLn handler ("Welcome, " ++ name ++ "!")

  -- duplicate Chan
  duppedChan <- dupChan chan

  -- fork off a thread for reading from the duplicated channel
  -- read from broadcast
  reader <- forkIO $ fix $ \loop -> do
    (nextNum, line) <- readChan duppedChan
    when (msgNum /= nextNum) $ hPutStrLn handler line
    loop
  print $ "read threadId:" ++ show reader  

  -- read lines form the socket and echo them back to the user
  -- write to broadcast
  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
    line <- fmap init (hGetLine handler)
    case line of
      "quit" -> hPutStrLn handler "Bye!"
      _ -> broadcast (name ++ ": " ++ line) >> loop

  -- kill after the loop ends
  killThread reader
  broadcast ("<--" ++ name ++ " left.")
  hClose handler
