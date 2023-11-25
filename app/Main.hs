{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (fromMaybe)
import Format
import Handle
--import Network.Simple.TCP (HostPreference (..), recv, send, serve)
import Syntax
import System.Environment (getArgs)

import Control.Concurrent (ThreadId, forkIO)
import Data.Function
import Data.Functor
import Network.Socket (
    Family (..),
    SockAddr (..),
    Socket,
    SocketOption (..),
    SocketType (..),
    PortNumber (..),
    accept,
    bind,
    close,
    defaultProtocol,
    listen,
    setSocketOption,
    socket,
 )
import Network.Socket.ByteString (recv, send)

did :: ByteString
did = "id: 0"

ddata :: ByteString
ddata = "data: hello world"

datum :: ByteString
datum = "data: hello world"

sser :: ByteString
sser = "200 OK\r\ncontent-type: text/event-stream\r\n\r\n"

eachConn :: Env -> Socket -> IO ()
eachConn env conn = do
    req <- recv conn 1024
    res <- handle env req
    print req
    _ <- send conn (toBs res)
    close conn
    --case runParser req of
        --Right rast -> do
            ----res <- Handler.handle rast
            --send conn "200" -- res
            ----_ <- forkIO $ handle conn
            --return ()
        --Left _ -> close conn

--let isSse = B.empty == getHeader "text/event-stream" (headers' bsRes)
--if isSse
--then do
    --print "SSE START\n\n"
    --send serverSocket (toBs bsRes)
    --print (toBs bsRes)
    --forever $ do
        ----forkIO $ handle conn store
        --print "SSE\n\n"
        --send serverSocket did
        --send serverSocket ddata
--else send serverSocket (toBs bsRes)

main :: IO ()
main = do
    argv <- getArgs

    BLC.putStrLn "Logs from your program will appear here"

    let port = 4221 :: PortNumber
        env = case argv of
            ["--directory", directory] -> Env $ pack directory
            _ -> Env B.empty

    BLC.putStrLn $ "Listening on port: " <> BLC.pack (show port)

    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port 0)
    listen sock 5
    forever $ do
        (conn, address) <- accept sock
        forkIO $ eachConn env conn
